# Green-function bubble route to fxc

This change adds an initial diagonal, frozen-screening GW route to the
`ChiFxc` export. The existing level-correction route remains the default:
`ChiGMode="LEVELS"`. The G modes are `G0`, `COHSEX`, `QP` and `DYSON`.
`COHSEX` works with native static screening; `QP` takes the energies of any
diagonal GW run (G0W0 or GW0, PPA/MPA or real axis) as unit-weight poles;
`DYSON` uses PPA dynamic screening.

Base: Adolfo-PL/yambo, branch `claude/nbd-file-generation-path-yidcr2`,
commit `2d64938d8328b184ffbf7ce82972d737a8e5c12a`.

## Implemented approximation

The orbitals, KS reference, chemical potential and screening are fixed. The
existing PPA GW machinery evaluates the diagonal correlation self-energy on a
real energy grid. With the new `GRetarded` flag it uses retarded denominators
for both occupied and empty intermediate states. The exported propagator is

```
DeltaSigma_nk(z) = Sigma_x,nk + Sigma_c,nk(z) - Vxc_KS,nk
G_nk(z) = 1 / [z - epsilon_KS,nk - DeltaSigma_nk(z)]
A_nk(E) = -Im G_nk(E + i eta) / pi
```

All energies here are in Yambo's internal KS frame, which OCCUPATIONS_Fermi
shifts so that the Fermi level (the VBM of an insulator) is zero. Yambo keeps
the unshifted Fermi energy in `E_Fermi`; `ndb.G` stores it as
`G_CHEMICAL_POTENTIAL` only to identify the reference. The loaders never use
it as a chemical potential: occupations are those of the T=0 KS reference, and
the chemical potential of the response is placed in the middle of the QP gap
(between the highest occupied and the lowest empty QP pole or peak).
Positive `GDmRnge` controls eta; `GDamping` must be zero so Sigma and G are
evaluated at the same complex energy. The retarded PPA path preserves this
imaginary part when preparing its self-energy sampling grid.

For static W, Yambo's native COHSEX/Newton solver produces diagonal energies
`E_COHSEX = epsilon_KS + Sigma_SEX + Sigma_COH - Vxc_KS` and unit residues.
`ChiGMode="COHSEX"` reads its `ndb.QP` and constructs

```
G_nk(z) = 1 / [z - E_COHSEX,nk]
A_nk(E) = delta(E - E_COHSEX,nk)
```

The bubble is evaluated analytically from these poles. No spectral energy
grid, numerical delta broadening, fitted Z, or second subtraction of Vxc is
introduced. With fixed orbitals this static diagonal approximation gives
the same bubble as using the corrected energies with unit residues. It
does not reproduce dynamical GW satellites or lifetimes.

For each q, the response loops over the complete requested band-pair
range and BZ k points, using Yambo's `qindx_X` mapping k -> k-q and density
vertices from `scatter_Bamp`. At the optical q=0 point, the G=0 density
vertex instead uses Yambo's position dipoles and its numerical q0 norm;
the remaining G components use the native k-star rotation. The frequency
factor for a pair a,k and b,k-q is

```
integral dE dE' A_a,k(E) A_b,k-q(E')
    * [f(E') - f(E)] / [z - E + E']
```

It multiplies `rho*(G) rho(G')/(Nk_BZ V_cell)`, including the appropriate
spin factor. Spectral quadrature uses nonuniform trapezoid weights. It does
not divide spectra by their integrated weight. Missing spectral tails or
poor resolution trigger an error when `ChiGNormTol` is exceeded.

The independent KS bubble uses the analytic G0 pole limit of this expression.
Before export, it must agree with the native `X_irredux` KS response within
`ChiGCheckTol`. The export contains

```
ChiGMode="G0":    chi0=P_G0; fxc=0
ChiGMode="COHSEX":chi0=P_G0; P=P_G_COHSEX; fxc=chi0^-1-P_G_COHSEX^-1
ChiGMode="QP":    chi0=P_G0; P=P_G_QP;     fxc=chi0^-1-P_G_QP^-1
ChiGMode="DYSON": chi0=P_G0; P=P_G; fxc=chi0^-1-P_G^-1
```

Inversions use double-precision, Coulomb-symmetrized matrices with LAPACK
factorization, condition estimation and inversion checks. No pseudoinverse
or regularization is applied. An ill-conditioned basis is rejected.

This is a **bare-vertex dressed-bubble kernel**. It does not implement
`delta Sigma/delta G`, `delta W/delta G`, a Bethe-Salpeter vertex, or the full
physical GW density response. It should not be interpreted as the final
excitonic kernel from the background derivation. No extra `-v` enters the
inverse difference because P is irreducible here. KS G0 is evaluated
analytically; no large real-space G0 database is constructed.

## Build and first run

Apply the patch to the base checkout, then rebuild Yambo using your usual
Linux/HPC configuration, including NetCDF and LAPACK. Both new source files
are registered in their directories' `.objects` lists. A clean rebuild is
recommended because the QP and Chi module types changed.

Start with one CPU MPI rank, a small insulating SAVE database, a small
response band range and a small density basis. Include q index 1 for the
optical test.
The COHSEX route has no spectral convolution and needs only pole-pair sums.
The dynamical spectral convolution costs approximately
`Nk * Nb^2 * NE^2 * N_response_frequencies`, before the density-matrix
accumulation. Validate the G0 baseline against native `X_irredux` before
using the COHSEX kernel.

### Static screening: COHSEX route

Use this route when you have only a static W calculation. Retain SAVE and
the native `ndb.em1s` database and its fragments. The standalone `ndb.W`
export is not an input to the native COHSEX solver. If the native screening
databases are absent or incompatible, Yambo recalculates static screening.

Generate a COHSEX input with the Newton solver:

```sh
yambo -p c -g n -F cohsex.in -J cohsex
```

Keep your converged static screening settings and choose `QPkrange` to
cover **every response band, k point and spin**, with diagonal states only.
Keep the KS starting energies and orbitals unmodified; do not enable
external energy/width/residue corrections, self-consistent iterations or
mixed self-energies. Converge the native COHSEX band and screening cutoffs.
Run the edited input to produce `cohsex/ndb.QP`:

```sh
yambo -F cohsex.in -J cohsex
yambo -d s -F chi_cohsex.in -J chi_cohsex
```

In the generated static response input, set/add:

```text
ChiFxc
ChiGMode="G0"
ChiGAxis="IMAG"
ChiGNuSteps=16
% ChiGNuRnge
 0.0 | 20.0 | eV
%
ChiGCheckTol=0.0001
ChiRcondMin=1.0e-12
XTermKind="none"
% EhEngyXs
 0.0 | 0.0 | eV
%
```

Set `BndsRnXs` to the response band range, `NGsBlkXs` to the density basis,
and include q index 1 in `QpntsRXs` for an optical calculation. Set
`LongDrXs` to the desired optical field direction and later use the same
direction in BSE's `BLongDir`. Disable double grids and external
X energy, width, residue and Green-function corrections. Run once in G0
mode in a fresh `chi_g0` job to validate the KS baseline. Then change/add:

```sh
yambo -F chi_cohsex.in -J chi_g0
```

```text
ChiGMode="COHSEX"
ChiGDb="cohsex/ndb.QP"
```

```sh
yambo -F chi_cohsex.in -J chi_cohsex
```

The imaginary export grid can contain many frequencies even though native
screening is static. Its response matrix and native frequency grid are
preserved. The loader rejects a non-COHSEX QP database, nonunit residues,
linewidths, off-diagonal or duplicate states, incomplete state coverage,
different KS reference energies, and corrected poles that close the gap (an
occupied pole above an empty one). Such crossings need a separate treatment of
the chemical potential and occupations. `ChiGNormTol` is used only for the
dynamic spectral route; static poles have exact unit weight.

### GW energies as unit-weight poles: QP route

`ChiGMode="QP"` builds the same pole bubble from the energies of any diagonal
GW `ndb.QP` written by the Newton solver: COHSEX, G0W0 or GW0, with PPA/MPA or
real-axis screening. Each state becomes

```
A_nk(E) = delta(E - Re E_QP,nk)
```

The linearized energy `E_QP = e_KS + Z <Sigma(e_KS) - Vxc>` already contains Z,
but the pole keeps unit weight. The loader therefore drops the residue Z and
Im E, and reports both in the report file (`Z dropped from QP poles`, `max |Im E|
dropped from QP poles`). A width larger than a quarter of the state's QP shift
is a lifetime rather than damping noise; it is dropped with a warning that
points to `DYSON`. Weighting the poles by Z alone would break the spectral sum
rule, so it is not offered. All other checks are those of the COHSEX route:
diagonal states, full band/k/spin coverage, the KS reference energies and an
open QP gap. A Green-function `ndb.G` is rejected;
use the Newton `ndb.QP`. `ChiGMode="COHSEX"` keeps its strict checks and still
rejects a non-COHSEX database.

For G0W0 energies, run PPA GW with the Newton solver and a `QPkrange` that
covers every response band and k point, then set:

```text
ChiGMode= "QP"
ChiGDb= "gw/ndb.QP"
```

The `ndb.Chi` header records `QP poles from G0W0 (PPA/MPA)` (or the actual
self-energy kind) as its QP action.

For an optical Casida calculation, create a matrix BSE input from the same
`SAVE`, set `BSKmod="CHI"`, and set `BSENGfxc` to the exact G-basis size stored
in `ndb.Chi`. Point the BSE run at the response job database and use KS
transition energies (`KfnQPdb` off). Set `BSEQptR` to q index 1,
`QShiftOrder=1`, and `BLongDir` to the response `LongDrXs` direction. The
loader checks the direction,
normalization, G basis, and zero-frequency Hermiticity before projection.
The BSE path currently requires one CPU rank, scalar or spinor (SOC) states
without collinear spin polarization, and a matrix solver. Its static Casida matrix uses only the first (zero)
frequency of the exported kernel. Start by comparing `G0` against the
ordinary KS calculation; then compare the `COHSEX` result. With coupling,
the BSE bands and the G basis of the `ndb.Chi`, the static Casida response
reproduces the `ndb.Chi` response at zero frequency exactly (checked on
monolayer WS2). Use a new BSE job when changing the response database or
optical direction; Chi kernel restarts are rejected because the old kernel
cannot be verified against the new inputs.

### Frequency-dependent Casida: `BSEChiDyn`

A static kernel reproduces the zero-frequency response but cannot move the
absorption peaks: the Casida poles stay close to the KS transitions and only
their weights change. `BSEChiDyn` uses the whole real-axis kernel instead.
With `BSKmod="CHI"`, `K_kernel` keeps fxc out of the BSE matrix and stores the
transition vertices; `K_Chi_dynamic` then solves, at every BSE frequency
z = w + i eta,

```
Resp(w)  = -Co B^T [ z - H0 - Kf(z) ]^-1 A
Kf_xy(z) = spin_occ/(Nk V) diag(sqrt f) V_x^H fxc(z) V_y diag(sqrt f)
```

`H0` is the ordinary BSE matrix (KS transition energies plus exchange), `V_x`
the vertices of the static projection (excitation vertices, and de-excitation
vertices for the anti-resonant pairs), `A` and `B` the residual vectors of the
diagonalization solver. All blocks see the same retarded fxc(z). Since
fxc = chi0^-1 - P^-1, the Woodbury identity makes the coupled response equal
to P plus local fields whenever the BSE transitions build the same chi0 as the
`ndb.Chi`: same bands, k grid, G basis, frequencies and damping. For a
z-independent Hermitian fxc it equals the diagonalization result.

```text
BSKmod= "CHI"
BSEChiDyn
BSEmod= "coupling"
BSENGexx= <G-basis size of ndb.Chi>  RL
BSENGfxc= <G-basis size of ndb.Chi>  RL
% BEnRange
 <first Re w> | <last Re w> | eV       # the ndb.Chi grid
%
BEnSteps= <number of ndb.Chi frequencies>
% BDmRange
 <eta> | <eta> | eV
%
BSEprop= "abs"
```

The `ndb.Chi` must be a real-axis, retarded export (`ChiGAxis="REAL"` in the G
modes, which force retarded ordering; `GrFnTpXd="R"` in LEVELS mode). fxc is
not interpolated in frequency: the BSE grid must be the `ndb.Chi` grid, and the
error message prints the `BEnRange`, `BEnSteps` and `BDmRange` to use. The
solver replaces the requested `BSSmod` and writes `o-<job>.eps_q1_chidyn_bse`
and `o-<job>.eel_q1_chidyn_bse`. Each frequency costs one dense LU solve of
size N (TDA, twice) or 2N (coupling); frequencies are distributed over OpenMP
threads, each holding about three (2N)^2 complex(8) matrices with coupling.
The restrictions of the static path apply (one MPI rank, scalar or spinor
states, no collinear spin polarization), plus the length gauge and absorption only.

Three conditions matter in practice:

- **Use coupling.** fxc(z) inverts the full resonant plus anti-resonant
  bubbles. In TDA their mixing is dropped and the KS poles are not cancelled.
  In silicon, TDA makes the main peak 1.9 times too high and eps1(0) 13%
  too large.
  The solver warns when TDA is used.
- **The BSE band window must not split a degenerate multiplet.** Yambo warns
  `User bands ... break level degeneracy`. A split multiplet makes the
  transition basis depend on the wavefunction gauge. The zero-frequency
  response hardly notices, but near resonances the dynamic identity fails. In
  silicon, bands 3-6 (split Gamma multiplets) give up to 25% error at the
  peaks, whereas bands 1-8 agree to 3e-4.
- **More bands in `ndb.Chi` than in the BSE break the identity.** fxc(z) is the
  inverse difference of the full bubbles, while the BSE window contains only
  part of chi0. The exact reference is an `ndb.Chi` built with the BSE bands;
  a wider `ndb.Chi` gives an approximation whose error has to be converged.

Validation on bulk silicon (LDA Troullier-Martins pseudopotential, 4x4x4
Gamma-centred grid with 64 BZ points, symmorphic operations, 15 G vectors,
LEVELS mode with a 1 eV scissor on P, retarded ordering, 81 real frequencies
0-8 eV, eta = 0.1 eV, bands 1-8):

| Run | Reference | Max. relative difference |
| --- | --- | --- |
| Hartree BSE, coupling | KS RPA+LF of `ndb.Chi` | 5.6e-4 |
| `BSEChiDyn`, coupling | QP RPA+LF of `ndb.Chi` | 3.0e-4 |
| IP column of the same run | KS IP of `ndb.Chi` | 1.1e-4 |

The dynamic kernel moves the main eps2 peak from the KS position (4.0 eV) to
the QP position (5.0 eV) with the QP height; the static kernel cannot.

### Kernel from the BSE and exciton binding energies: `BSEChiOut`

A P built from G0W0 Green functions is a QP bubble: it has no electron-hole
attraction, so Casida with its fxc opens the gap but binds no exciton (in
monolayer WS2 the static onset stays at the KS gap). The attraction is the W
term of the BSE. `BSEChiOut` takes P from a coupled BSE instead. With the BSE
matrix M (transition energies, exchange vbar, and W when `BSKmod="SEX"`) and
the Chi transition vertices collected on the fly, `K_Chi_export` computes at
every frequency

```
chib(z) = Co/(4 pi) B^T [ z - M ]^-1 A        (G-resolved BSE response)
P^-1    = chib^-1 + vbar
"exc" : fxc = Pref^-1  - P^-1,  Pref = bubble of the BSE transition energies
"full": fxc = chi0^-1  - P^-1,  chi0 = KS bubble (Yambo's bare energies)
"mdr" : as "exc", with P of the Marini-Del Sole-Rubio approximation (below)
```

and writes `ndb.Chi` (chi0 column = Pref or chi0, P, fxc) into the BSE job
directory. By the Woodbury identity, Casida with the same reference energies,
the same vbar and this fxc(z) reproduces the BSE response exactly, its exciton
poles included. The binding energy of the frequency-dependent Casida is
therefore the BSE binding energy. The first frequency of the file is z = 0
exactly (static Casida); the remaining ones are the BSE grid (`BSEChiDyn`).

```text
BSKmod= "SEX"
BSEmod= "coupling"
BSSmod= "d"
BSEChiOut= "exc"                 # or "full"
BSENGexx= <n> RL                 # vbar and the Chi basis must coincide
BSENGfxc= <n> RL
BSENGBlk= <W block>
KfnQPdb= "E < <gw job>/ndb.QP"   # QP transition energies
```

Casida then uses a copy of the exported `ndb.Chi` (without the BSE kernel
database, which would be taken as a restart) with `BSKmod="CHI"`, coupling,
the same bands, `BSENGexx` and `BSENGfxc`, and:

- "exc": the same `KfnQPdb` (the kernel is measured from the QP bubble; the
  run stops if the QP corrections are missing);
- "full": KS energies (the kernel carries the QP shift too; the run stops if QP
  corrections are applied).

Add `BSEChiDyn` and the BSE grid for the dynamic solver; without it, the static
diagonalization uses fxc(0).

Every diagonalization now reports its lowest excitations, their relative
optical strength |R_left R_right|, the lowest bright one (strength above 1% of
the maximum), the lowest transition of the window and the binding energy
`gap - lowest bright excitation` (in the `r-` file, `[BSE]` lines). With QP
transitions the gap is the QP gap on the k grid. For a "full" kernel the
transitions are KS ones; take the gap from the exporting BSE.

With partly filled bands (`nbf < nbm`, e.g. a doped layer) the window also holds the
transitions out of those bands, so its lowest transition is not a gap and the lines above
give a meaningless binding energy. The report then adds interband lines: the gap is the
lowest transition whose lower band is full (band <= nbf: the band edge, Pauli-blocked
transitions included), an excitation is interband when more than half of its eigenvector
weight lies on such transitions, and the binding energy is `interband gap - lowest bright
interband excitation`. These lines need the eigenvectors of the run and are skipped when the
diagonalization is read back from `ndb.BS_diago`. The lines above are unchanged.

```text
[BSE] partly filled bands, first and last
[BSE] interband gap, lowest transition from a full band [eV]
[BSE] lowest interband excitations (weight > 50% on full-band transitions) [eV]
[BSE] interband excitations, relative optical strength
[BSE] lowest bright interband excitation (strength > 1%) [eV]
[BSE] binding energy, interband gap - lowest bright interband excitation [eV]
```

Doped WS2 test of the next section: window gap 0.033 eV (bands 27 -> 28 at K) and binding
-0.84 eV; interband gap 1.600 eV, lowest bright interband excitation 0.875 eV, binding
0.726 eV, the same with coupling and in TDA. The intra-band excitations at 0.05 and 0.22 eV
are not counted.

### Partly filled bands (doped layers)

The `BSEChiOut` export and the Casida paths accept partial occupations; the `ChiGMode` routes
still reject a metallic reference. Yambo builds the transitions as for any metal
(`K_Transition_check.F`): f_t = (f_v - f_c)/spin_occ; a partly filled band is both valence and
conduction; a transition out of a state holding less than 1e-5 electrons is dropped; f < 0
goes to the anti-resonant block; every E = 0 transition is dropped. At q = 0 the intraband
(Drude) response is therefore absent (Yambo warns `The system is a metal but Drude term not
included`, in the screening and in the BSE). The doped electrons enter through Pauli blocking
and, at finite q, through transitions inside the partly filled bands.

Test: monolayer WS2 with SOC, 6x6 k grid, 50 Ry, `tot_charge = -0.02` in QE (n = 2.3e13 cm^-2;
QE spreads the compensating charge over the cell), bands 23-30, KS energies, SEX, coupling,
`BSEChiOut="full"`, then `BSKmod="CHI"` with `BSEChiDyn`. At 300 K the Fermi level lies 45 meV
below the K conduction minimum: f = 0.150 (K, band 27), 0.047 (K, band 28), 0.055 (Q, band 27).
Maximum relative difference of eps, Casida with fxc(w) against the BSE:

| Case | Im eps | Re eps |
| --- | --- | --- |
| q1, 1 G | 3.2e-5 | 6.3e-5 |
| q1, 21 G with `BSEGinplane` | 5.8e-5 | 1.0e-4 |
| q2, 1 G, explicit anti-resonant rows | 2.3e-5 | 3.0e-5 |
| q1, 1 G, export and Casida 0.1 K apart | 2.5e-4 | 3.3e-4 |

At q2 the rows include 10 transitions inside bands 27/28 (k-q -> k, f = 0.05-0.15) and their
anti-resonant partners with f < 0.

Temperature. `p2y` gives Yambo 300 K whenever the DFT run used smearing
(`interfaces/p2y/mod_p2y.F:298`); `ElecTemp` overrides it. The identity needs the same
occupations in the exporting BSE and in the Casida run (last row of the table). A job that
reads another job's databases takes the temperature stored in them (`src/io/io_header.F`), and
the Fermi-level search may then add a 0.1 K step (`OCCUPATIONS_Fermi.F`), so two jobs of a
chain can differ by 0.1 K. Compare the `[X] Electronic Temperature` lines of the two `r-` files.
The `BSE bubble vs ndb.Chi chi0` check sees the mismatch (9e-6 against 4e-8) but warns only
above 1e-3. A negative `ElecTemp` keeps the temperature fixed but also freezes the full and
partly filled bands at their T = 0 values (`src/setup/setup.F`); here that dropped the four
transitions out of the thermally occupied band 28.

The export therefore stores the occupations of its BSE levels in `ndb.Chi` (BSE bands, every
IBZ k, the full and last occupied bands, the temperature), and the Casida run compares them
with its own before the projection and the solver:

```text
[Chi/BSK] occupations vs the exporting BSE: max |df|, its and this temperature [K]
```

It stops when max |df| exceeds 1e-6 or the full / last occupied bands differ. Doped test: a
Casida run 0.1 K warmer than its export (|df| = 7e-5) stops after 1 s; one at the same
temperature (|df| = 0) runs and matches the BSE to 3e-5. In an insulator a 0.1 K difference
leaves |df| = 0 (undoped WS2: passes, 5e-5 from the BSE). An `ndb.Chi` exported before this check
has no occupations: the run warns and goes on. Yambo returns exit status 0 after an `[ERROR]`,
so a script must look for the output files.

Restrictions: one MPI rank (OpenMP threads parallelize the frequencies), coupling with
the diagonalization solver, length gauge, scalar or spinor states (see "Spinor
states" below; no collinear spin polarization, magnons or perturbative SOC), no
transition widths or Z factors, `BSENGexx = BSENGfxc`.
The export stops if the reference bubble or the BSE response is
ill-conditioned (`[Chi/BSK] minimum rcond` below `ChiRcondMin`); reduce the
G basis then. Monolayer WS2 (bands 11-16, 9 G, no Coulomb cutoff) passes with
rcond 2e-9: the eight pure-G_z plane waves are nearly dependent, the eigenvalues of
D chi0 D span 1e-8 to 2.4, and the body of fxc reaches 3e6 in units of v. The head
and the optical response are exact (the exported P gives the BSE eps to 3e-7, the
8-digit precision of the text output), but for the kernel itself a smaller basis is
cleaner. `plot_summary.py --chi` prints this diagnostic. The same run with one plane
wave (`BSENGexx = BSENGfxc = 1`) gives the same BSE (2.2342 eV, binding 0.309 eV), the
same spectra (5e-7) and the same head alpha(w) (4e-6), with rcond = 1. Its 13->14 fxc
block is then exactly rank one, F_tt' = -c alpha(z) d_t conj(d_t') with d_t the optical
dipoles: for in-plane optics the exact kernel is a frequency-dependent LRC kernel. Each frequency costs one LU solve of size 2N. The Coulomb factor
is D = sqrt(4 pi)/bare_qpg as a complex number, the form the BSE exchange
(1/bare_qpg**2) uses, so the identity also holds with a Coulomb cutoff where
v_cut(q+G) < 0 for some G.

`BSEChiDyn` also compares the bubble of the BSE transitions with the `ndb.Chi`
chi0 at its first frequency (`[Chi/BSK] BSE bubble vs ndb.Chi chi0`, in the
Coulomb-symmetrized basis) and warns above 1e-3: the kernel is then an
approximation, not the exact one (bands, energies or k grid differ). Silicon:
3e-8 with an exported kernel, 4e-6 with a native `ndb.Chi` of the same bands,
0.15 (warning) for bands 3-6 against a bands 1-8 `ndb.Chi`.

### The Casida matrix as text: `BSEChiKoutW`, `BSEChiKoutB`

With `BSEChiDyn`, the matrix the dynamic solver inverts can be written at the
`ndb.Chi` frequencies whose real part lies in a window:

```text
% BSEChiKoutW
 0.0 | 0.0 | eV        # static point z=0 (and z=0+i eta) of an exported ndb.Chi
%
% BSEChiKoutB
 13 | 14 |             # one valence | conduction pair; 0 | 0 = all transitions
%
```

`o-<job>.Ktt_q1_transitions` lists each transition t (index T in the BSE, BZ and
IBZ k, k in reduced units, v, c, energy E_t in eV, occupation factor f_t and
residual a_t = d_t sqrt(f_t)). `o-<job>.Ktt_q1_w<iw>` holds, per pair t t',
the blocks in eV

```
M(z) = [[ diag(E) + X_rr + F_rr ,           X_rc + F_rc ],
        [           X_cr + F_cr , -diag(E) + X_cc + F_cc ]]
```

X is the static part of the BSE matrix without the energies (exchange), F the
fxc(z) part as it enters M (F_rr = Kf_rr, F_rc = i Kf_rc, F_cr = i Kf_cr,
F_cc = -Kf_cc). The response is Resp(z) = -Co B^T [z - M(z)]^-1 A with
A = (a, i b), B = (b, i a), b = conj(a). In TDA only the rr columns are
written. With a band pair, the file holds those rows and columns of the full
matrix, not a separate Casida problem. Silicon check: the eigenvalues of M(0)
rebuilt from the files match the static Casida run to 4e-5 eV (single-precision
matrix, 8 printed digits); the response at z = 0.1i eV matches the dynamic
solver to 1e-6.

`sbin/chi_tools/ktt_casida.py` reads these files, builds M(z), and prints its
lowest eigenvalues with their oscillator strengths, the lowest bright one and
its binding energy; `--tda`, `--no-fxc` and `--spectrum` are optional:

```sh
python sbin/chi_tools/ktt_casida.py o-<job>.Ktt_q1_transitions o-<job>.Ktt_q1_w<iw>
```

At finite q (explicit anti-resonant transitions, see "Finite momentum") the files are
`o-<job>.Ktt_q<iq>_*` and row N+t of M is anti-resonant transition t, with its own k,
energy and oscillator, listed in `o-<job>.Ktt_q<iq>_antiresonant` (electron in v at k,
hole in c at k-q; E_t < 0, f_t < 0, A_t and B_t). The lower-right block is then
diag(E_ares) + X_cc + F_cc, every F block is c s_x s_y V_x^H fxc(z) V_y with
s = sqrt(f) (i sqrt|f| for f < 0), and A = s d, B = s conj(d) row by row. In the
resonant file at finite q, t is v at k-q -> c at k (k of the conduction state).
`ktt_casida.py` and `sc_scan.py` read the anti-resonant file when it is there. hBN,
q = (0, 1/6, 0), 144 + 144 rows, six frequencies: B^T [z-M]^-1 A rebuilt from the files
is a constant multiple of the Casida eps of the same run to 2e-7 (6e-7 at q = 1).
`BSEChiKoutW= 0.0 | 0.0 eV` writes F at z = 0, the static kernel.

The kernel is exact only on the whole transition space. A single band pair
loses the binding: silicon at z = 3.4 + 0.1i eV gives 3.452 eV with all
transitions (BSE exciton 3.428 eV) but 3.589 eV with the 4->5 pair alone.

A single matrix is not a binding energy: with the exact kernel the exciton
solves lambda(w) = w, where lambda is the lowest bright eigenvalue of M(w). A
kernel exported with `BDmRange 0 | 0` sits on the undamped real axis (accepted by
`BSEChiDyn` for BSE-exported kernels only). Silicon, full matrix on 3.00-3.50 eV:
lambda falls from 3.486 eV and crosses w at 3.4276 eV (BSE 3.4278 eV); at w = 0
it is 3.558 eV. A damped z = w + i eta, or one band pair, misses the root.

`sbin/chi_tools/sc_scan.py` automates this. It diagonalizes each w-file,
records lambda(w), and interpolates the crossing with a polynomial (degree up
to 3) through the neighbouring grid points. At a pole of fxc(w) the attractive
kernel drives lambda towards -infinity and it comes back from above: a sign
change where lambda rises faster than 10 w is flagged as such a jump, not a
solution. A steep fall is a crossing: just below the WS2 exciton
d lambda / d w is about -12. `--delete` removes each file once it is read,
`--append` merges the batches of a scan into one table, and `--table`
re-analyses a finished table:

```sh
python sbin/chi_tools/sc_scan.py o-<job>.Ktt_q1_transitions o-<job>.Ktt_q1_w* \
       --out sc_scan.dat --exciton <BSE energy> [--delete] [--append]
python sbin/chi_tools/sc_scan.py --table sc_scan.dat --exciton <BSE energy>
```

Silicon, 7 points on 3.30-3.48 eV in three batches: the crossing is at
3.4272 eV. The finer 0.02 eV grid gives 3.4276 eV. The BSE value is 3.4278 eV.

Monolayer WS2 (bands 11-16, 2025 transitions, 9 G, coupling): lambda(w) is
2.504 eV at w = 2.0 eV, only 0.04 eV below the 2.543 eV gap, and falls
steeply as alpha(w) grows towards the exciton.

| Grid | Step | lambda_bright(w) = w | BSE | Difference |
| --- | --- | --- | --- | --- |
| 2.00-2.48 eV, 17 points | 30 meV | 2.2320 eV | 2.234196 eV | -2.2 meV |
| 2.221-2.245 eV, 13 points | 2 meV | 2.234196 eV | 2.234196 eV | < 1 ueV |

On the real axis alpha(w) is real (Im ~ 1e-17) and has a pole near 2.29 eV,
where P(w) passes through zero between the exciton (2.234 eV) and the next
bright state (2.604 eV).

`sbin/chi_tools/plot_summary.py` draws the figures of the chain. Each figure
is made only when its inputs are given:

| Figure | Contents |
| --- | --- |
| `energies.png` | GW corrections of the states near the gap; the vb->cb gap at every k, KS and QP, with the exciton and static-Casida levels |
| `spectra.png` | eps2 for the QP transitions, the static-fxc Casida and the fxc(w) Casida |
| `fxc_omega.png` | ndb.Chi kernel in units of v: head alpha(w), one diagonal body element per shell, and the matrix at w = 0 |
| `fkq.png` | F part of K_tt' as a matrix; its diagonal next to the exchange X_tt; the diagonal over the zone (`--hex` for hexagonal lattices) |
| `selfconsistency.png` | lambda(w) and the line lambda = w |

```sh
python sbin/chi_tools/plot_summary.py --qp o-g0w0.qp --vb 13 \
   --eps "QP transitions=o-cas_st_exc.eps_q1_diago_bse:ip" \
   --eps "static fxc(0)=o-cas_st_exc.eps_q1_diago_bse" \
   --eps "fxc(w)=o-cas_dyn_exc.eps_q1_chidyn_bse" \
   --chi kx_exc --ktt o-cas_dyn_exc.Ktt_q1_transitions o-cas_dyn_exc.Ktt_q1_w225 \
   --scan sc_scan.dat --bse-report r-bse_sex_exc_* --static-report r-cas_st_exc_*
```

The `fxc_omega.png` figure needs netCDF4 (the script reads `ndb.Chi` itself).

`sbin/chi_tools/fxc_export.py` writes the kernel of an `ndb.Chi` as plain text and
`.npz` for use in other codes: q, the G vectors with |q+G|, and fxc_GG'(q, w) for every
q fragment present (`--static`: w = 0 only; `--sym`: also fxc |q+G||q+G'|/4pi, whose head
is -alpha/4pi; `--inplane`: only the G with G_z = 0). A `BSEChiOut` export holds one
fragment per BSE momentum (see "Finite momentum" below):

```sh
python sbin/chi_tools/fxc_export.py kx_exc --out fxc_GGq --static --sym [--inplane]
```

Silicon, same setup as above (bands 1-8, 15 G, 1 eV scissor through
`KfnQP_E`), static screening with 12 bands, `BSKmod="SEX"`, `BSENGBlk= 15 RL`:

| Run | Lowest bright exciton | Binding energy | Spectrum vs BSE |
| --- | --- | --- | --- |
| SEX BSE, coupling (reference) | 3.428 eV | 0.173 eV | - |
| exported P, QP RPA+LF (`chi_spectrum.py`) | - | - | 1.4e-4 |
| Casida, "exc", fxc(z), `BSEChiDyn` | peak 3.40 eV | = BSE | 1.4e-4 |
| Casida, "full", fxc(z), `BSEChiDyn` | peak 3.40 eV | = BSE | 1.4e-4 |
| Casida, "exc", static fxc(0), QP energies | 3.558 eV | 0.043 eV | 0.71 |
| Casida, "full", static fxc(0), KS energies | 2.611 eV | none (onset at KS gap) | 0.92 |

The window gap is 3.601 eV (2.601 eV KS gap plus the scissor). Both static
kernels reproduce eps1(0) (19.84) but not the exciton: the frequency
dependence of the exact kernel carries the binding. The spectra were compared
on the 0.1 eV grid with eta = 0.1 eV. In this build, gfortran 13 at -O3
miscompiled `K_correlation_collisions_std` (the SEX kernel crashed without
the export and gave NaN with it); the file compiled at -O1 runs correctly.

### Finite momentum: `BSEChiOut` with `BSEQptR`

The export runs at every BSE momentum of `BSEQptR` and writes one `ndb.Chi` with
one fragment per q. At finite q the transitions are v(k-q) -> c(k), with
E = E_c(k) - E_v(k-q), and the vertices rho_cv(k, q+G) come from the
wavefunctions, not the dipoles. The exchange follows `Lkind`. By default (`Lkind="bar"`) it is vbar
(no G=0), so P^-1 = chib^-1 + vbar. With `Lkind="full"` (or `"default"` with a
Coulomb cutoff at q > 1) it is the full v (G=0 included), so P^-1 = chib^-1 + v.
The `r-` file says which (`[Chi/BSK] exchange with/without G=0`).

The header (G vectors, optical q0) is written by the first q of the run. Start
`BSEQptR` at 1 so the file keeps the q0 direction of the optical point. At every q
the head of the exported response chib_00(z) is compared, at nine frequencies, with
Yambo's own response of the same BSE matrix (the one behind `o-*.eps_q<iq>*`). The
comparison fits one complex scale, then reports the residual:

```text
[Chi/BSK] head of the exported response vs the BSE response: residual, |scale/c|, arg(scale)
```

The residual should be at round-off and |scale/c| = 1; the run warns above 1e-4.
Casida with the exported kernel at a finite q uses `BSKmod="CHI"` and
`BSEQptR q | q`, and reads fragment q of the same `ndb.Chi`.
`fxc_export.py` writes all fragments.

#### Anti-resonant transitions at finite q

Stock Yambo derives the anti-resonant blocks of a coupled BSE from the resonant
ones when the system has time reversal or space inversion. This shortcut is exact
only at q = 0. Its coupling block pairs rho_t(q+G) with rho_t'(q-G) and v(q+G). It is
computed for t <= t' and mirrored into a symmetric matrix, which holds only when
v(q+G) = v(q-G). The explicit construction (each anti-resonant transition with its
own bands, energy, occupation and oscillator; `ImposeAsym` in stock Yambo) uses the
general kernel for every block.

An exchange-only BSE (`BSKmod="Hartree"`) must give the G-space RPA with local fields
of `X_redux` (same bands, G set, scissor and broadening). Monolayer hBN, 6x6 k grid,
bands 3-6, maximum relative deviation of eps:

| G set | q | Derived (stock) | Explicit |
| --- | --- | --- | --- |
| 9 RL (z-only G) | (0, 1/6, 0) | 4e-4 | 3e-4 |
| 17 RL (in-plane star) | (0, 1/6, 0) | 3.1e-2 | 4e-4 |
| 29 RL | (0, 1/6, 0) | 7.2e-2 | 1.3e-4 |
| 29 RL | optical | 6e-5 | 1.6e-2 |

The independent-particle columns agree to 4e-7 in every case. In this fork
`K_driver_init` therefore builds the anti-resonant transitions explicitly at
finite q whenever the BSE has coupling (`[BSK] finite q with coupling: explicit
anti-resonant transitions` in the `r-` file). At q = 0 the derived blocks are kept;
the explicit path is off there (its optical-limit oscillators), and `BSEChiOut`
refuses `ImposeAsym` at the optical q. The explicit BSE is about twice the kernel
work of the derived one.

The export and the dynamic Casida then run over all 2N rows of the BSE matrix
(`TDDFT_Chi_full_*`). Every row A has its vertex V_A (mode "R" on its own
transition), s_A = sqrt(f_A) (i sqrt|f_A| for the anti-resonant rows, as in
`K_kernel`), energy E_A and oscillator d_A from `BSS_eh_f`, `BSS_eh_E` and
`BSS_dipoles_opt`:

```
chib(G,G') = c sum_AB s_A V_A(G) [z - M]^-1_AB s_B conj(V_B(G'))
chi0(G,G') = c sum_A s_A^2 V_A(G) conj(V_A(G'))/(z - E_A)
Casida:     M(z) = H0 + Kf(z),  Kf_AB = c s_A s_B V_A^H fxc(z) V_B
```

With derived anti-resonant rows (q = 0), V = (V_r, V_c), s = (s, i s),
E = (E, -E) and d = (d, conj d), and these are the earlier pair formulas. hBN with
the stock (derived) blocks at q = (0, 1/6, 0): exported head vs BSE 2e-2, Casida
vs BSE 2.6e-2. With explicit rows: head below 1e-15 at q = 2, 3, 4; Casida vs BSE 6e-5 to
1e-4 with 17 G (rcond 3e-5) and 2e-5 to 5e-5 with one G, the level of the optical
q = 1 (single-precision BSE matrix and `ndb.Chi`).

After pulling these changes, rebuild with a fresh dependency list. Yambo writes
`config/stamps_and_lists/global_modules_dep.list` at configure time. A source file
added to the fork after that is missing from the list, and is not recompiled when
a module it uses changes. gfortran resolves module variables by name, but nvfortran
addresses them by offset, so such a stale object reads the wrong variable. This is
the likely cause of a monolayer WS2 Casida run at finite q (nvfortran build) that
stopped with `dynamic solver: one BSE matrix only`, which a gfortran build of the
same source does not reproduce: the count would read the logical declared before
it, which nvfortran stores as -1.

```sh
make clean what=dep   # drop the configure-time dependency list
make clean            # all Yambo objects (external libraries are kept)
make yambo
```

### In-plane G only: `BSEGinplane`

For a layer in the xy plane with a large vacuum, the first G of the basis after G=0
are pure G_z, spaced by 2 pi/c. These nearly dependent plane waves make the
kernel ill-conditioned (WS2, 9 G: rcond 2e-9). `BSEGinplane` keeps the exchange
and the kernel on the in-plane G (G_z = 0):

- `K_exchange_collisions` zeroes the exchange vertex O_x(G) for every G_z != 0
  of `BSENGexx`.
- `K_Chi_export` builds the kernel on the in-plane set S and leaves it zero
  elsewhere.

With the exchange on S only, the BSE response on S obeys
chib_SS = chi0_SS + chi0_SS v_SS chib_SS. Hence the kernel computed on S is
exact for Casida with the same masked exchange.

This is a modelling choice: the local fields of the BSE itself (its exchange
term) come from the in-plane G only. The z-only G no longer contribute. The
screened W term (`BSENGBlk`) is unchanged. The Casida run must set
`BSEGinplane` too; `K_kernel` checks it against the `CHI_G_DB` string of the
`ndb.Chi`. CPU build only.

The G vectors are ordered by |G|, so `BSENGexx = BSENGfxc` must reach the first
in-plane star. For WS2 (c of about 38 bohr, |G_z| steps of 0.166 bohr^-1, first
in-plane star at 1.21 bohr^-1), 14 pure G_z lie below the star. 21 RL then hold
G=0 and the six in-plane G. The report prints the count:

```text
[Chi/BSK] BSEGinplane: kernel on the in-plane G of the basis   <ns>   <ng>
```

`fxc_export.py --inplane` lists only these G.

### Exciton energies at every q: `xq_summary.py`

The self-consistent condition lambda(w) = w of `sc_scan.py` holds at every q: the Casida
matrix with fxc(q, w) has the BSE exciton of momentum q as an eigenvalue at w = E_x(q).
A kernel exported on undamped real frequencies (`BDmRange 0 | 0`) below the lowest gap
of every q gives, per q:

1. the BSE excitons (lowest, lowest bright) and the gap of the window, from the report of
   the exporting run;
2. Casida at z = 0 (`BSEChiKoutW 0 | 0`, `ktt_casida.py`): the static kernel fxc(q, 0);
3. Casida on the frequencies within a few 10 meV of the BSE excitons (`sc_scan.py`): the
   crossing of the lowest bright eigenvalue, and of the lowest one, with w.

`sbin/chi_tools/xq_summary.py` collects them (`sc_q<iq>.dat`, `static_q<iq>.log`, the `r-`
report, |q| from a `fxc_export.py` npz). It writes a table (gap, BSE, self-consistent and static
Casida energies, binding energies gap - E), a plot, and the table "iq w*" of the
self-consistent energies. `fxc_export.py --at` takes that table and writes fxc_GG'(q, w*),
interpolated linearly between the two neighbouring real frequencies. F is linear in fxc, so
static Casida with this kernel has the exciton as an eigenvalue up to the interpolation error.

```sh
python sbin/chi_tools/xq_summary.py --bse 'r-<bse job>_*' --npz <static npz> --out xq_dispersion
python sbin/chi_tools/fxc_export.py <kernel dir> --at xq_dispersion_omega.dat --sym --inplane --out fxc_GGq_scw
```

hBN (bands 3-6, 17 G in-plane, q = 1 and q = (0, 1/6, 0), grid 3.55-4.45 eV in 0.01 eV
steps, scan +-0.04 eV around the BSE exciton):

| q | gap | BSE | Casida fxc(q, w), self-consistent | Casida fxc(q, 0) |
| --- | --- | --- | --- | --- |
| 1 | 5.6967 eV | 3.737126 eV | 3.737129 eV | 5.0354 eV |
| 2 | 6.5599 eV | 4.293060 eV | 4.293079 eV | 5.7308 eV |

Static Casida with fxc(q2, 4.293079 eV) from `--at`: lowest bright eigenvalue 4.293057 eV.
The static kernel fxc(q, 0) binds the exciton by 0.66-0.83 eV, against 1.96-2.27 eV: the
binding comes from the frequency dependence of the kernel, at finite q as at q = 0.

### Spinor states (noncollinear, spin-orbit coupling)

The export and the Casida paths accept spinor states (`noncolin`, with or without
`lspinorb`). Nothing in the formulas changes: the vertex of a transition is its density
rho_cv(q+G) = sum over the two spinor components (`scatter_Bamp`), the occupation factors
f_t = (f_v - f_c)/spin_occ and the constant Co carry spin_occ = 1 instead of 2, and each
spinor band holds one electron, so a band window doubles (WS2 bands 11-16 become 21-32)
and the number of transitions quadruples. Collinear spin polarization (`nspin=2`), magnons
and perturbative SOC are still refused.

Monolayer WS2 with spin-orbit coupling (`noncolin=.true., lspinorb=.true.`, fully
relativistic PseudoDojo norm-conserving W and S, W with 5s5p semicore, a = 3.184 A,
12 A cell, 50 Ry, 6x6 k grid, bands 23-30 = four valence and four conduction spinor
bands, 21 G of which 7 in-plane, scissor 0.8 eV). The valence splitting at K is 0.427 eV.

| k-point symmetry | head vs BSE | Casida vs BSE q1, q2 | dark, A, B at q1 [eV] | BSE eps2 vs the other |
| --- | --- | --- | --- | --- |
| 24 (spatial + time reversal), 7 IBZ k | 1e-15 | 9e-5, 2e-4 | 1.915244, 1.948497, 2.340670 | 4e-4 (q1), 2e-3 (q2) |
| none (`nosym`, `noinv`), 36 k | 2e-15 | 3e-4, 2e-4 | 1.915244, 1.948488, 2.340672 | |

The excitons agree to 0.01 meV at q1 and 0.3 meV at q2: with SOC, Yambo's spinor BSE
is the same with and without k-point symmetries, and the dark state (33 meV below A) and
the B exciton (0.39 eV above A) come out as expected. Each state is a near-degenerate pair
(the two valleys); how the strength splits within a pair depends on the basis and differs
between the runs, the spectra do not. Use the symmetries.

The exported P can also be checked against an independent BSE. P is the response to the
total macroscopic potential, i.e. the BSE with the exchange vbar switched off (direct W
ladder only), whose eps is 1 - (4 pi/|q|^2) P_00. The same input with `BSENGexx= 1 RL`
(only G=0, which vbar drops) and no `BSEChiOut` gives that BSE. Fitting eps - 1 = K P_00
over the 201 frequencies of the grid gives K = -4 pi/|q|^2 (q1: -1.2566e11, the optical
q0 = 1e-5; q2: -311.19, |q| = 0.2010 bohr^-1 = |b1|/6), real to 5e-5, with a residual of
1e-4 (q1) and 7e-5 (q2): the exported P is the exchange-free BSE response.

One case does break with symmetries: noncollinear states **without** SOC
(`lspinorb=.false.`). Every band is then exactly doubly degenerate and QE fixes the spin
direction inside each pair arbitrarily, differently at each k. In monolayer hBN (scalar
pseudopotentials, bands 3-6 become 5-12, 17 G) the kernel still reproduces the BSE in
every case (head 1e-15, Casida 3e-5 to 1e-4 at q1 and q2), but the BSE itself is right
only on the full grid:

| k-point symmetry | spinor BSE at q1 |
| --- | --- |
| 24 (spatial + time reversal) | six states 3.708-3.773 eV, strengths 0.06-1 |
| 12 (spatial, `noinv`) | 3.713-3.775 eV, triplet not dark |
| none (`nosym`, `noinv`) | dark triplet 3.7126 eV (strength 1e-9), bright singlet 3.7341 eV |

On the full grid the bright state matches the scalar run on the same grid to 0.1 meV
(3.7340 eV at q1, 4.2928 eV at q2). The exchange-only BSE and the RPA response are clean
with symmetries; only the screened direct term mixes singlet and triplet. This needs
noncollinear states without SOC, which have no use here (run those systems scalar).

### Yambo's own TDDFT kernels on the same system

The WS2 SOC test above (q1, bands 23-30, scissor 0.8 eV, 21 G, eta = 0.05 eV) with the
kernels Yambo already has, in the G-space Dyson equation (`optics chi`, `Chimod`) or in
transition space (`BSKmod`). The A peak is the first maximum of eps2 (5 meV grid, parabola);
the gap of the window is 2.3618 eV.

| Kernel | A peak [eV] | Binding [eV] |
| --- | --- | --- |
| BSE (`SEX`, coupling), reference | 1.9486 | 0.413 |
| exact kernel fxc(w), `CHI` + `BSEChiDyn` | 1.9483 | 0.413 |
| exact kernel fxc(0), `CHI` static | 2.3157 | 0.046 |
| IP, RPA (`Hartree`), ALDA in transition space | 2.392-2.397 | none |
| LRC, alpha = 0.686 (head of the exact fxc(0)) | 2.339 | 0.022 |
| LRC, alpha = 1.35 (head of the exact fxc at E_A) | 2.230 | 0.132 |
| LRC, alpha = 2.2 (fitted; eps2 peak 119 vs 19 of the BSE) | 1.927 | 0.435 |
| PF `EMP` (Botti, alpha from eps_M: 0.638) | 2.345 | 0.017 |
| PF `CUR` (0.524), `RBO` (RPA bootstrap, 0.518) | 2.357 | 0.005 |
| PF `JGM` (alpha = 7.06) | no bound peak | - |
| MDR kernel fxc(w), `BSEChiOut= "mdr"` + `BSEChiDyn` | 1.9580 | 0.404 |
| MDR kernel fxc(0), static | 2.3169 | 0.045 |
| `BSfxc` | not available (MDR above replaces it) | - |

- Yambo's LRC kernel is fxc_00 = +`LRC_alpha`/|q|^2 (`X_redux`: 1 - chi0 (v + fxc)), so an
  attractive (excitonic) LRC needs a negative `LRC_alpha`. The alpha of the exported kernel
  (fxc_00 = -alpha/|q|^2, as in `fxc_export.py`) is `LRC_alpha = -alpha`. Between
  alpha = 3 and 4 the static response turns over (Re eps(0) < 0) and the spectrum is unphysical.
- A head-only kernel with the exact head at E_A binds 0.13 of the 0.41 eV: the local-field
  (G /= 0) part of the exact kernel carries most of the binding. An LRC alpha fitted to the
  binding energy (about 2.15) puts the peak in place with six times its strength.
- The PF kernels take alpha from the macroscopic eps_M of the supercell, which in a slab
  depends on the vacuum; here they bind 5-17 meV. JGM, built on the density, is dominated
  by the low-density vacuum.
- G-space ALDA depends on the G set through the pure-G_z components that sample the vacuum
  (f_ALDA grows as n^-2/3): with 7, 13 and 27 G the A peak is at 2.392 (= RPA), 2.501 and
  2.042 eV. ALDA in transition space, with the same G, gives RPA (2.392 eV): ALDA binds no
  exciton, and the G-space values are an artifact of the vacuum. `FxcGRLc` smaller than
  `NGsBlkXd` stops the run in this build; use equal values.
- `BSfxc` builds the BSE kernel and stops: its fxc step is commented out in `K_solvers`
  and the matching branch of `X_redux` is empty in this version. It was the
  Marini-Del Sole-Rubio (MDR) kernel, now available as `BSEChiOut= "mdr"` (below).

### The Marini-Del Sole-Rubio kernel: `BSEChiOut= "mdr"`

The BSE-derived kernel of Marini, Del Sole and Rubio (PRL 91, 256402 (2003)), the
construction behind Yambo's legacy `BSfxc`, written by the same export. The exchange is
removed from the coupled BSE matrix (X = c s_A s_B sum_G conj(v_A) vbar_G v_B on the G of
the exchange; the export checks that M - X gives the exact P), and the exchange-free
matrix M_W is split into its diagonal Ed (the e-h attraction of each transition moved
into its energy) and its off-diagonal part K_off, kept to first order:

```
[z - M_W]^-1 ~ g + g K_off g,          g = diag(1/(z - Ed))
p0 = c V g U,   dp = c V g K_off g U   (shifted bubble, first-order term)
P^-1 = p0^-1 - p0^-1 dp p0^-1,         fxc = Pqp^-1 - P^-1
```

The inversions are exact in G space, so the kernel resums the first-order coupling
through the Dyson equation. It is exact when W is diagonal in the transitions and its
error is second order in K_off (`tests/chi_dynamic`). The ndb.Chi is read as an "exc"
kernel (QP energies in the Casida run). Use it as the approximate BSE-derived kernel of
the literature next to the exact one: same input as "exc", `BSEChiOut= "mdr"`.

WS2 SOC test (as above, q1 and q2, BSE grid 0-4 eV, eta = 0.05 eV; exchange check 2e-15):

| q | BSE / exact fxc(w) | MDR fxc(w) | MDR fxc(0) | exact fxc(0) |
| --- | --- | --- | --- | --- |
| q1, A peak (binding) | 1.9483 (0.414) | 1.9580 (0.404) | 2.3169 (0.045) | 2.3157 (0.046) |
| q2, A peak (binding) | 2.5687 (0.453) | 2.5979 (0.423) | 2.9913 (0.030) | |

MDR recovers 98% (q1) and 94% (q2) of the binding with its frequency dependence, and
nothing statically, like the exact kernel: the first-order coupling, resummed in G
space, carries most of the e-h attraction of this BSE.

The diagonal moved into the energies is per transition, Ed_t = E_t + W_tt, with W_tt the W
part of the exchange-free matrix (occupation factors included). Whether this is the
treatment of W_KK in the MDR paper or a variant of it has not been checked against the paper
in this repository (see "Open checks" below).

### The Nanoquanta kernel: `BSEChiOut= "nq"`

A first-order BSE-derived kernel, attributed to Sottile, Olevano and Reining (PRL 91,
056402 (2003)), with all of W, its diagonal included, at first order and no P inverted:

```
fxc_NQ(z) = chi0^-1(z) T2(z) chi0^-1(z)
T2(G,G';z) = c sum_tt' s_t v_t(G) W_tt' s_t' conj(v_t'(G')) / ((E_t - z)(E_t' - z))
```

chi0 is the bubble of the BSE transition energies E_t (QP energies with `KfnQPdb`, as for
"exc"), W_tt' the W part of the exchange-free BSE matrix M_W = M - X (the attraction as it
enters the BSE matrix, sign and occupation factors s_t s_t' included), v_t the Chi vertices
and c = Co/(4 pi). The definition was given for this mode as Eq. 3 of the paper; it has not
been checked against the paper here.

It is the MDR route with the diagonal of W left at first order: `Ed = E` and
`Koff = M_W - diag(E)`. Both modes go through two routines of `TDDFT_Chi_dynamic.F`, which
`K_Chi_export.F` calls: `TDDFT_Chi_exchange_free_split` (M_W, Ed, Koff and the W diagonal Wd)
and `TDDFT_Chi_first_order_kernel` (the kernel on the G set at one frequency). With Ed = E,
g = diag(1/(z - E)), the shifted bubble p0 of
`TDDFT_Chi_mdr_response` is chi0, its dP1 is T2, and `TDDFT_Chi_kernel_mdr(chi0, chi0, T2)`
returns chi0^-1 - chi0^-1 + chi0^-1 T2 chi0^-1 = chi0^-1 T2 chi0^-1 (the same matrix in both
slots, so the first two terms cancel identically). The ndb.Chi P column holds
(chi0^-1 - fxc_NQ)^-1, the response this kernel gives. Rows, the exchange removal and the
reading as an "exc" kernel (QP energies in the Casida run) are those of "mdr": all 2N rows of
the coupled matrix, anti-resonant rows derived at q = 0 and explicit at finite q. MDR and NQ
differ only in W_tt: MDR resums it in the energies, NQ keeps it at first order; with W_tt = 0
they give the same kernel. The export reports:

```text
[Chi/BSK] diagonal of the W part (resonant rows) [eV]: min, mean, max      (also for "mdr")
[Chi/BSK] NQ: bubble with Ed = E vs the reference bubble (max relative difference)
[Chi/BSK] NQ: P of the exchange-free matrix vs the exact P (max relative difference)
```

`tests/chi_dynamic` calls these two routines on a coupled BSE matrix assembled with the rules
of `K_stored_in_a_big_matrix` (diagonal real(K_ii) + E, coupling blocks i C mirrored as -conj),
for derived and for explicit anti-resonant rows, with the exchange on a G subset without G=0
and one +-G pair with an imaginary Coulomb factor (cut Coulomb). It compares them with
references built term by term from the W blocks (relative deviations, derived/explicit rows):

| Check | Result |
| --- | --- |
| split: Wd, Koff and Ed of both modes | 2e-16 / 2e-16 |
| "nq" kernel = chi0^-1 T2 chi0^-1 | 8e-15 / 7e-15 |
| "mdr" kernel = chi0^-1 - p0^-1 + p0^-1 dP1 p0^-1 with Ed = E + Wd | 6e-15 / 5e-15 |
| W without diagonal: "nq" kernel = "mdr" kernel | 5e-15 / 2e-14 |
| W with diagonal: the "mdr" kernel fails the "nq" comparison | 1.6 / 0.925 |
| exact kernel of the BSE with eps W minus eps fxc_NQ: error ratio when eps doubles | 4.01 / 3.98 |

Nine mutations of these routines each fail at least two of these checks: the nq diagonal
dropped, the sign of Wd, Ed = diag(M_W) for "nq", conj(s) or |d|^2 in the exchange, G=0 in the
exchange, chi0 in the p0 slot for "mdr", conj(d) in `TDDFT_Chi_kernel_mdr`, the sign of the
first-order term. Passing p0 instead of chi0 in the "nq" branch passes them, as it must (with
Ed = E, p0 is chi0). Not unit-tested: the remaining lines of `K_Chi_export.F` (frequency loop,
G set, I/O); in a material run they are checked by the head check and the exchange-removal
check, and `[Chi/BSK] NQ: bubble with Ed = E` compares two codings of chi0 (no threshold).

Monolayer WS2 with SOC, local test: 6x6 k grid, 50 Ry, 20 A cell without Coulomb cutoff,
static screening with 60 bands and a 3 Ry block (so W is weakly screened and the binding
large), bands 23-30, scissor 0.8 eV (`KfnQP_E`), coupling, 21 G with `BSEGinplane`
(7 in-plane G), eta = 0.05 eV, 5 meV grid on 1.2-2.8 eV. Window gap 2.3963 eV; diagonal of
the W part -0.788, -0.760, -0.740 eV (min, mean, max). Static: lowest bright excitation of
the static Casida; fxc(w): first eps2 peak of `BSEChiDyn`:

| Kernel | static fxc(0): binding [eV] | fxc(w): A peak [eV] | binding [eV] | peak height |
| --- | --- | --- | --- | --- |
| BSE (reference) | - | 1.6356 | 0.7607 | 11.35 |
| exact ("exc") | 0.1129 | 1.6356 | 0.7607 | 11.35 |
| MDR | 0.1117 | 1.6438 | 0.7526 | 11.27 |
| NQ | 0.1102 | 1.9759 | 0.4204 | 26.99 |

The static limits agree within 3 meV. With fxc(w), MDR keeps 99% of the binding and the
spectrum; NQ keeps 55% and puts most of the weight in one peak at 1.976 eV (height 2.4 times
the BSE A peak; the B peak and the 2.5-2.7 eV structure are gone; no negative eps2 in the
window). On this grid W_tt is close to the binding energy itself, and keeping it at first
order is the difference between the two kernels. The expansion parameter near the exciton is
W_tt/(E_t - E_X), about W_tt/E_b, here close to 1; with all transitions at one energy, or as
many transitions as G vectors, NQ is exact (reasoning, not a test in the tree). The 6x6 mesh and weak screening make W_tt
large; how NQ behaves on converged meshes is not tested here.

Open checks before naming these modes in a paper (the papers could not be opened from this
environment): how the MDR paper treats the diagonal W_KK (a per-transition shift as in "mdr",
a single shift per q, or something else), and the form of the "nanoquanta kernel" in Shi,
Turkowski, Le and Rahman (arXiv:2302.08375; the PRB 107, 155431 reference is unconfirmed here):
static or frequency dependent, G-space or density-matrix (k-space) representation, treatment
of the diagonal.

### MPI for the response

G0, COHSEX and DYSON support k-point distribution on CPUs. For N ranks,
set the response layout explicitly, for example with four ranks:

```text
X_and_IO_CPU="1 1 4 1 1"
X_and_IO_ROLEs="q g k c v"
X_and_IO_nCPU_LinAlg_INV=1
```

```sh
mpirun -np 4 yambo -F chi_cohsex.in -J chi_cohsex
```

Use N no larger than the number of BZ k points. Automatic layout selection
may distribute bands and is therefore not sufficient. Each rank computes
its owned k points; Yambo's native wavefunction partition includes their
k-q partners. The full bubble is summed in double precision across the
response communicator. The density matrices, G data and inversion work
remain replicated, and only the master writes `ndb.Chi`. q, G-vector and
conduction/valence-band distribution are rejected. GPU bubble kernels
remain unimplemented; use a CPU build for the response. Native COHSEX/GW
solver capabilities are unchanged by this response restriction.

### PPA dynamic screening route

### 1. Export retarded GW spectra

Generate a PPA GW input using the Green-function Dyson solver:

```sh
yambo -p p -g g -F gw_g.in -J gw_g
```

Retain your converged screening and GW cutoffs, and set/add the following
entries. These energy bounds and sample counts are illustrative: choose a
range that covers the poles and satellites of every requested state, then
refine until the spectral norm and the response converge.

```text
GRetarded
DysSolver="g"
GEnMode="absolute"
GEnSteps=1001
% GEnRnge
 -40.0 | 40.0 | eV
%
% GDmRnge
 0.2 | 0.2 | eV
%
GDamping=0.0 eV
GreenFTresh=0.0
GTermKind="none"
```

Remove the `GreenF2QP` flag. Do not apply `GfnQPdb` energy shifts. Set
`QPkrange` to cover **every k point, band and spin** that will enter the
response, with diagonal states only. Use the generated input's syntax for
your spin configuration. Set the internal `GbndRnge` independently to the
converged self-energy band range.

Run the edited input:

```sh
yambo -F gw_g.in -J gw_g
```

The database is normally `gw_g/ndb.G`. This step consumes the native PPA
screening databases (`ndb.pp` and associated `ndb.em1s`/fragments) underlying
your W. Keep these companion databases and use the same screening settings
to reuse them. **It does not import the standalone `ndb.W` export.** If the
native screening databases are missing or incompatible, Yambo's usual
screening machinery recalculates them. Do not enable `ChiFxc` in this GW
step; it forces response recalculation and the new bubble path excludes q=0.

The old `ndb.G` convention is not accepted by `ChiGDb`; rebuild it with
`GRetarded`. The new convention is also rejected by the historical
`XfnQPdb` Green-function remapper: use `ChiGDb` for this method.

### 2. Validate the independent KS bubble

Generate a dynamic dielectric input:

```sh
yambo -d d -F chi_g0.in -J chi_g0
```

Use the same SAVE and KS reference. Add:

```text
ChiFxc
ChiGMode="G0"
ChiGAxis="IMAG"
ChiGNuSteps=16
% ChiGNuRnge
 0.0 | 20.0 | eV
%
ChiGCheckTol=0.0001
ChiGNormTol=0.05
ChiRcondMin=1.0e-12
XTermKind="none"
% EhEngyXd
 0.0 | 0.0 | eV
%
```

Choose `QpntsRXd` entirely after q index 1; for an initial test, use one
existing finite q. Set `BndsRnXd` to the intended response bands. Disable
double-grid interpolation and external X energy, width, residue and
Green-function corrections. The implementation selects retarded ordering
and zero coarse-grid spacing for the KS baseline.

```sh
yambo -F chi_g0.in -J chi_g0
```

The run checks the native KS response and exports zero fxc. `IMAG` means a
continuous zero-temperature bosonic grid z=i nu, including nu=0 when
requested. This export grid does not change the native dielectric frequency
grid or replace the matrix used to calculate screening. `ChiGAxis="REAL"`
instead exports on the native response grid in the upper half plane; use
positive response damping to avoid real-axis poles.

### 3. Build the Dyson bubble kernel

Copy the validated response input to `chi_dyson.in` and change/add:

```text
ChiGMode="DYSON"
ChiGDb="gw_g/ndb.G"
```

```sh
yambo -F chi_dyson.in -J chi_dyson
```

The loader checks the k mesh and ordering, band/spin coverage, diagonal
state table, KS starting energies, energy reference, an open QP gap, positive damping,
causality, spectral norms, and consistency of exported G with its Dyson
self-energy. A partial QPkrange fails explicitly. `ChiGNormTol=0.05` is an
initial diagnostic threshold, not a convergence claim: tighten it while
expanding/refining the spectral grid and reducing eta.

## Database additions

The existing variables and per-q fragment layout of `ndb.Chi` are retained:

- `CHI_FREQ_Q_<iq>`: actual complex export frequencies in Hartree.
- `CHI0_Q_<iq>`, `P_Q_<iq>`, `FXC_Q_<iq>`: unsymmetrized PW matrices.
- `CHI_RCOND_Q_<iq>`: reciprocal condition estimates for chi0 and P,
  shape (2, Nfreq).
- `CHI_G0_ERROR_Q_<iq>`: relative G0/native-KS comparison error; -1 means
  the comparison was not performed in LEVELS mode.
- Header: `CHI_G_MODE`, `CHI_G_AXIS`, `CHI_G_DB`, and
  `CHI_G_PARAMETERS` = [norm tolerance, KS comparison tolerance, rcond
  threshold, chemical potential, kBT]. `CHI_OPTICAL_Q0` stores the optical
  field direction with Yambo's numerical q0 norm for q=0 projection.
- `BSEChiOut` header only: `CHI_OCC_PARS` = [BSE bands (2), IBZ k, spins, full bands, last
  occupied band, temperature] and `CHI_OCCUPATIONS` (bands, k, spin), the occupations of the
  exporting BSE levels, checked by the Casida run.

Older Chi files remain readable without the new optional metadata; absent
conditioning diagnostics read as zero and absent G0 diagnostics as -1.

Retarded `ndb.G` additionally stores `G_RETARDED`,
`G_CHEMICAL_POTENTIAL`, `G_REFERENCE_ENERGIES`, `G_SIGMA_X` and
`G_VXC_REFERENCE`. Existing `SE_Operator` stores DeltaSigma. Recover
Sigma_c as `SE_Operator - G_SIGMA_X + G_VXC_REFERENCE`, broadcasting the
static terms over frequencies. `Green_Functions_Energies` includes eta.

## Current scope and validation

The response G0, COHSEX and QP paths support optical q=0, serial CPUs and
k-only MPI. The DYSON optical limit remains unsupported. They force retarded
ordering on the response, so they stop in a job that also runs GW or a BSE:
build `ndb.Chi` in a screening-only job. They reject GPU execution,
finite temperature, metallic KS references, double grids,
transition-energy filtering and response terminators. The retarded GW
export requires PPA, diagonal Sigma, unshifted KS starting energies and no
GW terminator, Green-function zoom, GreenF2QP, self-consistent GW or mixed
electron-phonon/photon self-energies. Off-diagonal Sigma and the GW response
vertex remain separate extensions. The optical COHSEX and QP kernels and the
static Casida projection have been run on monolayer WS2; the frequency-dependent
Casida solver and the BSE kernel export have been validated end to end on bulk
silicon (see above).

Portable tests compile the production numerical module:

```sh
python tests/chi_green/run_tests.py --fc gfortran
```

To compile and exercise the production bubble, inversion and Dyson routines
with actual Reference LAPACK and synthetic material/IO fixtures:

```sh
python tests/chi_green/run_tests.py --fc gfortran --lapack-source /path/to/lapack
python tests/chi_green/run_tests.py --fc gfortran --single-precision --lapack-source /path/to/lapack
```

The tests check analytic poles and satellites, an independent fermionic
Matsubara sum, the G0 limit, causality, quadrature, XC subtraction, the
imaginary export grid, inverse response differences, screening restoration,
static COHSEX pole loading and inverse differences, preservation of retarded
sampling damping, and disjoint k partitions whose sum matches the serial
bubble (including an empty synthetic rank). Negative tests cover invalid
COHSEX databases, occupation crossings, missing states, legacy spectra,
reference mismatches, unsupported MPI layouts and ill-conditioned inversions.
They passed with GNU Fortran 16.2.0 and Reference LAPACK 3.12.1 in both host
precision settings.

A separate test uses real MPI collectives around the production bubble
with synthetic material/IO. Run it on a machine with a GNU-compatible MPI
Fortran compiler and launcher; no LAPACK source is needed for this test:

```sh
python tests/chi_green/run_tests.py --fc gfortran --mpifc mpifort --mpiexec mpiexec
python tests/chi_green/run_tests.py --fc gfortran --single-precision --mpifc mpifort --mpiexec mpiexec
```

The default runs use two and three ranks for a two-k-point fixture, testing
an empty rank as well. This test has not been executed in the Windows
validation environment, which has no MPI runtime. It does not exercise
Yambo's native communicator creation or wavefunction distribution.

The frequency-dependent Casida numerics have their own test, which compiles
the production module against LAPACK:

```sh
python tests/chi_dynamic/run_tests.py --fc gfortran
python tests/chi_dynamic/run_tests.py --fc gfortran --openmp
```

It checks the exact pair-space identity against a G-space Dyson inversion for
arbitrary non-Hermitian fxc(z), with coupling and in TDA; the static limit
against the matrix assembled as in `K_stored_in_a_big_matrix` and the
Lorentzian sums of `K_diago_response_functions`; error codes; and a threaded
frequency loop against the serial one. For `BSEChiOut` it checks that the pair
response of diag(E,-E) is the pair bubble and that Casida with the exported
"exc" and "full" kernels reproduces the response of a random coupled BSE
(QP energies, vbar and a random W part) at real and complex frequencies. It
also covers the full v (G=0 in the exchange, as at finite q with a Coulomb
cutoff) and an exchange restricted to a G subset, with the kernel computed on
the subset and embedded (`BSEGinplane`). The whole-space routines must equal the
pair ones for derived anti-resonant rows, and, for explicit anti-resonant rows
(their own vertices, energies, occupations f < 0 and oscillators, a random W
part), Casida with the exported kernel must return the BSE response for every
G, G'.

These tests use mock material and database IO. They do not establish a
 complete Yambo build, NetCDF round-trip, real MPI material run, or a material
 benchmark. Those
checks require your Linux/HPC build and SAVE/screening data. Begin with the
G0 run, then converge spectral coverage/resolution, eta, bands, k mesh and
density basis before drawing physical conclusions from the Dyson kernel.
