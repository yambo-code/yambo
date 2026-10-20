# Handoff: theory, code and implementation of the exact-kernel fork

This file passes the technical work on this Yambo fork to a new Claude Code session.
The other session keeps the cluster runs, convergence, results and explanations.
This session works on the theory, the Fortran, the tests and new features.
Read this file first, then `doc/greens_function_fxc.md`, which is the full reference at 1078 lines.

## 1. Goal and working rules

**Physics goal.** Model excitons, and later trions, in n-doped monolayer WS2 with TDDFT.
- The route is GW → G → χ → f_xc → Casida.
- The kernel f_xc is built so that TDDFT reproduces the BSE (Bethe–Salpeter equation).
- The advisor supplies a TDDFT trion code that will consume these kernels.

**Repository.**
- Fork: `Adolfo-PL/yambo`. Upstream base: `19c1241` (Yambo 5.3.0 r23959). First fork commit: `ac50f1f`.
- Branch: `claude/nbd-file-generation-path-yidcr2`. Develop and push only there.
- Push with `git push -u origin claude/nbd-file-generation-path-yidcr2`. On network errors, retry 4 times with 2, 4, 8 and 16 s waits.
- Do not open a pull request unless the user asks.
- Do not put model names or identifiers in commits, code or documents.
- End every commit message with:
  ```
  Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
  Claude-Session: <this session's URL>
  ```
- Before ending a turn, run `rm -rf sbin/chi_tools/__pycache__`. Never commit it.
- Match the surrounding Fortran style: the `!` spacer lines, the `ONLY:` lists, and short comments only where needed.

**About the user.**
- PhD student who is new to this formalism.
- Wants plain explanations backed by verified sources: cite the Yambo source line or the paper, and do not guess.
- Corrects overstatements, so be exact. Example: the exact kernel needs the BSE Hamiltonian H to be *built* but not diagonalized. That does **not** make it cheaper than a BSE run.
- Cluster time is expensive. Anything that runs there must fail fast and check itself before the expensive step.

## 2. Theory in one page

Notation:
- KS: Kohn–Sham. QP: quasiparticle. χ0: KS bubble. P: proper (irreducible) response. v: bare Coulomb. f_xc: exchange–correlation kernel.
- TDDFT Dyson equation: χ = χ0 + χ0 (v + f_xc) χ.
- Exact kernel for a target P: **f_xc = χ0⁻¹ − P⁻¹**, an N_G×N_G matrix for each (q, ω).

The fork offers several choices of P (`ChiGMode`, `BSEChiOut`):

| Choice | P(ω) | Where |
|---|---|---|
| G0 | KS bubble (a check: f_xc must vanish) | `Chi_G_bubble.F` |
| COHSEX | static QP bubble from ndb.QP | `Chi_G_bubble.F` |
| QP | GW energies as unit-weight poles | `Chi_G_bubble.F` |
| DYSON | convolution of the retarded GW spectral functions (needs PPA) | `Chi_G_bubble.F`, `QP_Green_Function.F` |
| BSE `exc` / `full` | proper BSE response with the exchange term removed, QP or KS reference | `K_Chi_export.F` |
| BSE `mdr` | Marini–Del Sole–Rubio kernel | `TDDFT_Chi_dynamic.F` |

**BSE as P.** P(ω) = ρ [ω − H̃]⁻¹ ρ* (Adler–Wiser form with BSE energies).
- H̃ = D + √F K √F, where D holds the transition energies and K is the BSE kernel.
- √F is diagonal with s_t = √(f_v − f_c).
- ρ_t(G) are the oscillator matrix elements, with explicit anti-resonant rows at finite q.
- The exchange part v̄ is left out of K, so P is the proper response. Check: Casida with the exported f_xc returns the BSE spectrum for every G, G′ (`tests/chi_dynamic`, and WS2 SOC in the doc).
- Only H̃ is needed, not its eigenvectors. Building H̃ is most of a BSE run's cost, so this route does not save time against a BSE.

**Casida with f_xc(ω).**
- The f_xc projected onto transitions depends on ω. Excitons are the solutions of λ(ω) = ω (`BSEChiDyn`, `K_Chi_dynamic.F`).
- A static kernel f_xc(ω = 0) gives only about 7 % of the WS2 binding energy (12×12 non-SOC run below; 11 % in the doc's 6×6 SOC test).
- The kernel has poles between the exciton energies. In the 12×12 non-SOC run, α(ω) changes sign between 2.31 and 2.32 eV (the doc's 6×6 SOC test has its pole near 2.29 eV).
- `sbin/chi_tools/sc_scan.py` tells a genuine crossing of λ(ω) = ω from a jump caused by a pole.

**MDR.** P_MDR = χ0 + χ0 (W-ladder) χ0 at first order, with the kernel f_xc = χ0⁻¹ P_MDR χ0⁻¹ (Marini, Del Sole and Rubio, PRL 91, 256402, 2003).
- Implemented as `TDDFT_Chi_mdr_response` and `TDDFT_Chi_kernel_mdr`.
- Selected with `BSEChiOut= "mdr"`. It replaces Yambo's disabled `BSfxc`.

**Kernel comparison on WS2**: the user's cluster run of `sbin/chi_tools/jobs/kernel_compare.sh`.
- Setup: 12×12 k-grid **without SOC**, bands 11–16, 21 G, `BSEGinplane`, GW energies from `g0w0/ndb.QP`, q1.
- Window gap 2.5433 eV; BSE A exciton at 2.2353 eV, so Eb = 0.308 eV.
- This run is newer than the doc. The doc's table (`greens_function_fxc.md`, "Yambo's own TDDFT kernels") is the 6×6 SOC test: bands 23–30, Eb 0.413 eV, MDR 98 %, fitted α 2.2, pole near 2.29 eV. The two sets of numbers come from different systems and are both correct.
- The doc's 0.309 eV run (bands 11–16, 9 G, no cutoff) is a third, earlier run.

| Kernel | Result |
|---|---|
| exact f_xc(ω) | matches the BSE exactly |
| exact static | 7 % of Eb |
| MDR(ω) | 93 % of Eb; peak height 8.14 against 8.00 |
| LRC, α fitted | α = 3.262; 11× too bright |
| LRC, α(E_X) = 2.3355 | 26 % of Eb |
| PF kernels | 5–8 % of Eb |
| JGM | overbinds |

## 3. Code map by feature

Run `git log --oneline 19c1241..HEAD` for the history. In total the fork changes 53 files: about +4600 and −100 lines.

| Feature (input variables) | Main files | What to know |
|---|---|---|
| **ndb.W** (`-W s|p`, i.e. `--Wpot`; static or PPA) | `pol_function/W_potential.F`, `io/io_W.F`, `modules/mod_Wpot.F`, `qp/W_Sigma_check.F` | Writes W(q,G,G′) to disk with the RIM-W corrections folded in. Checks Σc rebuilt from ndb.W. |
| **ndb.Chi** (`ChiFxc`, `ChiGMode`, `ChiGDb`, `ChiGAxis`, `ChiGNuSteps`, `ChiGNuRnge`, `ChiGNormTol`, `ChiGCheckTol`, `ChiRcondMin`) | `pol_function/Chi_fxc.F`, `pol_function/Chi_G_bubble.F`, `modules/mod_Chi.F`, `modules/mod_Chi_Green.F`, `io/io_Chi.F`, `pol_function/X_pre_setup.F` | Stores χ0, P and f_xc for each q and ω. Must run in a screening-only job. Uses k-only MPI. Rejects GPU, metals and terminators. |
| Retarded G export (DYSON) | `qp/QP_Green_Function.F`, `QP_driver.F`, `io/io_QP_and_GF.F`, `mod_QP.F` | Requires PPA, a diagonal Σ, and no terminator or zoom. |
| **BSKmod="CHI"** (projects ndb.Chi onto transitions) | `bse/K_kernel.F`, `tddft/TDDFT_Chi_Fourier.F`, `TDDFT_ALDA_eh_space_G_collisions_{L,R}.F` | Uses the optical q→0 limit with the head and wings. Uses the de-excitation vertex in the coupling block. |
| **BSEChiDyn** (frequency-dependent Casida) | `bse/K_Chi_dynamic.F`, `tddft/TDDFT_Chi_dynamic.F`, `K_solvers.F` | Does one solve per ndb.Chi frequency. Accepts undamped real frequencies for kernels exported from a BSE. |
| **BSEChiOut** = none/exc/full/mdr | `bse/K_Chi_export.F`, `K_diago_driver.F` (`binding_report`) | Builds P from the coupled BSE and writes ndb.Chi. Prints the `[BSE]` binding-energy report lines. |
| Finite q (`BSEQptR`) | `K_Chi_export.F`, `K_kernel.F`, `K_driver_init.F` | Adds explicit anti-resonant transitions and exports over all rows for each BSE q. |
| **BSEChiKoutW / BSEChiKoutB** | `bse/K_Chi_Kdump.F` | Dumps the Casida matrix as text. `sbin/chi_tools/ktt_casida.py` diagonalizes it. |
| **BSEGinplane** | `K_exchange_collisions.F`, `K_Chi_export.F`, `K_kernel.F` | Keeps only G_z = 0 for layers in the xy plane. |
| Spinors and SOC | `K_Chi_export.F`, `K_kernel.F`, ALDA collisions | Symmetry-safe with SOC. Use nosym only without SOC. |
| Coulomb cutoff race fix | `coulomb/cutoff_driver.F` | Adds `PP_wait` barriers after the ndb.cutoff read and after the master's write (`247d1c8`). Before the fix, ndb.cutoff was read while being written. |

**Runlevel gate.** `src/interface/INIT_barriers.F`, also changed by the fork, decides which runlevels survive together. Upstream switches `rim_w` off for MPA (`gw0_mpa` list). Forcing it on gave inconsistent gaps. That change was reverted, so MPA plus RIM-W stays unsupported.

**Python and Fortran tools** in `sbin/chi_tools/`:
- `fxc_export.py`: ndb.Chi → text or npz; `--at` picks one frequency.
- `xq_summary.py`: exciton energy at each q.
- `plot_summary.py`
- `sc_scan.py`
- `ktt_casida.py`
- `simple/`: a plain Fortran Casida solver and minimal scripts.

**Cluster job scripts** in `sbin/chi_tools/jobs/` (SLURM, stokes):
- `gw_converge.sh` and `bse_converge.sh`: staged convergence with MPI-layout preflight checks.
- `exact_kernel.sh`: production workflow from a fresh SAVE (GW, screening, reference BSE, kernel export at every q, Casida check, kernel files). The Casida check showed that spin-forbidden dark excitons (relative strength ~1e-8 with SOC) are not poles of the density response P, so no density kernel reproduces them.
- `kernel_compare.sh` and `plot_kernels.py`: run and plot the kernel comparison above.
- `xq_dispersion.sh`, `finiteq_test.sh`, `fkq_dump.sh`, `fq_export.sh`
- `test_gw_converge_mpi.sh`: local, oversubscribed MPI test; set `S`.

The other session owns these scripts. Change them here only for code-side reasons, such as a new input variable or a new output format.

## 4. Yambo internals learned the hard way

**MPI roles.** From `parallel/PARALLEL_structure.F` and `PARALLEL_global_dimensions.F`: a role's CPU count must not exceed its number of elements.
- DIP `k c v`: c is the number of conduction DipBands.
- X `q g k c v`
- SE `q qp b`:
  - In the Fock step, b = max(nbm, QP_nb). In Σc, b = N bands.
  - The same `SE_CPU` string is used for both steps.
- BS `k eh t`:
  - k ≤ the number of IBZ k-points.
  - eh is not checked.
  - t is checked against a stale PHASE_space = nc·VB − (VB−nv+1) + 1.

**2D output.** With a slab cutoff (`CUTGeo="slab z"`), the optical output is `o-*.alpha_q1_*`, not `eps`.

**RIM-W.**
- Needs enough X G-vectors. The run fails with "G_max 165 > 53" if `NGsBlkXs` is too small for `RandGvecW`.
- Not supported with MPA (see the runlevel gate above).

**Local MPI testing.**
- Use OpenMPI with `--oversubscribe --mca mpi_yield_when_idle 1`, and set `HDF5_USE_FILE_LOCKING=FALSE`.
- A run of 48 tasks on 4 cores reproduces cluster-only races.

## 5. Build and test

Configure line used locally; the serial build uses `--disable-mpi` instead of the MPI settings:
```
./configure FC=gfortran F77=gfortran CC=gcc MPIFC=mpif90 MPIF77=mpif77 MPICC=mpicc \
  --enable-mpi --enable-open-mp --disable-hdf5-par-io \
  --with-netcdf-libs="-L/usr/lib/x86_64-linux-gnu -lnetcdf" --with-netcdf-includedir=/usr/include \
  --with-netcdff-libs="-L/usr/lib/x86_64-linux-gnu -lnetcdff" --with-netcdff-includedir=/usr/include \
  --with-hdf5-libs="-L/usr/lib/x86_64-linux-gnu/hdf5/serial/lib -lhdf5_fortran -lhdf5_hl -lhdf5" \
  --with-hdf5-includedir=/usr/lib/x86_64-linux-gnu/hdf5/serial/include \
  --with-fft-path=/usr --with-blas-libs=-lblas --with-lapack-libs=-llapack
make yambo
```
- Install the packages first: gfortran, OpenMPI, libnetcdf(f), libhdf5, BLAS/LAPACK and FFTW.
- The first build downloads Yambo's bundled libraries and takes a while.
- For fast iteration, copy edited `.F` files into a configured build tree and run `make yambo`.
- A fresh container has no build and no test SAVE databases. The earlier ones lived in the old session's scratchpad: a 12×12 WS2 with 80 bands, a 6×6 SOC WS2, and silicon. Rebuild them with Quantum ESPRESSO + p2y if a material test is needed, or ask the user for a small SAVE.

**gfortran 13 miscompile.** gfortran 13 at `-O3` miscompiles `src/bse/K_correlation_collisions_std.F`. A plain SEX BSE crashes, and with the export it gives NaN (doc, end of the BSEChiOut section).
- Earlier sessions recompiled that one file at `-O1` and relinked, using a sandbox-only script that was not kept. No source change was made.
- Before any local material BSE, reproduce that workaround and check that a plain SEX BSE gives no NaN.
- It does not affect the unit tests. The user's cluster SEX BSE runs gave sane binding energies, but which compiler built the fork there has not been checked.

Unit tests need only gfortran, plus LAPACK where noted:
```
python tests/chi_green/run_tests.py --fc gfortran [--lapack-source DIR] [--mpifc mpifort --mpiexec mpiexec]
python tests/chi_fxc_fourier/run_tests.py --fc gfortran
python tests/chi_dynamic/run_tests.py --fc gfortran [--openmp]
```
- `tests/chi_dynamic` covers the pair-space identity, the static limit against `K_stored_in_a_big_matrix`, BSEChiOut exc/full against a random coupled BSE, finite-q anti-resonant rows, BSEGinplane, and the MDR checks.
- These tests use mock I/O. They do not replace a material run.

## 6. Physics numbers the code work relies on

Monolayer WS2:
- GW gap without SOC: about 2.79 ± 0.03 eV, extrapolated in 1/N_k from the RIM-W 9/12/15/18 grids.
- SOC correction: Δ_SOC = −0.264 eV, so the final gap is about 2.53 ± 0.03 eV. Literature: 2.36–2.53 eV.
- Converged GW settings: EXX 40 Ry; 800 bands, or 400 with the BG terminator; X block 12 Ry; RIM-W.
- BSE binding energy on 12×12 (non-SOC, convergence run): 0.725 eV. The k-grid convergence (k9 → k30) is still running.
- Electron counts: VB = 13 without SOC (26 with SOC). Bands 25–28 cover the SOC splittings.
- Degenerate pairs at Γ without SOC: 11/12, 14/15 and 16/17. A BSE band window must not split them.

## 7. Open code items

1. **Doping (needed next).** The BSE route already accepts partial occupations; the G routes do not. No doped material run has been made yet.
   - Upstream BSE, ground state: `src/bse/K_Transition_check.F`.
     - f_t = (f_v − f_c)/spin_occ (line 64).
     - f < 0 goes to the anti-resonant block (line 110). f = 0 is kept only on the E > 0 side, for the JDOS (line 116).
     - Every E = 0 transition is dropped (line 47). At q = 0 this removes all intraband (Drude) terms.
     - In the length gauge at q = 0, transitions below `Dip%Energy_treshold` are dropped (line 44).
     - A partly filled band is both valence and conduction: `V_bands` ends at `maxval(Ken%nbm)` and `C_bands` starts at `minval(Ken%nbf)+1` (`K_Transitions_setup.F:273-274`).
     - `n_met_bands` (`K_Transitions_setup.F:275-285`) is inside `#if defined _RT`: real-time carriers only, not the ground-state path. `BS_not_const_eh_f` (line 107) is set but read nowhere else.
   - Fork, exact-kernel route (`BSEChiOut` → Casida): `K_Chi_export.F` (about line 129) forms s = √f as a complex number. `tests/chi_dynamic` checks the identity with non-unit and negative occupation factors (`test_dynamic.f90:534-535`).
   - Blocked, G routes (`ChiGMode`):
     - `X_pre_setup.F` (about lines 144–145) stops any metallic KS reference (`nbf /= nbm`).
     - `Chi_G_bubble.F` (about lines 235–248) treats each state as full (f > 0.5) or empty, and stops when an occupied QP level lies above an empty one.
   - Open: the intraband (Drude) term; the screening W of the doped layer at q → 0; the `[BSE]` report, whose "gap" (lowest transition of the window) means nothing when a band is partly filled.
2. **Trions.** Define the interface to the advisor's TDDFT trion code. It probably takes f_xc_GG′(q, ω) from `fxc_export.py` at several q (BSEChiOut at finite q already exports this) plus the band structure. Agree the format with the user before writing code.
3. **ChiGMode limits.** The DYSON optical limit, off-diagonal Σ, and MPI beyond k-only are unsupported (doc, "Current scope").
4. **Possible upstream patch.** The `cutoff_driver.F` barrier fix is generic and could go to Yambo upstream. Only do this if the user asks.
5. **Optional.** Clean MPA + RIM-W support would need RIM-W added to the MPA W path, not just to the runlevel list. That is a deeper change to `QP_mpa` and the RIM-W tables.
