# Xo MPI investigation: conduction-band (`c`) level

This document records an investigation of the MPI workload distribution in the irreducible polarizability (`Xo`) driver, with specific focus on conduction-band parallelism. No load-balancing changes are proposed as implementation work here.

## Executive conclusions

- The `c` parallelization distributes contiguous blocks of conduction-band indices, balancing only their count. Rank 0 gets the lowest block and, when there is a remainder, is one of the first ranks to get an extra band.
- In the supplied run, every rank has exactly 247 conduction bands and exactly 1,280,448 raw transitions per q-point.
- Transition-energy grouping is performed locally, after the MPI ownership masks have already selected each rank's bands. There is no global grouping followed by redistribution.
- The first number in `[X-CG] R(p) Tot o/o(of R)` is `coarse_grid_N`: the number of local transition-energy groups and, in this run, the number of main Xo coarse-grid iterations. The second is the local raw-transition count. The final `100` is not their ratio.
- P1 has substantially more coarse-grid groups than P8: summed over 30 q-points, 7,771,736 versus 5,458,797, a ratio of 1.424.
- The corresponding `Xo (procedure)` ratio is 5736.26/3197.29 = 1.794. Across all eight ranks, grouped-count and Xo time have Pearson correlation 0.9976.
- P1 is slow because its lowest conduction-band block produces less energy degeneracy—more distinct coarse groups—not because it has more bands or raw transitions, and not primarily because it is MPI rank 0.
- The final Xo all-reduction exposes the imbalance directly: P1 waits only 0.64 s in `Xo (REDUX)`, while P8 waits 2549.43 s. Compute plus reduction is approximately 5747 s for every rank.

## Source and run provenance

The authoritative revision for the supplied run is `a4a1dfaa9`, on branch `5.4`. The checkout used for this investigation is exactly that commit, and the relevant source files match it without local differences. Both the source references and the interpretation of the observed timings therefore describe the implementation that produced the run.

Run data:

```text
/home/nicola/tmp/anatase_on_leonardo-booster_2node_8mpi_8gpus
```

The run used 8 MPI ranks, 8 NVIDIA A100 GPUs, and one MPI rank per GPU. The X parallel structure was:

```text
X_and_IO_CPU   = "1 1 1 8 1"
X_and_IO_ROLEs = "q g k c v"
```

Thus q, G, k, and v were not distributed, while all eight ranks participated in `c` parallelism.

## Execution flow

```text
PARALLEL_global_indexes
  `- contiguous c-band ownership mask
       |
       v
X_irredux
  |- X_eh_setup(-iq): build local raw transition energies
  |- FREQUENCIES_coarse_grid: sort and group local energies
  |- X_eh_setup(+iq): populate sorted transition descriptors
  |
  `- for every local coarse group
       |- X_irredux_residuals
       |    |- generate and store every raw-transition oscillator
       |    `- form the group residue with one GEMM
       |- evaluate one Green function for the group
       `- accumulate its residue into Xo
            |
            v
       MPI barriers + MPI_Allreduce of Xo
```

## 1. Conduction-band distribution

The response-function distribution is initialized in `src/pol_function/X_dielectric_matrix.F`, around lines 229–255, before entering the q loop and before calling `X_irredux`.

`PARALLEL_global_indexes(...,"Response_G_space_and_IO",X=X)` creates the hierarchy `q,g,k,c,v`. The communicator hierarchy is constructed by:

- `src/parallel/PARALLEL_global_Response_G.F`, routine `PARALLEL_global_Response_G`;
- `src/parallel/PARALLEL_structure.F`, routine `PARALLEL_structure`;
- `src/parallel/PARALLEL_assign_chains_and_COMMs.F`;
- `src/parallel/PARALLEL_build_up_child_INTER_chains.F`.

For the `1 1 1 8 1` hierarchy, the `c` communicator contains all global ranks in global-rank order. Its `CPU_id` therefore equals the zero-based MPI rank.

The band limits are established in `src/parallel/PARALLEL_global_dimensions.F`, around lines 61–66:

```fortran
PAR_n_c_bands = (/minval(E%nbf)+1,PAR_X_ib(2)/)
```

For the supplied insulating run, the report gives 24 valence bands and an X band range ending at 2000, so the conduction range is 25–2000: 1976 bands.

The ownership mask is built in `src/parallel/PARALLEL_global_indexes.F`, around lines 163–188:

```fortran
call PARALLEL_index(PAR_IND_CON_BANDS_X(X_type),(/PAR_n_c_bands(2)/), &
                    low_range=(/PAR_n_c_bands(1)/),                  &
                    COMM=PAR_COM_CON_INDEX_X(X_type),                &
                    CONSECUTIVE=.TRUE.,NO_EMPTIES=.TRUE.)
```

The important object is `PAR_IND_CON_BANDS_X(X%whoami)`, a `PP_indexes` structure containing:

- `element_1D(ic)`: true when the current rank owns global band `ic`;
- `first_of_1D(rank+1)`;
- `last_of_1D(rank+1)`;
- `n_of_elements(rank+1)`.

`X_eh_setup` tests `element_1D(ic)` before accepting a conduction band.

### Distribution algorithm

The `CONSECUTIVE=.TRUE.` branch is in `src/parallel/PARALLEL_index.F`, around lines 73–105. For `N` elements and `P` ranks:

```text
base      = floor(N/P)
remainder = N - base*P
```

The first `remainder` communicator ranks receive `base+1` elements; the rest receive `base`. The blocks remain contiguous and ordered from the lowest to highest global index.

Consequently:

- rank 0 always receives the lowest conduction bands;
- low-numbered ranks receive any remainder bands;
- this is block distribution, not cyclic or block-cyclic distribution.

Here `1976/8 = 247` exactly, so there is no remainder:

| Process | MPI rank | Conduction bands | Count |
|---|---:|---:|---:|
| P1 | 0 | 25–271 | 247 |
| P2 | 1 | 272–518 | 247 |
| P3 | 2 | 519–765 | 247 |
| P4 | 3 | 766–1012 | 247 |
| P5 | 4 | 1013–1259 | 247 |
| P6 | 5 | 1260–1506 | 247 |
| P7 | 6 | 1507–1753 | 247 |
| P8 | 7 | 1754–2000 | 247 |

The P-number convention is explicitly `P = myid + 1` in `src/modules/mod_LIVE_t.F`, around lines 233–239. Therefore P1 is MPI rank 0.

## 2. Construction of raw transitions

`X_irredux` calls `X_eh_setup` twice:

1. `X_eh_setup(-iq,...)` constructs the local transition energies.
2. After grouping, `X_eh_setup(+iq,...)` reconstructs the same transitions and writes their descriptors in sorted/grouped order.

The central loops in `src/pol_function/X_eh_setup.F`, around lines 59–139, are:

```fortran
do i_sp=1,n_sp_pol
  do ik_bz=1,Xk%nbz
    do iv=X%ib(1),X%ib(1)+Nv(i_sp)-1
      do ic=ic_min,X%ib(2)
```

Each index is filtered through its MPI ownership mask:

- `PAR_IND_Xk_bz%element_1D(ik_bz)`;
- `PAR_IND_VAL_BANDS_X(... )%element_1D(iv)`;
- `PAR_IND_CON_BANDS_X(... )%element_1D(ic)`.

For a normal, non-terminator transition, the energy is:

```fortran
E_eh = Xen%E(ic,ik,i_sp) - Xen%E(iv,ik_m_q,i_sp)
```

Here `ik_m_q` is derived through `qindx_X`. The occupation factor is:

```fortran
f_eh = Xen%f(iv,ik_m_q,i_sp) * &
       (spin_occ-Xen%f(ic,ik,i_sp)) / spin_occ
```

Transitions with effectively zero occupation or outside `X%ehe` are excluded.

For the supplied run:

- one spin polarization;
- 216 full-BZ k-points;
- 24 valence bands;
- 247 local conduction bands;
- no active electron-hole energy cutoff;
- zero-temperature insulating occupations.

Hence every rank has:

```text
216 x 24 x 247 = 1,280,448 raw transitions per q
```

This exactly matches the second `[X-CG]` field in every process log.

## 3. Transition grouping

Grouping is implemented by `FREQUENCIES_coarse_grid` in `src/common/FREQUENCIES_coarse_grid.F`.

### Criterion

The local raw `E_eh` values are sorted. Starting from a reference member of each group, another transition joins that group when:

```fortran
abs(E_eh-E_reference) <= 1.e-5 Hartree
```

The tolerance is approximately `2.72e-4 eV`.

This is reference-based, not transitive nearest-neighbor chaining: when a new group starts, its first member becomes the next reference.

With an X terminator, both the transition energy and the initial-state energy must satisfy separate `1.e-5` tolerances. The supplied run has `XTermKind="none"`, so only `E_eh` matters.

### Group representation

The relevant arrays are:

- `ordered_grid_index(raw_index)`: position of an original transition in energy-sorted order;
- `coarse_grid_index(raw_index)`: its coarse-group index;
- `bare_grid_N(group)`: number of raw transitions in the group;
- `coarse_grid_Pt(group)`: mean transition energy of the group;
- `X_poles_tab(sorted_index,:)`: `(ik_bz,iv,ic,spin)` descriptor.

The second `X_eh_setup(+iq,...)` call uses `ordered_grid_index` to populate `X_poles_tab` in the same sorted order used by `bare_grid_N`.

Within `X_irredux`:

```fortran
i_bg = sum(bare_grid_N(1:i_cg-1)) + 1
```

selects the first sorted transition belonging to coarse group `i_cg`. Its descriptor is passed to `X_GreenF_analytical` as the representative of the group. The exact transition energies within the group differ by no more than the grouping tolerance.

### What grouping actually saves

Grouping does not discard the member transitions. `X_irredux_residuals` still loops over all `bare_grid_N(i_cg)` members and includes every oscillator residue.

What it saves is repeated work that depends on the transition energy:

- one residue matrix is formed for the complete group;
- one Green function is evaluated for the group;
- that grouped residue is accumulated into Xo once per output frequency.

Thus:

```text
raw transitions
   -> all still contribute their matrix elements

energy groups
   -> determine the number of Green-function/Xo accumulation iterations
```

Grouping occurs independently on every rank after `c` distribution. There is no collective operation in `FREQUENCIES_coarse_grid`, and there is no redistribution based on the resulting group counts.

## 4. Exact meaning of `[X-CG] R(p) Tot o/o(of R)`

The message is generated in `src/common/FREQUENCIES_coarse_grid.F`, around lines 178–181:

```fortran
write(ch,'(3a)') '[',title,'-CG] R(p) Tot o/o(of R)  '
call msg('rs',trim(ch), &
         (/coarse_grid_N,npts, &
           int(real(coarse_grid_N)/real(dncg)*100._SP)/))
```

Its fields are:

1. `coarse_grid_N`: final local number of transition-energy groups;
2. `npts`: local number of raw accepted transitions;
3. `100*coarse_grid_N/dncg`: fraction retained relative to the initially detected non-degenerate grid.

The final percentage is not `coarse_grid_N/npts`.

This run uses `X poles = 100%`. Therefore no additional user-requested coarse reduction is applied beyond the `1.e-5` degeneracy grouping, and:

```text
coarse_grid_N = dncg
```

This explains why a line can show:

```text
[X-CG] R(p) Tot o/o(of R): 118887 1280448 100
```

although `118887/1280448` is only 9.28%.

For this run, the first number is simultaneously:

- the number of local transition-energy groups;
- the number of coarse-grid entries;
- the number of iterations of the main `i_cg=1,coarse_grid_N` loop;
- effectively the number of calls to `X_irredux_residuals` and grouped Xo accumulations.

It is not the number of raw transitions.

## 5. Expensive Xo work

### Implementation used by the supplied run

At revision `a4a1dfaa9`, each `i_cg` iteration performs the following:

1. Clear `Xo_res`.
2. Loop over every raw transition in `bare_grid_N(i_cg)`.
3. Call `scatter_Bamp_gpu` to construct the oscillator.
4. Store each transition's oscillator in `rhotw_mp_R` and, when required by the row/column distribution, `rhotw_mp_C`.
5. Form the whole group residue with one `devxlib_xGEMM_gpu` call whose inner dimension is `nt=bare_grid_N(i_cg)`.
6. Evaluate `X_GreenF_analytical` once for the group.
7. Accumulate the grouped residue into Xo for each output frequency.

The run has an X matrix size of 753 and two PPA frequencies. Both the per-transition and per-group matrix operations are therefore substantial.

A useful workload model for the executed implementation is:

```text
work ~= Nraw * (Coscillator + Cpacking)
      + sum_groups CGEMM(nt=bare_grid_N(group))
      + Ncg * (Cmatrix-clear + CGreenF
               + Nfreq * Cmatrix-accumulate)
```

Consequences:

- `Nraw` alone is insufficient because it is identical on all ranks. It equals `sum_groups bare_grid_N(group)`, so the total of all GEMM inner dimensions is also identical.
- `coarse_grid_N` controls the number of `Xo_res` clears, GEMM calls, Green-function evaluations, and frequency accumulations.
- At fixed `Nraw`, more groups mean smaller groups on average: more small GEMMs, potentially lower GPU efficiency, and greater per-group overhead.
- This strengthens the quantitative interpretation of the logs: P1 averages only 4.94 transitions per group, versus 7.04 for P8, so P1 executes more GEMM calls with poorer batching.
- `coarse_grid_N` is still not an exact scalar cost model. The complete group-size distribution, individual GEMM efficiency, oscillator reuse at q=1, and memory behavior also matter.

## 6. Quantitative analysis of the supplied run

There are 30 Xo calls, one for each q-point. The main report contains P1's `[X-CG]` values because report output is restricted, while every per-process log contains its local values.

The table uses:

- `CG q1`: first `[X-CG]` count;
- `CG sum`: sum over all 30 q-points;
- relative columns normalized to P8, the minimum;
- reconstructed contiguous conduction-band ranges.

| Process | MPI rank | c range | c count | CG q1 | CG sum, 30 q | CG/P8 | Xo procedure (s) | Time/P8 | Xo REDUX (s) |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| P1 | 0 | 25–271 | 247 | 118,887 | 7,771,736 | 1.424 | 5736.3 | 1.794 | 0.6 |
| P2 | 1 | 272–518 | 247 | 110,710 | 6,708,925 | 1.229 | 4412.2 | 1.380 | 1334.9 |
| P3 | 2 | 519–765 | 247 | 106,809 | 6,275,420 | 1.150 | 3961.8 | 1.239 | 1785.2 |
| P4 | 3 | 766–1012 | 247 | 104,521 | 6,039,288 | 1.106 | 3731.6 | 1.167 | 2015.5 |
| P5 | 4 | 1013–1259 | 247 | 103,103 | 5,893,370 | 1.080 | 3571.6 | 1.117 | 2175.1 |
| P6 | 5 | 1260–1506 | 247 | 101,640 | 5,738,892 | 1.051 | 3402.1 | 1.064 | 2344.7 |
| P7 | 6 | 1507–1753 | 247 | 99,625 | 5,534,715 | 1.014 | 3225.7 | 1.009 | 2521.0 |
| P8 | 7 | 1754–2000 | 247 | 98,702 | 5,458,797 | 1.000 | 3197.3 | 1.000 | 2549.4 |

Additional quantitative observations:

- Raw transitions over 30 q-points are identical: 38,413,440 per rank.
- P1 averages approximately 4.94 raw transitions per energy group.
- P8 averages approximately 7.04 raw transitions per energy group.
- P1 therefore experiences substantially less grouping.
- `CG(P1)/CG(P8) = 1.424`.
- `time(P1)/time(P8) = 1.794`.
- Pearson correlation between per-rank `CG sum` and `Xo procedure` is 0.9976.
- P1 has the largest group count at every one of the 30 q-points; the counts decrease nearly monotonically from P1 to P8.

The timing ratio being larger than the group-count ratio confirms that `[X-CG]` is a strong proxy, not a complete operation-count model.

## 7. Why P1 is the slowest rank

### Demonstrated explanation

P1 owns the lowest conduction-band block, 25–271. For every q-point, transitions from that block collapse into fewer-member groups than transitions from the progressively higher band blocks. P1 therefore has the most coarse-grid iterations.

Equivalently:

- P1 has more distinct local transition energies;
- P1 has more energy groups;
- P1 has smaller average group sizes;
- P1 performs more per-group matrix accumulation and GPU-launch work.

### Is rank 0 intrinsically special?

Not in the timed Xo compute region.

In the implementation used for the run:

- `master_thread` means the OpenMP master thread on every MPI rank, not MPI rank 0;
- the run uses one X thread per GPU rank;
- there is no rank-0-only computational branch capable of explaining thousands of seconds;
- the Drude path is inactive for this insulating calculation;
- dipole I/O and wavefunction loading occur before the `Xo (procedure)` timer;
- report/database I/O is timed separately and is orders of magnitude smaller.

P1 does have reporting and I/O privileges elsewhere, but the report shows only about 11 s for `io_X`, versus a 2539 s P1/P8 Xo-procedure difference.

### Alternative explanations considered

#### Extra remainder bands

Ruled out for this run: every rank owns exactly 247 conduction bands.

#### Different raw transition counts

Ruled out: every process log reports 1,280,448 raw transitions per q-point.

#### Node or GPU asymmetry

P1–P4 ran on `lrdn1018`, while P5–P8 ran on `lrdn1214`. Work counts and procedure times change smoothly with band index across the P4/P5 node boundary. This strongly disfavors node placement or an anomalous GPU as the primary cause.

#### MPI communication or reduction asymmetry

This is a consequence, not the original cause. P8 spends longer in the reduction precisely because it finishes its local calculation sooner.

#### Microscopic reason higher bands group more

The logs prove that this rank's higher conduction-band blocks generate fewer groups, but they do not identify the microscopic reason. Distinguishing band degeneracy, dispersion, symmetry, and single-precision energy coincidences would require inspecting the actual `E_eh` values and group memberships.

## 8. MPI synchronization and reductions

At the end of `X_irredux`, the procedure timer is stopped and the reduction timer is started. For each frequency, the code reduces only `X_par%blc(:,:,iw)` over `PAR_COM_X_WORLD_RL_resolved`, as shown around lines 438–450 of `src/pol_function/X_irredux.F`.

`PP_redux_wait` for a complex matrix, in `src/modules/mod_parallel_interface.F`, performs:

1. `MPI_Barrier`;
2. `MPI_Allreduce(...,MPI_SUM,...)`;
3. another `MPI_Barrier`.

The timer records elapsed wall time. With OpenMP enabled, it uses `qe_cclock`, implemented with `gettimeofday` in `src/tools/ct_cptimer.c`.

The run gives direct evidence that faster ranks wait for P1:

| Process | Xo procedure + REDUX (s) |
|---|---:|
| P1 | 5736.9 |
| P2 | 5747.1 |
| P3 | 5747.0 |
| P4 | 5747.2 |
| P5 | 5746.7 |
| P6 | 5746.8 |
| P7 | 5746.7 |
| P8 | 5746.7 |

The faster ranks' reduction times almost exactly fill the gap to P1. Total Xo wall time is therefore determined by the slowest rank.

## Distinction between workload quantities

For the supplied run:

1. **Conduction bands assigned to each rank:** exactly 247.
2. **Raw accepted transitions per rank and q-point:** exactly 1,280,448.
3. **Transition-energy groups after degeneracy treatment:** rank- and q-dependent; for q1, 118,887 on P1 versus 98,702 on P8.
4. **First `[X-CG]` number:** the final group count `coarse_grid_N`; here it is also the outer Xo loop count.
5. **Actual expensive work:** a combination of fixed raw-transition work and group-dependent residue clearing, Green-function evaluation, frequency accumulation, and GPU launch/batching costs.

These quantities are not interchangeable.

## Direct answers

### A. Is the initial MPI workload balanced only according to conduction bands?

Yes, for the selected `c` parallelization. It balances the number of contiguous band indices without considering raw-transition validity, degeneracy, coarse groups, matrix cost, or measured execution time. In this run, the raw counts also happen to balance exactly.

### B. Does later transition grouping introduce a substantial imbalance?

Yes. Local coarse-group totals differ by 42.4% between P1 and P8 despite identical band and raw-transition counts.

### C. Is this the main explanation for the P1/P8 timing difference?

Yes, with strong evidence: the correlation is 0.998, P1 has the largest group count at every q-point, and the reduction wait times compensate the compute-time difference. The group count is not an exact cost model, so it does not alone explain why the time ratio is 1.794 rather than 1.424.

### D. Why is P1/rank 0 slowest?

Contiguous distribution assigns it bands 25–271, and those bands produce the largest number of locally distinct transition-energy groups. There is no intrinsic rank-0 Xo compute responsibility that plausibly explains the difference. Rank 0 is indirectly exposed because the algorithm always maps it to the lowest band block.

### E. Where would effective-work balancing need to change?

The balance decision must occur after transition energies—or sufficiently accurate per-band weights—are known, but before the `i_cg` Xo loop.

Two levels are possible:

- A smaller change could replace equal contiguous `c` blocks with weighted or mixed band ownership before `X_eh_setup`.
- Exact balancing of coarse-group work would require grouping transition energies first and then distributing the resulting groups/work units, potentially decoupling work ownership from wavefunction/band ownership.

## Possible directions for improved load balancing

- Use noncontiguous or cyclic `c` blocks to mix low- and high-band regions. This would likely mitigate the monotonic trend seen here but would not guarantee balance.
- Estimate a per-band cost from the number of distinct `E_c(k)-E_v(k-q)` groups, then use weighted explicit band masks.
- Construct grouped transition work globally and distribute coarse groups directly. This is the most faithful approach but requires revisiting wavefunction locality, oscillator access, and the final reduction strategy.
- Use a hybrid estimate incorporating both raw transitions and group count, since all raw members still incur residue work and `[X-CG]` is not the complete kernel cost.

## Evidence classification

### Demonstrated directly by source and logs

- Contiguous count-based `c` distribution.
- Exact per-rank band ranges for this run.
- Equal raw transition counts.
- Local post-distribution grouping with a `1.e-5` Hartree criterion.
- Exact meanings of the three `[X-CG]` numbers.
- Per-rank group-count imbalance and its correlation with Xo timing.
- Barrier/all-reduce synchronization and the complementary `Xo (REDUX)` wait times.
- Absence of a material rank-0-only branch in the timed Xo procedure.

### Interpretation supported by the evidence

- The coarse-group imbalance is the dominant cause of the observed Xo procedure imbalance.
- P1 is slow because its lower conduction-band block has less effective transition-energy degeneracy.

### Not established by the available data

- The detailed physical reason why this material's higher conduction bands generate more grouping.
- A single exact scalar cost formula from the logs: they do not report the complete group-size distribution or the efficiency of each GEMM.
