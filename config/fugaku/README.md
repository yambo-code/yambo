# Building Yambo on Fugaku

## Build

On the login node, in a new directory on project storage:

```sh
YAMBO_WORK=/path/to/project-storage/yambo-build bash config/fugaku/build.sh
```

This copies the source to `$YAMBO_WORK/yambo`, builds `yambo`, `ypp`, `p2y`
and `a2y` there, and writes `$YAMBO_WORK/env.sh` and a check program
`$YAMBO_WORK/verify`. 

Libraries (Spack prefixes in `build.sh`): Fujitsu 4.12 / TCS 1.2.41, NetCDF
4.9.2 / 4.6.1, parallel HDF5 1.14.6, Fujitsu FFTW 1.1.0, and
LibXC 6.2.2. Double precision, MPI, OpenMP, parallel I/O.

## Optimisation flags

Fortran is compiled with **`-Kfast -Kocl -g -Kopenmp`** (override with
`YAMBO_FCFLAGS`). Do not use `-O2`: the GW-PPA self-energy was 1.6× and
the BSE kernel 1.5× slower than with `-Kfast`, mostly from strict complex
division and runtime complex `abs`/`**` calls. 

`-Kfast` includes reciprocal-approximation division (`-Kfp_relaxed`), which
turns an overflowed `1/Inf` into NaN; so in our commits we capped the Bose and Fermi
exponents so finite-temperature GW and metals are correct. We wanted to use
`-Ksimd=2`, but found it leads to crashes. So other work arounds were introduced
to properly vectorize.

`-Kocl` enables `!OCL` lines (ignored by default). One keeps exact
division and no FMA in `mod_vec_operate.F`: with `-Kfast` there, GrCo GW
quasiparticle energies shifted by 0.2 meV.

## Run

Four MPI ranks × 12 OpenMP threads per node. Check the build in a job:

```sh
source "$YAMBO_WORK/env.sh"
OMP_NUM_THREADS=12 mpiexec -n 4 "$YAMBO_WORK/verify"
```

## Source changes to make the code work

- Fujitsu NaN detection (from `tech-fujitsu` commit `349dd331`) and two
  redundant `+ +` operators removed (`QP_check_interpolate_W.F`).
- `PHASE_space` initialised for roles without a phase space
  (`PARALLEL_structure.F`); distributed ScaLAPACK failed or ran serially.
- Overflow-safe Bose and Fermi factors (`mod_functions.F`,
  `QP_ppa_cohsex.F`, `QP_mpa.F`). Required for `-Kfast`.
- Accumulating BLAS-wrapper outputs declared `intent(inout)`
  (`mod_wrapper.F`, `mod_wrapper_omp.F`).
- Vectorisable PPA self-energy loop on CPUs (`QP_ppa_cohsex.F`).
- `1/|q+G|^2` computed once per q in the BSE exchange kernel
  (`K_kernel.F`, `K_exchange_kernel.F`).
- Reads of variables that were never set, found with frt run-time checks
  (`SET_job_strings_and_dirs.F`, `mod_com.F`,
  `msg_characters_vector_int.F`).
- Haydock dot products broadcast from rank 0 (`K_dot_product.F`, Fujitsu
  only). SSL2's ZDOTC rounds differently for data at different addresses,
  so rank replicas diverged and multi-rank Haydock spectra were wrong.
- `!OCL NOFP_RELAXED,NOFP_CONTRACT` on `vec_operate` (`mod_vec_operate.F`):
  `-Kfast` arithmetic in the k/q/G conversions shifted metal QP energies.
