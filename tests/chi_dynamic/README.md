# Frequency-dependent Casida numerics

`BSKmod="CHI"` with `BSEChiDyn` solves the Casida response once per BSE
frequency with the kernel fxc(G,G';z) of a real-axis `ndb.Chi`
(`src/tddft/TDDFT_Chi_dynamic.F`, driven by `src/bse/K_Chi_dynamic.F`):

    Resp(w) = -Co B^T [z - H0 - Kf(z)]^-1 A
    Kf_xy(z) = c diag(sqrt f) V_x^H fxc(z) V_y diag(sqrt f)

H0 holds the KS transition energies and the static (exchange) kernel in
Yambo's BS_mat convention (coupling blocks carry the factor i), V_x are the
excitation (x=res) and de-excitation (x=cpl) vertices of the static CHI
projection, and A, B are the residual vectors of the diagonalization solver.

`test_dynamic.f90` checks:

1. the exact pair-space identity: with coupling, the response equals the head
   of [chi0^-1 - fxc]^-1 computed in G space from the same transitions, for an
   arbitrary complex, non-Hermitian fxc(z); in TDA, the block-diagonal version;
2. the static limit: for a Hermitian fxc that is real in real space, the
   projected blocks have the symmetries `K_stored_in_a_big_matrix` assumes, and
   the response equals the Lorentzian sums of `K_diago_response_functions`
   (TDA with its anti-resonant mirror, and coupling with left/right
   eigenvectors and their overlap);
3. error codes for inconsistent sizes;
4. a frequency loop run from OpenMP threads against the serial loop.
5. the Marini-Del Sole-Rubio kernel (`BSEChiOut= "mdr"`): exact for a diagonal W,
   second-order error in the off-diagonal W, and Casida with it returns its P.
6. the Nanoquanta kernel (`BSEChiOut= "nq"`), for anti-resonant rows derived from the
   resonant ones and for explicit ones: the export route (the MDR routines with Ed = E,
   the whole W part, chi0 passed for p0) equals chi0^-1 T2 chi0^-1 built term by term,
   T2(G,G') = c sum_AB s_A v(G,A) W_AB s_B conj(v(G',B))/((E_A - z)(E_B - z)); it equals
   MDR when W has no diagonal and differs from it otherwise; the exact kernel of the BSE
   with eps W differs from it at second order in eps.

Run with `python run_tests.py --fc gfortran` and, for the threaded loop,
`python run_tests.py --fc gfortran --openmp`. LAPACK and BLAS are linked with
`-llapack -lblas` unless `--lapack` says otherwise.

The end-to-end validation on bulk silicon is described in
`doc/greens_function_fxc.md`.
