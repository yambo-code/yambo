!
! License-Identifier: GPL
!
! Copyright (C) 2026 The Yambo Team
!
! Authors (see AUTHORS file for details):
!
module Chi_m
 !
 ! Irreducible polarizability in the plane-wave basis, with and without quasiparticle
 ! dressing, and the exchange-correlation kernel that maps one onto the other (ndb.Chi).
 !
 !   chi0(q,G,G';w)  irreducible bubble with the Kohn-Sham levels
 !   P   (q,G,G';w)  irreducible bubble with the quasiparticle levels applied to the
 !                   X energies through XfnQPdb (energies, and Z factors when the
 !                   database provides them: see X_irredux_residuals, Z_eh_occ)
 !   fxc (q,G,G';w)  = chi0^-1 - P^-1
 !
 ! In LEVELS mode both bubbles are computed by X_irredux. G0/COHSEX/DYSON
 ! instead use explicit finite-q vertices with analytic poles or spectral pairs.
 ! Both routes use yambo's normalization and vertex convention:
 ! Adler-Wiser sum over transitions normalized by 1/(N_kbz V_cell), rho^*(G) rho(G') in
 ! the residual. LEVELS uses X_GreenF_analytical with the requested ordering.
 ! In LEVELS mode the q=0 head and wings use the dipole limit at q0 (X%q0), as
 ! everywhere else in yambo.
 !
 ! The inversion is done in double precision on the symmetrized matrices
 !
 !   chi~ = D chi D,  D = diag( sqrt(4 pi)/|q+G| )     (|q+G| = bare_qpg, cutoff included)
 !
 ! whose head stays O(1) at q->0, and fxc = D (chi0~^-1 - P~^-1) D. The stored fxc is the
 ! bare (unsymmetrized) G-space kernel in Hartree atomic units. Its q->0 head is evaluated
 ! at q0 in LEVELS mode. G modes currently exclude q=0 and a response vertex.
 !
 ! Self-test: with no XfnQPdb the two bubbles are computed from identical levels through
 ! identical code and fxc must vanish exactly.
 !
 use pars,        ONLY:SP,DP,schlen
 use descriptors, ONLY:IO_desc
 use electrons,   ONLY:levels
 !
#include<y_memory.h>
 !
 type(IO_desc)     :: Chi_desc
 !
 logical           :: l_Chi_fxc           =.FALSE.  ! ChiFxc input flag
 logical           :: Chi_header_done =.FALSE.
 logical           :: Chi_QP_applied  =.FALSE.
 ! Optical q->0 direction and numerical norm used for the density head.  These
 ! must be retained with fxc because its head and wings scale with that norm.
 real(SP)          :: Chi_optical_q0(3)=0._SP
 logical           :: Chi_optical_q0_known=.FALSE.
 integer           :: Chi_ng          =0
 integer           :: Chi_n_freqs     =0
 character(schlen) :: Chi_QP_action   ="none"
 character(schlen) :: Chi_convention  ="chi0,P: yambo Xo [1/(Nk V)]; fxc=chi0^-1-P^-1 [Ha], bare G-space"
 ! LEVELS preserves the existing QP-energy path. COHSEX uses exact unit-weight
 ! poles from a native static ndb.QP; DYSON reads conventional retarded ndb.G.
 character(schlen) :: Chi_G_mode="LEVELS",Chi_G_db=""
 character(schlen) :: Chi_G_axis="IMAG"
 character(schlen) :: Chi_G_source="none"   ! self-energy kind of the ndb.QP read in COHSEX/QP mode
 integer :: Chi_G_nu_steps=16
 real(SP) :: Chi_G_nu_range(2)=(/0._SP,1._SP/)
 real(SP) :: Chi_G_norm_tol=0.05_SP,Chi_G_check_tol=1.E-4_SP
 real(SP) :: Chi_rcond_floor=1.E-12_SP,Chi_G_mu=0._SP,Chi_G_kbt=0._SP
 real(SP) :: Chi_G0_error=0._SP
 real(SP), allocatable :: Chi_rcond(:,:),Chi_G_norm(:,:,:)
 real(DP), allocatable :: Chi_G_energy(:,:,:,:),Chi_G_weight(:,:,:,:),Chi_G_occupation(:,:,:,:)
 real(DP), allocatable :: Chi_G_static_energy(:,:,:),Chi_G_static_occupation(:,:,:)
 !
 ! Kohn-Sham levels, duplicated before the QP corrections are applied to the X levels
 !
 type(levels)      :: Chi_KS_levels
 !
 ! Per q-point (IBZ)
 !
 complex(SP), allocatable :: Chi_freqs(:)      ! (n_freqs)
 complex(SP), allocatable :: Chi_0(:,:,:)      ! chi0 (ng,ng,n_freqs)
 complex(SP), allocatable :: Chi_P(:,:,:)      ! P    (ng,ng,n_freqs)
 complex(SP), allocatable :: Chi_fxc(:,:,:)    ! fxc  (ng,ng,n_freqs)
 !
 ! Frequency-dependent Casida (BSKmod="CHI", BSEChiDyn). K_kernel leaves fxc out of the
 ! BSE matrix and keeps the transition vertices instead; K_Chi_dynamic projects fxc(w)
 ! at every BSE frequency. Index t is the resonant transition (BS_mat row/column t).
 !   Chi_dyn_V(:,t,1) excitation vertex    (O_tddft_R, mode "R")
 !   Chi_dyn_V(:,t,2) vertex of BS_mat row/column N+t: with the anti-resonant blocks
 !                    derived from the resonant ones (q=0) the de-excitation vertex of
 !                    transition t (mode "C"); with explicit anti-resonant transitions
 !                    (finite q) the vertex of anti-resonant transition t (mode "R")
 !   Chi_dyn_s(t)     sqrt of the transition occupation factor
 !   Chi_dyn_filled(t,1:2) vertex collected for row t / N+t
 !   Chi_dyn_c        Co/(4 pi) of K_kernel
 complex(DP), allocatable :: Chi_dyn_V(:,:,:)
 real(DP),    allocatable :: Chi_dyn_s(:)
 logical,     allocatable :: Chi_dyn_filled(:,:)
 real(DP)                 :: Chi_dyn_c=0._DP
 !
 ! Occupations of the BSE levels behind a BSEChiOut export: Ken%f on the BSE bands, every
 ! IBZ k and spin, the full / partly filled bands and the temperature. The exact kernel
 ! holds only with these occupations, so the Casida run (BSKmod="CHI") checks them.
 real(SP), allocatable    :: Chi_occupations(:,:,:)
 integer                  :: Chi_occ_bands(2)=0,Chi_occ_nk=0,Chi_occ_nsp=0,Chi_occ_nbf=0,Chi_occ_nbm=0
 real(SP)                 :: Chi_occ_Tel=0._SP
 !
 contains
   !
   subroutine Chi_alloc(ng,nw)
     integer, intent(in) :: ng,nw
     call Chi_free()
     YAMBO_ALLOC(Chi_freqs,(nw))
     YAMBO_ALLOC(Chi_0,(ng,ng,nw))
     YAMBO_ALLOC(Chi_P,(ng,ng,nw))
     YAMBO_ALLOC(Chi_fxc,(ng,ng,nw))
     YAMBO_ALLOC(Chi_rcond,(2,nw))
   end subroutine Chi_alloc
   !
   subroutine Chi_free()
     YAMBO_FREE(Chi_freqs)
     YAMBO_FREE(Chi_0)
     YAMBO_FREE(Chi_P)
     YAMBO_FREE(Chi_fxc)
     YAMBO_FREE(Chi_rcond)
   end subroutine Chi_free
   subroutine Chi_occ_alloc()
     call Chi_occ_free(.FALSE.)
     YAMBO_ALLOC(Chi_occupations,(Chi_occ_bands(2)-Chi_occ_bands(1)+1,Chi_occ_nk,Chi_occ_nsp))
   end subroutine Chi_occ_alloc
   !
   subroutine Chi_occ_free(reset)
     logical, intent(in) :: reset
     YAMBO_FREE(Chi_occupations)
     if (.not.reset) return
     Chi_occ_bands=0
     Chi_occ_nk=0
     Chi_occ_nsp=0
     Chi_occ_nbf=0
     Chi_occ_nbm=0
     Chi_occ_Tel=0._SP
   end subroutine Chi_occ_free
   !
   subroutine Chi_dyn_free()
     if (allocated(Chi_dyn_V)) deallocate(Chi_dyn_V)
     if (allocated(Chi_dyn_s)) deallocate(Chi_dyn_s)
     if (allocated(Chi_dyn_filled)) deallocate(Chi_dyn_filled)
     Chi_dyn_c=0._DP
   end subroutine Chi_dyn_free
   !
   ! Spectra live across q points and are released by the driver.
   subroutine Chi_G_free()
     if (allocated(Chi_G_energy)) deallocate(Chi_G_energy)
     if (allocated(Chi_G_weight)) deallocate(Chi_G_weight)
     if (allocated(Chi_G_occupation)) deallocate(Chi_G_occupation)
     if (allocated(Chi_G_norm)) deallocate(Chi_G_norm)
     if (allocated(Chi_G_static_energy)) deallocate(Chi_G_static_energy)
     if (allocated(Chi_G_static_occupation)) deallocate(Chi_G_static_occupation)
   end subroutine
   !
end module Chi_m
