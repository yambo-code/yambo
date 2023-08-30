!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
module YPP_interfaces
 !
 implicit none
 !
 interface
   !
#if defined _YPP_RT
   !
   subroutine  RT_OBSERVABLES_IO(what,J,P,IO_error,first_sec_only,G_lesser_only,COM_)
     use YPP_real_time,  ONLY:OBS_el
     use electrons,      ONLY:levels
     implicit none
     character(*)           :: what
     type(OBS_el), optional :: J,P
     logical,      optional :: IO_error,first_sec_only,G_lesser_only
     integer,      optional :: COM_
   end subroutine
   !
#endif
   !
   subroutine electrons_dos_elemental(k,E,bands,el_dos,dos_E,spin_fac,pdos_fac,USE_the_DbGd,QUIET,FORCE_occ)
     use pars,           ONLY:SP
     use electrons,      ONLY:levels,n_spin
     use R_lattice,      ONLY:bz_samp 
     use electrons,      ONLY:n_spin
     use YPPm,           ONLY:DOS_E_steps
     implicit none
     type(bz_samp),    intent(in) :: k
     type(levels),     intent(in) :: E
     integer,          intent(in) :: bands(2)
     real(SP),         intent(in) :: dos_E(DOS_E_steps)
     real(SP),         intent(out):: el_dos(DOS_E_steps,n_spin)
     real(SP),pointer, optional, intent(in):: spin_fac(:,:,:)
     real(SP),pointer, optional, intent(in):: pdos_fac(:,:,:)
     logical, optional,intent(in) :: USE_the_DbGd,QUIET,FORCE_occ
   end subroutine electrons_dos_elemental
   !
   subroutine excitons_sort_and_report(iq,BS_R,BS_E,BS_E_SOC_corr,EXC_spin)
     use pars,          ONLY:SP
     use BS_solvers,    ONLY:BSS_n_eig
     use YPPm,          ONLY:EXCITON_spin
     implicit none
     integer                      :: iq
     complex(SP)                  :: BS_R(BSS_n_eig),BS_E(BSS_n_eig)
     real(SP), optional, pointer  :: BS_E_SOC_corr(:,:)
     type(EXCITON_spin),optional  :: EXC_spin
   end subroutine
   !
   subroutine PROJECTION_setup( what, bands)
     character(*)       :: what
     integer   ,optional:: bands(2)
   end subroutine
   !
   subroutine k_special( LIST_only )
     logical   ,optional:: LIST_only
   end subroutine
   !
   subroutine PROJECTION_plot( what, N , Ns, bands, W, TIME, OBS_K, OBS_B)
     use pars,           ONLY:SP
     use YPPm,           ONLY:PROJECT_N_par
     use interpolate,    ONLY:GRID_k
     character(*)       :: what
     integer            :: N,Ns,bands(2)
     real(SP)  ,optional:: W(N),TIME(N)
     real(SP)  ,optional:: OBS_K(PROJECT_N_par,N,Ns)
     real(SP)  ,optional:: OBS_B(bands(1):bands(2),PROJECT_N_par,Ns)
   end subroutine
   !
 end interface
 !
end module YPP_interfaces
