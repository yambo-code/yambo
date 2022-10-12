!
 subroutine Dipole_driver_cut(Xen, Xk, Dip)
 use pars
 use com,                     ONLY : msg, grid_paths
 use LIVE_t,                  ONLY : live_timing
 use electrons,               ONLY : levels, n_spin, n_spinor, n_sp_pol
 use parallel_m,              ONLY : PAR_DIPk_ibz_index,PAR_DIPk_nibz,PAR_DIPk_bz_index,PAR_DIPk_nbz
 use R_lattice,               ONLY :  bz_samp, nXkibz,nXkbz
 use X_m,                     ONLY : X_t
 use DIPOLES,                 ONLY : DIPOLE_t
 use IO_int,                  ONLY : io_control,IO_and_Messaging_switch
 use IO_m,                    ONLY : OP_RD_CL, OP_WR_CL, VERIFY, REP
 use wave_func,               ONLY : wf_ng, WF
 use interfaces,              only : WF_load, WF_free
 use optcut,                  ONLY : DIP_iR_cut, ng_limits, setup_optcut, pscut, DIP_alloc_cut
 use drivers,                 ONLY : l_col_cut
 use timing_m,       ONLY:timing
 use ras_module,              ONLY : lras

#include<memory.h>
 !implicit none
 type(bz_samp),  intent(in)       :: Xk
 type(levels),   intent(in)       :: Xen
 type(DIPOLE_t)                   :: Dip
!ws
 integer                          :: ID, io_err, i_spin
 integer                          :: ik, i1, icfft, ivfft, ic, iv, is,i_k

 real(SP)                         :: omega, field_dir_rot(3)
 complex(SP)                      :: PS(3), spinor_avg(3)
 real(SP), parameter              :: fac = 0.70710678118654752440_SP
 character(schlen)                :: sch 
 logical                          :: use_trans_gauge
 integer,  external               :: io_DIPOLES_cut

 call timing('Dipoles_cut',OPR='start')
 !
 call section('=','Optical oscillators with cut off')

   YAMBO_FREE(PAR_DIPk_ibz_index)
!  YAMBOFREE(PAR_DIPk_bz_index)
   YAMBO_ALLOC(PAR_DIPk_ibz_index,(nXkibz))
    do i_k=1,nXkibz
      PAR_DIPk_ibz_index(i_k)=i_k
    enddo
!    YAMBOALLOC(PAR_DIPk_bz_index,(nXkbz))
!     do i_k=1,nXkbz
!      PAR_DIPk_bz_index(i_k)=i_k
!     enddo
   PAR_DIPk_nibz=nXkibz
!  PAR_DIPk_nbz=nXkbz

 io_err=-1

 call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1/),MODE=VERIFY,ID=ID)
 io_err = io_DIPOLES_cut(Dip,ID)

 if (io_err/=0) then

   call WF_load(WF,0,1,(/1,Dip%ib(2)/),(/1,Xk%nibz/),space='G',title='-Oscillators/G space')
   
   call nG_limits(PAR_DIPk_nibz)

   call DIP_alloc_cut('DIP_iR_cut',(/3,Dip%ib_lim(2),Dip%ib(2),Dip%ib(1),Dip%ib_lim(1),PAR_DIPk_nibz/))
   !
   ! Cutoff with shifted grids has been removed because it is too unstable.
   !
   if(trim(grid_paths)/='none') call warning('Cutoff with shifted grids not supported! Reverting to transverse gauge')
   !
   call Dipole_transverse_cut(Xen,Xk,Dip)
   ! 
   call WF_free(WF)
  
  call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1/),ID=ID)
  io_err = io_DIPOLES_cut(Dip,ID)
end if 
 
 call timing('Dipoles_cut',OPR='stop')

end subroutine Dipole_driver_cut
