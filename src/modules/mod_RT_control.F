! 
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS
! 
! This file is distributed under the terms of the GNU 
! General Public License. You can redistribute it and/or 
! modify it under the terms of the GNU General Public 
! License as published by the Free Software Foundation; 
! either version 2, or (at your option) any later version.
!
! This program is distributed in the hope that it will 
! be useful, but WITHOUT ANY WARRANTY; without even the 
! implied warranty of MERCHANTABILITY or FITNESS FOR A 
! PARTICULAR PURPOSE.  See the GNU General Public License 
! for more details.
!
! You should have received a copy of the GNU General Public 
! License along with this program; if not, write to the Free 
! Software Foundation, Inc., 59 Temple Place - Suite 330,Boston, 
! MA 02111-1307, USA or visit http://www.gnu.org/copyleft/gpl.txt.
!
module RT_control
 !
 use pars,       ONLY:SP,lchlen,schlen,rZERO
 use units,      ONLY:AUT2FS
 use real_time,  ONLY:l_NE_dynamics,l_NE_with_fields,l_elph_scatt
 !
#include<memory.h>
 !
 integer                  :: CACHE_OBS_last_point
 real(SP)                 :: CACHE_OBS_INTERVAL_time
 real(SP)                 :: CACHE_OBS_INTERVAL_time_INPUT
 integer                  :: CACHE_OBS_steps
 integer                  :: CACHE_OBS_steps_now 
 !
 integer                  :: ID(8)                    ! (/3 GFPAR,4 OBS,5 GFSER,6 OCC,7 THETA,8 REF)
 character(lchlen)        :: RT_DB_file_name(8) = " "
 integer                  :: NE_i_start_time    = 1   ! restart point
 integer                  :: last_it_evaluated  = 0   ! When RT_dynamics_jumps are non zero this is 
 !                                                    ! the last evaluated point
 type RT_IO_t
   character(schlen)     :: name
   integer               :: N
   integer               :: INTERVAL_steps=0
   integer               :: last_point
   logical               :: first_IO(3)=.TRUE. ! Sec #1, #2 and #3
   real(SP)              :: INTERVAL_time
   real(SP)              :: INTERVAL_time_INPUT
   real(SP), allocatable :: Time(:)
 end type
 !
 logical                  :: RT_NAN_found=.FALSE.
 logical                  :: SAVE_G_history
 !
 type(RT_IO_t),save  :: OBS_RT_IO_t
 type(RT_IO_t),save  :: CARR_RT_IO_t
 type(RT_IO_t),save  :: OUTPUT_RT_IO_t
 type(RT_IO_t),save  :: GF_RT_IO_t
 !
 ! J, P and M (spin) cache 
 !
 complex(SP), allocatable    :: J_cache(:,:)
 complex(SP), allocatable    :: P_cache(:,:)
 complex(SP), allocatable    :: Ms_cache(:,:)
 complex(SP), allocatable    :: Ml_cache(:,:,:)
 complex(SP), allocatable    :: A_cache(:,:,:)
 !
 ! TD-ABS and RT-BSE [yambo]
 !
 logical                 :: NEQ_Kernel
 logical                 :: NEQ_Residuals
 character(schlen)       :: RT_ctl_db(4)
 integer                 :: RT_ctl_interp_neigh(4)=1
 ! 
 interface
   !
   subroutine RT_output(what,VALUEs,TIME,E)
     use pars,       ONLY:SP
     character(*),       intent(in) :: what
     real(SP), optional, intent(in) :: VALUEs(:) 
     real(SP), optional, intent(in) :: TIME
     real(SP), optional, intent(in) :: E
   end subroutine
   !
   subroutine RT_apply(bands,E,K,what,VERBOSE,IO_COM,Time,keep_RT_carriers,TIMES_ONLY)
    use pars,           ONLY:SP
    use electrons,      ONLY:levels
    use R_lattice,      ONLY:bz_samp
    implicit none
    integer     , intent(in)    :: bands(2)
    type(levels), intent(inout) :: E
    type(bz_samp), intent(in)   :: K
    character(*),optional, intent(in) :: what ! X,K,G
    logical ,    optional, intent(in) :: VERBOSE
    integer ,    optional, intent(in) :: IO_COM
    real(SP),    optional, intent(in) :: Time
    logical ,    optional, intent(in) :: keep_RT_carriers
    logical ,    optional, intent(in) :: TIMES_ONLY
   end subroutine
   !
   subroutine RT_carriers_object(RT,E,K,DB_id,WHAT)
     use real_time,  ONLY:RT_t
     use R_lattice,  ONLY:bz_samp
     use electrons,  ONLY:levels
     type(RT_t),    intent(in)           :: RT
     type(bz_samp), intent(in), optional :: K
     type(levels),  intent(in), optional :: E
     integer,       intent(in), optional :: DB_id
     character(*),  intent(in), optional :: WHAT
   end subroutine RT_carriers_object
   !
 end interface
 !
 contains
   !
   subroutine RT_control_alloc(what)
     !
     use pars,           ONLY:cZERO,rZERO
     use real_time,      ONLY:NE_steps
     use electrons,      ONLY:n_spin
     !
     character(*), intent(in) :: what
     !
     if(trim(what)=="cache" .or. trim(what)=="all") then
       !
       YAMBO_ALLOC(J_cache,(3,CACHE_OBS_steps))
       J_cache=cZERO
       YAMBO_ALLOC(P_cache,(3,CACHE_OBS_steps))
       P_cache=cZERO
       if(n_spin>1) then
         YAMBO_ALLOC(Ms_cache,(3,CACHE_OBS_steps))
         Ms_cache=cZERO
         YAMBO_ALLOC(Ml_cache,(3,2,CACHE_OBS_steps))
         Ml_cache=cZERO
       endif
       if(l_NE_with_fields) then
         YAMBO_ALLOC(A_cache,(3,3,CACHE_OBS_steps))
         A_cache=cZERO
       endif
       !
     endif
     !
     if(trim(what)=="times" .or. trim(what)=="all") then
       !
       YAMBO_ALLOC(OBS_RT_IO_t%Time,(NE_steps))
       OBS_RT_IO_t%Time=rZERO
       !
       YAMBO_ALLOC(CARR_RT_IO_t%Time,(NE_steps))
       CARR_RT_IO_t%Time=rZERO
       !
       YAMBO_ALLOC(GF_RT_IO_t%Time,(NE_steps))
       GF_RT_IO_t%Time=rZERO
       !
     endif
     !
   end subroutine RT_control_alloc
   !
   subroutine RT_control_free(what)
     !
     character(*), intent(in) :: what
     !
     if(trim(what)=="cache" .or. trim(what)=="all") then
       YAMBO_FREE(J_cache)
       YAMBO_FREE(P_cache)
       YAMBO_FREE(Ms_cache)
       YAMBO_FREE(Ml_cache)
       YAMBO_FREE(A_cache)
     endif
     if(trim(what)=="times" .or. trim(what)=="all") then
       YAMBO_FREE(OBS_RT_IO_t%Time)
       YAMBO_FREE(CARR_RT_IO_t%Time)
       YAMBO_FREE(GF_RT_IO_t%Time)
     endif
     !
   end subroutine RT_control_free
   !
   integer function T2I(TIME,STEP)
     !
     use real_time, ONLY:RT_step
     !
     real(SP)          :: TIME
     real(SP), optional:: STEP
     !
     if (present(STEP)) then
       T2I=nint(TIME/STEP)+1
     else
       T2I=nint(TIME/RT_step)+1
     endif
     !
   end function
   !
   real(SP) function TIME_adjust(TIME,STEP)
     !
     use real_time, ONLY:RT_step
     !
     real(SP)          :: TIME
     real(SP), optional:: STEP
     !
     if (     present(STEP)) TIME_adjust=real(nint(TIME/STEP),SP)*STEP
     if (.not.present(STEP)) TIME_adjust=real(nint(TIME/RT_step),SP)*RT_step
     !
   end function
   !
   real(SP) function STEP_adjust(TIME,STEP1,STEP2)
     !
     use real_time, ONLY:RT_step
     !
     real(SP)           :: TIME,STEP1
     real(SP), optional :: STEP2
     !
     integer  :: N1,N2,i1
     real(SP) :: STEP2_
     !
     if (     present(STEP2)) STEP2_=STEP2 
     if (.not.present(STEP2)) STEP2_=RT_step
     !
     N1=nint(TIME/STEP1)
     N2=nint(TIME/STEP2_)
     !
     do i1=N1,N2
       if( N2/i1*i1==N2) exit 
     enddo
     !
     STEP_adjust=TIME/real(i1,SP)
     !
   end function
   !
   character(schlen) function TIME_string(TIME)
     !
     use stderr, ONLY:real2ch
     !
     real(SP)      :: TIME,TIME_fs
     character(14) :: tmp_ch
     integer       :: i_c
     !
     TIME_fs=TIME*AUT2FS
     if (TIME_fs<1._SP) then
       write(tmp_ch,'(f5.1,a)') TIME_fs*1000._SP,"_as"
     else if (TIME_fs<999._SP) then
       write(tmp_ch,'(f8.4,a)') TIME_fs,"_fs"
     else 
       write(tmp_ch,'(f11.7,a)') TIME_fs/1000._SP,"_ps"
     endif
     !
     do i_c=1,11
       if (tmp_ch(i_c:i_c)=="s") cycle
       if (tmp_ch(i_c:i_c)==" ") tmp_ch(i_c:i_c)="_"
     enddo
     !
     TIME_string=tmp_ch
     !
   end function
   !
   subroutine SETUP_RT_IO_type(TYP,who,what)
     !
     use real_time, ONLY:NE_tot_time,RT_dyn_step,RT_step,NE_i_time
     !
     type(RT_IO_t) :: TYP
     character(*), optional :: what,who
     !
     if (present(what)) then
       if (what=="INIT") then
         if (allocated(TYP%Time)) then
           YAMBO_FREE(TYP%Time)
         endif
         TYP%name         =who
         TYP%INTERVAL_time=TYP%INTERVAL_time_INPUT
       endif
     endif
     !
     ! Initialization always using RT_step
     !
     if (TYP%INTERVAL_time<=0)           TYP%INTERVAL_time =max(NE_tot_time/1000._SP,RT_step)
     !
     if (TYP%INTERVAL_time<=RT_dyn_step) TYP%INTERVAL_time = RT_dyn_step
     !
     TYP%INTERVAL_time  = nint(TYP%INTERVAL_time/RT_dyn_step)*RT_dyn_step
     !
     ! The steps are always relative to RT_step
     !
     TYP%INTERVAL_steps = nint(TYP%INTERVAL_time/RT_step)
     !
     if (what=="INIT"   ) TYP%last_point     = 0
     if (what=="RESTART") TYP%last_point     = NE_i_start_time-1
     if (what=="UPDATE" ) TYP%last_point     = NE_i_time
     !
     if (present(what)) then
       if (what=="INIT") then
         TYP%INTERVAL_time_INPUT=TYP%INTERVAL_time
         TYP%N                  = 0
         TYP%first_IO           = .TRUE.
       endif
     endif
     !
   end subroutine
   !
   subroutine RT_carriers_to_RT_k( K )
     use R_lattice,      ONLY:bz_samp,bz_samp_reset
     use real_time,      ONLY:RT_carriers
     type(bz_samp) :: K
     integer       :: ik
     call bz_samp_reset(K)
     K%units='i'
     K%description="RT_GRID"
     K%nibz=RT_carriers%nk
     YAMBO_ALLOC(K%pt,(K%nibz,3))
     YAMBO_ALLOC(K%weights,(K%nibz))
     do ik=1,K%nibz
       K%pt(ik,:)   =RT_carriers%kpt(ik,:)
       K%weights(ik)=RT_carriers%k_weight(ik)
     enddo
   end subroutine
   !
   subroutine RT_carriers_to_RT_E(E_eq,E,what)
     use electrons,      ONLY:levels,E_reset,n_sp_pol
     use real_time,      ONLY:RT_carriers
     type(levels) :: E,E_eq
     character(*) :: what
     integer      :: i_RT,i_sp_pol,ib,ik_RT
     if (.not.allocated(E%E)) then
       call E_reset(E)
       E%nk=RT_carriers%nk
       E%nb=RT_carriers%nb(2)
       E%nbf=E_eq%nbf
       E%nbm=E_eq%nbm
       E%E_Fermi  =E_eq%E_Fermi
       E%E_VBM    =E_eq%E_VBM
       E%E_CBm    =E_eq%E_CBm
       E%E_dir_gap=E_eq%E_dir_gap
       YAMBO_ALLOC(E%E,(E%nb,E%nk,n_sp_pol))
       YAMBO_ALLOC(E%f,(E%nb,E%nk,n_sp_pol))
       E%E=rZERO
       E%f=rZERO
     endif
     do i_RT=1,RT_carriers%nstates
       ib      =RT_carriers%table(i_RT,1)
       ik_RT   =RT_carriers%table(i_RT,3)
       i_sp_pol=RT_carriers%table(i_RT,4)
       if (what=="Eo") E%E(ib,ik_RT,i_sp_pol)=RT_carriers%E_bare(i_RT)
       if (what=="E" ) E%E(ib,ik_RT,i_sp_pol)=RT_carriers%E_bare(i_RT)+RT_carriers%delta_E(i_RT)
       if (what=="fo") E%f(ib,ik_RT,i_sp_pol)=RT_carriers%f_bare(i_RT)
       if (what=="f" ) E%f(ib,ik_RT,i_sp_pol)=RT_carriers%f_bare(i_RT)+RT_carriers%delta_f(i_RT)
       if (what=="df") E%f(ib,ik_RT,i_sp_pol)=RT_carriers%delta_f(i_RT)
     enddo
   end subroutine
   !
end module RT_control
