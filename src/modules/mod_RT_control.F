! 
!        Copyright (C) 2000-2018 the YAMBO team
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
 use pars,       ONLY:SP,lchlen,schlen
 use units,      ONLY:AUT2FS
 use real_time,  ONLY:l_NE_dynamics,l_elph_scatt
 !
#include<memory.h>
 !
 integer                  :: n_RT_descriptions = 0
 character(lchlen)        :: RT_description(100)
 !
 integer                  :: n_TIME_STEP_CONF_descriptions = 0
 character(lchlen)        :: TIME_STEP_CONF_description(100)
 !
 integer                  :: IO_OBS_cache_steps=1
 !
 integer                  :: ID(8)
 integer                  :: NE_i_start_time    = 1   ! restart point
 integer                  :: last_it_evaluated  = 0   ! When RT_dynamics_jumps are non zero this is 
 !                                                    ! the last evaluated point
 type RT_IO_t
   integer               :: N              = 0
   integer               :: INTERVAL_steps =-1
   integer               :: last_point     = 0
   real(SP)              :: INTERVAL_time
   real(SP)              :: INTERVAL_time_SAVE
   real(SP), allocatable :: Time(:)
 end type
 !
 logical                  :: SAVE_G_history
 !
 type(RT_IO_t),save  :: OBS_RT_IO_t
 type(RT_IO_t),save  :: OUTPUT_RT_IO_t
 type(RT_IO_t),save  :: GF_RT_IO_t
 !
 ! J, P and M (spin) cache 
 !
 real(SP), allocatable    :: J_cache(:,:)
 real(SP), allocatable    :: P_cache(:,:)
 real(SP), allocatable    :: Ms_cache(:,:)
 real(SP), allocatable    :: Ml_cache(:,:)
 real(SP), allocatable    :: D_cache(:,:)
 !
 ! TD-ABS and RT-BSE [yambo]
 !
 logical                 :: NEQ_Kernel
 logical                 :: NEQ_Residuals
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
   subroutine RT_apply(bands,E,K,what,VERBOSE,IO_COM,Time,keep_RT_carriers)
    use pars,           ONLY:SP
    use electrons,      ONLY:levels
    use R_lattice,      ONLY:bz_samp
    implicit none
    integer      :: bands(2)
    type(levels) :: E
    type(bz_samp):: K
    character(*),optional :: what ! X,K,G
    logical ,    optional :: VERBOSE
    integer ,    optional :: IO_COM
    real(SP),    optional :: Time
    logical ,    optional :: keep_RT_carriers
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
   subroutine RT_alloc_control()
     !
     use pars,           ONLY:rZERO
     use real_time,      ONLY:NE_steps
     use electrons,      ONLY:n_spin
     use ELPH,           ONLY:ph_modes
     !
     if(l_NE_dynamics) then
       !
       YAMBO_ALLOC(J_cache,(6,IO_OBS_cache_steps))
       J_cache=rZERO
       YAMBO_ALLOC(P_cache,(6,IO_OBS_cache_steps))
       P_cache=rZERO
       if(l_elph_scatt) then
         YAMBO_ALLOC(D_cache,(2*ph_modes,IO_OBS_cache_steps))
         D_cache=rZERO
       endif
       if(n_spin>1) then
         YAMBO_ALLOC(Ms_cache,(6,IO_OBS_cache_steps))
         Ms_cache=rZERO
         YAMBO_ALLOC(Ml_cache,(6,IO_OBS_cache_steps))
         Ml_cache=rZERO
       endif
       !
     endif
     !
     YAMBO_FREE(OBS_RT_IO_t%Time)
     !
     YAMBO_ALLOC(OBS_RT_IO_t%Time,(NE_steps))
     OBS_RT_IO_t%Time=rZERO
     !
     YAMBO_FREE(GF_RT_IO_t%Time)
     !
     YAMBO_ALLOC(GF_RT_IO_t%Time,(NE_steps))
     GF_RT_IO_t%Time=rZERO
     !
   end subroutine RT_alloc_control
   !
   subroutine RT_free_control
     !
     YAMBO_FREE(J_cache)
     YAMBO_FREE(P_cache)
     YAMBO_FREE(Ms_cache)
     YAMBO_FREE(Ml_cache)
     YAMBO_FREE(OBS_RT_IO_t%Time)
     YAMBO_FREE(GF_RT_IO_t%Time)
     !
   end subroutine RT_free_control
   !
   logical function RT_do_it(what)
     !
     use real_time, ONLY:NE_i_time,RT_step,NE_time_step_update_jump,NE_time_step_update_last_point
     character(*) :: what
     !
     ! Work Space
     !
     integer :: DO_IT_steps,i_grp,IT_last
     !
     RT_do_it= .FALSE.
     IT_last = 1
     !
     if (what=="OBS") then
       DO_IT_steps=OBS_RT_IO_t%INTERVAL_steps
       IT_last    =OBS_RT_IO_t%last_point
     endif
     if (what=="OUT") then
       DO_IT_steps=OUTPUT_RT_IO_t%INTERVAL_steps
       IT_last    =OUTPUT_RT_IO_t%last_point
     endif
     if (what=="G") then
       DO_IT_steps=GF_RT_IO_t%INTERVAL_steps
       IT_last    =GF_RT_IO_t%last_point
     endif
     if (what=="deltaT") then
       DO_IT_steps=nint(NE_time_step_update_jump/RT_step)
       IT_last    =NE_time_step_update_last_point
     endif
     !
     ! In RESTART mode the GF and J/P/C databases could be not time aligned
     !
     if (IT_last>=NE_i_time) return
     !
     if (DO_IT_steps<0) then
       RT_do_it=.TRUE. 
       return
     endif
     !
     i_grp=nint(real(NE_i_time-IT_last)/real(DO_IT_steps))
     !
     RT_do_it= (NE_i_time-IT_last)==(i_grp*DO_IT_steps)
     !
   end function
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
   subroutine SETUP_RT_IO_type(TYP,INIT)
     !
     use real_time, ONLY:NE_tot_time,RT_dyn_step,RT_step
     type(RT_IO_t) :: TYP
     logical       :: INIT
     !
     if (INIT) TYP%INTERVAL_time=TYP%INTERVAL_time_SAVE
     !
     ! Initialization always using RT_step
     !
     if (TYP%INTERVAL_time<=0)           TYP%INTERVAL_time =max(NE_tot_time/1000._SP,RT_step)
     !
     if (TYP%INTERVAL_time<=RT_dyn_step) TYP%INTERVAL_time = RT_dyn_step
     !
     TYP%INTERVAL_time  =            nint(TYP%INTERVAL_time/RT_dyn_step)*RT_dyn_step
     !
     ! The steps are always relative to RT_step
     !
     TYP%INTERVAL_steps             =nint(TYP%INTERVAL_time/RT_step)
     !
     if (INIT) TYP%INTERVAL_time_SAVE=TYP%INTERVAL_time
     !
     if (INIT.and.TYP%last_point==0) TYP%last_point=-TYP%INTERVAL_steps+1
     !
   end subroutine
   !
end module RT_control
