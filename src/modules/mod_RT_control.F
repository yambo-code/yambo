! 
!        Copyright (C) 2000-2016 the YAMBO team
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
 use units,      ONLY:FS2AUT
 !
 implicit none
 !
 integer                  :: n_RT_descriptions = 0
 character(lchlen)        :: RT_description(100)
 !
 integer                  :: n_TIME_STEP_CONF_descriptions = 0
 character(lchlen)        :: TIME_STEP_CONF_description(100)
 !
 integer,  parameter      :: J_and_P_cache_size=18
 !
 integer                  :: ID(8)
 integer                  :: it_start           = 1   ! restart point
 integer                  :: last_it_evaluated  = 0   ! When RT_dynamics_jumps are non zero this is 
 !                                                    ! the last evaluated point
 type RT_IO_t
   !
   character(schlen)     :: what
   integer               :: N_IO_times    = 0
   integer               :: N_EVAL_times  = 0
   integer               :: IO_time_steps =-1
   real(SP)              :: IO_time_interval
   real(SP), allocatable :: IO_time(:)
   !
 end type
 !
 type(RT_IO_t),save  :: JPO_RT_IO_t
 type(RT_IO_t),save  :: OUTPUT_RT_IO_t
 type(RT_IO_t),save  :: GF_RT_IO_t
 !
 ! J and P cache 
 !
 real(SP), allocatable    :: J_and_P_cache(:,:)
 !
 ! Transient Absorption [ypp]
 !
 real(SP)                 :: OCC_deltaT
 real(SP)                 :: OCC_T_range(2)
 real(SP),allocatable     :: delta_f(:,:,:)
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
     use memory_m,       ONLY:mem_est
     use real_time,      ONLY:l_NE_dynamics
     !
     if(l_NE_dynamics) then
       allocate(J_and_P_cache(J_and_P_cache_size,JPO_RT_IO_t%IO_time_steps))
       call mem_est('Current_Cache',(/size(J_and_P_cache)/),(/SP/))
     endif
     !
   end subroutine RT_alloc_control
   !
   subroutine RT_free_control
     !
     use memory_m,       ONLY:mem_est
     use real_time,      ONLY:l_NE_dynamics
     !
     if(l_NE_dynamics) then
       deallocate(J_and_P_cache)
       call mem_est('Current_Cache')
     endif
     !
   end subroutine RT_free_control
   !
   logical function RT_do_it(what)
     !
     use real_time, ONLY:NE_i_time,RT_dyn_step,RT_step,NE_time_step_update_jump
     character(*) :: what
     !
     ! Work Space
     !
     integer :: DO_IT_steps,i_grp
     !
     RT_do_it=.FALSE.
     !
     if (what=="J".or.what=="P".or.what=="CARRIERS") DO_IT_steps=JPO_RT_IO_t%IO_time_steps
     if (what=="OUT")      DO_IT_steps=OUTPUT_RT_IO_t%IO_time_steps
     if (what=="G")        DO_IT_steps=GF_RT_IO_t%IO_time_steps
     if (what=="deltaT")   DO_IT_steps=nint(NE_time_step_update_jump/RT_step)
     !
     if (DO_IT_steps<0) then
       RT_do_it=.TRUE. 
       return
     endif
     !
     i_grp=nint(real(NE_i_time-1)/real(DO_IT_steps))
     !
     if (NE_i_time-1>=i_grp*DO_IT_steps.and.NE_i_time-1-nint(RT_dyn_step/RT_step)<i_grp*DO_IT_steps) RT_do_it=.TRUE.
     if (NE_i_time==0) RT_do_it=.TRUE.
     !
   end function
   !
end module RT_control
