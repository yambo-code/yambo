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
module rt_ctl
 !
 use pars,       ONLY:SP,lchlen
 use units,      ONLY:FS2AUT
 !
 implicit none
 !
 integer                  :: n_RT_descriptions = 0
 character(lchlen)        :: RT_description(100)
 !
 integer,  parameter      :: J_and_P_cache_size=18
 !
 integer                  :: ID(8)
 integer                  :: it_dyn
 integer                  :: it_start           = 1   ! restart point
 integer                  :: last_it_evaluated  = 0   ! When RT_dynamics_jumps are non zero this is 
 !                                                    ! the last evaluated point
 integer                  :: RT_IO_JPO_steps    = -1
 integer                  :: RT_IO_output_steps = -1
 integer                  :: RT_IO_G_steps      = -1
 integer                  :: RT_IO_steps_DB     = -1
 real(SP)                 :: RT_IO_time(3)             ! Three values for J,P,OCC - OUPUT - GF's
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
   subroutine RT_alloc_ctl()
     !
     use memory_m,       ONLY:mem_est
     use real_time,      ONLY:l_NE_dynamics
     !
     if(l_NE_dynamics) then
       allocate(J_and_P_cache(J_and_P_cache_size,RT_IO_JPO_steps))
       call mem_est('Current_Cache',(/size(J_and_P_cache)/),(/SP/))
     endif
     !
   end subroutine RT_alloc_ctl
   !
   subroutine RT_free_ctl
     !
     use memory_m,       ONLY:mem_est
     use real_time,      ONLY:l_NE_dynamics
     !
     if(l_NE_dynamics) then
       deallocate(J_and_P_cache)
       call mem_est('Current_Cache')
     endif
     !
   end subroutine RT_free_ctl
   !
   logical function RT_do_IO(what)
     !
     use real_time, ONLY:NE_i_time
     character(*) :: what
     !
     if (what=="J".or.what=="P".or.what=="CARRIERS") RT_do_IO=mod(NE_i_time-1,RT_IO_JPO_steps)==0
     if (what=="OUT") RT_do_IO=mod(NE_i_time-1,RT_IO_output_steps)==0
     if (what=="G")   RT_do_IO=mod(NE_i_time-1,RT_IO_G_steps)==0
     !
   end function
   !
   !
end module rt_ctl
