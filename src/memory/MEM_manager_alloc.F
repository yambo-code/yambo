!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM DS
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
subroutine MEM_manager_alloc(what,Sz,Kn,where)
 !
 use pars,         ONLY:schlen,DP,IPL
 use com,          ONLY:secnm
 use LIVE_t,       ONLY:LIVE_message
 use stderr,       ONLY:intc
 use units,        ONLY:Kilobyte
 use openmp,       ONLY:master_thread
 use memory,       ONLY:TOT_MEM_Kb,MEM_element_copy,&
&                       MEM_err,MEM_msg,N_MEM_elements,LARGE_MEMs,N_MEM_SAVE_max,&
&                       N_MEM_SAVE_elements,N_MEM_SAVE_max,&
&                       MEM_SAVE_treshold,MAX_MEM_Kb,MEMs,&
&                       N_MEM_max_element_components
 !
 implicit none
 !
 character(*), intent(in) :: what
 integer(IPL), intent(in) :: Sz
 integer,      intent(in) :: Kn
 integer,      intent(in) :: where
 ! 
 ! Defined in the init
 !
 logical           :: just_started
 integer           :: i_SAVE,i_COMPONENT,i_SHELF
 character(schlen) :: SHELF,TRUNK,TMP_MSG
 !
 ! Work
 !
 real(DP):: LARGE_wk
 integer :: MEM_now_Kb
 !
 if (.not.master_thread) return
 !
#if defined _MEM_CHECK
 !
 call MEM_manager_init(what,.true.,Sz,i_SHELF,i_COMPONENT,i_SAVE,SHELF,TRUNK,just_started)
 !
 ! Error Message of an already allocated array and/or other errors
 !=================================================================
 if (MEM_err/=0) then
   call LIVE_message("[WARNING] Allocation of "//what//" failed with code "//trim(intc(MEM_err)))
   call LIVE_message("[WARNING] "//trim(MEM_msg))
   if (i_SHELF> 0) TMP_MSG="Object was already allocated in "//trim(MEMs(i_SHELF)%shelf)
   if (i_SHELF<=0) TMP_MSG="Object was already allocated, tough, not present in the archive."
   call LIVE_message("[WARNING] "//trim(TMP_MSG))
   return
 endif
 if (Sz==0) then
   call warning("Allocation attempt of "//what//" of zero size.")
   return
 endif
 if (Sz<0) call error("Allocation attempt of "//what//" of negative size.")
 !
 ! Database Update
 !=================
 !
 ! ALLOCATION => ADDITION
 !.......................
 !
 LARGE_wk=real(Sz,DP)*real(Kn,DP)/Kilobyte
 MEM_now_Kb=int(LARGE_wk)
 !
 if (MEM_now_Kb==0) MEM_now_Kb=1
 !
 TOT_MEM_Kb(where)=TOT_MEM_Kb(where)+MEM_now_Kb
 !
 if (TOT_MEM_Kb(where)>MAX_MEM_Kb(where)) MAX_MEM_Kb(where)=TOT_MEM_Kb(where)
 !
 if (i_SHELF==0) then
   if (N_MEM_elements<N_MEM_SAVE_max) then
     N_MEM_elements=N_MEM_elements+1
   else
     call error("Max Number of MEM elements reached.")
   endif
   MEMs(N_MEM_elements)%shelf=SHELF
   MEMs(N_MEM_elements)%use =MEM_now_Kb
   i_SHELF=N_MEM_elements
 else
   MEMs(i_SHELF)%use = MEMs(i_SHELF)%use+MEM_now_Kb
 endif
 !
 if (i_COMPONENT==0) then
   MEMs(i_SHELF)%N=MEMs(i_SHELF)%N+1
   i_COMPONENT=MEMs(i_SHELF)%N
   if (i_COMPONENT>N_MEM_max_element_components) then
     call error("Max Number of MEM elements COMPONENTS reached.")
   endif
   MEMs(i_SHELF)%desc(i_COMPONENT)=trim(secnm)
   MEMs(i_SHELF)%name(i_COMPONENT)=what
   MEMs(i_SHELF)%kind(i_COMPONENT)=Kn
   MEMs(i_SHELF)%where(i_COMPONENT)=where
   MEMs(i_SHELF)%composed(i_COMPONENT)=.FALSE.
 else
   MEMs(i_SHELF)%composed(i_COMPONENT)=.TRUE.
 endif
 MEMs(i_SHELF)%size(i_COMPONENT)=MEMs(i_SHELF)%size(i_COMPONENT)+Sz
 !
 if (MEMs(i_SHELF)%use>MEM_SAVE_treshold.and..not.just_started) then
   if (i_SAVE==0) then
     if (N_MEM_SAVE_elements<N_MEM_SAVE_max) then
       N_MEM_SAVE_elements=N_MEM_SAVE_elements+1
     else
       call error("Max Number of LARGE MEM elements reached.")
     endif
     call MEM_element_copy(MEMs(i_SHELF),LARGE_MEMs(N_MEM_SAVE_elements))
   else
     call MEM_element_copy(MEMs(i_SHELF),LARGE_MEMs(i_SAVE))
   endif
 endif
 !
 if(.not.just_started) call MEM_manager_messages(what,.true.,MEM_now_Kb,where)
 !
#else
 call MEM_error(what)
#endif
 !
end subroutine MEM_manager_alloc
