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
 use units,        ONLY:Kilobyte
 use openmp,       ONLY:master_thread
 use memory,       ONLY:TOT_MEM_Kb,MEM_element_copy,&
&                       MEM_err,N_MEM_elements,LARGE_MEMs,N_MEM_SAVE_max,&
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
 character(schlen) :: SHELF,TRUNK
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
   if (i_SHELF> 0) call warning("Allocation of "//what//" failed. Object was already allocated in "//trim(MEMs(i_SHELF)%shelf))
   if (i_SHELF<=0) call warning("Allocation of "//what//" failed. Object was already allocated, tough, not present in the archive.")
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
 if (MEM_err/=0) call error("Allocation of "//what//" failed")
#endif
 !
end subroutine MEM_manager_alloc
!
subroutine MEM_manager_free(what,Sz)
 !
 use pars,         ONLY:schlen,DP,IPL
 use units,        ONLY:Kilobyte
 use openmp,       ONLY:master_thread
 use memory,       ONLY:TOT_MEM_Kb,MEM_element_copy,N_MEM_elements,MEMs
 !
 implicit none
 !
 character(*), intent(in) :: what
 integer(IPL), intent(in) :: Sz
 ! 
 ! Defined in the init
 !
 logical           :: just_started
 integer           :: i_SAVE,i_COMPONENT,i_SHELF
 character(schlen) :: SHELF,TRUNK
 !
 ! Work space
 !
 logical :: verbose=.false.
 real(DP):: LARGE_wk
 integer :: i_mem,i_el,MEM_now_Kb
 integer :: where
 !
 if (.not.master_thread) return
 !
#if defined _MEM_CHECK
 !
 if(Sz==-1 .and. verbose) then
   call warning("De-allocation attempt of "//what//" which was not allocated.")
   return
 endif
 !
 call MEM_manager_init(what,.false.,Sz,i_SHELF,i_COMPONENT,i_SAVE,SHELF,TRUNK,just_started)
 !
 if (.not. (i_SHELF/=0.and.i_COMPONENT/=0) ) return
 !
 LARGE_wk=real(Sz,DP)*real(MEMs(i_SHELF)%kind(i_COMPONENT),DP)/Kilobyte
 where=MEMs(i_SHELF)%where(i_COMPONENT)
 MEM_now_Kb=int(LARGE_wk)
 TOT_MEM_Kb(where)=TOT_MEM_Kb(where)-MEM_now_Kb
 !
 if (MEM_now_Kb==0) MEM_now_Kb=1
 !
 MEMs(i_SHELF)%use = MEMs(i_SHELF)%use-MEM_now_Kb
 MEMs(i_SHELF)%size(i_COMPONENT) = MEMs(i_SHELF)%size(i_COMPONENT)-Sz
 !
 if ( MEMs(i_SHELF)%use <= 0 ) then
   !
   do i_mem=i_SHELF,N_MEM_elements
     call MEM_element_copy(MEMs(i_mem+1),MEMs(i_mem))
   enddo    
   !
   N_MEM_elements=N_MEM_elements-1
   !
 else if (MEMs(i_SHELF)%size(i_COMPONENT) <= 0) then
   !
   do i_el=i_COMPONENT,MEMs(i_SHELF)%N-1
     MEMs(i_SHELF)%name(i_el)=MEMs(i_SHELF)%name(i_el+1)
     MEMs(i_SHELF)%desc(i_el)=MEMs(i_SHELF)%desc(i_el+1)
     MEMs(i_SHELF)%kind(i_el)=MEMs(i_SHELF)%kind(i_el+1)
     MEMs(i_SHELF)%size(i_el)=MEMs(i_SHELF)%size(i_el+1)
     MEMs(i_SHELF)%where(i_el)=MEMs(i_SHELF)%where(i_el+1)
   enddo    
   !
   MEMs(i_SHELF)%N=MEMs(i_SHELF)%N-1
   !
 endif
 !
 if(.not.just_started) call MEM_manager_messages(what,.false.,MEM_now_Kb,where)
 !
#endif
 !
end subroutine MEM_manager_free
!
subroutine MEM_manager_error(what)
 !
 use memory,       ONLY:MEM_err
 !
 implicit none
 !
 character(*), intent(in) :: what
 !
 ! Error Message
 !===============
 if (MEM_err/=0) call error("Allocation of "//trim(what)//" failed")
 !
end subroutine MEM_manager_error
!
subroutine MEM_manager_init(what,adding,Sz,i_SHELF,i_COMPONENT,i_SAVE,SHELF,TRUNK,just_started)
 !
 use pars,         ONLY:schlen,IPL
 use drivers,      ONLY:infile_editing
 use com,          ONLY:repfile,secnm
 use C_driver,     ONLY:code_bin
 use memory,       ONLY:MEM_treshold,MEM_shelf,MEM_jump_treshold,MEM_LIBs,MEMs,&
&                       MEM_SAVE_treshold
 !
 implicit none
 !
 character(*),     intent(in)  :: what
 logical,          intent(in)  :: adding
 integer(IPL),     intent(in)  :: Sz
 integer,          intent(out) :: i_SHELF,i_COMPONENT,i_SAVE
 character(schlen),intent(out) :: SHELF,TRUNK
 logical,          intent(out) :: just_started
 !
 ! Work Space
 !
 integer :: i_LIB
 integer, external :: MEM_LIB_finder,MEM_shelf_finder,MEM_save_finder,MEM_component_finder
 !
 ! Setup
 !=======
 MEM_SAVE_treshold=10 *MEM_treshold
 MEM_jump_treshold=100*MEM_treshold
 !
#if defined _MEM_CHECK
 !
 just_started=infile_editing
 if (index(code_bin,"ypp")==0.and.len_trim(repfile)==0) just_started=.TRUE.
 !
 ! Search the element
 !====================
 !
 !...in the library (here I associate to what a SHELF)
 TRUNK=what
 if (index(what,"%")>0) TRUNK=what(index(what,"%"):len(what))
 i_LIB=MEM_LIB_finder(trim(TRUNK))
 !
 if (i_LIB>0) then
   SHELF=MEM_shelf(MEM_LIBs(i_LIB)%shelf)
 else if (index(what,"%")>0) then
   SHELF="["//trim(TRUNK)//"]"
 else
   SHELF=what
 endif
 !...here i search the SHELF & the COMPONENT in the already allocated objects (to add/remove an element)
 i_SHELF=MEM_shelf_finder(trim(SHELF))
 i_COMPONENT=0
 if (i_SHELF>0) then
   SHELF=MEMs(i_SHELF)%shelf
   i_COMPONENT=MEM_component_finder(i_SHELF,what,Sz,trim(secnm),adding)
 endif
 i_SAVE =MEM_save_finder(trim(SHELF))
 !
#endif
 !
end subroutine MEM_manager_init
!
subroutine MEM_manager_messages(what,adding,MEM_now_Kb,where)
 !
 use pars,         ONLY:lchlen
 use com,          ONLY:msg
 use memory,       ONLY:TOT_MEM_Kb,TOT_MEM_Kb_MEMSTAT,TOT_MEM_Kb_SAVE,MEM_treshold,&
&                       MEM_jump_treshold,mem_string,HOST_,DEV_
 !
 implicit none
 !
 character(*), intent(in) :: what
 logical,      intent(in) :: adding
 integer,      intent(in) :: MEM_now_Kb
 integer,      intent(in) :: where
 !
 ! Work space
 !
 logical           :: deliver_a_msg
 character(lchlen) :: msg_,label
 !
 label=" "
#if defined _CUDA
 if (where==HOST_) label=" (HOST)" 
 if (where==DEV_)  label=" (DEV)" 
#endif
 !
#if defined _MEM_CHECK
 !
 ! Messages
 !==========
 !#1
 deliver_a_msg=MEM_now_Kb>MEM_treshold
 if (deliver_a_msg) then
   call MEM_STAT_update( )
   msg_="[MEMORY]"
   if (     adding) msg_=trim(msg_)//" Alloc "//what
   if (.not.adding) msg_=trim(msg_)//"  Free "//what
   msg_=trim(msg_)//"("//trim(mem_string(MEM_now_Kb))//")"
   msg_=trim(msg_)//trim(label)//" TOTAL: "//trim(mem_string(TOT_MEM_Kb(where)))//" (traced)"
   if (TOT_MEM_Kb_MEMSTAT(where)>0) then
     msg_=trim(msg_)//" "//trim(mem_string(TOT_MEM_Kb_MEMSTAT(where)))//" (memstat)"
   endif
   call msg("s",trim(msg_))
 endif
 !#2
 deliver_a_msg=int((TOT_MEM_Kb(where)-TOT_MEM_Kb_SAVE(where))/1000)>int(MEM_jump_treshold/10.).and.adding
 if (deliver_a_msg) then
   call MEM_STAT_update( )
   msg_="[MEMORY] In use: "
   msg_=trim(msg_)//trim(label)//" TOTAL: "//trim(mem_string(TOT_MEM_Kb(where)))//" (traced)"
   if (TOT_MEM_Kb_MEMSTAT(where)>0) then
     msg_=trim(msg_)//" "//trim(mem_string(TOT_MEM_Kb_MEMSTAT(where)))//" (memstat)"
   endif
   call msg("s",trim(msg_))
   TOT_MEM_Kb_SAVE(where)=TOT_MEM_Kb(where)
 endif
 !
#endif
 !
end subroutine MEM_manager_messages
