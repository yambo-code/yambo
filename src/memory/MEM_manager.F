!
!        Copyright (C) 2000-2016 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
subroutine MEM_manager(what,Sz,Kn,REPORT)
 !
 use pars,         ONLY:lchlen,schlen
 use drivers,      ONLY:infile_editing
 use com,          ONLY:msg,warning,secnm,error
 use openmp,       ONLY:master_thread
 use units,        ONLY:Kilobyte,Megabyte
 use memory,       ONLY:TOT_MEM_bytes,TOT_MEM_kilobytes_MEMSTAT,MEM_element_copy,&
&                       MEM_err,MEM_treshold,N_MEM_elements,LARGE_MEMs,N_MEM_SAVE_max,&
&                       N_MEM_SAVE_elements,MEM_shelf,N_MEM_SAVE_max,MEM_LIBs,mem_string,&
&                       MEM_SAVE_treshold,MAX_MEM_bytes,TOT_MEM_bytes_SAVE,MEMs,&
&                       N_MEM_max_element_components,MEM_jump_treshold
 !
 implicit none
 !
 character(*)         :: what
 integer,optional     :: Sz
 integer,optional     :: Kn
 character(*),optional:: REPORT
 ! 
 ! Work Space
 !
 logical :: adding,deliver_a_msg
 integer :: i_mem,i_el,MEM_now_kb,MEM_now_b,i_FOUND,i_FOUND_SAVE,i_FOUND_COMPONENT,i_LIB
 character(lchlen) :: msg_
 character(schlen) :: SHELF
 integer, external :: MEM_LIB_finder
 !
 ! Error Message
 !===============
 if (MEM_err/=0.and..not.present(Sz)) call error("Allocation of "//what//" failed")
 !
 if (.not.present(Sz).and..not.present(Kn).and..not.present(REPORT)) return
 if (.not.master_thread) return
 !
#if defined _MEM_CHECK
 !
 ! REPORTs 
 !=========
 if (present(REPORT)) then
   !
   call MEM_report( REPORT )
   !
   return
   !
 endif
 !
 adding  = present(Kn)
 !
 ! Search the element
 !====================
 i_LIB=MEM_LIB_finder(what)
 if (i_LIB>0) then
   SHELF=MEM_shelf(MEM_LIBs(i_LIB)%shelf)
 else
   SHELF=what
 endif
 call MEM_element_finder(trim(SHELF),what,Sz,i_FOUND,i_FOUND_COMPONENT,i_FOUND_SAVE)
! if (index(what,"%c")>0) then
!  write (*,*) 
!  write (*,*) what,trim(SHELF),i_FOUND,i_FOUND_component,adding,Sz
!  if (i_FOUND>0) then
!    write (*,*) MEMs(i_FOUND)%N
!    do i_el=1,MEMs(i_FOUND)%N
!      write (*,*) i_el,trim(MEMs(i_FOUND)%name(i_el)),MEMs(i_FOUND)%size(i_el)
!    enddo
!  endif
!  write (*,*) 
! endif
 !
 ! Error Message of an already allocated array
 !=============================================
 !
 if (MEM_err/=0) then
   if (i_FOUND>0.and.I_FOUND_COMPONENT>0) then
     call warning("Allocation of "//what//" failed. Object was already allocated in "//&
&                 trim(MEMs(i_FOUND)%section(I_FOUND_COMPONENT)))
   else
     call warning("Allocation of "//what//" failed. Object was already allocated, tough, not present in the archive.")
   endif
   return
 endif
 !
 ! Database Update
 !=================
 if (adding) then
   !
   MEM_now_b=Sz*Kn
   MEM_now_kb=nint(real(MEM_now_b)/Kilobyte)
   !
   if (MEM_now_kb==0) MEM_now_kb=1
   !
   TOT_MEM_bytes=TOT_MEM_bytes+MEM_now_b
   !
   if (TOT_MEM_bytes>MAX_MEM_bytes) MAX_MEM_bytes=TOT_MEM_bytes
   !
   if (i_FOUND==0) then
     if (N_MEM_elements<N_MEM_SAVE_max) then
       N_MEM_elements=N_MEM_elements+1
     else
       call error("Max Number of MEM elements reached.")
     endif
     MEMs(N_MEM_elements)%shelf=SHELF
     MEMs(N_MEM_elements)%use =MEM_now_kb
     i_FOUND=N_MEM_elements
   else
     MEMs(i_FOUND)%use = MEMs(i_FOUND)%use+MEM_now_kb
   endif
   !
   MEMs(i_FOUND)%N=MEMs(i_FOUND)%N+1
   I_FOUND_COMPONENT=MEMs(i_FOUND)%N
   if (I_FOUND_COMPONENT>N_MEM_max_element_components) then
     call error("Max Number of MEM elements reached.")
   endif
   !
   MEMs(i_FOUND)%section(I_FOUND_COMPONENT)=secnm
   MEMs(i_FOUND)%name(I_FOUND_COMPONENT)=what
   MEMs(i_FOUND)%kind(I_FOUND_COMPONENT)=Kn
   MEMs(i_FOUND)%size(I_FOUND_COMPONENT)=MEMs(i_FOUND)%size(I_FOUND_COMPONENT)+Sz
   !
   if (MEMs(i_FOUND)%use>MEM_SAVE_treshold) then
     if (i_FOUND_SAVE==0) then
       if (N_MEM_SAVE_elements<N_MEM_SAVE_max) then
         N_MEM_SAVE_elements=N_MEM_SAVE_elements+1
       else
         call error("Max Number of LARGE MEM elements reached.")
       endif
       call MEM_element_copy(MEMs(i_FOUND),LARGE_MEMs(N_MEM_SAVE_elements))
     else
       call MEM_element_copy(MEMs(i_FOUND),LARGE_MEMs(i_FOUND_SAVE))
     endif
   endif
   !
 else if (i_FOUND/=0.and.i_FOUND_component/=0) then
   !
   MEM_now_b=MEMs(i_FOUND)%size(i_FOUND_component)*MEMs(i_FOUND)%kind(i_FOUND_component)
   TOT_MEM_bytes=TOT_MEM_bytes-MEM_now_b
   MEM_now_kb=nint(real(MEM_now_b)/Kilobyte)
   !
   if (MEM_now_kb==0) MEM_now_kb=1
   !
   MEMs(i_FOUND)%use = MEMs(i_FOUND)%use-MEM_now_kb
   MEMs(i_FOUND)%size(i_FOUND_component) = MEMs(i_FOUND)%size(i_FOUND_component)-MEM_now_kb
   !
   if ( MEMs(i_FOUND)%use <= 0 ) then
     !
     do i_mem=i_FOUND,N_MEM_elements
       call MEM_element_copy(MEMs(i_mem+1),MEMs(i_mem))
     enddo    
     !
     N_MEM_elements=N_MEM_elements-1
     !
   else if ( MEMs(i_FOUND)%size(i_FOUND_component)<=0) then
     !
     do i_el=i_FOUND_component,MEMs(i_FOUND)%N
       MEMs(i_FOUND)%name(i_el)=MEMs(i_FOUND)%name(i_el+1)
       MEMs(i_FOUND)%section(i_el)=MEMs(i_FOUND)%section(i_el+1)
       MEMs(i_FOUND)%kind(i_el)=MEMs(i_FOUND)%kind(i_el+1)
       MEMs(i_FOUND)%size(i_el)=MEMs(i_FOUND)%size(i_el+1)
     enddo    
     !
     MEMs(i_FOUND)%N=MEMs(i_FOUND)%N-1
     !
   endif
   !
 else if (i_FOUND/=0.and.i_FOUND_component==0) then
   !
   call warning(what//" not found in the "//trim(SHELF)//" shelf")
   !
 else if (i_FOUND==0.and.i_FOUND_component==0) then
   !
   call warning(what//" object and "//trim(SHELF)//" shelf not found")
   !
 endif
 !
 if (MEM_treshold==0) return
 !
 ! Messages
 !==========
 !#1
 deliver_a_msg=MEM_now_kb>MEM_treshold.and..not.infile_editing
 if (deliver_a_msg) then
   call MEM_STAT_update( )
   msg_="[MEMORY]"
   if (     adding) msg_=trim(msg_)//" Alloc "//what
   if (.not.adding) msg_=trim(msg_)//" Free "//what
   msg_=trim(msg_)//"("//trim(mem_string(MEM_now_b))//")"
   msg_=trim(msg_)//" TOTAL: "//trim(mem_string(TOT_MEM_bytes))//" (traced)"
   msg_=trim(msg_)//" "//trim(mem_string(TOT_MEM_kilobytes_MEMSTAT*int(Kilobyte)))//" (memstat)"
   call msg("s",trim(msg_))
 endif
 !#2
 deliver_a_msg=(TOT_MEM_bytes-TOT_MEM_bytes_SAVE)>int(MEM_jump_treshold*Megabyte/Kilobyte).and.adding
 if (deliver_a_msg) then
   call MEM_STAT_update( )
   msg_="[MEMORY] In use: "
   msg_=trim(msg_)//" TOTAL: "//trim(mem_string(TOT_MEM_bytes))//" (traced)"
   msg_=trim(msg_)//" "//trim(mem_string(TOT_MEM_kilobytes_MEMSTAT*int(Kilobyte)))//" (memstat)"
   call msg("s",trim(msg_))
   TOT_MEM_bytes_SAVE=TOT_MEM_bytes
 endif
 !
#endif
 !
end subroutine
