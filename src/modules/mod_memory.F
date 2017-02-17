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
module memory_m
 !
 use pars,       ONLY:SP,lchlen,schlen
 use stderr,     ONLY:string_split
 use openmp,     ONLY:master_thread
 !
 implicit none
 !
 integer, parameter :: max_mem_elements=100000
 integer, parameter :: preset_kind=2*SP
 character(schlen), private  :: mem_element(max_mem_elements)
 integer          , private  :: element_loaded
 real(SP)         , private  :: mem_element_size(max_mem_elements)
 real(SP)         :: mem_sum
 !
 contains
   !
   subroutine mem_reset()
     mem_element=' '
     mem_element_size=0._SP
     element_loaded=0
     mem_sum=0._SP
   end subroutine
   !
   subroutine mem_est(what,elements_size,elements_kind,errors,quiet,overall_report)
     use drivers,      ONLY:infile_editing
     use com,          ONLY:msg,error,warning
     use units,        ONLY:Gigabyte
     character(*)         :: what
     integer,optional     :: elements_size(:)
     integer,optional     :: elements_kind(:)
     integer,optional     :: errors(:)
     logical,optional     :: quiet,overall_report
     ! 
     ! Work Space
     !
     integer :: i1,i2,local_kind
     logical :: loading,stdout_report
     real(SP):: mem_moved,mem_element_size_change(max_mem_elements)
     real(SP),parameter :: mem_tresh=0.01_SP  ! ( in Gb. CAREFUL! mem_tresh must be >0)
     character(lchlen)  ::ch(2)
     character(schlen)  ::input_name(10),action
     !
     if (.not.master_thread) return
     !
     mem_element_size_change=0._SP
     !
     loading=present(elements_size)
     !
     ! Split what in input_names
     !
     call string_split(what,input_name)
     !
     mem_moved=0._SP
     ch(1)=""
     if (loading) then
       action="Alloc"
       do i1=1,10
         if (len_trim(input_name(i1))==0) cycle
         do i2=1,max_mem_elements
           if (trim(mem_element(i2))=="") exit
           if (trim(mem_element(i2))==trim(input_name(i1))) exit
         enddo
         if (i2==element_loaded+1) element_loaded=element_loaded+1
         !
         if (i2>max_mem_elements) call error(" Too many elements in memory!! ")
         !
         mem_element(i2)=trim(input_name(i1))
         local_kind=preset_kind
         if (present(elements_kind)) local_kind=elements_kind(i1)
         mem_element_size(i2)=mem_element_size(i2)+real(elements_size(i1),SP)*real(local_kind,SP)/Gigabyte
         mem_element_size_change(i2)=mem_element_size_change(i2)+real(elements_size(i1),SP)*real(local_kind,SP)/Gigabyte
         if (present(errors)) then
           if (errors(i1)/=0) then
             write (ch(1),'(3a,f10.5,a)') 'Mem All. failed. Element ',trim(mem_element(i2)),&
&                                         ' require ',mem_element_size(i2), ' [Gb]'
             call error(trim(ch(1)))
           endif
         endif
         mem_sum=mem_sum+mem_element_size_change(i2)
         mem_moved=mem_moved+mem_element_size_change(i2)
         write (ch(2),'(3a)') trim(ch(1))," ",trim(mem_element(i2))
         ch(1)=ch(2)
       enddo
     else if (.not.present(overall_report)) then
       action="Free"
       do i1=1,10
         if (len_trim(input_name(i1))==0) cycle
         do i2=1,max_mem_elements
           if(trim(mem_element(i2))==trim(input_name(i1))) then
              write (ch(2),'(3a)') trim(ch(1))," ",trim(mem_element(i2))
              ch(1)=ch(2)
              mem_element(i2)=""
              mem_sum=mem_sum-mem_element_size(i2)
              mem_moved=mem_moved+mem_element_size(i2)
              mem_element_size(i2)=0
           endif
         enddo
       enddo
     endif
     !
     !  Overall Report
     !
     !   character(schlen)  :: mem_element(max_mem_elements)
     !   real(SP)           :: mem_element_size(max_mem_elements)
     !
     if (present(overall_report).and.element_loaded>0) then
       do i1=1,element_loaded
         if (len_trim(mem_element(i1))==0) cycle
         write (ch(1),'(4a,t70,f15.5)') "[MEM@",trim(what),"] ",&
&              trim(mem_element(i1)),mem_element_size(i1)
         call msg("r",trim(ch(1)))
       enddo
     endif
     !
     if(mem_moved.lt.0.0_SP.or.mem_sum.lt.0.0_SP) then
       mem_moved=0._SP
       mem_sum  =0._SP
     endif
     !
     stdout_report=mem_moved>mem_tresh.and..not.infile_editing
     if (stdout_report.and.present(quiet)) stdout_report=.not.quiet
     if (.not.stdout_report) return
     !
     write (ch(2),'(a,f6.3,4a,f6.3,a)') "[M ",mem_sum," Gb] ",trim(action),trim(ch(1))," (",mem_moved,")"
     call msg('s',trim(ch(2)))
     !
   end subroutine
   !  
end module memory_m
