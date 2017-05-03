!
!        Copyright (C) 2000-2017 the YAMBO team
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
module memory
 !
 use pars,          ONLY:SP,schlen,IP,DP,LP,N_MEM_max,N_MEM_SAVE_max,&
&                        N_MEM_shelf_max,N_MEM_LIBs_max,N_MEM_max_element_components
 use openmp,        ONLY:master_thread
 use iso_c_binding, ONLY:c_int
 !
 implicit none
 !
 integer :: MEM_err                  = 0
 integer(kind=c_int) :: TOT_MEM_kilobytes_MEMSTAT 
 integer             :: MAX_MEM_bytes = 0
 integer             :: TOT_MEM_bytes
 integer             :: TOT_MEM_bytes_SAVE =  0
 integer             :: MEM_treshold             ! Mb*1000=Kb
 integer             :: MEM_SAVE_treshold        ! "
 integer             :: MEM_jump_treshold        ! "
 !
 integer :: N_MEM_elements           = 0
 type MEM_element
   !
   ! Total...
   character(schlen) :: shelf   = " "
   integer           :: use     =0  
   !
   ! Specific...
   integer           :: N       =0
   character(schlen) :: name(   N_MEM_max_element_components) = " "
   character(schlen) :: section(N_MEM_max_element_components) = " "
   integer           :: kind(   N_MEM_max_element_components) =0  
   integer           :: size(   N_MEM_max_element_components) =0  
   !
 end type MEM_element
 type(MEM_element) :: MEMs(N_MEM_max)
 !
 integer :: N_MEM_SAVE_elements = 0
 type(MEM_element) :: LARGE_MEMs(N_MEM_SAVE_max)
 !
 ! Library
 !
 integer :: N_MEM_shelf       = 0
 integer :: N_MEM_LIBs        = 0
 !
 type MEM_lib_element
   character(schlen) :: name  = " "
   integer           :: shelf = 0
 end type MEM_lib_element
 !
 character(schlen)     :: MEM_shelf(N_MEM_shelf_max)
 type(MEM_lib_element) :: MEM_LIBs(N_MEM_LIBs_max)
 !
 interface MEM_dri
   module procedure MEM_clean,MEM_error,&
&                             MEM_i1,MEM_r1,MEM_c1,MEM_l1,MEM_cd1,MEM_rd1,MEM_lp1,&
&                             MEM_i2,MEM_r2,MEM_c2,MEM_l2,MEM_cd2,MEM_rd2,&
&                             MEM_i3,MEM_r3,MEM_c3,MEM_l3,MEM_cd3,MEM_rd3,MEM_lp3,&
&                                    MEM_r4,MEM_c4,MEM_l4,MEM_cd4,&
&                             MEM_i5,MEM_r5,MEM_c5,       MEM_cd5
 end interface MEM_dri
 !
 interface  
   !
   subroutine MEM_manager(what,Sz,Kn,REPORT)
     character(*)         :: what
     integer,optional     :: Sz
     integer,optional     :: Kn
     character(*),optional:: REPORT
   end subroutine 
   !
   subroutine MEM_library(action,member,shelf)
     character(*)         :: action
     character(*),optional:: member
     character(*),optional:: shelf
   end subroutine 
   !
 end interface
 !
 contains
   !
   character(schlen) function mem_string(MEM)
     use units,        ONLY:Kilobyte,Megabyte,Gigabyte
     use stderr,       ONLY:intc
     integer :: MEM !in Bytes
     if (MEM<Kilobyte) then
       mem_string=trim(intc(MEM))//"b"
     else if (MEM<Megabyte) then
       mem_string=trim(intc(int(real(MEM)/Kilobyte)))//"Kb"
     else if (MEM<Gigabyte) then
       mem_string=trim(intc(int(real(MEM)/Megabyte)))//"Mb"
     else
       mem_string=trim(intc(int(real(MEM)/Gigabyte)))//"Gb"
     endif
   end function
   !
   subroutine MEM_element_copy(M1,M2)
     type(MEM_element) :: M1,M2
     M2%shelf  = M1%shelf
     M2%use    = M1%use
     M2%N      = M1%N
     M2%name   = M1%name
     M2%section= M1%section
     M2%kind   = M1%kind
     M2%size   = M1%size
   end subroutine
   !
   subroutine MEM_error(name)
     character(*)::name
     call MEM_manager(name)
   end subroutine
   subroutine MEM_clean(name,N)
     character(*)::name
     integer     ::N
     call MEM_manager(name,Sz=N)
   end subroutine
   subroutine MEM_i1(name,i)
     character(*)::name
     integer     ::i(:)
     call MEM_manager(name,Sz=size(i),Kn=kind(i(1)))
   end subroutine
   subroutine MEM_lp1(name,i)
     character(*)::name
     integer(LP) ::i(:)
     call MEM_manager(name,Sz=size(i),Kn=kind(i(1)))
   end subroutine
   subroutine MEM_i2(name,i)
     character(*)::name
     integer     ::i(:,:)
     call MEM_manager(name,Sz=size(i),Kn=kind(i(1,1)))
   end subroutine
   subroutine MEM_i3(name,i)
     character(*)::name
     integer     ::i(:,:,:)
     call MEM_manager(name,Sz=size(i),Kn=kind(i(1,1,1)))
   end subroutine
   subroutine MEM_lp3(name,i)
     character(*)::name
     integer(LP) ::i(:,:,:)
     call MEM_manager(name,Sz=size(i),Kn=kind(i(1,1,1)))
   end subroutine
   subroutine MEM_i5(name,i)
     character(*)::name
     integer     ::i(:,:,:,:,:)
     call MEM_manager(name,Sz=size(i),Kn=kind(i(1,1,1,1,1)))
   end subroutine
   subroutine MEM_r1(name,r)
     character(*)::name
     real(SP)    ::r(:)
     call MEM_manager(name,Sz=size(r),Kn=kind(r))
   end subroutine
   subroutine MEM_r2(name,r)
     character(*)::name
     real(SP)    ::r(:,:)
     call MEM_manager(name,Sz=size(r),Kn=kind(r(1,1)))
   end subroutine
   subroutine MEM_rd1(name,r)
     character(*)::name
     real(DP)    ::r(:)
     call MEM_manager(name,Sz=size(r),Kn=kind(r))
   end subroutine
   subroutine MEM_rd2(name,r)
     character(*)::name
     real(DP)    ::r(:,:)
     call MEM_manager(name,Sz=size(r),Kn=kind(r(1,1)))
   end subroutine
   subroutine MEM_rd3(name,r)
     character(*)::name
     real(DP)    ::r(:,:,:)
     call MEM_manager(name,Sz=size(r),Kn=kind(r(1,1,1)))
   end subroutine
   subroutine MEM_r3(name,r)
     character(*)::name
     real(SP)    ::r(:,:,:)
     call MEM_manager(name,Sz=size(r),Kn=kind(r(1,1,1)))
   end subroutine
   subroutine MEM_r4(name,r)
     character(*)::name
     real(SP)    ::r(:,:,:,:)
     call MEM_manager(name,Sz=size(r),Kn=kind(r(1,1,1,1)))
   end subroutine
   subroutine MEM_r5(name,r)
     character(*)::name
     real(SP)    ::r(:,:,:,:,:)
     call MEM_manager(name,Sz=size(r),Kn=kind(r(1,1,1,1,1)))
   end subroutine
   subroutine MEM_c1(name,c)
     character(*)::name
     complex(SP) ::c(:)
     call MEM_manager(name,Sz=size(c),Kn=2*kind(c(1)))
   end subroutine
   subroutine MEM_cd1(name,c)
     character(*)::name
     complex(DP) ::c(:)
     call MEM_manager(name,Sz=size(c),Kn=2*kind(c(1)))
   end subroutine
   subroutine MEM_cd2(name,c)
     character(*)::name
     complex(DP) ::c(:,:)
     call MEM_manager(name,Sz=size(c),Kn=2*kind(c(1,1)))
   end subroutine
   subroutine MEM_cd3(name,c)
     character(*)::name
     complex(DP) ::c(:,:,:)
     call MEM_manager(name,Sz=size(c),Kn=2*kind(c(1,1,1)))
   end subroutine
   subroutine MEM_cd4(name,c)
     character(*)::name
     complex(DP) ::c(:,:,:,:)
     call MEM_manager(name,Sz=size(c),Kn=2*kind(c(1,1,1,1)))
   end subroutine
   subroutine MEM_cd5(name,c)
     character(*)::name
     complex(DP) ::c(:,:,:,:,:)
     call MEM_manager(name,Sz=size(c),Kn=2*kind(c(1,1,1,1,1)))
   end subroutine
   subroutine MEM_c2(name,c)
     character(*)::name
     complex(SP) ::c(:,:)
     call MEM_manager(name,Sz=size(c),Kn=2*kind(c(1,1)))
   end subroutine
   subroutine MEM_c3(name,c)
     character(*)::name
     complex(SP) ::c(:,:,:)
     call MEM_manager(name,Sz=size(c),Kn=2*kind(c(1,1,1)))
   end subroutine
   subroutine MEM_c4(name,c)
     character(*)::name
     complex(SP) ::c(:,:,:,:)
     call MEM_manager(name,Sz=size(c),Kn=2*kind(c(1,1,1,1)))
   end subroutine
   subroutine MEM_c5(name,c)
     character(*)::name
     complex(SP) ::c(:,:,:,:,:)
     call MEM_manager(name,Sz=size(c),Kn=2*kind(c(1,1,1,1,1)))
   end subroutine
   subroutine MEM_l1(name,l)
     character(*)::name
     logical     ::l(:)
     call MEM_manager(name,Sz=size(l),Kn=kind(l(1)))
   end subroutine
   subroutine MEM_l2(name,l)
     character(*)::name
     logical     ::l(:,:)
     call MEM_manager(name,Sz=size(l),Kn=kind(l(1,1)))
   end subroutine
   subroutine MEM_l3(name,l)
     character(*)::name
     logical     ::l(:,:,:)
     call MEM_manager(name,Sz=size(l),Kn=kind(l(1,1,1)))
   end subroutine
   subroutine MEM_l4(name,l)
     character(*)::name
     logical     ::l(:,:,:,:)
     call MEM_manager(name,Sz=size(l),Kn=kind(l(1,1,1,1)))
   end subroutine
end module memory
