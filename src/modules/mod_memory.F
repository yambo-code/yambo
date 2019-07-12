!
!        Copyright (C) 2000-2019 the YAMBO team
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
 integer(kind=c_int) :: TOT_MEM_Kb_MEMSTAT 
 integer(IP)         :: MAX_MEM_Kb      = 0
 integer(IP)         :: TOT_MEM_Kb
 integer(IP)         :: TOT_MEM_Kb_SAVE =  0
 integer(IP)         :: MEM_treshold       =  1  ! Mb*1000=Kb
 integer(IP)         :: MEM_SAVE_treshold  =  10 ! "
 integer(IP)         :: MEM_jump_treshold  =  100! "
 !
 integer :: N_MEM_elements           = 0
 type MEM_element
   !
   ! Total...
   character(schlen) :: shelf
   integer           :: use  
   !
   ! Specific...
   integer           :: N
   character(schlen) :: name(N_MEM_max_element_components)
   character(schlen) :: desc(N_MEM_max_element_components)
   integer           :: kind(N_MEM_max_element_components)
   integer           :: size(N_MEM_max_element_components)
   logical           :: composed(N_MEM_max_element_components)
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
   character(schlen) :: name
   integer           :: shelf
 end type MEM_lib_element
 !
 character(schlen)     :: MEM_shelf(N_MEM_shelf_max)
 type(MEM_lib_element) :: MEM_LIBs(N_MEM_LIBs_max)
 !
 interface MEM_dri
   module procedure MEM_clean,MEM_error,&
#ifdef _CUDA
&                             MEM_i1g,       MEM_c1g,&
&                             MEM_i2g,       MEM_c2g,&
&                             MEM_i3g,       MEM_c3g,&
&                             MEM_i4g,       MEM_c4g,&
&                             MEM_i5g,       MEM_c5g,&
&                                            MEM_c6g,&
#  ifndef _DOUBLE
&                             MEM_cd1g,& 
&                             MEM_cd2g,&
&                             MEM_cd3g,& 
&                             MEM_cd4g,& 
&                             MEM_cd5g,& 
&                             MEM_cd6g,& 
#  endif
#endif
&                             MEM_i1, MEM_i2, MEM_i3, MEM_i4, MEM_i5,                  &
&                             MEM_r1, MEM_r2, MEM_r3, MEM_r4, MEM_r5,                  &
&                             MEM_c1, MEM_c2, MEM_c3, MEM_c4, MEM_c5, MEM_c6,          &
#ifndef _DOUBLE
&                             MEM_rd1,MEM_rd2,MEM_rd3,                                 &
&                             MEM_cd1,MEM_cd2,MEM_cd3,MEM_cd4,MEM_cd5,MEM_cd6,         &
#endif
&                             MEM_l1, MEM_l2, MEM_l3, MEM_l4,                          &
&                             MEM_lp1,        MEM_lp3,                                 &
&                             MEM_ch1,        MEM_ch3
 end interface MEM_dri
 !
 contains
   !
   subroutine MEM_element_init(MEM_element_to_init,size_)
     integer,           intent(in)  :: size_
     type(MEM_element), intent(out) :: MEM_element_to_init(size_)
     integer                        :: i1
     do i1=1,size_
       MEM_element_to_init(i1)%shelf      = " "
       MEM_element_to_init(i1)%use        = 0
       MEM_element_to_init(i1)%N          = 0
       MEM_element_to_init(i1)%name(:)    = " "
       MEM_element_to_init(i1)%desc(:)    = " "
       MEM_element_to_init(i1)%kind(:)    = 0
       MEM_element_to_init(i1)%size(:)    = 0
       MEM_element_to_init(i1)%composed(:)= .false.
     enddo
   end subroutine
   !
   character(schlen) function mem_string(MEM)
     use stderr,       ONLY:intc,real2ch
     integer :: MEM !in Kb
     if (MEM<1000) then
       mem_string=trim(real2ch(real(MEM,SP)))//"Kb"
     else if (MEM<1000000) then
       mem_string=trim(real2ch(real(MEM,SP)/1000))//"Mb"
     else
       mem_string=trim(real2ch(real(MEM,SP)/1000000))//"Gb"
     endif
   end function
   !
   subroutine MEM_element_copy(M1,M2)
     type(MEM_element) :: M1,M2
     M2%shelf      = M1%shelf
     M2%use        = M1%use
     M2%N          = M1%N
     M2%name       = M1%name
     M2%desc       = M1%desc
     M2%kind       = M1%kind
     M2%size       = M1%size
     M2%composed   = M1%composed
   end subroutine
   !
   subroutine MEM_error(name)
     character(*)::name
     call MEM_manager_error(name)
   end subroutine
   !
   subroutine MEM_clean(name,N)
     character(*)::name
     integer     ::N
     call MEM_manager_free(name,N)
   end subroutine
   !
   subroutine MEM_i1(name,i)
     character(*)::name
     integer     ::i(:)
     call MEM_manager_alloc(name,size(i),kind(i(1)))
   end subroutine
   subroutine MEM_lp1(name,i)
     character(*)::name
     integer(LP) ::i(:)
     call MEM_manager_alloc(name,size(i),kind(i(1)))
   end subroutine
   subroutine MEM_ch1(name,ch)
     character(*)::name
     character   ::ch(:)
     call MEM_manager_alloc(name,size(ch),kind(ch(1)))
   end subroutine
   subroutine MEM_i2(name,i)
     character(*)::name
     integer     ::i(:,:)
     call MEM_manager_alloc(name,size(i),kind(i(1,1)))
   end subroutine
   subroutine MEM_i3(name,i)
     character(*)::name
     integer     ::i(:,:,:)
     call MEM_manager_alloc(name,size(i),kind(i(1,1,1)))
   end subroutine
   subroutine MEM_i4(name,i)
     character(*)::name
     integer     ::i(:,:,:,:)
     call MEM_manager_alloc(name,size(i),kind(i(1,1,1,1)))
   end subroutine
   subroutine MEM_lp3(name,i)
     character(*)::name
     integer(LP) ::i(:,:,:)
     call MEM_manager_alloc(name,size(i),kind(i(1,1,1)))
   end subroutine
   subroutine MEM_ch3(name,ch)
     character(*)::name
     character   ::ch(:,:,:)
     call MEM_manager_alloc(name,size(ch),kind(ch(1,1,1)))
   end subroutine
   subroutine MEM_i5(name,i)
     character(*)::name
     integer     ::i(:,:,:,:,:)
     call MEM_manager_alloc(name,size(i),kind(i(1,1,1,1,1)))
   end subroutine
#ifdef _CUDA
   subroutine MEM_i1g(name,i)
     character(*)::name
     integer, device ::i(:)
     call MEM_manager_alloc(name,size(i),kind(i(1)))
   end subroutine
   subroutine MEM_i2g(name,i)
     character(*)::name
     integer, device ::i(:,:)
     call MEM_manager_alloc(name,size(i),kind(i(1,1)))
   end subroutine
   subroutine MEM_i3g(name,i)
     character(*)::name
     integer, device ::i(:,:,:)
     call MEM_manager_alloc(name,size(i),kind(i(1,1,1)))
   end subroutine
   subroutine MEM_i4g(name,i)
     character(*)::name
     integer, device ::i(:,:,:,:)
     call MEM_manager_alloc(name,size(i),kind(i(1,1,1,1)))
   end subroutine
   subroutine MEM_i5g(name,i)
     character(*)::name
     integer, device ::i(:,:,:,:,:)
     call MEM_manager_alloc(name,size(i),kind(i(1,1,1,1,1)))
   end subroutine
#endif
   subroutine MEM_r1(name,r)
     character(*)::name
     real(SP)    ::r(:)
     call MEM_manager_alloc(name,size(r),kind(r))
   end subroutine
   subroutine MEM_r2(name,r)
     character(*)::name
     real(SP)    ::r(:,:)
     call MEM_manager_alloc(name,size(r),kind(r(1,1)))
   end subroutine
#ifndef _DOUBLE
   subroutine MEM_rd1(name,r)
     character(*)::name
     real(DP)    ::r(:)
     call MEM_manager_alloc(name,size(r),kind(r))
   end subroutine
   subroutine MEM_rd2(name,r)
     character(*)::name
     real(DP)    ::r(:,:)
     call MEM_manager_alloc(name,size(r),kind(r(1,1)))
   end subroutine
   subroutine MEM_rd3(name,r)
     character(*)::name
     real(DP)    ::r(:,:,:)
     call MEM_manager_alloc(name,size(r),kind(r(1,1,1)))
   end subroutine
#endif
   subroutine MEM_r3(name,r)
     character(*)::name
     real(SP)    ::r(:,:,:)
     call MEM_manager_alloc(name,size(r),kind(r(1,1,1)))
   end subroutine
   subroutine MEM_r4(name,r)
     character(*)::name
     real(SP)    ::r(:,:,:,:)
     call MEM_manager_alloc(name,size(r),kind(r(1,1,1,1)))
   end subroutine
   subroutine MEM_r5(name,r)
     character(*)::name
     real(SP)    ::r(:,:,:,:,:)
     call MEM_manager_alloc(name,size(r),kind(r(1,1,1,1,1)))
   end subroutine
   subroutine MEM_c1(name,c)
     character(*)::name
     complex(SP) ::c(:)
     call MEM_manager_alloc(name,size(c),2*kind(c(1)))
   end subroutine
   subroutine MEM_c2(name,c)
     character(*)::name
     complex(SP) ::c(:,:)
     call MEM_manager_alloc(name,size(c),2*kind(c(1,1)))
   end subroutine
   subroutine MEM_c3(name,c)
     character(*)::name
     complex(SP) ::c(:,:,:)
     call MEM_manager_alloc(name,size(c),2*kind(c(1,1,1)))
   end subroutine
   subroutine MEM_c4(name,c)
     character(*)::name
     complex(SP) ::c(:,:,:,:)
     call MEM_manager_alloc(name,size(c),2*kind(c(1,1,1,1)))
   end subroutine
   subroutine MEM_c5(name,c)
     character(*)::name
     complex(SP) ::c(:,:,:,:,:)
     call MEM_manager_alloc(name,size(c),2*kind(c(1,1,1,1,1)))
   end subroutine
   subroutine MEM_c6(name,c)
     character(*)::name
     complex(SP) ::c(:,:,:,:,:,:)
     call MEM_manager_alloc(name,size(c),2*kind(c(1,1,1,1,1,1)))
   end subroutine
#ifndef _DOUBLE
   subroutine MEM_cd1(name,c)
     character(*)::name
     complex(DP) ::c(:)
     call MEM_manager_alloc(name,size(c),2*kind(c(1)))
   end subroutine
   subroutine MEM_cd2(name,c)
     character(*)::name
     complex(DP) ::c(:,:)
     call MEM_manager_alloc(name,size(c),2*kind(c(1,1)))
   end subroutine
   subroutine MEM_cd3(name,c)
     character(*)::name
     complex(DP) ::c(:,:,:)
     call MEM_manager_alloc(name,size(c),2*kind(c(1,1,1)))
   end subroutine
   subroutine MEM_cd4(name,c)
     character(*)::name
     complex(DP) ::c(:,:,:,:)
     call MEM_manager_alloc(name,size(c),2*kind(c(1,1,1,1)))
   end subroutine
   subroutine MEM_cd5(name,c)
     character(*)::name
     complex(DP) ::c(:,:,:,:,:)
     call MEM_manager_alloc(name,size(c),2*kind(c(1,1,1,1,1)))
   end subroutine
   subroutine MEM_cd6(name,c)
     character(*)::name
     complex(DP) ::c(:,:,:,:,:,:)
     call MEM_manager_alloc(name,size(c),2*kind(c(1,1,1,1,1,1)))
   end subroutine
#endif
#ifdef _CUDA
   subroutine MEM_c1g(name,c)
     character(*)::name
     complex(SP), device ::c(:)
     call MEM_manager_alloc(name,size(c),2*kind(c(1)))
   end subroutine
   subroutine MEM_c2g(name,c)
     character(*)::name
     complex(SP), device ::c(:,:)
     call MEM_manager_alloc(name,size(c),2*kind(c(1,1)))
   end subroutine
   subroutine MEM_c3g(name,c)
     character(*)::name
     complex(SP), device ::c(:,:,:)
     call MEM_manager_alloc(name,size(c),2*kind(c(1,1,1)))
   end subroutine
   subroutine MEM_c4g(name,c)
     character(*)::name
     complex(SP), device ::c(:,:,:,:)
     call MEM_manager_alloc(name,size(c),2*kind(c(1,1,1,1)))
   end subroutine
   subroutine MEM_c5g(name,c)
     character(*)::name
     complex(SP), device ::c(:,:,:,:,:)
     call MEM_manager_alloc(name,size(c),2*kind(c(1,1,1,1,1)))
   end subroutine
   subroutine MEM_c6g(name,c)
     character(*)::name
     complex(SP), device ::c(:,:,:,:,:,:)
     call MEM_manager_alloc(name,size(c),2*kind(c(1,1,1,1,1,1)))
   end subroutine
#ifndef _DOUBLE
   subroutine MEM_cd1g(name,c)
     character(*)::name
     complex(DP), device ::c(:)
     call MEM_manager_alloc(name,size(c),2*kind(c(1)))
   end subroutine
   subroutine MEM_cd2g(name,c)
     character(*)::name
     complex(DP), device ::c(:,:)
     call MEM_manager_alloc(name,size(c),2*kind(c(1,1)))
   end subroutine
   subroutine MEM_cd3g(name,c)
     character(*)::name
     complex(DP), device ::c(:,:,:)
     call MEM_manager_alloc(name,size(c),2*kind(c(1,1,1)))
   end subroutine
   subroutine MEM_cd4g(name,c)
     character(*)::name
     complex(DP), device ::c(:,:,:,:)
     call MEM_manager_alloc(name,size(c),2*kind(c(1,1,1,1)))
   end subroutine
   subroutine MEM_cd5g(name,c)
     character(*)::name
     complex(DP), device ::c(:,:,:,:,:)
     call MEM_manager_alloc(name,size(c),2*kind(c(1,1,1,1,1)))
   end subroutine
   subroutine MEM_cd6g(name,c)
     character(*)::name
     complex(DP), device ::c(:,:,:,:,:,:)
     call MEM_manager_alloc(name,size(c),2*kind(c(1,1,1,1,1,1)))
   end subroutine
#endif
#endif
   subroutine MEM_l1(name,l)
     character(*)::name
     logical     ::l(:)
     call MEM_manager_alloc(name,size(l),kind(l(1)))
   end subroutine
   subroutine MEM_l2(name,l)
     character(*)::name
     logical     ::l(:,:)
     call MEM_manager_alloc(name,size(l),kind(l(1,1)))
   end subroutine
   subroutine MEM_l3(name,l)
     character(*)::name
     logical     ::l(:,:,:)
     call MEM_manager_alloc(name,size(l),kind(l(1,1,1)))
   end subroutine
   subroutine MEM_l4(name,l)
     character(*)::name
     logical     ::l(:,:,:,:)
     call MEM_manager_alloc(name,size(l),kind(l(1,1,1,1)))
   end subroutine
end module memory
