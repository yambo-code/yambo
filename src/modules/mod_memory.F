!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
module y_memory
 !
 use pars,          ONLY:SP,schlen,IP,IPL,DP,LP,SP6,N_MEM_max,N_MEM_SAVE_max,&
&                        N_MEM_shelf_max,N_MEM_LIBs_max,N_MEM_max_element_components
 use openmp,        ONLY:master_thread
 use iso_c_binding, ONLY:c_int
 !
#include<dev_defs.h>
 !
 implicit none
 !
 character(schlen)   :: MEM_msg
 character(schlen)   :: USER_MEM_limit_string=" "
 character(2)        :: USER_MEM_unit="Gb"
 integer             :: MEM_err = 0
 integer, parameter  :: HOST_=1
 integer, parameter  :: DEV_=2
 integer(kind=c_int) :: TOT_MEM_Kb_MEMSTAT(2)    ! 2 slots: HOST, DEV
 integer(IP)         :: USER_MEM_limit     = 0
 integer(IP)         :: MAX_MEM_Kb(2)      = 0
 integer(IP)         :: TOT_MEM_Kb(2)
 integer(IP)         :: TOT_MEM_Kb_SAVE(2) =  0
 integer(IP)         :: MEM_treshold       =  1  ! Mb*1000=Kb
 integer(IP)         :: MEM_SAVE_treshold  =  10 ! "
 integer(IP)         :: MEM_jump_treshold  =  100! "
 !
 integer :: N_MEM_elements           = 0
 !
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
   integer(IPL)      :: size(N_MEM_max_element_components)
   integer(IPL)      :: where(N_MEM_max_element_components)
   logical           :: composed(N_MEM_max_element_components)
   !
 end type MEM_element
 !
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
 interface
   !
   subroutine MEM_global_mesg(what,xkind,HOST_SIZE)
    use pars,          ONLY:IPL
    character(*), intent(in) :: what
    integer,      intent(in) :: xkind
    integer(IPL), intent(in) :: HOST_SIZE(:)
   end subroutine MEM_global_mesg
   !
 end interface
 !
 interface MEM_count
   module procedure &
#ifdef _CUDAF
&                             MEM_i1_d, MEM_i2_d, MEM_i3_d, MEM_i4_d, MEM_i5_d,             &
&                             MEM_r1_d, MEM_r2_d, MEM_r3_d, MEM_r4_d, MEM_r5_d,             &
&                             MEM_c1_d, MEM_c2_d, MEM_c3_d, MEM_c4_d, MEM_c5_d, MEM_c6_d,   &
#  ifndef _DOUBLE
&                             MEM_cd1_d,MEM_cd2_d,MEM_cd3_d,MEM_cd4_d,MEM_cd5_d,MEM_cd6_d,  &
#  endif
#endif
&                             MEM_i1,  MEM_i2,  MEM_i3,  MEM_i4,  MEM_i5,  MEM_i6,  MEM_i7, &
&                             MEM_r1,  MEM_r2,  MEM_r3,  MEM_r4,  MEM_r5,                   &
&                             MEM_c1,  MEM_c2,  MEM_c3,  MEM_c4,  MEM_c5,  MEM_c6,          &
#ifndef _DOUBLE
&                             MEM_rd1, MEM_rd2, MEM_rd3, MEM_rd4, MEM_rd5,                  &
&                             MEM_cd1, MEM_cd2, MEM_cd3, MEM_cd4, MEM_cd5, MEM_cd6,         &
#else
                              MEM_c1_s,                                                     &
#endif
&                             MEM_l1,  MEM_l2,  MEM_l3,  MEM_l4,  MEM_l5,  MEM_l6,          &
&                             MEM_lp1,          MEM_lp3,                                    &
&                             MEM_ch1,          MEM_ch3
 end interface MEM_count
 !
 interface MEM_count_d
   module procedure &
#  ifndef _DOUBLE
&                             MEM_cd1_d,MEM_cd2_d,MEM_cd3_d,MEM_cd4_d,MEM_cd5_d,MEM_cd6_d,  &
#  endif
&                             MEM_i1_d, MEM_i2_d, MEM_i3_d, MEM_i4_d, MEM_i5_d,             &
&                             MEM_r1_d, MEM_r2_d, MEM_r3_d, MEM_r4_d, MEM_r5_d,             &
&                             MEM_c1_d, MEM_c2_d, MEM_c3_d, MEM_c4_d, MEM_c5_d, MEM_c6_d
 end interface MEM_count_d
 !
 contains
   !
   subroutine GET_user_MEM_limit( )
     use stderr,   ONLY:intc
     integer           :: i_pos(2)
     i_pos=(/index(trim(USER_MEM_limit_string),"Gb"),index(trim(USER_MEM_limit_string),"Mb")/) 
     if (all(i_pos==0)) then
       USER_MEM_limit_string=" "
       return
     endif
     if (i_pos(1)/=0) then
       read (USER_MEM_limit_string(:i_pos(1)-1),*) USER_MEM_limit
       USER_MEM_limit=USER_MEM_limit*1000000
       USER_MEM_unit="Gb"
     else
       read (USER_MEM_limit_string(:i_pos(2)-1),*) USER_MEM_limit
       USER_MEM_limit=USER_MEM_limit*1000
       USER_MEM_unit="Mb"
     endif
     USER_MEM_limit_string=trim(intc(USER_MEM_limit))//USER_MEM_unit
   end subroutine
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
       MEM_element_to_init(i1)%where(:)   = 0
       MEM_element_to_init(i1)%composed(:)= .false.
     enddo
   end subroutine
   !
   character(schlen) function mem_string(MEM,where)
     use stderr,       ONLY:intc,real2ch
     integer :: MEM !in Kb
     integer, optional :: where
     !
     character(64) :: where_str
     ! 
     where_str=" "
     if (present(where)) then
       if (where==HOST_) where_str=" Host"
       if (where==DEV_)  where_str="  Dev"
     endif
     !
     if (MEM<1000) then
       mem_string=trim(real2ch(real(MEM,SP)))//" [Kb]"
     else if (MEM<1000000) then
       mem_string=trim(real2ch(real(MEM,SP)/1000))//" [Mb]"
     else
       mem_string=trim(real2ch(real(MEM,SP)/1000000))//" [Gb]"
     endif
     mem_string=trim(mem_string)//trim(where_str)
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
     M2%where      = M1%where
     M2%composed   = M1%composed
   end subroutine
   !
   subroutine MEM_i1(name,i)
     character(*)::name
     integer     ::i(:)
     call MEM_manager_alloc(name,size(i,KIND=IPL),kind(i(1)),HOST_)
   end subroutine
   subroutine MEM_lp1(name,i)
     character(*)::name
     integer(LP) ::i(:)
     call MEM_manager_alloc(name,size(i,KIND=IPL),kind(i(1)),HOST_)
   end subroutine
   subroutine MEM_ch1(name,ch)
     character(*)::name
     character   ::ch(:)
     call MEM_manager_alloc(name,size(ch,KIND=IPL),kind(ch(1)),HOST_)
   end subroutine
   subroutine MEM_i2(name,i)
     character(*)::name
     integer     ::i(:,:)
     call MEM_manager_alloc(name,size(i,KIND=IPL),kind(i(1,1)),HOST_)
   end subroutine
   subroutine MEM_i3(name,i)
     character(*)::name
     integer     ::i(:,:,:)
     call MEM_manager_alloc(name,size(i,KIND=IPL),kind(i(1,1,1)),HOST_)
   end subroutine
   subroutine MEM_i4(name,i)
     character(*)::name
     integer     ::i(:,:,:,:)
     call MEM_manager_alloc(name,size(i,KIND=IPL),kind(i(1,1,1,1)),HOST_)
   end subroutine
   subroutine MEM_lp3(name,i)
     character(*)::name
     integer(LP) ::i(:,:,:)
     call MEM_manager_alloc(name,size(i,KIND=IPL),kind(i(1,1,1)),HOST_)
   end subroutine
   subroutine MEM_ch3(name,ch)
     character(*)::name
     character   ::ch(:,:,:)
     call MEM_manager_alloc(name,size(ch,KIND=IPL),kind(ch(1,1,1)),HOST_)
   end subroutine
   subroutine MEM_i5(name,i)
     character(*)::name
     integer     ::i(:,:,:,:,:)
     call MEM_manager_alloc(name,size(i,KIND=IPL),kind(i(1,1,1,1,1)),HOST_)
   end subroutine
   subroutine MEM_i6(name,i)
     character(*)::name
     integer     ::i(:,:,:,:,:,:)
     call MEM_manager_alloc(name,size(i,KIND=IPL),kind(i(1,1,1,1,1,1)),HOST_)
   end subroutine
   subroutine MEM_i7(name,i)
     character(*)::name
     integer     ::i(:,:,:,:,:,:,:)
     call MEM_manager_alloc(name,size(i,KIND=IPL),kind(i(1,1,1,1,1,1,1)),HOST_)
   end subroutine
   !
   ! GPU routines
   subroutine MEM_i1_d(name,i)
     character(*)::name
     integer DEV_ATTR ::i(:)
     call MEM_manager_alloc(name,size(i,KIND=IPL),kind(i(1)),DEV_)
   end subroutine
   subroutine MEM_i2_d(name,i)
     character(*)::name
     integer DEV_ATTR ::i(:,:)
     call MEM_manager_alloc(name,size(i,KIND=IPL),kind(i(1,1)),DEV_)
   end subroutine
   subroutine MEM_i3_d(name,i)
     character(*)::name
     integer DEV_ATTR ::i(:,:,:)
     call MEM_manager_alloc(name,size(i,KIND=IPL),kind(i(1,1,1)),DEV_)
   end subroutine
   subroutine MEM_i4_d(name,i)
     character(*)::name
     integer DEV_ATTR ::i(:,:,:,:)
     call MEM_manager_alloc(name,size(i,KIND=IPL),kind(i(1,1,1,1)),DEV_)
   end subroutine
   subroutine MEM_i5_d(name,i)
     character(*)::name
     integer DEV_ATTR ::i(:,:,:,:,:)
     call MEM_manager_alloc(name,size(i,KIND=IPL),kind(i(1,1,1,1,1)),DEV_)
   end subroutine
   ! end of GPU subroutines
   !
   subroutine MEM_r1(name,r)
     character(*)::name
     real(SP)    ::r(:)
     call MEM_manager_alloc(name,size(r,KIND=IPL),kind(r),HOST_)
   end subroutine
   subroutine MEM_r2(name,r)
     character(*)::name
     real(SP)    ::r(:,:)
     call MEM_manager_alloc(name,size(r,KIND=IPL),kind(r(1,1)),HOST_)
   end subroutine
   subroutine MEM_r3(name,r)
     character(*)::name
     real(SP)    ::r(:,:,:)
     call MEM_manager_alloc(name,size(r,KIND=IPL),kind(r(1,1,1)),HOST_)
   end subroutine
   subroutine MEM_r4(name,r)
     character(*)::name
     real(SP)    ::r(:,:,:,:)
     call MEM_manager_alloc(name,size(r,KIND=IPL),kind(r(1,1,1,1)),HOST_)
   end subroutine
   subroutine MEM_r5(name,r)
     character(*)::name
     real(SP)    ::r(:,:,:,:,:)
     call MEM_manager_alloc(name,size(r,KIND=IPL),kind(r(1,1,1,1,1)),HOST_)
   end subroutine
#ifndef _DOUBLE
   subroutine MEM_rd1(name,r)
     character(*)::name
     real(DP)    ::r(:)
     call MEM_manager_alloc(name,size(r,KIND=IPL),kind(r),HOST_)
   end subroutine
   subroutine MEM_rd2(name,r)
     character(*)::name
     real(DP)    ::r(:,:)
     call MEM_manager_alloc(name,size(r,KIND=IPL),kind(r(1,1)),HOST_)
   end subroutine
   subroutine MEM_rd3(name,r)
     character(*)::name
     real(DP)    ::r(:,:,:)
     call MEM_manager_alloc(name,size(r,KIND=IPL),kind(r(1,1,1)),HOST_)
   end subroutine
   subroutine MEM_rd4(name,r)
     character(*)::name
     real(DP)    ::r(:,:,:,:)
     call MEM_manager_alloc(name,size(r,KIND=IPL),kind(r(1,1,1,1)),HOST_)
   end subroutine
   subroutine MEM_rd5(name,r)
     character(*)::name
     real(DP)    ::r(:,:,:,:,:)
     call MEM_manager_alloc(name,size(r,KIND=IPL),kind(r(1,1,1,1,1)),HOST_)
   end subroutine
#endif
   !
   ! more GPU subroutines
   subroutine MEM_r1_d(name,r)
     character(*)::name
     real(SP) DEV_ATTR ::r(:)
     call MEM_manager_alloc(name,size(r,KIND=IPL),kind(r),DEV_)
   end subroutine
   subroutine MEM_r2_d(name,r)
     character(*)::name
     real(SP) DEV_ATTR ::r(:,:)
     call MEM_manager_alloc(name,size(r,KIND=IPL),kind(r(1,1)),DEV_)
   end subroutine
   subroutine MEM_r3_d(name,r)
     character(*)::name
     real(SP) DEV_ATTR ::r(:,:,:)
     call MEM_manager_alloc(name,size(r,KIND=IPL),kind(r(1,1,1)),DEV_)
   end subroutine
   subroutine MEM_r4_d(name,r)
     character(*)::name
     real(SP) DEV_ATTR ::r(:,:,:,:)
     call MEM_manager_alloc(name,size(r,KIND=IPL),kind(r(1,1,1,1)),DEV_)
   end subroutine
   subroutine MEM_r5_d(name,r)
     character(*)::name
     real(SP) DEV_ATTR ::r(:,:,:,:,:)
     call MEM_manager_alloc(name,size(r,KIND=IPL),kind(r(1,1,1,1,1)),DEV_)
   end subroutine
   ! end of GPU subroutines
   !
   subroutine MEM_c1(name,c)
     character(*)::name
     complex(SP) ::c(:)
     call MEM_manager_alloc(name,size(c,KIND=IPL),2*kind(c(1)),HOST_)
   end subroutine
   subroutine MEM_c2(name,c)
     character(*)::name
     complex(SP) ::c(:,:)
     call MEM_manager_alloc(name,size(c,KIND=IPL),2*kind(c(1,1)),HOST_)
   end subroutine
   subroutine MEM_c3(name,c)
     character(*)::name
     complex(SP) ::c(:,:,:)
     integer(IPL) ::Sz 
     Sz = size(c,1,KIND=IPL)*size(c,2,KIND=IPL)*size(c,3,KIND=IPL)
     call MEM_manager_alloc(name,Sz,2*kind(c(1,1,1)),HOST_)
   end subroutine
   subroutine MEM_c4(name,c)
     character(*)::name
     complex(SP) ::c(:,:,:,:)
     call MEM_manager_alloc(name,size(c,KIND=IPL),2*kind(c(1,1,1,1)),HOST_)
   end subroutine
   subroutine MEM_c5(name,c)
     character(*)::name
     complex(SP) ::c(:,:,:,:,:)
     call MEM_manager_alloc(name,size(c,KIND=IPL),2*kind(c(1,1,1,1,1)),HOST_)
   end subroutine
   subroutine MEM_c6(name,c)
     character(*)::name
     complex(SP) ::c(:,:,:,:,:,:)
     call MEM_manager_alloc(name,size(c,KIND=IPL),2*kind(c(1,1,1,1,1,1)),HOST_)
   end subroutine
#ifdef _DOUBLE
   subroutine MEM_c1_s(name,c)
     character(*)::name
     complex(SP6) ::c(:)
     call MEM_manager_alloc(name,size(c,KIND=IPL),2*kind(c(1)),HOST_)
   end subroutine
#endif
#ifndef _DOUBLE
   subroutine MEM_cd1(name,c)
     character(*)::name
     complex(DP) ::c(:)
     call MEM_manager_alloc(name,size(c,KIND=IPL),2*kind(c(1)),HOST_)
   end subroutine
   subroutine MEM_cd2(name,c)
     character(*)::name
     complex(DP) ::c(:,:)
     call MEM_manager_alloc(name,size(c,KIND=IPL),2*kind(c(1,1)),HOST_)
   end subroutine
   subroutine MEM_cd3(name,c)
     character(*)::name
     complex(DP) ::c(:,:,:)
     call MEM_manager_alloc(name,size(c,KIND=IPL),2*kind(c(1,1,1)),HOST_)
   end subroutine
   subroutine MEM_cd4(name,c)
     character(*)::name
     complex(DP) ::c(:,:,:,:)
     call MEM_manager_alloc(name,size(c,KIND=IPL),2*kind(c(1,1,1,1)),HOST_)
   end subroutine
   subroutine MEM_cd5(name,c)
     character(*)::name
     complex(DP) ::c(:,:,:,:,:)
     call MEM_manager_alloc(name,size(c,KIND=IPL),2*kind(c(1,1,1,1,1)),HOST_)
   end subroutine
   subroutine MEM_cd6(name,c)
     character(*)::name
     complex(DP) ::c(:,:,:,:,:,:)
     call MEM_manager_alloc(name,size(c,KIND=IPL),2*kind(c(1,1,1,1,1,1)),HOST_)
   end subroutine
#endif
   !
   ! GPU subroutines
   subroutine MEM_c1_d(name,c)
     character(*)::name
     complex(SP) DEV_ATTR ::c(:)
     call MEM_manager_alloc(name,size(c,KIND=IPL),2*kind(c(1)),DEV_)
   end subroutine
   subroutine MEM_c2_d(name,c)
     character(*)::name
     complex(SP) DEV_ATTR ::c(:,:)
     call MEM_manager_alloc(name,size(c,KIND=IPL),2*kind(c(1,1)),DEV_)
   end subroutine
   subroutine MEM_c3_d(name,c)
     character(*)::name
     complex(SP) DEV_ATTR ::c(:,:,:)
     call MEM_manager_alloc(name,size(c,KIND=IPL),2*kind(c(1,1,1)),DEV_)
   end subroutine
   subroutine MEM_c4_d(name,c)
     character(*)::name
     complex(SP) DEV_ATTR ::c(:,:,:,:)
     call MEM_manager_alloc(name,size(c,KIND=IPL),2*kind(c(1,1,1,1)),DEV_)
   end subroutine
   subroutine MEM_c5_d(name,c)
     character(*)::name
     complex(SP) DEV_ATTR ::c(:,:,:,:,:)
     call MEM_manager_alloc(name,size(c,KIND=IPL),2*kind(c(1,1,1,1,1)),DEV_)
   end subroutine
   subroutine MEM_c6_d(name,c)
     character(*)::name
     complex(SP) DEV_ATTR ::c(:,:,:,:,:,:)
     call MEM_manager_alloc(name,size(c,KIND=IPL),2*kind(c(1,1,1,1,1,1)),DEV_)
   end subroutine
#ifndef _DOUBLE
   subroutine MEM_cd1_d(name,c)
     character(*)::name
     complex(DP) DEV_ATTR ::c(:)
     call MEM_manager_alloc(name,size(c,KIND=IPL),2*kind(c(1)),DEV_)
   end subroutine
   subroutine MEM_cd2_d(name,c)
     character(*)::name
     complex(DP) DEV_ATTR ::c(:,:)
     call MEM_manager_alloc(name,size(c,KIND=IPL),2*kind(c(1,1)),DEV_)
   end subroutine
   subroutine MEM_cd3_d(name,c)
     character(*)::name
     complex(DP) DEV_ATTR ::c(:,:,:)
     call MEM_manager_alloc(name,size(c,KIND=IPL),2*kind(c(1,1,1)),DEV_)
   end subroutine
   subroutine MEM_cd4_d(name,c)
     character(*)::name
     complex(DP) DEV_ATTR ::c(:,:,:,:)
     call MEM_manager_alloc(name,size(c,KIND=IPL),2*kind(c(1,1,1,1)),DEV_)
   end subroutine
   subroutine MEM_cd5_d(name,c)
     character(*)::name
     complex(DP) DEV_ATTR ::c(:,:,:,:,:)
     call MEM_manager_alloc(name,size(c,KIND=IPL),2*kind(c(1,1,1,1,1)),DEV_)
   end subroutine
   subroutine MEM_cd6_d(name,c)
     character(*)::name
     complex(DP) DEV_ATTR ::c(:,:,:,:,:,:)
     call MEM_manager_alloc(name,size(c,KIND=IPL),2*kind(c(1,1,1,1,1,1)),DEV_)
   end subroutine
#endif
   ! end of GPU subroutines
   !
   subroutine MEM_l1(name,l)
     character(*)::name
     logical     ::l(:)
     call MEM_manager_alloc(name,size(l,KIND=IPL),kind(l(1)),HOST_)
   end subroutine
   subroutine MEM_l2(name,l)
     character(*)::name
     logical     ::l(:,:)
     call MEM_manager_alloc(name,size(l,KIND=IPL),kind(l(1,1)),HOST_)
   end subroutine
   subroutine MEM_l3(name,l)
     character(*)::name
     logical     ::l(:,:,:)
     call MEM_manager_alloc(name,size(l,KIND=IPL),kind(l(1,1,1)),HOST_)
   end subroutine
   subroutine MEM_l4(name,l)
     character(*)::name
     logical     ::l(:,:,:,:)
     call MEM_manager_alloc(name,size(l,KIND=IPL),kind(l(1,1,1,1)),HOST_)
   end subroutine
   subroutine MEM_l5(name,l)
     character(*)::name
     logical     ::l(:,:,:,:,:)
     call MEM_manager_alloc(name,size(l,KIND=IPL),kind(l(1,1,1,1,1)),HOST_)
   end subroutine
   subroutine MEM_l6(name,l)
     character(*)::name
     logical     ::l(:,:,:,:,:,:)
     call MEM_manager_alloc(name,size(l,KIND=IPL),kind(l(1,1,1,1,1,1)),HOST_)
   end subroutine
end module y_memory
