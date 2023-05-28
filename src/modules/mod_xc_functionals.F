!
! Copyright (C) 2003 M. Marques, A. Castro, A. Rubio, G. Bertsch
!
!        Copyright (C) 2000-2023 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, MG, DV
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
module xc_functionals
 !
 use pars,      ONLY:SP,lchlen
 use xc_f03_lib_m
 !
 implicit none
 !
 integer  :: xc_spin(2)
 integer, allocatable :: xc_spin_map1(:,:)
 integer, allocatable :: xc_spin_map2(:,:,:)
 !
 integer,  public:: GS_xc_KIND
 integer,  public:: GS_xc_FUNCTIONAL
 real(SP), public:: GS_exx_FRACTION, GS_exx_SCREENING
 real(SP), public:: E_xc_val=0.0
 real(SP), public, allocatable:: magn(:,:)
 real(SP), public, allocatable:: F_xc(:,:,:)
 real(SP), public, allocatable:: V_xc(:,:)
 real(SP), public, allocatable:: E_xc(:)
 !
 complex(SP), allocatable:: F_xc_mat(:,:,:,:,:)
 !
 ! Messagging
 !
 character(lchlen) :: current_xc_string = ' '
 logical :: LIBXC_message_done = .FALSE.
 !
 type xc_fnctl_t
   integer         :: family
   integer         :: kind              
   integer         :: id               
   integer         :: spin_channels
   integer         :: flags
   real(SP) :: exx_fraction
   real(SP) :: exx_screening
   character(lchlen) :: name
   type(xc_f03_func_t)      :: conf
   type(xc_f03_func_info_t) :: info
 end type xc_fnctl_t
 !
 integer, public, parameter :: &
    XC_FACTOR        = 1000,&
    XC_NOT_AVAILABLE = 999999, & 
    NOXC             = 0, &    
    XC_LDA_C_KP      = 99, &
    XC_HYB_GGA_XC_GAUPBE        = 999
 !
 character(*), parameter, dimension(3) :: XC_KIND_DESCR = (/'(X) ','(C) ','(XC)'/)
 !
 interface
   !
   subroutine XC_potential_driver(en,Xk,KIND,FUNCTIONAL,ORDER,EXX_FRACTION,EXX_SCREENING)
     use pars,          ONLY:SP,DP,RZERO
     use R_lattice,     ONLY:bz_samp
     use electrons,     ONLY:levels
     use com,           ONLY:msg
     implicit none
     type(levels),  intent(in)       :: en
     type(bz_samp), intent(in)       :: Xk
     integer,       intent(inout)    :: KIND,FUNCTIONAL
     integer,       intent(in)       :: ORDER
     real(SP),      intent(inout),optional :: EXX_FRACTION,EXX_SCREENING
   end subroutine XC_potential_driver
   !
   subroutine XC_libxc_driver(en,Xk,FUNCTIONAL,ORDER,EXX_FRACTION,EXX_SCREENING)
     use pars,          ONLY:SP
     use R_lattice,     ONLY:bz_samp
     use electrons,     ONLY:levels
     implicit none
     type(levels) ::en
     type(bz_samp)::Xk
     integer      ::FUNCTIONAL,ORDER
     real(SP), optional :: EXX_FRACTION,EXX_SCREENING
   end subroutine XC_libxc_driver
   !
 end interface
 !
 contains
   !
   integer function XC_libxcID(yamboID,which)
     !
     integer, intent(in) :: yamboID,which
     ! ws:
     integer func(2)
     !
     if (yamboID==XC_NOT_AVAILABLE) then
       XC_libxcID = XC_NOT_AVAILABLE
       return
     end if
     !
     if (which.gt.2.or.which.lt.1) return
     func(1) = yamboID/XC_factor 
     func(2) = yamboID-func(1)*XC_factor
     XC_libxcID = func(which)
     !
   end function XC_libxcID
   !
   subroutine xc_setup_fnctl(f,fun,nsp)
     !
     !use xc_f03_lib_m
     implicit none
     integer, intent(in) :: fun, nsp
     ! YAMBO internal
     type(xc_fnctl_t), intent(out) :: f(2)
     integer :: ixc
     type(xc_f03_func_t)      :: xc_func
     type(xc_f03_func_info_t) :: xc_info
     !
     ! Example
     !program xcinfo
     ! use xc_f03_lib_m
     ! implicit none
     ! type(xc_f03_func_t) :: xc_func
     ! type(xc_f03_func_info_t) :: xc_info
     ! integer :: i
     ! character(len=120) :: kind, family     !! Initialize the functional
     ! call xc_f03_func_init(xc_func, XC_GGA_X_B88, XC_UNPOLARIZED)
     ! xc_info = xc_f03_func_get_info(xc_func)
     ! ! Get the type of the functional
     ! select case(xc_f03_func_info_get_kind(xc_info))
     ! select case (xc_f03_func_info_get_family(xc_info))
     ! ! Print out information
     ! write(*,'("The functional ''", a, "'' is ", a, ", it belongs to the ''", a, "'' family and is defined in the reference(s):")') &
     !   trim(xc_f03_func_info_get_name(xc_info)), trim(kind), trim(family)
     ! ! Print out references
     ! i = 0
     ! do while(i >= 0)
     !  write(*, '(a,i1,2a)') '[', i+1, '] ', trim(xc_f03_func_reference_get_ref(xc_f03_func_info_get_references(xc_info, i)))
     ! end do
     ! Free the memory
     ! call xc_f03_func_end(xc_func)
     !end program xcinfo
     !
     do ixc=1,2
       f(ixc)%id=XC_libxcID(fun,ixc)
       if (f(ixc)%id==NOXC) cycle
       call xc_f03_func_init(xc_func,f(ixc)%id,nsp)
       xc_info = xc_f03_func_get_info(xc_func)
       f(ixc)%family=xc_f03_func_info_get_family(xc_info)
       f(ixc)%kind=xc_f03_func_info_get_kind(xc_info)
       f(ixc)%name=trim(xc_f03_func_info_get_name(xc_info))
       f(ixc)%id=fun
       f(ixc)%spin_channels=nsp
       f(ixc)%flags=999
       !f(ixc)%flags=xc_f03_info_flags(xc_info)
       f(ixc)%conf=xc_func
       f(ixc)%info=xc_info
     enddo
     !
   end subroutine xc_setup_fnctl
   !
   subroutine xc_setup_hyb(f,exx_frac,exx_screen)
     !
     use pars,         ONLY:DP
     use xc_f03_lib_m, ONLY:XC_HYB_GGA_XC_HSE06,XC_HYB_GGA_XC_PBEH,XC_HYB_GGA_XC_B3LYP
     implicit none
     type(xc_fnctl_t), intent(inout) :: f(2)
     real(SP), intent(in) :: exx_frac, exx_screen 
     integer :: ixc
     real(DP) :: exx_frac_, exx_screen_
     !real(xc_f03_kind) :: exx_frac_, exx_screen_
     !
     exx_frac_   = real(exx_frac,DP)
     exx_screen_ = real(exx_screen,DP)
     do ixc=1,2
       if (f(ixc)%family.ne.XC_FAMILY_HYB_GGA) return
       f(ixc)%exx_fraction=exx_frac
       f(ixc)%exx_screening=exx_screen
       if (XC_libxcID(f(ixc)%id,ixc)==XC_HYB_GGA_XC_HSE06) &
       &  call xc_f03_func_set_ext_params(f(ixc)%conf,(/exx_frac_,exx_screen_/))
       if (XC_libxcID(f(ixc)%id,ixc)==XC_HYB_GGA_XC_PBEH) & 
       &  call xc_f03_func_set_ext_params(f(ixc)%conf,(/exx_frac_/))
     enddo    
   end subroutine xc_setup_hyb
   
   character(lchlen) function xc_string(functional)
     !     
     integer        :: functional
     character(4)   :: ch_kind
     type(xc_fnctl_t) :: f(2)
     integer        :: ixc
     !
     xc_string = ''
     if (functional.eq.XC_HYB_GGA_XC_GAUPBE*XC_FACTOR) then
        xc_string="GAUPBE(XC)" 
        return
     endif
     call xc_setup_fnctl(f,functional,XC_UNPOLARIZED)
     !
     do ixc = 1,2  
       if (f(ixc)%id==NOXC) cycle
       if (ixc==2) xc_string= trim(xc_string)//'+'
       ch_kind = XC_KIND_DESCR(f(ixc)%kind+1)
       xc_string= trim(xc_string)//trim(f(ixc)%name)//trim(ch_kind)
     end do
     !
   end function xc_string
   !
 end module xc_functionals
