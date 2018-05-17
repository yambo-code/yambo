!
! Copyright (C) 2003 M. Marques, A. Castro, A. Rubio, G. Bertsch
!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, MG
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
 use xc_f90_lib_m
 use xc_f90_types_m
 use libxc_funcs_m
 implicit none
 !
 integer,  public:: GS_xc_KIND
 integer,  public:: GS_xc_FUNCTIONAL
 real(SP), public:: GS_exx_FRACTION, GS_exx_SCREENING
 real(SP), public:: E_xc_val
 real(SP), public, allocatable:: magn(:,:)
 real(SP), public, allocatable:: F_xc(:,:,:)
 real(SP), public, allocatable:: V_xc(:,:)
 real(SP), public, allocatable:: E_xc(:)
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
   type(xc_f90_pointer_t) :: conf
   type(xc_f90_pointer_t) :: info
 end type xc_fnctl_t
 !
 integer, public, parameter :: &
    XC_FACTOR        = 1000,&
    XC_NOT_AVAILABLE = 999999, & 
    NOXC             = 0, &    
    XC_LDA_C_KP      = 99
 !
 character(*), parameter, dimension(3) :: XC_KIND_DESCR = (/'(X) ','(C) ','(XC)'/)
 !
 interface
   !
   subroutine XC_potential_driver(en,Xk,KIND,FUNCTIONAL,ORDER,EXX_FRACTION,EXX_SCREENING)
     use pars,          ONLY:SP,DP,RZERO
     use R_lattice,     ONLY:bz_samp
     use electrons,     ONLY:levels
     use com,           ONLY:msg,warning
     use libxc_funcs_m, ONLY: XC_LDA_X, XC_LDA_C_PW
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
     use xc_f90_types_m
     use xc_f90_lib_m, ONLY:xc_f90_func_init,xc_f90_info_family,&
          xc_f90_info_kind,xc_f90_info_flags,xc_f90_hyb_exx_coef
     implicit none
     integer, intent(in) :: fun, nsp
     type(xc_fnctl_t), intent(out) :: f(2)
     integer :: ixc
     type(xc_f90_pointer_t) :: p,info 
     !
     do ixc=1,2
       f(ixc)%id=XC_libxcID(fun,ixc)
       if (f(ixc)%id==NOXC) cycle
       call xc_f90_func_init(p,info,f(ixc)%id,nsp)
       call xc_f90_info_name(info,f(ixc)%name)
       f(ixc)%family=xc_f90_info_family(info)
       f(ixc)%kind=xc_f90_info_kind(info)
       f(ixc)%id=fun
       f(ixc)%spin_channels=nsp
       f(ixc)%flags=xc_f90_info_flags(info)
       f(ixc)%conf=p
       f(ixc)%info=info
     enddo
     !
   end subroutine xc_setup_fnctl
   !
   subroutine xc_setup_hyb(f,exx_frac,exx_screen)
     !
     use xc_f90_types_m
     use xc_f90_lib_m, ONLY:xc_f90_hyb_gga_xc_hse_set_par,&
&                           xc_f90_hyb_gga_xc_pbeh_set_par,&
&                           XC_HYB_GGA_XC_HSE06,XC_HYB_GGA_XC_PBEH,&
&                           XC_HYB_GGA_XC_B3LYP
     implicit none
     type(xc_fnctl_t), intent(inout) :: f(2)
     real(SP), intent(in) :: exx_frac, exx_screen 
     integer :: ixc
     real(xc_f90_kind) :: exx_frac_, exx_screen_
     !
     exx_frac_   = exx_frac
     exx_screen_ = exx_screen
     do ixc=1,2
       if (f(ixc)%family.ne.XC_FAMILY_HYB_GGA) return
       f(ixc)%exx_fraction=exx_frac
       f(ixc)%exx_screening=exx_screen
       if (XC_libxcID(f(ixc)%id,ixc)==XC_HYB_GGA_XC_HSE06) &
&        call xc_f90_hyb_gga_xc_hse_set_par(f(ixc)%conf,exx_frac_,exx_screen_)
       if ((XC_libxcID(f(ixc)%id,ixc)==XC_HYB_GGA_XC_PBEH).or.&
&          (XC_libxcID(f(ixc)%id,ixc)==XC_HYB_GGA_XC_B3LYP)) &
&        call xc_f90_hyb_gga_xc_pbeh_set_par(f(ixc)%conf,exx_frac_)
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
