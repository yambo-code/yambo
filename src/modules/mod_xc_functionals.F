!
! Copyright (C) 2003 M. Marques, A. Castro, A. Rubio, G. Bertsch
!
! Copyright (C) 2000-2010 A. Marini, M. Gruning and the YAMBO team
!              http://www.yambo-code.org
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
#if defined _LIBXC
 use xc_f90_types_m
#endif
 implicit none
 !
 integer     , public :: GS_xc_KIND
 integer     , public :: GS_xc_FUNCTIONAL
 real(SP), public, allocatable:: two_spin_density(:,:)
 real(SP), public, allocatable:: V_xc(:,:)
 real(SP), public, allocatable:: E_xc(:)
 real(SP), public, allocatable:: F_xc(:) 
 !
 ! whether interface to dft code used libxc (for compatibility issues)
 !
 integer, public, parameter :: XC_USED_LIBXC = 4
# if defined _LIBXC
 type xc_fnctl_t
   integer         :: family
   integer         :: kind              
   integer         :: id               
   integer         :: spin_channels     
   integer         :: flags             	
   type(xc_f90_pointer_t) :: conf
   type(xc_f90_pointer_t) :: info
 end type xc_fnctl_t
 integer, parameter :: &
 XC_FACTOR = 1000,     & 
 XC_NOT_AVAILABLE = 999999, & 
 NOXC = 0 
#else
 integer, public, parameter :: &
    XC_UNPOLARIZED       =  1, &  ! Spin unpolarized
    XC_POLARIZED         =  2     ! Spin polarized
 !
 integer, public, parameter :: &
    XC_NON_RELATIVISTIC  =  0, &  ! Functional includes or not realtivistic
    XC_RELATIVISTIC      =  1     ! corrections. Only available in some functionals.
 !
 ! Kinds
 integer, public, parameter :: &
    NONE                    = 0,  &
    XC_EXCHANGE             = 1,  &
    XC_CORRELATION          = 2,  &
    XC_EXCHANGE_CORRELATION = 3 
 !
 ! the LDAs
 integer, public, parameter :: &
    XC_LDA_X             =  1,  &  ! Exchange                  
    XC_LDA_C_WIGNER      =  2,  &  ! Wigner parametrization    
    XC_LDA_C_RPA         =  3,  &  ! Random Phase Approximation
    XC_LDA_C_HL          =  4,  &  ! Hedin & Lundqvist         
    XC_LDA_C_GL          =  5,  &  ! Gunnarson & Lundqvist     
    XC_LDA_C_XALPHA      =  6,  &  ! Slaters Xalpha           
    XC_LDA_C_VWN         =  7,  &  ! Vosko, Wilk, & Nussair    
    XC_LDA_C_PZ          =  8,  &  ! Perdew & Zunger           
    XC_LDA_C_OB_PZ       =  9,  &  ! Ortiz & Ballone (PZ)      
    XC_LDA_C_PW          = 10,  &  ! Perdew & Wang             
    XC_LDA_C_OB_PW       = 11,  &  ! Ortiz & Ballone (PW)      
    XC_LDA_C_LYP         = 12,  &  ! Lee, Yang, & Parr LDA     
    XC_LDA_C_AMGB        = 13,  &  ! Attacalite et al
    XC_LDA_C_KP          = 14      ! Kurth & Perdew
 !
 ! the GGAs
 integer, public, parameter :: &
    XC_GGA_X_PBE         = 101, &  ! Perdew, Burke & Ernzerhof exchange
    XC_GGA_C_PBE         = 102, &  ! Perdew, Burke & Ernzerhof correlation
    XC_GGA_XC_LB         = 103     ! van Leeuwen & Baerends
 !
 ! the meta-GGAs
 integer, public, parameter :: &
    XC_MGGA_X_TPSS       = 201, &  ! Perdew, Tao, Staroverov & Scuseria exchange
    XC_MGGA_C_TPSS       = 202     ! Perdew, Tao, Staroverov & Scuseria correlation
 !
 contains
   !
   character(lchlen) function xc_string(kind,functional)
     !     
     integer        :: kind,functional
     character(4)   :: ch_kind
     integer          :: i1,fun
     character(*)     :: ch_fun
     type(xc_fnctl_t) :: xcs
     !
     select case (kind)
       case(-1,NONE)
         ch_kind=' '
       case(XC_EXCHANGE)
         ch_kind='(x)'
       case(XC_CORRELATION)
         ch_kind='(c)'
       case(XC_EXCHANGE_CORRELATION)
         ch_kind='(xc)'
     end select
     !
     select case (functional)
       case(-1,3,5,7,9)
         xc_string='Unknown '//trim(ch_kind)
       case(NONE)
         xc_string='none'
       case(XC_LDA_X)
         xc_string='Exchange only '//trim(ch_kind)
       case(XC_LDA_C_WIGNER)
         xc_string='Wigner '//trim(ch_kind)
       case(XC_LDA_C_HL)
         xc_string='Hedin & Lundqvist '//trim(ch_kind)
       case(XC_LDA_C_XALPHA)
         xc_string='Slaters Xalpha '//trim(ch_kind)
       case(XC_LDA_C_PZ)
         xc_string='Perdew & Zunger '//trim(ch_kind)
       case(XC_LDA_C_PW)
         xc_string='Perdew & Wang '//trim(ch_kind)
       case(XC_LDA_C_KP)
         xc_string='Kurth & Perdew '//trim(ch_kind)
     end select
     !
   end function
#endif
!
#if defined _LIBXC
  !
 contains
   !
  subroutine xc_setup_fnctl(f,fun,nsp)
    !
    use xc_f90_types_m
    use xc_f90_lib_m, ONLY:xc_f90_func_init,xc_f90_info_family,&
                           xc_f90_info_kind,xc_f90_info_flags
    implicit none
    integer, intent(in) :: fun, nsp
    type(xc_fnctl_t), intent(out) :: f(2)
    integer :: ixc
    type(xc_f90_pointer_t) :: p,info 
    !
    call xc_f90_func_init(p,info,NOXC,1)
    !
    f(1)%id = fun/XC_FACTOR
    f(2)%id = fun-f(1)%id*XC_FACTOR
    !
    do ixc=1,2
      if (f(ixc)%id==0) cycle
      call xc_f90_func_init(p,info,f(ixc)%id,nsp) 
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
  character(lchlen) function xc_string(f)
    !
    use xc_f90_types_m
    use xc_f90_lib_m, ONLY:XC_EXCHANGE,XC_EXCHANGE_CORRELATION,&
                           XC_CORRELATION,XC_FAMILY_UNKNOWN,&
                           XC_FAMILY_NONE, XC_FAMILY_LDA,XC_FAMILY_GGA, &
                           XC_FAMILY_MGGA, XC_FAMILY_LCA, XC_FAMILY_OEP, &
                           XC_FAMILY_HYB_GGA,xc_f90_info_name
    implicit none
    type(xc_fnctl_t), intent(in) :: f
    character(lchlen) :: ch_fun
    character(4) :: ch_kind
    character(7) :: ch_fam
    !
    ch_fun = "none"
    if (f%id>0) call xc_f90_info_name(f%info,ch_fun)
    ch_kind = '  '
    select case(f%kind)
    case(XC_EXCHANGE)
      ch_kind='X=>'
    case(XC_CORRELATION)
      ch_kind='C=>'
    case(XC_EXCHANGE_CORRELATION)
      ch_kind='XC=>'         
    end select
!
    select case(f%family)
      case(XC_FAMILY_UNKNOWN)
        ch_fam  = ' [NONE]'
      case(XC_FAMILY_LDA)
        ch_fam  = ' [LDA]'
      case(XC_FAMILY_GGA)
        ch_fam  = ' [GGA]'
      case(XC_FAMILY_MGGA)
        ch_fam  = ' [MGGA]'
      case(XC_FAMILY_LCA)
        ch_fam  = ' [LCA]'
      case(XC_FAMILY_OEP)
        ch_fam  = ' [OEP]'
      case(XC_FAMILY_HYB_GGA)
        ch_fam  = ' [HYB]'
    end select  
    write(xc_string,'(3a)') ch_kind, trim(ch_fun), ch_fam
  end function xc_string
  !
#endif   
  !
end module xc_functionals
