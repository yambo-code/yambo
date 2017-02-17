!
! Copyright (C) 2003 M. Marques, A. Castro, A. Rubio, G. Bertsch
!
! Copyright (C) 2000-2012 A. Marini and the YAMBO team
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
 implicit none
 !
 integer     , public :: GS_xc_KIND
 integer     , public :: GS_xc_FUNCTIONAL
 real(SP), public, allocatable:: two_spin_density(:,:)
 real(SP), public, allocatable:: V_xc(:,:)
 real(SP), public, allocatable:: E_xc(:)
 real(SP), public, allocatable:: F_xc(:) 
 !
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
    XC_NOT_AVAILABLE        = -1, &     
    NONE                    = 0,  &
    XC_EXCHANGE             = 1,  &
    XC_CORRELATION          = 2,  &
    XC_EXCHANGE_CORRELATION = 3 
 !
 ! Families
 integer, public, parameter :: &
    XC_FAMILY_UNKNOWN = -1, &
    XC_FAMILY_NONE = 0, &
    XC_FAMILY_LDA = 1, &
    XC_FAMILY_GGA = 2, &
    XC_FAMILY_MGGA = 4, &
    XC_FAMILY_LCA = 8, &
    XC_FAMILY_OEP = 16, &
    XC_FAMILY_HYB_GGA = 32  
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
    XC_LDA_C_KP          = 14,  &  ! Kurth & Perdew
    XC_LDA_XC_TETER93    = 15,  &  ! Teter Parametrization(93) 
    XC_LDA_C_VWN_RPA     = 16,  &  ! Vosko, Wilk, & Nussair (RPA) 
    XC_LDA_C_PZ_MOD      = 17,  &  ! Perdew & Zunger (Modified) 
    XC_LDA_C_PW_MOD      = 18,  &  ! Perdew & Wang (Modified) 
    XC_LDA_C_vBH         = 19      ! von Barth & Hedin
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
 ! strings with XC description 
 !
 integer, public, parameter :: N_Available_LDA = 19
 character(*), parameter, dimension( N_Available_LDA) :: XC_LDA_DESCRIPTION =&
& (/"Exchange only               ",&                 
    "Wigner parametrization      ",&    
    "Random Phase Approximation  ",&
    "Hedin & Lundqvist           ",&         
    "Gunnarson & Lundqvist       ",&    
    "Slaters Xalpha              ",&          
    "Vosko, Wilk, & Nussair      ",&   
    "Perdew & Zunger             ",&         
    "Ortiz & Ballone(PZ)         ",&      
    "Perdew & Wang               ",&           
    "Ortiz & Ballone(PW)         ",&     
    "Lee, Yang, & Parr LDA       ",&     
    "Attacalite et al            ",&
    "Kurth & Perdew              ",&
    "Teter Parametrization(93)   ",&
    "Vosko, Wilk, & Nussair(RPA) ",& 
    "Perdew & Zunger (Modified)  ",&
    "Perdew & Wang (Modified)    ",&
    "von Barth & Hedin           "/)
 !
 contains
   !
   logical function xc_OnlyInLibxc(func)
     integer func
     xc_OnlyInLibxc = .false.
     if (func==XC_LDA_XC_TETER93.or. &
      func==XC_LDA_C_RPA.or. &
      func==XC_LDA_C_GL.or.&
      func==XC_LDA_C_VWN.or.&
      func==XC_LDA_C_VWN_RPA.or.&
      func==XC_LDA_C_OB_PZ.or.&
      func==XC_LDA_C_OB_PW.or.&
      func==XC_LDA_C_PZ_MOD.or.&
      func== XC_LDA_C_vBH.or.&
      func==XC_LDA_C_AMGB.or.&
      func==XC_LDA_C_PW_MOD)  xc_OnlyInLibxc=.true.
   end function xc_OnlyInLibxc
   !     
   character(lchlen) function xc_string(kind,functional)
     !     
     integer        :: kind,functional
     character(4)   :: ch_kind
     integer        :: i1,fun
     !
     select case (kind)
       case(NONE)
         ch_kind=' '
       case(XC_EXCHANGE)
         ch_kind='(x)'
       case(XC_CORRELATION)
         ch_kind='(c)'
       case(XC_EXCHANGE_CORRELATION)
         ch_kind='(xc)'
     end select
     !
     select case(functional)
       case (XC_NOT_AVAILABLE)
         xc_string='Unknown '//trim(ch_kind)
       case (NONE)
         xc_string='None '//trim(ch_kind)
       case (XC_LDA_X:N_Available_LDA)
         xc_string=trim(XC_LDA_DESCRIPTION(functional))//' '//trim(ch_kind) 
    end select
    !
   end function
   !
end module xc_functionals
