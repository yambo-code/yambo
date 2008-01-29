!
! Copyright (C) 2003 M. Marques, A. Castro, A. Rubio, G. Bertsch
!
! Copyright (C) 2000-2005 A. Marini and the SELF team
!         http://www.fisica.uniroma2.it/~self
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
 integer     , public :: GS_xc_KIND,GS_xc_FUNCTIONAL
 real(SP), public, allocatable:: el_mag(:,:)
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
#if defined _SC
 ! the OEP/KLI/CEDA
 integer, public, parameter :: &
   XC_OEP_EXX           = 301    ! OEP-EXX
 !
 ! the GKS
 integer, public, parameter :: &
   XC_GKS_HF            = 401, &  ! Hartree-Fock
   XC_GKS_SX            = 402, &  ! Screened-eXchange
   XC_GKS_SXLDA         = 403, &  ! Kleinman-Bylander Screened-Exchange LDA
   XC_GKS_CHSX          = 404     ! Coulomb-Hole Screened-eXchange
#endif
 !
 contains
   !
   character(lchlen) function xc_string(kind,functional)
     !
     use stderr, ONLY:string_pack
     implicit none
     integer        :: kind,functional
     character(4)   :: ch_kind
     !
     select case (kind)
       case(-1)
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
        xc_string=string_pack('Unknown ',ch_kind)
       case(XC_LDA_X)
        xc_string=string_pack('Exchange only ',ch_kind)
       case(XC_LDA_C_WIGNER)
        xc_string=string_pack('Wigner ',ch_kind)
       case(XC_LDA_C_HL)
        xc_string=string_pack('Hedin & Lundqvist ',ch_kind)
       case(XC_LDA_C_XALPHA)
        xc_string=string_pack('Slaters Xalpha ',ch_kind)
       case(XC_LDA_C_PZ)
        xc_string=string_pack('Perdew & Zunger ',ch_kind)
       case(XC_LDA_C_PW)
        xc_string=string_pack('Perdew & Wang ',ch_kind)
       case(XC_LDA_C_KP)
        xc_string=string_pack('Kurth & Perdew ',ch_kind)
#if defined _SC
       case(XC_OEP_EXX)
        xc_string=string_pack('Exact-Exchange (OEP) ',ch_kind)
       case(XC_GKS_HF)
        xc_string=string_pack('Hartree-Fock (nonlocal) ',ch_kind)
       case(XC_GKS_SXLDA)  
        xc_string=string_pack('Screened-eXchange LDA (nonlocal) ',ch_kind)
       case(XC_GKS_CHSX)  
        xc_string=string_pack('Coulomb-Hole Screened-eXchange (nonlocal) ',ch_kind)
#endif
     end select
     !
   end function
   !
end module xc_functionals
