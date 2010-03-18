!
! Copyright (C) 2000-2010 Myrta Gruning and the YAMBO team
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
module dftxc2libxc
 !
 use libxc_funcs_m
 use xc_functionals, ONLY:XC_FACTOR,XC_NOT_AVAILABLE
 implicit none
 !
 integer, parameter :: N_Abinit_XC = 28
 !
 integer, parameter, dimension(N_Abinit_XC) :: A2LibXC =&
& (/XC_LDA_XC_TETER93*XC_FACTOR,         & !ixc= 1
    XC_LDA_X*XC_FACTOR+XC_LDA_C_PZ,      & !ixc= 2 
    XC_LDA_X*XC_FACTOR+XC_LDA_C_PZ,      & !ixc= 3 (???) 
    XC_LDA_X*XC_FACTOR+XC_LDA_C_WIGNER,  & !ixc= 4 
    XC_LDA_X*XC_FACTOR+XC_LDA_C_HL,      & !ixc= 5 
    XC_LDA_X*XC_FACTOR+XC_LDA_C_XALPHA,  & !ixc= 6
    XC_LDA_X*XC_FACTOR+XC_LDA_C_PW,      & !ixc= 7
    XC_LDA_X*XC_FACTOR,                  & !ixc= 8 
    XC_LDA_X*XC_FACTOR+XC_LDA_C_PW,      & !ixc= 9 (???)
    XC_NOT_AVAILABLE,                    & !ixc= 10
    XC_GGA_X_PBE*XC_FACTOR+XC_GGA_C_PBE, & !ixc= 11
    XC_GGA_X_PBE*XC_FACTOR,              & !ixc= 12
    XC_GGA_XC_LB*XC_FACTOR,              & !ixc= 13
    XC_GGA_X_PBE_R*XC_FACTOR+XC_GGA_C_PBE,&!ixc= 14 (???)
    XC_GGA_X_RPBE*XC_FACTOR+XC_GGA_C_PBE, &!ixc= 15 (???)
    XC_GGA_XC_HCTH_93*XC_FACTOR,          &!ixc= 16 
    XC_GGA_XC_HCTH_120*XC_FACTOR,         &!ixc= 17
    XC_NOT_AVAILABLE,                     &!ixc= 18
    XC_NOT_AVAILABLE,                     &!ixc= 19
    XC_NOT_AVAILABLE,                     &!ixc= 20
    XC_NOT_AVAILABLE,                     &!ixc= 21
    XC_NOT_AVAILABLE,                     &!ixc= 22
    XC_GGA_X_WC*XC_FACTOR,                &!ixc= 23 (???)
    XC_NOT_AVAILABLE,                     &!ixc= 24
    XC_NOT_AVAILABLE,                     &!ixc= 25
    XC_GGA_XC_HCTH_147*XC_FACTOR,         &!ixc= 26 
    XC_GGA_XC_HCTH_407*XC_FACTOR,         &!ixc= 27 
    XC_NOT_AVAILABLE/)                    !ixc= 28
 !    
end module
