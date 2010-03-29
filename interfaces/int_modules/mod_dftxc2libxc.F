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
 use xc_functionals, ONLY:XC_FACTOR,XC_NOT_AVAILABLE, NOXC
 implicit none
 !
 integer, parameter :: N_Abinit_XC = 28
 integer, parameter :: N_PW_X = 9, N_PW_C = 12, N_PW_GX = 12,&
&                      N_PW_GC = 9
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
  character(len=6),parameter,dimension(N_PW_X) :: PW_X=&  
& (/' NOX', ' SLA', ' SL1', ' RXC', ' OEP', '  HF', 'PB0X', 'B3LP', ' KZK'/)
  character(len=6),parameter,dimension(N_PW_C) :: PW_C=&  
& (/ ' NOC', '  PZ', ' VWN', ' LYP', '  PW', ' WIG', '  HL', ' OBZ', &
              ' OBW', '  GL' , 'B3LP', ' KZK' /)
  character(len=6),parameter,dimension(N_PW_GX) :: PW_GX=& 
& (/ 'NOGX', ' B88', ' GGX', ' PBX',  ' RPB', 'HCTH', 'OPTX',&
               'META', 'PB0X', 'B3LP',' PSX', ' WCX'/)
  character(len=6),parameter,dimension(N_PW_GC) :: PW_GC=&
& (/ 'NOGC', ' P86', ' GGC', 'BLYP', ' PBC', 'HCTH', 'META',&
                'B3LP', ' PSC'/)
 integer,parameter,dimension(N_PW_X) :: PWX2LIBXC=&
& (/ NOXC, XC_LDA_X, XC_LDA_X*XC_FACTOR+XC_LDA_C_XALPHA, XC_NOT_AVAILABLE, &
&    XC_NOT_AVAILABLE, XC_NOT_AVAILABLE, XC_NOT_AVAILABLE, XC_NOT_AVAILABLE,&
     XC_NOT_AVAILABLE/)
 integer,parameter,dimension(N_PW_C) :: PWC2LIBXC=&
& (/ NOXC, XC_LDA_C_PZ, XC_LDA_C_VWN, XC_GGA_C_LYP, XC_LDA_C_PW,&
&    XC_LDA_C_WIGNER, XC_LDA_C_HL, XC_LDA_C_OB_PZ, XC_LDA_C_OB_PW,XC_LDA_C_GL,&
&    XC_NOT_AVAILABLE, XC_NOT_AVAILABLE/)
 integer,parameter,dimension(N_PW_GX) :: PWGX2LIBXC=&
& (/ NOXC, XC_GGA_X_B88, XC_GGA_X_PW91, XC_GGA_X_PBE, XC_GGA_X_PBE_R,&
&    XC_GGA_XC_HCTH_93, XC_GGA_X_OPTX, XC_MGGA_X_TPSS, XC_HYB_GGA_XC_PBEH,&
&    XC_HYB_GGA_XC_B3LYP, XC_GGA_X_PBE_SOL, XC_GGA_X_WC/)
 integer,parameter,dimension(N_PW_GC) :: PWGC2LIBXC=&
& (/ NOXC, XC_GGA_C_P86, XC_GGA_C_PW91, XC_GGA_C_LYP, XC_GGA_C_PBE, XC_GGA_XC_HCTH_93,&
&    XC_MGGA_C_TPSS, XC_HYB_GGA_XC_B3LYP, XC_GGA_C_PBE_SOL/)
end module


! character(len=6),parameter,dimension(N_PW_XC) :: PW_XC=&
!& (/ 'BP', 'BLYP', 'PBE', 'REVPBE', 'PBESOL', 'HCTH', 'OLYP', &
!&    'TPSS', 'WC', 'PBE0', 'B3LYP'/)
! integer,parameter,dimension(N_PW_XC) :: PWXC2LIBXC=&
!& (/ XC_GGA_X_B88*XC_FACTOR+XC_GGA_C_P86,&
!&    XC_GGA_X_PW91*XC_FACTOR+XC_GGA_C_PW91,&
!&    XC_GGA_X_PBE*XC_FACTOR+XC_GGA_C_PBE,&
!&    XC_GGA_X_PBE_R*XC_FACTOR+XC_GGA_C_PBE,&
!&    XC_GGA_X_PBE_SOL*XC_FACTOR+XC_GGA_C_PBE_SOL,&
!&    XC_GGA_XC_HCTH_120,&
!&    XC_GGA_X_OPTX*XC_FACTOR+XC_GGA_C_LYP,&
!&    XC_MGGA_X_TPSS*XC_FACTOR+XC_MGGA_C_TPSS,&
!&    XC_GGA_X_WC*XC_FACTOR+XC_GGA_C_PBE,&
!&    XC_HYB_GGA_XC_B3LYP/)
