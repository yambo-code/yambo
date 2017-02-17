!
! Copyright (C) 2000-2012 Myrta Gruning and the YAMBO team
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
module mod_xc2y
 !
 use xc_functionals
 implicit none
 !
 integer, parameter :: N_Abinit_XC = 28, N_LibXC_LDA= 20 
!  integer, parameter :: N_PW_X = 9, N_PW_C = 12, N_PW_GX = 12,&
! &                      N_PW_GC = 9
 !
 integer, parameter, dimension(N_Abinit_XC) :: XC_A2Y =&
& (/XC_LDA_XC_TETER93,         & !ixc= 1
    XC_LDA_C_PZ,               & !ixc= 2 
    XC_LDA_C_PZ,               & !ixc= 3 (???) 
    XC_LDA_C_WIGNER,           & !ixc= 4 
    XC_LDA_C_HL,               & !ixc= 5 
    XC_LDA_C_XALPHA,           & !ixc= 6
    XC_LDA_C_PW,               & !ixc= 7
    XC_LDA_X,                  & !ixc= 8 
    XC_LDA_C_PW,               & !ixc= 9 (???)
    XC_NOT_AVAILABLE,          & !ixc= 10
    XC_NOT_AVAILABLE,          & !ixc= 11
    XC_NOT_AVAILABLE,          & !ixc= 12
    XC_NOT_AVAILABLE,          & !ixc= 13
    XC_NOT_AVAILABLE,          &!ixc= 14 (???)
    XC_NOT_AVAILABLE,          &!ixc= 15 (???)
    XC_NOT_AVAILABLE,          &!ixc= 16 
    XC_NOT_AVAILABLE,          &!ixc= 17
    XC_NOT_AVAILABLE,          &!ixc= 18
    XC_NOT_AVAILABLE,          &!ixc= 19
    XC_NOT_AVAILABLE,          &!ixc= 20
    XC_NOT_AVAILABLE,          &!ixc= 21
    XC_NOT_AVAILABLE,          &!ixc= 22
    XC_NOT_AVAILABLE,          &!ixc= 23 (???)
    XC_NOT_AVAILABLE,          &!ixc= 24
    XC_NOT_AVAILABLE,          &!ixc= 25
    XC_NOT_AVAILABLE,          &!ixc= 26
    XC_NOT_AVAILABLE,          &!ixc= 27
    XC_NOT_AVAILABLE/)         !ixc= 28
 integer, parameter, dimension(N_LibXC_LDA) :: LDA_Libxc2Y =&
      & (/XC_LDA_X,    &   !1
      XC_LDA_C_WIGNER, &   !2
      XC_LDA_C_RPA,    &   !3
      XC_LDA_C_HL,     &   !4
      XC_LDA_C_GL,     &   !5
      XC_LDA_C_XALPHA, &   !6
      XC_LDA_C_VWN,    &   !7
      XC_LDA_C_VWN_RPA,&   !8
      XC_LDA_C_PZ,     &   !9
      XC_LDA_C_PZ_MOD, &   !10
      XC_LDA_C_OB_PZ,  &   !11
      XC_LDA_C_PW,     &   !12
      XC_LDA_C_PW_MOD, &   !13
      XC_LDA_C_OB_PW,  &   !14
      XC_NOT_AVAILABLE,&   !15
      XC_NOT_AVAILABLE,&   !16
      XC_LDA_C_vBH,    &   !17
      XC_NOT_AVAILABLE,&   !18
      XC_NOT_AVAILABLE,&   !19
      XC_LDA_XC_TETER93/)  !20       
contains
  subroutine XC_Abinit2Yambo(abint_func,yambo_func,yambo_kind)
    integer abint_func, yambo_func,yambo_kind
    !
    yambo_func = XC_NOT_AVAILABLE
    yambo_kind = NONE
    !
    if (abint_func == 0) then
      yambo_func = NONE
    else
      if (abint_func.le.N_Abinit_XC) yambo_func = XC_A2Y(abint_func)
      yambo_kind = XC_EXCHANGE
      if (yambo_func.ne.XC_LDA_X) yambo_kind = yambo_kind +  XC_CORRELATION  
    end if
      !
    end subroutine XC_Abinit2Yambo
    !
    subroutine XC_Libxc2Yambo(libxc_func,yambo_func,yambo_kind)
      integer libxc_func,func(2),yambo_func,yambo_kind,ii,ixc
      integer, parameter :: factor = 1000
      !
      ! This serves when abinit was used with libxc
      ! in that case, ixc < 0 
      !
      ixc = libxc_func
      if (libxc_func<0) ixc = -libxc_func
      !
      func(1) = ixc/factor 
      func(2) = ixc-func(1)*factor
      yambo_func = XC_NOT_AVAILABLE     
      yambo_kind = NONE
      !      
      do ii=1,2
        select case (func(ii))
          case(0)
            if (yambo_func==NONE.or.yambo_func==XC_NOT_AVAILABLE)&
        	& yambo_func = NONE 
          case(1)
            yambo_func = LDA_Libxc2Y(func(ii))
            yambo_kind = yambo_kind + XC_EXCHANGE
          case(2:N_LibXC_LDA)
            yambo_func = LDA_Libxc2Y(func(ii))
            yambo_kind = yambo_kind + XC_CORRELATION
            if (yambo_func==XC_LDA_XC_TETER93) yambo_kind = XC_EXCHANGE_CORRELATION
          case DEFAULT
            yambo_func = XC_NOT_AVAILABLE
            yambo_kind = NONE
        end select
        if (yambo_func==XC_NOT_AVAILABLE) EXIT
      end do
      !
    end subroutine XC_Libxc2Yambo
  !
end module mod_xc2y
!
! FOR PWSCF: FOR LATER USE
! 
!   character(len=6),parameter,dimension(N_PW_X) :: PW_X=&  
! & (/' NOX', ' SLA', ' SL1', ' RXC', ' OEP', '  HF', 'PB0X', 'B3LP', ' KZK'/)
!   character(len=6),parameter,dimension(N_PW_C) :: PW_C=&  
! & (/ ' NOC', '  PZ', ' VWN', ' LYP', '  PW', ' WIG', '  HL', ' OBZ', &
!               ' OBW', '  GL' , 'B3LP', ' KZK' /)
!   character(len=6),parameter,dimension(N_PW_GX) :: PW_GX=& 
! & (/ 'NOGX', ' B88', ' GGX', ' PBX',  ' RPB', 'HCTH', 'OPTX',&
!                'META', 'PB0X', 'B3LP',' PSX', ' WCX'/)
!   character(len=6),parameter,dimension(N_PW_GC) :: PW_GC=&
! & (/ 'NOGC', ' P86', ' GGC', 'BLYP', ' PBC', 'HCTH', 'META',&
!                 'B3LP', ' PSC'/)
!  integer,parameter,dimension(N_PW_X) :: PWX2LIBXC=&
! & (/ NOXC, XC_LDA_X, XC_LDA_X*XC_FACTOR+XC_LDA_C_XALPHA, XC_NOT_AVAILABLE, &
! &    XC_NOT_AVAILABLE, XC_NOT_AVAILABLE, XC_NOT_AVAILABLE, XC_NOT_AVAILABLE,&
!      XC_NOT_AVAILABLE/)
!  integer,parameter,dimension(N_PW_C) :: PWC2LIBXC=&
! & (/ NOXC, XC_LDA_C_PZ, XC_LDA_C_VWN, XC_GGA_C_LYP, XC_LDA_C_PW,&
! &    XC_LDA_C_WIGNER, XC_LDA_C_HL, XC_LDA_C_OB_PZ, XC_LDA_C_OB_PW,XC_LDA_C_GL,&
! &    XC_NOT_AVAILABLE, XC_NOT_AVAILABLE/)
!  integer,parameter,dimension(N_PW_GX) :: PWGX2LIBXC=&
! & (/ NOXC, XC_GGA_X_B88, XC_GGA_X_PW91, XC_GGA_X_PBE, XC_GGA_X_PBE_R,&
! &    XC_GGA_XC_HCTH_93, XC_GGA_X_OPTX, XC_MGGA_X_TPSS, XC_HYB_GGA_XC_PBEH,&
! &    XC_HYB_GGA_XC_B3LYP, XC_GGA_X_PBE_SOL, XC_GGA_X_WC/)
!  integer,parameter,dimension(N_PW_GC) :: PWGC2LIBXC=&
! & (/ NOXC, XC_GGA_C_P86, XC_GGA_C_PW91, XC_GGA_C_LYP, XC_GGA_C_PBE, XC_GGA_XC_HCTH_93,&
! &    XC_MGGA_C_TPSS, XC_HYB_GGA_XC_B3LYP, XC_GGA_C_PBE_SOL/)
! end module


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
!
! For LATER USE: ABINIT GGAs
!
    ! XC_GGA_X_PBE*XC_FACTOR+XC_GGA_C_PBE, & !ixc= 11
    ! XC_GGA_X_PBE*XC_FACTOR,              & !ixc= 12
    ! XC_GGA_XC_LB*XC_FACTOR,              & !ixc= 13
    ! XC_GGA_X_PBE_R*XC_FACTOR+XC_GGA_C_PBE,&!ixc= 14 (???)
    ! XC_GGA_X_RPBE*XC_FACTOR+XC_GGA_C_PBE, &!ixc= 15 (???)
    ! XC_GGA_XC_HCTH_93*XC_FACTOR,          &!ixc= 16 
    ! XC_GGA_XC_HCTH_120*XC_FACTOR,         &!ixc= 17
    ! XC_GGA_X_WC*XC_FACTOR,     &!ixc= 23 (???)
    ! XC_GGA_XC_HCTH_147*XC_FACTOR,         &!ixc= 26 
    ! XC_GGA_XC_HCTH_407*XC_FACTOR,         &!ixc= 27 
