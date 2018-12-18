!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): MG
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
 use xc_f90_types_m
 use libxc_funcs_m
 use xc_f90_lib_m,   ONLY:xc_f90_func_init, xc_f90_func_end, &
&                         xc_f90_info_kind, XC_UNPOLARIZED
 use xc_functionals, ONLY:XC_FACTOR, XC_NOT_AVAILABLE, NOXC, XC_libxcID 
 !
 implicit none
 !
 integer, parameter :: N_Abinit_XC = 28,N_PW_X = 12, N_PW_C = 13, N_PW_GX = 19, N_PW_GC = 14
 !
 !  Map for ABINIT/ETSF_IO
 !
 integer, parameter, dimension(N_Abinit_XC) :: XC_A2Y =&
& (/XC_LDA_XC_TETER93*XC_FACTOR,                  & !ixc= 1
    XC_LDA_X*XC_FACTOR+XC_LDA_C_PZ,               & !ixc= 2 
    XC_NOT_AVAILABLE,                             & !ixc= 3 not clear what it is
    XC_LDA_X*XC_FACTOR+XC_LDA_C_WIGNER,           & !ixc= 4 
    XC_LDA_X*XC_FACTOR+XC_LDA_C_HL,               & !ixc= 5 
    XC_LDA_X*XC_FACTOR+XC_LDA_C_XALPHA,           & !ixc= 6
    XC_LDA_X*XC_FACTOR+XC_LDA_C_PW,               & !ixc= 7
    XC_LDA_X*XC_FACTOR,                           & !ixc= 8 
    XC_LDA_X*XC_FACTOR+XC_LDA_C_PW_RPA,           & !ixc= 9 
    XC_NOT_AVAILABLE,                             & !ixc= 10 not defined
    XC_GGA_X_PBE*XC_FACTOR+XC_GGA_C_PBE,          & !ixc= 11
    XC_GGA_X_PBE*XC_FACTOR,                       & !ixc= 12
    XC_NOT_AVAILABLE,                             & !ixc= 13 does not seem to be the same as XC_GGA_X_LB*XC_FACTOR,
    XC_GGA_X_PBE_R*XC_FACTOR+XC_GGA_C_PBE,        & !ixc= 14 
    XC_GGA_X_RPBE*XC_FACTOR+XC_GGA_C_PBE,         & !ixc= 15 
    XC_GGA_XC_HCTH_93*XC_FACTOR,                  & !ixc= 16 
    XC_GGA_XC_HCTH_120*XC_FACTOR,                 & !ixc= 17
    XC_NOT_AVAILABLE,                             & !ixc= 18
    XC_NOT_AVAILABLE,                             & !ixc= 19
    XC_NOT_AVAILABLE,                             & !ixc= 20
    XC_NOT_AVAILABLE,                             & !ixc= 21
    XC_NOT_AVAILABLE,                             & !ixc= 22
    XC_NOT_AVAILABLE,                             & !ixc= 23 !does not seem to be the same as XC_GGA_X_WC*XC_FACTOR
    XC_NOT_AVAILABLE,                             & !ixc= 24 !does not seem to be the same as XC_GGA_X_C09X*XC_FACTOR
    XC_NOT_AVAILABLE,                             & !ixc= 25
    XC_GGA_XC_HCTH_147*XC_FACTOR,                 & !ixc= 26 
    XC_GGA_XC_HCTH_407*XC_FACTOR,                 & !ixc= 27 
    XC_NOT_AVAILABLE/)                              !ixc= 28
 !
 ! Map for PWSCF (note that some of the acronims were used in past versions of pw.x 
 ! 
 character(len=6),parameter,dimension(N_PW_X) :: PW_X=&  
& (/'NOX ', 'SLA ','LDA ', 'SL1 ', 'RXC ', 'OEP ', 'HF  ', 'PB0X', 'B3LP', 'KZK ', 'PZ  ','PW  '/)
 character(len=6),parameter,dimension(N_PW_C) :: PW_C=&  
& (/ 'NOC ', 'PZ  ','LDA ', 'VWN ', 'LYP ', 'PW  ', 'WIG ', 'HL  ', 'OBZ ', &
&    'OBW ', 'GL  ' , 'B3LP', 'KZK ' /)
 character(len=8),parameter,dimension(N_PW_GX) :: PW_GX=& 
& (/ 'NOGX  ', 'B88   ', 'BLYP  ', 'GGX   ', 'PW91  ', 'PBX   ', 'PBE   ', 'RPB   ', 'HCTH  ', 'OPTX  ',&
&    'META  ', 'PB0X  ', 'PBE0  ','PBESOL', 'B3LP  ', 'B3LYP ', 'PSX   ', 'WCX   ', 'HSE   '/)
 character(len=8),parameter,dimension(N_PW_GC) :: PW_GC=&
& (/ 'NOGC  ', 'P86   ', 'GGC   ', 'PW91  ', 'BLYP  ', 'PBC   ', 'PBE   ','PBE0  ','PBESOL','HCTH  ', 'META  ',&
&    'B3LP  ', 'B3LYP ', 'PSC   '/)
 ! 
 integer,parameter,dimension(N_PW_X) :: PWX2LIBXC=&
& (/ NOXC, XC_LDA_X*XC_FACTOR, XC_LDA_X*XC_FACTOR, XC_LDA_X*XC_FACTOR+XC_LDA_C_XALPHA, XC_NOT_AVAILABLE, &
&    XC_NOT_AVAILABLE, NOXC, NOXC, XC_NOT_AVAILABLE,XC_NOT_AVAILABLE,XC_LDA_X*XC_FACTOR,XC_LDA_X*XC_FACTOR/)
 integer,parameter,dimension(N_PW_C) :: PWC2LIBXC=&
& (/ NOXC, XC_LDA_C_PZ, XC_LDA_C_PZ, XC_LDA_C_VWN, XC_GGA_C_LYP, XC_LDA_C_PW,&
&    XC_LDA_C_WIGNER, XC_LDA_C_HL, XC_LDA_C_OB_PZ, XC_LDA_C_OB_PW,XC_LDA_C_GL,&
&    NOXC, XC_NOT_AVAILABLE/)
 integer,parameter,dimension(N_PW_GX) :: PWGX2LIBXC=&
& (/ NOXC, XC_GGA_X_B88, XC_GGA_X_B88, XC_GGA_X_PW91, XC_GGA_X_PW91, XC_GGA_X_PBE, XC_GGA_X_PBE, XC_GGA_X_PBE_R,&
&    XC_GGA_XC_HCTH_93, XC_GGA_X_OPTX, XC_MGGA_X_TPSS, XC_HYB_GGA_XC_PBEH, XC_HYB_GGA_XC_PBEH,XC_GGA_X_PBE_SOL, &
&    XC_HYB_GGA_XC_B3LYP, XC_HYB_GGA_XC_B3LYP, XC_GGA_X_PBE_SOL, XC_GGA_X_WC, XC_HYB_GGA_XC_HSE06/)
 integer,parameter,dimension(N_PW_GC) :: PWGC2LIBXC=&
& (/ NOXC, XC_GGA_C_P86, XC_GGA_C_PW91, XC_GGA_C_PW91, XC_GGA_C_LYP, XC_GGA_C_PBE, XC_GGA_C_PBE, NOXC,&
&    XC_GGA_C_PBE_SOL,NOXC, XC_MGGA_C_TPSS, NOXC, NOXC, XC_GGA_C_PBE_SOL/)
 !
 ! Note that in libxc some functional as b3lyp, pbe0 and hcth are defined as one xc functional 
 ! e.g. exchange and correlation are not separable!!!
 ! Note as well that in libxc the GGA contains the whole functional, not only the corrections as in
 ! 
 contains
   !
   integer function XC_yamboID(dft_program,abinit_func,pw_func)
   character(len=6), intent(in)   :: dft_program
   integer, intent(in),optional   :: abinit_func
   character(256), intent(in),optional :: pw_func
   !
!   LOGICAL, EXTERNAL             :: matches
   integer ii,XC_yamboID_ 
   !
   XC_yamboID = XC_NOT_AVAILABLE 
   select case(dft_program)
     case('abinit')
       if (.not.present(abinit_func)) return
       if (abinit_func.lt.0) then
         XC_yamboID = -abinit_func ! In this case abinit uses libxc, thus the same convention as in Yambo (except for the sign) 
       else if (abinit_func.gt.0) then
         XC_yamboID = XC_A2Y(abinit_func) ! Standard abinit XC convention, use the map
       else
         XC_yamboID = NOXC
       end if
     case('pwscf_')
       !
       if (.not.present(pw_func)) return
       !
       do ii=1,N_PW_GX ! I scan before the GGAs
        if (matches(PW_GX(ii),pw_func)) XC_yamboID = PWGX2LIBXC(ii)*XC_FACTOR
       end do
       if (XC_ComponentsAreSeparate(XC_yamboID)) then
         XC_yamboID_ = 0
         do ii=1,N_PW_GC
           if (matches(PW_GC(ii),pw_func)) XC_yamboID_ = PWGC2LIBXC(ii)
         end do
         XC_yamboID = XC_yamboID + XC_yamboID_
       endif
       if (XC_yamboID == XC_NOT_AVAILABLE .or. XC_yamboID == NOXC) then ! Only if no GGAs or no 
                                                                        ! XC are found I scan LDA (this avoids 'double counting')
         do ii=1,N_PW_X
           if (matches(PW_X(ii),pw_func)) XC_yamboID = PWX2LIBXC(ii)
         end do
         do ii=1,N_PW_C
           if (matches(PW_C(ii),pw_func)) XC_yamboID = XC_yamboID + PWC2LIBXC(ii)
         end do
       end if
     case DEFAULT 
       return
     end select
   end function XC_yamboID
   !
   integer function XC_yamboID2kind(yamboID)
     integer, intent(in) :: yamboID 
     !
     type(xc_f90_pointer_t)  p, info
     integer ifunc, func
     !
     if (yamboID == XC_NOT_AVAILABLE) return
     XC_yamboID2kind = 0
     do ifunc = 1,2
       func = XC_libxcID(yamboID,ifunc)
       if (func == NOXC) cycle
       call xc_f90_func_init(p, info, func, XC_UNPOLARIZED) ! I put  XC_UNPOLARIZED, I think it is not relevant here
       XC_yamboID2kind = XC_yamboID2kind + (xc_f90_info_kind(info) + 1) ! In yambo the type are shifted by +1 wrt libxc 
       call xc_f90_func_end(p)
     end do
     !
   end function XC_yamboID2kind
   !
   logical function XC_ComponentsAreSeparate(yamboID)
     integer, intent(in) :: yamboID
     integer ID
     logical lcheck
     !
     ID=yamboID/XC_FACTOR
     lcheck = (ID==XC_GGA_XC_HCTH_93).or.&
             &(ID==XC_HYB_GGA_XC_PBEH).or.&
             &(ID==XC_HYB_GGA_XC_B3LYP).or.&
             &(ID==XC_HYB_GGA_XC_HSE06)
     XC_ComponentsAreSeparate=.not.lcheck
   end function XC_ComponentsAreSeparate
   !
   !-----------------------------------------------------------------------
   FUNCTION matches( string1, string2 )  
   !-----------------------------------------------------------------------
     !
     ! ... .TRUE. if string1 is contained in string2, .FALSE. otherwise
     !
     IMPLICIT NONE
     !
     CHARACTER (LEN=*), INTENT(IN) :: string1, string2
     LOGICAL                       :: matches
     INTEGER                       :: len1, len2, l  
     !
     len1 = LEN_TRIM( string1 )  
     len2 = LEN_TRIM( string2 )  
     !
     DO l = 1, ( len2 - len1 + 1 )  
       !   
       IF ( string1(1:len1) == string2(l:(l+len1-1)) ) THEN  
         !
         matches = .TRUE.  
         !
         RETURN  
         !
       END IF
       !
     END DO
     !
     matches = .FALSE.
     ! 
     RETURN
     !
   END FUNCTION matches
   !
 end module mod_xc2y
