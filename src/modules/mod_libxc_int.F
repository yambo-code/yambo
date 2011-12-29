!
! Copyright (C) 2000-2012 M. Gruning and the YAMBO team
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
module libxc_int
 !
 ! Interface between libxc and yambo
 ! 
 use xc_functionals, ONLY:  N_Available_LDA
 use xc_f90_lib_m
 use xc_f90_types_m
 use libxc_funcs_m
 implicit none
 !
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
 NOXC = 0,               &
 XC_LDA_C_KP = 99         !This is not defined within libxc
 !
 ! This matrix of integers maps XC numbering of Yambo for LDA into libxc
 ! Note that though with the same name the parameters (defined in libxc_funcs_m)
 ! correspond in principle to a different number than in mod_xc_functionals 
 !
 integer, parameter, dimension(N_Available_LDA) :: LDA_Y2LIBXC =&
& (/XC_LDA_X,                            & !1
    XC_LDA_C_WIGNER,                     & !2 
    XC_LDA_C_RPA,                        & !3 
    XC_LDA_C_HL,                         & !4
    XC_LDA_C_GL,                         & !5
    XC_LDA_C_XALPHA,                     & !6
    XC_LDA_C_VWN,                        & !7
    XC_LDA_C_PZ,                         & !8
    XC_LDA_C_OB_PZ,                      & !9
    XC_LDA_C_PW,                         & !10
    XC_LDA_C_OB_PW,                      & !11
    XC_GGA_C_LYP,                        & !12
    XC_LDA_C_2D_AMGB,                    & !13
    XC_LDA_C_KP,                         & !14
    XC_LDA_XC_TETER93,                   & !15  
    XC_LDA_C_VWN_RPA,                    & !16 
    XC_LDA_C_PZ_MOD,                     & !17 
    XC_LDA_C_PW_MOD,                     & !18 
    XC_LDA_C_vBH/)
 ! 
 contains
   !
   integer function XC_y2libxc(Yambo_Func,Yambo_kind)
     integer Yambo_Func,Yambo_kind, Tmp_Func, libxc_kind
     !
     ! This function map the information from Yambo_Func & Yambo_kind 
     ! in one single number = [X] * 1000 + [C] following the libxc conventions
     ! Status(22/07/10): Only LDA supported   
     !
     XC_y2libxc = 0
     if (Yambo_kind == NOXC) return
     Tmp_Func = LDA_Y2LIBXC(Yambo_Func) 
     libxc_kind = Yambo_kind -1 ! Note that the kind in Yambo are shifted by +1 wrt libxc
     if (Tmp_Func==XC_LDA_XC_TETER93) libxc_kind = XC_CORRELATION ! since this functional contains already X
     if ((libxc_kind) == XC_EXCHANGE.or.(libxc_kind) == XC_EXCHANGE_CORRELATION)&
          &XC_y2libxc = XC_LDA_X*1000  
     if (libxc_kind == XC_CORRELATION.or.libxc_kind == XC_EXCHANGE_CORRELATION)&
          &XC_y2libxc =XC_y2libxc + Tmp_Func    
   end function XC_y2libxc   
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
end module libxc_int
