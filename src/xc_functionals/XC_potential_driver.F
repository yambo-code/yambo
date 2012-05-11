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
subroutine XC_potential_driver(en,Xk,KIND,FUNCTIONAL,ORDER)
 !
 ! Wrapper for calculation of the XC potential. If available libxc,
 ! will be used instead of internal routines
 !
 use pars,          ONLY:SP,DP
 use R_lattice,     ONLY:bz_samp
 use electrons,     ONLY:levels
 use com,           ONLY:msg,warning
 use libxc_funcs_m, ONLY: XC_LDA_X, XC_LDA_C_PW
 use xc_functionals,ONLY:XC_EXCHANGE_CORRELATION,xc_string,&
&                        XC_NOT_AVAILABLE,XC_FACTOR
 implicit none
 type(levels), intent(in)  ::en
 type(bz_samp), intent(in) ::Xk
 integer, intent(inout)    :: KIND,FUNCTIONAL
 integer, intent(in)       :: ORDER
 !
 ! COM
 !
 if (KIND>0.and.FUNCTIONAL>0) then
   call msg('rsn','[xc] Functional ',xc_string(FUNCTIONAL))
 else if (FUNCTIONAL==XC_NOT_AVAILABLE) then
   KIND=XC_EXCHANGE_CORRELATION
   FUNCTIONAL=XC_LDA_X*XC_FACTOR+XC_LDA_C_PW
   call msg('rsn','[xc] Functional unknown. Used ',xc_string(FUNCTIONAL))
 endif
 !
 ! Maybe also (spin)density should be calculated in the wrapper, since it does 
 ! not use anything of libxc.   
 !
 call msg('rsn','[xc] LIBXC used to calculate xc functional ')
 !
 call XC_libxc_driver(en,Xk,KIND,FUNCTIONAL,ORDER)
 !
end subroutine XC_potential_driver


