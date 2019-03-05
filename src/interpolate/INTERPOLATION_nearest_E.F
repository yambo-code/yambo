!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, DS
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
subroutine INTERPOLATION_nearest_E(Energy,E_reference,N_qp,E_diff_tresh,i_qp_ref)
 !
 use pars,           ONLY:SP
 use vec_operate,    ONLY:sort
 !
 implicit none
 !
 real(SP),           intent(in) :: Energy,E_reference(N_qp)
 integer,            intent(in) :: N_qp
 real(SP),           intent(out):: E_diff_tresh
 integer,            intent(out):: i_qp_ref
 !
 ! Work Space
 !
 integer  :: E_indx(N_qp),i_qp
 real(SP) :: E_tmp(N_qp),E_diff
 !
 E_tmp=E_reference
 !
 call sort(arrin=E_tmp,indx=E_indx)
 !
 E_diff_tresh=1000._SP
 !
 do i_qp=1,N_qp
   !
   E_diff=abs( Energy-E_tmp(i_qp) )
   !
   if (E_diff<E_diff_tresh) then
     i_qp_ref=E_indx(i_qp)
     E_diff_tresh=E_diff
   endif
   !
 enddo
 !
end subroutine INTERPOLATION_nearest_E
