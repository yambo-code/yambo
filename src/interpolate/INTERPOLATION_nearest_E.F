!
!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine INTERPOLATION_nearest_E(Energy,qp,E_diff_tresh,i_qp_ref,i_sp_pol)
 !
 use pars,           ONLY:SP
 use QP_m,           ONLY:QP_t
 use vec_operate,    ONLY:sort
 use electrons,      ONLY:spin
 !
 implicit none
 !
 real(SP),           intent(in) :: Energy
 type(QP_t),         intent(in) :: qp
 integer,            intent(in) :: i_sp_pol
 real(SP),           intent(out):: E_diff_tresh
 integer,            intent(out):: i_qp_ref
 !
 ! Work Space
 !
 integer  :: E_indx(qp%n_states),i_qp
 real(SP) :: E_tmp(qp%n_states),E_diff
 !
 E_tmp=qp%E_bare
 !
 call sort(arrin=E_tmp,indx=E_indx)
 !
 E_diff_tresh=1000._SP
 !
 do i_qp=1,qp%n_states
   !
   if (spin(qp%table(E_indx(i_qp),:))/=i_sp_pol) cycle
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
