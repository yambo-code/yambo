!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM CH
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
subroutine Dipole_check_shifted_grids( Xk_shift, Xk, iq, num_shifted_grid,q_mod,grid_order)
 !
 use pars,            ONLY:SP
 use stderr,          ONLY:intc
 use com,             ONLY:msg
 use vec_operate,     ONLY:v_is_zero, iku_v_norm, c2a
 use R_lattice,       ONLY:bz_samp,q0_shift_norm
 !
 implicit none
 !
 ! Find number of shifted grids
 !
 type(bz_samp), intent(in)    :: Xk_shift, Xk
 integer,       intent(in)    :: iq,num_shifted_grid
 integer,       intent(inout) :: grid_order(3)
 real(SP),      intent(inout) :: q_mod(3)
 !
 ! Work Space
 !
 real(SP)         :: q_init(3),qtemp(3),q_long_tmp(3)
 integer          :: ik
 !
 if(Xk_shift%nibz/=Xk%nibz) call error(' Bad number of k-points in shifted DBs '//trim(intc(Xk_shift%nibz)))
 !
 ! Find the displacement vector for each grid
 !
 ! q_long_tmp(:,i)   is the shift vector of grid(i) in [iku]
 ! q_mod(i) is the modulus of the grid shift vector (i) in [a.u.]
 !
 q_mod(iq) = 0._SP
 !
 q_init(:) = Xk_shift%pt(1,:) - Xk%pt(1,:) ! units of [iku]
 !
 ! This is a dirty fix to solve problems arising with small shifts 
 ! close to the limit of single precision
 !where(abs(q_init).lt.abs(Xk%pt(1,:)*1e-6)) q_init = 0._SP
 !
 ! Check grid is uniformly shifted
 !
 do ik = 2, Xk%nibz
   if( v_is_zero( Xk_shift%pt(ik,:)-Xk%pt(ik,:)-q_init(:) ) ) cycle
   call error(' Shifts are not uniform for all {k} in grid '//trim(intc(iq)))
   return
 enddo
 !
 q_long_tmp(:) = q_init(:)
 call c2a(v_in=q_long_tmp(:), v_out=qtemp, mode="ki2c")
 call msg('s','Grid '//trim(intc(iq))//' has shift vector [c.c.] ',qtemp )
 !
 if( qtemp(1)>=1.E-6 .and. qtemp(2)< 1.E-6 .and. qtemp(3)< 1.E-6) grid_order(iq)=1
 if( qtemp(1)< 1.E-6 .and. qtemp(2)>=1.E-6 .and. qtemp(3)< 1.E-6) grid_order(iq)=2
 if( qtemp(1)< 1.E-6 .and. qtemp(2)< 1.E-6 .and. qtemp(3)>=1.E-6) grid_order(iq)=3
 !
 q_mod(iq) = iku_v_norm(q_long_tmp(:))
 call msg('r','            and modulus [a.u.] ',q_mod(iq))
 if (iq==num_shifted_grid) call msg('r','')
 !
 ! Check the lengths are compatible (not abolutely required)
 ! CDH 270907 This is not clear...
 !
 q0_shift_norm = q_mod(iq)  ! a.u. 
 !
 if (iq>1) then
   if(.not. abs(q_mod(iq)-q_mod(iq-1))<1.E-5 ) &
   & call error(' Shifts are not uniform between grid'//trim(intc(iq))//' and previous grid')
 endif
 !
end subroutine Dipole_check_shifted_grids
