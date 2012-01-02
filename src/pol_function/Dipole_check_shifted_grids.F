!
! Copyright (C) 2000-2012 C. Hogan, A. Marini and the YAMBO team 
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
subroutine Dipole_check_shifted_grids( Xk_shift, Xk, ifail, num_shifted_grid )
 !
 use pars,            ONLY: SP
 use stderr,          ONLY: intc
 use com,             ONLY: msg,warning
 use vec_operate,     ONLY: v_is_zero, iku_v_norm, c2a
 use R_lattice,       ONLY: bz_samp,q0_shift_norm
#if defined _RAS
 use ras_long,        ONLY: q_long
#endif
 implicit none
 !
 ! Find number of shifted grids
 !
 type(bz_samp), intent(in)    :: Xk_shift, Xk
 integer,       intent(inout) :: ifail,num_shifted_grid
 !
 ! Work Space
 !
 real(SP)         :: q_init(3), q_mod(3), qtemp(3), ca(3,3)
 real(SP)         :: a(3), b(3), c(3), x(3), y(3), z(3), q_long_tmp(3,3)
 integer          :: i_grid_k, iq, ik
 !
 if(ifail>0) return
 !
 if(mod(Xk_shift%nibz,Xk%nibz)/=0) then
   call msg('r',&
&       'Bad number of k-points in shifted DBs:'//intc(Xk%nibz)//intc(Xk_shift%nibz))
   ifail = ifail + 1
   return
 endif
 !
 num_shifted_grid = Xk_shift%nibz / Xk%nibz
 call msg('r','Shifted grids found: '//intc(num_shifted_grid))
  !
#if defined _RAS
 if(.not.(num_shifted_grid.ge.1.and.num_shifted_grid.le.4)) then
   call warning('Bad number of shifted grids (1/2/3).')
   ifail = ifail + 1
   return
 endif
#else
 if(num_shifted_grid/=3) then
   call warning('Shifted grids must be 3 and parallel to the three carthesian axis')
   ifail = ifail + 1
   return
 endif
#endif
 !
 ! Find the displacement vector for each grid
 !
 ! q_long_tmp(:,i)   is the shift vector of grid(i) in [iku]
 ! q_mod(i) is the modulus of the grid shift vector (i) in [a.u.]
 !
 q_mod(1:3) = 0.0_SP
 do iq = 1, num_shifted_grid
   !
   i_grid_k = (iq-1)*Xk%nibz
   q_init(:) = Xk_shift%pt(i_grid_k + 1,:) - Xk%pt(1,:) ! units of [iku]
   !
   ! This is a dirty fix to solve problems arising with small shifts 
   ! close to the limit of single precision
   !
   where(abs(q_init).lt.abs(Xk%pt(1,:)*1e-6)) q_init = 0.0_SP
   !
   ! Check grid is uniformly shifted
   !
   do ik = 2, Xk%nibz
     if(.not.v_is_zero( Xk_shift%pt(i_grid_k+ik,:) - &
&                        Xk%pt(ik,:) - q_init(:) )) then
       call warning('Shifts are not uniform for all {k}.')
       ifail = ifail + 1
       return
     endif
   enddo
   q_long_tmp(:,iq) = q_init(:)
   call c2a(v_in=q_long_tmp(:,iq), v_out=qtemp, mode="ki2c")
   call msg('r','Grid '//trim(intc(iq))//' has shift vector [c.c.]: ',qtemp )
   q_mod(iq) = iku_v_norm(q_long_tmp(:,iq))
   call msg('r','            and modulus [a.u.]: ',q_mod(iq))
   if (iq==num_shifted_grid) call msg('r','')
 enddo
 !
 ! Check the lengths are compatible (not abolutely required)
 ! CDH 270907 This is not clear...
 !
 q0_shift_norm = q_mod(1)  ! a.u. 
 q_mod(1:num_shifted_grid) = q_mod(1:num_shifted_grid) - q0_shift_norm
 if(.not.all( abs(q_mod).lt. 1.0e-5_SP )) then
   call msg('r','Shifts are not uniform for all {k}.')
   ifail = ifail + 1
 endif
 !
#if defined _RAS
 q_long = q_long_tmp
#endif
 !
end subroutine Dipole_check_shifted_grids
