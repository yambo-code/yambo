!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM DV
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
subroutine exciton_spin(Xk,Xen,BS_R_right,BS_E,BS_E_degs)
 !
 ! Excitonc Spin
 !
 use pars,          ONLY:SP,rZERO
 use BS_solvers,    ONLY:BSS_n_eig
 use R_lattice,     ONLY:bz_samp
 use electrons,     ONLY:levels,n_sp_pol,n_spinor
 use YPP,           ONLY:excitons_sort_and_report,excitons_degeneracy_average,EXCITONS_n_user_states
 !
 implicit none
 !
 type(bz_samp) :: Xk
 type(levels)  :: Xen
 integer       :: BS_E_degs(BSS_n_eig)
 complex(SP)   :: BS_R_right(BSS_n_eig),BS_E(BSS_n_eig)
 !
 ! Work Space
 ! 
 integer       :: i_c
 real(SP)      :: Triplet(BSS_n_eig,3),&  ! Triplet (-1,0,1) projections
&                 Singlet(BSS_n_eig),&    ! Singlet projection
&                 S_eh(BSS_n_eig)         ! e/h S (in pure spin states it is 1,0)
 !
 call section('+','Spin properties')
 !                 ===============
 Singlet = rZERO
 Triplet = rZERO
 S_eh    = rZERO
 !
 if (n_sp_pol==1.and.n_spinor==1) return
 !
 if (n_sp_pol==2) call excitons_collinear(Singlet,Triplet,S_eh)
 !if (n_spinor==2) call excitons_non_collinear(Xk,Singlet,Triplet,S_eh)
 !
 !
 call section('=','Degeneracy average')
 !                 ==================
 call excitons_degeneracy_average( EXCITONS_n_user_states, BS_E_degs, R=Singlet, VERB=.TRUE. )
 call excitons_degeneracy_average( EXCITONS_n_user_states, BS_E_degs, R=S_eh, VERB=.FALSE. )
 do i_c=1,3
   call excitons_degeneracy_average( EXCITONS_n_user_states, BS_E_degs, R=Triplet(:,i_c), VERB=.FALSE. )
 enddo
 !
 ! Sorting & reporting'
 !=====================
 !
 call excitons_sort_and_report(Xen,BS_R_right,BS_E,Singlet=Singlet,Triplet=Triplet,S_eh=S_eh)
 !
end subroutine exciton_spin   
