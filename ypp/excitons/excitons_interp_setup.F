!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): CA
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
subroutine excitons_interp_setup(k,Xk,en,Xen,q_coarse,q_fine,EXC_interp_en)
 !
 ! This subroutine map exciton dispersion from q_coarse to q_fine
 ! and generate the a double-grid q-map for the wave-function interpolation
 !
 use pars,          ONLY:SP
 use R_lattice,     ONLY:bz_samp
 use interpolate,   ONLY:INTERPOLATION_BZ,INTERP_obj,INTERPOLATION_coefficients
 use electrons,     ONLY:levels,n_sp_pol
 use com,           ONLY:msg
 use units,         ONLY:HA2EV
 use BS_solvers,    ONLY:BSS_n_eig
 use YPPm,          ONLY:BS_E,EXCITONS_user_indexes,EXCITONS_n_user_states,INTERP_grid
 !
#include<memory.h> 
 !
 type(bz_samp) ::Xk,k
 type(bz_samp) ::q_coarse,q_fine 
 type(levels)  ::Xen,en
 real(SP), intent(out) :: EXC_interp_en(EXCITONS_n_user_states,q_fine%nibz)
 !
 ! Work space
 !
 integer :: iq,ID_INTERP,i_c,i_l
 real(SP), allocatable    :: BS_all_E(:,:)
 !
 YAMBO_ALLOC(BS_all_E,(EXCITONS_n_user_states,q_coarse%nibz))
 !
 call section('*','Excitons Interpolation')
 !
 call excitons_read(k,Xk,en,Xen,1,"clean")
 !
 ! # Read all eigenvalues and eigenvectors
 !
 do iq=1,q_coarse%nibz
   call msg('s','Reading excitons at @ Q-index #',iq)
   call excitons_read(k,Xk,en,Xen,iq,"check")
   call excitons_read(k,Xk,en,Xen,iq,"eigenvalues")
   do i_c=1,EXCITONS_n_user_states
     i_l=EXCITONS_user_indexes(i_c)
     BS_all_E(i_c,iq)=BS_E(i_l)
   enddo
   call excitons_read(k,Xk,en,Xen,iq,"clean")
 enddo
 !
 ! # Fourier interpolation of exciton dispersion
 !
 call INTERPOLATION_BZ_setup(q_coarse)
 ID_INTERP=1
 INTERP_obj(ID_INTERP)%what="excitons"
 call INTERPOLATION_coefficients(R1D=BS_all_E,k=q_coarse,NK=q_coarse%nibz,ID=ID_INTERP,ID_obj=ID_INTERP)
 !
 ! Interpolate on a grid 
 !
 call INTERPOLATION_BZ(K=q_fine,NK=q_fine%nibz,R1D=EXC_interp_en,ID=ID_INTERP)
 !
end subroutine
