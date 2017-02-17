!
! Copyright (C) 2000-2013 A. Marini and the YAMBO team 
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
subroutine Dipole_dip_dot_field(Xen,Xk,X,field_dir)
 !
 use pars,           ONLY:SP
 use drivers,        ONLY:l_optics
 use drivers,        ONLY:l_col_cut
 use electrons,      ONLY:levels,n_sp_pol
 use D_lattice,      ONLY:nsym,i_time_rev,dl_sop,sop_inv
 use R_lattice,      ONLY:bz_samp,q0_def_norm,q_norm,bare_qpg
 use fields,         ONLY:global_gauge
 use X_m,            ONLY:DIP_q_dot_iR,X_alloc,X_t,DIP_iR
 use vec_operate,    ONLY:v_norm
 !
 implicit none
 !
 type(bz_samp), intent(inout) :: Xk
 type(levels),  intent(inout) :: Xen
 type(X_t),     intent(inout) :: X
 real(SP),      intent(inout) :: field_dir(3)
 !
 ! Work Space
 !
 integer                   :: ik,i1,ic,iv,is,i_spin
 real(SP)                  :: field_dir_rot(3)
 !
 ! The field direction and the gamma-point norm must be renormalized here in case the
 ! oscillator strengths have been calculated using the shifted grids.
 ! In this case q0_def_norm is not the default one but corresponds
 ! to the norm of the grid shift.
 !
 field_dir=field_dir*q0_def_norm/v_norm(field_dir)
 !
 q_norm(1)=q0_def_norm
 if (.not.l_col_cut) bare_qpg(1,1)=q0_def_norm
 !
 ! Calculate the q-dependent oscillators
 !
 call X_alloc('DIP_q_dot_iR',(/X%ib(2),Xen%nbm,Xk%nbz/))
 DIP_q_dot_iR = (0.0_SP,0.0_SP)
 !
 do i1 = 1,Xk%nbz
   !
   ik = Xk%sstar(i1,1)
   is = sop_inv(Xk%sstar(i1,2))
   field_dir_rot = matmul( dl_sop(:,:,is), field_dir )
   !
   do iv = X%ib(1), Xen%nbm
     !
     do ic = Xen%nbf+1, X%ib(2)
       !
        i_spin=1
       ! 
         !
         ! DIP_iq_dot_r = i q . < v k | r | c k >
         !
         if ( is<= nsym/(i_time_rev+1) ) DIP_q_dot_iR(ic,iv,i1,i_spin) = dot_product( field_dir_rot, DIP_iR(:,ic,iv,ik,i_spin)  )
         if ( is > nsym/(i_time_rev+1) ) DIP_q_dot_iR(ic,iv,i1,i_spin) = dot_product( DIP_iR(:,ic,iv,ik,i_spin), field_dir_rot ) 
         !
       !
     enddo
     DIP_q_dot_iR(iv,iv,i1,:) = (1.0_SP,0.0_SP)
   enddo
   !
 enddo
 !
end subroutine
