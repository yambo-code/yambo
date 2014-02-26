!
! Copyright (C) 2000-2014 A. Marini and the YAMBO team 
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
 use pars,           ONLY:SP,cZERO
 use drivers,        ONLY:l_col_cut
 use parallel_m,     ONLY:PAR_Xk_nbz,PAR_Xk_bz_index,PAR_DIPk_ibz_index,PAR_IND_Xk_bz,&
&                         PAR_IND_DIPk_ibz,PP_redux_wait,PAR_COM_Xk_bz_INDEX,&
&                         l_par_X_G_all_q
 use electrons,      ONLY:levels,n_sp_pol
 use D_lattice,      ONLY:nsym,i_time_rev,dl_sop,sop_inv
 use R_lattice,      ONLY:bz_samp,q0_def_norm,q_norm,bare_qpg
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
 integer                   :: ik,ik_bz,ic,iv,is,i_spin,ik_mem,ik_bz_mem,i_star
 real(SP)                  :: field_dir_rot(3)
 complex(SP)               :: dipole(3)
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
 call X_alloc('DIP_q_dot_iR',(/X%ib(2),Xen%nbm,PAR_Xk_nbz/))
 DIP_q_dot_iR = (0.0_SP,0.0_SP)
 !
 do ik=1,Xk%nibz
   !
   do iv = X%ib(1), Xen%nbm
     !
     do ic = Xen%nbf+1, X%ib(2)
       !
       do i_spin=1,n_sp_pol
         !
         dipole = cZERO
         !
         if (PAR_IND_DIPk_ibz%element_1D(ik)) then
           !      
           ik_mem = PAR_DIPk_ibz_index(ik)
           dipole = DIP_iR(:,ic,iv,ik_mem,i_spin)
           !
         endif
         !
         ! Here I use PAR_COM_Xk_bz_INDEX as it connects two CPUS belonging to different
         ! k_bz points but with the same k_ibz. Those two CPUS belong to the q communicator
         !
         if (l_par_X_G_all_q) call PP_redux_wait(dipole,COMM=PAR_COM_Xk_bz_INDEX%COMM)
         !
         do i_star=1,Xk%nstar(ik)
           !
           is              = sop_inv(Xk%star(ik,i_star))
           ik_bz           = i_star
           if (ik>1) ik_bz = sum(Xk%nstar(1:ik-1))+i_star
           !
           field_dir_rot = matmul( dl_sop(:,:,is), field_dir )
           !
           if (PAR_IND_Xk_bz%element_1D(ik_bz)) then
             !
             ik_bz_mem=PAR_Xk_bz_index(ik_bz)
             !
             if ( is<= nsym/(i_time_rev+1) ) then
               DIP_q_dot_iR(ic,iv,ik_bz_mem,i_spin) = dot_product( field_dir_rot, dipole(:) )
             else
               DIP_q_dot_iR(ic,iv,ik_bz_mem,i_spin) = dot_product( dipole(:), field_dir_rot ) 
             endif
             !
             if (iv==ic) DIP_q_dot_iR(ic,iv,ik_bz_mem,:) = (1.0_SP,0.0_SP)
             ! 
           endif
           !
         enddo
         !
       enddo
       !
     enddo
     !
   enddo
   !
 enddo
 !
end subroutine
