!
!        Copyright (C) 2000-2014 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
 use X_m,            ONLY:DIP_q_dot_iR,X_alloc,X_t,DIP_iR,Dipole_bands_ordered
 use vec_operate,    ONLY:v_norm
#if defined _KERR
 use drivers,        ONLY:l_kerr
 use KERR,           ONLY:DIP_q_dot_iR_kerr,KERR_alloc,KERR_alloc,Dip_P_symm
 use fields,         ONLY:global_gauge
 use X_m,            ONLY:DIP_P
#endif
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
 integer                   :: ik,ik_bz,ic,iv,is,i_spin,ik_mem,ik_bz_mem,i_star,iv_max,ic_min,&
&                             nb_alloc,i1
 real(SP)                  :: field_dir_rot(3)
 complex(SP)               :: dipole(3)
 logical                   :: t_rev
#if defined _KERR
 real(SP)                  :: dipole_dir(3),dipole_dir_rot(3)
#endif
 !
 ! The field direction and the gamma-point norm must be renormalized here in case the
 ! oscillator strengths have been calculated using the shifted grids.
 ! In this case q0_def_norm is not the default one but corresponds
 ! to the norm of the grid shift.
 !
 field_dir=field_dir*q0_def_norm/v_norm(field_dir)
 !
#if defined _KERR
 if(trim(global_gauge)=='velocity') field_dir=field_dir/q0_def_norm
 if(l_kerr) then ! if field=(0,1,0) dipole=(1,0,0) OK  p_x=\aplha_xy E_y
   if(field_dir(2).ne.0) then
     dipole_dir(1)=-1.
     dipole_dir(2)=-dipole_dir(1)*(field_dir(1)/field_dir(2))
     dipole_dir(3)= 0.
   else
     dipole_dir(3)= 0.
     dipole_dir(2)= 1.
     dipole_dir(1)=-dipole_dir(2)*(field_dir(2)/field_dir(1))
   endif
   dipole_dir=dipole_dir*v_norm(field_dir)/v_norm(dipole_dir)
 endif
#endif
 !
 q_norm(1)=q0_def_norm
 if (.not.l_col_cut) bare_qpg(1,1)=q0_def_norm
 !
 ! Calculate the q-dependent oscillators
 !
 if (Dipole_bands_ordered) then
   iv_max=Xen%nbm
   ic_min=Xen%nbf+1
   nb_alloc=Xen%nbm
 else
   iv_max=X%ib(2)
   ic_min=X%ib(1)
   nb_alloc=X%ib(2)
 endif
 !
 call X_alloc('DIP_q_dot_iR',(/X%ib(2),nb_alloc,PAR_Xk_nbz/))
 DIP_q_dot_iR = (0.0_SP,0.0_SP)
 !
#if defined _KERR
 if(trim(global_gauge)=='velocity') then
   i1=1
   if(l_kerr) i1=2
   call KERR_alloc('DIP_P_symm',(/i1,X%ib(2),nb_alloc,PAR_Xk_nbz/))
   DIP_P_symm = 0.0_SP
 endif
 !
 if(l_kerr) then
   call KERR_alloc('DIP_q_dot_iR',(/X%ib(2),nb_alloc,PAR_Xk_nbz/))
   DIP_q_dot_iR_kerr = (0.0_SP,0.0_SP)
 endif
#endif
 !
 do ik=1,Xk%nibz
   !
   do iv=X%ib(1),iv_max 
     !
     do ic=ic_min,X%ib(2)
       !
       do i_spin=1,n_sp_pol
         !
         dipole = cZERO
         !
         if (PAR_IND_DIPk_ibz%element_1D(ik)) then
           !      
           ik_mem = PAR_DIPk_ibz_index(ik)
#if defined _KERR
           if(trim(global_gauge)=='velocity') dipole = DIP_P(:,ic,iv,ik_mem,i_spin)
           if(trim(global_gauge)=='length'  ) dipole = DIP_iR(:,ic,iv,ik_mem,i_spin)
#else
           dipole = DIP_iR(:,ic,iv,ik_mem,i_spin)
#endif
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
           if ( is<= nsym/(i_time_rev+1) ) t_rev=.false.
           if ( is > nsym/(i_time_rev+1) ) t_rev=.true.
           !
           field_dir_rot = matmul( dl_sop(:,:,is), field_dir )
#if defined _KERR
           if (l_kerr) dipole_dir_rot = matmul( dl_sop(:,:,is), dipole_dir )
#endif
           !
           if (PAR_IND_Xk_bz%element_1D(ik_bz)) then
             !
             ik_bz_mem=PAR_Xk_bz_index(ik_bz)
             !
#if defined _KERR
             if(trim(global_gauge)=='velocity') then
               if(.not.t_rev) DIP_P_symm(1,ic,iv,ik_bz_mem,i_spin) = dot_product( field_dir_rot, dipole )
               if(     t_rev) DIP_P_symm(1,ic,iv,ik_bz_mem,i_spin) = dot_product( dipole, field_dir_rot )
               if(l_kerr) then
                 if(.not.t_rev) DIP_P_symm(2,ic,iv,ik_bz_mem,i_spin) = dot_product( dipole_dir_rot, dipole )
                 if(     t_rev) DIP_P_symm(2,ic,iv,ik_bz_mem,i_spin) = dot_product( dipole, dipole_dir_rot )
               endif
             endif
             if(trim(global_gauge)=='length') then
               if(.not.t_rev) DIP_q_dot_iR(ic,iv,ik_bz_mem,i_spin) = dot_product( field_dir_rot, dipole )
               if(     t_rev) DIP_q_dot_iR(ic,iv,ik_bz_mem,i_spin) = dot_product( dipole, field_dir_rot ) 
               if(l_kerr) then
                 if(.not.t_rev) DIP_q_dot_iR_kerr(ic,iv,ik_bz_mem,i_spin) = dot_product( dipole_dir_rot, dipole )
                 if(     t_rev) DIP_q_dot_iR_kerr(ic,iv,ik_bz_mem,i_spin) = dot_product( dipole, dipole_dir_rot )
               endif
             endif
#else
             if(.not.t_rev) DIP_q_dot_iR(ic,iv,ik_bz_mem,i_spin) = dot_product( field_dir_rot, dipole )
             if(     t_rev) DIP_q_dot_iR(ic,iv,ik_bz_mem,i_spin) = dot_product( dipole, field_dir_rot ) 
#endif
             !
#if defined _KERR
             if(trim(global_gauge)=='velocity') then
               if (iv==ic)            DIP_P_symm(1,ic,iv,ik_bz_mem,:) = (1.0_SP,0.0_SP)
               if (iv==ic.and.l_kerr) DIP_P_symm(2,ic,iv,ik_bz_mem,:) = (1.0_SP,0.0_SP)
             endif
             if(trim(global_gauge)=='length') then
               if (iv==ic)            DIP_q_dot_iR(ic,iv,ik_bz_mem,:)      = (1.0_SP,0.0_SP)
               if (iv==ic.and.l_kerr) DIP_q_dot_iR_kerr(ic,iv,ik_bz_mem,:) = (1.0_SP,0.0_SP)
             endif
#else
             if (iv==ic) DIP_q_dot_iR(ic,iv,ik_bz_mem,:) = (1.0_SP,0.0_SP)
#endif
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
