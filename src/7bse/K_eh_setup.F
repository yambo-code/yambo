!
! Copyright (C) 2000-2005 A. Marini and the SELF team 
!         http://www.fisica.uniroma2.it/~self
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
subroutine K_eh_setup(iq,Ken,Xk)
 !
 ! Out: 
 !
 !   BS_K_dim
 !   BS_eh_table(BS_K_dim,4)
 !   BS_eh_E(BS_K_dim))
 !   O_v_state(Xk%nbz,maxval(O_n_v_states),n_sp_pol)
 !   O_c_state(Xk%nbz,maxval(O_n_c_states),n_sp_pol)
 !
 use pars,         ONLY:SP,IP
 use memory_m,     ONLY:mem_est
 use com,          ONLY:error
 use BS,           ONLY:BS_K_dim,O_v_state,O_c_state,BS_res_K_corr,&
&                       O_n_v_states,O_n_c_states,BS_eh_E,BS_eh_f,BS_eh_en,&
&                       BS_eh_table,BS_bands,BS_blk_dim,BS_K_coupling,&
&                       BS_not_const_eh_f
#if defined PJ_SPIN 
 use electrons,    ONLY:filled_tresh
#endif
 use electrons,    ONLY:levels,n_sp_pol,spin_occ
 use R_lattice,    ONLY:qindx_X,bz_samp
 !
 implicit none
 type(levels) ::Ken
 type(bz_samp)::Xk
 integer      ::iq
 !
 ! Work Space
 !
 integer :: Iic(BS_bands(2),n_sp_pol),Iiv(BS_bands(2),n_sp_pol)
 integer :: ikbz,iv,ic,ik,ikp,table_dim,i_sp
 real(SP):: E_eh,f_eh
 !
 do while(.TRUE.) 
   !
   BS_K_dim=0
   !
   O_n_v_states=0
   O_n_c_states=0
   BS_blk_dim=0
   !
   do ikbz=1,Xk%nbz
     Iic=0
     Iiv=0
     !
     do iv=BS_bands(1),Ken%nbm
       do ic=Ken%nbf+1,BS_bands(2)
         !
         ik =Xk%sstar(ikbz,1)
         ikp=Xk%sstar(qindx_X(iq,ikbz,1),1)
         do i_sp=1,n_sp_pol
           !
           E_eh=Ken%E(ic,ik,i_sp)-Ken%E(iv,ikp,i_sp)
           if (abs(E_eh)<1.E-5) cycle
           !
           ! Same ordering procedure of the Xo case (see X_eh_setup.F)
           !
           f_eh=Ken%f(iv,ikp,i_sp)*(spin_occ-Ken%f(ic,ik,i_sp))
           if (abs(f_eh)<epsilon(1._SP)) cycle
           f_eh=f_eh/spin_occ/spin_occ          
#if defined PJ_SPIN
           !
           if (Ken%f(iv,ikp,i_sp)<=filled_tresh.or.&
&              abs(spin_occ-Ken%f(ic,ik,i_sp))<=filled_tresh ) cycle
#endif
           !
           ! A 5% fractional occupation swiches on the particular
           ! treatment of finite temp. BSE 
           !
           if ( abs(f_eh-1.)> 0.05) BS_not_const_eh_f=.TRUE.
           !
           ! The e/h occ must be 1. in the T=0 case as the spin factor (spin_occ)
           ! would erronously increase the e-e scattering
           !
           if (any((/abs(E_eh)<BS_eh_en(1).and.BS_eh_en(1)>0.,&
&                    abs(E_eh)>BS_eh_en(2).and.BS_eh_en(2)>0./))) cycle
           !
           BS_K_dim=BS_K_dim+1
           BS_blk_dim(ik)=BS_blk_dim(ik)+1
#if defined PJ_SPIN
           if (allocated(BS_eh_table)) BS_eh_table(BS_K_dim,:)=(/ikbz,iv,ic,i_sp/)
#else
           if (allocated(BS_eh_table)) BS_eh_table(BS_K_dim,:)=(/ikbz,iv,ic/)
#endif
           if (allocated(BS_eh_E)) BS_eh_E(BS_K_dim)=E_eh
           if (allocated(BS_eh_f)) BS_eh_f(BS_K_dim)=f_eh
           Iiv(iv,i_sp)=1
           Iic(ic,i_sp)=1
         enddo
       enddo
     enddo
     !
     if (.not.BS_K_coupling.and.BS_not_const_eh_f) then
       call error('Fractional e/h occupations found. BS coupling forced.')
     endif
     if (.not.BS_res_K_corr) cycle
     !
     do iv=BS_bands(1),Ken%nbm
       !
       do i_sp=1,n_sp_pol
         if (Iiv(iv,i_sp)==1) then
           O_n_v_states(ikbz,i_sp)=O_n_v_states(ikbz,i_sp)+1
           if (allocated(O_v_state)) O_v_state(ikbz,O_n_v_states(ikbz,i_sp),i_sp)=iv
          endif
       enddo
       !
     enddo
     do ic=Ken%nbf+1,BS_bands(2)
       !
       do i_sp=1,n_sp_pol
         if (Iic(ic,i_sp)==1) then
            O_n_c_states(ikbz,i_sp)=O_n_c_states(ikbz,i_sp)+1
            if (allocated(O_c_state)) O_c_state(ikbz,O_n_c_states(ikbz,i_sp),i_sp)=ic
         endif
       enddo
       !
     enddo
   enddo
   !
   if (allocated(BS_eh_table)) return
   !
   table_dim=3
#if defined PJ_SPIN
   table_dim=4
#endif
   allocate(BS_eh_table(BS_K_dim,table_dim),BS_eh_E(BS_K_dim),BS_eh_f(BS_K_dim))
   call mem_est("BS_eh_table BS_eh_E BS_eh_f",&
&               (/table_dim*BS_K_dim,BS_K_dim,BS_K_dim/),(/IP,SP,SP/))
   !
   if (.not.BS_res_K_corr) cycle
   allocate(O_v_state(Xk%nbz,maxval(O_n_v_states),n_sp_pol),&
&           O_c_state(Xk%nbz,maxval(O_n_c_states),n_sp_pol))
   call mem_est("O_v_state O_c_state",(/size(O_v_state),size(O_c_state)/),(/IP,IP/))
   !
 enddo
 !
end subroutine
