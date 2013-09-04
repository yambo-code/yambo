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
subroutine RT_G_retarded_EP_initialize(E,k)
 !
 use units
 use pars,           ONLY:rZERO,SP,IP4
 use R_lattice,      ONLY:nkibz,nqbz,bz_samp,qindx_S
 use electrons,      ONLY:levels,BZ_RIM_tot_nkpts,BZ_RIM_nkpt,BZ_RIM_table
 use SC,             ONLY:SC_bands
 use LIVE_t,         ONLY:live_timing
 use real_time,      ONLY:B_mat_index,Gr_ep_matrix,l_G_retarded_DB,Gr,Gr_peak_pos,&
&                         Gr_T_steps,Gr_T_step,l_RT_uses_E_RIM
 use memory_m,       ONLY:mem_est
 use parallel_m,     ONLY:PAR_IND_B_mat,PAR_Xk_nibz,PAR_nQ,PAR_n_B_mat_elements,PAR_IND_Xk_ibz,&
&                         PAR_Xk_ibz_index,PAR_IND_Q,PAR_B_mat_index,PAR_Q_index
 use ELPH,           ONLY:ph_modes,ph_freqs_sq
 !
 implicit none
 !
 type(levels)  :: E
 type(bz_samp) :: k
 !
 ! Work Space
 !
 real(SP):: GAMMA_,E_center
 integer :: ik,iqbz,i_n,i_np,ik_mem,i_b_mat_mem,iq_mem,ik_m_q,il,is,i_s,i_trans(2),kin_scatt(3),&
&           ik_rim_1,ik_rim_2,i1,i2
 real(SP), allocatable :: Gr_integrand(:)
 real(SP),    external :: Laplace_transform
 real(SP):: TRANSITIONS_E((SC_bands(2)-SC_bands(1)+1)*nkibz)
 logical :: eval_it
 integer(IP4)          :: TRANSITIONS_table((SC_bands(2)-SC_bands(1)+1)*nkibz,&
&                                           (SC_bands(2)-SC_bands(1)+1)*nkibz,3)
 !
 ! Allocation
 !
 call live_timing('[Gret] Electron-Phonon:',PAR_Xk_nibz*PAR_n_B_mat_elements*PAR_nQ)
 !
 allocate(Gr_ep_matrix(PAR_Xk_nibz,PAR_n_B_mat_elements,PAR_nQ,2*ph_modes))
 Gr_ep_matrix=rZERO
 call mem_est("Gr_ep_matrix",(/size(Gr_ep_matrix)/))
 !
 if (l_G_retarded_DB) then
   allocate(Gr_integrand(Gr_T_steps))
   !
   TRANSITIONS_E=0.
   TRANSITIONS_table=0
   !
   i_trans(1)=0
   do i_n=SC_bands(1),SC_bands(2)
     do ik=1,nkibz
       i_trans(1)=i_trans(1)+1
       TRANSITIONS_E(i_trans(1))=Gr_peak_pos(ik,i_n)
     enddo
   enddo
 endif
 !
 do ik=1,nkibz
   !
   if (.not.PAR_IND_Xk_ibz%element_1D(ik)) cycle
   ik_mem=PAR_Xk_ibz_index(ik)
   !
   do iqbz=1,nqbz
     !
     if (.not.PAR_IND_Q%element_1D(iqbz)) cycle
     iq_mem=PAR_Q_index(iqbz)
     !
     ik_m_q =k%sstar(qindx_S(ik,iqbz,1),1)
     !
     do i_n=SC_bands(1),SC_bands(2)
       !
       do i_np=SC_bands(1),SC_bands(2)
         !
         if ( .not. PAR_IND_B_mat%element_1D( B_mat_index(i_n,i_np) ) ) cycle
         i_b_mat_mem=PAR_B_mat_index(i_n,i_np)
         !
         do il=1,ph_modes
           !
           do is=-1,1,2
             !
             if (l_G_retarded_DB) then
               !
               E_center=Gr_peak_pos(ik,i_n)-Gr_peak_pos(ik_m_q,i_np)+i_s*abs(sqrt(ph_freqs_sq(iqbz,il)))
               !           
               if (abs(E_center)>2.*E%E_ind_gap(1)) cycle
               !
               i_trans(1)=transition_finder(Gr_peak_pos(ik,i_n))
               i_trans(2)=transition_finder(Gr_peak_pos(ik_m_q,i_np))
               !
               if (il==1.and.is==-1) then
                 kin_scatt=(/ik_mem,i_b_mat_mem,iq_mem/)
                 eval_it=.TRUE.
                 !
                 if (TRANSITIONS_table(i_trans(1),i_trans(2),1)/=0) then
                   eval_it=.FALSE.
                   kin_scatt=TRANSITIONS_table(i_trans(1),i_trans(2),:)
                 else if (TRANSITIONS_table(i_trans(2),i_trans(1),1)/=0) then
                   eval_it=.FALSE.
                   kin_scatt=TRANSITIONS_table(i_trans(2),i_trans(1),:)
                 endif
               endif
               !
               if (eval_it) then
                 Gr_integrand(:)=real( conjg(Gr(ik,i_n,:))*Gr(ik_m_q,i_np,:) )
                 Gr_ep_matrix(ik_mem,i_b_mat_mem,iq_mem,il+max(is,0)*ph_modes)=&
&                             abs(Laplace_transform(Gr_integrand,Gr_T_steps,Gr_T_step,E_center))
                 TRANSITIONS_table(i_trans(1),i_trans(2),:)=(/ik_mem,i_b_mat_mem,iq_mem/)
               else
                 Gr_ep_matrix(ik_mem,i_b_mat_mem,iq_mem,il+max(is,0)*ph_modes)=&
&                             Gr_ep_matrix(kin_scatt(1),kin_scatt(2),kin_scatt(3),il+max(is,0)*ph_modes)
               endif
               !
             else
               !
               if (l_RT_uses_E_RIM) then
                 Gr_ep_matrix(ik_mem,i_b_mat_mem,iq_mem,il+max(is,0)*ph_modes)=0._SP
                 GAMMA_ = abs(E%W(i_n,ik,1))+abs(E%W(i_np,ik_m_q,1))
                 do i1=1,BZ_RIM_nkpt(ik)
                   ik_rim_1=BZ_RIM_table(sum(k%nstar(:ik-1))+1,i1)
                   do i2=1,BZ_RIM_nkpt(ik_m_q)
                     ik_rim_2=BZ_RIM_table(sum(k%nstar(:ik_m_q-1))+1,i2)
                     E_center=E%E_RIM(i_n,ik_rim_1,1)-E%E_RIM(i_np,ik_rim_2,1)+is*abs(sqrt(ph_freqs_sq(iqbz,il)))
                     if (abs(E_center)>2.*E%E_ind_gap(1)) cycle
                     Gr_ep_matrix(ik_mem,i_b_mat_mem,iq_mem,il+max(is,0)*ph_modes)=&
&                       Gr_ep_matrix(ik_mem,i_b_mat_mem,iq_mem,il+max(is,0)*ph_modes)+&
&                       GAMMA_/(E_center**2+GAMMA_**2)/real(BZ_RIM_nkpt(ik))/real(BZ_RIM_nkpt(ik_m_q))
                   enddo
                 enddo
               else
                 E_center=E%E(i_n,ik,1)-E%E(i_np,ik_m_q,1)+is*abs(sqrt(ph_freqs_sq(iqbz,il)))
                 !
                 if (abs(E_center)>2.*E%E_ind_gap(1)) cycle
                 !
                 GAMMA_ = abs(E%W(i_n,ik,1))+abs(E%W(i_np,ik_m_q,1))
                 !
                 Gr_ep_matrix(ik_mem,i_b_mat_mem,iq_mem,il+max(is,0)*ph_modes)=GAMMA_/(E_center**2+GAMMA_**2)
                 !
               endif
             endif
             !
           enddo
           !
         enddo
         !
         call live_timing(steps=1)
         !
       enddo
       !
     enddo
     !
   enddo
   !
 enddo
 !
 call live_timing()
 !
 contains
   !
   integer function transition_finder(E)
     integer  :: ic
     real(SP) :: E
     transition_finder=-1
     do ic=1,(SC_bands(2)-SC_bands(1)+1)*nkibz
       if (abs(E-TRANSITIONS_E(ic))<1.E-5) then
         transition_finder=ic
         return
       endif
     enddo
   end function
   !
end subroutine RT_G_retarded_EP_initialize

