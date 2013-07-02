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
subroutine RT_G_retarded_EE_initialize(E,k,q)
 !
 use pars,           ONLY:SP,rZERO,IP
 use QP_m,           ONLY:QP_n_states,QP_table
 use real_time,      ONLY:Gr_ee_matrix,l_G_retarded_DB,Gr_T_steps,Gr,Gr_peak_pos,Gr_T_step
 use memory_m,       ONLY:mem_est
 use SC,             ONLY:SC_bands
 use D_lattice,      ONLY:nsym
 use R_lattice,      ONLY:bz_samp,nqbz,qindx_S,nkibz
 use parallel_m,     ONLY:PAR_nQP,PAR_nQPp,PAR_nQ,PAR_IND_Q,PAR_IND_QP,PAR_IND_QPp,PAR_Q_index,&
&                         PAR_QP_index,PAR_QPp_index
 use LIVE_t,         ONLY:live_timing
 use electrons,      ONLY:levels
 use matrix_operate, ONLY:matrix_linear_index
 implicit none
 !
 type(levels)  :: E
 type(bz_samp) :: k,q
 !
 ! Work Space
 !
 integer :: i_qp1,i_qp2,i_qp1_mem,i_qp2_mem,i_n,i_k,i_m,i_p,i_np,i_kmq,i_mp,&
&           i_q,i_q_s,iqbz,iq_mem,i_p_s,i_p_star,i_pmq_bz,i_pmq,i_pmq_G0,&
&           i_trans(4),kin_scatt(4),i_t,N_2bodies,N_4bodies,I_ij,I_kl,I_ij_kl,i_trans_ord(4),n_trans
 real(SP):: GAMMA_(2),WIDTH(4,2),E_center
 real(SP):: TRANSITIONS_E((SC_bands(2)-SC_bands(1)+1)*nkibz)
 real(SP), allocatable :: Gr_integrand(:)
 real(SP),    external :: Laplace_transform
 logical                   :: eval_the_laplace
 integer(IP), allocatable  :: TRANSITIONS_table(:,:)
 !
 call k_sym2sym(q,'k')
 call k_sym2sym(k,'k')
 !
 n_trans=0
 TRANSITIONS_E=rZERO
 call mem_est("TRANSITIONS_E",(/size(TRANSITIONS_E)/),(/SP/))
 do i_n=SC_bands(1),SC_bands(2)
   do i_k=1,nkibz
     n_trans=n_trans+1
     TRANSITIONS_E(n_trans)=Gr_peak_pos(i_k,i_n)
   enddo
 enddo
 !
 ! Gr (e-e)
 !==========
 !
 ! Dimension of the table (2/7/13). If |i>=|nk> I have that
 !
 !  Gr_{ij,lk}= Gr_{ji,lk}
 !            = Gr_{ij,kl}
 !            = Gr_{ji,kl}
 !            = Gr_{lk,ij}
 !            = Gr_{lk,ji}
 !            = Gr_{kl,ij}
 !            = Gr_{kl,ji}
 !
 ! Thus I can re-order ij and kl in globally order indexes 
 ! that runs up to N_2bodies, and re-order (ij) and (kl) in N_4bodies
 !
 N_2bodies= matrix_linear_index(1,(SC_bands(2)-SC_bands(1)+1)*nkibz)-1
 N_4bodies= matrix_linear_index(1,N_2bodies)-1
 !
 ! Allocation
 !
 if (l_G_retarded_DB) then
   allocate(TRANSITIONS_table(N_4bodies,4))
   TRANSITIONS_table=0
   call mem_est("TRANSITIONS_table",(/size(TRANSITIONS_table)/),(/IP/))
   TRANSITIONS_table=0
   allocate(Gr_integrand(Gr_T_steps))
   call mem_est("Gr_ee_matrix",(/size(Gr_ee_matrix)/))
 endif
 allocate(Gr_ee_matrix(PAR_nQP,PAR_nQPp,PAR_nQ,nsym))
 Gr_ee_matrix=rZERO
 call live_timing('[Gret] Electron-Electron:',PAR_nQP*PAR_nQPp*PAR_nQ)
 !
 do i_qp1=1,QP_n_states
   !
   if (.not.PAR_IND_QP%element_1D(i_qp1)) cycle
   i_qp1_mem=PAR_QP_index(i_qp1)
   !
   i_n  = QP_table(i_qp1,1)
   i_np = QP_table(i_qp1,2)
   i_k  = QP_table(i_qp1,3)
   if (l_G_retarded_DB) i_trans(4)=transition_finder(Gr_peak_pos(i_k,i_n))
   !
   do i_qp2=1,QP_n_states
     !
     if (.not.PAR_IND_QPp%element_1D(i_qp2)) cycle
     i_qp2_mem=PAR_QPp_index(i_qp2)
     !
     i_m   = QP_table(i_qp2,1)
     i_mp  = QP_table(i_qp2,2)
     i_p   = QP_table(i_qp2,3)
     if (l_G_retarded_DB) i_trans(2)=transition_finder(Gr_peak_pos(i_p,i_m))
     !
     do iqbz=1,nqbz
       !
       if (.not.PAR_IND_Q%element_1D(iqbz)) cycle
       iq_mem=PAR_Q_index(iqbz)
       !
       i_q   =q%sstar(iqbz,1)
       i_q_s =q%sstar(iqbz,2)
       i_kmq =k%sstar(qindx_S(i_k,iqbz,1),1)
       if (l_G_retarded_DB) then
         i_trans(1)=transition_finder(Gr_peak_pos(i_kmq,i_np))
         i_trans_ord(1)=min(i_trans(1),i_trans(2))
         i_trans_ord(2)=max(i_trans(1),i_trans(2))
         I_ij=matrix_linear_index(i_trans_ord(1),i_trans_ord(2))-1
       endif
       !
       do i_p_star=1,k%nstar(i_p)
         !
         i_p_s =k%star(i_p,i_p_star)
         !
         call RT_EE_scattering_kinematics(k,q,i_p,i_p_s,i_q,i_q_s,i_pmq_bz,i_pmq_G0)
         !
         i_pmq =k%sstar(i_pmq_bz,1)
         if (l_G_retarded_DB) then
           i_trans(3)=transition_finder(Gr_peak_pos(i_pmq,i_mp))
           i_trans_ord(3)=min(i_trans(3),i_trans(4))
           i_trans_ord(4)=max(i_trans(3),i_trans(4))
           I_kl=matrix_linear_index(i_trans_ord(3),i_trans_ord(4))-1
           I_ij_kl=matrix_linear_index(min(I_ij,I_kl),max(I_ij,I_kl))-1
         endif
         !
         if (l_G_retarded_DB) then
           !
           E_center=Gr_peak_pos(i_kmq,i_np)-Gr_peak_pos(i_k,i_n)+Gr_peak_pos(i_p,i_m)-Gr_peak_pos(i_pmq,i_mp)
           !
           kin_scatt=(/i_qp1_mem,i_qp2_mem,iq_mem,i_p_s/)
           eval_the_laplace=.TRUE.
           !
           if (TRANSITIONS_table(I_ij_kl,1)/=0) then
             eval_the_laplace=.FALSE.
             kin_scatt=TRANSITIONS_table(I_ij_kl,:)
           endif
           !
           if (eval_the_laplace) then
             !
             TRANSITIONS_table(I_ij_kl,:)=(/i_qp1_mem,i_qp2_mem,iq_mem,i_p_s/)
             !
             Gr_integrand(:)=real( Gr(i_kmq,i_np,:)*Gr(i_p,i_m,:)*&
&                                  conjg(Gr(i_pmq,i_mp,:))*conjg(Gr(i_k,i_n,:)) )
             Gr_ee_matrix(i_qp1_mem,i_qp2_mem,iq_mem,i_p_s)=&
&                         Laplace_transform(Gr_integrand,Gr_T_steps,Gr_T_step,E_center)
             !
 !write (200,*) i_kmq,i_np,i_p,i_m
 !write (200,*) i_pmq,i_mp,i_k,i_n
 !write (200,*) i_trans_ord,Gr_ee_matrix(i_qp1_mem,i_qp2_mem,iq_mem,i_p_s),' EVALUED'
           else
             !
             Gr_ee_matrix(i_qp1_mem,i_qp2_mem,iq_mem,i_p_s)=Gr_ee_matrix(kin_scatt(1),kin_scatt(2),&
&                                                                        kin_scatt(3),kin_scatt(4))
 !write (200,*) i_trans,Gr_ee_matrix(i_qp1_mem,i_qp2_mem,iq_mem,i_p_s),' LOADED'
             !
           endif
           !
         else
           !
           !pre_factor=8.7
           !
           E_center=E%E(i_np,i_kmq,1)-E%E(i_n,i_k,1)+E%E(i_m,i_p,1)-E%E(i_mp,i_pmq,1)
           !
           !DEBUG>
           !WIDTH(1,:)=set_WIDTH(i_np,i_kmq,abs(E%W(i_np,i_kmq,1)))
           !WIDTH(2,:)=set_WIDTH(i_n,i_k,   abs(E%W(i_n,i_k,1)))
           !WIDTH(3,:)=set_WIDTH(i_m,i_p,   abs(E%W(i_m,i_p,1)))
           !WIDTH(4,:)=set_WIDTH(i_mp,i_pmq,abs(E%W(i_mp,i_pmq,1)))
           !DEBUG<
           !
           GAMMA_(1) = WIDTH(1,1)+WIDTH(2,2)+WIDTH(3,1)+WIDTH(4,2)
           GAMMA_(2) = WIDTH(1,2)+WIDTH(2,1)+WIDTH(3,2)+WIDTH(4,1)
           !
           !DEBUG>
           !if (E_center> GAMMA_(2)*pre_factor) goto 2
           !if (E_center<-GAMMA_(1)*pre_factor) goto 2
           !DEBUG<
           !
           GAMMA_(1) = abs(E%W(i_np,i_kmq,1))+abs(E%W(i_m,i_p,1))+abs(E%W(i_mp,i_pmq,1))+abs(E%W(i_n,i_k,1))
           !
           Gr_ee_matrix(i_qp1_mem,i_qp2_mem,iq_mem,i_p_s)=GAMMA_(1)/(E_center**2+GAMMA_(1)**2)
           !
         endif
         !
         !DEBUG>
         !write (100,*) E_center*27.21, Gr_ee_matrix(i_qp1_mem,i_qp2_mem,iq_mem,i_p_s)
         !DEBUG<
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
 call live_timing()
 !
 if (l_G_retarded_DB) deallocate(Gr_integrand,TRANSITIONS_table)
 call mem_est("TRANSITIONS_table")
 !
 contains
   !
   integer function transition_finder(E)
     integer  :: ic
     real(SP) :: E
     transition_finder=-1
     do ic=1,(SC_bands(2)-SC_bands(1)+1)*nkibz
       if (abs(E-TRANSITIONS_E(ic))<1.E-10) then
         transition_finder=ic
         return
       endif
     enddo
   end function
   !
   function set_WIDTH(i_n,i_k,W)
     !
     integer  :: i_k,i_n
     real(SP) :: W
     real(SP) :: set_WIDTH(2)
     !
     set_WIDTH=W
     !
     !DEBUG>
     !if (i_n<=E%nbf  ) set_WIDTH(2)=min( W ,abs( E%E(i_n,i_k,1) - E_vb_max )) 
     !if (i_n>=E%nbm+1) set_WIDTH(1)=min( W ,abs( E%E(i_n,i_k,1) - E_cb_min )) 
     !DEBUG<
     !
   end function
   !
end subroutine RT_G_retarded_EE_initialize
