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
 use pars,           ONLY:SP,rZERO
 use units,          ONLY:HA2EV
 use QP_m,           ONLY:QP_n_states,QP_table
 use real_time,      ONLY:Gr_ee_matrix,l_G_retarded_DB,Gr_T_steps,Gr,Gr_peak_pos,Gr_T_step
 use memory_m,       ONLY:mem_est
 use D_lattice,      ONLY:nsym
 use R_lattice,      ONLY:bz_samp,nqbz,qindx_S
 use parallel_m,     ONLY:PAR_nQP,PAR_nQPp,PAR_nQ,PAR_IND_Q,PAR_IND_QP,PAR_IND_QPp,PAR_Q_index,&
&                         PAR_QP_index,PAR_QPp_index
 use timing,         ONLY:live_timing
 use electrons,      ONLY:levels
 implicit none
 !
 type(levels)  :: E
 type(bz_samp) :: k,q
 !
 ! Work Space
 !
 integer :: i_qp1,i_qp2,i_qp1_mem,i_qp2_mem,i_n,i_k,i_m,i_p,i_np,i_kmq,i_mp,&
&           i_q,i_q_s,iqbz,iq_mem,i_p_s,i_p_star,i_pmq_bz,i_pmq,i_pmq_G0
 real(SP):: GAMMA_(2),WIDTH(4,2),pre_factor,E_center
 real(SP), allocatable :: Gr_integrand(:)
 real(SP),    external :: Laplace_transform
 !
 ! Allocation
 !
 allocate(Gr_ee_matrix(PAR_nQP,PAR_nQPp,PAR_nQ,nsym))
 Gr_ee_matrix=rZERO
 call mem_est("Gr_ee_matrix",(/size(Gr_ee_matrix)/))
 !
 if (l_G_retarded_DB) allocate(Gr_integrand(Gr_T_steps))
 !
 call live_timing('Electron-Electron:',PAR_nQP*PAR_nQPp*PAR_nQ)
 !
 call k_sym2sym(q,'k')
 call k_sym2sym(k,'k')
 !
 ! Gr (e-e)
 !==========
 !
 do i_qp1=1,QP_n_states
   !
   if (.not.PAR_IND_QP%element_1D(i_qp1)) cycle
   i_qp1_mem=PAR_QP_index(i_qp1)
   !
   i_n  = QP_table(i_qp1,1)
   i_np = QP_table(i_qp1,2)
   i_k  = QP_table(i_qp1,3)
   !
   do i_qp2=1,QP_n_states
     !
     if (.not.PAR_IND_QPp%element_1D(i_qp2)) cycle
     i_qp2_mem=PAR_QPp_index(i_qp2)
     !
     i_m   = QP_table(i_qp2,1)
     i_mp  = QP_table(i_qp2,2)
     i_p   = QP_table(i_qp2,3)
     !
     do iqbz=1,nqbz
       !
       if (.not.PAR_IND_Q%element_1D(iqbz)) cycle
       iq_mem=PAR_Q_index(iqbz)
       !
       i_q   =q%sstar(iqbz,1)
       i_q_s =q%sstar(iqbz,2)
       i_kmq =k%sstar(qindx_S(i_k,iqbz,1),1)
       !
       do i_p_star=1,k%nstar(i_p)
         !
         i_p_s =k%star(i_p,i_p_star)
         !
         call RT_EE_scattering_kinematics(k,q,i_p,i_p_s,i_q,i_q_s,i_pmq_bz,i_pmq_G0)
         !
         i_pmq =k%sstar(i_pmq_bz,1)
         !
         if (l_G_retarded_DB) then
           !
           Gr_integrand(:)=real( Gr(i_kmq,i_np,:)*Gr(i_p,i_m,:)*conjg(Gr(i_pmq,i_mp,:))*conjg(Gr(i_k,i_n,:)) )
           !
           E_center=Gr_peak_pos(i_kmq,i_np)-Gr_peak_pos(i_k,i_n)+Gr_peak_pos(i_p,i_m)-Gr_peak_pos(i_pmq,i_mp)
           !
           !DEBUG>
           !pre_factor=4.8
           !WIDTH(1,:)=set_WIDTH(i_np,i_kp,maxval(  Gr_E_ranges(i_kp,i_np,:) ))
           !WIDTH(2,:)=set_WIDTH(i_n,i_k,  maxval(  Gr_E_ranges(i_k,i_n,:) ))
           !WIDTH(3,:)=set_WIDTH(i_m,i_p,  maxval(  Gr_E_ranges(i_p,i_m,:) ))
           !WIDTH(4,:)=set_WIDTH(i_mp,i_pp,maxval(  Gr_E_ranges(i_pp,i_mp,:) ))
           !GAMMA_(1) = WIDTH(1,1)+WIDTH(2,2)+WIDTH(3,1)+WIDTH(4,2)
           !GAMMA_(2) = WIDTH(1,2)+WIDTH(2,1)+WIDTH(3,2)+WIDTH(4,1)
           !
           !if (E_center> GAMMA_(2)*pre_factor) goto 2
           !if (E_center<-GAMMA_(1)*pre_factor) goto 2
           !DEBUG<
           !
           Gr_ee_matrix(i_qp1_mem,i_qp2_mem,iq_mem,i_p_s)=Laplace_transform(Gr_integrand,Gr_T_steps,Gr_T_step,E_center)
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
 !if (i_qp1_mem==1.and.i_qp2_mem==1.and.iq_mem==1.and.i_p_s==1) write (*,*)  E_center*27.21, Gr_ee_matrix(i_qp1_mem,i_qp2_mem,iq_mem,i_p_s)
 !if (i_qp1_mem==1.and.i_qp2_mem==2.and.iq_mem==2.and.i_p_s==1) write (*,*)  E_center*27.21, Gr_ee_matrix(i_qp1_mem,i_qp2_mem,iq_mem,i_p_s)
 !if (i_qp1_mem==2.and.i_qp2_mem==18.and.iq_mem==1.and.i_p_s==1) write (*,*)  E_center*27.21, Gr_ee_matrix(i_qp1_mem,i_qp2_mem,iq_mem,i_p_s)
 !if (i_qp1_mem==2.and.i_qp2_mem==22.and.iq_mem==5.and.i_p_s==1) write (*,*)  E_center*27.21, Gr_ee_matrix(i_qp1_mem,i_qp2_mem,iq_mem,i_p_s)
 !if (i_qp1_mem==2.and.i_qp2_mem==22.and.iq_mem==5.and.i_p_s==1) stop
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
 if (l_G_retarded_DB) deallocate(Gr_integrand)
 !
 contains
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
