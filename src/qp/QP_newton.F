!
!        Copyright (C) 2000-2019 the YAMBO team
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
subroutine QP_newton(X,Xen,Xk,en,k,q,qp,Xw,Dip)
 !
 use pars,          ONLY:SP,cZERO
 use units,         ONLY:HA2EV
 use drivers,       ONLY:l_ppa,l_elel_corr,l_cohsex
 use com,           ONLY:msg
 use DIPOLES,       ONLY:DIPOLE_t
 use X_m,           ONLY:X_t
 use QP_m,          ONLY:QP_t,QP_dSc_steps,QP_Vnl_xc,QP_Vxc,QP_Sc,&
&                        QP_n_states,QP_dSc,QP_dSc_delta,QP_n_G_bands,&
&                        GWo_SC_done,GWo_iterations,SC_E_threshold,On_Mass_Shell_approx
 use frequency,     ONLY:w_samp
 use electrons,     ONLY:levels
 use R_lattice,     ONLY:bz_samp
 use QP_CTL_m,      ONLY:QP_apply
#if defined _ELPH
 use drivers,       ONLY:l_elph_corr
#endif
#if defined _QED
 use drivers,       ONLY:l_elphoton_corr
#endif
 implicit none
 type(levels) ::en,Xen
 type(bz_samp)::Xk,k,q
 type(X_t)    ::X
 type(QP_t)   ::qp
 type(DIPOLE_t) :: Dip
 type(w_samp) ::Xw
 !
 ! Work Space
 !
 integer     :: i1,i2,iter
 real(SP)    :: SC_corr_prev,SC_corr_now
 complex(SP) :: Z(QP_dSc_steps-1),Eqp(QP_dSc_steps-1)
 !
 ! Dyson equation: Newton solver 
 !
 if (l_cohsex) then
   !
   call msg('r', '[Newton] Sc step   [ev]:',QP_dSc_delta*HA2EV)
   call msg('r', '[Newton] Sc steps      :',QP_dSc_steps)
   !
 endif
 !
 call msg('rn','[Newton] SC iterations :',GWo_iterations)
 !
 iter=0
 SC_corr_prev=1._SP
 !
 ! GPL_EXCLUDE_START
 !
 do while((iter<=GWo_iterations.or.GWo_iterations<0).and..not.GWo_SC_done) 
   !
   ! GPL_EXCLUDE_END
   !
   if (l_elel_corr) then
     if (l_ppa.or.l_cohsex) then
       !
       call RIM_W(q)
       call QP_ppa_cohsex(X,Xk,en,k,q,qp,Xw,iter)
       !
     else 
       !
       call QP_real_axis(X,Xen,Xk,en,k,q,qp,Xw,Dip,iter)
       !
     endif
   else
     QP_Sc=cZERO
   endif
   !
# if defined _ELPH 
   !
   if (l_elph_corr)     call ELPH_Sigma_c(en,k,q,qp)
   !
#endif
   !
# if defined _QED 
   !
   if (l_elphoton_corr) then
     !
     QP_dSc=cZERO
     !
     call QED_Sigma_c(en,k,q,qp)
     !
   endif
   !
#endif
   !
   do i1=1,QP_n_states
     !
     if(.not.l_cohsex) then
       !     
# if defined _QED 
       !
       ! In the e-gamma case the derivative is calculated analitically
       !     
       if (.not.l_elphoton_corr) then
#endif
         !
         QP_dSc(i1,1)=cZERO
         !     
         if (.not.On_Mass_Shell_approx) then
           do i2=1,QP_dSc_steps-1
             QP_dSc(i1,i2)=(QP_Sc(i1,i2+1)-QP_Sc(i1,i2))/QP_dSc_delta
           enddo
         endif
         !
# if defined _QED 
       endif
#endif
       !
       do i2=1,QP_dSc_steps-1
         !
         Z(i2)=1._SP/(1._SP-QP_dSc(i1,i2))
         !
         Eqp(i2)=qp%E_bare(i1)+Z(i2)*QP_Sc(i1,1)
         !
         if (l_elel_corr) Eqp(i2)=Eqp(i2)+Z(i2)*(QP_Vnl_xc(i1)-QP_Vxc(i1))
         !
       enddo
       !
       qp%E(i1)=Eqp(1)
       qp%Z(i1)=Z(1)
       !
     else
       !
       ! COHSEX: no energy dependence
       !
       qp%E(i1)=qp%E_bare(i1)+(QP_Sc(i1,1)+QP_Vnl_xc(i1)-QP_Vxc(i1))
       qp%Z(i1)=1._SP
       !
     endif
     !
   enddo
   !
   SC_corr_now= maxval(real(qp%E(:)-qp%E_bare(:)))
   GWo_SC_done=abs( SC_corr_prev-SC_corr_now )<SC_E_threshold.or.iter==GWo_iterations
   !
   ! GPL_EXCLUDE_START
   !
   if (iter>0) call msg('rs','[SC] Max corrections at prev/current cycle [ev]',&
&                      (/SC_corr_prev,SC_corr_now/)*HA2EV)
   SC_corr_prev=SC_corr_now
   !
   if (.not.GWo_SC_done.and.GWo_iterations/=0) then
     !
     ! Now I need to transfer the calculated QP corrections to the "en" type
     ! and to the qp%Eo array
     !
     call QP_apply(QP_n_G_bands,en,k,'G',msg_fmt='rs',qp_impose=qp)
     !
   endif
   !
   ! Increase the counter
   !
   iter=iter+1
   !
 enddo
 !
 ! GPL_EXCLUDE_END
 !
 ! Update GWo_iterations
 !
 if (GWo_iterations<0) GWo_iterations=iter
 !
end subroutine

subroutine RIM_W(q)
 !
 use pars,          ONLY:SP
 use com,           ONLY:msg
 use R_lattice,     ONLY:bz_samp,RIM_ng,g_vec,b,k_grid_b,ng_vec,rl_sop
 use memory,        ONLY:MEM_err
 use vec_operate,   ONLY:c2a,v_is_zero
 !
 implicit none
 type(bz_samp):: q
 !
 ! Work Space
 !
 integer :: iq1, ig1, iq2, ig2, idx_q(3), out_unit_G=101, out_unit_qpG_in=102,out_unit_qpG_out,id
 integer :: iq_ibz, is, find, nn, i
 integer, allocatable :: idx_q_nn(:,:,:),idx_G_nn(:,:,:),idx_is_nn(:,:,:)
 real(SP) :: qpG(3), qpG_nn(3,4), delta_q(3,2), qpG_trial(3)
 logical :: debug
 !
 debug = .false.
 call k_ibz2bz(q,'a',.true.)
 if (debug) then
   open (unit=out_unit_G,file="G-vec.dat",action="write",status="replace")
   open (unit=out_unit_qpG_in,file="qpG_in.dat",action="write",status="replace")
   open (unit=out_unit_qpG_out,file="qpG_out.dat",action="write",status="replace")
 end if
 !
 allocate(idx_q_nn (4,q%nbz,RIM_ng),stat=MEM_err)
 allocate(idx_G_nn (4,q%nbz,RIM_ng),stat=MEM_err)
 allocate(idx_is_nn (4,q%nbz,RIM_ng),stat=MEM_err)
 !
 !Calculate infinitesimal steps in rlu
 do id=1,2
   delta_q(:,id) = k_grid_b(id,:)
   call c2a(b_in=b,v_in=delta_q(:,id),mode='kc2a')
   if (debug) then
     call msg('r', "delta_q =", delta_q(:,id))
   end if
 end do
 !
 !Cycles over qpG points
 do ig1=1,RIM_ng
   !
   if (debug) then
     !Print G vectors into a file
     write(out_unit_G,"(I3,E15.6,E15.6,E15.6)") ig1,g_vec(ig1,1), g_vec(ig1,2), g_vec(ig1,3)
   end if
   !
   do iq1=1,q%nbz
     !
     find = 0
     !
     !Calculate qpG and qpG nearest neighbour
     qpG = matmul(rl_sop(:,:,q%sstar(iq1,2)),q%pt(q%sstar(iq1,1),:)+g_vec(ig1,:))
     call c2a(b_in=b,v_in=qpG,mode='ki2a')
     qpG_nn(:,1) = qpG + delta_q(:,1)
     qpG_nn(:,2) = qpG + delta_q(:,2)
     qpG_nn(:,3) = qpG - delta_q(:,1)
     qpG_nn(:,4) = qpG - delta_q(:,2)
     !
     !Print qpG and qpG_nn into a file
     write(out_unit_qpG_in,"(I4,I4,F7.3,F7.3,F7.3,F7.3,F7.3,F7.3,F7.3,F7.3,F7.3,F7.3,F7.3,F7.3,F7.3,F7.3,F7.3)")&
& iq1,ig1,qpG(1), qpG(2), qpG(3),qpG_nn(1,1), qpG_nn(2,1), qpG_nn(3,1)&
&,qpG_nn(1,2), qpG_nn(2,2), qpG_nn(3,2),qpG_nn(1,3), qpG_nn(2,3), qpG_nn(3,3)&
&,qpG_nn(1,4), qpG_nn(2,4), qpG_nn(3,4)
     !
     !Find nn indexes
     do i=1,ng_vec
       !
       !Try first ig2 near ig1
       if (i < 2*ig1) then
         ig2 = (2*(i-2*(i/2))-1)*(i/2) + ig1
       else
         ig2 = i
       end if
       !
       do iq2=1,q%nbz
         !
         !Get ibz and is indexes
         iq_ibz = q%sstar(iq2,1)
         is = q%sstar(iq2,2)
         qpG_trial = matmul(rl_sop(:,:,is),q%pt(iq_ibz,:)+g_vec(ig2,:))
         call c2a(b_in=b,v_in=qpG_trial,mode='ki2a')
         !
         do nn=1,4
           if (v_is_zero(qpG_trial-qpG_nn(:,nn))) then
             idx_q_nn(nn,iq1,ig1) = iq_ibz
             idx_G_nn(nn,iq1,ig1) = ig2
             idx_is_nn(nn,iq1,ig1) = is
             find = find + 1
           end if
         end do
         !
         if (find == 4) then
           exit
         end if
         !
       end do
       !
       if (find == 4) then
         exit
       end if
       !
     end do
     !
     if (debug) then
       !Check if nearest neighbours are obtained correctly
       qpG_nn(:,1) = matmul(rl_sop(:,:,idx_is_nn(1,iq1,ig1)),q%pt(idx_q_nn(1,iq1,ig1),:)+g_vec(idx_G_nn(1,iq1,ig1),:))
       qpG_nn(:,2) = matmul(rl_sop(:,:,idx_is_nn(2,iq1,ig1)),q%pt(idx_q_nn(2,iq1,ig1),:)+g_vec(idx_G_nn(2,iq1,ig1),:))
       qpG_nn(:,3) = matmul(rl_sop(:,:,idx_is_nn(3,iq1,ig1)),q%pt(idx_q_nn(3,iq1,ig1),:)+g_vec(idx_G_nn(3,iq1,ig1),:))
       qpG_nn(:,4) = matmul(rl_sop(:,:,idx_is_nn(4,iq1,ig1)),q%pt(idx_q_nn(4,iq1,ig1),:)+g_vec(idx_G_nn(4,iq1,ig1),:))
       !
       !From iku to rlu
       call c2a(b_in=b,v_in=qpG_nn(:,1),mode='ki2a')
       call c2a(b_in=b,v_in=qpG_nn(:,2),mode='ki2a')
       call c2a(b_in=b,v_in=qpG_nn(:,3),mode='ki2a')
       call c2a(b_in=b,v_in=qpG_nn(:,4),mode='ki2a')
       !Write qpG out into a file
       write(out_unit_qpG_out,"(I4,I4,F7.3,F7.3,F7.3,F7.3,F7.3,F7.3,F7.3,F7.3,F7.3,F7.3,F7.3,F7.3,F7.3,F7.3,F7.3)")&
& iq1,ig1,qpG(1), qpG(2), qpG(3),qpG_nn(1,1), qpG_nn(2,1), qpG_nn(3,1)&
&,qpG_nn(1,2), qpG_nn(2,2), qpG_nn(3,2),qpG_nn(1,3), qpG_nn(2,3), qpG_nn(3,3)&
&,qpG_nn(1,4), qpG_nn(2,4), qpG_nn(3,4)
     end if
     !
   end do
   !
 end do
 !
end subroutine





