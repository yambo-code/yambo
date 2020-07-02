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
       call interpolate_W(X,Xw,q)
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

subroutine interpolate_W(X,Xw,q)
 !
 use pars,          ONLY:SP,pi,zero_dfl,schlen
 use com,           ONLY:msg
 use R_lattice,     ONLY:bz_samp,RIM_ng,g_vec,b,k_grid_b,ng_vec,rl_sop,bare_qpg,&
&                        cut_geometry,RIM_W
 use memory,        ONLY:MEM_err
 use vec_operate,   ONLY:c2a,v_is_zero,v_norm
 use X_m,           ONLY:X_mat,X_t,X_alloc
 use frequency,     ONLY:w_samp
 use IO_m,          ONLY:io_control,OP_RD_CL,NONE,RD_CL,OP_RD,RD_CL_IF_END,&
&                        deliver_IO_error_message,OP_WR_CL,REP,VERIFY
 use stderr,        ONLY:STRING_split
 !
 implicit none
 type(X_t)    ::X
 type(w_samp) ::Xw
 type(bz_samp):: q
 !
 ! Work Space
 !
 integer :: iq1, ig1, iq2, ig2, iq, ig
 integer :: out_unit_G=100, out_unit_qpG_in=101,out_unit_qpG_out=102,out_unit_idx=103,out_unit_vX=104
 integer :: out_unit_f_coeff=105
 integer :: iq_ibz, is, find, nn, i, iq_to_start,idm, ng_tmp
 integer, allocatable :: idx_q(:,:,:),idx_G(:,:,:),idx_is(:,:,:)
 real(SP) :: G_max_ibz(q%nibz), f_func(5)
 complex(SP), allocatable :: vX(:,:,:)
 real :: f_coeff (5,q%nibz,RIM_ng)
 real(SP) :: qpG(3,5), delta_q(3,2), qpG_trial(3), v1(3)
 logical :: debug
 integer :: ID, io_err, ng_save
 integer,     external ::io_X
 logical          :: cut_is_slab,is_cut(3)
 character(schlen):: str_piece(5)
 integer, external:: io_RIM_W
 !
 !
 debug = .true.
 call k_ibz2bz(q,'a',.true.)
 if (debug) then
   open (unit=out_unit_G,file="G-vec.dat",action="write",status="replace")
   open (unit=out_unit_qpG_in,file="qpG_in.dat",action="write",status="replace")
   open (unit=out_unit_qpG_out,file="qpG_out.dat",action="write",status="replace")
   open (unit=out_unit_idx,file="idx.dat",action="write",status="replace")
   open (unit=out_unit_vX,file="vX.dat",action="write",status="replace")
   open (unit=out_unit_f_coeff,file="f_coeff.dat",action="write",status="replace")
 end if
 !
 allocate(idx_q (5,q%nibz,RIM_ng),stat=MEM_err)
 allocate(idx_G (5,q%nibz,RIM_ng),stat=MEM_err)
 allocate(idx_is (5,q%nibz,RIM_ng),stat=MEM_err)
 allocate(vX (5,q%nibz,RIM_ng),stat=MEM_err)
 !allocate(f_coeff (5,q%nibz,RIM_ng),stat=MEM_err)
 !
 !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 !Calculation of the nearest neighbours indexes
 !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 !
 !Calculate infinitesimal steps in rlu
 do idm=1,2
   v1 = k_grid_b(idm,:)
   call c2a(b_in=b,v_in=v1,mode='kc2a')
   if (abs(v1(1)) < zero_dfl) then
     delta_q(:,2) = v1
   else if (abs(v1(2)) < zero_dfl) then
     delta_q(:,1) = v1
   else
     call msg('r', 'Error in the selection of Delta_q')
   end if
 end do
 !
 if (debug) call msg('r', "delta_q(1) =", delta_q(:,1))
 if (debug) call msg('r', "delta_q(2) =", delta_q(:,2))
 !
 !Cycles over qpG points
 do ig1=1,RIM_ng
   !
   do iq1=1,q%nibz
     !
     find = 0
     !
     idx_q(1,iq1,ig1) = iq1
     idx_is(1,iq1,ig1) = 1
     idx_G(1,iq1,ig1) = ig1
     !
     !Calculate qpG and qpG nearest neighbour
     qpG(:,1) = matmul(rl_sop(:,:,idx_is(1,iq1,ig1)),q%pt(idx_q(1,iq1,ig1),:)+g_vec(ig1,:))
     call c2a(b_in=b,v_in=qpG(:,1),mode='ki2a')
     !
     !Index ordering of nearest neighbours
     !
     !       2
     !       ^
     !       |
     ! 5 <-- 1 --> 3
     !       |
     !       v
     !       4
     !                 
     qpG(:,2) = qpG(:,1) + delta_q(:,2)
     qpG(:,3) = qpG(:,1) + delta_q(:,1)
     qpG(:,4) = qpG(:,1) - delta_q(:,2)
     qpG(:,5) = qpG(:,1) - delta_q(:,1)
     !
     if (debug) then
       !Print qpG and qpG into a file
       write(out_unit_qpG_in,"(I4,I4,F7.3,F7.3,F7.3,F7.3,F7.3,F7.3,F7.3,F7.3,F7.3,F7.3,F7.3,F7.3,F7.3,F7.3,F7.3)")&
& iq1,ig1,qpG(1,1), qpG(2,1), qpG(3,1),qpG(1,2), qpG(2,2), qpG(3,2)&
&,qpG(1,3), qpG(2,3), qpG(3,3),qpG(1,4), qpG(2,4), qpG(3,4)&
&,qpG(1,5), qpG(2,5), qpG(3,5)
     end if
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
         do nn=2,5
           if (v_is_zero(qpG_trial-qpG(:,nn))) then
             idx_q(nn,iq1,ig1) = iq_ibz
             idx_G(nn,iq1,ig1) = ig2
             idx_is(nn,iq1,ig1) = is
             find = find + 1
           end if
         end do
         !
         if (find == 4) exit
         !
       end do
       !
       if (find == 4) exit
       !
     end do
     !
     if (debug) then
       !Write indexes into a file
       write(out_unit_idx,"(I4,I4,I4,I4,I4,I4,I4,I4,I4,I4,I4,I4,I4,I4,I4,I4)") iq1 &
&,idx_q(1,iq1,ig1),idx_is(1,iq1,ig1),idx_G(1,iq1,ig1)&
&,idx_q(2,iq1,ig1),idx_is(2,iq1,ig1),idx_G(2,iq1,ig1)&
&,idx_q(3,iq1,ig1),idx_is(3,iq1,ig1),idx_G(3,iq1,ig1)&
&,idx_q(4,iq1,ig1),idx_is(4,iq1,ig1),idx_G(4,iq1,ig1)&
&,idx_q(5,iq1,ig1),idx_is(5,iq1,ig1),idx_G(5,iq1,ig1)

       !Check if nearest neighbours are obtained correctly
       qpG(:,2) = matmul(rl_sop(:,:,idx_is(2,iq1,ig1)),q%pt(idx_q(2,iq1,ig1),:)+g_vec(idx_G(2,iq1,ig1),:))
       qpG(:,3) = matmul(rl_sop(:,:,idx_is(3,iq1,ig1)),q%pt(idx_q(3,iq1,ig1),:)+g_vec(idx_G(3,iq1,ig1),:))
       qpG(:,4) = matmul(rl_sop(:,:,idx_is(4,iq1,ig1)),q%pt(idx_q(4,iq1,ig1),:)+g_vec(idx_G(4,iq1,ig1),:))
       qpG(:,5) = matmul(rl_sop(:,:,idx_is(5,iq1,ig1)),q%pt(idx_q(5,iq1,ig1),:)+g_vec(idx_G(5,iq1,ig1),:))
       !
       !From iku to cc
       call c2a(b_in=b,v_in=qpG(:,1),mode='ka2c')
       call c2a(b_in=b,v_in=qpG(:,2),mode='ki2c')
       call c2a(b_in=b,v_in=qpG(:,3),mode='ki2c')
       call c2a(b_in=b,v_in=qpG(:,4),mode='ki2c')
       call c2a(b_in=b,v_in=qpG(:,5),mode='ki2c')
       !Write qpG out into a file
       write(out_unit_qpG_out,"(I4,I4,F7.3,F7.3,F7.3,F7.3,F7.3,F7.3,F7.3,F7.3,F7.3,F7.3,F7.3,F7.3,F7.3,F7.3,F7.3)")&
& iq1,ig1,qpG(1,1), qpG(2,1), qpG(3,1),qpG(1,2), qpG(2,2), qpG(3,2)&
&,qpG(1,3), qpG(2,3), qpG(3,3),qpG(1,4), qpG(2,4), qpG(3,4)&
&,qpG(1,5), qpG(2,5), qpG(3,5)
     end if
     !
   end do
   !
 end do
 !
 !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 !Load of X_mat
 !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 !
 ng_save = X%ng
 iq_to_start = 1 !This variable will be used in the future to parallelize the code
 !Initialize G_max_ibz to RIM_ng
 forall (iq=1:q%nibz) G_max_ibz(iq) = RIM_ng
 !
 !Look if G_max_ibz must be increased
 do iq=1,q%nibz
   do ig=1,RIM_ng
     do nn=2,5
       if (G_max_ibz(idx_q(nn,iq,ig)) < idx_G(nn,iq,ig)) then
         G_max_ibz(idx_q(nn,iq,ig)) = idx_G(nn,iq,ig)
       end if
     end do
   end do
 end do
 call msg('r', 'G_max_used =', G_max_ibz(:))
 !Print G vectors into a file
 if (debug) then
   do ig=1,maxval(G_max_ibz)
     v1 = g_vec(ig,:)
     call c2a(b_in=b,v_in=v1,mode='ki2a')
     write(out_unit_G,"(I3,E15.6,E15.6,E15.6)") ig,v1(1), v1(2), v1(3)
   end do
 end if
 do iq=1,q%nibz
   !
   !
   ! I/O of X
   !
   !Load only the G needed
   X%ng = G_max_ibz(iq)
   !
   if (iq==iq_to_start) then
     call io_control(ACTION=OP_RD,COM=NONE,       SEC=(/1,2,2*iq+1/),ID=ID)
   else if (q%nibz==1) then
     call io_control(ACTION=OP_RD_CL,COM=NONE,    SEC=(/1,2,3/),ID=ID)
   else if (iq > iq_to_start) then
     call io_control(ACTION=RD_CL_IF_END,COM=NONE,SEC=(/2*iq,2*iq+1/),ID=ID)
   endif
   !
   io_err=io_X(X,Xw,ID)
   !
   call deliver_IO_error_message(io_err,'PP/Em1s',STOP_it=.TRUE.)
   !
   !Assign X
   do ig1=1,RIM_ng
     !
     do iq1=1,q%nibz
       !
       !Assign X
       do nn=1,5
         !
         if (idx_q(nn,iq1,ig1) == iq) then
           vX(nn,iq1,ig1) = X_mat(idx_G(nn,iq1,ig1),idx_G(nn,iq1,ig1),1)
         end if
         !
       end do
       !
     end do
     !
   end do
   !
   call X_alloc('X')
 end do
 !
 !Print vX into a file
 if (debug) then
   do ig1=1,RIM_ng
     !
     do iq1=1,q%nibz
       write(out_unit_vX,"(I4,I4,I4,E15.6,I4,I4,E15.6,I4,I4,E15.6,I4,I4,E15.6,I4,I4,E15.6)") iq1,&
& idx_q(1,iq1,ig1),idx_G(1,iq1,ig1),real(vX(1,iq1,ig1)),&
& idx_q(2,iq1,ig1),idx_G(2,iq1,ig1),real(vX(2,iq1,ig1)),&
& idx_q(3,iq1,ig1),idx_G(3,iq1,ig1),real(vX(3,iq1,ig1)),&
& idx_q(4,iq1,ig1),idx_G(4,iq1,ig1),real(vX(4,iq1,ig1)),&
& idx_q(5,iq1,ig1),idx_G(5,iq1,ig1),real(vX(5,iq1,ig1))
     end do
   end do
 end if
 X%ng = ng_save 
 !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 !Calculation of the interpolation coefficients
 !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 !
 do ig1=1,RIM_ng
  !
  do iq1=1,q%nibz
    !
    !Evaluate f_func
    do nn=1,5
      !
      !Select the index of the reference/n.n.
      iq2 = idx_q(nn,iq1,ig1)
      ig2 = idx_G(nn,iq1,ig1)
      !
      f_func(nn) = bare_qpg(iq2,ig2)**2/(4*pi)*vX(nn,iq1,ig1)/(vX(nn,iq1,ig1)-1)
      !
    end do
    !Store coefficients
    f_coeff (1,iq1,ig1) = f_func(1)                                                     !f0
    f_coeff (2,iq1,ig1) = (f_func(3)-f_func(5))/(2*v_norm(delta_q(:,1)))                !f1x
    f_coeff (3,iq1,ig1) = (f_func(2)-f_func(4))/(2*v_norm(delta_q(:,2)))                !f1y
    f_coeff (4,iq1,ig1) = (f_func(3)-2*f_func(1)+f_func(5))/(2*v_norm(delta_q(:,1))**2) !f2xx
    f_coeff (5,iq1,ig1) = (f_func(2)-2*f_func(1)+f_func(4))/(2*v_norm(delta_q(:,2))**2) !f2yy
    !
    !Store coefficients into file
    if (debug) then
      write(out_unit_f_coeff,"(I4,I4,E15.6,E15.6,E15.6,E15.6,E15.6)") iq1, ig1 &
&,f_coeff (1,iq1,ig1),f_coeff (2,iq1,ig1),f_coeff (3,iq1,ig1),f_coeff (4,iq1,ig1),f_coeff (5,iq1,ig1)
    end if
  end do
  !
 end do
 !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 !Calculation of RIM-W
 !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 !
 !Here, we need to check if we want to perform the RIM
 !of bare_qpg or the RIM of cutoff_slab
 !
 call STRING_split(cut_geometry,str_piece)
 !
 cut_is_slab=trim(str_piece(1))=='slab'
 !
 ! Cutoffed directions
 !
 is_cut(1)=index(str_piece(2),'x')/=0.or.index(str_piece(2),'X')/=0
 is_cut(2)=index(str_piece(2),'y')/=0.or.index(str_piece(2),'Y')/=0
 is_cut(3)=index(str_piece(2),'z')/=0.or.index(str_piece(2),'Z')/=0
 !
 call io_control(ACTION=OP_RD_CL,COM=REP,MODE=VERIFY,SEC=(/1,2/),ID=ID)
 io_err=io_RIM_W(ID)
 !
 if (io_err/=0) then
   call rim('c',cut_is_slab,is_cut,f_coeff)
   call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1,2/),ID=ID)
   io_err=io_RIM_W(ID)
 end if
 call msg('r', "shape(RIM_W) =", shape(RIM_W))
 call msg('r', "RIM_W =", RIM_W(:,1,1))

end subroutine





