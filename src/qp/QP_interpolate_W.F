!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AG
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
subroutine interpolate_W(X,Xw,q)
 !
 use pars,          ONLY:SP,pi,zero_dfl,schlen
 use com,           ONLY:msg, warning
 use R_lattice,     ONLY:bz_samp,RIM_ng,g_vec,b,k_grid_b,ng_vec,rl_sop,bare_qpg,&
&                        RIM_W_is_diagonal,RIM_W,f_coeff,cut_is_slab,idir
 use memory,        ONLY:MEM_err
 use vec_operate,   ONLY:c2a,v_is_zero,v_norm
 use X_m,           ONLY:X_mat,X_t,X_alloc
 use frequency,     ONLY:w_samp
 use IO_m,          ONLY:io_control,OP_RD_CL,NONE,RD_CL,OP_RD,RD_CL_IF_END,&
&                        deliver_IO_error_message,OP_WR_CL,REP,VERIFY
 use stderr,        ONLY:STRING_split
 use D_lattice,     ONLY:alat,a
 use timing_m,      ONLY:timing
 !
 implicit none
 type(X_t)    ::X
 type(w_samp) ::Xw
 type(bz_samp):: q
 !
 ! Work Space
 !
 integer     :: iq1,ig1,iq2,ig2,iq,ig,igr,igc,ig2max
 integer     :: iq_ibz,is,find,nn,i,iq_to_start,idm,ng_tmp
 integer     :: idx_q(RIM_ng,q%nibz,5),idx_G(RIM_ng,q%nibz,5),idx_is(RIM_ng,q%nibz,5)
 real(SP)    :: f_func(5),lcut, v(5), D_det
 complex(SP) :: vX(5,q%nibz,RIM_ng,RIM_ng)
 real(SP)    :: qpG(3,5),delta_q(3,2),qpG_trial(3),v1(3),delta_q_cc(3),gr_2d_mod,gc_2d_mod
 integer     :: ID, io_err, ng_save,G_max_ibz(q%nibz)
 character(schlen) :: str_piece(5)
 integer, external :: io_X
 integer, external :: io_RIM_W
 !
!!! AG DBG
 logical :: debug
 integer :: out_unit_G=100, out_unit_qpG_in=101,out_unit_qpG_out=102,out_unit_idx=103,out_unit_vX=104
 integer :: out_unit_f_coeff=105
!!! AG DBG
 !
 !
 debug = .false.
 call timing('RIM-W-coeff',OPR='start')
 call k_ibz2bz(q,'a',.true.)
 allocate(f_coeff(RIM_ng,RIM_ng,q%nibz,6),stat=MEM_err)
 !
!!! AG DBG
 if (debug) then
   open (unit=out_unit_G,file="G-vec.dat",action="write",status="replace")
   open (unit=out_unit_qpG_in,file="qpG_in.dat",action="write",status="replace")
   open (unit=out_unit_qpG_out,file="qpG_out.dat",action="write",status="replace")
   open (unit=out_unit_idx,file="idx.dat",action="write",status="replace")
   open (unit=out_unit_vX,file="vX.dat",action="write",status="replace")
   open (unit=out_unit_f_coeff,file="f_coeff.dat",action="write",status="replace")
 end if
!!! AG DBG
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
     delta_q(:,2) = abs(v1)
   else if (abs(v1(2)) < zero_dfl) then
     delta_q(:,1) = abs(v1)
   else
     call msg('r', 'Error in the selection of Delta_q')
   end if
 end do
 !
!!! AG DBG
 if (debug) call msg('r', "delta_q(1) =", delta_q(:,1))
 if (debug) call msg('r', "delta_q(2) =", delta_q(:,2))
!!! AG DBG
 !
 !Cycles over qpG points
 do iq1=1,q%nibz
   !
   do ig1=1,RIM_ng
     !
     find = 0
     !
     idx_q(ig1,iq1,1) = iq1
     idx_is(ig1,iq1,1) = 1
     idx_G(ig1,iq1,1) = ig1
     !
     !Calculate qpG and qpG nearest neighbour
     qpG(:,1) = matmul(rl_sop(:,:,idx_is(ig1,iq1,1)),q%pt(idx_q(ig1,iq1,1),:)+g_vec(ig1,:))
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
!!! AG DBG
     if (debug) then
       !Print qpG into a file
       write(out_unit_qpG_in,"(I4,I4,F7.3,F7.3,F7.3,F7.3,F7.3,F7.3,F7.3,F7.3,F7.3,F7.3,F7.3,F7.3,F7.3,F7.3,F7.3)")&
&            iq1,ig1,qpG(1,1), qpG(2,1), qpG(3,1),qpG(1,2), qpG(2,2), qpG(3,2),&
&            qpG(1,3), qpG(2,3), qpG(3,3),qpG(1,4), qpG(2,4), qpG(3,4),&
&            qpG(1,5), qpG(2,5), qpG(3,5)
     endif
!!! AG DBG
     !
     !Find nn indexes
     do i=1,ng_vec
       !
       !Try first ig2 near ig1
       if (i < 2*ig1) then
         ig2 = (2*(i-2*(i/2))-1)*(i/2) + ig1
       else
         ig2 = i
       endif
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
             idx_q(ig1,iq1,nn) = iq_ibz
             idx_G(ig1,iq1,nn) = ig2
             idx_is(ig1,iq1,nn) = is
             find = find + 1
           endif
         enddo
         !
         if (find == 4) exit
         !
       enddo
       !
       if (find == 4) exit
       !
     enddo
     !
!!! AG DBG
     if (debug) then
       !Write indexes into a file
       write(out_unit_idx,"(I4,I4,I4,I4,I4,I4,I4,I4,I4,I4,I4,I4,I4,I4,I4,I4)") iq1, &
&            idx_q(ig1,iq1,1),idx_is(ig1,iq1,1),idx_G(ig1,iq1,1),&
&            idx_q(ig1,iq1,2),idx_is(ig1,iq1,2),idx_G(ig1,iq1,2),&
&            idx_q(ig1,iq1,3),idx_is(ig1,iq1,3),idx_G(ig1,iq1,3),&
&            idx_q(ig1,iq1,4),idx_is(ig1,iq1,4),idx_G(ig1,iq1,4),&
&            idx_q(ig1,iq1,5),idx_is(ig1,iq1,5),idx_G(ig1,iq1,5)

       !Check if nearest neighbours are obtained correctly
       qpG(:,2) = matmul(rl_sop(:,:,idx_is(ig1,iq1,2)),q%pt(idx_q(ig1,iq1,2),:)+g_vec(idx_G(ig1,iq1,2),:))
       qpG(:,3) = matmul(rl_sop(:,:,idx_is(ig1,iq1,3)),q%pt(idx_q(ig1,iq1,3),:)+g_vec(idx_G(ig1,iq1,3),:))
       qpG(:,4) = matmul(rl_sop(:,:,idx_is(ig1,iq1,4)),q%pt(idx_q(ig1,iq1,4),:)+g_vec(idx_G(ig1,iq1,4),:))
       qpG(:,5) = matmul(rl_sop(:,:,idx_is(ig1,iq1,5)),q%pt(idx_q(ig1,iq1,5),:)+g_vec(idx_G(ig1,iq1,5),:))
       !
       !From iku to rlu
       !call c2a(b_in=b,v_in=qpG(:,1),mode='ka2c')
       call c2a(b_in=b,v_in=qpG(:,2),mode='ki2a')
       call c2a(b_in=b,v_in=qpG(:,3),mode='ki2a')
       call c2a(b_in=b,v_in=qpG(:,4),mode='ki2a')
       call c2a(b_in=b,v_in=qpG(:,5),mode='ki2a')
       !
       !Write qpG out into a file
       write(out_unit_qpG_out,"(I4,I4,F7.3,F7.3,F7.3,F7.3,F7.3,F7.3,F7.3,F7.3,F7.3,F7.3,F7.3,F7.3,F7.3,F7.3,F7.3)")&
&            iq1,ig1,qpG(1,1), qpG(2,1), qpG(3,1),qpG(1,2), qpG(2,2), qpG(3,2), &
&            qpG(1,3), qpG(2,3), qpG(3,3),qpG(1,4), qpG(2,4), qpG(3,4), &
&            qpG(1,5), qpG(2,5), qpG(3,5)
     endif
!!! AG DBG
     !
   enddo
   !
 enddo
 !
 !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 !Load of X_mat
 !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 !
 ng_save = X%ng
 !
 ! This variable will be used in the future to parallelize the code
 iq_to_start = 1 
 !
 !Initialize G_max_ibz to RIM_ng
 G_max_ibz(1:q%nibz) = RIM_ng
 !
 !Look if G_max_ibz must be increased
 do iq=1,q%nibz
   do ig=1,RIM_ng
     do nn=2,5
       if (G_max_ibz(idx_q(ig,iq,nn)) < idx_G(ig,iq,nn)) then
         G_max_ibz(idx_q(ig,iq,nn)) = idx_G(ig,iq,nn)
       endif
     enddo
   enddo
 enddo
 !
 call msg('r', 'G_max_used =', G_max_ibz(:))
 !
!!! AG DBG
 !Print G vectors into a file
 if (debug) then
   do ig=1,maxval(G_max_ibz)
     v1 = g_vec(ig,:)
     call c2a(b_in=b,v_in=v1,mode='ki2a')
     write(out_unit_G,"(I3,E15.6,E15.6,E15.6)") ig,v1(1), v1(2), v1(3)
   enddo
 endif
!!! AG DBG
 !
 do iq=1,q%nibz
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
     ig2max = RIM_ng
     if (RIM_W_is_diagonal) ig2max=ig1
     do iq1=1,q%nibz
     do ig2=ig1,ig2max 
       !
       do nn=1,5
         !
         if (idx_q(ig1,iq1,nn) == iq) then
           vX(nn,iq1,ig1,ig2) = X_mat(idx_G(ig1,iq1,nn),idx_G(ig2,iq1,nn),1)
         endif
         !
       enddo
       !
     enddo
     enddo
   enddo
   !
   call X_alloc('X')
 enddo
 !
!!! AG DBG
 !Print vX into a file
 if (debug) then
   do ig1=1,RIM_ng
     !
     ig2max = RIM_ng
     if (RIM_W_is_diagonal) ig2max=ig1
     do ig2=ig1,ig2max
       !
       do iq1=1,q%nibz
         write(out_unit_vX,"(7I4,E15.6,4I4,E15.6,4I4,E15.6,4I4,E15.6,4I4,E15.6)") &
&              iq1, ig1, ig2,&
&              idx_q(ig1,iq1,1),idx_G(ig1,iq1,1),idx_q(ig2,iq1,1),idx_G(ig2,iq1,1),real(vX(1,iq1,ig1,ig2)),&
&              idx_q(ig1,iq1,2),idx_G(ig1,iq1,2),idx_q(ig2,iq1,2),idx_G(ig2,iq1,2),real(vX(2,iq1,ig1,ig2)),&
&              idx_q(ig1,iq1,3),idx_G(ig1,iq1,3),idx_q(ig2,iq1,3),idx_G(ig2,iq1,3),real(vX(3,iq1,ig1,ig2)),&
&              idx_q(ig1,iq1,4),idx_G(ig1,iq1,4),idx_q(ig2,iq1,4),idx_G(ig2,iq1,4),real(vX(4,iq1,ig1,ig2)),&
&              idx_q(ig1,iq1,5),idx_G(ig1,iq1,5),idx_q(ig2,iq1,5),idx_G(ig2,iq1,5),real(vX(5,iq1,ig1,ig2))
       enddo
     enddo
   enddo
 endif
!!! AG DBG
 !
 X%ng = ng_save 
 !
 !Check if RIM-W has been alreay obtained
 call io_control(ACTION=OP_RD_CL,COM=REP,MODE=VERIFY,SEC=(/1,2/),ID=ID)
 io_err=io_RIM_W(ID)
 !
 if (io_err==0) then
   call timing('RIM-W-coeff',OPR='stop')
   return
 end if
 !
 !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 !Calculation of the interpolation coefficients
 !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 !
 D_det = a(1,1)*a(2,2)-a(1,2)*a(2,1)
 !
 do igr=1,RIM_ng
  !
  gr_2D_mod = sqrt(g_vec(igr,1)**2+g_vec(igr,2)**2)
  !
  ig2max = RIM_ng
  if (RIM_W_is_diagonal) ig2max=igr
  do igc=igr,ig2max
    !
    gc_2D_mod = sqrt(g_vec(igc,1)**2+g_vec(igc,2)**2)
    !
    do iq1=1,q%nibz
      !
      !Evaluate f_func
      do nn=1,5
        !
        !Select the index of the reference/n.n.
        iq2 = idx_q(igr,iq1,nn)
        ig1 = idx_G(igr,iq1,nn)
        ig2 = idx_G(igc,iq1,nn)
        !
        f_func(nn) = bare_qpg(iq2,ig1)*bare_qpg(iq2,ig2)/(4*pi)*vX(nn,iq1,igr,igc)/(vX(nn,iq1,igr,igc)+1)
        !
      enddo
      !
      ! q == 0 terms must be tratened separately
      if (iq1 == 1 .and. igr == 1 .and. igc == 1) then
        !
        f_coeff(igr,igc,iq1,1) = vX(1,iq1,igr,igc)*(4*pi) / &
&                                ( bare_qpg(iq1,igr)*bare_qpg(iq1,igc)*(2*pi*alat(idir(1)))**2 )
        !
        do nn=2,3
          idm = MOD(nn+1,2)+1
          call c2a(b,delta_q(:,idm),delta_q_cc,'ka2c')
          f_coeff(igr,igc,iq1,nn) = -1._SP/delta_q(idm,idm)*log(f_func(nn)/ &
&                                 ( v_norm(delta_q_cc)**2*f_coeff(1,iq1,igr,igc)) )
        enddo
        !
      else
        !
        !Coefficients in rlu
        v(1) = f_func(1)                                             !f0
        v(2) = (f_func(3)-f_func(5))/(2*delta_q(1,1))                !f1
        v(3) = (f_func(2)-f_func(4))/(2*delta_q(2,2))                !f2
        v(4) = (f_func(3)-2*f_func(1)+f_func(5))/(2*delta_q(1,1)**2) !f11
        v(5) = (f_func(2)-2*f_func(1)+f_func(4))/(2*delta_q(2,2)**2) !f22
        !
        !Coefficients in iku
        f_coeff(igr,igc,iq1,1) =  v(1)                                                                       !f
        f_coeff(igr,igc,iq1,2) =  (a(1,1)*v(2)*alat(2)+a(2,1)*v(3)*alat(1))/(D_det)                          !fx
        f_coeff(igr,igc,iq1,3) =  (a(1,2)*v(2)*alat(2)+a(2,2)*v(3)*alat(1))/(D_det)                          !fy
        f_coeff(igr,igc,iq1,4) =  (a(1,1)*a(1,1)*v(4)*alat(2)**2+a(2,1)*a(2,1)*v(5)*alat(1)**2)/(D_det)**2   !fxx
        f_coeff(igr,igc,iq1,5) =  (a(1,1)*a(1,2)*v(4)*alat(2)**2+a(2,1)*a(2,2)*v(5)*alat(1)**2)/(D_det)**2   !fxy
        f_coeff(igr,igc,iq1,6) =  (a(1,2)*a(1,2)*v(4)*alat(2)**2+a(2,2)*a(2,2)*v(5)*alat(1)**2)/(D_det)**2   !fyy
      endif
      !
!!! AG DBG
      !Store coefficients into file
      if (debug) then
        write(out_unit_f_coeff,"(I4,I4,I4,E15.6,E15.6,E15.6,E15.6,E15.6,E15.6)") iq1, igr, igc &
&,f_coeff (igr,igc,iq1,1),f_coeff (igr,igc,iq1,2),f_coeff (igr,igc,iq1,3)&
&,f_coeff (igr,igc,iq1,4),f_coeff (igr,igc,iq1,5),f_coeff (igr,igc,iq1,6)
      endif
!!! AG DBG
      !
    enddo
  enddo
 enddo
 call timing('RIM-W-coeff',OPR='stop')
 !
 !if (debug) call check_W_interpolation(q,f_coeff,delta_q,vX,idx_q,idx_is,idx_G)
 !
 !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 !Calculation of RIM-W
 !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 !
 call rim('c')
 call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1,2/),ID=ID)
 io_err=io_RIM_W(ID)
 call msg('r', "shape(RIM_W) =", shape(RIM_W))
 call msg('r', "RIM_W =", RIM_W(:,1,1))
 !
end subroutine

subroutine check_W_interpolation(q,f_coeff,delta_q,vX,idx_q,idx_is,idx_G)
 !
 use pars,          ONLY:SP,pi,zero_dfl,schlen,DP
 use com,           ONLY:msg
 use R_lattice,     ONLY:bz_samp,RIM_ng,g_vec,b,k_grid_b,ng_vec,rl_sop,bare_qpg,&
&                        RIM_W,q0_def_norm,cut_is_slab,idir
 use memory,        ONLY:MEM_err
 use vec_operate,   ONLY:c2a,v_is_zero,v_norm,iku_v_norm
 use X_m,           ONLY:X_mat,X_t,X_alloc
 use frequency,     ONLY:w_samp
 use IO_m,          ONLY:io_control,OP_RD_CL,NONE,RD_CL,OP_RD,RD_CL_IF_END,&
&                        deliver_IO_error_message,OP_WR_CL,REP,VERIFY
 use stderr,        ONLY:STRING_split
 use D_lattice,     ONLY:alat
 !
 implicit none
 type(bz_samp) :: q
 real(SP)      :: f_coeff(RIM_ng,RIM_ng,q%nibz,6)
 real(SP)      :: delta_q(3,2)
 complex(SP)   :: vX(5,q%nibz,RIM_ng,RIM_ng)
 integer       :: idx_q(RIM_ng,q%nibz,5),idx_G(RIM_ng,q%nibz,5),idx_is(RIM_ng,q%nibz,5)
 !
 ! Work Space
 !
 integer  :: out_unit_W_x_intp=200, out_unit_W_x_num=300,out_unit_W_y_intp=400, out_unit_W_y_num=500
 integer  :: Np=1001, i, ig1, ig2, iqibz, is, gmax, nn, igr, igc, pos
 real(SP) :: dq, q_sampl(3), func, lcut,slab_vz1, slab_vplane1,slab_vz2, &
&            slab_vplane2, W_sampl,pre_factor,r1,vslab
 real(SP) :: g1_2D_mod, g2_2D_mod, qpG1(3), qpG2(3), idx_q1, q_out(3)
 character(schlen):: str_piece1, str_piece2
 !
 dq = 1./(Np-1)
 q_sampl = (/0.,0.,0./)
 gmax = 13
 lcut=alat(idir(1))/2._SP
 !
 !Print to file interpolated W data
 do igr=1,gmax
   !
   do igc=igr,gmax
     !
     write(str_piece1, '(I2.2)') igr
     write(str_piece2, '(I2.2)') igc 
     open (unit=out_unit_W_y_intp,file="W_y_intp_g"//trim(str_piece1)//"_g"//trim(str_piece2)//".dat",&
&          action="write",status="replace")
     open (unit=out_unit_W_y_num,file="W_y_num_g"//trim(str_piece1)//"_g"//trim(str_piece2)//".dat",&
&          action="write",status="replace")
     !
     !Evaluate W along y
     do nn = 1,3
       !
       !Map to the nn position
       !pos = 3*nn**2-13*nn+15
       !
       pos = 2*nn**2-9*nn+11
       call msg("r", "pos =", pos)
       !
       !Get the index that indentify the qpG(G') point
       iqibz = idx_q(igr,1,pos)
       is = idx_is(igr,1,pos)
       ig1 = idx_G(igr,1,pos)
       ig2 = idx_G(igc,1,pos)
       !
       qpG1 = matmul(rl_sop(:,:,is),q%pt(iqibz,:)+g_vec(ig1,:))
       qpG2 = matmul(rl_sop(:,:,is),q%pt(iqibz,:)+g_vec(ig2,:))
       !
       g1_2D_mod = sqrt(g_vec(ig1,1)**2+g_vec(ig1,2)**2)
       g2_2D_mod = sqrt(g_vec(ig2,1)**2+g_vec(ig2,2)**2)
       !
       !-----------------------------
       !Print to file numerical data-
       !-----------------------------
       !Evaluate vslab
       vslab = 4*pi/bare_qpg(iqibz,ig1)/bare_qpg(iqibz,ig2)
       !Evaluate func
       func = vX(pos,1,igr,igc)/vslab/(vX(pos,1,igr,igc)+1)
       !Evaluate W
       W_sampl = -vX(pos,1,igr,igc)*vslab
       !Get q output
       q_out = qpG1!-g_vec(ig1,:)
       call c2a(b_in=b,v_in=q_out,mode='ki2c')      
       write(out_unit_W_y_num,"(I5, E15.6, E15.6, E15.6, E15.6, E15.6, E15.6)") &
&                         iqibz, q_out(1),q_out(2),q_out(3), func, vslab, W_sampl


       !--------------------------
       !Print to file fitted data-
       !--------------------------
       if (pos == 4) cycle
       do i=1,Np
         !
         !q_sampl in rlu
         q_sampl = (/0.,0.,0./)
         q_sampl(2) = 1.0*(dq*(i-1)-0.5)*abs(delta_q(2,2))
         !
         !Convert q_sampl to iku
         call c2a(b_in=b,v_in=q_sampl,mode='ka2i')
         !
         !Evaluate v_slab
         r1 = 1.
         if (iku_v_norm(qpG1+q_sampl) < q0_def_norm) then
           r1 = r1*q0_def_norm
         else
           r1 = r1*iku_v_norm(qpG1+q_sampl)
         end if
         if (iku_v_norm(qpG2+q_sampl) < q0_def_norm) then
           r1 = r1*q0_def_norm
         else
           r1 = r1*iku_v_norm(qpG2+q_sampl)
         end if
         !r1=iku_v_norm(qpG1+q_sampl)*iku_v_norm(qpG2+q_sampl)
         !kz
         slab_vz1=(q_sampl(idir(1))+qpG1(idir(1)))*2.*pi/alat(idir(1))
         slab_vz2=(q_sampl(idir(1))+qpG2(idir(1)))*2.*pi/alat(idir(1))
         !kxy
         slab_vplane1=sqrt(((q_sampl(idir(2))+qpG1(idir(2)))*2.*pi/alat(idir(2)))**2+&
&                          ((q_sampl(idir(3))+qpG1(idir(3)))*2.*pi/alat(idir(3)))**2)
         !     
         slab_vplane2=sqrt(((q_sampl(idir(2))+qpG2(idir(2)))*2.*pi/alat(idir(2)))**2+&
&                          ((q_sampl(idir(3))+qpG2(idir(3)))*2.*pi/alat(idir(3)))**2)
         !
         if (slab_vplane1 < q0_def_norm) slab_vplane1 = q0_def_norm
         if (slab_vplane2 < q0_def_norm) slab_vplane2 = q0_def_norm
         !
         pre_factor = sqrt(1.-exp(-slab_vplane1*lcut)*cos(slab_vz1*lcut))*&
&                     sqrt(1.-exp(-slab_vplane2*lcut)*cos(slab_vz2*lcut))
         !        
         vslab=4._DP*pi*pre_factor
         !
         !Evaluate func and W
         if (iqibz == 1) then
           if (ig1 == 1 .and. ig2 == 1) then
             !
             if (iku_v_norm(q_sampl) < 1.e-5) then
               W_sampl = f_coeff(ig1,ig2,iqibz,1)*(2*pi*alat(idir(1)))**2
               q_out = qpG1-g_vec(ig1,:)+q_sampl
               call c2a(b_in=b,v_in=q_out,mode='ki2c') 
               write(out_unit_W_y_intp,"(I5, E15.6, E15.6, E15.6, E15.6, E15.6, E15.6)") &
&                           iqibz, q_out(1),q_out(2),q_out(3), 0.0, 0.0, W_sampl
               cycle
             end if
             !
             func = f_coeff(1,1,1,1)*exp(-f_coeff(1,1,1,2)*iku_v_norm(q_sampl))
             W_sampl = vslab*func*vslab/r1/(1-vslab*func)
             func = func*iku_v_norm(q_sampl)**2
             !
           else if ((ig1 == 1 .and. g2_2D_mod < 1.e-5) .or. (g1_2D_mod < 1.e-5 .and. ig2 == 1)) then
             !
             func = f_coeff(ig1,ig2,iqibz,2)*iku_v_norm(q_sampl)**2
             W_sampl = vslab/r1*func*vslab/r1/(1-vslab/r1*func)
             !
           else
             !
             !Convert q_sampl to rlu
             call c2a(b_in=b,v_in=q_sampl,mode='ki2a')
             func = f_coeff(ig1,ig2,iqibz,1)+q_sampl(1)*f_coeff(ig1,ig2,iqibz,2)+q_sampl(1)*f_coeff(ig1,ig2,iqibz,4)*q_sampl(1)&
&                                           +q_sampl(2)*f_coeff(ig1,ig2,iqibz,3)+q_sampl(2)*f_coeff(ig1,ig2,iqibz,5)*q_sampl(2)
             W_sampl = vslab/r1*func*vslab/r1/(1-vslab/r1*func)
             !
             !
             !Convert q_sampl to iku
             call c2a(b_in=b,v_in=q_sampl,mode='ka2i')
             !
           end if
           !
         else
           !
           !Convert q_sampl to rlu
           call c2a(b_in=b,v_in=q_sampl,mode='ki2a')
           func = f_coeff(ig1,ig2,iqibz,1)+q_sampl(1)*f_coeff(ig1,ig2,iqibz,2)+q_sampl(1)*f_coeff(ig1,ig2,iqibz,4)*q_sampl(1)&
&                                         +q_sampl(2)*f_coeff(ig1,ig2,iqibz,3)+q_sampl(2)*f_coeff(ig1,ig2,iqibz,5)*q_sampl(2)
           W_sampl = vslab/r1*func*vslab/r1/(1-vslab/r1*func)
           !
           !Convert q_sampl to iku
           call c2a(b_in=b,v_in=q_sampl,mode='ka2i')
           !
         end if
         !
         !Get the q output
         q_out = qpG1+q_sampl!-g_vec(ig1,:)
         call c2a(b_in=b,v_in=q_out,mode='ki2c')      
         write(out_unit_W_y_intp,"(I5, E15.6, E15.6, E15.6, E15.6, E15.6, E15.6)") &
&                           iqibz, q_out(1),q_out(2),q_out(3), func, vslab/r1, -W_sampl

       enddo
     enddo
   enddo
 enddo
 close (unit=out_unit_W_y_num)
 close (unit=out_unit_W_y_intp)
end subroutine

