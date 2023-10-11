!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AG
!
subroutine QP_interpolate_W(X,Xw,q,mode)
 !
 use pars,          ONLY:SP,pi,zero_dfl,schlen,DP,rZERO
 use com,           ONLY:msg
 use R_lattice,     ONLY:bz_samp,RIM_W_ng,b,k_grid_b,bare_qpg,&
&                        RIM_W_is_diagonal,RIM_W,RIM_W_d,f_coeff,idir,RIM_W_E,&
&                        RIM_id_epsm1_reference,RIM_epsm1,RIM_qpg
 use vec_operate,   ONLY:c2a,v_norm
 use X_m,           ONLY:X_ALLOC_elemental,X_mat,X_t
 use frequency,     ONLY:w_samp
 use IO_m,          ONLY:OP_RD_CL,NONE,RD_CL,OP_RD,RD_CL_IF_END,OP_WR_CL,&
&                        deliver_IO_error_message,manage_action,REP,VERIFY
 use IO_int,        ONLY:io_control
 use stderr,        ONLY:intc,STRING_pack,gen_fmt
 use D_lattice,     ONLY:alat,a
 use timing_m,      ONLY:timing
 use cuda_m,        ONLY:have_cuda
 use parallel_m,    ONLY:master_cpu
 !
#include<memory.h>
 !
 type(X_t)    :: X
 type(w_samp) :: Xw
 type(bz_samp):: q
 character(3) :: mode
 !
 ! Work Space
 !
 integer           :: iq1,ig1,iq2,ig2,iq,ig
 integer           :: nn,idm,igr,igc,ig2max,iomega
 integer           :: idx_q(RIM_W_ng,q%nibz,5),idx_G(RIM_W_ng,q%nibz,5),idx_is(RIM_W_ng,q%nibz,5)
 real(SP)          :: dummy(2),anis_factor,em1_anis(3)
 complex(DP)       :: f_func(5),dp_dummy(5)
 complex(DP)       :: vX_nn(5,q%nibz,RIM_W_ng,RIM_W_ng,Xw%n_freqs)
 real(SP)          :: v1(3),r_dum(2)
 real(SP)          :: q_grid_b_rlu(3,2),q_grid_b_iku(3,2),q_grid_b_cc(3,2)
 integer           :: ID, IO_ACT, io_err, ng_save,G_max_ibz(q%nibz), G_max
 integer, external :: io_X, io_RIM_W
 character(schlen) :: ch_dum,fmt_dum,msg_dum(2)
 !
 call timing('RIM-W-coeff',OPR='start')
 !
 YAMBO_ALLOC(f_coeff,(6,RIM_W_ng,RIM_W_ng,q%nibz,Xw%n_freqs))
 !
 call section("+", "RIM-W interpolation")
 !
 !Load q_grid_b in iku
 do idm=1,2
   v1 = k_grid_b(idm,:)
   call c2a(v_in=v1,mode='kc2a')
   !
   if (abs(v1(1)) < zero_dfl) then
     !
     q_grid_b_rlu(:,2) = abs(v1)
     !
   elseif (abs(v1(2)) < zero_dfl) then
     !
     q_grid_b_rlu(:,1) = abs(v1)
     !
   else
     call error('Error in loading q_min')
   endif
 enddo
 !
 !Get q_grid_b in iku and cc
 do idm=1,2
   call c2a(v_in=q_grid_b_rlu(:,idm),v_out=q_grid_b_iku(:,idm),mode='ka2i')
   call c2a(v_in=q_grid_b_rlu(:,idm),v_out=q_grid_b_cc (:,idm),mode='ka2c')
 enddo
 !
 call warning ('RIM-W works only for 2D semiconductors with slab cutoff')
 call msg('r', 'Found compatible q grid: 2D')
 call msg('r', 'q_min[ 1 ]', q_grid_b_rlu(:,1), '[rlu]')
 call msg('r', '          ', q_grid_b_iku(:,1), '[iku]')
 call msg('r', 'q_min[ 2 ]', q_grid_b_rlu(:,2), '[rlu]')
 call msg('r', '          ', q_grid_b_iku(:,2), '[iku]')
 call msg('r', 'G-vectors ', RIM_W_ng)
 !
 !Check if RIM-W has been alreay calculated
 call io_control(ACTION=OP_RD_CL,COM=REP,MODE=VERIFY,SEC=(/1,2/),ID=ID)
 io_err=io_RIM_W(ID,mode,Xw)
 !
 if (io_err==0) then
   call timing('RIM-W-coeff',OPR='stop')
   YAMBO_FREE(f_coeff)
   if (have_cuda) then
     YAMBO_ALLOC_SOURCE(RIM_W_d,RIM_W)
   endif
   return
 endif
 !
 !Calculation of the nearest neighbours indexes
 call find_q_nns(q,q_grid_b_iku,idx_q,idx_is,idx_G)
 !
 !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 !Load of X_mat
 !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 !
 ng_save = X%ng
 !
 !Initialize G_max_ibz to RIM_W_ng
 G_max_ibz(1:q%nibz) = RIM_W_ng
 !
 ! Look if G_max_ibz must be increased
 !
 do iq=1,q%nibz
   do ig=1,RIM_W_ng
     do nn=2,5
       if (G_max_ibz(idx_q(ig,iq,nn)) < idx_G(ig,iq,nn)) then
         G_max_ibz(idx_q(ig,iq,nn)) = idx_G(ig,iq,nn)
       endif
     enddo
   enddo
 enddo
 !
 G_max=maxval(G_max_ibz)
 !
 if (G_max > X%ng) call error(' G_max '//trim(intc(G_max))//&
&                             ' for RIM-W is higher than G_max '//trim(intc(X%ng))//' of vX')
 !
 call msg('r', 'G-vectors loaded', maxval(G_max_ibz))
 call msg('r', 'Number of interpolated frequencies', Xw%n_freqs)
 call msg('r','')
 !
 X%ng = G_max_ibz(1)
 call io_control(ACTION=OP_RD,COM=REP,SEC=(/1/),ID=ID)
 io_err=io_X(X,Xw,ID)
 !
 do iq=1,q%nibz
   !
   ! I/O of X
   !
   !Load only the G needed
   X%ng = G_max_ibz(iq)
   !
   IO_ACT=manage_action(RD_CL_IF_END,iq,1,q%nibz)
   call io_control(ACTION=IO_ACT,COM=NONE,SEC=(/2*iq,2*iq+1/),ID=ID)
   !
   io_err=io_X(X,Xw,ID)
   !
   call deliver_IO_error_message(io_err,'PP/Em1s',STOP_it=.TRUE.)
   !
   !Assign X
   do iomega=1,Xw%n_freqs
   do ig1=1,RIM_W_ng
     !
     ig2max = RIM_W_ng
     if (RIM_W_is_diagonal) ig2max=ig1
     do iq1=1,q%nibz
     do ig2=ig1,ig2max
       !
       do nn=1,5
         !
         if (idx_q(ig1,iq1,nn) == iq) then
           vX_nn(nn,iq1,ig1,ig2,iomega) = X_mat(idx_G(ig1,iq1,nn),idx_G(ig2,iq1,nn),iomega)
         endif
         !
       enddo
       !
     enddo
     enddo
   enddo
   enddo
   !
   call X_ALLOC_elemental('X')
   !
 enddo
 !
 X%ng = ng_save
 !
 !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 !Calculation of the interpolation coefficients
 !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 !
 !Anisotropy initialization
 em1_anis=RIM_epsm1(:)-1._SP
 !
 if (RIM_id_epsm1_reference<0.or.RIM_id_epsm1_reference>3) RIM_id_epsm1_reference=0
 if (RIM_id_epsm1_reference==0) em1_anis=0.
 !
 do iomega=1,Xw%n_freqs
 do igr=1,RIM_W_ng
  !
  ig2max = RIM_W_ng
  if (RIM_W_is_diagonal) ig2max=igr
  do igc=igr,ig2max
    !
    do iq1=1,q%nibz
      !
      ! Evaluate f_func nearest neighbour
      !
      do nn=1,5
        !
        !Select the index of the reference/n.n.
        iq2 = idx_q(igr,iq1,nn)
        ig1 = idx_G(igr,iq1,nn)
        ig2 = idx_G(igc,iq1,nn)
        !
        f_func(nn) = real(bare_qpg(iq2,ig1)*bare_qpg(iq2,ig2),kind=DP)/(4._DP*pi)*&
&                    vX_nn(nn,iq1,igr,igc,iomega)/(vX_nn(nn,iq1,igr,igc,iomega)+1._DP)
        !
      enddo
      !
      ! q == 0 terms must be tratened separately
      if (iq1 == 1 .and. igr == 1 .and. igc == 1) then
        !
        call msg("r","Evaluating coefficients q = 0")
        !
        if (RIM_id_epsm1_reference/=0 .and. iomega==1) then
          !
          !vX is included in the anis_factor
          call msg('r','This is the anysotropy case')
          f_coeff(1,igr,igc,iq1,iomega)=(4._DP*pi)/(bare_qpg(iq1,igr)*bare_qpg(iq1,igc)*(2._DP*pi*alat(idir(1)))**2)
          !
        else
          f_coeff(1,igr,igc,iq1,iomega) = vX_nn(1,iq1,igr,igc,iomega)*(4._DP*pi) / &
&                                  (bare_qpg(iq1,igr)*bare_qpg(iq1,igc)*(2._DP*pi*alat(idir(1)))**2)
        end if
        !
        do nn=2,3
          idm = MOD(nn+1,2)+1
          !
          if (RIM_id_epsm1_reference/=0 .and. iomega==1) then
            !
            anis_factor=dot_product(em1_anis,q_grid_b_cc(:,idm)**2)
            f_coeff(nn,igr,igc,iq1,iomega)=-1._DP/q_grid_b_rlu(idm,idm)*log(f_func(nn)/ &
&                                 (f_coeff(1,iq1,igc,iq1,iomega)*anis_factor))
          else
            f_coeff(nn,igr,igc,iq1,iomega)=-1._DP/q_grid_b_rlu(idm,idm)*log(f_func(nn)/ &
&                                 (v_norm(q_grid_b_cc(:,idm))**2*f_coeff(1,iq1,igc,iq1,iomega)))
          end if
          !
        enddo
        !
      else 
        !
        !call msg("r","Evaluating coefficients q/= 0")
        !
        !Coefficients in rlu
        dp_dummy(1) = f_func(1)                                                          !f0
        dp_dummy(2) = (f_func(3)-f_func(5))/(2._DP*q_grid_b_rlu(1,1))                    !f1
        dp_dummy(3) = (f_func(2)-f_func(4))/(2._DP*q_grid_b_rlu(2,2))                    !f2
        dp_dummy(4) = (f_func(3)-2._DP*f_func(1)+f_func(5))/(2._DP*q_grid_b_rlu(1,1)**2) !f11
        dp_dummy(5) = (f_func(2)-2._DP*f_func(1)+f_func(4))/(2._DP*q_grid_b_rlu(2,2)**2) !f22
        !
        !
        !Coefficients in iku
        !
        f_coeff(1,igr,igc,iq1,iomega) =  dp_dummy(1)                                           !f   (iku)
        f_coeff(2,igr,igc,iq1,iomega) =  dp_dummy(2)+dp_dummy(3)*a(2,1)/alat(1)                !fx  (iku)
        f_coeff(3,igr,igc,iq1,iomega) =  dp_dummy(3)+dp_dummy(2)*a(1,2)/alat(2)                !fy  (iku)
        f_coeff(4,igr,igc,iq1,iomega) =  dp_dummy(4)+dp_dummy(5)*(a(2,1)/alat(1))**2           !fxx (iku)
        f_coeff(5,igr,igc,iq1,iomega) =  dp_dummy(4)*a(1,2)/alat(2)+dp_dummy(5)*a(2,1)/alat(1) !fxy (iku)
        f_coeff(6,igr,igc,iq1,iomega) =  dp_dummy(5)+dp_dummy(4)*(a(1,2)/alat(2))**2           !fyy (iku)
        !
      endif
      !
      f_coeff(:,igc,igr,iq1,iomega) = f_coeff(:,igr,igc,iq1,iomega)
      !
    enddo
  enddo
 enddo
 enddo
 !
 call timing('RIM-W-coeff',OPR='stop')
 !
 !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 !Calculation of RIM-W integrals
 !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 !
 call rim('c',Xw)
 !
 !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 !Debugging output
 !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 !
 if(allocated(RIM_qpg)) then
   call msg('nr','Comparison between head Wc = W - v averages: < Wc > [au] RIM-W/RIM')
   do iq1=1,q%nibz,2
       !
       msg_dum=' '
       !
       do iq2=iq1,min(iq1+1,q%nibz)
         !
         ch_dum=' < -Wc [Q = '//trim(intc(iq2))//'] >'
         if (iq1/=iq2) ch_dum=' * < -Wc [Q = '//trim(intc(iq2))//'] >'
         !
         r_dum(1)= -RIM_W(1,iq2,1,1)*2._SP*pi
         r_dum(2)= -vX_nn(1,iq2,1,1,1)*RIM_qpg(iq2,1,1)*2_SP*pi
         !
         fmt_dum='(a,2('//trim(gen_fmt(r_v=r_dum))//'))'
         write (msg_dum(iq2-iq1+1),trim(fmt_dum)) trim(ch_dum),r_dum
         !
       enddo
       if (iq1==1) call msg('nr',trim(STRING_pack(msg_dum(1),msg_dum(2))))
       if (iq1/=1) call msg(' r',trim(STRING_pack(msg_dum(1),msg_dum(2))))
   enddo
   call msg('r','')
   call msg('nr','Comparison between head Wc = W - v averages: < Wc > [au] RIM-W/RIM')
   do ig1=1,RIM_W_ng,2
       !
       msg_dum=' '
       !
       do ig2=ig1,min(ig1+1,RIM_W_ng)
         !
         ch_dum=' < -Wc [G = '//trim(intc(ig2))//',G`=0] >'
         if (ig1/=ig2) ch_dum=' * < -Wc [G = '//trim(intc(ig2))//',G`=0] >'
         !
         r_dum(1)= -RIM_W(1,1,ig2,1)*2._SP*pi
         r_dum(2)= -vX_nn(1,1,ig2,1,1)*RIM_qpg(1,ig2,1)*2_SP*pi
         !
         fmt_dum='(a,2('//trim(gen_fmt(r_v=r_dum))//'))'
         write (msg_dum(ig2-ig1+1),trim(fmt_dum)) trim(ch_dum),r_dum
         !
       enddo
       if (ig1==1) call msg('nr',trim(STRING_pack(msg_dum(1),msg_dum(2))))
       if (ig1/=1) call msg(' r',trim(STRING_pack(msg_dum(1),msg_dum(2))))
   enddo

 end if
 !
 !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 !Calculation of RIM-W PPA energies (if needed)
 !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 !
 if (mode == 'PPA') then
   !
   YAMBO_ALLOC(RIM_W_E,(RIM_W_ng,RIM_W_ng))
   RIM_W_E = rZERO
   !
   do ig2=1,RIM_W_ng
     do ig1=1,RIM_W_ng
       !
       if(ig1==1.and.ig2==1) then
         !
         dummy(:) = f_coeff(1,1,1,1,:)
         if (RIM_id_epsm1_reference/=0) then
           dummy(1) = dummy(1)*0.5_SP*(em1_anis(idir(2))+em1_anis(idir(3)))
         end if
         !
       else
         !
         ! AF: a tentative implementation with a mixing constant of 1.E-5
         !     instead of 1.E-4 as below has been proposed, but led to
         !     numerical differencies in the results. To be investigated
         !
         dummy(:) = f_coeff(1,ig1,ig2,1,:)+1.E-4*(f_coeff(2,ig1,ig2,1,:)+f_coeff(3,ig1,ig2,1,:)&
&                                         +1.E-4*(f_coeff(4,ig1,ig2,1,:)+f_coeff(6,ig1,ig2,1,:)&
                                          +2._SP*f_coeff(5,ig1,ig2,1,:)))
         dummy(:) = dummy(:)*4._SP*pi/(bare_qpg(1,ig1)*bare_qpg(1,ig2))
         dummy(:) = dummy/(1._SP-dummy)
         !
       endif
       !
       if (dummy(1)/dummy(2)<=1._SP) then
         RIM_W_E(ig1,ig2)=X%ppaE
       else
         RIM_W_E(ig1,ig2)=sqrt(dummy(1)/dummy(2)-1._SP)
       endif
       !
     end do
   end do
   !
 end if
 !
 !Write the database
 !
 if (master_cpu) then
   call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1,2/),ID=ID)
   io_err=io_RIM_W(ID,mode,Xw)
 endif
 !
 if (have_cuda) then
   YAMBO_ALLOC_SOURCE(RIM_W_d,RIM_W)
 endif
 !
 YAMBO_FREE(f_coeff)
 !
end subroutine
!
subroutine find_q_nns(q,q_grid_b_iku,idx_q,idx_is,idx_G)
 use pars,          ONLY:SP
 use R_lattice,     ONLY:bz_samp,RIM_W_ng,g_vec,ng_vec,rl_sop
 use vec_operate,   ONLY:v_is_zero
 !
#include<memory.h>
 !
 type(bz_samp):: q
 real(SP)     :: q_grid_b_iku(3,2)
 !
 !The following variables map the extended BZ and n.n.
 !
 integer     :: idx_q (RIM_W_ng,q%nibz,5) !q    index irreduc. BZ
 integer     :: idx_is(RIM_W_ng,q%nibz,5) !Symm index
 integer     :: idx_G (RIM_W_ng,q%nibz,5) !G    index
 !
 !Index ordering of nearest-neighbour maps
 !
 !       2
 !       ^
 !       |
 ! 5 <-- 1 --> 3
 !       |
 !       v
 !       4
 !
 ! Work Space
 !
 integer     :: iq,ig,iq_trial,ig_trial
 integer     :: iq_trial_ibz,is_trial,i_dummy,nn_find,nn
 real(SP)    :: qpG(3,5),qpG_trial(3)
 !
 !Cycles over qpG points
 do iq=1,q%nibz
   !
   do ig=1,RIM_W_ng
     !
     nn_find = 0
     !
     idx_q (ig,iq,1) = iq
     idx_is(ig,iq,1) = 1
     idx_G (ig,iq,1) = ig
     !
     !Calculate qpG and qpG nearest neighbour
     qpG(:,1) = q%pt(iq,:)+g_vec(ig,:)
     !
     qpG(:,2) = qpG(:,1) + q_grid_b_iku(:,2)
     qpG(:,3) = qpG(:,1) + q_grid_b_iku(:,1)
     qpG(:,4) = qpG(:,1) - q_grid_b_iku(:,2)
     qpG(:,5) = qpG(:,1) - q_grid_b_iku(:,1)
     !
     !Find nn indexes
     do i_dummy=1,ng_vec
       !
       !Try first ig2 near ig
       if (i_dummy < 2*ig) then
         ig_trial = (2*(i_dummy-2*(i_dummy/2))-1)*(i_dummy/2) + ig
       else
         ig_trial = i_dummy
       endif
       !
       do iq_trial=1,q%nbz
         !
         !Get ibz and is indexes
         iq_trial_ibz = q%sstar(iq_trial,1)
         is_trial = q%sstar(iq_trial,2)
         qpG_trial = matmul(rl_sop(:,:,is_trial),q%pt(iq_trial_ibz,:)+g_vec(ig_trial,:))
         !
         do nn=2,5
           if (v_is_zero(qpG_trial-qpG(:,nn))) then
             idx_q (ig,iq,nn) = iq_trial_ibz
             idx_is(ig,iq,nn) = is_trial
             idx_G (ig,iq,nn) = ig_trial
             nn_find = nn_find + 1
           endif
         enddo
         !
         if (nn_find == 4) exit
         !
       enddo
       !
       if (nn_find == 4) exit
       !
     enddo
     !
     if(nn_find < 4) call error('Nearest neighbours searching failed in the W interpolation')
     !
   enddo
   !
 enddo
 !
end subroutine
