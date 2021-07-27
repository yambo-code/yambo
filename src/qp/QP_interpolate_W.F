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
subroutine interpolate_W(X,Xw,q,mode)
 !
 use pars,          ONLY:SP,pi,zero_dfl,schlen,DP,rZERO
 use com,           ONLY:msg
 use R_lattice,     ONLY:bz_samp,RIM_W_ng,g_vec,b,k_grid_b,ng_vec,rl_sop,bare_qpg,&
&                        RIM_W_is_diagonal,RIM_W,RIM_W_d,f_coeff,cut_is_slab,idir,RIM_W_E,&
&                        RIM_id_epsm1_reference,RIM_epsm1,RIM_W_for_graph,RIM_qpg
 use memory,        ONLY:MEM_err
 use vec_operate,   ONLY:c2a,v_is_zero,v_norm
 use X_m,           ONLY:X_mat,X_t,X_alloc
 use frequency,     ONLY:w_samp
 use IO_m,          ONLY:io_control,OP_RD_CL,NONE,RD_CL,OP_RD,RD_CL_IF_END,&
&                        deliver_IO_error_message,OP_WR_CL,REP,VERIFY
 use stderr,        ONLY:STRING_split,intc,STRING_pack,gen_fmt,STRING_split
 use D_lattice,     ONLY:alat,a
 use timing_m,      ONLY:timing
 use cuda_m,        ONLY:have_cuda
 !
#include<memory.h>
 !
 type(X_t)    ::X
 type(w_samp) ::Xw
 type(bz_samp):: q
 character(3) :: mode
 !
 ! Work Space
 !
 integer     :: iq1,ig1,iq2,ig2,iq,ig,igr,igc,ig2max,iomega
 integer     :: iq_ibz,is,i,find,nn,idm
 integer     :: idx_q(RIM_W_ng,q%nibz,5),idx_G(RIM_W_ng,q%nibz,5),idx_is(RIM_W_ng,q%nibz,5)
 real(SP)    :: v(5),dummy(2),anis_factor,em1_anis(3)
 real(DP)    :: f_func(5)
 complex(SP) :: vX(5,q%nibz,RIM_W_ng,RIM_W_ng,2)
 real(SP)    :: qpG(3,5),delta_q(3,2),qpG_trial(3),v1(3),delta_q_cc(3),r_dum(2)
 integer     :: ID, io_err, ng_save,G_max_ibz(q%nibz)
 integer, external :: io_X
 integer, external :: io_RIM_W
 character(schlen):: ch_dum,fmt_dum,msg_dum(2)
 !
 call timing('RIM-W-coeff',OPR='start')
 call k_ibz2bz(q,'a',.true.)
 allocate(f_coeff(6,RIM_W_ng,RIM_W_ng,q%nibz,Xw%n_freqs),stat=MEM_err)
 !
 call section("+", "RIM-W interpolation")
 !
 !Load infinitesimal steps in iku
 do idm=1,2
   v1 = k_grid_b(idm,:)
   call c2a(b_in=b,v_in=v1,mode='kc2a')
   if (abs(v1(1)) < zero_dfl) then
     delta_q(:,2) = abs(v1)
     call c2a(v_in=delta_q(:,2),mode='ka2i')
   else if (abs(v1(2)) < zero_dfl) then
     delta_q(:,1) = abs(v1)
     call c2a(v_in=delta_q(:,1),mode='ka2i')
   else
     call msg('r', 'Error in loading q_min')
   end if
 end do
 !
 call warning ("RIM-W works only for 2D semiconductors")
 call msg('r', 'Found compatible q grid: 2D')
 call msg('r', 'q_min[ 1 ]', delta_q(:,1), '[iku]')
 call msg('r', 'q_min[ 2 ]', delta_q(:,2), '[iku]')
 call msg('r', 'G-vectors ', RIM_W_ng)
 !
 !Check if RIM-W has been alreay calculated
 call io_control(ACTION=OP_RD_CL,COM=REP,MODE=VERIFY,SEC=(/1,2/),ID=ID)
 io_err=io_RIM_W(ID,mode)
 !
 if (io_err==0) then
   call timing('RIM-W-coeff',OPR='stop')
   return
 endif
 !
 !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 !Calculation of the nearest neighbours indexes
 !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 !
 !Cycles over qpG points
 do iq1=1,q%nibz
   !
   do ig1=1,RIM_W_ng
     !
     find = 0
     !
     idx_q(ig1,iq1,1) = iq1
     idx_is(ig1,iq1,1) = 1
     idx_G(ig1,iq1,1) = ig1
     !
     !Calculate qpG and qpG nearest neighbour
     qpG(:,1) = matmul(rl_sop(:,:,idx_is(ig1,iq1,1)),q%pt(idx_q(ig1,iq1,1),:)+g_vec(ig1,:))
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
     if(find < 4) call error('Nearest neighbours searching failed in the W interpolation')
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
 !Initialize G_max_ibz to RIM_W_ng
 G_max_ibz(1:q%nibz) = RIM_W_ng
 !
 !Look if G_max_ibz must be increased
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
 if(maxval(G_max_ibz) > X%ng) call error('G_max for RIM-W is lower than G_max of vX')
 !
 !
 call msg('r', 'G-vectors loaded', maxval(G_max_ibz))
 call msg('r','')
 do iq=1,q%nibz
   !
   ! I/O of X
   !
   !Load only the G needed
   X%ng = G_max_ibz(iq)
   !
   if (iq==1) then
     call io_control(ACTION=OP_RD,COM=REP,       SEC=(/1,2,2*iq+1/),ID=ID)
   else if (q%nibz==1) then
     call io_control(ACTION=OP_RD_CL,COM=NONE,    SEC=(/1,2,3/),ID=ID)
   else if (iq > 1) then
     call io_control(ACTION=RD_CL_IF_END,COM=NONE,SEC=(/2*iq,2*iq+1/),ID=ID)
   endif
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
           vX(nn,iq1,ig1,ig2,iomega) = X_mat(idx_G(ig1,iq1,nn),idx_G(ig2,iq1,nn),iomega)
         endif
         !
       enddo
       !
     enddo
     enddo
   enddo
   enddo
   !
   call X_alloc('X')
 enddo
 !
 X%ng = ng_save 
 !
 !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 !Calculation of the interpolation coefficients
 !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 !
 !from now on, delta_q must be in rlu
 do idm=1,2 
   call c2a(v_in=delta_q(:,idm),mode='ki2a')
 end do
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
      !Evaluate f_func
      do nn=1,5
        !
        !Select the index of the reference/n.n.
        iq2 = idx_q(igr,iq1,nn)
        ig1 = idx_G(igr,iq1,nn)
        ig2 = idx_G(igc,iq1,nn)
        !
        f_func(nn) = real(bare_qpg(iq2,ig1)*bare_qpg(iq2,ig2),kind=DP)/(4._DP*pi)*&
&                    real(vX(nn,iq1,igr,igc,iomega),kind=DP)/(real(vX(nn,iq1,igr,igc,iomega),kind=DP)+1._DP)
        !
      enddo
      !
      ! q == 0 terms must be tratened separately
      if (iq1 == 1 .and. igr == 1 .and. igc == 1 .and. (.not. RIM_W_for_graph)) then
        !
        if(RIM_id_epsm1_reference/=0 .and. iomega==1) then
          !vX is included in the anis_factor
          call msg('r',"This is the anysotropy case")
          f_coeff(1,igr,igc,iq1,iomega)=(4._DP*pi)/(bare_qpg(iq1,igr)*bare_qpg(iq1,igc)*(2._DP*pi*alat(idir(1)))**2)
        else
          f_coeff(1,igr,igc,iq1,iomega) = vX(1,iq1,igr,igc,iomega)*(4._DP*pi) / &
&                                  (bare_qpg(iq1,igr)*bare_qpg(iq1,igc)*(2._DP*pi*alat(idir(1)))**2)
        end if
        !
        do nn=2,3
          idm = MOD(nn+1,2)+1
          call c2a(b,delta_q(:,idm),delta_q_cc,'ka2c')
          if(RIM_id_epsm1_reference/=0 .and. iomega==1) then
            anis_factor=dot_product(em1_anis,delta_q_cc**2)
            f_coeff(nn,igr,igc,iq1,iomega)=-1._DP/delta_q(idm,idm)*log(f_func(nn)/ &
&                                 (f_coeff(1,iq1,igc,iq1,iomega)*anis_factor))
          else
            f_coeff(nn,igr,igc,iq1,iomega)=-1._DP/delta_q(idm,idm)*log(f_func(nn)/ &
&                                 (v_norm(delta_q_cc)**2*f_coeff(1,iq1,igc,iq1,iomega)))
          end if

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
        !
        f_coeff(1,igr,igc,iq1,iomega) =  v(1)                                      !f   (iku)
        f_coeff(2,igr,igc,iq1,iomega) =  v(2)+v(3)*a(2,1)/alat(1)                  !fx  (iku)
        f_coeff(3,igr,igc,iq1,iomega) =  v(3)+v(2)*a(1,2)/alat(2)                  !fy  (iku)
        f_coeff(4,igr,igc,iq1,iomega) =  v(4)+v(5)*(a(2,1)/alat(1))**2             !fxx (iku)
        f_coeff(5,igr,igc,iq1,iomega) =  v(4)*a(1,2)/alat(2)+v(5)*a(2,1)/alat(1)   !fxy (iku)
        f_coeff(6,igr,igc,iq1,iomega) =  v(5)+v(4)*(a(1,2)/alat(2))**2             !fyy (iku)
      endif
      !
      f_coeff(:,igc,igr,iq1,iomega) = f_coeff(:,igr,igc,iq1,iomega)
      !
    enddo
  enddo
 enddo
 enddo
 call timing('RIM-W-coeff',OPR='stop')
 !
 !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 !Calculation of RIM-W
 !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 !
 call rim('c')
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
         !iq=q_norm_i(i2)
         ch_dum=' < -Wc [Q = '//trim(intc(iq2))//'] >'
         if (iq1/=iq2) ch_dum=' * < -Wc [Q = '//trim(intc(iq2))//'] >'
         !
         r_dum(1)= -RIM_W(iq2,1,1)*2._SP*pi
         r_dum(2)= -vX(1,iq2,1,1,1)*RIM_qpg(iq2,1,1)*2_SP*pi
         !
         fmt_dum='(a,2('//trim(gen_fmt(r_v=r_dum))//'))'
         write (msg_dum(iq2-iq1+1),trim(fmt_dum)) trim(ch_dum),r_dum
         !
       enddo
       if (iq1==1) call msg('nr',trim(STRING_pack(msg_dum(1),msg_dum(2))))
       if (iq1/=1) call msg(' r',trim(STRING_pack(msg_dum(1),msg_dum(2))))
   enddo
 end if
 !
 !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 !Calculation of RIM-W energies
 !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 allocate(RIM_W_E(RIM_W_ng,RIM_W_ng),stat=MEM_err)
 RIM_W_E = rZERO
 if (mode == 'PPA') then
   !
   do ig2=1,RIM_W_ng
     do ig1=1,RIM_W_ng
       !
       if(ig1==1.and.ig2==1.and.(.not. RIM_W_for_graph)) then
         dummy(:) = f_coeff(1,1,1,1,:)  
         if (RIM_id_epsm1_reference/=0) then
           dummy(1) = dummy(1)*0.5*(em1_anis(idir(2))+em1_anis(idir(3)))
         end if
         !
       else
         dummy(:) =  f_coeff(1,ig1,ig2,1,:)+1.E-4*(f_coeff(2,ig1,ig2,1,:)+f_coeff(3,ig1,ig2,1,:)&
&                                          +1.E-4*(f_coeff(4,ig1,ig2,1,:)+f_coeff(6,ig1,ig2,1,:)&
                                           +2._SP*f_coeff(5,ig1,ig2,1,:)))
         dummy(:) = dummy(:)*4._SP*pi/(bare_qpg(1,ig1)*bare_qpg(1,ig2))
         dummy(:) = dummy/(1-dummy)
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
 call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1,2/),ID=ID)
 io_err=io_RIM_W(ID,mode)
 !
 if (have_cuda) then
   YAMBO_ALLOC_SOURCE(RIM_W_d,RIM_W)
 endif
 !
end subroutine
