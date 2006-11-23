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
 subroutine bz_samp_indexes(k,Xk,q)
!==================================
!
! ikbz=(ik,is) --<--:--<-- okbz=(ok,os) = (IK-Q)
!                   :
!                  /:\ iqbz=(iq,is)
!                   :
!
! iq_is = ik_is-ok_os+Go
!
! qindx_X(iq,ikbz,1)=okbz
! qindx_X(iq,ikbz,2)=iGo
!
! qindx_B(ikbz,okbz,1)=iqbz
! qindx_B(ikbz,okbz,2)=iGo
!
! qindx_S(ik,iqbz,1)=okbz
! qindx_S(ik,iqbz,2)=iGo
!
 use pars,           ONLY:SP,lchlen
 use drivers,        ONLY:l_bse
 use memory_m,       ONLY:mem_est
 use com,            ONLY:msg,gen_fmt,error
 use par_proc_m,     ONLY:pp_redux_wait
 use matrix_operate, ONLY:m3inv
 use vec_operate,    ONLY:v_is_zero,c2a,rlu_v_is_zero,iku_v_norm
 use D_lattice,      ONLY:nsym
 use R_lattice,      ONLY:b,d3q_factor,RL_v_comp_acc,RL_vol,g_vec,ng_vec,&
&                         nqibz,nqbz,qindx_X,qindx_B,qindx_S,qp_states_k,&
&                         bse_scattering,qindx_alloc,rl_sop,Xk_grid_is_uniform,&
&                         k_grid,k_grid_b,qpt,bz_samp,q_module,q0_defmod
 use IO_m,           ONLY:io_control,OP_RD_CL,OP_WR_CL,VERIFY,REP
 implicit none
 type(bz_samp)::k,Xk,q
!
!ws
 integer :: io_db,io_err,ioQINDX
 integer :: i1,i2,i3,i4,iv1(3),iqibz,iqbz,ikbz,iqs
 integer, allocatable :: q_map(:,:,:),q_iptbz(:,:)
 real(SP) :: v1(3),k_b_m1(3,3)
 logical  :: connected_grids,user_defined_qpts,gamma_point_only
!
 call section('*','Transferred momenta grid: Indexes')
!------------------------------------------------------
 user_defined_qpts=.false.
 gamma_point_only=.false.
 if (.not.bse_scattering) bse_scattering=l_bse
 q%description='q'
!
 call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1,2,3/),MODE=VERIFY,ID=io_db)
 io_err=ioQINDX(Xk,q,io_db)
 if (io_err==0) then
   call k_expand(q)
   call Q_report
   allocate(qpt(q%nibz,3),q_module(q%nibz))
   qpt=q%pt
   do i1=1,q%nibz
     q_module(i1)=iku_v_norm(qpt(i1,:))
   enddo
   q_module(1)=q0_defmod
   call mem_est("q-pt q_module",(/q%nibz*3,q%nibz/),(/SP,SP/))   
   return
 endif
!
! First I map the Xk grid in a simple cubic Net
!
 call k_ibz2bz(Xk,'a',.false.)  

 allocate(q_map(k_grid(1),k_grid(2),k_grid(3)),q_iptbz(Xk%nbz,3))
!
! k_b_m1=transpose(k_grid_b) in rlu
!
! k_grid_b is in cc !
!
 call c2a(v_in=k_grid_b(1,:),v_out=k_b_m1(:,1),mode='kc2a')
 call c2a(v_in=k_grid_b(2,:),v_out=k_b_m1(:,2),mode='kc2a')
 call c2a(v_in=k_grid_b(3,:),v_out=k_b_m1(:,3),mode='kc2a')
!
! q_iptbz(i,:) = Xk%ptbz(1,:) - Vo  in units of k_grid_b
! q_map gives the index of q_iptbz given the components along k_grid_b
!
! As first step I define the map with respect to Q defined as difference
! of K-pts
!
 call m3inv(M=k_b_m1)
 call define_q_map(Xk%ptbz(:,:),Xk%ptbz(1,:))
!
! THE GRID IS UNIFORM IF ALL THE Q_MAP IS FILLED
!
 Xk_grid_is_uniform=all(q_map/=0)
!
 if (Xk_grid_is_uniform) then
   q%nbz=Xk%nbz
!
!  q_ptbz in iku for k_reduce
!
   allocate(q%ptbz(q%nbz,3))
   do i1=1,q%nbz
     call c2a(v_in=Xk%ptbz(i1,:)-Xk%ptbz(1,:),v_out=q%ptbz(i1,:),mode='ka2i')
   enddo
   if (.not.allocated(qpt)) then
     call k_reduce(q)
     deallocate(q%ptbz)
     allocate(qpt(q%nibz,3),q_module(q%nibz))
     qpt=q%pt
     do i1=1,q%nibz
       q_module(i1)=iku_v_norm(qpt(i1,:))
     enddo
     q_module(1)=q0_defmod
     call mem_est("q-pt q_module",(/q%nibz*3,q%nibz/),(/SP,SP/))   
   else
     q%nibz=nqibz
     allocate(q%pt(q%nibz,3))
     q%pt=qpt
     call msg('rsn','[K latt] User defined Q-grid.')
     user_defined_qpts=.true.
     bse_scattering=.true.
   endif
!
!  q_ptbz in rlu for qindx_*.
!  At difference with the Q list used in evaluating the map before
!  here I need top recalculate the map so that it correctly points
!  to the q%ptbz obtaine trought q_expand
!
   call k_expand(q)
   call k_ibz2bz(q,'a',.false.)
   nqbz=q%nbz
   nqibz=q%nibz
   call Q_report()
!
! When using USER defined Q-points q%nbz may be different from Xk%nbz
! if the given list is not correct.
! In this case I switch to the Gamma only support
!
   if (q%nbz/=Xk%nbz) then
     call msg('rsn','[K latt] Q BZ pts are /= from X grid BS pts. Gamma point only.')
     deallocate(q%pt,qpt)
     allocate(q%pt(1,3),qpt(1,3),q_module(1)) 
     q%pt=0.
     qpt=0.
     q_module=q0_defmod
     nqbz=1
     nqibz=1
     q%nibz=1
     q%nbz=1
!DEBUG >
! when imposing the G point with a uniform X grid I get
! segmentation fault in the BSE. 
     Xk_grid_is_uniform=.false.
!DEBUG <
     if (.not.Xk_grid_is_uniform) bse_scattering=.false.
     gamma_point_only=.true.
   else
     call define_q_map(q%ptbz(:,:),(/0.,0.,0./))
   endif
 else
   call msg('rsn','[K latt] X grid is not uniform.  Gamma point only.')
   nqbz=1
   nqibz=1
   q%nibz=1
   q%nbz=1
   allocate(q%pt(1,3)) 
   q%pt=0.
   gamma_point_only=.true.
 endif
!
! Allocate
!
 call qindx_alloc()
!
! Gamma point only
!
 if (gamma_point_only) then
   forall(i1=1:Xk%nbz) qindx_X(1,i1,1)=i1
   forall(i1=1:Xk%nbz) qindx_X(1,i1,2)=1
   goto 1
 endif

 if (.not.bse_scattering) call msg('nr',':: Indices: polarization function')
 if (     bse_scattering) call msg('nr',':: Indices: polarization function + BSE')
 qindx_X=0
 qindx_S=0
 if (bse_scattering) qindx_B=0
!
! X indexes
!
! qindx_X(iq,ikbz,1)=okbz
! qindx_X(iq,ikbz,2)=iGo
!
! qindx_B(ikbz,okbz,1)=iqbz
! qindx_B(ikbz,okbz,2)=iGo
!
 do i1=1,Xk%nbz
   do i2=1,Xk%nbz
     v1=matmul(k_b_m1,Xk%ptbz(i1,:)-Xk%ptbz(i2,:)) ! K_i1-K_i2= Q + Go
     iv1=nint(v1)
     do while(.not.v_is_zero(v1-real(iv1)))
       if (RL_v_comp_acc>1.E-3) exit
       RL_v_comp_acc=RL_v_comp_acc*10.
     enddo 
     call k_grid_shift(iv1)
     i3=q_map(iv1(1),iv1(2),iv1(3))
     iqibz=q%sstar(i3,1)
     iqs=q%sstar(i3,2)
     if (iqs/=1.and..not.bse_scattering) cycle
     if (iqs==1) qindx_X(iqibz,i1,1)=i2
     if (bse_scattering) qindx_B(i1,i2,1)=i3
     v1=Xk%ptbz(i1,:)-Xk%ptbz(i2,:)-q%ptbz(i3,:) 
     call c2a(v_in=v1,mode='ka2i')
     do i3=1,ng_vec
       if (v_is_zero(v1-g_vec(i3,:))) then
         if (iqs==1) qindx_X(iqibz,i1,2)=i3
         if (bse_scattering) qindx_B(i1,i2,2)=i3
         exit
       endif
     enddo
   enddo
 enddo
 if (any(qindx_X==0)) call error('Error in Q-grid search [qindx_X]')
!
 call msg('rn',':: Indices: Self-Energy')
 call k_ibz2bz(k,'a',.false.)
 connected_grids=.true.
 do i1=qp_states_k(1),qp_states_k(2),1
   ikbz=sum(k%nstar(:i1-1))+1
   do i2=1,k%nbz
     v1=matmul(k_b_m1,k%ptbz(ikbz,:)-k%ptbz(i2,:))
     iv1=nint(v1)
     if (.not.v_is_zero(v1-real(iv1))) connected_grids=.false.
     if (.not.v_is_zero(v1-real(iv1))) cycle
     call k_grid_shift(iv1)
     iqbz=q_map(iv1(1),iv1(2),iv1(3))
     qindx_S(i1,iqbz,1)=i2
     v1=k%ptbz(ikbz,:)-k%ptbz(i2,:)-q%ptbz(iqbz,:)
     call c2a(v_in=v1,mode='ka2i')
     do i3=1,ng_vec
       if (v_is_zero(v1-g_vec(i3,:))) then
         qindx_S(i1,iqbz,2)=i3
         exit
       endif
     enddo
   enddo
   if (any(qindx_S(i1,:,:)==0)) call error('Null transition detected [qindx_S]')
 enddo
 if (.not.connected_grids) call msg('rsn','[K latt] X & Total k-grids are not connected')

1 call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1,2,3/),ID=io_db)
 io_err=ioQINDX(Xk,q,io_db)
!
!CLEAN
!
 call k_ibz2bz(Xk,'d',.false.)
 deallocate(q_map,q_iptbz)
 if (Xk_grid_is_uniform) deallocate(q%ptbz)
 call pp_redux_wait

 contains 

   subroutine Q_report()
   character(lchlen)::ch(4)
   call msg('nr','IBZ Q-points :',q%nibz)
   call msg('rn','BZ  Q-points :',q%nbz)
   d3q_factor=RL_vol/real(q%nbz)
   do i1=1,q%nibz
     ch(2)=gen_fmt(r_v=q%pt(i1,:))
     ch(3)=gen_fmt(r_v=(/q%weights(i1)/))
     write (ch(1),'(7a)') '(a,i3.3,a,3(',trim(ch(2)),',1x),a,',trim(ch(3)),',1x)'
     write (ch(2),trim(ch(1)))   'Q [',i1,'] :',q%pt(i1,:),'(iku) * weight ',q%weights(i1)
     call msg('r',trim(ch(2)))
   enddo
   end subroutine

   subroutine k_grid_shift(v)
   implicit none
   integer  :: v(3),u(3),i1
   do i1=1,3
     if (v(i1)>=0) u(i1)=mod(v(i1)+1,k_grid(i1))
     if (v(i1)>=0.and.u(i1)==0) u(i1)=k_grid(i1)
     if (v(i1)<0) u(i1)=mod(v(i1),k_grid(i1))
     if (v(i1)<0.and.u(i1)/=0) u(i1)=u(i1)+k_grid(i1)+1
     if (v(i1)<0.and.u(i1)==0) u(i1)=1
   enddo
   v=u 
   end subroutine

   subroutine define_q_map(qpt_map,q_ref)
   real(SP)::qpt_map(Xk%nbz,3),q_ref(3)
   q_map=0
   do i1=1,Xk%nbz
     v1=matmul(k_b_m1,qpt_map(i1,:)-q_ref)
     do while(.not.rlu_v_is_zero(v1))
       RL_v_comp_acc=RL_v_comp_acc*10.
       if (RL_v_comp_acc>1.E-3) return
     enddo
     q_iptbz(i1,:)=nint(v1)
     call k_grid_shift(q_iptbz(i1,:))
     if (q_map(q_iptbz(i1,1),q_iptbz(i1,2),q_iptbz(i1,3))/=0) then
       call error('Equivalent points in the rlu grid [overlapping stars?]')
     endif
     q_map(q_iptbz(i1,1),q_iptbz(i1,2),q_iptbz(i1,3))=i1
   enddo
!
! Now I fill the holes in the map shifting the whole grid
!
   do i1=1,Xk%nbz
     do i2=-2,2
       do i3=-2,2
         do i4=-2,2
           v1=matmul(k_b_m1,qpt_map(i1,:)-q_ref+(/i2,i3,i4/))
           iv1=nint(v1)
           call k_grid_shift(iv1)
           if (q_map(iv1(1),iv1(2),iv1(3))==0) q_map(iv1(1),iv1(2),iv1(3))=i1
         enddo
       enddo
     enddo
   enddo
   end subroutine

 end subroutine
