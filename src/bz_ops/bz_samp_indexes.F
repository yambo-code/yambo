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
subroutine bz_samp_indexes(k,Xk,q)
 !
 ! ikbz=(ik,is) --<--:--<-- okbz=(ok,os) = (IK-Q)
 !                   :
 !                  /:\ iqbz=(iq,is)
 !                   :
 !
 ! iq_is = ik_is-ok_os-Go
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
 ! qindx_C(ikbz,iqbz,1)=okbz
 ! qindx_C(ikbz,iqbz,2)=iGo
 !
 use pars,           ONLY:SP,lchlen,zero_dfl
 use drivers,        ONLY:l_bse,l_elel_scatt
 use stderr,         ONLY:gen_fmt
 use com,            ONLY:msg
 use parallel_m,     ONLY:PP_redux_wait
 use LIVE_t,         ONLY:live_timing
 use matrix_operate, ONLY:m3inv,m3det
 use vec_operate,    ONLY:v_is_zero,c2a
 use BS,             ONLY:BS_res_mode,BS_cpl_mode
 use R_lattice,      ONLY:d3q_factor,RL_vol,nqibz,nqbz,QP_states_k,&
&                         qindx_X,qindx_B,qindx_S,qindx_C,&
&                         bse_scattering,qindx_alloc,Xk_grid_is_uniform,&
&                         k_grid,k_grid_b,q_pt,k_pt,bz_samp,q_norm,q0_def_norm,&
&                         q_sstar,coll_scattering,q_source,Xk_grid_new_mapping
 use IO_m,           ONLY:io_control,OP_RD_CL,OP_WR_CL,VERIFY,REP
 use parser_m,       ONLY:parser
 use timing_m,       ONLY:timing
 use openmp,         ONLY:n_threads,master_thread,OPENMP_update
#if defined _ELPH
 use ELPH,           ONLY:elph_use_q_grid
#endif
#include<memory.h>
 !
 type(bz_samp)::k,Xk,q
 !
 !Work Space
 !
 integer, external :: G_index
 !
 character(lchlen) :: string
 integer :: io_db,io_err
 integer :: i1,i2,i3,i4,iv1(3),iqibz,iqbz,ikibz,ikbz,ikbz_p,iqs,iks,id
 real(SP):: v1(3),k_b_m1(3,3),local_zero(3),q_step(3)
 logical :: connected_grids,user_defined_qpts,gamma_point_only
#if defined _ELPH
 logical :: bse_setup_scatt
#endif
 integer, allocatable :: q_map(:,:,:),q_iptbz(:,:)
 !
 integer, external    :: io_QINDX 
 !
 call section('*','Transferred momenta grid')
 !
 user_defined_qpts=.false.
 gamma_point_only=.false.
 !
 if (.not.bse_scattering ) bse_scattering =l_bse .and. (index(BS_res_mode,'c')/=0.or.index(BS_cpl_mode,'c')/=0)
 if (.not.coll_scattering) coll_scattering=l_elel_scatt
#if defined _ELPH
 call parser('BSEscatt',bse_setup_scatt)
 if (.not.bse_scattering ) bse_scattering =elph_use_q_grid.or.bse_setup_scatt.or.&
&                                          trim(q_source)=="Electron-Phonon databases"
#endif
 !
 q%description='q'
 !
 call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1,2,3/),MODE=VERIFY,ID=io_db)
 io_err=io_QINDX(Xk,q,io_db)
 if (io_err==0) then
   call k_expand(q)
   call Q_report('iku')
   call Q_report('cc ')
   call q_shadows(.false.)
   return
 endif
 !
 call timing('bz_samp_indexes',OPR='start')
 !
 !
 ! First I map the Xk grid in a simple cubic Net
 !
 call k_ibz2bz(Xk,'a',.false.)  
 !
 YAMBO_ALLOC(q_map,(k_grid(1),k_grid(2),k_grid(3)))
 YAMBO_ALLOC(q_iptbz,(Xk%nbz,3))
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
 if (abs(m3det(k_b_m1))>1.E-7) then
   !
   call m3inv(M=k_b_m1)
   !
   if(     Xk_grid_new_mapping) call define_q_map_new( Xk%ptbz(:,:), Xk%ptbz(1,:))
   if(.not.Xk_grid_new_mapping) call define_q_map    ( Xk%ptbz(:,:), Xk%ptbz(1,:))
   !
   ! THE GRID IS UNIFORM IF ALL THE Q_MAP IS FILLED
   !
   Xk_grid_is_uniform=all(q_map/=0)
   !
 else
   Xk_grid_is_uniform=.false.
 endif
 !
 if (Xk_grid_is_uniform) then
   !
   q%nbz=Xk%nbz
   !
   ! q_ptbz in iku for k_reduce
   !
   YAMBO_ALLOC(q%ptbz,(q%nbz,3))
   do i1=1,q%nbz
     !
     v1=Xk%ptbz(i1,:)-Xk%ptbz(1,:)
     !
     call c2a(v_in= v1,v_out=q%ptbz(i1,:),mode='ka2i')
     !
   enddo
   if (.not.allocated(q_pt)) then
     call k_reduce(q,.TRUE.)
     YAMBO_FREE(q%ptbz)
   else
     q%nibz=nqibz
     YAMBO_ALLOC(q%pt,(q%nibz,3))
     q%pt=q_pt(:q%nibz,:)
     call msg('rsn','[RL indx] Q-grid'//trim(q_source))
     user_defined_qpts=.true.
   endif
   !
   ! q_ptbz in rlu for qindx_*.
   ! At difference with the Q list used in evaluating the map before
   ! here I need to recalculate the map so that it correctly points
   ! to the q%ptbz obtaine trought q_expand
   !
   call k_expand(q)
   call q_shadows(.false.)
   call k_ibz2bz(q,'a',.false.)
   nqbz =q%nbz
   nqibz=q%nibz
   call Q_report('iku')
   call Q_report('cc ')
   !
   ! When using USER defined Q-points q%nbz may be different from Xk%nbz
   ! if the given list is not correct.
   ! In this case I switch to the Gamma only support
   !
   if (q%nbz/=Xk%nbz) then
     call msg('rsn','[RL indx] Q BZ pts are /= from X grid BS pts. Gamma point only.')
     call q_shadows(.true.)
     !
     ! when imposing the G point with a uniform X grid I get
     ! segmentation fault in the BSE. 
     !
     Xk_grid_is_uniform=.false.
     bse_scattering=.false.
     gamma_point_only=.true.
   else
     !  
     if(     Xk_grid_new_mapping) call define_q_map_new(q%ptbz(:,:),(/0._SP,0._SP,0._SP/))
     if(.not.Xk_grid_new_mapping) call define_q_map    (q%ptbz(:,:),(/0._SP,0._SP,0._SP/))
     !
   endif
   !
 else
   !
   call msg('nrsn','[RL indx] X grid is not uniform. Gamma point only.')
   call q_shadows(.true.)
   gamma_point_only=.true.
   bse_scattering=.false.
   !
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
 !
 string=' Indices: polarization function'
 if ( bse_scattering) string=trim(string)//' + BSE'
 if (coll_scattering) string=trim(string)//' + COLL'
 call msg('nr',trim(string))
 !
 qindx_X=0
 qindx_S=0
 if ( bse_scattering) qindx_B=0
 if (coll_scattering) qindx_C=0
 !
 ! X indexes
 !
 ! qindx_X(iq,ikbz,1)=okbz
 ! qindx_X(iq,ikbz,2)=iGo
 !
 ! qindx_B(ikbz,okbz,1)=iqbz
 ! qindx_B(ikbz,okbz,2)=iGo
 !
 if (n_threads> Xk%nbz) call live_timing('X indexes',1)
 if (n_threads<=Xk%nbz) call live_timing('X indexes',Xk%nbz/n_threads)
 !
 if(Xk_grid_new_mapping) call k_find_smallest_q(Xk%ptbz,q_step,Xk%nbz)
 !
 !$omp parallel default(shared), private(ikbz,ikbz_p,v1,iv1,iqbz,iqibz,iqs)
 call OPENMP_update(master_thread)
 !
 !$omp do
 do ikbz=1,Xk%nbz
   !
   do ikbz_p=1,Xk%nbz
     !
     if(Xk_grid_new_mapping) then
       !  
       v1=Xk%ptbz(ikbz,:)-Xk%ptbz(ikbz_p,:)
       v1=v1-nint(v1)       ! Bring in the BZ, remove the G0
       where(v1<-zero_dfl)  ! Bring between [0,1)
         v1(:)=1._SP+v1(:)
       end where
       do id=1,3
         if(q_step(id)==0) then
           iv1(id)=1 ! Non-periodic directions or no kpoints in this direction
         else
           iv1(id)=nint(v1(id)/q_step(id))+1
         endif
       enddo
       !
     else
       !  
       v1=matmul(k_b_m1,Xk%ptbz(ikbz,:)-Xk%ptbz(ikbz_p,:)) ! K-K_p= Q + Go
       iv1=nint(v1)
       call k_grid_shift(iv1)
       !
     endif
     !
     iqbz =q_map(iv1(1),iv1(2),iv1(3))
     iqibz=q%sstar(iqbz,1)
     iqs  =q%sstar(iqbz,2)
     if (iqs/=1.and..not.bse_scattering) cycle
     if (iqs==1) qindx_X(iqibz,ikbz,1)=ikbz_p
     if (bse_scattering) qindx_B(ikbz,ikbz_p,1)=iqbz
     v1=Xk%ptbz(ikbz,:)-Xk%ptbz(ikbz_p,:)-q%ptbz(iqbz,:)
     call c2a(v_in=v1,mode='ka2i')
     !
     if (iqs==1)         qindx_X(iqibz,ikbz  ,2)=G_index(v1)
     if (bse_scattering) qindx_B(ikbz ,ikbz_p,2)=G_index(v1)
     !
   enddo
   !
   if (master_thread) call live_timing(steps=1)
   !
 enddo
 !$omp end do
 !$omp end parallel
 !
 call live_timing()
 !
 if (any(qindx_X==0)) call error('Error in Q-grid search [qindx_X]')
 !
 if(coll_scattering) then
   call msg('rn',' Indices: Scattering and Self-Energy')
   if (n_threads> k%nbz) call live_timing('SSE indexes',1)
   if (n_threads<=k%nbz) call live_timing('SSE indexes',Xk%nbz/n_threads)
 else
   call msg('rn',' Indices: Self-Energy')
   i1=QP_states_k(2)-QP_states_k(1)+1
   if (n_threads> i1) call live_timing('SE indexes',1)
   if (n_threads<=i1) call live_timing('SE indexes',i1/n_threads)
 endif
 !
 call k_ibz2bz(k,'a',.false.)
 !
 connected_grids=.true.
 local_zero=1.E-4_SP
 !
 ! qindx_S(ik,iqbz,1)=okbz
 ! qindx_S(ik,iqbz,2)=iGo
 !
 ! qindx_C(ikbz,iqbz,1)=okbz
 ! qindx_C(ikbz,iqbz,2)=iGo
 !
 !$omp parallel default(shared), private(ikbz,ikibz,iks,ikbz_p,v1,iv1,iqbz)
 call OPENMP_update(master_thread)
 !
 !$omp do
 do ikbz=1,k%nbz
   !
   ikibz=k%sstar(ikbz,1)
   iks  =k%sstar(ikbz,2)
   !
   if (iks/=1.and..not.coll_scattering) cycle
   !
   do ikbz_p=1,k%nbz
     !
     if(Xk_grid_new_mapping) then
       !
       v1=Xk%ptbz(ikbz,:)-Xk%ptbz(ikbz_p,:)
       v1=v1-nint(v1)       ! Bring in the BZ, remove the G0
       where(v1<-zero_dfl)  ! Bring between [0,1)
         v1(:)=1._SP+v1(:)
       end where
       do id=1,3
         if(q_step(id)==0) then
           iv1(id)=1 ! Non-periodic directions or no kpoints in this direction
         else
           iv1(id)=nint(v1(id)/q_step(id))+1
         endif
       enddo
       !
     else
       !   
       v1=matmul(k_b_m1,k%ptbz(ikbz,:)-k%ptbz(ikbz_p,:))
       iv1=nint(v1)
       !
       if (.not.v_is_zero(v1-real(iv1,SP),zero_=local_zero)) then
          !$omp critical
          connected_grids=.false.
          !$omp end critical
          cycle
       endif
       !
       call k_grid_shift(iv1)
       !
     endif
     !
     iqbz=q_map(iv1(1),iv1(2),iv1(3))
     !
     if (iks==1) qindx_S(ikibz,iqbz,1)=ikbz_p
     if (coll_scattering) qindx_C(ikbz,iqbz,1)=ikbz_p
     v1=k%ptbz(ikbz,:)-k%ptbz(ikbz_p,:)-q%ptbz(iqbz,:)
     call c2a(v_in=v1,mode='ka2i')
     if (iks==1) qindx_S(ikibz,iqbz,2)=G_index(v1)
     if (coll_scattering) qindx_C(ikbz,iqbz,2)=G_index(v1)
   enddo
   !
   if (master_thread) call live_timing(steps=1)
   !
 enddo
 !$omp end do
 !$omp end parallel
 !
 call live_timing()
 !
 do ikbz=1,k%nbz
   ikibz=k%sstar(ikbz,1)
   if (any(qindx_S(ikibz,:,:)==0)) then
     call warning(' Null transition detected [qindx_S]. Gamma point only.')
     call q_shadows(.true.)
     Xk_grid_is_uniform=.false.
     bse_scattering=.false.
     gamma_point_only=.true.
     exit
   endif
 enddo
 !
 ! For some reason I have seen CPU's loading the DB while/before it is actually written.
 ! This is avoided here by using the PP_redux_wait call.
 !
 call PP_redux_wait()
 !
 if (.not.connected_grids) call msg('rsn','[RL indx] X & Total k-grids are not connected')
 !
1 call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1,2,3/),ID=io_db)
 io_err=io_QINDX(Xk,q,io_db)
 !
 !CLEAN
 !
 call k_ibz2bz(Xk,'d',.false.)
 YAMBO_FREE(q_map)
 YAMBO_FREE(q_iptbz)
 if (Xk_grid_is_uniform) then
   YAMBO_FREE(q%ptbz)
 endif
 call PP_redux_wait()
 !
 call timing('bz_samp_indexes',OPR='stop')
 !
 contains 
   !
   subroutine Q_report(kind)
     character(3)     ::kind
     character(lchlen)::ch(4)
     real(SP)         ::q_cc(3)
     if(trim(kind)=='iku') then
       call msg('nr','IBZ Q-points ',q%nibz)
       call msg('rn','BZ  Q-points ',q%nbz)
     endif
     if(trim(kind)=='cc') call msg('r',' ')
     d3q_factor=RL_vol/real(q%nbz)
     do i1=1,q%nibz
       if(trim(kind)=='cc') call c2a(v_in=q%pt(i1,:),v_out=q_cc,mode='ki2c')
       ch(2)=gen_fmt(r_v=q%pt(i1,:))
       ch(3)=gen_fmt(r_v=(/q%weights(i1)/))
       write (ch(1),'(7a)') '(a,i5.5,a,3(',trim(ch(2)),',1x),a,',trim(ch(3)),',1x)'
       if(trim(kind)=='cc')  write (ch(2),trim(ch(1)))   'Q [',i1,'] ',q_cc(:),'(cc ) * weight ',q%weights(i1)
       if(trim(kind)=='iku') write (ch(2),trim(ch(1)))   'Q [',i1,'] ',q%pt(i1,:),'(iku) * weight ',q%weights(i1)
       call msg('r',trim(ch(2)))
     enddo
   end subroutine
   !
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
   !
   subroutine define_q_map(qpt_map,q_ref)
     !
     implicit none
     !
     real(SP)::qpt_map(Xk%nbz,3),q_ref(3)
     !
     q_map=0
     do i1=1,Xk%nbz
       v1=matmul(k_b_m1,qpt_map(i1,:)-q_ref)
       q_iptbz(i1,:)=nint(v1)
       call k_grid_shift(q_iptbz(i1,:))
       if (q_map(q_iptbz(i1,1),q_iptbz(i1,2),q_iptbz(i1,3))/=0) then
         call warning('[RL indx] 2 equivalent points in the rlu grid found')
         q_map=0
         return
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
             v1=matmul(k_b_m1,qpt_map(i1,:)-q_ref+real((/i2,i3,i4/)))
             iv1=nint(v1)
             call k_grid_shift(iv1)
             if (q_map(iv1(1),iv1(2),iv1(3))==0) q_map(iv1(1),iv1(2),iv1(3))=i1
           enddo
         enddo
       enddo
     enddo
   end subroutine
   !
   subroutine define_q_map_new(qpt_map,q_ref)
     !
     use pars,      ONLY:zero_dfl
     !
     implicit none
     !
     real(SP)  :: qpt_map(Xk%nbz,3)
     real(SP), intent(in)     :: q_ref(3)
     !
     ! Work space
     !
     real(SP) :: qpt_save(Xk%nbz,3),q_step(3)
     integer  :: id
     !
     qpt_save=qpt_map
     !
     ! Remove shift if present 
     do id=1,3
       qpt_map(:,id)=qpt_map(:,id)-q_ref(id)
     enddo
     !
     qpt_map(:,:)=qpt_map(:,:)-nint(qpt_map(:,:)) ! Bring in the BZ
     !  
     ! Bring k-points between [0,1)
     !
     where(qpt_map(:,:)<-zero_dfl)
       qpt_map(:,:)=1._SP+qpt_map(:,:)     
     end where
     !
     ! Find the smallest q in the 3 direction 
     !  
     call k_find_smallest_q(qpt_map,q_step,Xk%nbz)
     !
     q_map=0
     do i1=1,Xk%nbz
       !
       do id=1,3
         !
         if(q_step(id)==0._SP) then ! Non periodic directions
           q_iptbz(i1,id)=1
           cycle
         endif
         !
         q_iptbz(i1,id)=nint(qpt_map(i1,id)/q_step(id))+1
         !
       enddo
       !
       q_map(q_iptbz(i1,1),q_iptbz(i1,2),q_iptbz(i1,3))=i1
       !
     enddo
     !
     qpt_map=qpt_save
     !
     if(any(q_map==0)) then
       call error('[RL indx] error in mapping')
       q_map=0
       return
     endif
     !
   end subroutine
   !
   subroutine q_shadows(force_gamma_only)
     use vec_operate,    ONLY:iku_v_norm
     logical                ::force_gamma_only
     !
     ! Gamma only, deallocate and reallocate 
     ! using Gamma point only definition
     !
     if (force_gamma_only) then
       YAMBO_FREE(k_pt)
       YAMBO_FREE(q_pt)
       YAMBO_FREE(q_norm)
       YAMBO_FREE(q_sstar)
       YAMBO_FREE(q%pt)
       nqbz=1
       nqibz=1
       q%nibz=1
       q%nbz=1
     endif
     !
     if (.not.allocated(q_pt)) then
       YAMBO_ALLOC(q_pt,(q%nibz,3))
     endif
     if (.not.allocated(k_pt)) then
       YAMBO_ALLOC(k_pt,(k%nibz,3))
     endif
     if (.not.allocated(q_norm)) then
       YAMBO_ALLOC(q_norm,(q%nibz))
     endif
     if (allocated(q%sstar).and..not.allocated(q_sstar)) then
       YAMBO_ALLOC(q_sstar,(q%nbz,2))
     endif
     !
     if (force_gamma_only) then
       YAMBO_ALLOC(q%pt,(q%nibz,3))
       if (.not.allocated(q_sstar)) then
         YAMBO_ALLOC(q_sstar,(q%nbz,2))
       endif
       q%pt=0._SP
       q_pt=0._SP
       k_pt=k%pt
       if(allocated(q_sstar)) q_sstar=1
       q_norm=q0_def_norm
     else
       q_pt=q%pt
       k_pt=k%pt
       do i1=1,q%nibz
         q_norm(i1)=iku_v_norm(q_pt(i1,:))
       enddo
       q_norm(1)=q0_def_norm
       q_sstar=q%sstar
     endif
     !
   end subroutine
   !
end subroutine
