!
!        Copyright (C) 2000-2018 the YAMBO team
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
subroutine QP_real_axis(X,Xen,Xk,en,k,q,qp,Xw,GW_iter)
 !
 ! This routine calculates 
 ! 
 ! [1] Real axis GW S.E.
 ! [2] Electronic Lifetimes
 !
 use pars,          ONLY:SP,schlen,pi,cZERO,cI
 use units,         ONLY:HA2EV
 use drivers,       ONLY:l_life
 use electrons,     ONLY:levels,spin,spin_occ
 use frequency,     ONLY:w_samp,coarse_grid_index,coarse_grid_N,W_reset
 use LIVE_t,        ONLY:live_timing
 use com,           ONLY:msg
 use parallel_m,    ONLY:PP_redux_wait,PAR_IND_Xk_ibz,PAR_IND_G_b,PAR_IND_QP,&
&                        PAR_IND_Q,PAR_QP_index,PAR_Q_bz_index,PAR_G_bands_index,&
&                        n_WF_bands_to_load,PAR_Q_index,PAR_nQP,&
&                        PAR_nG_bands,PAR_nQ_bz
 use interfaces,    ONLY:PARALLEL_global_indexes,PARALLEL_WF_index,WF_load,WF_free,PARALLEL_WF_distribute
 use collision_el,  ONLY:elemental_collision,elemental_collision_free,elemental_collision_alloc
 use wave_func,     ONLY:WF
 use IO_m,          ONLY:io_control,REP,VERIFY,DUMP,NONE,RD,RD_CL,OP_RD,&
&                        OP_WR_CL,OP_APP_WR_CL,RD_CL_IF_END,OP_RD_CL,io_RESPONSE,io_SCREEN
 use QP_m,          ONLY:QP_t,QP_n_G_bands,QP_dSc_steps,QP_Sc,QP_n_states,QP_G_damp,QP_table,QP_dSc_delta,&
&                        QP_W,QP_solver,QP_W_er,QP_W_dr,QP_n_W_freqs,QP_Sc_steps,&
&                        QP_n_W_freqs_redux,e2et,h2ht
 use X_m,           ONLY:X_alloc,X_mat,X_t
 use functions,     ONLY:bose_decay
 use R_lattice,     ONLY:qindx_S,bz_samp
 use D_lattice,     ONLY:nsym,i_space_inv,i_time_rev,mag_syms
 use stderr,        ONLY:intc
 use wrapper,       ONLY:Vstar_dot_V,V_dot_V
 use wrapper_omp,   ONLY:Vstar_dot_V_omp,V_dot_V_omp
#if defined _TIMING
 use timing_m,      ONLY:timing
#endif
 !
#include<memory.h>
 type(levels) ::en,Xen
 type(bz_samp)::Xk,k,q
 type(X_t)    ::X
 type(QP_t)   ::qp
 type(w_samp) ::Xw
 integer      ::GW_iter
 !
 ! WorkSpace
 !
 type(elemental_collision) ::scattering_main,scattering_prime
 type(w_samp)     ::Sc_W(qp%n_states),X_life_W(q%nibz)
 integer          ::i1,i2,i3,i4,i_or,iqbz,iqibz,ib,i_err,i_q_to_start,iqs,&
&                   iv4(4),io_err,X_id,WID,timing_steps,i_q_mem,X_range(2),i_qp,i_qp_mem,i_q_W_mem,i_b_mem
 complex(SP)      :: lrhotw(X%ng),X_mat_ws(X%ng,X%ng),W_dummy
 real(SP)            ::life_Fe,life_Fh
 integer, allocatable::life_W_table(:,:)
 integer, external   ::io_X,QP_state_extract,io_QP_and_GF,X_dielectric_matrix,QP_life_transitions
 character(schlen)   ::ch,Sc_name
 logical             ::X_is_TR_rotated
 !
 ! Presets
 !=========
 !
 call elemental_collision_free(scattering_main)
 call elemental_collision_free(scattering_prime)
 !
 if (l_life) then
   do iqibz=1,q%nibz
     call W_reset(X_life_W(iqibz))
     X_life_W(iqibz)%dr=Xw%dr
   enddo
 else
   do i_qp=1,qp%n_states
     call W_reset(Sc_W(i_qp))
   enddo
 endif
 !
 call k_expand(k)
 !
 ! ALLOCATION 
 !============
 !
 ! W energy and damping ranges
 !-----------------------------
 YAMBO_ALLOC(QP_W_er,(q%nbz,2))
 YAMBO_ALLOC(QP_W_dr,(q%nbz,2))
 !
 ! Sc Energy points (1 type each QP state !)
 !-------------------------------------------
 if (trim(QP_solver)=='n') then
   do i_qp=1,qp%n_states
     Sc_W(i_qp)%n_freqs=QP_dSc_steps
     YAMBO_ALLOC(Sc_W(i_qp)%p,(Sc_W(i_qp)%n_freqs))
     forall (i2=1:QP_dSc_steps) Sc_W(i_qp)%p(i2)=qp%E_bare(i_qp)+(i2-1)*QP_dSc_delta
   enddo
 else if (trim(QP_solver)=='g') then
   !
   ! I need to put to 0. the G damping embodied in QP_W2Sc
   !
   QP_G_damp=0._SP
   !
   do i_qp=1,qp%n_states
     Sc_W(i_qp)%n_freqs =QP_Sc_steps
     call FREQUENCIES_Green_Function(i_qp,Sc_W(i_qp),en%E,.FALSE.)
     QP_Sc_steps=Sc_W(i_qp)%n_freqs
   enddo
 endif
 !
 ! Messagging 
 !============
 !
 Sc_name="G"//trim(intc(GW_iter))
 !
 if (GW_iter==0) call section('+',trim(Sc_name)//"W0 on the real axis")
 if (GW_iter> 0) call section('=',trim(Sc_name)//"W0 on the real axis")
 !
 if (.not.l_life) call msg('r', '[GW] Bands range     :',QP_n_G_bands)
 call msg('r', '[GW] G damping   [ev]:',QP_G_damp*HA2EV)
 call msg('r','')
 iv4=(/1,1,0,0/)
 do while(QP_state_extract(iv4)>0)
   write (ch,'(4(a,i3.3))') 'QP @ K ',iv4(1),' - ',iv4(2),' : b ',iv4(3),' - ',iv4(4)
   call msg('r',trim(ch))
 enddo
 call msg('r','')
 !
 if (.not.l_life) then
   !
   ! W DB (I/O)
   !============
   i_q_to_start= 1
   io_err     =-1
   if (io_SCREEN) then
     call io_control(ACTION=OP_RD,COM=REP,SEC=(/1,2/),MODE=VERIFY,ID=WID)
     io_err=io_QP_and_GF('W',qp,WID)
     if (io_err>0) i_q_to_start=io_err
   endif
   !
   ! W => Sc after succesfull I/O
   !==============================
   !
   if (io_err==0) then
     !
     ! ALLOCATION (W)
     !==================
     QP_n_W_freqs=Xw%n_freqs
     QP_n_W_freqs_redux=Xw%n_freqs
     YAMBO_ALLOC(QP_W,(QP_n_states,1,QP_n_G_bands(2),QP_n_W_freqs))
     !
     if (QP_solver=="n".or.QP_solver=="g") then
       QP_Sc=cZERO
       call live_timing(trim(Sc_name)//'W0',q%nbz)
       do iqbz=1,q%nbz
         if (iqbz< q%nbz) call io_control(ACTION=RD,COM=NONE,SEC=(/2+iqbz/),ID=WID)
         if (iqbz==q%nbz) call io_control(ACTION=RD_CL,COM=NONE,SEC=(/2+iqbz/),ID=WID)
         io_err=io_QP_and_GF('W',qp,WID)
         !
         ! Xw%p is stored in ioX, not in ioW !
         !
         Xw%er=QP_W_er(iqbz,:)
         Xw%dr=QP_W_dr(iqbz,:)
         Xw%n_freqs=QP_n_W_freqs
         call FREQUENCIES_setup(Xw)
         ! 
         call QP_W2Sc(iqbz,k,en,Xw,Sc_W)
         call live_timing(steps=1)
       enddo
       YAMBO_FREE(QP_W)
       call live_timing()
       !
       return
       !
     endif
     !
     if (QP_solver=="s") return
     !
   endif
   !
 endif
 !
 !=========================
 ! Actual Calculation Body
 !=========================
 !
#if defined _TIMING
 call timing('GW(real-axis)',OPR='start')
#endif
 !
 ! Lifetimes Transitions 
 !=======================
 if (l_life) then
   !
   call section('=','Lifetimes Transitions Selector')
   !
   QP_n_W_freqs=QP_life_transitions(-1,en,k,q,X_life_W(1))
   YAMBO_ALLOC(life_W_table,(q%nibz,QP_n_W_freqs))
   life_W_table=0
   QP_n_W_freqs_redux=-1
   do iqibz=1,q%nibz
     !
     X_life_W(iqibz)%dr=Xw%dr
     !
     i1=QP_life_transitions(iqibz,en,k,q,X_life_W(iqibz))
     life_W_table(iqibz,:size(coarse_grid_index))=coarse_grid_index(:)
     !
     QP_n_W_freqs_redux=max(QP_n_W_freqs_redux,coarse_grid_N)
     YAMBO_FREE(coarse_grid_index)
     !
   enddo
   !
   do iqibz=1,q%nibz
     X%iq=iqibz
     i_err=X_dielectric_matrix(Xen,Xk,q,X,X_life_W(iqibz),iqibz>1)
     call PP_redux_wait()
   enddo
   !
 endif
 !
 ! Dynamical interaction DB I/O read to dump the first section variables.
 !
 if (io_RESPONSE) then
   call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1,2/),MODE=DUMP,ID=X_id)
   if (l_life)      i_err=io_X(X,X_life_W(1),X_id)
   if (.not.l_life) i_err=io_X(X,Xw,X_id)
 endif
 !
 ! WF distributed & load
 !=======================
 !
 call PARALLEL_global_indexes(Xen,Xk,q,"Self_Energy")
 call PARALLEL_WF_distribute(K_index=PAR_IND_Xk_ibz,B_index=PAR_IND_G_b,CLEAN_UP=.TRUE.)
 call PARALLEL_WF_distribute(QP_index=PAR_IND_QP)
 call PARALLEL_WF_index( )
 !
 ! Redefine i_q_to_start to be CPU-dependent
 !
 do iqbz=1,q%nbz 
   if (PAR_IND_Q%element_1D(q%sstar(iqbz,1))) then
     i_q_to_start=iqbz
     exit
   endif
 enddo
 !
 ! WFs
 !
 call WF_load(WF,X%ng,maxval(qindx_S(:,:,2)),(/QP_n_G_bands(1),n_WF_bands_to_load/),(/1,k%nibz/),title='-Sc')
 !
 ! W
 !
 if (.not.l_life) then
   QP_n_W_freqs=Xw%n_freqs
   QP_n_W_freqs_redux=Xw%n_freqs
   YAMBO_ALLOC(QP_W,(PAR_nQP,PAR_nQ_bz,PAR_nG_bands,QP_n_W_freqs))
   QP_W=cZERO
 endif
 !
 ! Test the spatial Inversion
 !
 call WF_spatial_inversion(en,Xk)
 !
 ! ALLOCATION (scattering_main)
 !
 call X_alloc('X',(/X%ng,X%ng,QP_n_W_freqs_redux/))
 !
 call elemental_collision_alloc(scattering_main,NG=X%ng,NG_GAMP=(/X%ng,X%ng/),TITLE="GW")
 call elemental_collision_alloc(scattering_prime,NG=X%ng,TITLE="GW")
 !
 QP_Sc=cZERO
 scattering_main%iqref=0
 !
 ! Main Loop
 !===========
 !
 call PP_redux_wait()
 !
 ! ... Timing...
 !--------------
 timing_steps=0
 do iqbz=i_q_to_start,q%nbz
   if (.not.PAR_IND_Q%element_1D( q%sstar(iqbz,1) )) cycle
   timing_steps=timing_steps+1
 enddo
 !
 timing_steps=timing_steps*PAR_nQP*count( PAR_IND_G_b%element_1D(QP_n_G_bands(1):QP_n_G_bands(2)) )
 !
 if (.not.l_life) call live_timing(trim(Sc_name)//'W0',timing_steps)
 if (l_life)      call live_timing('Lifetimes',timing_steps)
 !
 do iqbz=i_q_to_start,q%nbz
   !
   if (.not.l_life.and.io_SCREEN) QP_W=cZERO
   !
   scattering_main%qs(2:)=(/q%sstar(iqbz,1),q%sstar(iqbz,2)/)
   iqibz=scattering_main%qs(2)
   iqs  =scattering_main%qs(3)
   !
   i_q_W_mem=PAR_Q_bz_index(iqbz)
   !
   if (.not.PAR_IND_Q%element_1D(iqibz)) cycle
   !
   if (iqibz/=scattering_main%iqref) then
     !
     i_q_mem=PAR_Q_index(iqibz)
     X_range=(/1,QP_n_W_freqs_redux/)
     !
     if (.not.io_RESPONSE) X_range=(/QP_n_W_freqs_redux*(i_q_mem-1)+1,QP_n_W_freqs_redux*(i_q_mem-1)+X_life_W(iqibz)%n_freqs/)
     !
     call scatter_Gamp(scattering_main,'c')
     !
     ! I/O
     !
     if (io_RESPONSE) then
       !
       if (iqbz ==i_q_to_start) call io_control(ACTION=OP_RD,COM=NONE,       SEC=(/1,2,2*iqibz+1/),ID=X_id)
       if (q%nbz==1           ) call io_control(ACTION=OP_RD_CL,COM=NONE,    SEC=(/1,2,3/),ID=X_id)
       if (iqbz > i_q_to_start) call io_control(ACTION=RD_CL_IF_END,COM=NONE,SEC=(/2*iqibz,2*iqibz+1/),ID=X_id)
       !
       if (l_life) then
         !
         io_err=io_X(X,X_life_W(iqibz),X_id)
         !
       else
         !
         io_err=io_X(X,Xw,X_id)
         !
         ! Xw%er/dr/n are not known here and are not read from ioX so
         ! I need to redefine them in terms of Xw%p (read from ioX)
         !
         Xw%er=(/real(Xw%p(1)),real(Xw%p(Xw%n_freqs))/)
         Xw%dr=(/aimag(Xw%p(1)),aimag(Xw%p(Xw%n_freqs))/)
         !
       endif
       !
     endif
     !
     if (l_life) then
       call X_delta_part(X%ng,X_life_W(iqibz)%n_freqs,X_mat(:,:,X_range(1):X_range(2)),scattering_main%gamp)
     else
       call X_delta_part(X%ng,Xw%n_freqs,X_mat(:,:,X_range(1):X_range(2)),scattering_main%gamp)
     endif
     !
     if (l_life) QP_n_W_freqs=0
     !
     X_is_TR_rotated=.false.
     !
   endif
   !
   if (iqs>nsym/(i_time_rev+1) .and. (i_space_inv==0.or.mag_syms) .and..not.X_is_TR_rotated) then
     X_is_TR_rotated=.true.
     do i3=X_range(1),X_range(2)
       forall(i1=1:X%ng,i2=1:X%ng) X_mat_ws(i2,i1)=X_mat(i1,i2,i3)
       X_mat(:,:,i3)=X_mat_ws(:,:)
     enddo
   endif
   !
   qp_loop: do i_qp=1,QP_n_states
     !
     i_qp_mem=PAR_QP_index(i_qp)
     !
     scattering_main%is=(/QP_table(i_qp,1),QP_table(i_qp,3),1,spin(QP_table(i_qp,:))/)
     scattering_main%os(2:)=(/k%sstar(qindx_S(scattering_main%is(2),iqbz,1),:),spin(QP_table(i_qp,:))/)
     scattering_prime%is=(/QP_table(i_qp,2),QP_table(i_qp,3),1,spin(QP_table(i_qp,:))/)
     !
     scattering_main%qs(1)=qindx_S(QP_table(i_qp,3),iqbz,2)
     scattering_prime%qs=scattering_main%qs
     !
     if (.not.PAR_IND_QP%element_1D(i_qp)) then
       !
       ! Bug spotted by AM (Feb 2015). The QP_n_W_freqs was erronously accumulated
       ! in the loop when the code was run in parallel. These lines are a short work-around
       ! that should be replaced by a full parallelization also of the frequencies tabulation
       !
       if (l_life) then
         do ib=QP_n_G_bands(1),QP_n_G_bands(2)
           scattering_main%os(1)=ib
           i_or=IOR(e2et((/scattering_main%is(:2),scattering_main%is(4)/),            &
&                        (/scattering_main%os(:2),scattering_main%os(4)/),en,life_Fe),&
&                   h2ht((/scattering_main%is(:2),scattering_main%is(4)/),            &
&                        (/scattering_main%os(:2),scattering_main%os(4)/),en,life_Fh))
           QP_n_W_freqs=QP_n_W_freqs+i_or
         enddo
       endif
       cycle qp_loop
     endif
     !
     bands_loop: do ib=QP_n_G_bands(1),QP_n_G_bands(2)
       !
       scattering_main%os(1)=ib
       !
       i_b_mem=PAR_G_bands_index(ib)
       !
       if (l_life) then
         i_or=IOR(e2et((/scattering_main%is(:2),scattering_main%is(4)/),            &
&                      (/scattering_main%os(:2),scattering_main%os(4)/),en,life_Fe),&
&                 h2ht((/scattering_main%is(:2),scattering_main%is(4)/),            &
&                      (/scattering_main%os(:2),scattering_main%os(4)/),en,life_Fh))
         QP_n_W_freqs=QP_n_W_freqs+i_or
       endif
       !
       ! The Parallel cycle has to be here in order to have a proper accumulation of QP_n_W_freqs
       !
       if (.not.PAR_IND_G_b%element_1D(ib))  cycle bands_loop
       !
       scattering_prime%os=scattering_main%os
       !
       call live_timing(steps=1)
       !
       if (l_life.and.i_or==0) cycle
       !
       call scatter_Bamp(scattering_main)
       scattering_prime%rhotw=scattering_main%rhotw
       !
       if (any(scattering_main%is/=scattering_prime%is)) call scatter_Bamp(scattering_prime)
       !
       if (l_life) then
         !
         i2=life_W_table(iqibz,QP_n_W_freqs)
         !
         do i3=1,X%ng
           lrhotw(i3)=V_dot_V_omp(X%ng,scattering_main%rhotw,X_mat(1,i3,i2+X_range(1)-1))
         enddo
         !       
         ! To compensate the Tel/w divergence of the Bose function at finite
         ! Tel I multiply the X_mat function by [w/(Tel*Bose_E_cut)]^2
         !
         ! Note that the same procedure is applied in QP_W2Sc when S_c is
         ! calculated
         ! 
         qp%E(i_qp)=qp%E(i_qp)-2._SP*cI*pi*&
&                   (life_Fe+life_Fh)*bose_decay( real(X_life_W(iqibz)%p(i2)) )*&
&                   Vstar_dot_V_omp(X%ng,scattering_prime%rhotw,lrhotw)
         !
       else ! .not.l_life
         !
#if defined _OPENMP
         !
!$omp parallel do default(shared), private(i2,i3,i4)
         !
         do i2=1,QP_n_W_freqs
           W_dummy=cZERO
           do i3=1,X%ng
             do i4=1,X%ng
               W_dummy=W_dummy+scattering_main%rhotw(i3)*X_mat(i3,i4,i2+X_range(1)-1)*conjg(scattering_prime%rhotw(i4))
             enddo
           enddo
           QP_W(i_qp_mem,i_q_W_mem,i_b_mem,i2)=-4._SP/spin_occ*W_dummy
         enddo
         !
!$omp end parallel do
#else
         !
         do i2=1,QP_n_W_freqs
           !
           do i3=1,X%ng
             lrhotw(i3)=V_dot_V(X%ng,scattering_main%rhotw,X_mat(1,i3,i2+X_range(1)-1))
           enddo
           !
           W_dummy=Vstar_dot_V_omp(X%ng,scattering_prime%rhotw,lrhotw)
           !
           QP_W(i_qp_mem,i_q_W_mem,i_b_mem,i2)=-4._SP/spin_occ*W_dummy
           !
         enddo
#endif
         !
       endif
       !
     enddo bands_loop
   enddo qp_loop
   !
   if (.not.l_life) then
     !
     QP_W_er(iqbz,:)=Xw%er
     QP_W_dr(iqbz,:)=Xw%dr
     QP_n_W_freqs=Xw%n_freqs
     !
     if (trim(QP_solver)=='n'.or.trim(QP_solver)=='g') then
       !
       ! W(w) => \Sigma(w) via KK
       !
       call QP_W2Sc(iqbz,k,en,Xw,Sc_W)
       !
     endif
     !
     if (io_SCREEN) then
       !
       if (iqbz==1) then
         call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1,2,3/),ID=WID)
       else
         call io_control(ACTION=OP_APP_WR_CL,COM=REP,SEC=(/2+iqbz/),ID=WID)
       endif
       io_err=io_QP_and_GF('W',qp,WID)
       !
     endif
     !
   endif
   !
 enddo 
 !
 call live_timing()
 call PP_redux_wait()
 !
 ! CLEAN
 !
 if (l_life) then
   YAMBO_FREE(life_W_table)
   call PP_redux_wait(qp%E)
 else
   call PP_redux_wait(QP_Sc)
 endif
 !
 if (io_SCREEN) then
   if (trim(QP_solver)=='n'.or.trim(QP_solver)=='g') then
     YAMBO_FREE(QP_W)
   endif
 endif
 if (l_life) then
   do iqibz=1,q%nibz
     call W_reset(X_life_W(iqibz))
   enddo
 else
   do i_qp=1,qp%n_states
     call W_reset(Sc_W(i_qp))
   enddo
 endif
 !
 call X_alloc('X')
 call WF_free(WF)
 call elemental_collision_free(scattering_main)
 call elemental_collision_free(scattering_prime)
 call PP_redux_wait()
 !
#if defined _TIMING
 call timing('GW(real-axis)',OPR='stop')
#endif
 !
end subroutine
