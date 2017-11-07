!
!        Copyright (C) 2000-2014 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM DS
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
subroutine COLLISIONS_NEQ_GW_static(X,Xk,E,k,q,Xw)
 !
 ! NEQ GW collisions 
 !
 use pars,          ONLY:SP,rZERO
 use frequency,     ONLY:w_samp
 use electrons,     ONLY:levels
 use LIVE_t,        ONLY:live_timing
 use com,           ONLY:error
 use parallel_m,    ONLY:PP_redux_wait,PAR_PLASMA_index,&
&                        PAR_IND_Q,PAR_Q_index,PAR_COM_QP_A2A,PAR_IND_Plasma,&
&                        PAR_nQ,PAR_COM_G_b_A2A,PAR_COM_Q_INDEX
 use collision_el,  ONLY:elemental_collision,elemental_collision_free,elemental_collision_alloc
 use collision_ext, ONLY:COLLISIONS_parallel_SC_structure,&
&                        GW_NEQ_COLL_element,COLL_bands,GW_NEQ_collisions
 use plasma,        ONLY:EH_gas
#if defined _TIMING
 use timing_m,      ONLY:timing
#endif
 use functions,     ONLY:bose_f
 use IO_m,          ONLY:io_control,OP_RD_CL,REP,VERIFY,NONE,Io_and_Messaging_switch,io_COLLs
 use X_m,           ONLY:X_alloc,X_mat,X_t
 use interfaces,    ONLY:WF_load
 use wave_func,     ONLY:WF
 use R_lattice,     ONLY:qindx_S,qindx_C,bz_samp
 use D_lattice,     ONLY:DL_vol
 use wrapper,       ONLY:M_by_V
 !
#include<memory.h>
 !
 type(levels)             ::E
 type(bz_samp)            ::k,q,Xk
 type(X_t)                ::X
 type(w_samp)             ::Xw
 !
 ! Work Space
 !
 integer                  ::i_coll1,i_coll2,i1,i2,iqbz,iqibz,alloc_err,iqs,&
&                           i_n,i_k,i_k_s,i_k_sp,i_J,i_J_mem,PAR_NC_local,&
&                           i_np,i_kmq,i_kmq_bz,i_kmq_s,i_kmq_sp,i_kmq_G0,&
&                           i_m,i_p,i_p_bz,i_p_s,i_p_sp,i_p_star,PAR_nPLASMA_local,PAR_nPLASMA_step,&
&                           i_mp,i_pmq,i_pmq_s,i_pmq_sp,i_pmq_G0,i_pmq_bz,i_q_mem,i_coll1_mem
 complex(SP), allocatable ::EM1s(:,:)
 type(elemental_collision)::isc,iscp
 integer          ::io_err,ID
 integer, external::io_X
 complex(SP)      ::local_rhotw(X%ng),pre_factor
 !
 ! Reset...
 !
 alloc_err=0
 !
 call elemental_collision_free(isc)
 call elemental_collision_free(iscp)
 !
 ! Setup...
 !
 call k_build_up_BZ_tables(k)
 call k_build_up_BZ_tables(q)
 call k_expand(k)
 !
 ! Dimensions...
 !
 isc%iqref =0
 isc%ngrho =X%ng
 iscp%ngrho=X%ng
 !
 ! WFs...
 !
 call WF_load(WF,isc%ngrho,maxval(qindx_S(:,:,2)),COLL_bands,(/1,k%nibz/),title='-SC')
 !
 ! Plasmon-Pole/Static interaction DB I/O
 !
 call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1,2/),MODE=VERIFY,ID=ID)
 io_err=io_X(X,Xw,ID)
 !
 if (io_err<0) call error('Incomplete and/or broken PPA/Static diel. fun. database')
 !
#if defined _TIMING
 call timing('NEQ GW collisions',OPR='start')
#endif
 !
 ! Test the spatial Inversion
 !   
 call WF_spatial_inversion(E,Xk)
 !
 ! ALLOCATION
 !------------
 !
 call X_alloc('X',(/X%ng,X%ng,1/))
 YAMBO_ALLOC(EM1s,(X%ng,X%ng))
 !
 call elemental_collision_alloc(isc, NG=isc%ngrho,NG_GAMP=(/X%ng,X%ng/),TITLE="NEQ_GW")
 call elemental_collision_alloc(iscp,NG=isc%ngrho,TITLE="NEQ_GW")
 !
 ! Parallel Indexes & LIVE-TIMING STEPS
 !
 PAR_NC_local=GW_NEQ_collisions%PAR_N
 if (allocated(GW_NEQ_collisions%IO_status)) then
   PAR_NC_local=0
   do i_coll1=1,GW_NEQ_collisions%N
     if (.not.GW_NEQ_collisions%PAR_IND%element_1D(i_coll1)) cycle
     if (GW_NEQ_collisions%IO_status(i_coll1)/=-1) cycle
     PAR_NC_local=PAR_NC_local+1
   enddo
 endif
 !
 PAR_nPLASMA_step =max(1,count(PAR_IND_Plasma%element_1D(:))/100)
 PAR_nPLASMA_local=count(PAR_IND_Plasma%element_1D(:))/PAR_nPLASMA_step
 !
 call live_timing("Collisions",PAR_NC_local*PAR_nQ*PAR_nPLASMA_local)
 !
 ! Write Database Header 
 !
 if (io_COLLs) call COLLISIONS_linearize_and_IO("GW_NEQ",0,GW_NEQ_collisions,GW_NEQ_COLL_element)
 !
 do i_coll1=1,GW_NEQ_collisions%N
   !
   if (.not.GW_NEQ_collisions%PAR_IND%element_1D(i_coll1)) cycle
   !
   i_coll1_mem=GW_NEQ_collisions%PAR_map(i_coll1)
   !
   if (io_COLLs) then
     if (GW_NEQ_collisions%IO_status(i_coll1)/=-1) cycle
     GW_NEQ_COLL_element(1)%v3_r=rZERO
   endif
   !
   do iqbz=1,q%nbz 
     !
     iqibz    =q%sstar(iqbz,1)
     iqs      =q%sstar(iqbz,2)
     !
     isc%qs   =(/0,iqibz,iqs/)
     iscp%qs  =(/0,iqibz,iqs/)
     !
     if (iqibz/=isc%iqref) then
       !
       call scatter_Gamp(isc,'c')
       !
       call Io_and_Messaging_switch("-fragments_IO_log")
       !
       ! I/O
       !
       if (iqbz ==1) call io_control(ACTION=OP_RD_CL,COM=NONE, SEC=(/1,2,2*iqibz+1/),ID=ID)
       if (iqbz>  1) call io_control(ACTION=OP_RD_CL,COM=NONE, SEC=(/2*iqibz,2*iqibz+1/),ID=ID)
       io_err=io_X(X,Xw,ID)
       !
       call Io_and_Messaging_switch("+fragments_IO_log")
       !
       ! Poles and Residuals
       !
       forall(i1=1:X%ng) X_mat(i1,i1,1)=X_mat(i1,i1,1)+1._SP
       !
#if defined _OPENMP
!$omp parallel do default(shared), private(i1,i2)
#endif
       do i1=1,X%ng
         do i2=1,X%ng
           EM1s(i1,i2)=X_mat(i1,i2,1)*isc%gamp(i1,i2)
         enddo
       enddo
#if defined _OPENMP
!$omp end parallel do
#endif
       if (iqibz==1) EM1s(1,1)=X_mat(1,1,1)*sqrt(isc%gamp(1,1)/DL_vol/real(q%nbz,SP))
       !
     endif
     !
     if (.not.PAR_IND_Q%element_1D(iqbz)) cycle
     i_q_mem=PAR_Q_index(iqbz)
     !
     ! Scattering geometry (I)
     !=========================
     i_n     = GW_NEQ_collisions%state(i_coll1,1)
     i_np    = GW_NEQ_collisions%state(i_coll1,2)
     i_k     = GW_NEQ_collisions%state(i_coll1,3)
     !
     i_k_s   = 1
     !
     i_kmq_bz =qindx_S(i_k,iqbz,1)
     i_kmq_G0 =qindx_S(i_k,iqbz,2)
     !
     i_kmq   = k%sstar(i_kmq_bz,1)
     i_kmq_s = k%sstar(i_kmq_bz,2)
     !
     ! Spins
     !
     i_k_sp  = GW_NEQ_collisions%state(i_coll1,4)
     i_p_sp  = i_k_sp
     i_kmq_sp= i_k_sp
     i_pmq_sp= i_k_sp
     !
     isc%is  =(/i_n ,i_k,i_k_s,i_k_sp/)
     isc%os  =(/i_np,i_kmq,i_kmq_s,i_kmq_sp/)
     !
     isc%qs(1)=i_kmq_G0
     !
     call scatter_Bamp(isc)
     !
     do i_coll2=1,GW_NEQ_collisions%N
       !
       i_m     = GW_NEQ_collisions%state(i_coll2,1)
       i_mp    = GW_NEQ_collisions%state(i_coll2,2)
       i_p     = GW_NEQ_collisions%state(i_coll2,3)
       !
       do i_p_star=1,k%nstar(i_p)
         !
         ! Scattering geometry (II)
         !==========================
         !
         i_p_s =k%star(i_p,i_p_star)
         i_p_bz=k%k_table(i_p,i_p_s)
         !
         i_pmq_bz = qindx_C(i_p_bz,iqbz,1)
         i_pmq_G0 = qindx_C(i_p_bz,iqbz,2)
         !
         i_pmq   = k%sstar(i_pmq_bz,1)
         i_pmq_s = k%sstar(i_pmq_bz,2)
         !
         ! (i_m,i_p,i_p_s)--<--.--<--(i_mp,i_pmq,i_pmq_s)
         !                     |
         !                    /|\ W(i_pmq_G0,iqibz,iqs)
         !                     |
         !     (i_n,i_k,1)-->--.-->--(i_np,i_kmq,i_kmq_s)
         !
         ! W_{nn' k}{mm' p}^{q} = [W_{n'n k+q}{m'm p+q}^{-q}]^{*} = W_{mm' p}{nn' k}^{q}
         !
         i_J=EH_gas%tab_m1(i_coll2,i_p_s,i_q_mem)
         !
         if (.not.PAR_IND_Plasma%element_1D(i_J)) cycle
         !
         i_J_mem=PAR_PLASMA_index(i_J)
         !
         iscp%is =(/i_m,i_p,i_p_s,i_p_sp/)
         iscp%os =(/i_mp,i_pmq,i_pmq_s,i_pmq_sp/)
         !
         iscp%qs(1)=i_pmq_G0
         !
         call scatter_Bamp(iscp)
         !
         call M_by_V('N', X%ng, EM1s, conjg(iscp%rhotw), local_rhotw)
         !
         pre_factor=0._SP
#if defined _OPENMP
!$omp parallel do default(shared), private(i1), reduction(+:pre_factor)
#endif
         do i1=1,X%ng
           pre_factor=pre_factor+isc%rhotw(i1)*local_rhotw(i1)
         enddo
#if defined _OPENMP
!$omp end parallel do
#endif
         !
         if (io_COLLs) then
           !
           GW_NEQ_COLL_element(1)%v3_r(i_J,1,iqbz)=&
&             GW_NEQ_COLL_element(1)%v3_r(i_J,1,iqbz)                 +abs(pre_factor)**2
           !
         else
           !
           GW_NEQ_COLL_element(i_coll1_mem)%v3_r(i_J_mem,1,i_q_mem)=&
&             GW_NEQ_COLL_element(i_coll1_mem)%v3_r(i_J_mem,1,i_q_mem)+abs(pre_factor)**2
           !
         endif
         !
         if ( mod(count(PAR_IND_Plasma%element_1D(:i_J)),PAR_nPLASMA_step)==0 ) call live_timing(steps=1)
         !
       enddo
       !
     enddo
     !
   enddo 
   !
   if (io_COLLs) then
     if (COLLISIONS_parallel_SC_structure) then
       call PP_redux_wait(GW_NEQ_COLL_element(1)%v3_r,COMM=PAR_COM_QP_A2A%COMM)
       call PP_redux_wait(GW_NEQ_COLL_element(1)%v3_r,COMM=PAR_COM_Q_INDEX%COMM)
     else
       call PP_redux_wait(GW_NEQ_COLL_element(1)%v3_r,COMM=PAR_COM_G_b_A2A%COMM)
     endif
   endif
   !
   if (io_COLLs) call COLLISIONS_linearize_and_IO("GW_NEQ",i_coll1,GW_NEQ_collisions,GW_NEQ_COLL_element)
   !
 enddo
 !
 call live_timing()
 !
#if defined _TIMING
 call timing('NEQ GW collisions',OPR='stop')
#endif
 !
 ! CLEAN
 !
 YAMBO_FREE(EM1s)
 call X_alloc('X')
 call elemental_collision_free(isc)
 call elemental_collision_free(iscp)
 !
end subroutine
