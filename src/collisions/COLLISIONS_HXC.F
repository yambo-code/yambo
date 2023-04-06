!
!        Copyright (C) 2000-2022 the YAMBO team
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
subroutine COLLISIONS_HXC(X,Xk,E,k,q,Xw)
 !
 ! HXC collisions 
 !
 use drivers,       ONLY:l_use_COH_collisions
 use pars,          ONLY:SP,pi,cZERO
 use com,           ONLY:msg
 use frequency,     ONLY:w_samp
 use electrons,     ONLY:levels,spin_occ,spin,n_spin,n_spinor
 use LIVE_t,        ONLY:live_timing
 use hamiltonian,   ONLY:H_potential
 use D_lattice,     ONLY:DL_vol,sop_inv
 use tddft,         ONLY:tddft_wf_t
 use xc_functionals,ONLY:F_xc,V_xc,F_xc_mat,magn,XC_potential_driver
 use global_XC,     ONLY:WF_kind,WF_xc_functional
 use parallel_int,  ONLY:PP_redux_wait
 use parallel_m,    ONLY:PAR_IND_Q_bz,MPI_COMM_NULL,ncpu,myid,COMM_copy,yMPI_COMM,&
&                        PAR_n_Bp_mat_elements,PAR_COM_QP_A2A,PAR_COM_QP_INDEX,PAR_COM_Q_INDEX,&
&                        PAR_IND_Bp_mat,PAR_nQ_bz,PAR_COM_G_b_A2A,PAR_COM_G_b_index_global
 use collision_el,  ONLY:elemental_collision,elemental_collision_free,elemental_collision_alloc
 use collision_ext, ONLY:COLLISIONS_parallel_SC_structure,COLLISIONS_HXC_local,COLLISIONS_HXC_MB,&
&                        COLL_bands,HXC_COLL_element,HXC_collisions,COH_COLL_element,COH_collisions,&
&                        COLLISIONS_HXC_use_TDDFT,LAST_COLL_sync,PAR_COLL_min
 use timing_m,      ONLY:timing
 use QP_m,          ONLY:QP_ng_SH,QP_ng_Sx,QP_ng_Sc
 use IO_int,        ONLY:io_control,IO_and_Messaging_switch
 use IO_m,          ONLY:OP_RD_CL,REP,VERIFY,NONE,io_COLLs
 use X_m,           ONLY:X_ALLOC_elemental,X_mat,X_t
 use interfaces,    ONLY:WF_load,TDDFT_ALDA_R_space
 use wave_func,     ONLY:WF,WF_buffer,WF_buffered_IO
 use R_lattice,     ONLY:qindx_S,bz_samp,g_rot,minus_G
 use hamiltonian,   ONLY:B_mat_index
 use fft_m,         ONLY:fft_size
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
 integer                 :: i_coll,first_coll,last_coll,ig1,ig2,iqbz,iqibz,alloc_err,iqs,iGo_max,NG_max,NG_XC,LOCAL_COMM(2),&
&                           i_n,i_k,i_k_s,i_sp,i_m,i_mp,N_g_vecs_H,N_g_vecs_X,N_g_vecs_C,i_star,i_p,i_p_s,i_loop,&
&                           i_np,i_kmq,i_kmq_bz,i_kmq_s,i_kmq_G0,PAR_NC_local,n_steps,bands_to_load(2),n_colls
 logical                 :: l_HXC_collisions,eval_coll,l_compute_screening
 !
 type(elemental_collision):: isc_XC,iscp_XC,isc_H,iscp_H
 type(yMPI_comm)          :: LOCAL_yMPI_COMM
 type(tddft_wf_t), target :: tddft_wf
 real(SP)                 :: Co
 complex(SP)              :: pre_factor1,pre_factor2
 !
 complex(SP), allocatable :: EM1s(:,:),rhotw_tmp(:)
 integer                  :: io_err,ID_X,ID_HXC,ID_COH,TMP_VEC(ncpu)
 integer, external        :: io_X
 !
 call timing(trim(H_potential)//' Collisions',OPR='start')
 !
 ! Reset
 !
 alloc_err=0
 !
 call elemental_collision_free(isc_H)
 call elemental_collision_free(iscp_H)
 !
 call elemental_collision_free(isc_XC)
 call elemental_collision_free(iscp_XC)
 !
 ! Setup
 !
 l_compute_screening  = index(H_potential,"COH")>0   .or. index(H_potential,"SEX")>0
 l_HXC_collisions     = COLLISIONS_HXC_local .or. COLLISIONS_HXC_MB 
 !
 call k_build_up_BZ_tables(k)
 call k_build_up_BZ_tables(q)
 call k_expand(k)
 !
 if (l_compute_screening) then
   !
   ! Plasmon-Pole/Static interaction DB I/O
   !
   call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1,2/),MODE=VERIFY,ID=ID_X)
   io_err=io_X(X,Xw,ID_X)
   if (io_err<0) call error(' Incomplete and/or broken PPA/Static diel. fun. database')
   !
   if (X%ng<QP_ng_Sc) then
     QP_ng_Sc=X%ng
     call warning('SEX size (CORRLvcs) reduced to match the Screening size (NGsBlkXs)')
   endif 
   !
 endif
 !
 ! Dimensions
 !
 N_g_vecs_H =0
 N_g_vecs_X =0
 N_g_vecs_C =0
 iGo_max    =maxval(qindx_S(:,:,2))
 !
 if(COLLISIONS_HXC_local                      ) N_g_vecs_H  = QP_ng_SH
 if(COLLISIONS_HXC_MB                         ) N_g_vecs_X  = QP_ng_Sx
 if(COLLISIONS_HXC_MB .or.l_use_COH_collisions) N_g_vecs_C  = QP_ng_Sc
 !
 call fft_check_size(iGo_max,N_g_vecs_C,N_g_vecs_H,"SH")
 call fft_check_size(iGo_max,N_g_vecs_C,N_g_vecs_X,"SF")
 !
 if(COLLISIONS_HXC_local) QP_ng_SH=N_g_vecs_H
 if(COLLISIONS_HXC_MB) QP_ng_Sx=N_g_vecs_X
 !
 NG_max=maxval((/N_g_vecs_H,N_g_vecs_X,N_g_vecs_C/))
 NG_XC =max(N_g_vecs_X,N_g_vecs_C)
 !
 call msg('s', '['//trim(H_potential)//'] Plane waves (H,X,C) ',(/N_g_vecs_H,N_g_vecs_X,N_g_vecs_C/))
 !
 ! WFs
 !
 bands_to_load=COLL_bands
 if (COLLISIONS_HXC_use_TDDFT) then
   bands_to_load(1)=1
   bands_to_load(2)=max(COLL_bands(2),maxval(E%nbm))
 endif
 !
 if (WF_buffered_IO) call WF_load(WF_buffer,0,1,bands_to_load,(/1,k%nibz/),space='B',title='-BUFFER')
 !
 call WF_load(WF,NG_max,iGo_max,bands_to_load,(/1,k%nibz/),title='-'//trim(H_potential)//' Collisions')
 !
 if (COLLISIONS_HXC_local) then
   !
   iscp_H%qs   =1
   iscp_H%iqref=0
   iscp_H%ngrho=N_g_vecs_H
   !
   isc_H%qs    =1
   isc_H%iqref =0
   isc_H%ngrho =N_g_vecs_H
   !
   call elemental_collision_alloc(isc_H ,NG=N_g_vecs_H,NG_GAMP=(/N_g_vecs_H,    1   /),TITLE='HARTREE')
   call elemental_collision_alloc(iscp_H,NG=N_g_vecs_H,                                TITLE='HARTREE')
   !
   YAMBO_ALLOC(rhotw_tmp,(N_g_vecs_H))
   !
 endif
 !
 iscp_XC%qs   =1
 iscp_XC%iqref=0
 iscp_XC%ngrho=NG_XC
 !
 isc_XC%qs    =1
 isc_XC%iqref =0
 isc_XC%ngrho =NG_XC
 !
 call elemental_collision_alloc(iscp_XC,NG=NG_XC,NG_GAMP=(/N_g_vecs_X,  1  /),TITLE=trim(H_potential))
 !
 if (l_compute_screening) then
   !
   ! Test the spatial Inversion
   !   
   call WF_spatial_inversion(E,Xk)
   !
   ! Allocation (EM1S can be bigger than N_g_vecs_C)
   !
   call X_ALLOC_elemental('X',(/X%ng,X%ng,1/))
   YAMBO_ALLOC(EM1s,(N_g_vecs_C,N_g_vecs_C))
   !
   call elemental_collision_alloc(isc_XC, NG=NG_XC,NG_GAMP=(/N_g_vecs_C,N_g_vecs_C/),TITLE=trim(H_potential))
   !
 else
   !
   call elemental_collision_alloc(isc_XC, NG=NG_XC,                                  TITLE=trim(H_potential))
   !
 endif
 !
 Co=1._SP
 if (COLLISIONS_HXC_use_TDDFT) then
   Co=4._SP*pi/DL_vol/real(q%nbz,SP)
   YAMBO_ALLOC(tddft_wf%rhotwr1,(fft_size*n_spinor*n_spinor))
   YAMBO_ALLOC(tddft_wf%rhotwr2,(fft_size*n_spinor*n_spinor))
   YAMBO_ALLOC(tddft_wf%WF_symm1,(fft_size,n_spinor))
   YAMBO_ALLOC(tddft_wf%WF_symm2,(fft_size,n_spinor))
   YAMBO_ALLOC(F_xc,(fft_size,n_spin,n_spin))
   YAMBO_ALLOC(V_xc,(fft_size,n_spin))
   V_xc=0._SP
   if(n_spin>1) then
     YAMBO_ALLOC(magn,(fft_size,3))
   endif
   call XC_potential_driver(E,K,WF_kind,WF_xc_functional,2)
   YAMBO_ALLOC(F_xc_mat,(fft_size,n_spin,n_spin,n_spin,n_spin))
   call Build_F_xc_mat(V_xc,F_xc,F_xc_mat)
   YAMBO_FREE(F_xc)
   YAMBO_FREE(V_xc)
   if(n_spin>1) then
     YAMBO_FREE(magn)
   endif
 endif
 !
 n_colls=max(HXC_collisions%N,COH_collisions%N)
 !
 ! Parallel Indexes & LIVE-TIMING STEPS
 !
 PAR_NC_local=HXC_collisions%PAR_N
 PAR_COLL_min=0
 !
 first_coll=-1
 !
 do i_loop=1,2
   if (allocated(HXC_collisions%PAR_IND%element_1D)) then
     PAR_NC_local=0
     do i_coll=1,HXC_collisions%N
       if (.not.HXC_collisions%PAR_IND%element_1D(i_coll)) cycle
       last_coll=i_coll
       if (allocated(HXC_collisions%IO_status)) then
         if (HXC_collisions%IO_status(i_coll)/=-1) cycle
       endif
       if (first_coll==-1) first_coll=i_coll
       PAR_NC_local=PAR_NC_local+1
       if (i_loop==2.and.PAR_NC_local<=PAR_COLL_min) LAST_COLL_sync=i_coll
     enddo
   else if (allocated(COH_collisions%PAR_IND%element_1D)) then
     PAR_NC_local=0
     do i_coll=1,COH_collisions%N
       if (.not.COH_collisions%PAR_IND%element_1D(i_coll)) cycle
       last_coll=i_coll
       if (allocated(COH_collisions%IO_status)) then
         if (COH_collisions%IO_status(i_coll)/=-1) cycle
       endif
       if (first_coll==-1) first_coll=i_coll
       PAR_NC_local=PAR_NC_local+1
       if (i_loop==2.and.PAR_NC_local<=PAR_COLL_min) LAST_COLL_sync=i_coll
     enddo
   endif
   if(i_loop==1) then
     TMP_VEC=0
     TMP_VEC(myid+1)=PAR_NC_local
     call PP_redux_wait(TMP_VEC)
     PAR_COLL_min=minval(TMP_VEC)
   endif
 enddo
 !
 n_steps=0
 if (COLLISIONS_HXC_local)                      n_steps=n_steps+PAR_NC_local*K%nibz*PAR_n_Bp_mat_elements
 if (COLLISIONS_HXC_MB.or.l_use_COH_collisions) n_steps=n_steps+PAR_NC_local*PAR_nQ_bz*PAR_n_Bp_mat_elements
 !
 ! Define the local COMM
 !
 if (COLLISIONS_parallel_SC_structure) then
   LOCAL_COMM(1)=PAR_COM_QP_A2A%COMM
   LOCAL_COMM(2)=PAR_COM_Q_INDEX%COMM
   call COMM_copy(PAR_COM_QP_INDEX,LOCAL_yMPI_COMM)
 else
   LOCAL_COMM(1)=PAR_COM_G_b_A2A%COMM
   LOCAL_COMM(2)=MPI_COMM_NULL
   call COMM_copy(PAR_COM_G_b_index_global,LOCAL_yMPI_COMM)
 endif
 !
 ! Write Database Header 
 !
 if (l_HXC_collisions    ) call COLLISIONS_linearize_and_IO("HXC",0,last_coll,HXC_collisions,&
&                                                             HXC_COLL_element,ID_HXC,LOCAL_yMPI_COMM)
 if (l_use_COH_collisions) call COLLISIONS_linearize_and_IO("COH",0,last_coll,COH_collisions,&
&                                                             COH_COLL_element,ID_COH,LOCAL_yMPI_COMM)
 !
 call live_timing("Collisions",n_steps)
 !
 do i_coll=1,n_colls
   !
   if (l_HXC_collisions) then
     if(.not.HXC_collisions%PAR_IND%element_1D(i_coll)) cycle
   else if (l_use_COH_collisions) then
     if (.not.COH_collisions%PAR_IND%element_1D(i_coll)) cycle
   endif
   !
   if (io_COLLs.and.l_HXC_collisions) then
     if (HXC_collisions%IO_status(i_coll)/=-1) cycle
   endif
   if (io_COLLs.and.l_use_COH_collisions) then
     if (COH_collisions%IO_status(i_coll)/=-1) cycle
   endif
   !
   if( l_HXC_collisions ) then
     i_n     = HXC_collisions%state(i_coll,1)
     i_m     = HXC_collisions%state(i_coll,2)
     i_k     = HXC_collisions%state(i_coll,3)
     i_sp    = HXC_collisions%state(i_coll,4)
   elseif( index(H_potential,"COH")>0 ) then
     i_n     = COH_collisions%state(i_coll,1)
     i_m     = COH_collisions%state(i_coll,2)
     i_k     = COH_collisions%state(i_coll,3)
     i_sp    = COH_collisions%state(i_coll,4)
   endif   
   !
   if( l_HXC_collisions )     HXC_COLL_element(1)%v3_c=cZERO
   if( l_use_COH_collisions ) COH_COLL_element(1)%v3_c=cZERO
   !
   ! HARTREE collisions
   !
   if ( COLLISIONS_HXC_local )                      call compute_loc_HXC_collisions()
   !
   ! XC collisions within MBPT
   !
   if ( COLLISIONS_HXC_MB.or.l_use_COH_collisions ) call compute_MBPT_XC_collisions()
   !
   if(l_HXC_collisions) then
     call PP_redux_wait(HXC_COLL_element(1)%v3_c,COMM=LOCAL_COMM(1))
     call PP_redux_wait(HXC_COLL_element(1)%v3_c,COMM=LOCAL_COMM(2))
     call COLLISIONS_linearize_and_IO("HXC",i_coll,last_coll,HXC_collisions,&
&                                       HXC_COLL_element,ID_HXC,LOCAL_yMPI_COMM)
   endif
   !
   if(l_use_COH_collisions) then
     call PP_redux_wait(COH_COLL_element(1)%v3_c,COMM=LOCAL_COMM(1))
     call PP_redux_wait(COH_COLL_element(1)%v3_c,COMM=LOCAL_COMM(2))
     call COLLISIONS_linearize_and_IO("COH",i_coll,last_coll,COH_collisions,&
&                                       COH_COLL_element,ID_COH,LOCAL_yMPI_COMM)
   endif
   !
 enddo ! loop on i_coll
 !
 call live_timing()
 !
 ! CLEAN
 !
 if (l_compute_screening) then
   YAMBO_FREE(EM1s)
   call X_ALLOC_elemental('X')
   call elemental_collision_free(isc_H)
   call elemental_collision_free(iscp_H)
   call elemental_collision_free(isc_XC)
   call elemental_collision_free(iscp_XC)
 endif
 !
 if (COLLISIONS_HXC_use_TDDFT) then
   YAMBO_FREE(tddft_wf%rhotwr1)
   YAMBO_FREE(tddft_wf%rhotwr2)
   YAMBO_FREE(tddft_wf%WF_symm1)
   YAMBO_FREE(tddft_wf%WF_symm2)
   YAMBO_FREE(F_xc_mat)
   YAMBO_FREE(magn)
 endif
 !
 call timing(trim(H_potential)//' Collisions',OPR='stop')
 !
 contains
   !
   subroutine compute_loc_HXC_collisions()
     !
     ! Scattering geometry
     !=====================           _ \ _
     !                               /  /  \
     !                   G_p{np,mp} |      | (i_p,i_p_s)
     !                              \_ _ _/
     !                                 | 
     !                                 | W(q=0)
     !                                 |
     !     (i_n,i_k,1)------>----------.---------->----(i_m,i_k,1)
     !                          
     !                          
     integer :: i_q_fake, i_p_bz
     !                          
     isc_H%qs   = 1
     iscp_H%qs  = 1
     !
     call scatter_Gamp(isc_H,'x')
     !
     i_k_s      = 1
     !
     isc_H%is  =(/i_n,i_k,i_k_s,i_sp/)
     isc_H%os  =(/i_m,i_k,i_k_s,i_sp/)
     !
     call scatter_Bamp(isc_H)
     !
     do i_q_fake=1,q%nbz
       !
       if (.not.PAR_IND_Q_bz%element_1D(i_q_fake)) cycle
       !
       i_p_bz  =qindx_S(i_k,i_q_fake,1)
       i_p     =k%sstar(i_p_bz,1)
       !
       if (k%sstar(i_p_bz,2)/=1) cycle
       !
       do i_mp=COLL_bands(1),COLL_bands(2)
         do i_np=i_mp,COLL_bands(2)
           !
           if ( .not. PAR_IND_Bp_mat%element_1D( B_mat_index(i_np,i_mp,COLL_bands) ) .and. &
&               .not. PAR_IND_Bp_mat%element_1D( B_mat_index(i_mp,i_np,COLL_bands) )         ) cycle
           !
           iscp_H%is =(/i_np,i_p,1,i_sp/)
           iscp_H%os =(/i_mp,i_p,1,i_sp/)
           !
           eval_coll=.not.all((/i_n==i_np,i_m==i_mp,i_k==i_p/))
           if (     eval_coll) call scatter_Bamp(iscp_H)
           if (.not.eval_coll) iscp_H%rhotw=isc_H%rhotw
           !
           rhotw_tmp=iscp_H%rhotw
           !
           do i_star=2,k%nstar(i_p)
             !
             i_p_s =k%star(i_p,i_star)
             !
             !$omp parallel do default(shared), private(ig1)
             do ig1=2,N_g_vecs_H
               rhotw_tmp(ig1)=rhotw_tmp(ig1)+iscp_H%rhotw(g_rot(ig1,sop_inv(i_p_s)))
             enddo
             !$omp end parallel do
             !
           enddo
           !
           if ( PAR_IND_Bp_mat%element_1D( B_mat_index(i_np,i_mp,COLL_bands) ) ) then
             !
             pre_factor1=cZERO
             if(index(H_potential,"HARTREE")>0) then
               !$omp parallel do default(shared), private(ig1), reduction(+:pre_factor1)
               do ig1=2,N_g_vecs_H
                 pre_factor1=pre_factor1+conjg(rhotw_tmp(ig1))*isc_H%gamp(ig1,1)*isc_H%rhotw(ig1)
               enddo
               !$omp end parallel do
             endif
             !
             pre_factor2=cZERO
             if(COLLISIONS_HXC_use_TDDFT) then
               do i_star=1,k%nstar(i_p)
                 i_p_s =k%star(i_p,i_star)
                 iscp_H%is(3) = i_p_s
                 iscp_H%os(3) = i_p_s
                 pre_factor2=pre_factor2+TDDFT_ALDA_R_space(isc_H%is,isc_H%os,iscp_H%is,iscp_H%os,tddft_wf,'RES')
               enddo
             endif
             !
             HXC_COLL_element(1)%v3_c(i_np,i_mp,i_p)=4._SP*pi*pre_factor1+Co*pre_factor2
             !
             call live_timing(steps=1)
             !
           endif
           !
           if(i_np==i_mp) cycle
           !
           if ( PAR_IND_Bp_mat%element_1D( B_mat_index(i_mp,i_np,COLL_bands) ) ) then
             !
             pre_factor1=cZERO
             if(index(H_potential,"HARTREE")>0) then
               !$omp parallel do default(shared), private(ig1), reduction(+:pre_factor1)
               do ig1=2,N_g_vecs_H
                 pre_factor1=pre_factor1+rhotw_tmp(minus_G(ig1))*isc_H%gamp(ig1,1)*isc_H%rhotw(ig1)
               enddo
               !$omp end parallel do
             endif
             !
             pre_factor2=cZERO
             if(COLLISIONS_HXC_use_TDDFT) then
               do i_star=1,k%nstar(i_p)
                 i_p_s =k%star(i_p,i_star)
                 iscp_H%is(3) = i_p_s
                 iscp_H%os(3) = i_p_s
                 pre_factor2=pre_factor2+TDDFT_ALDA_R_space(isc_H%is,isc_H%os,iscp_H%is,iscp_H%os,tddft_wf,'CPL')
               enddo
             endif
             !
             HXC_COLL_element(1)%v3_c(i_mp,i_np,i_p)=4._SP*pi*pre_factor1+Co*pre_factor2
             !
             call live_timing(steps=1)
             !
           endif
           !
         enddo
         !
       enddo
       !
     enddo
     !
   end subroutine compute_loc_HXC_collisions
   !
   subroutine compute_MBPT_XC_collisions()
     !
     ! Scattering geometry
     !=====================
     !
     use IO_m, ONLY:OP_RD,RD,RD_CL
     !
     !                      W(i_kmq_G0,iqibz,iqs)                    
     !                        _ _ _ _ \ _ _ _ _
     !                       /        /        \
     !                      /                   \
     !                     /     G_{np,mp}       \
     !     (i_n,i_k,1)-->--.---------->----------.---->----(i_m,i_k,1)
     !                          (i_kmq,i_kmq_s)
     !
     !
     integer :: TMP_INDEX(COLL_bands(1):COLL_bands(2))
     !
     if (l_compute_screening.and.i_coll==first_coll) then
       call io_control(ACTION=OP_RD,COM=NONE, SEC=(/1/),ID=ID_X)
       io_err=io_X(X,Xw,ID_X)
     endif
     !
     do iqbz=1,q%nbz
       !
       if (.not.PAR_IND_Q_bz%element_1D(iqbz)) cycle
       !
       iqibz    =q%sstar(iqbz,1)
       iqs      =q%sstar(iqbz,2)
       !
       isc_XC%qs   =(/0,iqibz,iqs/)
       iscp_XC%qs  =(/0,iqibz,iqs/)
       !
       i_k_s   = 1
       !
       i_kmq_bz =qindx_S(i_k,iqbz,1)
       i_kmq_G0 =qindx_S(i_k,iqbz,2)
       !
       i_kmq   = k%sstar(i_kmq_bz,1)
       i_kmq_s = k%sstar(i_kmq_bz,2)
       !
       isc_XC%is  =(/i_n,i_k,  i_k_s,  i_sp/)
       isc_XC%os  =(/0  ,i_kmq,i_kmq_s,i_sp/)
       !
       iscp_XC%is =(/i_m,i_k,  i_k_s,  i_sp/)
       iscp_XC%os =(/0,  i_kmq,i_kmq_s,i_sp/)
       !
       isc_XC%qs(1) =i_kmq_G0
       iscp_XC%qs(1)=i_kmq_G0
       !
       if (iqibz/=isc_XC%iqref) call scatter_Gamp(iscp_XC,'x')
       !
       if( iqibz/=isc_XC%iqref .and. l_compute_screening) then
         !
         call scatter_Gamp(isc_XC ,'c')
         !
         ! I/O
         !
         call IO_and_Messaging_switch("-fragments_IO_log")
         !
         call io_control(ACTION=RD,COM=NONE, SEC=(/2*iqibz,2*iqibz+1/),ID=ID_X)
         io_err=io_X(X,Xw,ID_X)
         !
         call IO_and_Messaging_switch("+fragments_IO_log")
         !
         ! Poles and Residuals
         !
         !$omp parallel do default(shared), private(ig1,ig2)
         do ig1=1,N_g_vecs_C
           do ig2=1,N_g_vecs_C
             EM1s(ig1,ig2)=X_mat(ig1,ig2,1)*isc_XC%gamp(ig1,ig2)
           enddo
         enddo
         !$omp end parallel do
         !
       endif
       !
       do i_mp=COLL_bands(1),COLL_bands(2)
         !
         do i_np=COLL_bands(1),COLL_bands(2)
           TMP_index(i_np)=B_mat_index(i_np,i_mp,COLL_bands)
         enddo
         !
         if ( .not. any(PAR_IND_Bp_mat%element_1D(TMP_index(:))) ) cycle
         !
         iscp_XC%os(1)=i_mp
         !
         call scatter_Bamp(iscp_XC)
         !
         do i_np=COLL_bands(1),COLL_bands(2)
           !
           ! COLLISIONS(n,n',qp_n_states) = \sum_{G,G',q} ( rho(m,n,G,q,k) x EM1s(G ,G') x rho*(m',n',G',q,k) /|q+G|/|q+G'| )
           !
           if ( .not. PAR_IND_Bp_mat%element_1D( B_mat_index(i_np,i_mp,COLL_bands) ) ) cycle
           !
           isc_XC%os(1)=i_np
           !
           if (any(isc_XC%is/=iscp_XC%is).or.any(isc_XC%os/=iscp_XC%os)) then
             call scatter_Bamp(isc_XC)
           else
             isc_XC%rhotw=iscp_XC%rhotw
           endif
           !
           pre_factor1=cZERO
           if(index(H_potential,"FOCK")>0.or.index(H_potential,"SEX")>0) then 
             !$omp parallel do default(shared), private(ig1), reduction(+:pre_factor1)
             do ig1=1,N_g_vecs_X
               pre_factor1=pre_factor1+isc_XC%rhotw(ig1)*iscp_XC%gamp(ig1,1)*conjg(iscp_XC%rhotw(ig1))
             enddo
             !$omp end parallel do
           endif
           !
           pre_factor2=cZERO
           if(index(H_potential,"COH")>0.or.index(H_potential,"SEX")>0) then 
             !$omp parallel do default(shared), private(ig1,ig2), reduction(+:pre_factor2)
             do ig1=1,N_g_vecs_C
               do ig2=1,N_g_vecs_C
                 pre_factor2=pre_factor2+isc_XC%rhotw(ig2)*EM1s(ig2,ig1)*conjg(iscp_XC%rhotw(ig1))
               enddo
             enddo
             !$omp end parallel do
           endif
           !
           if(index(H_potential,"SEX")>0 .or. index(H_potential,"FOCK")>0) &
&            HXC_COLL_element(1)%v3_c(i_np,i_mp,i_kmq)=HXC_COLL_element(1)%v3_c(i_np,i_mp,i_kmq) &
&                                                      -4._SP/spin_occ*pi*(pre_factor1+pre_factor2)
           if(index(H_potential,"COH")>0 ) &
&            COH_COLL_element(1)%v3_c(i_np,i_mp,i_kmq)=COH_COLL_element(1)%v3_c(i_np,i_mp,i_kmq) &
&                                                      +2._SP         *pi*             pre_factor2
           !
           call live_timing(steps=1)
           !
         enddo ! loop on the band i_np
         !
       enddo ! loop on the band i_mp
       !
     enddo ! loop on iqbz
     !
     if (l_compute_screening.and.i_coll==last_coll) then
       call io_control(ACTION=RD_CL,COM=NONE, SEC=(/-2/),ID=ID_X)
       io_err=io_X(X,Xw,ID_X)
     endif
     !
   end subroutine compute_MBPT_XC_collisions
   !
end subroutine
