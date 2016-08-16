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
subroutine COLLISIONS_HXC(X,Xk,E,k,q,Xw)
 !
 ! HXC collisions 
 !
 use pars,          ONLY:SP,pi,cZERO
 use memory_m,      ONLY:mem_est
 use com,           ONLY:msg
 use frequency,     ONLY:w_samp
 use electrons,     ONLY:levels,spin_occ,spin,n_spin
 use LIVE_t,        ONLY:live_timing
 use com,           ONLY:error
 use SC,            ONLY:SC_potential
 use D_lattice,     ONLY:DL_vol
 use xc_functionals,ONLY:F_xc,XC_potential_driver
 use global_XC,     ONLY:WF_kind,WF_xc_functional
 use parallel_m,    ONLY:PP_redux_wait,PAR_IND_Q,&
&                        PAR_n_Bp_mat_elements,PAR_COM_QP_A2A,&
&                        PAR_IND_Bp_mat,PAR_nQ,PAR_COM_G_b_A2A,PAR_COM_Q_index
 use collision,     ONLY:elemental_collision,elemental_collision_free,COLLISIONS_parallel_SC_structure,&
&                        COLL_bands,HXC_COLL_element,HXC_collisions,COH_COLL_element,COH_collisions,   &
&                        elemental_collision_alloc
#if defined _TIMING
 use timing_m,      ONLY:timing
#endif
 use QP_m,          ONLY:QP_ng_SH,QP_ng_Sx
 use IO_m,          ONLY:io_control,OP_RD_CL,REP,VERIFY,NONE,Io_and_Messaging_switch,io_COLLs
 use X_m,           ONLY:X_alloc,X_mat,X_t
 use interfaces,    ONLY:WF_load
 use wave_func,     ONLY:WF,WF_buffer,WF_buffered_IO
 use R_lattice,     ONLY:qindx_S,bz_samp,g_rot,minus_G
 use hamiltonian,   ONLY:B_mat_index
 use fft_m,         ONLY:fft_size
! use wrapper_omp,   ONLY:Vstar_dot_V_omp
 !
 implicit none
 !
 type(levels)             ::E
 type(bz_samp)            ::k,q,Xk
 type(X_t)                ::X
 type(w_samp)             ::Xw
 !
 ! Work Space
 !
 integer                 :: i_coll,ig1,ig2,iqbz,iqibz,alloc_err,iqs,iGo_max,NG_max,&
&                           i_n,i_k,i_k_s,i_sp,i_m,i_mp,N_g_vecs_H,N_g_vecs_XC,i_star,&
&                           i_np,i_kmq,i_kmq_bz,i_kmq_s,i_kmq_G0,PAR_NC_local,n_steps,bands_to_load(2)
 logical                 :: l_HXC_collisions,l_COH_collisions,l_MBPT_XC_collisions,l_loc_HXC_collisions,&
&                           eval_coll,l_compute_screening,l_tddft
 !
 type(elemental_collision):: isc_XC,iscp_XC,isc_H,iscp_H
 real(SP)                 :: Co
 complex(SP)              :: pre_factor1,pre_factor2
 !
 complex(SP), allocatable :: EM1s(:,:),rhotw_tmp(:)
 integer                  :: io_err,ID
 integer, external        :: io_X
 complex(SP),   external  :: TDDFT_ALDA_R_space
 !
#if defined _TIMING
 call timing(trim(SC_potential)//' Collisions',OPR='start')
#endif
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
 l_loc_HXC_collisions = index(SC_potential,"HARTREE")>0 .or. index(SC_potential,"GS_xc")>0 .or. index(SC_potential,"default")>0
 l_MBPT_XC_collisions = index(SC_potential,"FOCK")>0 .or. index(SC_potential,"SEX")>0
 !
 l_HXC_collisions     = l_loc_HXC_collisions .or. l_MBPT_XC_collisions 
 !
 l_COH_collisions     = index(SC_potential,"COH")>0
 l_compute_screening  = index(SC_potential,"COH")>0 .or. index(SC_potential,"SEX")>0
 l_tddft              = index(SC_potential,"GS_xc")>0 .or. index(SC_potential,"default")>0 
 !
 call k_build_up_BZ_tables(k)
 call k_build_up_BZ_tables(q)
 call k_expand(k)
 !
 ! Dimensions
 !
 N_g_vecs_H =0
 N_g_vecs_XC=0
 iGo_max    =maxval(qindx_S(:,:,2))
 !
 if(l_loc_HXC_collisions                     ) N_g_vecs_H  = QP_ng_SH
 if(l_MBPT_XC_collisions .or.l_COH_collisions) N_g_vecs_XC = QP_ng_Sx
 !
 call fft_check_size(iGo_max,N_g_vecs_XC,N_g_vecs_H)
 !
 if(l_loc_HXC_collisions) QP_ng_SH=N_g_vecs_H
 !
 NG_max=max(N_g_vecs_H,N_g_vecs_XC)
 !
 call msg('r', '['//trim(SC_potential)//'] Plane waves :',(/N_g_vecs_H,N_g_vecs_XC/))
 !
 ! WFs
 !
 bands_to_load=COLL_bands
 if (l_tddft) then
   bands_to_load(1)=1
   bands_to_load(2)=max(COLL_bands(2),E%nbm)
 endif
 !
 if (WF_buffered_IO) call WF_load(WF_buffer,0,1,bands_to_load,(/1,k%nibz/),space='B',title='-BUFFER')
 !
 call WF_load(WF,NG_max,iGo_max,bands_to_load,(/1,k%nibz/),title='-'//trim(SC_potential)//' Collisions')
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
 allocate(rhotw_tmp(N_g_vecs_H))
 !
 iscp_XC%qs   =1
 iscp_XC%iqref=0
 iscp_XC%ngrho=N_g_vecs_XC
 !
 isc_XC%qs    =1
 isc_XC%iqref =0
 isc_XC%ngrho =N_g_vecs_XC
 !
 call elemental_collision_alloc(iscp_XC,NG=N_g_vecs_XC,NG_GAMP=(/N_g_vecs_XC,  1  /),TITLE=trim(SC_potential))
 !
 if(l_compute_screening) then
   !
   ! Plasmon-Pole/Static interaction DB I/O
   !
   call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1,2/),MODE=VERIFY,ID=ID)
   io_err=io_X(X,Xw,ID)
   if (io_err<0) call error(' Incomplete and/or broken PPA/Static diel. fun. database')
   !
   if (X%ng<N_g_vecs_XC) call error(' Screening size (NGsBlkXs) cannot be smaller than SEX size (EXXRLvcs)')
   !
   ! Test the spatial Inversion
   !   
   call WF_spatial_inversion(E,Xk)
   !
   ! Allocation (EM1S can be bigger than N_g_vecs_XC)
   !
   call X_alloc('X',(/X%ng,X%ng,1/))
   allocate(EM1s(N_g_vecs_XC,N_g_vecs_XC),stat=alloc_err)
   call mem_est("EM1s",(/size(EM1s)/),errors=(/alloc_err/))
   !
   call elemental_collision_alloc(isc_XC, NG=N_g_vecs_XC,NG_GAMP=(/N_g_vecs_XC,N_g_vecs_XC/),TITLE=trim(SC_potential))
   !
 else
   !
   call elemental_collision_alloc(isc_XC, NG=N_g_vecs_XC,                                    TITLE=trim(SC_potential))
   !
 endif
 !
 if(l_tddft) then
    Co=4._SP*pi/DL_vol/real(q%nbz,SP)
   allocate(F_xc(fft_size,n_spin,n_spin))
   call XC_potential_driver(E,K,WF_kind,WF_xc_functional,2)
 endif
 !
 ! Parallel Indexes & LIVE-TIMING STEPS
 !
 PAR_NC_local=HXC_collisions%PAR_N
 if (associated(HXC_collisions%IO_status)) PAR_NC_local=count(HXC_collisions%IO_status==-1)
 !
 n_steps=0
 if(index(SC_potential,"HARTREE")>0) n_steps=n_steps+PAR_NC_local*K%nibz*PAR_n_Bp_mat_elements
 if(l_MBPT_XC_collisions           ) n_steps=n_steps+PAR_NC_local*PAR_nQ*PAR_n_Bp_mat_elements
 !
 call live_timing("Collisions",n_steps)
 !
 do i_coll=1,HXC_collisions%N
   !
   if (.not.HXC_collisions%PAR_IND%element_1D(i_coll)) cycle
   !
   if (io_COLLs.and.l_HXC_collisions) then
     if (HXC_collisions%IO_status(i_coll)/=-1) cycle
   endif
   if (io_COLLs.and.l_COH_collisions) then
     if (COH_collisions%IO_status(i_coll)/=-1) cycle
   endif
   !
   if( l_HXC_collisions ) then
     i_n     = HXC_collisions%state(i_coll,1)
     i_m     = HXC_collisions%state(i_coll,2)
     i_k     = HXC_collisions%state(i_coll,3)
     i_sp    = HXC_collisions%state(i_coll,4)
   elseif( index(SC_potential,"COH")>0 ) then
     i_n     = COH_collisions%state(i_coll,1)
     i_m     = COH_collisions%state(i_coll,2)
     i_k     = COH_collisions%state(i_coll,3)
     i_sp    = COH_collisions%state(i_coll,4)
   endif   
   !
   if( l_HXC_collisions ) HXC_COLL_element(1)%v3_c=cZERO
   if( l_COH_collisions ) COH_COLL_element(1)%v3_c=cZERO
   !
   ! HARTREE collisions
   !
   if ( l_loc_HXC_collisions ) call compute_loc_HXC_collisions()
   !
   ! XC collisions within MBPT
   !
   if ( l_MBPT_XC_collisions ) call compute_MBPT_XC_collisions()
   !
   !
   if (COLLISIONS_parallel_SC_structure) then
     if(l_HXC_collisions) then
       call PP_redux_wait(HXC_COLL_element(1)%v3_c,COMM=PAR_COM_QP_A2A%COMM)
       call PP_redux_wait(HXC_COLL_element(1)%v3_c,COMM=PAR_COM_Q_index%COMM)
     endif
     if(l_COH_collisions) then
       call PP_redux_wait(COH_COLL_element(1)%v3_c,COMM=PAR_COM_QP_A2A%COMM)
       call PP_redux_wait(COH_COLL_element(1)%v3_c,COMM=PAR_COM_Q_index%COMM)
     endif
   else
     if(l_HXC_collisions) call PP_redux_wait(HXC_COLL_element(1)%v3_c,COMM=PAR_COM_G_b_A2A%COMM)
     if(l_COH_collisions) call PP_redux_wait(COH_COLL_element(1)%v3_c,COMM=PAR_COM_G_b_A2A%COMM)
   endif
   !
   if (l_HXC_collisions) call COLLISIONS_linearize_and_IO("HXC",i_coll,HXC_collisions,HXC_COLL_element)
   if (l_COH_collisions) call COLLISIONS_linearize_and_IO("COH",i_coll,COH_collisions,COH_COLL_element)
   !
 enddo ! loop on i_coll
 !
 call live_timing()
 !
 ! CLEAN
 !
 if(l_compute_screening) then
   deallocate(EM1s)
   call mem_est("EM1s")
   call X_alloc('X')
   call elemental_collision_free(isc_H)
   call elemental_collision_free(iscp_H)
   call elemental_collision_free(isc_XC)
   call elemental_collision_free(iscp_XC)
 endif
 !
#if defined _TIMING
 call timing(trim(SC_potential)//' Collisions',OPR='stop')
#endif
 !
 contains
   !
   subroutine compute_loc_HXC_collisions()
     !
     ! Scattering geometry
     !=====================          _ \ _
     !                              /  /  \
     !                   G_{np,mp} |       | (i_kmq,i_kmq_s)
     !                              \_ _ _/
     !                                 | 
     !                                 |  W(i_kmq_G0,1,1)
     !                                 |
     !     (i_n,i_k,1)------>----------.---------->----(i_m,i_k,1)
     !                          
     !
     ! Only q=0 term
     !
     if (.not.PAR_IND_Q%element_1D(1)) return
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
     do i_kmq=1,k%nibz
       !
       !i_kmq_s =k%star(i_kmq,1)
       !i_kmq_bz=k%k_table(i_kmq,1)
       !
       do i_mp=COLL_bands(1),COLL_bands(2)
         do i_np=i_mp,COLL_bands(2)
           !
           if ( .not. PAR_IND_Bp_mat%element_1D( B_mat_index(i_np,i_mp,COLL_bands) ) .and. &
&               .not. PAR_IND_Bp_mat%element_1D( B_mat_index(i_mp,i_np,COLL_bands) )         ) cycle
           !
           iscp_H%is =(/i_np,i_kmq,1,i_sp/)
           iscp_H%os =(/i_mp,i_kmq,1,i_sp/)
           !
           eval_coll=.not.all((/i_n==i_np,i_m==i_mp,i_k==i_kmq/))
           if (     eval_coll) call scatter_Bamp(iscp_H)
           if (.not.eval_coll) iscp_H%rhotw=isc_H%rhotw
           !
           rhotw_tmp=iscp_H%rhotw
           !
           do i_star=2,k%nstar(i_kmq)
             !
             i_kmq_s =k%star(i_kmq,i_star)
             !i_kmq_bz=k%k_table(i_kmq,i_kmq_s)
             !
             !$omp parallel do default(shared), private(ig1)
             do ig1=2,N_g_vecs_H
               rhotw_tmp(ig1)=rhotw_tmp(ig1)+iscp_H%rhotw(g_rot(i_kmq_s,ig1))
             enddo
             !$omp end parallel do
             !
           enddo
           !
           if ( PAR_IND_Bp_mat%element_1D( B_mat_index(i_np,i_mp,COLL_bands) ) ) then
             !
             pre_factor1=cZERO
             if(index(SC_potential,"HARTREE")>0) then
               !$omp parallel do default(shared), private(ig1), reduction(+:pre_factor1)
               do ig1=2,N_g_vecs_H
                 pre_factor1=pre_factor1+conjg(rhotw_tmp(ig1))*isc_H%gamp(ig1,1)*isc_H%rhotw(ig1)
               enddo
               !$omp end parallel do
             endif
             !
             pre_factor2=cZERO
             if(l_tddft) then
               do i_star=1,k%nstar(i_kmq)
                 i_kmq_s =k%star(i_kmq,i_star)
                 iscp_H%is(3) = i_kmq_s
                 iscp_H%os(3) = i_kmq_s
                 pre_factor2=pre_factor2+TDDFT_ALDA_R_space(isc_H%is,isc_H%os,iscp_H%is,iscp_H%os,'RES')
               enddo
             endif
             !
             HXC_COLL_element(1)%v3_c(i_np,i_mp,i_kmq)=4._SP*pi*pre_factor1+Co*pre_factor2
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
             if(index(SC_potential,"HARTREE")>0) then
               !$omp parallel do default(shared), private(ig1), reduction(+:pre_factor1)
               do ig1=2,N_g_vecs_H
                 pre_factor1=pre_factor1+rhotw_tmp(minus_G(ig1))*isc_H%gamp(ig1,1)*isc_H%rhotw(ig1)
               enddo
               !$omp end parallel do
             endif
             !
             pre_factor2=cZERO
             if(l_tddft) then
               do i_star=1,k%nstar(i_kmq)
                 i_kmq_s =k%star(i_kmq,i_star)
                 iscp_H%is(3) = i_kmq_s
                 iscp_H%os(3) = i_kmq_s
                 pre_factor2=pre_factor2+TDDFT_ALDA_R_space(isc_H%is,isc_H%os,iscp_H%is,iscp_H%os,'CPL')
               enddo
             endif
             !
             HXC_COLL_element(1)%v3_c(i_mp,i_np,i_kmq)=4._SP*pi*pre_factor1+Co*pre_factor2
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
     !                      W(i_kmq_G0,iqibz,iqs)                    
     !                        _ _ _ _ \ _ _ _ _
     !                       /        /        \
     !                      /                   \
     !                     /     G_{np,mp}       \
     !     (i_n,i_k,1)-->--.---------->----------.---->----(i_m,i_k,1)
     !                          (i_kmq,i_kmq_s)
     do iqbz=1,q%nbz
       !
       if (.not.PAR_IND_Q%element_1D(iqbz)) cycle
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
         call Io_and_Messaging_switch("-fragments_IO_log")
         !
         ! I/O
         !
         if (q%nbz==1) then
           call io_control(ACTION=OP_RD_CL,COM=NONE, SEC=(/1,2,3/),ID=ID)
         else
           if (iqbz ==1) call io_control(ACTION=OP_RD_CL,COM=NONE, SEC=(/1,2,2*iqibz+1/),ID=ID)
           if (iqbz>  1) call io_control(ACTION=OP_RD_CL,COM=NONE, SEC=(/2*iqibz,2*iqibz+1/),ID=ID)
         endif
         io_err=io_X(X,Xw,ID)
         !
         call Io_and_Messaging_switch("+fragments_IO_log")
         !
         ! Poles and Residuals
         !
         !$omp parallel do default(shared), private(ig1,ig2)
         do ig1=1,N_g_vecs_XC
           do ig2=1,N_g_vecs_XC
             EM1s(ig1,ig2)=X_mat(ig1,ig2,1)*isc_XC%gamp(ig1,ig2)
           enddo
         enddo
         !$omp end parallel do
         !
       endif
       !
       do i_mp=COLL_bands(1),COLL_bands(2)
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
           if(index(SC_potential,"FOCK")>0.or.index(SC_potential,"SEX")>0) then 
             !$omp parallel do default(shared), private(ig1), reduction(+:pre_factor1)
             do ig1=1,N_g_vecs_XC
               pre_factor1=pre_factor1+isc_XC%rhotw(ig1)*iscp_XC%gamp(ig1,1)*conjg(iscp_XC%rhotw(ig1))
             enddo
             !$omp end parallel do
           endif
           !
           pre_factor2=cZERO
           if(index(SC_potential,"COH")>0.or.index(SC_potential,"SEX")>0) then 
             !$omp parallel do default(shared), private(ig1,ig2), reduction(+:pre_factor2)
             do ig1=1,N_g_vecs_XC
               do ig2=1,N_g_vecs_XC
                 pre_factor2=pre_factor2+isc_XC%rhotw(ig2)*EM1s(ig2,ig1)*conjg(iscp_XC%rhotw(ig1))
               enddo
             enddo
             !$omp end parallel do
           endif
           !
           if(index(SC_potential,"SEX")>0 .or. index(SC_potential,"FOCK")>0) &
&            HXC_COLL_element(1)%v3_c(i_np,i_mp,i_kmq)=HXC_COLL_element(1)%v3_c(i_np,i_mp,i_kmq) &
&                                                      -4._SP/spin_occ*pi*(pre_factor1+pre_factor2)
           if(index(SC_potential,"COH")>0 ) &
&            COH_COLL_element(1)%v3_c(i_np,i_mp,i_kmq)=COH_COLL_element(1)%v3_c(i_np,i_mp,i_kmq) &
&                                                      -4._SP/spin_occ*pi*             pre_factor2
           !
           call live_timing(steps=1)
           !
         enddo ! loop on the band i_np
         !
       enddo ! loop on the band i_mp
       !
     enddo ! loop on iqbz
     !
   end subroutine compute_MBPT_XC_collisions
   !
end subroutine