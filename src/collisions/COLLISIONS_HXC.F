!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
subroutine COLLISIONS_HXC(X,Xk,E,k,q,Xw,IO_BS_err,ID_BS)
 !
 ! HXC collisions 
 !
 use drivers,       ONLY:l_use_COH_collisions
 use pars,          ONLY:SP,pi,cZERO
 use parser_m,      ONLY:parser
 use com,           ONLY:msg
 use frequency,     ONLY:w_samp
 use electrons,     ONLY:levels,spin_occ,spin,n_spin,n_spinor,n_full_bands,n_met_bands,n_sp_pol
 use LIVE_t,        ONLY:live_timing
 use hamiltonian,   ONLY:H_potential
 use BS,            ONLY:BS_blk,n_BS_blks
 use D_lattice,     ONLY:DL_vol,sop_inv
 use tddft,         ONLY:tddft_wf_t
 use xc_functionals,ONLY:F_xc,V_xc,F_xc_mat,magn,XC_potential_driver
 use global_XC,     ONLY:WF_kind,WF_xc_functional
 use stderr,        ONLY:intc
 use parallel_int,  ONLY:PP_redux_wait
 use parallel_m,    ONLY:PAR_IND_Q_bz,MPI_COMM_NULL,ncpu,myid,COMM_copy,yMPI_COMM,&
&                        PAR_n_Bp_mat_elements,PAR_COM_QP_A2A,PAR_COM_QP_INDEX,PAR_COM_Q_INDEX,&
&                        PAR_IND_Bp_mat,PAR_nQ_bz,PAR_COM_G_b_A2A,PAR_COM_G_b_index_global
 use collision_el,  ONLY:elemental_collision,elemental_collision_free,elemental_collision_alloc
 use collision_ext, ONLY:COLLISIONS_parallel_SC_structure,COLLISIONS_HXC_local,COLLISIONS_HXC_MB,&
&                        COLL_bands,HXC_COLL_element,HXC_collisions,COH_COLL_element,COH_collisions,&
&                        COLLISIONS_HXC_use_TDDFT,PAR_COLL_min,COLLISIONS_CV_only
 use timing_m,      ONLY:timing
 use QP_m,          ONLY:QP_ng_SH,QP_ng_Sx,QP_ng_Sc
 use IO_int,        ONLY:io_control,IO_and_Messaging_switch
 use IO_m,          ONLY:OP_RD_CL,REP,VERIFY,NONE,io_COLLs
 use ALLOC,         ONLY:X_ALLOC_elemental
 use X_m,           ONLY:X_mat,X_t
 use interfaces,    ONLY:WF_load,TDDFT_ALDA_eh_space_R_kernel!,eval_G_minus_G
 use wave_func,     ONLY:WF,WF_buffer,WF_buffered_IO
 use R_lattice,     ONLY:qindx_S,bz_samp,g_rot,minus_G,G_m_G
 use hamiltonian,   ONLY:B_mat_index
 use fft_m,         ONLY:fft_size
 !
#include<y_memory.h>
 !
 type(levels)             ::E
 type(bz_samp)            ::k,q,Xk
 type(X_t)                ::X
 type(w_samp)             ::Xw
 integer                  ::ID_BS
 integer, intent(in)      ::IO_BS_err
 !
 ! Work Space
 !
 integer                 :: i_coll,first_coll,last_coll,ig1,ig2,iqbz,iqibz,alloc_err,iqs,iGo_max,NG_max,NG_XC,LOCAL_COMM(2),&
&                           i_n,i_k,i_k_s,i_sp,ip_sp,i_m,i_mp,N_g_vecs_H,N_g_vecs_X,N_g_vecs_C,i_star,i_p,i_p_s,i_loop,iB,&
&                           i_np,i_kmq,i_kmq_bz,i_kmq_s,i_kmq_G0,PAR_NC_local,n_steps,i_steps,bands_to_load(2),n_colls,n_GmG,&
&                           n_skip_colls,n_incl_colls
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
 logical                  :: doing_cv_colls,cc_vv_prime_only
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
 n_skip_colls      = 0
 n_incl_colls      = 0
 l_compute_screening  = index(H_potential,"COH")>0   .or. index(H_potential,"SEX")>0
 l_HXC_collisions     = COLLISIONS_HXC_local .or. COLLISIONS_HXC_MB 
 !
 call k_build_up_BZ_tables(k)
 call k_build_up_BZ_tables(q)
 call k_expand(k)
 !
 if (l_compute_screening.and.(IO_BS_err/=0.or.(.not.COLLISIONS_CV_only))) then
   !
   ! Plasmon-Pole/Static interaction DB I/O
   !
   call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1,2/),MODE=VERIFY,ID=ID_X)
   io_err=io_X(X,Xw,ID_X)
   if (io_err<0) call error(' Incomplete and/or broken PPA/Static diel. fun. database')
   !
   if (X%ng<QP_ng_Sc) then
     if (IO_BS_err/=0) then
       QP_ng_Sc=X%ng
       call warning('SEX size (CORRLvcs) reduced to match the Screening size (NGsBlkXs)')
     else
       call error('SEX size (CORRLvcs) bigger than Screening size (NGsBlkXs)')
     endif
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
 if (COLLISIONS_HXC_local) N_g_vecs_H  = QP_ng_SH
 if (COLLISIONS_HXC_MB   ) N_g_vecs_X  = QP_ng_Sx
 if (l_compute_screening ) N_g_vecs_C  = QP_ng_Sc
 !
 if (IO_BS_err/=0.or.(.not.COLLISIONS_CV_only)) then
   call fft_check_size(iGo_max,N_g_vecs_C,N_g_vecs_H,"SH")
   call fft_check_size(iGo_max,N_g_vecs_C,N_g_vecs_X,"SF")
   if (IO_BS_err==0) then
     if(QP_ng_SH/=N_g_vecs_H) call warning("Hartree cutoff is different between cv and cc/vv channels")
     if(QP_ng_Sx/=N_g_vecs_X) call warning("Exchange cutoff is different between cv and cc/vv channels")
   endif
 endif
 !
 if(COLLISIONS_HXC_local) QP_ng_SH=N_g_vecs_H
 if(COLLISIONS_HXC_MB)    QP_ng_Sx=N_g_vecs_X
 !
 NG_max=maxval((/N_g_vecs_H,N_g_vecs_X,N_g_vecs_C/))
 NG_XC =max(N_g_vecs_X,N_g_vecs_C)
 !
 ! DEBUG <
 ! 2022/06/30 DS: This is a check for consistency with BSE
 ! - In the BSE case the elemental collisions are always computed with iqs=1 and iqG=1
 !   and later there are symmetry operations with G_m_G which require to increase the O_ng (i.e. NG_XC)
 ! - In the RT/SC collisions case the symmetries and the Go shifts are directly handled by scatter Gamp
 !   via fft_g_table. So it is not needed to increase NG_XC
 !if (COLLISIONS_HXC_MB) then
 !  n_GmG=eval_G_minus_G(NG_XC,iGo_max)
 !  NG_XC=maxval(G_m_G)
 !endif
 ! DEBUG >
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
 if (IO_BS_err/=0.or.(.not.COLLISIONS_CV_only)) then
   !
   if (WF_buffered_IO) call WF_load(WF_buffer,0,1,bands_to_load,(/1,k%nibz/),space='B',title='-BUFFER')
   call WF_load(WF,NG_max,iGo_max,bands_to_load,(/1,k%nibz/),title='-'//trim(H_potential)//' Collisions')
   !
   if (COLLISIONS_HXC_local) then
     !
     call elemental_collision_alloc(isc_H ,NG=N_g_vecs_H,GAMP_NG=(/N_g_vecs_H,    1   /),TITLE='HARTREE')
     call elemental_collision_alloc(iscp_H,NG=N_g_vecs_H,                                TITLE='HARTREE')
     !
     YAMBO_ALLOC(rhotw_tmp,(N_g_vecs_H))
     !
   endif
   !
   call elemental_collision_alloc(iscp_XC,NG=NG_XC,GAMP_NG=(/N_g_vecs_X,  1  /),TITLE=trim(H_potential))
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
     call elemental_collision_alloc(isc_XC, NG=NG_XC,GAMP_NG=(/N_g_vecs_C,N_g_vecs_C/),TITLE=trim(H_potential))
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
 last_coll =0
 !
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
   enddo
 endif
 TMP_VEC=0
 TMP_VEC(myid+1)=PAR_NC_local
 call PP_redux_wait(TMP_VEC)
 PAR_COLL_min=minval(TMP_VEC)
 !
 n_steps=0
 if (COLLISIONS_HXC_local)                      n_steps=n_steps+PAR_NC_local*K%nibz*PAR_n_Bp_mat_elements*n_sp_pol
 if (COLLISIONS_HXC_MB.or.l_use_COH_collisions) n_steps=n_steps+PAR_NC_local*PAR_nQ_bz*PAR_n_Bp_mat_elements*n_sp_pol
 !
 if (IO_BS_err==0) then
   i_steps=2
   if (COLLISIONS_CV_only) then
     i_steps=1
     n_steps=PAR_NC_local*K%nibz*PAR_n_Bp_mat_elements*n_sp_pol
   endif
 endif
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
   i_k_s      = 1
   !
   if( l_HXC_collisions )     HXC_COLL_element(1)%v3_c=cZERO
   if( l_use_COH_collisions ) COH_COLL_element(1)%v3_c=cZERO
   !
   if (IO_BS_err==0) then
     !
     doing_cv_colls=.not.((i_n> n_met_bands(i_sp)  .and. i_m>n_met_bands(i_sp) ) .or. &
     &                    (i_n<=n_full_bands(i_sp) .and. i_m<=n_full_bands(i_sp))     )
     !
     ! HXC collisions from BSE kernel if in the cv channel
     if (doing_cv_colls) call map_BSE_to_HXC_collisions()
     !
     if (.not.COLLISIONS_CV_only) then
       !
       ! If (in,im) is in the cv channel, I need (inp,imp) only in the cc/vv channel
       ! If (in,im) is not in the cv channel, I need (inp,imp) everywhere
       cc_vv_prime_only=doing_cv_colls
       ! Hartree collisions
       if ( COLLISIONS_HXC_local )                      call compute_Hartree_collisions(cc_vv_prime_only)
       ! XC collisions within MBPT
       if ( COLLISIONS_HXC_MB.or.l_use_COH_collisions ) call compute_MBPT_XC_collisions(cc_vv_prime_only)
     endif
     !
   else
     !
     ! Hartree collisions
     if ( COLLISIONS_HXC_local )                      call compute_Hartree_collisions(.false.)
     ! XC collisions within MBPT
     if ( COLLISIONS_HXC_MB.or.l_use_COH_collisions ) call compute_MBPT_XC_collisions(.false.)
     !
   endif
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
 if(n_skip_colls>0) call warning(" BSE conversion, ignored "//&
 &     trim(intc(n_skip_colls))//"/"//trim(intc(n_incl_colls))//" collisions")
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
   subroutine map_BSE_to_HXC_collisions()
     !
     ! Yambo uses the BSE matrix  with the square root of the occupations
     ! The real time collisions must be constructed with the standard form
     ! This requires a cI factor in the coupling terms.
     ! Moreover, since the occupation factors are contained in the density matrix,
     ! there is a -1 factor for "Q" and "A"
     !
     ! The excitonic matrix contains the occupations and it is pseudo-hermitian.
     !  R      iC       R  iC 
     ! -(iC)*  -R*  =  iC* -R*
     !
     ! The collisions do not contain the occupations and the matrix is hermitian.
     !   R   C
     !   C*  R*
     !
     use electrons,     ONLY:spin_occ
     use pars,          ONLY:cI,cZERO
     use BS,            ONLY:BS_blk,BS_K_dim,BS_K_io_map,l_BSE_kernel_full
     use BS_solvers,    ONLY:BSS_eh_table_m1
     use IO_int,        ONLY:io_control
     use IO_m,          ONLY:RD
     !
     complex(SP)  :: M_ij
     integer      :: i_BSE,j_BSE,i_k_bz_bse,i_p_bz_bse,iB,jB,i_q_fake,i_p_bz
     logical      :: l_use_mat,l_conj,l_cI,l_m1
     !
     iB=-1
     jB=-1
     !
     i_k_bz_bse=BS_K_io_map(k%k_table(i_k,1))
     !
     i_BSE=BSS_eh_table_m1(i_k_bz_bse,i_m,i_n,i_sp,i_sp)
     iB=mod(i_BSE-1,BS_K_dim(1))+1
     !
     if (i_BSE>0) then
       !
       do jB=iB,iB+BS_K_dim(1),BS_K_dim(1)
         allocate(BS_blk(jB)%mat (BS_blk(jB)%size(1),BS_blk(jB)%size(2)))
         allocate(BS_blk(jB)%tam (BS_blk(jB)%zise(1),BS_blk(jB)%zise(2)))
         BS_blk(jB)%mat=cZERO
         BS_blk(jB)%tam=cZERO
         call io_control(ACTION=RD,ID=ID_BS)
         call read_BS_par_collisions(1,jB,ID_BS)
       enddo
       !
     else
       !DEBUG<
       !write(*,*) "Block ignored ",i_k_bz_bse,i_m,i_n,i_sp,i_sp
       !DEBUG>
     endif
     !
     do ip_sp=1,n_sp_pol
     do i_q_fake=1,q%nbz
       !
       if (.not.PAR_IND_Q_bz%element_1D(i_q_fake)) cycle
       !
       i_p_bz=qindx_S(i_k,i_q_fake,1)
       i_p   =k%sstar(i_p_bz,1)
       !i_p_s =k%sstar(i_p_bz,2)
       !
       do i_mp=COLL_bands(1),COLL_bands(2)
         do i_np=COLL_bands(1),COLL_bands(2)
           !
           if ( (i_np> n_met_bands(ip_sp)  .and. i_mp> n_met_bands(ip_sp) ) ) cycle
           if ( (i_np<=n_full_bands(ip_sp) .and. i_mp<=n_full_bands(ip_sp)) ) cycle
           !
           if ( .not. PAR_IND_Bp_mat%element_1D(B_mat_index(i_np,i_mp,COLL_bands) ) ) cycle
           !
           !i_p_bz_bse=BS_K_io_map(k%k_table(i_p,i_p_s))
           i_p_bz_bse=BS_K_io_map(i_p_bz)
           !
           i_BSE=BSS_eh_table_m1(i_k_bz_bse,i_m,i_n,i_sp,i_sp)
           j_BSE=BSS_eh_table_m1(i_p_bz_bse,i_mp,i_np,ip_sp,ip_sp)
           !
           if (i_BSE==0.or.j_BSE==0) then
             n_skip_colls=n_skip_colls+1
             !DEBUG<
             !if(j_BSE==0) write(*,*) "Transition ignored ",i_p_bz_bse,i_mp,i_np,i_sp
             !DEBUG>
             cycle
           else
             n_incl_colls=n_incl_colls+1
           endif
           !
           if (i_BSE<=BS_K_dim(1) .and. j_BSE<=BS_K_dim(1)) then
             !
             l_cI=.false.
             l_m1=.false.
             if (l_BSE_kernel_full) then
               ! Resonant block
               iB=i_BSE ; jB=j_BSE; l_conj=.false.
               l_use_mat=.true. 
             else
               iB=i_BSE
               ! Resonant block
               if(j_BSE>=i_BSE) then; l_use_mat=.true. ; jB=j_BSE-i_BSE+1; l_conj=.false.; endif
               ! Resonant is hermitian
               if(j_BSE< i_BSE) then; l_use_mat=.false.; jB=j_BSE  ; l_conj=.true. ; endif
             endif
             !
           else if(i_BSE<=BS_K_dim(1) .and. j_BSE>BS_K_dim(1)) then
             !
             l_cI=.true.
             l_m1=.false.
             l_conj=.false.
             j_BSE=j_BSE-BS_K_dim(1)
             if (l_BSE_kernel_full) then
               ! Coupling block
               iB=i_BSE+BS_K_dim(1) ; jB=j_BSE
               l_use_mat=.true.
             else
               iB=i_BSE+BS_K_dim(1)
               ! Coupling block
               if(j_BSE>=i_BSE) then ; l_use_mat=.true. ; jB=j_BSE-i_BSE+1; endif
               ! Coupling is symmetric
               if(j_BSE< i_BSE) then ; l_use_mat=.false.; jB=j_BSE ; endif
             endif
             !
           else if(i_BSE> BS_K_dim(1) .and. j_BSE<=BS_K_dim(1)) then
             !
             l_cI=.true.
             l_m1=.true.
             l_conj=.true.
             i_BSE=i_BSE-BS_K_dim(1)
             if (l_BSE_kernel_full) then
               ! Anti-Coupling
               iB=j_BSE+BS_K_dim(1); jB=i_BSE
               l_use_mat=.true.
             else
               iB=i_BSE+BS_K_dim(1)
               ! Anti-Coupling from coupling: the whole BSE matrix is Pseudo-Hermitian
               if(j_BSE>=i_BSE) then; l_use_mat=.true. ; jB=j_BSE-i_BSE+1; endif
               ! Anti-Coupling is symmetric
               if(j_BSE< i_BSE) then; l_use_mat=.false.; jB=j_BSE  ; endif
             endif
             !
           else if(i_BSE> BS_K_dim(1) .and. j_BSE> BS_K_dim(1)) then
             !
             l_cI=.false.
             l_m1=.true.
             i_BSE=i_BSE-BS_K_dim(1)
             j_BSE=j_BSE-BS_K_dim(1)
             if (l_BSE_kernel_full) then
               ! Anti-resonant from Resonant
               iB=j_BSE; jB=i_BSE-j_BSE+1; l_conj=.true.
               l_use_mat=.true.
             else
               iB=i_BSE
               ! Anti-resonant from Resonant: the whole BSE matrix is Pseudo-Hermitian
               if(j_BSE>=i_BSE) then; l_use_mat=.true. ; jB=j_BSE-i_BSE+1; l_conj=.true.; endif
               ! Anti-resonant is hermitian
               if(j_BSE< i_BSE) then; l_use_mat=.false.; jB=j_BSE  ; l_conj=.false.; endif
             endif
             !
           endif
           !
           if (     l_use_mat) M_ij= BS_blk(iB)%mat(1,jB)
           if (.not.l_use_mat) M_ij= BS_blk(iB)%tam(jB,1)
           !
           if(l_conj) M_ij= conjg(M_ij)
           if(l_cI)   M_ij=  cI * M_ij
           if(l_m1)   M_ij=     - M_ij
           !
           ! - The occupation factor here is already contained in the density matrix
           ! - Compared to the BSE case, the collisions are summed over the points in the star of i_p
           !
           HXC_COLL_element(1)%v3_c(i_np,i_mp,i_p,ip_sp)=HXC_COLL_element(1)%v3_c(i_np,i_mp,i_p,ip_sp)-M_ij/spin_occ
           !
           call live_timing(steps=i_steps)
           !
         enddo
       enddo
     enddo
     enddo
     !
     i_BSE=BSS_eh_table_m1(i_k_bz_bse,i_m,i_n,i_sp,i_sp)
     iB=mod(i_BSE-1,BS_K_dim(1))+1
     !
     if (i_BSE>0) then
       do jB=iB,iB+BS_K_dim(1),BS_K_dim(1)
         deallocate(BS_blk(jB)%mat)
         deallocate(BS_blk(jB)%tam)
       enddo
     endif
     !
   end subroutine map_BSE_to_HXC_collisions
   !
   subroutine compute_Hartree_collisions(cc_vv_only)
     !
     ! Scattering geometry
     !=====================           _ \ _
     !                               /  /  \
     !             G_p{np,mp,ip_sp} |      | (i_p,i_p_s)
     !                              \_ _ _/
     !                                 | 
     !                                 | W(q=0)
     !                                 |
     !  (i_n,i_k,i_sp)------>----------.---------->----(i_m,i_k,i_sp)
     !                          
     !
     logical, intent(in)  :: cc_vv_only
     !
     logical :: cc_vv_colls
     integer :: i_q_fake, i_p_bz
     !                          
     isc_H%qs   = 1
     iscp_H%qs  = 1
     !
     call scatter_Gamp(isc_H,'x')
     !
     isc_H%is  =(/i_n,i_k,i_k_s,i_sp/)
     isc_H%os  =(/i_m,i_k,i_k_s,i_sp/)
     !
     call scatter_Bamp(isc_H)
     !
     do ip_sp=1,n_sp_pol
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
           cc_vv_colls=(i_np> n_met_bands(ip_sp)  .and. i_mp>n_met_bands(ip_sp) ) .or. &
           &           (i_np<=n_full_bands(ip_sp) .and. i_mp<=n_full_bands(ip_sp))
           !
           if ( COLLISIONS_CV_only .and.       cc_vv_colls  ) cycle
           if (         cc_vv_only .and. (.not.cc_vv_colls) ) cycle
           !
           if ( .not. PAR_IND_Bp_mat%element_1D( B_mat_index(i_np,i_mp,COLL_bands) ) .and. &
&               .not. PAR_IND_Bp_mat%element_1D( B_mat_index(i_mp,i_np,COLL_bands) )         ) cycle
           !
           iscp_H%is =(/i_np,i_p,1,ip_sp/)
           iscp_H%os =(/i_mp,i_p,1,ip_sp/)
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
                 pre_factor2=pre_factor2+TDDFT_ALDA_eh_space_R_kernel(isc_H%is,isc_H%os,iscp_H%is,iscp_H%os,tddft_wf,'RES')
               enddo
             endif
             !
             HXC_COLL_element(1)%v3_c(i_np,i_mp,i_p,ip_sp)=HXC_COLL_element(1)%v3_c(i_np,i_mp,i_p,ip_sp) &
             &                                                        +4._SP*pi*pre_factor1+Co*pre_factor2
             ! DS: 2023/10/25 Merge Check
             !HXC_COLL_element(1)%v3_c(i_np,i_mp,i_p,ip_sp)=HXC_COLL_element(1)%v3_c(i_np,i_mp,i_p,ip_sp) &
             !&                                                 +4._SP*pi*conjg(pre_factor1)+Co*pre_factor2
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
                 pre_factor2=pre_factor2+TDDFT_ALDA_eh_space_R_kernel(isc_H%is,isc_H%os,iscp_H%is,iscp_H%os,tddft_wf,'CPL')
               enddo
             endif
             !
             HXC_COLL_element(1)%v3_c(i_mp,i_np,i_p,ip_sp)=HXC_COLL_element(1)%v3_c(i_mp,i_np,i_p,ip_sp) &
             &                                                        +4._SP*pi*pre_factor1+Co*pre_factor2
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
     enddo
     !
   end subroutine compute_Hartree_collisions
   !
   subroutine compute_MBPT_XC_collisions(cc_vv_only)
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
     !                     /   G_{np,mp,i_sp}    \
     !  (i_n,i_k,i_sp)-->--.----------->---------.---->----(i_m,i_k,i_sp)
     !                          (i_kmq,i_kmq_s)
     !
     logical, intent(in)  :: cc_vv_only
     !
     logical :: cc_vv_colls
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
       i_kmq_bz =qindx_S(i_k,iqbz,1)
       i_kmq_G0 =qindx_S(i_k,iqbz,2)
       !
       i_kmq   = k%sstar(i_kmq_bz,1)
       i_kmq_s = k%sstar(i_kmq_bz,2)
       !
       isc_XC%qs  =(/i_kmq_G0,iqibz,iqs/)
       isc_XC%is  =(/i_n,i_k,  i_k_s,  i_sp/)
       isc_XC%os  =(/0  ,i_kmq,i_kmq_s,i_sp/)
       !
       iscp_XC%qs =(/i_kmq_G0,iqibz,iqs/)
       iscp_XC%is =(/i_m,i_k,  i_k_s,  i_sp/)
       iscp_XC%os =(/0,  i_kmq,i_kmq_s,i_sp/)
       !
       ! DS 2022/07/29
       ! The G shifts "i_kmq_G0" are need to construct M_{k,p}.
       ! The same procedure is applyed in the BSE subroutines.
       !
       ! However, for some reason, I get a non hermitian matrix, i.e. K_{nmk,n'm'k'} /= K^*_{n'm'k',nmk}
       ! due to a "-1" factor in the real part of some matrix elements.
       ! This issue was never seen in the BSE, since the hermiticity is imposed there.
       ! However, implementing the option to compute the full BSE blocks, i.e. without imposing 
       ! the hermiticity, the issue is there as well.
       !
       ! Without them the matrix M_{k,k+q}=M_{k,p+G} would be obtained
       ! Indeed setting i_kmq_G0=1 (i.e. G=0) the results are very bad.
       ! Both in the BSE and in the collisions, i.e. the matrix is not at all hermitian.
       !
       ! In the subrtouine QP_ppa_cohsex and XCo_Hartree_Fock.
       ! There the difference M_{k,k+q} vs M_{k,p} maybe non crucial, since
       ! the matrix is multiplied by a quantity (the occupaitons) which are invariant under G0 shift
       !
       if (iqibz/=iscp_XC%iqref) call scatter_Gamp(iscp_XC,'x')
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
           cc_vv_colls=(i_np> n_met_bands(i_sp)  .and. i_mp>n_met_bands(i_sp) ) .or. &
           &           (i_np<=n_full_bands(i_sp) .and. i_mp<=n_full_bands(i_sp))
           !
           if ( COLLISIONS_CV_only .and.       cc_vv_colls  ) cycle
           if (         cc_vv_only .and. (.not.cc_vv_colls) ) cycle
           !
           ! COLLISIONS(n,n',qp_n_states) = \sum_{G,G',q} ( rho(m,n,G,q,k) x EM1s(G ,G') x rho*(m',n',G',q,k) /|q+G|/|q+G'| )
           !
           if ( .not. PAR_IND_Bp_mat%element_1D( B_mat_index(i_np,i_mp,COLL_bands) ) ) cycle
           !
           isc_XC%os(1)=i_np
           !
           ! DS: 2023/10/25 Merge Check
           !if (any(isc_XC%is/=iscp_XC%is).or.any(isc_XC%os/=iscp_XC%os).or.any(isc_XC%qs/=iscp_XC%qs)) then
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
               ! DS: 2023/10/25 Merge Check
               !ig2=G_m_G(ig1,minus_G(i_kmq_G0))
               !pre_factor1=pre_factor1+conjg(isc_XC%rhotw(ig2))*iscp_XC%gamp(ig2,1)*iscp_XC%rhotw(ig2)
             enddo
             !$omp end parallel do
           endif
           !
           ! DEBUG <
           !ig2=G_m_G(1,minus_G(i_kmq_G0))
           !if ( (i_n==5 .and. i_m==9 .and. i_k==2 .and.  i_np==5 .and. i_mp==10 .and. i_kmq==1)  .or. &
           !&    (i_n==5 .and. i_m==10 .and. i_k==1 .and.  i_np==5 .and. i_mp==9 .and. i_kmq==2)  )    &
           !& write(201,*) i_n,i_m,i_k,i_np,i_mp,i_kmq,iqbz,iqibz,i_kmq_G0, &
           !&              conjg(isc_XC%rhotw(ig2)),iscp_XC%gamp(ig2,1),iscp_XC%rhotw(ig2)
           ! DEBUG >
           !
           pre_factor2=cZERO
           if(index(H_potential,"COH")>0.or.index(H_potential,"SEX")>0) then 
             !$omp parallel do default(shared), private(ig1,ig2), reduction(+:pre_factor2)
             do ig1=1,N_g_vecs_C
               do ig2=1,N_g_vecs_C
                 pre_factor2=pre_factor2+isc_XC%rhotw(ig2)*EM1s(ig2,ig1)*conjg(iscp_XC%rhotw(ig1))
                 ! DS: 2023/10/25 Merge Check
                 !pre_factor2=pre_factor2+conjg(isc_XC%rhotw(ig2))*EM1s(ig2,ig1)*conjg(iscp_XC%rhotw(ig1))
               enddo
             enddo
             !$omp end parallel do
           endif
           !
           if(index(H_potential,"SEX")>0 .or. index(H_potential,"FOCK")>0) &
&            HXC_COLL_element(1)%v3_c(i_np,i_mp,i_kmq,i_sp)=HXC_COLL_element(1)%v3_c(i_np,i_mp,i_kmq,i_sp) &
&                                                      -4._SP/spin_occ*pi*(pre_factor1+pre_factor2)
           if(index(H_potential,"COH")>0 ) &
&            COH_COLL_element(1)%v3_c(i_np,i_mp,i_kmq,i_sp)=COH_COLL_element(1)%v3_c(i_np,i_mp,i_kmq,i_sp) &
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
