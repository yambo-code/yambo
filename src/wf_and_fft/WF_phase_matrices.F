!
! License-Identifier: GPL
!
! Copyright (C) 2015 The Yambo Team
!
! Authors (see AUTHORS file for details): DS
!      
!> @brief Calculate overlaps between wave-function
!   <\psi_{Ra(kibz_i)} | \psi_{kbz_j} >
!  with for Ra(kibz_i)=kbz_j
!
!! @param[out]      WF_phases_S            Phase matrices
!
#include<dev_defs.h>
!
subroutine WF_phase_matrices(En,Xk,blim,klim)
 !    
 use pars,              ONLY:SP,cZERO
 use units,             ONLY:HA2EV
 use LIVE_t,            ONLY:live_timing
 use R_lattice,         ONLY:bz_samp,G_m_G,minus_G,g_vec,rl_sop
 use D_lattice,         ONLY:nsym,i_time_rev,sop_tab,DEV_VAR(spin_sop)
 use electrons,         ONLY:levels,n_max_deg
 use wave_func,         ONLY:WAVEs,io_WF_phases,WF_phases_b_map
 use wave_func,         ONLY:wf_ng_overlaps,wf_ng_1st_BZ,WF_copy,wf_nc_k
 use stderr,            ONLY:intc
 use electrons,         ONLY:n_spinor,n_sp_pol
 use wrapper,           ONLY:Vstar_dot_V
 use hamiltonian,       ONLY:B_mat_index
 use matrix_operate,    ONLY:m3det!,UP_matrix_index
 use vec_operate,       ONLY:degeneration_finder
 use parallel_m,        ONLY:PAR_IND_WF_k,PAR_IND_WF_b,PAR_IND_B_mat
 use parallel_int,      ONLY:PP_redux_wait,PARALLEL_WF_index,PARALLEL_WF_distribute
 use interfaces,        ONLY:WF_symm_kpoint_g,WF_symm_kpoint_g_BZ,WF_symm_kpoint_g_ssop,WF_load,WF_free,eval_G_minus_G
 use IO_int,            ONLY:io_control
 use IO_m,              ONLY:VERIFY,DUMP,REP,OP_WR,WR,WR_CL,OP_RD_CL
 use timing_m,          ONLY:timing
 !
#include<memory.h>
 !
 integer,       intent(in) :: blim(2),klim(2)
 type(levels),  intent(in) :: En
 type(bz_samp), intent(in) :: Xk 
 !
 ! Work Space
 !
 integer           :: first_el(blim(2)-blim(1)+1),n_of_el(blim(2)-blim(1)+1),&
 &                    blim_n(2),n_deg_grp,i_grp,g0_idx(3,2),nsz(5),max_igk
 real(SP)          :: g0_length(3)
 integer           :: ikbz,iRkbz,isk,isloop,isk_c,isk_y,iGk_y,iGk_c,iGk,iGloop,istark,&
 &                    iby,ibc,iby_phase,ibc_phase,i_sp_pol,ik,&
 &                    ifrag,s_count,i_wf_y,i_wf_c,iGp,ng_tmp,nb,NG_max
 complex(SP)       :: det,charac(blim(2)-blim(1)+1,nsym)
 complex(SP)       :: spin_sop_composed(2,2),s1(2,2),s2(2,2)
#if defined _CUDA
 complex(SP) DEV_ATTR     :: spin_sop_composed_d(2,2)
#endif
 complex(SP), allocatable :: WF_phases(:,:,:,:,:)
 !
 type(WAVEs) :: WF_k,WF_Rk_c,WF_Rk_y,WF_Rk
 !
 integer              :: io_err,ID
 !
 if (nsym==1) return
 !
 nsz=0
 call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1/),MODE=VERIFY,ID=ID)
 io_err=io_WF_phases(blim,0,0,0,ID,nsz)
 !
 if (io_err==0) return
 !
 call timing("WF_phases",OPR="start")
 !
 !call k_find_smallest_g0(g0_idx,g0_length)
 !iGp=max(maxval(g0_idx(:,1)),maxval(g0_idx(:,2)))
 iGp=2*maxval(Xk%g_table)
 ng_tmp=eval_G_minus_G(wf_ng_1st_BZ,iGp)
 !
 nb=blim(2)-blim(1)+1
 !
 !call PARALLEL_WF_distribute(K_index=PAR_IND_WF_k,B_index=PAR_IND_WF_b,CLEAN_UP=.TRUE.)
 call PARALLEL_WF_distribute(CLEAN_UP=.TRUE.)
 call PARALLEL_WF_index( )
 !
 YAMBO_ALLOC(WF_phases_b_map,(blim(1):blim(2),klim(1):klim(2),n_sp_pol,2))
 !
 call io_control(ACTION=OP_WR,COM=REP,SEC=(/1/),MODE=DUMP,ID=ID)
 io_err=io_WF_phases(blim,0,0,0,ID,nsz)
 !
 WF_phases_b_map=0
 !
 !call live_timing("WF_phases:",PAR_IND_WF_k%n_of_elements(PAR_IND_DIPk_bz_ID+1)*n_sp_pol)
 call live_timing("WF_phases:",(sum(Xk%nstar(klim(1):klim(2))))*n_sp_pol)
 !
 ifrag=0
 !
 if (maxval(wf_nc_k)>ng_tmp) then
  call error("MAX(wf_nc_k) ("//trim(intc(maxval(wf_nc_k)))//") > wf_ng ("//trim(intc(ng_tmp))&
             &//"), it's not possible to compute WF phases.")
 endif
 !
 spin_sop_composed=cZERO
 !
 do i_sp_pol=1,n_sp_pol
   !
   do ik=klim(1),klim(2)
     !
     ifrag=ifrag+1
     !
     call WF_load(WF_k,0,1,blim,(/ik,ik/),(/i_sp_pol,i_sp_pol/),&
     & space='G',title='-WF_phases',quiet=ifrag>1,keep_states_to_load=.true.)
     !
     call degeneration_finder(En%E(blim(1):blim(2),ik,i_sp_pol),nb,&
     &                           first_el,n_of_el,n_deg_grp,0.0001/HA2EV,Include_single_values=.true.)
     !
     do i_grp=1,n_deg_grp
       do iby_phase=1,n_of_el(i_grp)
         iby=iby_phase+first_el(i_grp)-1+blim(1)-1
         WF_phases_b_map(iby,ik,i_sp_pol,:)=(/iby_phase,i_grp/)
       enddo
     enddo
     !
     nsz=(/maxval(n_of_el),maxval(n_of_el),n_deg_grp,nsym,Xk%nstar(ik)/)
     !
     YAMBO_ALLOC(WF_phases,(nsz(1),nsz(2),nsz(3),nsz(4),nsz(5)))
     WF_phases=cZERO
     !
     do istark=1,Xk%nstar(ik)
       !
       !if ( .not.PAR_IND_WF_k%element_1D(ik) ) cycle
       !
       isk =Xk%star(ik,istark)
       ikbz=Xk%k_table(ik,isk)
       !
       iGk =minus_G(Xk%g_table(ik,isk))
       !
       call WF_copy(WF_k,WF_Rk)
       call WF_symm_kpoint_g(blim,ik,i_sp_pol,isk,iGk,WF_Rk)
       !
       do isloop=1,nsym
         !
         ! symmetry composed
         isk_c=sop_tab(isloop,isk)
         iGk_c =minus_G(Xk%g_table(ik,isk_c))
         !
         iGloop=G_m_G(iGk_c,iGk)
         !
         ! spin_sop with time-reversal are not a space group
         ! I need to build the composed spin symmetry operator
         !
         if(n_spinor==2) then
           s1=spin_sop(:,:,isk)
           s2=spin_sop(:,:,isloop)
           if (isk   >nsym/(1+i_time_rev)) s1=conjg(s1)
           if (isloop>nsym/(1+i_time_rev)) then
             s2=conjg(s2)
             s1=conjg(s1)
           endif
           spin_sop_composed=matmul(s2,s1)
           if (isk_c >nsym/(1+i_time_rev)) spin_sop_composed=conjg(spin_sop_composed)
#if defined _CUDA
           call dev_memcpy(spin_sop_composed_d,spin_sop_composed)
#endif
         endif
         !
         ! symmetry used by yambo
         isk_y=Xk%s_table(ik,isk_c)
         iGk_y =minus_G(Xk%g_table(ik,isk_y))
         !
         iRkbz =Xk%k_table(ik,isk_c)
         !
         ! OPTION 1: Apply second symmetry operatio to the WF already expanded in the BZ
         call WF_copy(WF_Rk,WF_Rk_c)
         call WF_symm_kpoint_g_BZ(blim,ik,i_sp_pol,isloop,iGloop,isk,iGk,WF_Rk_c)
         !
         ! OPTION 2: Use the composed symmetry, importing the composed spin soc
         !call WF_copy(WF_k,WF_Rk_c)
         !call WF_symm_kpoint_g_ssop(blim,ik,i_sp_pol,isk_c,iGk_c,spin_sop_composed,WF_Rk_c)
         !
         call WF_copy(WF_k,WF_Rk_y)
         call WF_symm_kpoint_g(blim,ik,i_sp_pol,isk_y,iGk_y,WF_Rk_y)
         !
         do i_grp=1,n_deg_grp
           blim_n=(/1,n_of_el(i_grp)/)
           blim_n=blim_n+first_el(i_grp)-1+blim(1)-1
           !
           do iby_phase=1,n_of_el(i_grp)
             iby=iby_phase+first_el(i_grp)-1+blim(1)-1
             i_wf_y=WF_k%index(iby,ik,i_sp_pol)
             do ibc_phase=1,n_of_el(i_grp)
               ibc=ibc_phase+first_el(i_grp)-1+blim(1)-1
               i_wf_c=WF_k%index(ibc,ik,i_sp_pol)
               !
               WF_phases(iby_phase,ibc_phase,i_grp,isloop,istark)= &
               & Vstar_dot_V(wf_ng_1st_BZ*n_spinor,WF_Rk_y%c(:,:,i_wf_y),WF_Rk_c%c(:,:,i_wf_c))
               !
             enddo
             charac(i_grp,isloop)=charac(i_grp,isloop)+WF_phases(iby_phase,iby_phase,i_grp,isloop,istark)
           enddo
           !
         enddo
         !
         !do ibk=blim(1),blim(2)
         !  i_wf_k=WF_k%index(ibk,ik,i_sp_pol)
         !  do ibp=blim(1),blim(2)
         !    i_wf_p=WF_p%index(ibp,ip,i_sp_pol)
         !    !if (.not.PAR_IND_B_mat_ordered%element_1D( UP_matrix_index(ib,ibp)-1 )) cycle
         !    !if (.not.PAR_IND_B_mat%element_1D( B_mat_index(ib,ibp,(/1,En%blim/))-1 )) cycle
         !    WF_phases(ibk,ibp,s_count,k_count,i_sp_pol)= &
         !    & Vstar_dot_V(wf_ng_1st_BZ*n_spinor,WF_k%c(:,:,i_wf_k),WF_p_tmp%c(:,:,i_wf_p))
         !  enddo ! ibp
         !enddo ! ibk
         !
         call WF_free(WF_Rk_c)
         call WF_free(WF_Rk_y)
         !
         ! DEBUG <
         !call SERIAL_inversion(nb,WF_phases(:,:,s_count,k_count,i_sp_pol),det,.true.)
         !!
         !write(*,*) i_sp_pol,ikbz,isp,iGp,abs(det)
         ! DEBUG >
         !
       enddo ! isp
       !
       call WF_free(WF_Rk)
       !
       call live_timing(steps=1)
       !
       !call PP_redux_wait(WF_phases(:,:,s_count,k_count,i_sp_pol))
       !
       ! DEBUG <
       !write(*,*) "Character table at k= ",ikbz
       !write(*,*) "Number of symmetry elements= ",s_count
       !do i_grp=1,n_deg_grp
       !  write(*,*) En%E(first_el(i_grp),ik,i_sp_pol)*HA2EV,n_of_el(i_grp),real(charac(i_grp,:s_count))
       !enddo
       !write(*,*) ""
       ! DEBUG >
       !
     enddo ! istar
     !
     call io_control(ACTION=WR,COM=REP,SEC=(/3/),MODE=DUMP,ID=ID)
     io_err=io_WF_phases(blim,ik,0,i_sp_pol,ID,nsz,WF_phases)
     !
     YAMBO_FREE(WF_phases)
     !
     call WF_free(WF_k)
     !
   enddo   ! ik
   !
 enddo ! i_sp_pol
 !
 call live_timing()
 !
 nsz=0
 call io_control(ACTION=WR_CL,COM=REP,SEC=(/2/),MODE=DUMP,ID=ID)
 io_err=io_WF_phases(blim,0,0,0,ID,nsz)
 !
 YAMBO_FREE(WF_phases_b_map)
 !
 call timing("WF_phases",OPR="stop")
 !
end subroutine WF_phase_matrices
