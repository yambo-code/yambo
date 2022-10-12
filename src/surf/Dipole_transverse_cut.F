!
subroutine Dipole_transverse_cut(Xen,Xk,Dip)
 !
 ! This routine returns <iR> and <P^2>.
 !
 use pars,                ONLY:SP,schlen,cZERO,pi
 use interfaces,          ONLY:WF_load,WF_free
 use LIVE_t,              ONLY:live_timing
 use electrons,           ONLY:levels,n_spinor,n_sp_pol
 use openmp,              ONLY:n_threads_DIP,OPENMP_set_threads
 use D_lattice,           ONLY:alat
 use R_lattice,           ONLY:bz_samp,nXkibz
 use R_lattice,           ONLY:g_vec
 use DIPOLES,             ONLY:DIPOLE_t
 use X_m,                 ONLY:l_X_terminator
 use wave_func,           ONLY:WF,wf_ng
 use com,                 ONLY:msg
 use timing_m,            ONLY:timing
 use parallel_m,    ONLY:PP_indexes,myid,PP_indexes_reset,PARALLEL_n_structures_active
 use parallel_int,  ONLY:PP_redux_wait,PARALLEL_index,PARALLEL_assign_chains_and_COMMs,PARALLEL_live_message
 use ras_module,              ONLY : lras
 use optcut,                  ONLY : loptcut
 use optcut,                  ONLY : DIP_iR_cut, setup_optcut, pscut
 use parallel_m,    ONLY:PAR_COM_DIPk_ibz_A2A,PAR_COM_DIPk_ibz_INDEX, &
&                        PAR_COM_CON_INDEX_DIP,PAR_COM_VAL_INDEX_DIP, &
&                        PAR_COM_DIPk_bz_A2A, &
&                        PAR_COM_DIPk_bz_INDEX,COMM_reset
 use parallel_m,    ONLY:PAR_IND_DIPk_ibz,PAR_IND_CON_BANDS_DIP,PAR_IND_OVLPk_ibz,&
&                        PAR_IND_VAL_BANDS_DIP,PAR_IND_DIPk_bz
 use parallel_m,    ONLY:PAR_DIPk_bz_index,PAR_n_c_bands,PAR_n_v_bands,PAR_DIPk_ibz_index
 use parallel_m,    ONLY:PAR_IND_DIPk_bz_ID,PAR_IND_DIPk_ibz_ID,&
&                        PAR_IND_CON_BANDS_DIP_ID,PAR_IND_VAL_BANDS_DIP_ID,PAR_COM_Xk_bz_INDEX
 use parallel_m,    ONLY:PAR_DIPk_nibz,PAR_DIPk_nbz,PAR_build_index
 use parallel_m,    ONLY:PAR_K_range, PAR_Dip_ib,PAR_Dip_ib_lim
 use IO_int,        ONLY:io_control,IO_and_Messaging_switch
 use IO_m,          ONLY:manage_action,RD_CL_IF_END,OP_RD,OP_RD_CL
 !
#include<dev_defs.h>
#include<memory.h>
 !
 type(bz_samp), intent(in)    :: Xk
 type(levels),  intent(in)    :: Xen
 type(DIPOLE_t),intent(inout) :: Dip
 !
 ! Work Space
 !
 character(schlen)            :: live_t_string
 integer                      :: ik,ic_wf,iv_wf,i_wf,ic,iv,ib,ifrag,nc,&
&                                i_sp_pol,n_LT_steps,ik_mem,ic_min,is,ia,pp_range(2),npp,npp_tab,i1,i_k
 complex(SP)                  :: rho(3)
 real(SP)                     :: Ev_m_Ev_KS,Dipole_Energy_treshold
 real(SP), allocatable            :: kg(:,:)
 complex(SP)                      :: PS(3)
 !
 !I/O and external functions
 !
 integer                       :: IO_ACT_NOW
 !
  call timing('Dipoles_transverse_cut',OPR='start') 

   PAR_Dip_ib=Dip%ib
   PAR_Dip_ib_lim=Dip%ib_lim

   PAR_K_range=Xk%nibz
   PAR_n_c_bands= (/PAR_Dip_ib_lim(2),PAR_Dip_ib(2)/)
   PAR_n_v_bands= (/PAR_Dip_ib(1),PAR_Dip_ib_lim(1)/)
   
   call PP_indexes_reset(PAR_IND_DIPk_ibz)
!  call PP_indexes_reset(PAR_IND_DIPk_bz)
   call PP_indexes_reset(PAR_IND_CON_BANDS_DIP)
   call PP_indexes_reset(PAR_IND_VAL_BANDS_DIP)
   PAR_IND_CON_BANDS_DIP_ID=0
   PAR_IND_VAL_BANDS_DIP_ID=0

  !call PARALLEL_get_ENVIRONMENT_structure("DIPOLES")
  
  !call PARALLEL_get_user_structure("DIPOLES",.TRUE.)
  
  call PARALLEL_global_defaults("DIPOLES_CUT")

  call PARALLEL_structure(3,(/"k","c","v"/))
   !  !
  call PARALLEL_assign_chains_and_COMMs(3,COMM_index_1=PAR_COM_DIPk_ibz_INDEX,&
  &                                        COMM_index_2=PAR_COM_CON_INDEX_DIP, &
  &                                        COMM_index_3=PAR_COM_VAL_INDEX_DIP, &
  &                                        COMM_A2A_1=PAR_COM_DIPk_ibz_A2A)
  call PARALLEL_index(PAR_IND_DIPk_ibz,(/Xk%nibz/),COMM=PAR_COM_DIPk_ibz_INDEX,CONSECUTIVE=.TRUE.,NO_EMPTIES=.TRUE.)
  PAR_IND_DIPk_ibz_ID=PAR_COM_DIPk_ibz_INDEX%CPU_id 
!  call PARALLEL_distribute_BZk_using_IBZk(PAR_COM_DIPk_ibz_INDEX,Xk,PAR_IND_DIPk_ibz,PAR_IND_DIPk_ibz_ID,&
!  &                                                       PAR_IND_DIPk_bz, PAR_IND_DIPk_bz_ID,&
!  &                                                       PAR_DIPk_bz_index,PAR_DIPk_nbz)
   call PAR_build_index(PAR_IND_DIPk_ibz,Xk%nibz,PAR_DIPk_ibz_index,PAR_DIPk_nibz)
   call PARALLEL_live_message("K(ibz)",ENVIRONMENT="DIPOLES_CUT",LOADED=PAR_DIPk_nibz,TOTAL=Xk%nibz,&
 &                           NCPU=PAR_COM_DIPk_ibz_INDEX%n_CPU)

 ! I/O privileges
 !
 if (PARALLEL_n_structures_active>1) then
   call IO_and_Messaging_switch("+io_out",CONDITION=PAR_COM_DIPk_ibz_A2A%CPU_id==0)
 else
   call IO_and_Messaging_switch("+io_out",CONDITION=.TRUE.)
 endif

  call OPENMP_set_threads(n_threads_in=n_threads_DIP)
   
   call PARALLEL_index(PAR_IND_CON_BANDS_DIP,(/PAR_n_c_bands(2)/),low_range=(/PAR_n_c_bands(1)/),&
    &                    COMM=PAR_COM_CON_INDEX_DIP,CONSECUTIVE=.TRUE.,NO_EMPTIES=.TRUE.)
     PAR_IND_CON_BANDS_DIP_ID=PAR_COM_CON_INDEX_DIP%CPU_id
     call PARALLEL_live_message("CON bands",ENVIRONMENT="DIPOLES_CUT",&
    &                           LOADED=PAR_IND_CON_BANDS_DIP%n_of_elements(PAR_COM_CON_INDEX_DIP%CPU_id+1),&
    &                           TOTAL=PAR_n_c_bands(2)-PAR_n_c_bands(1)+1,&
    &                           NCPU=PAR_COM_CON_INDEX_DIP%n_CPU)
!  
     call PARALLEL_index(PAR_IND_VAL_BANDS_DIP,(/PAR_n_v_bands(2)/),low_range=(/PAR_n_v_bands(1)/),&
    &                    COMM=PAR_COM_VAL_INDEX_DIP,CONSECUTIVE=.TRUE.,NO_EMPTIES=.TRUE.)
     PAR_IND_VAL_BANDS_DIP_ID=PAR_COM_VAL_INDEX_DIP%CPU_id
     call PARALLEL_live_message("VAL bands",ENVIRONMENT="DIPOLES_CUT",&
    &                           LOADED=PAR_IND_VAL_BANDS_DIP%n_of_elements(PAR_COM_VAL_INDEX_DIP%CPU_id+1),&
    &                           TOTAL=PAR_n_v_bands(2)-PAR_n_v_bands(1)+1,&
    &                           NCPU=PAR_COM_VAL_INDEX_DIP%n_CPU)
   !
 n_LT_steps=0
 do iv=Dip%ib(1),Dip%ib_lim(1)
  if(.not.PAR_IND_VAL_BANDS_DIP%element_1D(iv)) cycle
  ic_min=max(iv,Dip%ib_lim(2))
  if (l_X_terminator) ic_min=iv
  do ic=ic_min,Dip%ib(2)
    if(.not.PAR_IND_CON_BANDS_DIP%element_1D(ic)) cycle
    n_LT_steps=n_LT_steps+1
  enddo
 enddo
 n_LT_steps=n_LT_steps*n_sp_pol*PAR_IND_DIPk_ibz%n_of_elements(PAR_IND_DIPk_ibz_ID+1)
 IO_ACT_NOW=OP_RD
 if (n_LT_steps==0) IO_ACT_NOW=OP_RD_CL
 if (n_LT_steps>0) call live_timing('Dipole, cut (T)',n_LT_steps)

allocate( kg(3,wf_ng) )
 !
 ! Main loop over k in IBZ
 do i_sp_pol=1,n_sp_pol
   if (n_LT_steps==0) cycle
   do ik=1,Xk%nibz
      if (.not.PAR_IND_DIPk_ibz%element_1D(ik)) cycle
 
     ik_mem=PAR_DIPk_ibz_index(ik)
     
     do iv=Dip%ib(1),Dip%ib_lim(1)
       if(.not.PAR_IND_VAL_BANDS_DIP%element_1D(iv)) cycle 
       !
       ic_min=max(iv,Dip%ib_lim(2))
       if (l_X_terminator) ic_min=iv
       !
       do ic=ic_min,Dip%ib(2)
          if(.not.PAR_IND_CON_BANDS_DIP%element_1D(ic)) cycle
         !
         !
         iv_wf=WF%index(iv,ik,i_sp_pol)
         ic_wf=WF%index(ic,ik,i_sp_pol)
         !
         if (iv_wf==0) call error(" Error in parallel wf distribution (Dipole iv)")
         if (ic_wf==0) call error(" Error in parallel wf distribution (Dipole ic)")
         !
         !
         Dipole_Energy_treshold=Dip%Energy_treshold 
         !
           do i1 = 1,3
              kg(i1,:) = ( Xk%pt(ik,i1) + g_vec(1:wf_ng, i1) ) * 2. * pi/alat(i1) ! -i grad Wf
           enddo
           call setup_optcut( ik )
           rho(:)=(0._SP,0._SP)

           call PScut(PS, WF%c(:,i_sp_pol,iv_wf), WF%c(:,i_sp_pol,ic_wf), kg = kg )
 
           if (.not.allocated(Xen%Eo)) Ev_m_Ev_KS=Xen%E (iv,ik,i_sp_pol)-Xen%E (ic,ik,i_sp_pol)
           if (     allocated(Xen%Eo)) Ev_m_Ev_KS=Xen%Eo(iv,ik,i_sp_pol)-Xen%Eo(ic,ik,i_sp_pol)

           rho(1:3) = PS(:)/Ev_m_Ev_KS
           if (abs(Ev_m_Ev_KS)> 1.E-5_SP) DIP_iR_cut(:,ic,iv,ik_mem,i_sp_pol)=rho
           if (abs(Ev_m_Ev_KS)<=1.E-5_SP) DIP_iR_cut(:,ic,iv,ik_mem,i_sp_pol)=cZERO
         !
           call live_timing(steps=1)
       enddo    ! conduction band loop
     enddo      ! valence    band loop
     ! 
   enddo        ! k-points loop
 enddo          ! sp_pol   loop
 !
 call live_timing()
 deallocate(kg)
 !
 !
do i_sp_pol=1,n_sp_pol
  do ik=1,Xk%nibz
       if (.not.PAR_IND_DIPk_ibz%element_1D(ik)) cycle
       ik_mem=PAR_DIPk_ibz_index(ik)
       call PP_redux_wait(DIP_iR_cut(:,:,:,ik_mem,i_sp_pol),COMM=PAR_COM_DIPk_ibz_A2A%COMM)
  enddo
enddo

  call timing('Dipoles_transverse_cut',OPR='stop')

end subroutine 
