!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM FP
!
subroutine PHEL_driver(E,k,q)
 !
 use pars,           ONLY:SP,cI
 use drivers,        ONLY:l_life
 use electrons,      ONLY:levels,n_sp_pol,E_fine_grid
 use R_lattice,      ONLY:bz_samp,bz_samp_FineGd_default,bz_samp_FineGd_alloc,bz_fine_grid
 use interfaces,     ONLY:ELPH_alloc
 use ELPH,           ONLY:QP_PH_n_G_bands,elph_branches,GKKP_pe_sq,l_GKKP_DB_exists,&
&                         l_GKKP_expanded_DB_exists,elph_nb,elph_use_q_grid
 use PHEL,           ONLY:l_PH_uses_E_FineGd,PH_Self_Energy_mode,PHEL_free,PH_Q,PH_SF_W,PHEL_alloc,PHEL_qp,&
&                         PH_DbGd_points,PH_energy,KpQ_io_map,phel_info,PHEL_fit_Energies,&
&                         QPH_OMS,QPH_OMS_bare_Pole,QPH_full,QPH,QPHt_alloc,QPH_DDFPT
 use parallel_int,   ONLY:PP_redux_wait,PARALLEL_global_indexes
 use RT_lifetimes,   ONLY:RT_PE_life,RT_lifetime_alloc,Life_MEM_steps,l_initial_equilibrium,RT_ALL_lifetimes_free                        
 use RT_occupations, ONLY:RT_EL_occupations_alloc,RT_PH_occupations_alloc,RT_ALL_occupations_free
 use real_time,      ONLY:l_RT_uses_E_FineGd
 use QP_m,           ONLY:l_extended_output,QP_Sc_steps,QP_G_er,QP_G_damp,l_QP_eval_GF,QP_solver,On_Mass_Shell_approx
 use IO_int,         ONLY:io_control
 use IO_m,           ONLY:OP_RD_CL,VERIFY,REP,OP_WR_CL
 use frequency,      ONLY:W_reset
 use stderr,         ONLY:STRING_same
 use parallel_m,     ONLY:PARs_PH_Q_ibz
 !
#include<memory.h>
 !
 type(levels) ,intent(in) :: E
 type(bz_samp),intent(in) :: k,q
 !
 ! Work Space
 !
 integer             ::i_err,ID,nk_CG,iq,il
 logical             ::l_SS_test
 integer, external   ::io_PH
 type(E_fine_grid)   ::E_FG
 type(bz_fine_grid)  ::k_FG
 !
 call section('*',"Phonon-Electron Interaction and properties")
 !==============================================================
 !
 ! Setup
 !=======
 ! ... logicals
 call QP_logicals( )
 ! ... BZ Setup
 call k_build_up_BZ_tables(q)
 !
 ! El-Ph databases check
 !=======================
 call section('+',"Databases I/O")
 call ELPH_databases_check(elph_nb)
 !
 if (.not.any((/l_GKKP_DB_exists,l_GKKP_expanded_DB_exists/))) call error("GKKP databases absent or damaged")
 !
 ! Already calculated?
 ! =====================
 call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1/),MODE=VERIFY,ID=ID)
 i_err=io_PH(ID)
 if (i_err==0) then
   call io_control(ACTION=OP_RD_CL,SEC=(/2/),MODE=VERIFY,ID=ID)
   i_err=io_PH(ID)
   call PH_of(q)
   call CLEAN()
   return
 endif
 !
 ! Q ranges
 !============================
 !
 !             / ypp_ph            elph_use_q_grid=F
 ! elph_nQ = | 
 !             \ nqibz             elph_use_q_grid=T
 !
 ! Parallel Setup
 !===============
 call PARALLEL_global_indexes(E,k,q,"Phonon_Self_Energy")
 !
 ! Q/K grid remapping
 !====================
 call ELPH_databases_grids_map(k,q)
 !
 ! El-Ph databases I/O
 !=====================
 call section("=","El-Ph databases loading")
 call ELPH_databases_load(GKKP_pe_sq,E,k,q,QP_PH_n_G_bands,PH_Q,"Q-sym Q-IBZ K-BZ",PH_Self_Energy_mode)
 !
 !...acoustic branches
 call ELPH_acoustic_phonon_properties(q)
 !
 ! Static-Screening test/correction 
 !==================================
 call PHEL_screening(E,q,l_SS_test)
 if (l_SS_test) return
 !
 ! Maps into the RT kinds
 !========================
#if defined _RT
 ! ... grid in the IBZ
 l_RT_uses_E_FineGd=k%FGibz%N>0
 if (.not.l_RT_uses_E_FineGd) then
   call bz_samp_FineGd_alloc(k%FGibz,k%nibz)
   call bz_samp_FineGd_default(k%FGibz,k)
 endif
 k_FG = k%FGibz
 nk_CG= k%nibz
#else
 ! ...DbGd in the full BZ
 l_PH_uses_E_FineGd=k%FGbz%N>0
 call k_ibz2bz(k,'i',.true.)
 if (l_PH_uses_E_FineGd) then
   if (PH_DbGd_points<0) PH_DbGd_points=k%FGbz%N
 else
   call bz_samp_FineGd_alloc(k%FGbz,k%nbz)
   call bz_samp_FineGd_default(k%FGbz,k,k%nbz)
   PH_DbGd_points=0
 endif
 k_FG = k%FGbz
 nk_CG= k%nbz
#endif
 !
 ! Transitions and frequencies
 !=============================
 !
 ! ... select fine grid subset
 call PHEL_double_grid_subset(k,k_FG,nk_CG)
 !
 ! ... Spectral Function energy points
 if (l_life) then
   QP_Sc_steps=1
   call QPHt_alloc("OMS_bare_Pole",QPH_OMS_bare_Pole,0)
 else
   !
   if (On_Mass_Shell_approx) call QPHt_alloc("OMS",QPH_OMS,QP_Sc_steps)
   call QPHt_alloc("OMS_bare_Pole",QPH_OMS_bare_Pole,QP_Sc_steps)
   call QPHt_alloc("QPH",QPH,QP_Sc_steps)
   if (l_QP_eval_GF) then
     call QPHt_alloc("FULL",QPH_full,QP_Sc_steps)
   else 
     call QPHt_alloc("DDFPT",QPH_DDFPT,0)
   endif
   !
   allocate(PH_SF_W(elph_branches(1):elph_branches(2),PH_Q(1):PH_Q(2)))
   do iq=PH_Q(1),PH_Q(2)
     do il=elph_branches(1),elph_branches(2)
       call W_reset(PH_SF_W(il,iq))
       PH_SF_W(il,iq)%dr=QP_G_damp
       PH_SF_W(il,iq)%n_freqs=QP_Sc_steps 
       PH_SF_W(il,iq)%er=QP_G_er
       call FREQUENCIES_setup(PH_SF_W(il,iq))
       if (On_Mass_Shell_approx) QPH_OMS%W=PH_SF_W(il,iq)%p
       QPH_OMS_bare_Pole%W=PH_SF_W(il,iq)%p
       QPH%W=PH_SF_W(il,iq)%p
       if (l_QP_eval_GF) QPH_full%W=PH_SF_W(il,iq)%p
     enddo
   enddo
   !
   ! SE energy points
   if (STRING_same(QP_solver,"n")) then
     do iq=PH_Q(1),PH_Q(2)
       do il=elph_branches(1),elph_branches(2)
         call W_reset(PH_SF_W(il,iq))
         QP_Sc_steps=4
         PH_SF_W(il,iq)%n_freqs=4 
         !
         ! Fake range just to call FREQUENCIES_setup
         PH_SF_W(il,iq)%er=(/0._SP,1._SP/)
         call FREQUENCIES_setup(PH_SF_W(il,iq))
         !
         ! Manual points
         PH_SF_W(il,iq)%p(  1)=cI*QP_G_damp
         PH_SF_W(il,iq)%p(2:3)=PHEL_fit_Energies*PH_energy(q,il,iq)+cI*QP_G_damp
         PH_SF_W(il,iq)%p(  4)=cI*PH_energy(q,il,iq)
       enddo
     enddo
   endif
 endif
 !
 ! ... allocs
#if defined _RT
 Life_MEM_steps=1
 call RT_lifetime_alloc("ph-el",RT_PE_life,elph_branches,PH_Q(2))
#else
 call PHEL_alloc( )
#endif
 !
 ! ... occupations 
 call RT_EL_occupations_alloc((/QP_PH_n_G_bands(1),QP_PH_n_G_bands(2)/),k_FG%N)
#if defined _RT
 call RT_PH_occupations_alloc(elph_branches,PH_Q(2))
#endif
 !
 ! ... grids
 if (l_RT_uses_E_FineGd .or. l_PH_uses_E_FineGd) then
   YAMBO_ALLOC(E_FG%E,(elph_nb,k_FG%N,n_sp_pol))
   YAMBO_ALLOC(E_FG%W,(elph_nb,k_FG%N,n_sp_pol))
 else
   YAMBO_ALLOC(E_FG%E,(elph_nb,nk_CG,n_sp_pol))
   YAMBO_ALLOC(E_FG%W,(elph_nb,nk_CG,n_sp_pol))
 endif
 !
 ! ... occupations & levels
 call RT_occupations_and_levels_init(QP_PH_n_G_bands,E,k,q,k_FG,E_FG)
 !
 ! Get nearest k-q table if using custom qpoints
 if (.not.elph_use_q_grid) then 
   !
   call section("=","K-Q table (non uniform Q-grid)")
   !--------------------------------------------------
   YAMBO_ALLOC(KpQ_io_map,(PH_Q(1):PH_Q(2),k%nbz) )
   call PHEL_k_nearest_table(k)
 endif
 !
 ! Q-loops
 !
 do iq=PH_Q(1),PH_Q(2)
   !
   if (.not.PARs_PH_Q_ibz%IND%element_1D(iq)) cycle
   !
   ! Select allowed transitions for phonon scatterings
   !===================================================
   call PH_Grid_Check_Transitions(iq,k,q,QP_PH_n_G_bands,k_FG,E_FG)
   !
   ! Calculation of the lifetimes
   !  - Equilibrium (yambo_ph): linewidths with retarded self-energy S^R
   !  - Out of eq.  (yambo_rt): scatt. rates with lesser/greater S^< and S^>
   !==============================
#if defined _RT 
   l_initial_equilibrium=.TRUE.
   !call PHEL_Lifetimes(E,k,q,k_FG,E_FG)
#else
   call PHEL_Self_Energy(iq,E,k,q,k_FG,E_FG)
#endif
   !
   YAMBO_FREE(phel_info%table)
   !
 enddo
 !
 ! Redux
 call REDUX(PARs_PH_Q_ibz%COMM_a2a%COMM)
 call REDUX(PARs_PH_Q_ibz%COMM_i%COMM)
 !
 call section("=","Transitions Report")
 !-------------------------------------
 call PH_Transitions_Report( )
 !
 ! Output DB
 !===========
 call section('=',"Output databases")
 call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1,2/),ID=ID)
 i_err=io_PH(ID)
 !
 ! Output file
 !=============
 call PH_of(q)
 !
 ! Clean
 !=======
 call CLEAN()
 !
 contains
   !
   subroutine REDUX(COMM)
     integer :: COMM
#if defined _RT 
!     call PP_redux_wait(RT_PE_life%emit)
!     call PP_redux_wait(RT_PE_life%abs)
#endif
     call PP_redux_wait(PHEL_qp%PI,COMM=COMM)
     if (l_extended_output) then
       call PP_redux_wait(PHEL_qp%interband,COMM=COMM)
       call PP_redux_wait(PHEL_qp%intraband,COMM=COMM)
       call PP_redux_wait(PHEL_qp%allen,COMM=COMM)
     endif
   end subroutine
   !
   subroutine CLEAN()
     call RT_ALL_lifetimes_free(  )
     call RT_ALL_occupations_free(  )
     call ELPH_alloc('FREE',GKKP=GKKP_pe_sq)
     call PHEL_free( )
   end subroutine
   !
end subroutine PHEL_driver
