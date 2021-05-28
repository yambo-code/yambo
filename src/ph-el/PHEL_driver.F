!
!        Copyright (C) 2000-2021 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM FP
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
subroutine PHEL_driver(E,k,q)
 !
 use electrons,           ONLY:levels,n_sp_pol,E_fine_grid
 use R_lattice,           ONLY:bz_samp,bz_samp_FineGd_default,bz_samp_FineGd_alloc,bz_fine_grid
 use interfaces,          ONLY:ELPH_alloc
 use ELPH,                ONLY:QP_PH_n_G_bands,elph_branches,GKKP_pe_sq,l_GKKP_DB_exists,l_GKKP_expanded_DB_exists,&
&                              l_GKKP_hosts_bare_dV,elph_nb
 use PHEL,                ONLY:l_PH_uses_E_FineGd,PH_Self_Energy_mode,PHEL_free,PH_Q,PH_SF_W ,PHEL_alloc,PHEL_qp
 use parallel_int,        ONLY:PP_redux_wait,PARALLEL_global_indexes
 use RT_lifetimes,        ONLY:RT_PE_life,RT_lifetime_alloc,Life_MEM_steps,l_initial_equilibrium,RT_ALL_lifetimes_free                        
 use RT_occupations,      ONLY:RT_EL_occupations_alloc,RT_PH_occupations_alloc,RT_ALL_occupations_free
 use real_time,           ONLY:l_RT_uses_E_FineGd
 use parser_m,            ONLY:parser
 use QP_m,                ONLY:l_extended_output,QP_Sc_steps,QP_solver,QP_G_er,QP_G_damp
 use IO_int,              ONLY:io_control
 use IO_m,                ONLY:OP_RD_CL,VERIFY,REP,OP_WR_CL
 use frequency,           ONLY:W_reset
 !
#include<memory.h>
 !
 type(levels) ,intent(in) :: E
 type(bz_samp),intent(in) :: k,q
 !
 ! Work Space
 !
 integer             ::i_err,ID,nk_CG,nQ_W
 integer, external   ::io_PH
 type(E_fine_grid)   ::E_FG
 type(bz_fine_grid)  ::k_FG
 !
 call section('*',"Phonon Lifetimes")
 !
 ! Setup
 !=======
 if ( index(PH_Self_Energy_mode,"bare")>0 ) l_GKKP_hosts_bare_dV = .true.
 call parser('ExtendOut',  l_extended_output)
 ! ... BZ Setup
 call k_build_up_BZ_tables(q)
 !
 ! El-Ph databases check
 !=======================
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
 call ELPH_databases_load(GKKP_pe_sq,E,k,q,(/QP_PH_n_G_bands(1),QP_PH_n_G_bands(2)/),"Q-sym Q-IBZ K-BZ",PH_Self_Energy_mode)
 !
 !...acoustic branches
 !
 call ELPH_acoustic_phonon_properties(q)
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
 if (.not.l_PH_uses_E_FineGd) then
   call bz_samp_FineGd_alloc(k%FGbz,k%nbz)
   call bz_samp_FineGd_default(k%FGbz,k,k%nbz)
 endif
 k_FG = k%FGbz
 nk_CG= k%nbz
#endif
 !
 ! ... select fine grid subset
 call PHEL_double_grid_subset(k,k_FG,nk_CG)
 !
 ! ... allocs
 Life_MEM_steps=1
 nQ_W=PH_Q(2)
 if (trim(QP_solver)=="n") QP_Sc_steps=1
 if (trim(QP_solver)=="g") nQ_W=PH_Q(2)*QP_Sc_steps
 call RT_lifetime_alloc("ph-el",RT_PE_life,elph_branches,nQ_W)
 call PHEL_alloc( )
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
 ! Select allowed transitions for phonon scatterings
 !===================================================
 call PH_Grid_Check_Transitions(k,q,QP_PH_n_G_bands,k_FG,E_FG)
 !
 ! Spectral Function energy points
 !=================================
 if (trim(QP_solver)=="g") then
   call W_reset(PH_SF_W)
   PH_SF_W%n_freqs=QP_Sc_steps 
   PH_SF_W%er=QP_G_er
   PH_SF_W%dr=QP_G_damp
   call FREQUENCIES_setup(PH_SF_W)
 endif
 !
 ! Calculation of the lifetimes
 !  - Equilibrium (yambo_ph): linewidths with retarded self-energy S^R
 !  - Out of eq.  (yambo_rt): scatt. rates with lesser/greater S^< and S^>
 !==============================
#if defined _RT 
 l_initial_equilibrium=.TRUE.
 call PHEL_Lifetimes(E,k,q,k_FG,E_FG)
#else
 call PHEL_Lifetimes_equilibrium(E,k,q,k_FG,E_FG)
#endif
 !
 ! Redux
 !=======
#if defined _RT 
 call PP_redux_wait(RT_PE_life%emit)
 call PP_redux_wait(RT_PE_life%abs)
#endif
 if (l_initial_equilibrium) call PP_redux_wait(PHEL_qp%PI)
 if (l_extended_output) then
   call PP_redux_wait(PHEL_qp%interband)
   call PP_redux_wait(PHEL_qp%intraband)
   if (l_initial_equilibrium) call PP_redux_wait(PHEL_qp%approx)
 endif
 !
 call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1,2/),ID=ID)
 i_err=io_PH(ID)
 !
 ! Output
 !========
 call PH_of(q)
 !
 ! Clean
 !=======
 call CLEAN()
 !
 contains
   !
   subroutine CLEAN()
     call RT_ALL_lifetimes_free(  )
     call RT_ALL_occupations_free(  )
     call ELPH_alloc('FREE',GKKP=GKKP_pe_sq)
     call PHEL_free( )
   end subroutine
   !
end subroutine PHEL_driver
