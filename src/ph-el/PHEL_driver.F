!
!        Copyright (C) 2000-2020 the YAMBO team
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
 use R_lattice,           ONLY:bz_samp,nqibz,bz_samp_FineGd_default,bz_samp_FineGd_alloc,bz_fine_grid
 use interfaces,          ONLY:ELPH_alloc
 use ELPH,                ONLY:QP_PH_n_G_bands,elph_branches,GKKP_pe_sq,l_GKKP_DB_exists,l_GKKP_expanded_DB_exists,&
&                              l_GKKP_hosts_bare_dV,elph_nb,elph_nQ
 use PHEL,                ONLY:l_PH_uses_E_FineGd,PH_Self_Energy_mode,PHEL_free
 use parallel_int,        ONLY:PP_redux_wait,PARALLEL_global_indexes
 use RT_lifetimes,        ONLY:RT_PE_life,RT_lifetime_alloc,RT_extra_lifetime_alloc,Life_MEM_steps,&
&                              RT_PE_extra,l_initial_equilibrium,RT_ALL_lifetimes_free                        
 use RT_occupations,      ONLY:RT_EL_occupations_alloc,RT_PH_occupations_alloc,RT_ALL_occupations_free
 use real_time,           ONLY:l_RT_uses_E_FineGd
 use parser_m,            ONLY:parser
 use QP_m,                ONLY:l_extended_output
 use IO_m,                ONLY:io_control,OP_RD_CL,VERIFY,REP,OP_WR_CL
 !
#include<memory.h>
 !
 type(levels) ,intent(in) :: E
 type(bz_samp),intent(in) :: k,q
 !
 ! Work Space
 !
 integer             ::i_err,ID,nk_CG
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
 call RT_lifetime_alloc("ph-el",RT_PE_life,elph_branches,elph_nQ)
 if (l_extended_output) call RT_extra_lifetime_alloc("ph-el",RT_PE_extra,elph_branches,elph_nQ)
 !
 call RT_EL_occupations_alloc((/QP_PH_n_G_bands(1),QP_PH_n_G_bands(2)/),k_FG%N)
 call RT_PH_occupations_alloc(elph_branches,elph_nQ)
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
 ! Calculation of the lifetimes
 !==============================
 l_initial_equilibrium=.TRUE. 
 call PHEL_Lifetimes(E,k,q,k_FG,E_FG)
 !
 ! Redux
 !=======
 call PP_redux_wait(RT_PE_life%emit)
 call PP_redux_wait(RT_PE_life%abs)
 if (l_initial_equilibrium) then
   call PP_redux_wait(RT_PE_life%linewidth_eq)
 endif
 if (l_extended_output) then
   call PP_redux_wait(RT_PE_extra%interband)
   call PP_redux_wait(RT_PE_extra%intraband)
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
