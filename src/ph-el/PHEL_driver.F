!
!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine PHEL_driver(E,k,q)
 !
 use electrons,           ONLY:levels,n_sp_pol,E_fine_grid
 use R_lattice,           ONLY:bz_samp,nqibz,bz_samp_FineGd_default,bz_samp_FineGd_alloc
 use ELPH,                ONLY:QP_PH_n_G_bands,elph_branches,elph_global_free,phel_gkkp_expanded_sq
 use PHEL,                ONLY:PH_DbGd_points,l_PH_uses_E_FineGd
 use parallel_m,          ONLY:PARs_PH_Q_ibz,PARs_PH_K_bz 
 use parallel_int,        ONLY:PP_redux_wait,PARALLEL_global_indexes
 use RT_lifetimes,        ONLY:RT_PE_life,RT_lifetime_alloc,Life_MEM_steps,l_initial_equilibrium,RT_ALL_lifetimes_free
 use RT_occupations,      ONLY:RT_EL_occupations_alloc,RT_PH_occupations_alloc,RT_ALL_occupations_free
 use real_time,           ONLY:l_RT_uses_E_FineGd
 use IO_m,                ONLY:io_control,OP_RD_CL,VERIFY,REP,OP_WR_CL
 use LIVE_t,              ONLY:live_timing
 !
#include<memory.h>
 !
 type(levels) ,intent(in) :: E
 type(bz_samp),intent(in) :: k,q
 !
 ! Work Space
 !
 integer             ::i_err,iq,ID
 integer, external   ::ELPH_expanded_databases_check,io_PH
 type(E_fine_grid)   ::E_FG
 !
 call section('*',"Phonon Lifetimes")
 !
 ! Setup
 !=======
 ! ...DbGd
 PH_DbGd_points    =k%FGibz%N
 l_PH_uses_E_FineGd=PH_DbGd_points>0
 !
 ! ... BZ Setup
 call k_build_up_BZ_tables(q)
 !
 ! ... Already calculated?
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
 ! Parallel Setup
 !===============
 call PARALLEL_global_indexes(E,k,q,"Phonon_Self_Energy")
 !
 ! El-Ph databases check
 !=======================
 i_err=ELPH_expanded_databases_check(QP_PH_n_G_bands)
 if (i_err<0) then
   call error("Extended GKKP databases absent or damaged")
 endif
 !
 ! Q/K grid remapping
 !====================
 call ELPH_Q_databases_map(k,q)
 !
 ! El-Ph databases I/O
 !=====================
 call ELPH_expanded_databases_load(E,k,q,(/1,QP_PH_n_G_bands/),"IBZ")
 !
 !...acoustic branches
 !
 call ELPH_acoustic_phonon_properties(q)
 !
 ! Maps into the RT kinds
 !========================
 ! ... grid
 l_RT_uses_E_FineGd=k%FGibz%N>0
 if (.not.l_RT_uses_E_FineGd) then
   call bz_samp_FineGd_alloc(k%FGibz,k%nibz)
   call bz_samp_FineGd_default(k%FGibz,k)
 endif
 !
 ! ... allocs
 Life_MEM_steps=1
 call RT_lifetime_alloc("ph-el",RT_PE_life,elph_branches,nqibz)
 call RT_EL_occupations_alloc((/1,QP_PH_n_G_bands/),k%FGibz%N)
 call RT_PH_occupations_alloc(elph_branches,nqibz)
 if (l_RT_uses_E_FineGd) then
   YAMBO_ALLOC(E_FG%E,(QP_PH_n_G_bands,k%FGibz%N,n_sp_pol))
   YAMBO_ALLOC(E_FG%W,(QP_PH_n_G_bands,k%FGibz%N,n_sp_pol))
 else
   YAMBO_ALLOC(E_FG%E,(QP_PH_n_G_bands,k%nibz,n_sp_pol))
   YAMBO_ALLOC(E_FG%W,(QP_PH_n_G_bands,k%nibz,n_sp_pol))
 endif
 !
 ! ... occupations & levels
 call RT_occupations_and_levels_init((/1,QP_PH_n_G_bands/),E,k,q,k%FGibz,E_FG)
 !
 ! Loop
 !======
 l_initial_equilibrium=.TRUE.
 call live_timing('Phonon Lifetimes',PARs_PH_Q_ibz%N_par*PARs_PH_K_bz%N_par)
 do iq=1,nqibz
   if (.not.PARs_PH_Q_ibz%IND%element_1D(iq)) cycle
   call PHEL_lifetimes(iq,E,k,q,(/1,QP_PH_n_G_bands/),k%FGibz,E_FG)
 enddo
 call live_timing( )
 !
 ! Redux
 !=======
 call PP_redux_wait(RT_PE_life%emit)
 call PP_redux_wait(RT_PE_life%abs)
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
     call elph_global_free( )
     YAMBO_FREE(phel_gkkp_expanded_sq)
   end subroutine
   !
end subroutine PHEL_driver
