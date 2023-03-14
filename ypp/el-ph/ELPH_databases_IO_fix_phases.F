!
!        Copyright (C) 2000-2022 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS
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
subroutine ELPH_databases_IO_fix_phases(IDB,kpts)
 !
 use pars,                ONLY:SP,DP,cZERO
 use R_lattice,           ONLY:bz_samp,qindx_X
 use wave_func,           ONLY:WF,wf_ng
 use interfaces,          ONLY:WF_load,WF_free
 use parallel_m,          ONLY:PAR_IND_WF_k,PP_indexes_reset
 use parallel_int,        ONLY:PARALLEL_WF_distribute
 use ELPH,                ONLY:elph_use_q_grid
 use YPP_ELPH,            ONLY:DB_K_map,DB_nk,DB_gkkp,DB_Ovlp,DB_nb,DB_ph_modes,DB_Q_map
 use collision_el,        ONLY:elemental_collision,elemental_collision_free,elemental_collision_alloc
 !
 implicit none
 !
 integer       :: IDB
 type(bz_samp) :: kpts
 !
 ! Work Space
 !
 type(elemental_collision):: YPP_scatt
 !
 complex(SP) :: Y_overlap(DB_nb,DB_nb),PHASES_MATRIX(DB_nb,DB_nb),det_tmp
 integer     :: ik_DB,iq,ik,ok,ik_bz,ok_bz,isymm,osymm,ib1,ib2,i_sp_pol,i_l,iGo
 !
 iq=IDB
 if (elph_use_q_grid) iq=DB_Q_map(IDB)
 !
 i_sp_pol=1
 !
 Y_overlap=cZERO
 !
 call PP_indexes_reset(PAR_IND_WF_k)
 allocate(PAR_IND_WF_k%element_1D(kpts%nibz))
 !
 ! GKKP & Levels
 !===============
 do ik_db=1,DB_nk
   !
   ik_bz=DB_K_map(ik_db)
   !
   ok_bz=qindx_X(iq,ik_bz,1)
   iGo  =qindx_X(iq,ik_bz,2)
   !
   ik   =kpts%sstar(ik_bz,1) 
   isymm=kpts%sstar(ik_bz,2) 
   !
   ok   =kpts%sstar(ok_bz,1)
   osymm=kpts%sstar(ok_bz,2)
   !
   PAR_IND_WF_k%element_1d(:)=.false.
   PAR_IND_WF_k%element_1d(ik)=.true.
   PAR_IND_WF_k%element_1d(ok)=.true.
   call PARALLEL_WF_distribute(K_index=PAR_IND_WF_k,CLEAN_UP=.FALSE.)
   call WF_load(WF,wf_ng,iGo,(/1,DB_nb/),(/1,DB_nk/),(/i_sp_pol,i_sp_pol/),space='G',title='Fix GKKP phases')
   !
   call elemental_collision_alloc(YPP_scatt,NG=1,TITLE="BSE") 
   !
   do ib1=1,DB_nb
     do ib2=1,DB_nb
       !
       YPP_scatt%is=(/ib1,ik,isymm,i_sp_pol/)
       YPP_scatt%os=(/ib2,ok,osymm,i_sp_pol/)
       YPP_scatt%qs=(/iGo,iq,1/)
       !
       call scatter_Bamp(YPP_scatt)
       !
       Y_overlap(ib1,ib2)=-conjg(YPP_scatt%rhotw(1))
       !
     enddo
   enddo
   ! 
   call WF_free(WF)
   call elemental_collision_free(YPP_scatt)
   !
   call SERIAL_inversion(DB_nb,Y_overlap,det_tmp,.false.)
   PHASES_MATRIX=matmul(Y_overlap,DB_Ovlp(:,:,ik_db))
   !
   do i_l=1,DB_ph_modes
     DB_gkkp(:,:,i_l,ik_db)=matmul(PHASES_MATRIX,DB_gkkp(:,:,i_l,ik_db))
   enddo
   !
 enddo
 !
end subroutine
