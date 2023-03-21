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
 use R_lattice,           ONLY:bz_samp,qindx_X,nkibz
 use wave_func,           ONLY:WF
 use electrons,           ONLY:n_bands,n_sp_pol
 use interfaces,          ONLY:WF_load,WF_free
 use matrix_operate,      ONLY:hermitian
 use parallel_m,          ONLY:PAR_IND_WF_b_and_k,PP_indexes_reset
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
 complex(SP) :: PHASES_MATRIX(DB_nb,DB_nb),M_tmp(DB_nb,DB_nb)
 complex(SP) :: Y_overlap(DB_nb,DB_nb),det_tmp
 integer     :: ik_DB,iq,ik,ikpq,ik_bz,ikpq_bz,isymm1,isymm2,ib1,ib2,i_sp_pol,i_l,iGo
 !
 iq=IDB
 if (elph_use_q_grid) iq=DB_Q_map(IDB)
 !
 if (n_sp_pol==2) call error(' gkkp phase fixing not coded for n_sp_pol==2')
 !
 i_sp_pol=1
 n_bands=DB_nb
 nkibz=kpts%nibz
 !
 Y_overlap=cZERO
 !
 call PP_indexes_reset(PAR_IND_WF_b_and_k)
 allocate(PAR_IND_WF_b_and_k%element_2D(DB_nb,nkibz))
 !
 ! GKKP & Levels
 !===============
 do ik_db=1,DB_nk
   !
   ik_bz=DB_K_map(ik_db)
   !
   ikpq_bz=qindx_X(iq,ik_bz,1)
   iGo    =qindx_X(iq,ik_bz,2)
   !
   ik    =kpts%sstar(ik_bz,1) 
   isymm1=kpts%sstar(ik_bz,2) 
   !
   ikpq  =kpts%sstar(ikpq_bz,1)
   isymm2=kpts%sstar(ikpq_bz,2)
   !
   PAR_IND_WF_b_and_k%element_2d(:,:)=.false.
   PAR_IND_WF_b_and_k%element_2d(:,ik)=.true.
   PAR_IND_WF_b_and_k%element_2d(:,ikpq)=.true.
   !
   call PARALLEL_WF_distribute(B_and_K_index=PAR_IND_WF_b_and_k,CLEAN_UP=.TRUE.)
   call WF_load(WF,1,iGo,(/1,DB_nb/),(/1,nkibz/),(/i_sp_pol,i_sp_pol/),title='Fix GKKP phases',quiet=.true.)
   !
   call elemental_collision_alloc(YPP_scatt,NG=1,TITLE="BSE") 
   !
   do ib1=1,DB_nb
     do ib2=1,DB_nb
       !
       YPP_scatt%is=(/ib2,ikpq,isymm2,i_sp_pol/)
       YPP_scatt%os=(/ib1,ik,  isymm1,i_sp_pol/)
       YPP_scatt%qs=(/iGo,iq,1/)
       !
       ! rhotw(1)= < b2 k+q | b1 k>
       call scatter_Bamp(YPP_scatt)
       !
       ! Y_overlap= < b2 k+q | b1 k>
       Y_overlap(ib2,ib1)=YPP_scatt%rhotw(1)
       !
     enddo
   enddo
   ! 
   call WF_free(WF)
   call elemental_collision_free(YPP_scatt)
   !
   ! Eq. 6 notes Fulvio
   ! PHASES_MATRIX= (QE_overlap)^-1 * Y_overlap
   M_tmp=cmplx(DB_Ovlp(:,:,ik_db),kind=SP)
   call SERIAL_inversion(DB_nb,M_tmp,det_tmp,.false.)
   PHASES_MATRIX=matmul(M_tmp,Y_overlap)
   !
   ! Eq. 8 notes Fulvio
   ! U = (PHASES_MATRIX^dag)^-1
   PHASES_MATRIX=hermitian(PHASES_MATRIX)
   call SERIAL_inversion(DB_nb,PHASES_MATRIX,det_tmp,.false.)
   do i_l=1,DB_ph_modes
     ! Gkkp^y = U Gkkp^QE
     DB_gkkp(:,:,i_l,ik_db)=matmul(cmplx(PHASES_MATRIX,kind=DP),DB_gkkp(:,:,i_l,ik_db))
   enddo
   !
 enddo
 !
end subroutine
