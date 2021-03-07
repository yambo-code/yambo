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
subroutine PARALLEL_WF_distribute(B_index,Bp_index,K_index,B_and_K_index,&
&                                 QP_index,PLASMA_index,Bm_index,CLEAN_UP)
 !
 use parallel_m,      ONLY:PP_indexes,ncpu
 use wave_func,       ONLY:states_to_load
 use electrons,       ONLY:n_spin,n_bands
 use R_lattice,       ONLY:nkibz
 use QP_m,            ONLY:QP_n_states,QP_table
#if defined _SC || defined _RT
 use hamiltonian,     ONLY:B_mat_index,H_ref_bands
#endif
#if defined _RT
 use parallel_m,      ONLY:PAR_IND_Q_bz,PAR_Q_bz_index
 use plasma,          ONLY:EH_gas
 use R_lattice,       ONLY:nkbz
 use collision_ext,   ONLY:GW_NEQ_collisions
#endif
#include<memory.h>
 !
 type(PP_indexes), optional    :: K_index
 type(PP_indexes), optional    :: B_index
 type(PP_indexes), optional    :: B_and_K_index
 type(PP_indexes), optional    :: Bp_index
 type(PP_indexes), optional    :: QP_index
 type(PP_indexes), optional    :: PLASMA_index
 type(PP_indexes), optional    :: Bm_index
 logical, intent(in), optional :: CLEAN_UP
 !
 ! Work Space
 !
#if defined _RT
 integer :: i_J,i_J_bg,iqbz,i_q_mem,i_coll
#endif
 integer :: i_k,i_b,i_bp,i_qp,NB,NK,NBp
 logical :: condition
#if defined _SC || defined _RT
 logical :: Vl(H_ref_bands(2))
#endif
 !
 NB =n_bands
 NBp=n_bands
 NK=nkibz 
 if (present(K_index)) then 
   NK=size(K_index%element_1D)
 endif
 if (present(B_index)) then 
   NB=size(B_index%element_1D)
 endif
 if (present(Bp_index)) then 
   NBp=size(Bp_index%element_1D)
 endif
 if (present(B_and_K_index)) then 
   NB=size(B_and_K_index%element_2D,1)
   NK=size(B_and_K_index%element_2D,2)
 endif
 !
 if (present(CLEAN_UP)) then
   !
   YAMBO_FREE(states_to_load)
   YAMBO_ALLOC(states_to_load,(NB,NK,n_spin))
   !
   states_to_load=CLEAN_UP
   if (ncpu==1) states_to_load=.TRUE.
   !
 endif
 !
 if (ncpu==1) return
 !
 if (present(K_index).and..not.present(B_index)) then
   do i_k=1,NK
     if (.not.K_index%element_1D(i_k)) states_to_load(:,i_k,:)=.FALSE.
   enddo
 else if (.not.present(K_index).and.present(B_index)) then
   do i_b=1,NB
     if (.not.B_index%element_1D(i_b)) states_to_load(i_b,:,:)=.FALSE.
   enddo
 else if (present(K_index).and.present(B_index)) then
   do i_k=1,NK
     !
     if (.not.K_index%element_1D(i_k)) states_to_load(:,i_k,:)=.FALSE.
     !
     do i_b=1,NB
       !
       condition=K_index%element_1D(i_k).and..not.B_index%element_1D(i_b)
       !
       if (present(Bp_index)) then
         if (i_b<=NBp) condition=condition.and..not.Bp_index%element_1D(i_b)
       endif
       !
       if (condition) states_to_load(i_b,i_k,:)=.FALSE.
       !
     enddo
     !
   enddo
 endif
 !
 if (present(B_and_K_index)) then
   do i_k=1,NK
     do i_b=1,NB
       states_to_load(i_b,i_k,:)=B_and_K_index%element_2D(i_b,i_k)
     enddo
   enddo
 endif
 !
#if defined _SC || defined _RT
 if (present(K_index).and.present(Bm_index)) then
   do i_k=1,NK
     !
     if (K_index%element_1D(i_k)) then
       !
       Vl=.FALSE. 
       !
       do i_b=H_ref_bands(1),H_ref_bands(2)
         do i_bp=H_ref_bands(1),H_ref_bands(2)
           if (Bm_index%element_1D( B_mat_index(i_b,i_bp,H_ref_bands) ) ) then
             Vl(i_b )=.TRUE.
             Vl(i_bp)=.TRUE.
           endif
         enddo
       enddo
       do i_b=H_ref_bands(1),H_ref_bands(2)
         if (.not.Vl(i_b)) states_to_load(i_b,i_k,:)=.FALSE.
       enddo
     else
       states_to_load(:,i_k,:)=.FALSE.
     endif
     !
   enddo
   !
 endif
#endif
 !
 if (present(QP_index)) then
   !
   do i_qp=1,QP_n_states
     !
     if (.not.QP_index%element_1D(i_qp)) cycle
     !
     i_b =QP_table(i_qp,1)
     i_bp=QP_table(i_qp,2)
     i_k =QP_table(i_qp,3)
     !
     states_to_load(i_b ,i_k,:)=.TRUE.
     states_to_load(i_bp,i_k,:)=.TRUE.
     !
   enddo
   !
 endif
 !
#if defined _RT
 !
 if (present(PLASMA_index)) then
   ! 
   do iqbz=1,nkbz
     !
     if (.not.PAR_IND_Q_bz%element_1D(iqbz)) cycle
     i_q_mem=PAR_Q_bz_index(iqbz)
     !
     do i_J=1,EH_gas%N_poles(iqbz)
       !
       if (.not.PLASMA_index%element_1D(i_J)) cycle
       !
       do i_J_bg=EH_gas%grid(i_J,i_q_mem,1),EH_gas%grid(i_J,i_q_mem,2)
         !
         i_coll=EH_gas%tab(i_J_bg,i_q_mem,1)
         !
         i_b  = GW_NEQ_collisions%state(i_coll,1)
         i_bp = GW_NEQ_collisions%state(i_coll,2)
         i_k  = GW_NEQ_collisions%state(i_coll,3)
         !
         states_to_load(i_b ,i_k,:)=.TRUE.
         states_to_load(i_bp,i_k,:)=.TRUE.
         !
       enddo
       !
     enddo
     !
   enddo
   !
 endif
 !
#endif
 !
end subroutine
