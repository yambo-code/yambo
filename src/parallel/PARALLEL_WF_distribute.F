!
! Copyright (C) 2000-2013 A. Marini and the YAMBO team 
!              http://www.yambo-code.org
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
subroutine PARALLEL_WF_distribute(NB,NK,B_index,Bp_index,K_index,QP_index,Bm_index,CLEAN_UP)
 !
 use parallel_m,      ONLY:PP_indexes,ncpu
 use wave_func,       ONLY:states_to_load
 use electrons,       ONLY:n_sp_pol
 use QP_m,            ONLY:QP_n_states,QP_table
#if defined _SC
 use SC,              ONLY:SC_bands
 use real_time,       ONLY:B_mat_index
#endif
 !
 implicit none
 !
 integer, intent(in)           :: NB,NK
 type(PP_indexes), optional    :: K_index
 type(PP_indexes), optional    :: B_index
 type(PP_indexes), optional    :: Bp_index
 type(PP_indexes), optional    :: QP_index
 type(PP_indexes), optional    :: Bm_index
 logical, intent(in), optional :: CLEAN_UP
 !
 ! Work Space
 !
 integer :: i_k,i_b,i_bp,i_qp
 logical :: condition
 !
 if (present(CLEAN_UP)) then
   !
   if (allocated(states_to_load)) deallocate(states_to_load)
   allocate(states_to_load(NB,NK,n_sp_pol))
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
         if (i_b<=size(Bp_index%element_1D)) condition=condition.and..not.Bp_index%element_1D(i_b)
       endif
       !
       if (condition) states_to_load(i_b,i_k,:)=.FALSE.
       !
     enddo
     !
   enddo
 endif
 !
#if defined _SC
 if (present(K_index).and.present(Bm_index)) then
   do i_k=1,NK
     !
     if (.not.K_index%element_1D(i_k)) states_to_load(:,i_k,:)=.FALSE.
     !
     do i_b=SC_bands(1),SC_bands(2)
       do i_bp=SC_bands(1),SC_bands(2)
         if (K_index%element_1D(i_k).and..not.Bm_index%element_1D( B_mat_index(i_b,i_bp) ) ) then
           states_to_load(i_b ,i_k,:)=.FALSE.
           states_to_load(i_bp,i_k,:)=.FALSE.
         endif
       enddo
     enddo
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
end subroutine
