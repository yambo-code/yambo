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
module interfaces
 !
 implicit none
 !
 interface
   !
   subroutine WF_load(WF,iG_max,iGo_max,bands_to_load,kpts_to_load,&
&                     spins_to_load,space,title,impose_free_and_alloc,force_WFo)
     use wave_func,       ONLY:WAVEs
     integer :: iG_max,iGo_max,bands_to_load(2),kpts_to_load(2)
     type(WAVEs)           :: WF
     integer,     optional :: spins_to_load(2)
     character(*),optional :: space
     character(*),optional :: title
     logical     ,optional :: impose_free_and_alloc
     logical     ,optional :: force_WFo
   end subroutine
   !
   subroutine PARALLEL_WF_index(COMM)
     use parallel_m,      ONLY:MPI_comm
     type(MPI_comm), optional :: COMM
   end subroutine
   !
   subroutine PARALLEL_global_indexes(E,Xk,q,ENVIRONMENT,X,RESET,SERIAL)
     use X_m,           ONLY:X_t
     use electrons,     ONLY:levels
     use R_lattice,     ONLY:bz_samp
     implicit none
     type(levels)            ::E
     type(bz_samp)           ::Xk,q
     character(*)            ::ENVIRONMENT
     type(X_t),      optional::X
     logical,        optional::RESET
     logical,        optional::SERIAL
   end subroutine
   !
   subroutine PARALLEL_live_message(WHAT,ENVIRONMENT,LOADED,TOTAL,NCPU,CHAIN)
     character(*)          :: WHAT
     character(*),optional :: ENVIRONMENT
     integer,     optional :: LOADED,TOTAL,NCPU,CHAIN
   end subroutine
   !
   subroutine PARALLEL_WF_distribute(NB,NK,B_index,Bp_index,K_index,QP_index,CLEAN_UP)
     !
     use parallel_m,      ONLY:PP_indexes
     !
     implicit none
     !
     integer, intent(in)           :: NB,NK
     type(PP_indexes),    optional :: K_index
     type(PP_indexes),    optional :: B_index
     type(PP_indexes),    optional :: Bp_index
     type(PP_indexes),    optional :: QP_index
     logical, intent(in), optional :: CLEAN_UP
     !
   end subroutine 
   !
   subroutine OCCUPATIONS_Gaps(E,E_g_dir,E_g_ind,N_f,N_m)
     use pars,          ONLY: SP
     use electrons,     ONLY:levels,n_sp_pol
     type(levels)       :: E
     real(SP), optional :: E_g_dir(n_sp_pol,2) ! min - max
     real(SP), optional :: E_g_ind(n_sp_pol,2) ! min - max
     integer , optional :: N_f(n_sp_pol)
     integer , optional :: N_m(n_sp_pol)
   end subroutine 
   !
   subroutine PARALLEL_assign_chains_and_COMMs(n_elements,ROLE,COMM_index_1,COMM_index_2,COMM_index_3,&
&                                              COMM_index_4,COMM_A2A_1,COMM_A2A_2,COMM_A2A_3)
     use parallel_m,    ONLY:MPI_comm
     integer                 :: n_elements
     character(*)            :: ROLE(:)
     type(MPI_comm)          :: COMM_index_1
     type(MPI_comm),optional :: COMM_index_2
     type(MPI_comm),optional :: COMM_index_3
     type(MPI_comm),optional :: COMM_index_4
     type(MPI_comm),optional :: COMM_A2A_1
     type(MPI_comm),optional :: COMM_A2A_2
     type(MPI_comm),optional :: COMM_A2A_3
   end subroutine
   !
   subroutine PARALLEL_index(px,uplim,low_range,COMM,CONSECUTIVE)
     use parallel_m, ONLY:PP_indexes,MPI_COMM
     type(PP_indexes)       ::px
     integer                ::uplim(:)
     integer, optional      ::low_range(:)
     type(MPI_COMM),optional::COMM
     logical,       optional::CONSECUTIVE
   end subroutine
   !
 end interface
 !
end module
