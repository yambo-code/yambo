!
!        Copyright (C) 2000-2021 the YAMBO team
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
module EXCPH
 !
 use pars,           ONLY:SP,schlen,lchlen,cZERO,rZERO
 use BS_solvers,     ONLY:BSS_n_eig
 use BS,             ONLY:BS_H_dim
 use ELPH,           ONLY:ph_modes
 !
#include<memory.h>
 !
 ! Exciton-Phonon 
 !================
 !
 ! ... GKKP & EXC-PH Interaction
 !
 complex(SP), allocatable :: EXCPH_Gkkp(:,:,:)
 real(SP),    allocatable :: EXCPH_Gkkp_sq(:,:,:)
 integer                  :: EXCPH_states(2)
 integer                  :: EXCPH_sum(2)
 character(schlen)        :: EXCPH_kind
 !
 ! L_in and L_out paths
 !
 character(lchlen)       :: Lin_path 
 character(lchlen)       :: Lout_path 
 !
 ! L types
 !
 character(schlen)       :: L_kind_in
 character(schlen)       :: L_kind_out 
 !
 ! ExcPH gkkp
 ! 
 complex(SP), allocatable :: A_rot(:,:),Xi(:,:,:)
 integer,     allocatable :: BSS_eh_table_m1(:,:,:),BSS_eh_table_m1_in(:,:,:),k_plus_q_table(:,:)
 complex(SP), allocatable :: BS_E_in(:),BS_E(:)
 real(SP),    allocatable :: BS_all_E(:,:)
 complex(SP), allocatable :: BS_mat_in(:,:)
 integer,     allocatable :: BSS_eh_table_in(:,:)
 complex(SP), pointer :: BS_R_left(:),BS_R_right(:)
 real(SP),    allocatable :: BS_R(:)
 !
 ! ... Sigma
 !
 complex(SP), allocatable :: BS_Sat_E(:,:,:)
 complex(SP), allocatable :: BS_Sat_E_PH_abs(:,:,:)
 real(SP),    target, allocatable :: BS_Sat_WEIGHT(:,:,:)
 real(SP),    target, allocatable :: BS_Sat_WEIGHT_PH_abs(:,:,:)
 real(SP),    allocatable :: EXCPH_Renorm(:)
 real(SP),    allocatable :: EXCPH_Renorm_PH_abs(:)
 real(SP),    allocatable :: EXCPH_Renorm_PL(:)
 real(SP),    allocatable :: EXCPH_Renorm_PL_PH_abs(:) 
 complex(SP), allocatable :: BS_EXCPH_E(:)
 real(SP),    allocatable :: ph_EXCPH_E(:)
 complex(SP), allocatable :: EXCPH_Sigma_c_diag(:,:)
 complex(SP), allocatable :: EXCPH_Sigma_c(:,:,:)
 real(SP) :: L_damping
 !
 ! Minimum excitonic energy
 !
 real(SP) :: min_EXC_E
 !
 ! Double-grid
 !
 integer  :: ID_EXC,ID_EXC_obj
 real(SP) :: min_EXC_E_DbGd
 real(SP), allocatable :: EXC_E_DbGd(:,:)
 real(SP), target, allocatable :: BS_Sat_W_DG(:,:,:,:)
 real(SP), target, allocatable :: BS_Sat_W_DG_PH_abs(:,:,:,:)
 !
 ! Life Times
 !
 real(SP), allocatable :: EXC_Lifetime(:)
 !
 ! DB Q-mapping in case symmetries are removed
 !
 integer, allocatable :: DB_Q_map(:)
 !
 contains
   !      
   subroutine Sat_alloc()
     implicit none
     YAMBO_ALLOC(BS_Sat_E,(ph_modes,EXCPH_sum(1):EXCPH_sum(2),EXCPH_states(1):EXCPH_states(2)))
     YAMBO_ALLOC(BS_Sat_E_PH_abs,(ph_modes,EXCPH_sum(1):EXCPH_sum(2),EXCPH_states(1):EXCPH_states(2)))
     YAMBO_ALLOC(BS_Sat_WEIGHT,(ph_modes,EXCPH_sum(1):EXCPH_sum(2),EXCPH_states(1):EXCPH_states(2)))
     YAMBO_ALLOC(BS_Sat_WEIGHT_PH_abs,(ph_modes,EXCPH_sum(1):EXCPH_sum(2),EXCPH_states(1):EXCPH_states(2)))
     !
     BS_Sat_E            =rZERO
     BS_Sat_E_PH_abs     =rZERO
     BS_Sat_WEIGHT       =rZERO
     BS_Sat_WEIGHT_PH_abs=rZERO
     !
   end subroutine Sat_alloc
   !
   subroutine Sat_free()
     implicit none
     YAMBO_FREE(BS_Sat_E)
     YAMBO_FREE(BS_Sat_E_PH_abs)
     YAMBO_FREE(BS_Sat_WEIGHT)
     YAMBO_FREE(BS_Sat_WEIGHT_PH_abs)
   end subroutine Sat_free
   !
   subroutine Renorm_alloc()
     implicit none
     YAMBO_ALLOC(EXCPH_Renorm,(EXCPH_states(2)))
     YAMBO_ALLOC(EXCPH_Renorm_PH_abs,(EXCPH_states(2)))
     YAMBO_ALLOC(EXCPH_Renorm_PL,(EXCPH_states(2)))
     YAMBO_ALLOC(EXCPH_Renorm_PL_PH_abs,(EXCPH_states(2)))
     !
     EXCPH_Renorm       =rZERO
     EXCPH_Renorm_PH_abs=rZERO
     EXCPH_Renorm_PL       =rZERO
     EXCPH_Renorm_PL_PH_abs=rZERO
     !
   end subroutine Renorm_alloc
   !
   subroutine Renorm_free()
     implicit none
     YAMBO_FREE(EXCPH_Renorm)
     YAMBO_FREE(EXCPH_Renorm_PH_abs)
     YAMBO_FREE(EXCPH_Renorm_PL)
     YAMBO_FREE(EXCPH_Renorm_PL_PH_abs)
   end subroutine Renorm_free
   !
   subroutine EXCPH_gkkp_alloc()
     implicit none
     YAMBO_ALLOC(EXCPH_Gkkp,(ph_modes,EXCPH_sum(1):EXCPH_sum(2),EXCPH_states(1):EXCPH_states(2)))
     YAMBO_ALLOC(EXCPH_Gkkp_sq,(ph_modes,EXCPH_sum(1):EXCPH_sum(2),EXCPH_states(1):EXCPH_states(2)))
     YAMBO_ALLOC(Xi,(BS_H_dim,EXCPH_sum(1):EXCPH_sum(2),2))    
     EXCPH_gkkp          =cZERO
     EXCPH_gkkp_sq       =rZERO
   end subroutine EXCPH_gkkp_alloc   
   !
   subroutine EXCPH_gkkp_free
     implicit none
     YAMBO_FREE(EXCPH_Gkkp)
     YAMBO_FREE(EXCPH_Gkkp_sq)
     YAMBO_FREE(Xi)    
   end subroutine EXCPH_gkkp_free
   !
   subroutine free_EXC()
     implicit none
     !
     YAMBO_FREE(BS_E)
     YAMBO_FREE(BS_all_E)
     YAMBO_FREE_P(BS_R_left)
     YAMBO_FREE_P(BS_R_right)
     YAMBO_FREE(BS_R)
     !
   end subroutine free_EXC
   !
   subroutine alloc_EXC(q)
     use R_lattice,      ONLY:bz_samp
     implicit none
     type(bz_samp) ::q
     YAMBO_ALLOC(BS_E,(BSS_n_eig))
     YAMBO_ALLOC_P(BS_R_left,(BSS_n_eig))
     YAMBO_ALLOC_P(BS_R_right,(BSS_n_eig))
     YAMBO_ALLOC(BS_R,(BSS_n_eig))
     YAMBO_ALLOC(BS_E_in,(BSS_n_eig))
     YAMBO_ALLOC(BS_all_E,(BSS_n_eig,q%nibz))
   end subroutine alloc_EXC
   !
   subroutine build_Q_map(q)
    use R_lattice,      ONLY:bz_samp
    use zeros,          ONLY:k_iku_zero
    use vec_operate,    ONLY:v_is_zero
    use ELPH,           ONLY:PH_qpt
    implicit none
    type(bz_samp) ::q
    !
    integer :: iq,idb
    logical :: re_ordered
    !
    YAMBO_ALLOC(DB_Q_map,(q%nbz))
    !
    ! In this case nibz=nbz
    !
    DB_Q_map=0
    do iq=1,q%nbz
      do idb=1,q%nbz
        if(.not.v_is_zero(q%ptbz(iq,:)-PH_qpt(idb,:),zero_=k_iku_zero)) cycle
        DB_Q_map(iq)=idb
        exit
      enddo
    enddo
    if(any(DB_Q_map==0)) call error("Q-point not found in gkkp_expanded DBs")
    !
    re_ordered=.FALSE.
    do iq=1,q%nbz
      if(DB_Q_map(iq)/=iq) then
        call warning('Q-points in Yambo and gkkp_expanded DBs have a different order !')
        exit
      endif
    enddo
    ! 
   end subroutine build_Q_map
   !
end module EXCPH
