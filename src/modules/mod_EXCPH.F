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
 use pars,           ONLY:SP,schlen,lchlen
 !
 ! Exciton-Phonon 
 !================
 !
 ! ... GKKP & EXC-PH Interaction
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
 complex(SP),allocatable  :: A_rot(:,:),Xi(:,:,:)
 integer,    allocatable  :: BSS_eh_table_m1(:,:,:),BSS_eh_table_m1_in(:,:,:),k_plus_q_table(:,:)
 complex(SP), allocatable :: BS_E_in(:),BS_E(:)
 complex(SP), allocatable :: BS_mat_in(:,:)
 integer,     allocatable :: BSS_eh_table_in(:,:)
 !
 ! ... Sigma
 !
 complex(SP), allocatable :: BS_Sat_E(:,:,:)
 complex(SP), allocatable :: BS_Sat_E_PH_abs(:,:,:)
 real(SP),    allocatable :: BS_Sat_WEIGHT(:,:,:)
 real(SP),    allocatable :: BS_Sat_WEIGHT_PH_abs(:,:,:)
 real(SP),    allocatable :: EXCPH_Renorm(:)
 real(SP),    allocatable :: EXCPH_Renorm_PH_abs(:)
 complex(SP), allocatable :: BS_EXCPH_E(:)
 real(SP),    allocatable :: ph_EXCPH_E(:)
 complex(SP), allocatable :: EXCPH_Sigma_c_diag(:,:)
 complex(SP), allocatable :: EXCPH_Sigma_c(:,:,:)
 !
 ! Luminescence
 !
 real(SP) :: Exc_temp
 !
end module EXCPH
