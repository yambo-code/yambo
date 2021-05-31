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
module PHEL
 !
 use frequency,           ONLY:w_samp,W_reset
 use pars,                ONLY:SP,schlen,rZERO,cZERO
 use QP_m,                ONLY:QP_Sc_steps
 use ELPH,                ONLY:elph_branches
 !
#include<memory.h>
 !
 ! ... Kind
 character(schlen):: PH_Self_Energy_mode="dressed-dressed" ! "bare-dressed" (="dressed-bare") / "bare-bare"
 !
 ! ... # Q's
 integer              :: PH_Q(2)=0
 !
 ! ... Transitions
 real(SP)             :: PHEL_broad_factor=3
 !
 ! ... Double-grid
 integer              :: PH_DbGd_points=-1
 logical              :: l_PH_uses_E_FineGd=.FALSE.
 !
 ! ... Maps
 integer, allocatable :: KpQ_io_map(:,:)
 !
 ! ... GF
 type(w_samp)         :: PH_SF_W
 !
 ! ... Scatterings
 !
 ! table:
 ! 1 -> il [ph mode], 2 -> iq [ph q ibz], 3 -> ikbz [k bz], 4 -> ik_m_q [k-q (i)bz],
 ! 5 -> ik_fg [k (i)bz DbGd], 6 -> ip_fg [k-q (i)bz DbGd], 7 -> i_n [bnd], 8 -> i_np [bnd]
 type phel_scatterings
   integer               :: N=0        ! Number of accepted transitions
   integer, allocatable  :: table(:,:) ! Indices of accepted transitions: see above
 end type phel_scatterings             
 !
 type(phel_scatterings)  :: phel_info
 !
 type PHEL_qp_kind
   complex(SP), allocatable :: PI(:,:,:)
   real(SP),    allocatable :: interband(:,:)
   real(SP),    allocatable :: intraband(:,:)
   real(SP),    allocatable :: approx(:,:)
 end type PHEL_qp_kind
 !
 type(PHEL_qp_kind)  :: PHEL_qp
 !
 contains
   !
   subroutine PHEL_alloc( )
     YAMBO_ALLOC(PHEL_qp%PI,(elph_branches(1):elph_branches(2),PH_Q(1):PH_Q(2),QP_Sc_steps))
     PHEL_qp%PI=cZERO
     YAMBO_ALLOC(PHEL_qp%interband,(elph_branches(1):elph_branches(2),PH_Q(1):PH_Q(2)))
     YAMBO_ALLOC(PHEL_qp%intraband,(elph_branches(1):elph_branches(2),PH_Q(1):PH_Q(2)))
     YAMBO_ALLOC(PHEL_qp%approx,(elph_branches(1):elph_branches(2),PH_Q(1):PH_Q(2)))
     PHEL_qp%interband=rZERO
     PHEL_qp%intraband=rZERO
     PHEL_qp%approx=rZERO
   end subroutine
   !
   subroutine PHEL_free( )
     YAMBO_FREE(phel_info%table)
     YAMBO_FREE(KpQ_io_map)
     call W_reset(PH_SF_W)
     YAMBO_FREE(PHEL_qp%PI)
     YAMBO_FREE(PHEL_qp%interband)
     YAMBO_FREE(PHEL_qp%intraband)
     YAMBO_FREE(PHEL_qp%approx)
   end subroutine
   !
end module
