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
 use pars,                ONLY:SP,schlen,rZERO,cZERO,IPL
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
 real(SP)                 ::PHEL_broad_factor=3._SP
 real(SP)    ,allocatable ::PHEL_nTrans_percent(:)
 real(SP)    ,allocatable ::PHEL_Q_Radius(:)
 integer(IPL),allocatable ::PHEL_N_trans_yes(:)
 integer(IPL),allocatable ::PHEL_N_trans_tot(:)
 !
 ! ... Double-grid
 integer              :: PH_DbGd_points=-1
 logical              :: l_PH_uses_E_FineGd=.FALSE.
 !
 ! ... Maps
 integer, allocatable :: KpQ_io_map(:,:)
 !
 ! ... GF
 type(w_samp), allocatable :: PH_SF_W(:,:)
 !
 ! ... Scatterings
 !
 ! table:
 ! 1 -> il [ph mode] 2 -> ikbz/ik_rim [k bz]
 ! 3 -> ip_fg [k-q (i)bz DbGd], 4 -> i_n [bnd], 5 -> i_np [bnd]
 type phel_scatterings
   !
   integer(IPL)          :: N=0        ! Number of accepted transitions
   integer, allocatable  :: table(:,:) ! Indices of accepted transitions: see above
   !
 end type phel_scatterings             
 !
 type(phel_scatterings)  :: phel_info
 !
 type PHEL_qp_kind
   complex(SP), allocatable :: PI(:,:,:)
   real(SP),    allocatable :: interband(:,:)
   real(SP),    allocatable :: intraband(:,:)
   real(SP),    allocatable :: allen(:,:)
 end type PHEL_qp_kind
 !
 type(PHEL_qp_kind)  :: PHEL_qp
 !
 type QPHt
   real(SP)    :: PI_ref = rZERO
   complex(SP) :: PI_oms = cZERO
   complex(SP) :: beta   = cZERO
   complex(SP) :: Z      = cZERO
   complex(SP) :: Pole   = cZERO
   real(SP)    :: Half_Width_left  = rZERO
   real(SP)    :: Half_Width_right = rZERO
 end type QPHt
 !
 type (QPHt) :: QPH
 !
 contains
   !
   function PH_energy(q,il,iq)
     use ELPH,      ONLY:l_GKKP_expanded_DB_exists,l_GKKP_DB_exists,PH_freqs_sq
     use R_lattice, ONLY:bz_samp
     implicit none
     integer       :: iq,il
     real(SP)      :: PH_energy
     type(bz_samp) :: q
     if (l_GKKP_expanded_DB_exists) PH_energy=abs(sqrt(PH_freqs_sq(q%k_table(iq,1),il)))
     if (l_GKKP_DB_exists         ) PH_energy=abs(sqrt(PH_freqs_sq(iq,il)))
   end function
   !
   subroutine PHEL_gimme_Ks(k,iq,ikbz,ik,ik_m_q,l_RT)
     !
     use ELPH,      ONLY:elph_use_q_grid
     use R_lattice, ONLY:bz_samp,qindx_X
     !
     implicit none
     !
     type(bz_samp) :: k
     integer, intent(in)  :: iq,ikbz
     integer, intent(out) :: ik,ik_m_q
     logical       :: l_RT
     !
     if (l_RT) then
       !
       ! If running RT: obtain k-q and k in the IBZ
       ik_m_q =k%sstar(ik_m_q,1)
       ik     =k%sstar(ik,1)
     else
       !
       ! Obtain k-q in the full BZ from k in the bz and q in the ibz
       if (     elph_use_q_grid) ik_m_q = qindx_X(iq,ikbz,1)
       if (.not.elph_use_q_grid) ik_m_q = KpQ_IO_map(iq,ikbz)
       !
       ! Use k in the bz
       ik = ikbz
       !
     endif
     !
   end subroutine
   !
   subroutine PHEL_alloc( )
     YAMBO_ALLOC(PHEL_qp%PI,(elph_branches(1):elph_branches(2),PH_Q(1):PH_Q(2),QP_Sc_steps))
     PHEL_qp%PI=cZERO
     YAMBO_ALLOC(PHEL_qp%interband,(elph_branches(1):elph_branches(2),PH_Q(1):PH_Q(2)))
     YAMBO_ALLOC(PHEL_qp%intraband,(elph_branches(1):elph_branches(2),PH_Q(1):PH_Q(2)))
     YAMBO_ALLOC(PHEL_qp%allen,(elph_branches(1):elph_branches(2),PH_Q(1):PH_Q(2)))
     PHEL_qp%interband=rZERO
     PHEL_qp%intraband=rZERO
     PHEL_qp%allen=rZERO
     YAMBO_ALLOC(PHEL_nTrans_percent,(PH_Q(1):PH_Q(2)))
     YAMBO_ALLOC(PHEL_Q_Radius,(PH_Q(1):PH_Q(2)))
     allocate(PHEL_N_trans_yes(PH_Q(1):PH_Q(2)))
     allocate(PHEL_N_trans_tot(PH_Q(1):PH_Q(2)))
     PHEL_nTrans_percent=rZERO
     PHEL_Q_Radius=rZERO
     PHEL_N_trans_yes=0
     PHEL_N_trans_tot=0
   end subroutine
   !
   subroutine PHEL_free( )
     integer       :: iq,il
     YAMBO_FREE(KpQ_io_map)
     if (allocated(PH_SF_W)) then
       do iq=PH_Q(1),PH_Q(2)
         do il=elph_branches(1),elph_branches(2)
            call W_reset(PH_SF_W(il,iq))
         enddo
       enddo
     endif
     YAMBO_FREE(PHEL_qp%PI)
     YAMBO_FREE(PHEL_qp%interband)
     YAMBO_FREE(PHEL_qp%intraband)
     YAMBO_FREE(PHEL_qp%allen)
     YAMBO_FREE(PHEL_nTrans_percent)
     YAMBO_FREE(PHEL_Q_Radius)
     deallocate(PHEL_N_trans_yes)
     deallocate(PHEL_N_trans_tot)
   end subroutine
   !
end module
