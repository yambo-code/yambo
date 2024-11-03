!
!        Copyright (C) 2020-2023 the Yambo team
!
! Authors (see AUTHORS file for details): CA
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
subroutine OSCLL_compose_collision(k,q,i_n,i_m,i_k,COLL) 
 !
 ! This subroutine use long-range part of the Screened exchange
 ! and the oscillators to reconstruct the collisions
 ! (at present used only for testing purpose) 
 !
 use pars,           ONLY:SP,cZERO
 use nl_optics,      ONLY:NL_bands,Correlation,LSEX,LHF
 use collision_ext,  ONLY:OSCLL,W,ng_oscll
 use QP_m,           ONLY:QP_nk
 use R_lattice,      ONLY:bz_samp,qindx_S
 use parallel_m,     ONLY:PAR_Xk_ibz_index
 use wrapper,        ONLY:V_dot_V,M_by_V
#if defined _TIMING
 use timing_m,       ONLY:timing
#endif
 ! 
 implicit none
 !
 type(bz_samp), intent(in)  :: k,q
 !
 ! Collision indexes
 integer,       intent(in)  :: i_n,i_m,i_k
 !
 ! Work Space
 !
 integer :: ibp  ! self-energy external indexes
 integer :: ib,iqbz,i_kmq ! self-energy internal indexes
 integer :: ig1,ig2,ik_mem
 complex(SP) :: V_tmp(ng_oscll)
 !
 ! Collisions output
 !
 complex(SP), intent(out) :: COLL(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2),QP_nk)
 !
#if defined _TIMING
 call timing('OSCLL compose',OPR='start')
#endif
 !
 ik_mem=PAR_Xk_ibz_index(i_k)
 !
 ! Build Collisions from the Oscillators
 !
 COLL=cZERO
 ! 
 do iqbz=1,q%nbz
   i_kmq   = k%sstar(qindx_S(i_k,iqbz,1),1)
   !
   ! Build Collision on the fly
   !
   if(Correlation==LSEX) then
     !      
     do ig1=1,ng_oscll
       do ig2=1,ng_oscll
!$OMP WORKSHARE	 
	 forall(ibp=NL_bands(1):NL_bands(2),ib=NL_bands(1):NL_bands(2)) 
           COLL(ib,ibp,i_kmq)=COLL(ib,ibp,i_kmq)+OSCLL(i_n,ib ,ik_mem,iqbz,ig2)*conjg(OSCLL(i_m,ibp,ik_mem,iqbz,ig1)) &
& 			      *W(iqbz,ig2,ig1) 
         end forall
!$OMP END WORKSHARE 
       enddo
     enddo
     !
   elseif(Correlation==LHF) then
    do ig1=1,ng_oscll
!$OMP WORKSHARE	 
      forall(ibp=NL_bands(1):NL_bands(2),ib=NL_bands(1):NL_bands(2)) 
        COLL(ib,ibp,i_kmq)=COLL(ib,ibp,i_kmq)+OSCLL(i_n,ib ,ik_mem,iqbz,ig1)*conjg(OSCLL(i_m,ibp,ik_mem,iqbz,ig1)) &
& 			      *W(iqbz,ig1,ig1) 
      end forall
!$OMP END WORKSHARE 
    enddo
   endif
   !
 enddo  ! loop in iqbz
 !
#if defined _TIMING
 call timing('OSCLL compose',OPR='stop')
#endif
 !
end subroutine OSCLL_compose_collision 
