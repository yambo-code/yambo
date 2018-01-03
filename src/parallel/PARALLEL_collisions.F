!
!        Copyright (C) 2000-2018 the YAMBO team
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
subroutine PARALLEL_collisions( k, COLL )
 !
 ! SC
 !====
 !
 ! e-e correlation (HF and COHSEX)
 !=================================
 !
 ! n,k
 ! --<--.
 !      | 
 !      |
 !      |
 !     \|/ m,k-q
 !      |
 !      |
 !      | 
 !      .-->-- n',k
 !      
 ! "SC"="q.qp.b" 
 !  
 !   "q"   -> q 
 !   "qp"  -> QP (k,n,n')  
 !   "b"   -> m and PAR_IND_Bp_mat (this second is for the COLLISIONS case)
 !
 ! Local XC
 !==========
 !                        _ \ _
 !                       /  /  \
 !              G_p{n,m}|       | p
 !                       \_ _ _/
 !                          | 
 !                          | W
 !                          |
 !    (n,k)------>----------.---------->----(n',k)
 !
 ! "SC"="q.qp.b" 
 !  
 !   "q"   -> q 
 !   "qp"  -> QP (k,n,n')  
 !   "b"   -> m and PAR_IND_Bp_mat (this second is for the COLLISIONS case)
 !
 ! REAL-TIME
 !===========
 !
 ! e-e scattering
 !================
 !
 !      __/__ p,m'
 !    ./  \
 !    |\__\__ p-q,m
 !    |   /
 !   \|/
 !    | q
 !    |
 !    | __/__ k-q,n'
 !    ./  \
 !     \__\__ k,n
 !        /
 !      
 ! "RT"="k.b.q.qp"
 !  
 !   "k.b" -> (k,n,n')  
 !   "q"   -> q (BZ)
 !   "qp"  -> Plasma (J)
 !
 ! e-e correlation (HF and COHSEX)
 !=================================
 !
 ! n,k
 ! --<--.
 !      | m
 !      |
 !      |
 !     \|/ k-q
 !      |
 !      |
 !      | m'
 !      .-->-- n',k
 !      
 ! "RT"="k.b.q.qp" 
 !  
 !   "k.b" -> (k,n,n')  
 !   "q"   -> q (BZ)
 !   "qp"  -> Bp_mat (m,m')
 !
 ! Local XC
 !==========
 !                        _ \ _
 !                       /  /  \
 !              G_p{n,m}|       | p
 !                       \_ _ _/
 !                          | 
 !                          | W
 !                          |
 !    (n,k)------>----------.---------->----(n',k)
 !
 !   "k.b" -> (k,n,n')  
 !   "q"   -> q_fake (to span the "p= k-q")
 !   "qp"  -> Bp_mat (m,m')
 !
 ! e-photon correlation 
 !======================
 !
 ! m,k-q   n,k
 ! --<--.-<--
 !      | m
 !      |
 !     \|/ q,G,\alpha
 !      |
 !      
 ! "RT"="k.b.q.qp" 
 !  
 !   "k.b" -> (k,n,m)  
 !   "q"   -> q (BZ)
 !   "qp"  -> G
 !  
 !.........................................................................
 !   "k.b" -> QP (k,n,n')  
 !.........................................................................
 !
 use interfaces,    ONLY:PARALLEL_live_message
 use collision_ext, ONLY:COLLISIONS_group,COLL_bands
 use parallel_m,    ONLY:PAR_IND_Xk_ibz,PAR_IND_B_mat,PAR_IND_G_k,PAR_IND_Q,PAR_IND_QP,l_par_RT,l_par_SE,l_par_SC,l_par_NL
 use hamiltonian,   ONLY:B_mat_index
 use R_lattice,     ONLY:bz_samp,qindx_S,nqbz
#include<memory.h>
 !
 type(bz_samp)          :: k
 type(COLLISIONS_group) :: COLL
 !
 ! Work Space
 !
 integer :: i_coll,i_k,ib1,ib2,NC_tot,i_q,i_p,i_p_bz
 !
 if ( COLL%N == 0 ) return
 !
 YAMBO_ALLOC(COLL%PAR_map,(COLL%N))
 YAMBO_ALLOC(COLL%PAR_IND%element_1D,(COLL%N))
 !
 if (.not.allocated(PAR_IND_G_k%element_1D) ) then
   YAMBO_ALLOC(PAR_IND_G_k%element_1D,(k%nibz))
   PAR_IND_G_k%element_1D=.FALSE.
 endif
 COLL%PAR_IND%element_1D=.FALSE.
 COLL%PAR_N=0
 COLL%PAR_map=0
 NC_tot=0
 do i_coll=1,COLL%N
   !
   ib1 = COLL%state(i_coll,1)
   ib2 = COLL%state(i_coll,2)
   i_k = COLL%state(i_coll,3)
   NC_tot=NC_tot+1
   !
   if (l_par_RT.or.l_par_NL) then
     if (.not.PAR_IND_Xk_ibz%element_1D(i_k)) cycle
     if (.not.PAR_IND_B_mat%element_1D( B_mat_index(ib1,ib2,COLL_bands) ) ) cycle
   endif
   !
   ! l_par_SE is for LAMB
   if (l_par_SE.or.l_par_SC) then
     if (.not.PAR_IND_QP%element_1D(i_coll)) cycle
   endif
   !
   COLL%PAR_N=COLL%PAR_N+1
   COLL%PAR_map(i_coll)=COLL%PAR_N
   COLL%PAR_IND%element_1D(i_coll)=.TRUE.
   !
   PAR_IND_G_k%element_1D(i_k)=.TRUE.
   !
   if ( (.not.l_par_SE) .and. (.not.l_PAR_NL) ) then
     !
     do i_q=1,nqbz
       !
       if (.not.PAR_IND_Q%element_1D(i_q)) cycle
       !
       i_p_bz=qindx_S(i_k,i_q,1)
       i_p   =k%sstar(i_p_bz,1)
       !
       PAR_IND_G_k%element_1D(i_p)=.TRUE.
       !
     enddo
     !
   endif
   !
 enddo
 !
 call PARALLEL_live_message(trim(COLL%name)//" Number of Collisions",LOADED=COLL%PAR_N,TOTAL=NC_tot)
 !
end subroutine
