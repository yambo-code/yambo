!
!        Copyright (C) 2000-2016 the YAMBO team
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
 use collision,     ONLY:COLLISIONS_group,COLL_bands
 use parallel_m,    ONLY:PAR_IND_Xk_ibz,PAR_IND_B_mat,PAR_IND_G_k,PAR_IND_Q
 use hamiltonian,   ONLY:B_mat_index
 use R_lattice,     ONLY:bz_samp,qindx_S,nqbz
 implicit none
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
 allocate(COLL%PAR_map(COLL%N),COLL%PAR_IND%element_1D(COLL%N)) 
 !
 if (.not.associated(PAR_IND_G_k%element_1D) ) then
   allocate(PAR_IND_G_k%element_1D(k%nibz))
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
   if (.not.PAR_IND_Xk_ibz%element_1D(i_k)) cycle
   if (.not.PAR_IND_B_mat%element_1D( B_mat_index(ib1,ib2,COLL_bands) ) ) cycle
   !
   COLL%PAR_N=COLL%PAR_N+1
   COLL%PAR_map(i_coll)=COLL%PAR_N
   COLL%PAR_IND%element_1D(i_coll)=.TRUE.
   !
   PAR_IND_G_k%element_1D(i_k)=.TRUE.
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
 enddo
 !
 call PARALLEL_live_message(trim(COLL%name)//" Number of Collisions",LOADED=COLL%PAR_N,TOTAL=NC_tot)
 !
end subroutine
