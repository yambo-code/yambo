!
! Copyright (C) 2000-2013 A. Marini, M. Gruning and the YAMBO team 
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
subroutine RT_Sex_collision(k,q,G_input)
 !
 ! MG 30/01/13: Here I try to use the collision built for the e-e scattering
 !              for the Screened Exchange, so that the two things can be used together 
 !              NOTE: I used QP_Vnl_xc instead of QP_Sc usually reserved for cohsex (added to HF)  
 !
 use pars,            ONLY:SP,cI,rZero,pi
 use electrons,       ONLY:levels,spin_occ
 use R_lattice,       ONLY:qindx_S,bz_samp,qindx_X
 use SC,              ONLY:SC_bands,it_now
 use QP_m,            ONLY:QP_n_states,QP_n_states_for_collisions,QP_table,&
&                          QP_nk,QP_Vnl_xc
 use collisions_CACHE,ONLY:CACHE
 use par_proc_m,      ONLY:pp_redux_wait
 !
 implicit none
 !
 type(bz_samp), intent(in) :: k,q
 complex(SP), intent(in)   :: G_input(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),QP_nk)
 !
 ! Work Space ...
 !
 integer     :: i_k,i_qp,i_qp1,i_qp2,i_kmq,i_n,i_np,i_mp,i_lp,iqbz,iqibz
 !
 ! Select only the m.e. of QP_table relative to kpt i_k
 !======================================================
 !
 if (.not.allocated(QP_Vnl_xc))  allocate(QP_Vnl_xc(QP_n_states))
 !
 do i_qp=1,QP_n_states
   !
   !   if (.not.RT_qp_px%element_1D(i_qp1)) cycle
   !   if (.not.RT_k_px%element_1D(i_k)) cycle
   !
   i_k =QP_table(i_qp,3)
   i_n =QP_table(i_qp,1)
   i_np=QP_table(i_qp,2)
   !
   do i_qp1=1,QP_n_states_for_collisions
     !
     if (QP_table(i_qp1,3)/=i_k) cycle
     if (QP_table(i_qp1,1)/=i_n) cycle
     i_lp=QP_table(i_qp1,2)
     !
     do i_qp2=1,QP_n_states_for_collisions
       !
       if (QP_table(i_qp2,3)/=i_k) cycle
       if (QP_table(i_qp2,1)/=i_np) cycle
       i_mp=QP_table(i_qp2,2)
       !
       do iqbz=1,q%nbz
         !
         iqibz = q%sstar(iqbz,1)
         i_kmq = k%sstar(qindx_S(i_k,iqbz,1),1)
         !
         !
         QP_Vnl_xc(i_qp)=QP_Vnl_xc(i_qp)+&
              &4._SP/spin_occ*pi*cI*CACHE(i_qp1)%COLLISIONS_v3(i_qp2,1,iqbz)*G_input(i_lp,i_mp,i_kmq)
         !
       enddo
       !
     enddo
     !
   enddo
   !
 enddo
 !
 ! All-to-All
 !============
 !
 !call pp_redux_wait(E_E_Electron_lifetime(:,i_k))
 !call pp_redux_wait(E_E_Hole_lifetime(:,i_k))
 ! 
end subroutine RT_Sex_collision

