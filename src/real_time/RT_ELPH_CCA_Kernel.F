!
! Copyright (C) 2000-2012 A. Marini and the YAMBO team 
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
subroutine RT_ELPH_CCA_kernel(ik,ik_m_q,N_q,E,ph_freq,Gkkp,S_mat)
 !  
 use pars,           ONLY:SP,cI,pi
 use electrons,      ONLY:levels,spin_occ
 use SC,             ONLY:SC_bands,it_now
 use real_time,      ONLY:G_lesser,MEM_index
 !
 implicit none
 !
 type(levels),  intent(in)  :: E
 integer,       intent(in)  :: ik,ik_m_q
 real(SP),      intent(in)  :: ph_freq,N_q(2)
 complex(SP),   intent(in)  :: Gkkp(SC_bands(2)-SC_bands(1)+1,SC_bands(2)-SC_bands(1)+1)
 complex(SP),   intent(out) :: S_mat(SC_bands(2)-SC_bands(1)+1,SC_bands(2)-SC_bands(1)+1)
 !
 ! Work Space ...
 !
 integer  :: i_n,i_np,B_dim
 real(SP) :: DELTA,GAMMA_,f_k,f_k_m_q,nq
 !
 ! Dimensions
 !============
 !
 B_dim=SC_bands(2)-SC_bands(1)+1
 !
 ! Kernel kind
 !============= 
 !
 nq=N_q(-1)
 !
 do i_n=1,B_dim
   !
   f_k=-cI*G_lesser(i_n+SC_bands(1)-1,i_n+SC_bands(1)-1,ik,MEM_index(it_now))/spin_occ
   !
   do i_np=1,B_dim
     !
     f_k_m_q=-cI*G_lesser(i_np+SC_bands(1)-1,i_np+SC_bands(1)-1,ik_m_q,MEM_index(it_now))/spin_occ
     !
     DELTA  = E%E(i_np+SC_bands(1)-1,ik_m_q,1)-E%E(i_n+SC_bands(1)-1,ik,1)
     GAMMA_ = abs(E%W(i_np+SC_bands(1)-1,ik_m_q,1))+abs(E%W(i_n+SC_bands(1)-1,ik,1))
     !
     S_mat(i_n,i_n)=S_mat(i_n,i_n)+abs(Gkkp(i_np,i_n))**2*&
&                   ( (nq*(f_k-f_k_m_q)-f_k_m_q*(1.-f_k))*GAMMA_/(GAMMA_**2+(DELTA-ph_freq)**2)+&
&                     (f_k*(1.-f_k_m_q)-nq*(f_k_m_q-f_k))*GAMMA_/(GAMMA_**2+(DELTA+ph_freq)**2) )
     !
   enddo
   !
 enddo
 !
end subroutine RT_ELPH_CCA_kernel
