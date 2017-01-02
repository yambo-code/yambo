!
!        Copyright (C) 2000-2017 the YAMBO team
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
subroutine RT_ELPH_eq_kernel(ik,i_map,ik_m_q,N_q,E,TIME,ph_freq,Gkkp,S_mat)
 !  
 use pars,           ONLY:SP,cI,pi,cZERO
 use electrons,      ONLY:levels,spin_occ
 use real_time,      ONLY:RT_bands
 !
 implicit none
 !
 type(levels),  intent(in)  :: E
 integer,       intent(in)  :: ik,i_map(-1:1),ik_m_q
 real(SP),      intent(in)  :: TIME,ph_freq,N_q(2)
 complex(SP),   intent(in)  :: Gkkp(RT_bands(2)-RT_bands(1)+1,RT_bands(2)-RT_bands(1)+1)
 complex(SP),   intent(out) :: S_mat(2,RT_bands(2)-RT_bands(1)+1,RT_bands(2)-RT_bands(1)+1)
 !
 ! Work Space ...
 !
 integer  :: i_n,i_np,i_e,i_s,B_dim
 real(SP) :: DELTA,GAMMA_,OCCUPATIONS,&
&            f_k(RT_bands(2)-RT_bands(1)+1),f_k_m_q(RT_bands(2)-RT_bands(1)+1)
 !
 S_mat=cZERO
 !
 ! Dimensions
 !============
 !
 B_dim=RT_bands(2)-RT_bands(1)+1
 !
 do i_n=1,B_dim
   f_k(i_n)    = E%f(i_n+RT_bands(1)-1,ik,1)/spin_occ
   f_k_m_q(i_n)= E%f(i_n+RT_bands(1)-1,ik_m_q,1)/spin_occ
 enddo
 !
 do i_s=-1,1,2
   !
   do i_np=1,B_dim
     !
     do i_e=1,B_dim
       !
       OCCUPATIONS = N_q(i_map(-i_s))*(1._SP-f_k_m_q(i_e))*f_k(i_np) - N_q(i_map(i_s))*(1._SP-f_k(i_np))*f_k_m_q(i_e)
       DELTA       = E%E(i_e+RT_bands(1)-1,ik_m_q,1)-E%E(i_np+RT_bands(1)-1,ik,1)
       GAMMA_      = abs(E%W(i_e+RT_bands(1)-1,ik_m_q,1))+abs(E%W(i_np+RT_bands(1)-1,ik,1))
       !
       do i_n=1,B_dim
         !
         if (i_n/=i_np) cycle
         !
         S_mat(i_map(i_s),i_n,i_np)=S_mat(i_map(i_s),i_n,i_np)+&
&                                   conjg(Gkkp(i_e,i_n))*Gkkp(i_np,i_e)*OCCUPATIONS*&
&                                   exp(-(cI*DELTA+GAMMA_)*TIME)/(GAMMA_+cI*(DELTA-i_s*ph_freq))
         !
       enddo
       !
     enddo
     !
   enddo
   !
 enddo
 !
end subroutine RT_ELPH_eq_kernel
