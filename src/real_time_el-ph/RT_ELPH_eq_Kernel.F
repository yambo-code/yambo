!
!        Copyright (C) 2000-2019 the YAMBO team
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
subroutine RT_ELPH_eq_kernel(ik,i_map,ik_m_q,N_q,E,TIME,ph_freq,Gkkp_sq,S_mat)
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
 real(SP),      intent(in)  :: Gkkp_sq(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 complex(SP),   intent(out) :: S_mat(2,RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 !
 ! Work Space ...
 !
 integer  :: i_n,i_np,i_e,i_s
 real(SP) :: DELTA,GAMMA_,OCCUPATIONS
 !
 S_mat=cZERO
 !
 ! Dimensions
 !============
 !
 do i_s=-1,1,2
   !
   do i_np=RT_bands(1),RT_bands(2)
     !
     do i_e=RT_bands(1),RT_bands(2)
       !
       OCCUPATIONS = (N_q(i_map(-i_s))*(1._SP-E%f(i_e ,ik_m_q,1)/spin_occ)*E%f(i_np,ik    ,1)  -&
&                     N_q(i_map( i_s))*(1._SP-E%f(i_np,ik    ,1)/spin_occ)*E%f(i_e ,ik_m_q,1))/spin_occ
       DELTA       = E%E(i_e,ik_m_q,1)-E%E(i_np,ik,1)
       GAMMA_      = abs(E%W(i_e,ik_m_q,1))+abs(E%W(i_np,ik,1))
       !
       do i_n=RT_bands(1),RT_bands(2)
         !
         if (i_n/=i_np) cycle
         !
         S_mat(i_map(i_s),i_n,i_np)=S_mat(i_map(i_s),i_n,i_np)+&
&                                   Gkkp_sq(i_e,i_n)*OCCUPATIONS*&
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
