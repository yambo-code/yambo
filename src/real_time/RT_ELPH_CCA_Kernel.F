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
subroutine RT_ELPH_CCA_Kernel(ik,ik_m_q,f_k,Bose_f,E,ph_freq,Gkkp)
 !  
 use pars,           ONLY:SP,cI,pi
 use electrons,      ONLY:levels,spin_occ,n_full_bands
 use SC,             ONLY:SC_bands,it_now
 use real_time,      ONLY:G_lesser,MEM_index,Hole_lifetime,Electron_lifetime,Phonon_lifetime
 !
 implicit none
 !
 type(levels),  intent(in)  :: E
 integer,       intent(in)  :: ik,ik_m_q
 real(SP),      intent(in)  :: ph_freq,Bose_f,f_k(SC_bands(2)-SC_bands(1)+1)
 complex(SP),   intent(in)  :: Gkkp(SC_bands(2)-SC_bands(1)+1,SC_bands(2)-SC_bands(1)+1)
 !
 ! Work Space ...
 !
 integer  :: i_n,i_np,B_dim,i_n_shift,i_np_shift
 real(SP) :: DELTA,GAMMA_,f_k_m_q,P_ph_abs,P_ph_emit
 !
 ! Dimensions
 !============
 !
 B_dim=SC_bands(2)-SC_bands(1)+1
 !
 ! Kernel kind
 !============= 
 !
 do i_n=1,B_dim
   !
   i_n_shift=i_n+SC_bands(1)-1
   !
   do i_np=1,B_dim
     ! 
     i_np_shift=i_np+SC_bands(1)-1
     !
     f_k_m_q=-cI*G_lesser(i_np+SC_bands(1)-1,i_np+SC_bands(1)-1,ik_m_q,MEM_index(it_now))/spin_occ
     !
     DELTA  = E%E(i_np+SC_bands(1)-1,ik_m_q,1)-E%E(i_n+SC_bands(1)-1,ik,1)
     GAMMA_ = abs(E%W(i_np+SC_bands(1)-1,ik_m_q,1))+abs(E%W(i_n+SC_bands(1)-1,ik,1))
     !
     P_ph_abs =0.
     P_ph_emit=0.
     !
     ! 16/3/2012
     !
     ! Including "by hand" the limits on the signs of DELTA I managed to get
     ! (finally) a converged el-ph dynamics.
     !
     ! Updated on 29/3/2012 with more tailored constrains. Valid only in systems
     ! with a gap.
     !
     ! n>=n_full_bands+1 => i_np>=n_full_bands+1 
     !
     ! Fermi level ----------------
     !
     ! n<=n_full_bands => i_np<=n_full_bands 
     !
     if (i_n_shift<=n_full_bands.and.i_np_shift>  n_full_bands) cycle
     if (i_n_shift> n_full_bands.and.i_np_shift<= n_full_bands) cycle
     !
     if (abs(DELTA)<1.E-10) cycle
     !
     if (DELTA>1.E-10) P_ph_abs =GAMMA_/(GAMMA_**2+(DELTA-ph_freq)**2)
     if (DELTA<1.E-10) P_ph_emit=GAMMA_/(GAMMA_**2+(DELTA+ph_freq)**2)
     !
     Hole_lifetime(i_n_shift,ik)=Hole_lifetime(i_n_shift,ik)+abs(Gkkp(i_np,i_n))**2*P_ph_abs*f_k_m_q
     Electron_lifetime(i_n_shift,ik)=Electron_lifetime(i_n_shift,ik)+abs(Gkkp(i_np,i_n))**2*P_ph_emit*(1.-f_k_m_q)
     Phonon_lifetime(i_n_shift,ik)=Phonon_lifetime(i_n_shift,ik)+abs(Gkkp(i_np,i_n))**2*Bose_f*(f_k(i_n)-f_k_m_q)*(P_ph_abs+P_ph_emit)
     !
   enddo
   !
 enddo
 !
end subroutine RT_ELPH_CCA_Kernel
