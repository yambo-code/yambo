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
subroutine RT_mean_carrier_lifetimes(G_lesser,k,E)
 ! 
 use pars,           ONLY:SP,cI
 !DEBUG>
 !use units
 !DEBUG<
 use real_time,      ONLY:E_P_Hole_lifetime,E_P_Electron_lifetime,E_P_Phonon_lifetime,&
&                         E_E_Hole_lifetime,E_E_Electron_lifetime,RT_mean_lifetimes
 use QP_m,           ONLY:QP_nk
 use SC,             ONLY:SC_bands,it_now
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels,spin_occ
 implicit none
 !
 complex(SP),intent(in) :: G_lesser(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),QP_nk)
 type(levels)           :: E
 type(bz_samp)          :: k
 !
 ! Work space
 !
 integer            :: ib,ik,n_states
 real(SP)           :: f_occ
 real(SP),parameter :: f_tresh=1.E-5
 !
 n_states=0
 !
 RT_mean_lifetimes=0._SP
 !
 do ik=1,QP_nk
   do ib=1,SC_bands(2)
     if (ib< SC_bands(1)) then
       f_occ=spin_occ
     else 
       f_occ=-cI*G_lesser(ib,ib,ik)
     endif
     !
     if (abs(f_occ)>f_tresh.and.abs(f_occ-spin_occ)>f_tresh) then
       n_states=n_states+1
       if (allocated(E_P_Hole_lifetime)) then
         RT_mean_lifetimes(1)=RT_mean_lifetimes(1)+E_P_Hole_lifetime(ib,ik)
         RT_mean_lifetimes(2)=RT_mean_lifetimes(2)+E_P_Electron_lifetime(ib,ik)
         RT_mean_lifetimes(3)=RT_mean_lifetimes(3)+E_P_Phonon_lifetime(ib,ik)
       endif
       if (allocated(E_E_Hole_lifetime)) then
         RT_mean_lifetimes(4)=RT_mean_lifetimes(4)+E_E_Hole_lifetime(ib,ik)
         RT_mean_lifetimes(5)=RT_mean_lifetimes(5)+E_E_Electron_lifetime(ib,ik)
       endif
     endif
     !
     !DEBUG>
     !if (ib<SC_bands(1)) cycle
     !
     !if (allocated(E_P_Hole_lifetime)) then
     !  write (101,*) E%E(ib,ik,1)*HA2EV,E_P_Hole_lifetime(ib,ik)*HA2EV*1000 
     !  write (102,*) E%E(ib,ik,1)*HA2EV,E_P_Electron_lifetime(ib,ik)*HA2EV*1000 
     !endif
     !if (allocated(E_E_Hole_lifetime)) then
     !  write (201,*) E%E(ib,ik,1)*HA2EV,E_E_Hole_lifetime(ib,ik)*HA2EV*1000 
     !  write (202,*) E%E(ib,ik,1)*HA2EV,E_E_Electron_lifetime(ib,ik)*HA2EV*1000 
     !endif
     !DEBUG<
     !
   enddo
 enddo
 !DEBUG>
! if (it_now==2) then
   !write (*,*)
   !write (*,'(1x,a,f20.10)') 'E_P(hole)@k1b4',E_P_Hole_lifetime(4,1)*HA2EV*1000.
   !write (*,'(1x,a,f20.10)') 'E_E(hole)@k1b4',E_E_Hole_lifetime(4,1)*HA2EV*1000.
   !write (*,*)
   !write (*,'(1x,a,f20.10)') 'E_P(hole)@k1b8',E_P_Hole_lifetime(8,1)*HA2EV*1000.
   !write (*,'(1x,a,f20.10)') 'E_E(hole)@k1b8',E_E_Hole_lifetime(8,1)*HA2EV*1000.
   !write (*,*)
   !write (*,'(1x,a,f20.10)') 'E_P(electron)@k1b5',E_P_Electron_lifetime(4,1)*HA2EV*1000.
   !write (*,'(1x,a,f20.10)') 'E_E(electron)@k1b5',E_E_Electron_lifetime(4,1)*HA2EV*1000.
   !write (*,*)
   !write (*,'(1x,a,f20.10)') 'E_P(hole)@k2b4',E_P_Hole_lifetime(4,2)*HA2EV*1000.
   !write (*,'(1x,a,f20.10)') 'E_E(hole)@k2b4',E_E_Hole_lifetime(4,2)*HA2EV*1000.
   !write (*,*)
   !write (*,'(1x,a,f20.10)') 'E_P(electron)@k2b6',E_P_Electron_lifetime(6,2)*HA2EV*1000.
   !write (*,'(1x,a,f20.10)') 'E_E(electron)@k2b6',E_E_Electron_lifetime(6,2)*HA2EV*1000.
   !write (*,*)
   !write (*,'(1x,a,f20.10)') 'E_P(hole)@k5b3',E_P_Hole_lifetime(3,5)*HA2EV*1000.
   !write (*,'(1x,a,f20.10)') 'E_E(hole)@k5b3',E_E_Hole_lifetime(3,5)*HA2EV*1000.
   !write (*,*)
   !write (*,'(1x,a,f20.10)') 'E_P(hole)@k5b4',E_P_Hole_lifetime(4,5)*HA2EV*1000.
   !write (*,'(1x,a,f20.10)') 'E_E(hole)@k5b4',E_E_Hole_lifetime(4,5)*HA2EV*1000.
   !write (*,*)
   !write (*,'(1x,a,f20.10)') 'E_P(electron)@k5b5',E_P_Electron_lifetime(5,5)*HA2EV*1000.
   !write (*,'(1x,a,f20.10)') 'E_E(electron)@k5b5',E_E_Electron_lifetime(5,5)*HA2EV*1000.
   !write (*,*)
   !write (*,'(1x,a,f20.10)') 'E_P(electron)@k5b6',E_P_Electron_lifetime(6,5)*HA2EV*1000.
   !write (*,'(1x,a,f20.10)') 'E_E(electron)@k5b6',E_E_Electron_lifetime(6,5)*HA2EV*1000.
   !stop
! endif
 !DEBUG<
 !
 if (n_states>0) RT_mean_lifetimes=RT_mean_lifetimes/float(n_states)
 !
end subroutine RT_mean_carrier_lifetimes
