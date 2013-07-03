!
! Copyright (C) 2000-2013 A. Marini and the YAMBO team 
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
subroutine RT_manual_excitation(E,k)
 !
 use pars,           ONLY:SP,cI,cZERO
 use units,          ONLY:BO2ANG
 use D_lattice,      ONLY:DL_vol
 use R_lattice,      ONLY:bz_samp
 use SC,             ONLY:SC_bands
 use real_time,      ONLY:G_lesser,Eh_pumped_pair_energy,Eh_pumped_pair_width
 use stderr,         ONLY:intc
 use electrons,      ONLY:levels,spin_occ
 use com,            ONLY:msg
 !
 implicit none
 !
 type(levels)      :: E
 type(bz_samp)     :: k
 !
 ! Work Space
 !
 integer :: i1,i2,ib,ik
 real(SP):: local_E_window,delta_Nel
 logical :: Pumped_state(E%nb)
 !
 ! Possibility to pump by hand one or more electrons up
 !======================================================
 !
 if (Eh_pumped_pair_energy(2)<=0.) return
 !
 delta_Nel=0._SP
 !
 do ik=1,E%nk
   !
   i1=0
   Pumped_state=.FALSE.
   !
   local_E_window=Eh_pumped_pair_width/10.
   !
   do while(local_E_window<Eh_pumped_pair_width)
     !
     do i2=1,SC_bands(2)
       do ib=SC_bands(1),SC_bands(2)
         !
         if (Pumped_state(ib)) cycle
         !
         if (i1==0) then
           if(abs(E%E(ib,ik,1)-Eh_pumped_pair_energy(1))<local_E_window) i1=ib
         else
           if(abs(E%E(ib,ik,1)-Eh_pumped_pair_energy(2))<local_E_window) then
             G_lesser(i1,i1,ik,1) = cZERO
             G_lesser(ib,ib,ik,1) = spin_occ*cI
             Pumped_state(i1)=.TRUE.
             Pumped_state(ib)=.TRUE.
             call msg('sr','[RT] Eh pair pumped `by hand` @ k'//trim(intc(ik))//&
&                          ' bands '//trim(intc(i1))//' - '//trim(intc(ib)))
             i1=0
           endif
         endif
         !
         local_E_window=local_E_window+Eh_pumped_pair_width/10.
         !
         if (all(Pumped_state(SC_bands(1):SC_bands(2)))) local_E_window=Eh_pumped_pair_width*1.1
         !
       enddo
     enddo
   enddo
   !
   do ib=SC_bands(1),SC_bands(2)
     if (Pumped_state(ib)) delta_Nel=delta_Nel-cI*G_lesser(ib,ib,ik,1)*k%weights(ik)*2._SP
   enddo
   !
 enddo
 !
 call msg('sr','[RT] Carrier number and density [none,cm-3]:',delta_Nel*(/1._SP,1._SP/(DL_vol*BO2ANG**3*1.E-24)/))
 !
end subroutine RT_manual_excitation
