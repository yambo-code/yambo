!
! Copyright (C) 2000-2005 A. Marini and the SELF team 
!         http://www.fisica.uniroma2.it/~self
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
 subroutine G_RIM_Gf(W,E,transitions,Eo,Gf)
!==========================================
 use pars,          ONLY:SP
 use drivers,       ONLY:Finite_Tel
 use electrons,     ONLY:levels,BZ_RIM_table,BZ_RIM_nkpt
 use functions,     ONLY:bose_f
 use frequency,     ONLY:w_samp
 implicit none
 integer      :: transitions(3)
 type(w_samp) :: W
 type(levels) :: E
 complex(SP)  :: Gf(W%n(2))
 real(SP)     :: Eo
!ws
 integer      ::ib,ikbz,i_spin,iw,ik_rand,i1
 complex(SP)  ::lW
!
 Gf  = cmplx(0.,0.,SP)
!
! Transitions
!
 ib      = transitions(1)
 ikbz    = transitions(2)
 i_spin  = transitions(3)
!
 do i1=1,BZ_RIM_nkpt(ikbz)
!     
   ik_rand=BZ_RIM_table(ikbz,i1) 
!     
   do iw=1,W%n(2)
     
     lW=W%p(iw)
     if (Finite_Tel) lW=conjg(lW)
    
     Gf(iw)=Gf(iw)+&
&           (2.-E%f_RIM(ib,ik_rand,i_spin)+bose_f(Eo))/&
&           (lW-E%E_RIM(ib,ik_rand,i_spin)-Eo)+&
&           (E%f_RIM(ib,ik_rand,i_spin)+bose_f(Eo))/&
&           (conjg(W%p(iw))-E%E_RIM(ib,ik_rand,i_spin)+Eo)

   enddo
! 
 enddo
! 
 Gf=Gf/real(BZ_RIM_nkpt(ikbz))
!
 end subroutine

