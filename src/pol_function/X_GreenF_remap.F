!
! Copyright (C) 2000-2010 A. Marini and the YAMBO team 
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
subroutine X_GreenF_remap(bands,E,W)
 !
 use pars,          ONLY:SP,pi
 use units,         ONLY:HARTREE
 use timing,        ONLY:live_timing
 use electrons,     ONLY:levels,n_sp_pol
 use frequency,     ONLY:w_samp,W_reset
 use memory_m,      ONLY:mem_est
 !
 implicit none
 integer      :: bands(2)
 type(w_samp) :: W
 type(levels) :: E
 !
 ! Work Space
 !
 integer, parameter :: new_map_points=500
 integer            :: i_b,i_k,i_spin
 type(w_samp)       :: W_A
 type(levels)       :: E_map
 !
 real(SP)           :: W_sign,A_sign
 complex(SP)        :: Ao(E%GreenF_n_E_steps),zero_plus
 !
 ! Resets
 !
 call W_reset(W_A)
 nullify(E_map%GreenF)
 !
 ! New energy range
 !
 W_A%n=max(new_map_points,W%n(1))
 W_A%er=max(abs(W%er(1)),abs(W%er(2)))*(/0.,1./)
 W_A%dr=0.
 call FREQUENCIES_setup(W_A)
 W_A%p=real(W_A%p)
 !
 allocate(E_map%GreenF(bands(2),E%nk,n_sp_pol,W_A%n(1)))
 E_map%GreenF=(0.,0.)
 !
 call live_timing('GF remapping',(bands(2)-bands(1)+1)*E%nk)
 ! 
 do i_b=bands(1),bands(2)
   do i_k=1,E%nk
     do i_spin=1,n_sp_pol
       !
       if ( i_b> E%nbf ) then
         !
         ! Conduction
         !
         W_sign= 1.
         A_sign=-1.
         if ( E%GreenF_is_causal ) A_sign=1.
       else
         !
         ! Valence
         !
         W_sign=-1.
         A_sign=-1.
       endif
       !
       Ao=A_sign/pi*aimag(E%GreenF(i_b,i_k,i_spin,:))
       Ao(1)                  = (0.,0.)
       Ao(E%GreenF_n_E_steps) = (0.,0.)
       zero_plus=cmplx(0.,0.00001/HARTREE)
       !
       call Kramers_Kronig(Ao,&
&                          real(E%GreenF_W(i_b,i_k,i_spin,:)),E%GreenF_n_E_steps,&
&                          E_map%GreenF(i_b,i_k,i_spin,:),&
&                          W_sign*W_A%p,W_A%n(1),W_sign*zero_plus)
       !
     enddo
     !
     call live_timing(steps=1)
     !
   enddo
 enddo
 !
 call live_timing()
 !
 ! Transfer
 !
 ! Note that the spectral function of the Green Functions defined here have 
 ! an integral equal to (Z*pi)
 !
 nullify(E%GreenF,E%GreenF_W)
 call mem_est("E-GreenF E-GreenF_W")
 E%GreenF_n_E_steps=W_A%n(1)
 allocate(E%GreenF(bands(2),E%nk,n_sp_pol,E%GreenF_n_E_steps))
 call mem_est("E-GreenF",(/size(E%GreenF)/))
 allocate(E%GreenF_W(bands(2),E%nk,n_sp_pol,E%GreenF_n_E_steps))
 call mem_est("E-GreenF_W",(/size(E%GreenF_W)/))
 do i_b=bands(1),bands(2)
   do i_k=1,E%nk
     do i_spin=1,n_sp_pol
       E%GreenF(i_b,i_k,i_spin,:)  =aimag(E_map%GreenF(i_b,i_k,i_spin,:))
       E%GreenF_W(i_b,i_k,i_spin,:)=W_A%p
     enddo
   enddo
 enddo
 !
 ! CLEAN
 !
 call W_reset(W_A)
 nullify(E_map%GreenF)
 !
end subroutine
