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
complex(SP) function QP_eval_the_FIT(i_ACTION,i_spin,Energy,band_range,E,k,what)
 !
 use pars,          ONLY:SP,cZERO
 use units,         ONLY:HA2EV
 use electrons,     ONLY:levels,n_spin,n_spinor
 use R_lattice,     ONLY:bz_samp
 use QP_CTL_m,      ONLY:QP_action
 use interfaces,    ONLY:el_density_of_states
 !
 implicit none
 !
 type(levels)    ::E
 type(bz_samp)   ::k
 integer         ::i_ACTION,i_spin,band_range(2)
 real(SP)        ::Energy
 character(1)    ::what
 !
 ! Work Space
 !
 real(SP)    :: dos_factor(n_spin),E_ref,M_factor
 logical     :: is_valence
 integer     :: iref,jref,i_p
 !
 ! Valence or conduction
 !-----------------------
 is_valence=Energy<=1.E-5
 iref=0
 jref=0
 QP_eval_the_FIT=cZERO
 !
 ! Energy
 !========
 if (what=='E') then
   !
   if (is_valence) iref=2
   !
   ! Scissor correction
   QP_eval_the_FIT=QP_action(i_ACTION)%CTL(i_spin)%E(iref+1) 
   !
   ! Stretching correction
   if (.not.is_valence) E_ref=E%E_CBm
   if (     is_valence) E_ref=E%E_VBM
   M_factor=(QP_action(i_ACTION)%CTL(i_spin)%E(iref+2)-1._SP) 
   QP_eval_the_FIT=QP_eval_the_FIT+M_factor*(Energy-E_ref)
 endif
 !
 ! Lifetimes
 !===========
 if (what=="W") then
   !
   if (is_valence) then
     iref=3
     jref=1
     E_ref=QP_action(i_ACTION)%CTL(i_spin)%W_E_ref(2)
     !if (Energy>E_ref) return
   else
     E_ref=QP_action(i_ACTION)%CTL(i_spin)%W_E_ref(1)
     !if (Energy<E_ref) return
   endif
   !
   ! Constant correction 
   !QP_eval_the_FIT=QP_action(i_ACTION)%CTL(i_spin)%W(iref+1)
   !
   ! DOS proportional correction
   call el_density_of_states(k,E,Energy,0.1_SP/HA2EV,band_range,dos_factor,.false.)
   M_factor=QP_action(i_ACTION)%CTL(i_spin)%W_dos(jref+1)
   QP_eval_the_FIT=QP_eval_the_FIT+M_factor*dos_factor(i_spin)*real(n_spinor,SP)
   !
   ! Linear/Quadratic correction
   do i_p=1,2
     M_factor=QP_action(i_ACTION)%CTL(i_spin)%W(iref+1+i_p)
     !QP_eval_the_FIT=QP_eval_the_FIT+M_factor*(Energy-E_ref)**i_p
   enddo
   !
   if (.not.is_valence) QP_eval_the_FIT=-abs(QP_eval_the_FIT)
   if (     is_valence) QP_eval_the_FIT= abs(QP_eval_the_FIT)
   !
 endif
 !
 ! Renormalization factors
 !=========================
 if (what=='Z') then
   QP_eval_the_FIT=QP_action(i_ACTION)%CTL(i_spin)%Z
 endif
 !
end function
