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
subroutine QP_apply_the_ACTION(i_ACTION,band_range,E,k,apply_anyway)
 !
 ! Here I apply the strecth for both QP_action.
 !
 ! When apply_anyway=.TRUE. the corrections are applied anyway.
 !
 use pars,          ONLY:SP,lchlen,rZERO,cI,cONE,rONE
 use units,         ONLY:HA2EV
 use electrons,     ONLY:levels,n_sp_pol,n_spin,n_spinor
 use R_lattice,     ONLY:bz_samp
 use QP_CTL_m,      ONLY:QP_action
 use interfaces,    ONLY:el_density_of_states
 !
#include<memory.h>
 !
 type(levels)    ::E
 type(bz_samp)   ::k
 integer         ::i_ACTION,band_range(2)
 logical         ::apply_anyway
 !
 ! Work Space
 !
 integer  ::ib,ik,i_sp,n_of_coeff,i_spin
 real(SP) ::M_factors(6),dos_factor(n_spin)
 character(lchlen) :: ch
 logical           :: do_E,do_W,do_Z
 logical,external  :: QP_check_if_corrected
 !
 ! Loop on spins
 !
 do i_sp= 1,n_sp_pol
   !
   ! Is there anything to do ?
   !
   do_E=.false.
   do_W=.false.
   do_Z=.false.
   M_factors=rZERO
   !
   if (any(QP_action(i_ACTION)%CTL(i_sp)%E/=(/rZERO,rONE,rZERO,rONE/))) then
     if (.not.apply_anyway) then
       do_E=.not.QP_check_if_corrected(band_range,(/1,E%nk/),(/i_sp,i_sp/),E,'E')
     endif
   endif
   if (any((/QP_action(i_ACTION)%CTL(i_sp)%W(:)/=rZERO/))) then
     if (.not.apply_anyway) then
       do_W=.not.QP_check_if_corrected(band_range,(/1,E%nk/),(/i_sp,i_sp/),E,'W')
     endif
     if (.not.allocated(E%W)) then
       YAMBO_ALLOC(E%W,(E%nb,E%nk,n_sp_pol))
       E%W=rZERO
     endif
   endif
   if (QP_action(i_ACTION)%CTL(i_sp)%Z/=cONE) then
     if (.not.apply_anyway) then
       do_Z=.not.QP_check_if_corrected(band_range,(/1,E%nk/),(/i_sp,i_sp/),E,'Z')
     endif
     if (.not.allocated(E%Z)) then
       YAMBO_ALLOC(E%Z,(E%nb,E%nk,n_sp_pol))
       E%Z=1._SP
     endif
   endif
   if (.not.any((/do_E,do_W,do_Z/))) return
   !
   i_spin=i_sp
   if (do_E) then
     n_of_coeff=2
     M_factors(:4)=QP_action(i_ACTION)%CTL(i_sp)%E
     call do_fit_operation('E')
   endif
   if (do_W) then
     n_of_coeff=3
     M_factors(:6)=QP_action(i_ACTION)%CTL(i_sp)%W
     call do_fit_operation('W')
   endif
   if (do_Z) then
     n_of_coeff=1
     M_factors(:2)=(/real(QP_action(i_ACTION)%CTL(i_sp)%Z),aimag(QP_action(i_ACTION)%CTL(i_sp)%Z)/)
     call do_fit_operation('Z')
   endif
   !
 enddo
 !
 contains
   !
   subroutine do_fit_operation(what)
     character(1) :: what
     !
     ! Work Space
     !
     integer     :: iref
     logical     :: l_add_DB_corr
     real(SP)    :: delta,rref,M_factor_local
     complex(SP) :: Z_tmp
     !
     do ib=band_range(1),band_range(2)
       !
       do ik=1,E%nk
         !
         ! Valence or conduction
         if (E%Eo(ib,ik,i_spin)> 1.E-5) iref=0
         if (E%Eo(ib,ik,i_spin)<=1.E-5) iref=n_of_coeff
         !
         l_add_DB_corr = (QP_action(i_ACTION)%STATES_done(ib,ik,i_spin)/=1).or.apply_anyway
         !
         ! Energy
         !========
         if (what=='E') then
           ! Scissor correction
           delta=M_factors(iref+1)
           ! Stretching correction
           if (E%Eo(ib,ik,i_spin)> 1.E-5_SP) rref=E%E_CBm
           if (E%Eo(ib,ik,i_spin)<=1.E-5_SP) rref=E%E_VBM
           M_factor_local=(M_factors(iref+2)-1._SP)
           delta=delta+M_factor_local*(E%Eo(ib,ik,i_spin)-rref)
           !
           E%E(ib,ik,i_spin)=E%E(ib,ik,i_spin)+delta
           !
         endif
         !
         ! Free factors are given from input file. In the case of W the user-defined reference
         ! energy is given with respect to the CBM/VBM. The position of the VBM and CBM must be consistent with their
         ! actual position in the energy levels.
         !
         ! Lifetimes
         !===========
         if (what=="W") then
           M_factor_local=0._SP
           ! Constant correction (only value from input used)
           delta=M_factors(iref+1)
           ! DOS proportional correction
           call el_density_of_states(k,E,E%E(ib,ik,i_spin),0.05_SP/HA2EV,(/1,E%nb/),dos_factor,.false.)
           M_factor_local=M_factors(iref+2)
           delta=delta+M_factor_local*dos_factor(i_spin)*real(n_spinor,SP)
           ! Quadratic correction
           if (E%Eo(ib,ik,i_spin)> 1.E-5_SP) rref=QP_action(i_ACTION)%CTL(i_sp)%W_E_ref(1)
           if (E%Eo(ib,ik,i_spin)<=1.E-5_SP) rref=QP_action(i_ACTION)%CTL(i_sp)%W_E_ref(2)
           if (E%Eo(ib,ik,i_spin)> max(1.E-5_SP,rref) .or. E%Eo(ib,ik,i_spin)<min(1.E-5_SP,rref)) then
             delta=delta+M_factors(iref+3)*(E%Eo(ib,ik,i_spin)-rref)**2
           endif
           !
           if (E%Eo(ib,ik,i_spin)<=1.E-5_SP) E%W(ib,ik,i_spin)=E%W(ib,ik,i_spin)+abs(delta)
           if (E%Eo(ib,ik,i_spin)> 1.E-5_SP) E%W(ib,ik,i_spin)=E%W(ib,ik,i_spin)-abs(delta)
           !
         endif
         !
         ! Renormalization factors
         !=========================
         if (what=='Z') then
           Z_tmp=M_factors(1)+cI*M_factors(2)
           E%Z(ib,ik,i_spin)=Z_tmp
         endif
         !
         if (.not.apply_anyway) then
           if (what=='E') E%QP_corrected(ib,ik,i_spin)=E%QP_corrected(ib,ik,i_spin)+1
           if (what=='W') E%QP_corrected(ib,ik,i_spin)=E%QP_corrected(ib,ik,i_spin)+2
           if (what=='Z') E%QP_corrected(ib,ik,i_spin)=E%QP_corrected(ib,ik,i_spin)+4
         endif
         !
       enddo
     enddo
   end subroutine
   !
end subroutine
