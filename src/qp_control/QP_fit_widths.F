!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM DS
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
subroutine QP_fit_widths(i_ACTION,band_range,en,k,qp)
 !
 ! Performs a linear fit of the QP corrections in the database
 ! that will be passed to the QP_apply_stretch routine driven by
 ! the mod_QP_CTL module
 !
 use pars,          ONLY:SP,DP
 use units,         ONLY:HA2EV
 use R_lattice,     ONLY:bz_samp
 use QP_m,          ONLY:QP_t
 use QP_CTL_m,      ONLY:QP_action
 use electrons,     ONLY:levels,n_sp_pol,n_spin,n_spinor,spin
 use interfaces,    ONLY:el_density_of_states
 use POL_FIT_m,     ONLY:POL_Gilbreth_fit
 !
 implicit none
 !
 type(levels)    ::en
 type(bz_samp)   ::k
 type(QP_t)      ::qp
 integer         ::i_ACTION,band_range(2)
 !
 ! Work Space
 !
 integer  :: i_s,N,i_c,i_spin
 real(DP) :: Dx(qp%n_states),Dy(qp%n_states),COEFF(3),SIG(qp%n_states)
 real(SP) :: dos_broad,E_ref(2),DOS(n_spin,qp%n_states)
 logical  :: l_EP,l_EE
 character(1) :: where
 !
 ! Defaults
 !
 SIG=0.05
 dos_broad=0.1_SP/HA2EV
 l_EP=index(QP_action(i_ACTION)%desc,"E-P")>0
 l_EE=index(QP_action(i_ACTION)%desc,"E-E")>0
 !
 ! DOS
 !
 if (l_EP) then
   do i_s=1,qp%n_states
     call el_density_of_states(k,en,qp%E_bare(i_s),dos_broad,band_range,DOS(:,i_s),.false.)
     DOS(:,i_s)=DOS(:,i_s)*real(n_spinor,SP)
   enddo
 endif
 !
 do i_spin =1 , n_sp_pol
   !
   ! References
   !
   E_ref(1)= 1.E5_SP ! c
   E_ref(2)=-1.E5_SP ! v
   !
   do i_s=1,qp%n_states
     !
     if (qp%table(i_s,1)<band_range(1).or.qp%table(i_s,1)>band_range(2)) cycle
     if (spin(qp%table(i_s,:))/=i_spin) cycle
     !
     if (qp%E_bare(i_s)<=1.E-5_SP) then
       E_ref(2)=max(E_ref(2),qp%E_bare(i_s))
     else
       E_ref(1)=min(E_ref(1),qp%E_bare(i_s))
     endif
   enddo
   !
   E_ref(1)=E_ref(1)+en%E_ind_gap(i_spin)
   E_ref(2)=E_ref(2)-en%E_ind_gap(i_spin) 
   !
   QP_action(i_ACTION)%CTL(i_spin)%W_E_ref=E_ref
   !
   do i_c=1,2
     !
     if (i_c==1) where="c"
     if (i_c==2) where="v"
     !
     ! STEP 1
     !========
     !
     ! ...E-P =>  uses DOS trick+constant only insied twice the gap
     ! ...E-E => constant
     !
     N=0
     do i_s=1,qp%n_states
       if (.not.is_inside_twice_the_gap(qp%E_bare(i_s))) cycle
       if (l_EP.and.DOS(i_spin,i_s)<1.E-6_SP) cycle
       N=N+1
       Dx(N)=qp%E_bare(i_s)
       Dy(N)=aimag(qp%E(i_s))
       if (l_EP) Dy(N)=aimag(qp%E(i_s))/DOS(i_spin,i_s)
     enddo 
     if (N>1) then
       call POL_Gilbreth_fit(Dx(:N),Dy(:N),SIG(:N),COEFF(1:1))
       QP_action(i_ACTION)%CTL(i_spin)%W(3*(i_c-1)+1)=COEFF(1)
       if (l_EP) then
         QP_action(i_ACTION)%CTL(i_spin)%W_dos(i_c)=COEFF(1)
         QP_action(i_ACTION)%CTL(i_spin)%W(3*(i_c-1)+1)=0.
       endif
     endif
     !
     ! STEP 2
     !========
     !
     ! ...above twice the gap (only E-E)
     !
     ! \G-\G_STEP_1=COEFF(1)+COEFF(2)*(E-E_ref)+COEFF(3)(E-E_ref)^2
     !
     if (.not.l_EE) cycle
     !
     N=0
     do i_s=1,qp%n_states
       if (.not.is_outside_twice_the_gap(qp%E_bare(i_s))) cycle
       N=N+1
       Dx(N)=qp%E_bare(i_s)-E_ref(i_c)
       Dy(N)=aimag(qp%E(i_s))-COEFF(1)
       if (l_EP) Dy(N)=aimag(qp%E(i_s))-COEFF(1)*DOS(i_spin,i_s)
     enddo
     if (N>3) then
       call POL_Gilbreth_fit(Dx(:N),Dy(:N),SIG(:N),COEFF)
       QP_action(i_ACTION)%CTL(i_spin)%W(3*(i_c-1)+1)=QP_action(i_ACTION)%CTL(i_spin)%W(3*(i_c-1)+1)+COEFF(1)
       QP_action(i_ACTION)%CTL(i_spin)%W(3*(i_c-1)+2:3*(i_c-1)+3)=COEFF(2:3)
       QP_action(i_ACTION)%CTL(i_spin)%W_err(i_c)=0.
     endif
     !
   enddo
   !
 enddo
 !
 contains
   !
   logical function is_inside_twice_the_gap(E)
     real(SP)      :: E
     is_inside_twice_the_gap=.TRUE.
     if (qp%table(i_s,1)<band_range(1).or.qp%table(i_s,1)>band_range(2)) is_inside_twice_the_gap=.FALSE.
     if (where=="c") then
       is_inside_twice_the_gap=qp%E_bare(i_s)<E_ref(1).and.qp%E_bare(i_s)>1.E-5_SP
     else
       is_inside_twice_the_gap=qp%E_bare(i_s)>E_ref(2).and.qp%E_bare(i_s)<=1.E-5_SP
     endif
   end function
   !
   logical function is_outside_twice_the_gap(E)
     real(SP)      :: E
     is_outside_twice_the_gap=.TRUE.
     if (qp%table(i_s,1)<band_range(1).or.qp%table(i_s,1)>band_range(2)) is_outside_twice_the_gap=.FALSE.
     if (where=="c") then
       is_outside_twice_the_gap=qp%E_bare(i_s)>E_ref(1).and.qp%E_bare(i_s)>1.E-5_SP
     else
       is_outside_twice_the_gap=qp%E_bare(i_s)<E_ref(2).and.qp%E_bare(i_s)<=1.E-5_SP
     endif
   end function
   !
end subroutine
