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
 use pars,          ONLY:SP,rZERO
 use units,         ONLY:HA2EV
 use R_lattice,     ONLY:bz_samp
 use QP_m,          ONLY:QP_t
 use QP_CTL_m,      ONLY:QP_action
 use electrons,     ONLY:levels,n_sp_pol,n_spin,n_spinor,spin
 use interfaces,    ONLY:el_density_of_states
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
 integer  :: i1,Nqpc,Nqpv,Nqpc_DOS,Nqpv_DOS,fit_order,i_spin
 real(SP) :: Eoc,Eov,Ec,Ev,dos_factor(n_spin),Ev_ref,Ec_ref,dos_broad
 real(SP) :: EPS,Dy(qp%n_states),Dx(qp%n_states),Dy_DOS(qp%n_states),Dx_DOS(qp%n_states),&
&            TCv(3),TCc(3)
 !
 dos_broad=0.01_SP/HA2EV
 !
 ! Loop on spin polarizations
 !
 do i_spin =1 , n_sp_pol
   !
   ! Fit energy range 
   !
   Eov=-1.E5_SP
   Eoc= 1.E5_SP
   Ev =-1.E5_SP
   Ec = 1.E5_SP
   !
   do i1=1,qp%n_states
     if (qp%table(i1,1)<band_range(1).or.qp%table(i1,1)>band_range(2)) cycle
     if (spin(qp%table(i1,:))/=i_spin) cycle
     if (qp%E_bare(i1)<=1.E-5_SP) then
       Eov=max(Eov,qp%E_bare(i1))
       Ev=max(Ev ,real(qp%E(i1),SP)) 
     else
       Eoc=min(Eoc,qp%E_bare(i1))
       Ec=min(Ec,real(qp%E(i1),SP)) 
     endif
   enddo
   !
   Ev_ref=Eov-en%E_ind_gap(i_spin) 
   Ec_ref=Eoc+en%E_ind_gap(i_spin)
   !
   TCv=rZERO
   TCc=rZERO
   !
   ! a) perform a constant fit in the energy reagion E_CBM<E<E_CBM+E_gap
   !    of a function proportional to the DOS: A_coeff~W
   !    (This value will not be used)
   ! +
   ! b) perform a constant fit in the energy reagion E_CBM<E<E_CBM+E_gap
   !    of a function proportional to the DOS: B_coeff~W/dos(E)
   !=========================================================================
   !
   ! E>0 (conduction)
   !
   fit_order=0
   Nqpc=0
   Nqpc_DOS=0
   do i1=1,qp%n_states
     if (qp%table(i1,1)<band_range(1).or.qp%table(i1,1)>band_range(2).or. &
&           qp%E_bare(i1)>Ec_ref.or.qp%E_bare(i1)<=1.E-5_SP) cycle
     Nqpc=Nqpc+1
     Dx(Nqpc)=qp%E_bare(i1)
     Dy(Nqpc)=aimag(qp%E(i1))
     call el_density_of_states(k,en,qp%E_bare(i1),dos_broad,(/1,en%nb/),dos_factor,.false.)
     if(dos_factor(i_spin)<1.E-6_SP) cycle
     if(index(QP_action(i_ACTION)%desc,"E-P")==0) cycle
     Nqpc_DOS=Nqpc_DOS+1
     Dx_DOS(Nqpc_DOS)=qp%E_bare(i1)
     Dy_DOS(Nqpc_DOS)=aimag(qp%E(i1))/(dos_factor(i_spin)*real(n_spinor,SP))
   enddo
   if (Nqpc>1) then
     call pol_fit(Nqpc,Dx(:Nqpc),Dy(:Nqpc)    ,fit_order,TCc(1),EPS,0._SP)
     QP_action(i_ACTION)%CTL(i_spin)%W_err(1)=EPS
   endif
   if (Nqpc_DOS>1) then
     call pol_fit(Nqpc_DOS,Dx(:Nqpc_DOS),Dy_DOS(:Nqpc_DOS),fit_order,TCc(2),EPS,0._SP)
     QP_action(i_ACTION)%CTL(i_spin)%W_err(3)=EPS
   endif
   !
   ! E<0 (valence)
   !
   fit_order=0
   Nqpv=0
   Nqpv_DOS=0
   do i1=1,qp%n_states
     if (qp%table(i1,1)<band_range(1).or.qp%table(i1,1)>band_range(2).or.&
&          qp%E_bare(i1)<Ev_ref.or.qp%E_bare(i1)> 1.E-5) cycle
     Nqpv=Nqpv+1
     Dx(Nqpv)=qp%E_bare(i1)
     Dy(Nqpv)=aimag(qp%E(i1))
     call el_density_of_states(k,en,qp%E_bare(i1),dos_broad,(/1,en%nb/),dos_factor,.false.)
     if(dos_factor(i_spin)<1.E-6_SP) cycle
     if(index(QP_action(i_ACTION)%desc,"E-P")==0) cycle
     Nqpv_DOS=Nqpv_DOS+1
     Dx_DOS(Nqpv_DOS)=qp%E_bare(i1)
     Dy_DOS(Nqpv_DOS)=aimag(qp%E(i1))/(dos_factor(i_spin)*real(n_spinor,SP))
   enddo
   if (Nqpv>1) then
     call pol_fit(Nqpv,Dx(:Nqpv),Dy(:Nqpv)    ,fit_order,TCv(1),EPS,0._SP)
     QP_action(i_ACTION)%CTL(i_spin)%W_err(2)=EPS
   endif
   if (Nqpv_DOS>1) then
     call pol_fit(Nqpv_DOS,Dx(:Nqpv_DOS),Dy_DOS(:Nqpv_DOS),fit_order,TCv(2),EPS,0._SP)
     QP_action(i_ACTION)%CTL(i_spin)%W_err(4)=EPS
   endif
   !
   ! c) perform a quadratic fit in the energy reagion E>E_CBM+E_gap
   !    after removing the of a constant plus a function proportional to the DOS
   !    the result of the previous fit C_coeff=(W(E)-B_coeff*dos(E))/(E-Eref)**2
   !=============================================================================
   !
   ! E>0 (conduction)
   !
   fit_order=0
   Nqpc=0
   do i1=1,qp%n_states
     if (qp%table(i1,1)<band_range(1).or.qp%table(i1,1)>band_range(2).or. &
&          qp%E_bare(i1)<=Ec_ref.or.qp%E_bare(i1)<=1.E-5_SP) cycle
     Nqpc=Nqpc+1
     Dx(Nqpc)=qp%E_bare(i1)
     call el_density_of_states(k,en,qp%E_bare(i1),dos_broad,(/1,en%nb/),dos_factor,.false.)
     Dy(Nqpc)=(aimag(qp%E(i1))-dos_factor(i_spin)*real(n_spinor,SP)*TCc(2))/(qp%E_bare(i1)-Ec_ref)**2
   enddo
   if (Nqpc>1) then
     call pol_fit(Nqpc,Dx(:Nqpc),Dy(:Nqpc)    ,fit_order,TCc(3),EPS,0._SP)
     QP_action(i_ACTION)%CTL(i_spin)%W_err(5)=EPS
   endif
   !
   ! E<0 (valence)
   !
   fit_order=0
   Nqpv=0
   do i1=1,qp%n_states
     if (qp%table(i1,1)<band_range(1).or.qp%table(i1,1)>band_range(2).or.&
&          qp%E_bare(i1)>=Ev_ref.or.qp%E_bare(i1)> 1.E-5) cycle
     Nqpv=Nqpv+1
     Dx(Nqpv)=qp%E_bare(i1)
     call el_density_of_states(k,en,qp%E_bare(i1),dos_broad,(/1,en%nb/),dos_factor,.false.)
     Dy(Nqpv)=(aimag(qp%E(i1))-dos_factor(i_spin)*real(n_spinor,SP)*Tcv(2))/(qp%E_bare(i1)-Ev_ref)**2
   enddo
   if (Nqpv>1) then
     call pol_fit(Nqpv,Dx(:Nqpv),Dy(:Nqpv)    ,fit_order,TCv(3),EPS,0._SP)
     QP_action(i_ACTION)%CTL(i_spin)%W_err(6)=EPS
   endif
   !
   ! Storing
   !
   QP_action(i_ACTION)%CTL(i_spin)%W=QP_action(i_ACTION)%CTL(i_spin)%W+(/TCc(1),TCc(2),TCc(3),TCv(1),TCv(2),TCv(3)/)
   !
 enddo
 !
end subroutine
