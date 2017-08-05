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
subroutine QP_fit_energies(i_ACTION,bands,qp)
 !
 ! Performs a linear fit of the QP corrections in the database
 ! that will be passed to the QP_apply_stretch routine driven by
 ! the mod_QP_CTL module
 !
 use pars,          ONLY:SP
 use com,           ONLY:warning
 use QP_m,          ONLY:QP_t
 use QP_CTL_m,      ONLY:QP_action
 use electrons,     ONLY:n_sp_pol,spin
 use POL_FIT_m,     ONLY:POL_LK_fit
 !
 implicit none
 !
 type(QP_t)      ::qp
 integer         ::i_ACTION,bands(2)
 !
 ! Work Space
 !
 integer  :: i1,Nqpc,Nqpv,fit_order,i_spin
 real(SP) :: Eoc,Eov,Ec,Ev,EPS,Dy(qp%n_states),Dx(qp%n_states),Tt_def(2),TCv(2),TCc(2)
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
     if (qp%table(i1,1)<bands(1).or.qp%table(i1,1)>bands(2)) cycle
     if (spin(qp%table(i1,:))/=i_spin) cycle
     if (real(qp%E_bare(i1))<=1.E-5_SP) then
       Eov=max(Eov,real(qp%E_bare(i1),SP))
       Ev=max(Ev ,real(qp%E(i1),SP)) 
     else
       Eoc=min(Eoc,real(qp%E_bare(i1),SP))
       Ec=min(Ec,real(qp%E(i1),SP)) 
     endif
   enddo
   !
   Tt_def(1)=0._SP
   Tt_def(2)=1._SP
   TCv=Tt_def
   TCc=Tt_def
   !
   ! E>0 (conduction)
   !
   fit_order=1
   !
   Nqpc=0
   do i1=1,qp%n_states
     if (qp%table(i1,1)<bands(1).or.qp%table(i1,1)>bands(2).or.real(qp%E_bare(i1))<=1.E-5_SP) cycle
     Nqpc=Nqpc+1
     Dy(Nqpc)=real(qp%E(i1)-Ec)
     Dx(Nqpc)=real(qp%E_bare(i1)-Eoc)
   enddo
   if (Nqpc>1) then
     call POL_LK_fit(Nqpc,Dx(:Nqpc),Dy(:Nqpc),fit_order,TCc,EPS,0._SP)
     if (fit_order==0) then
       call warning(' Fit order on energies [c] reduced to 0')
       TCc(2)=Tt_def(2)
     endif
     QP_action(i_ACTION)%CTL(i_spin)%E_err(1)=EPS
   endif
   !
   ! E<0 (valence)
   !
   fit_order=1
   !
   Nqpv=0
   do i1=1,qp%n_states
     if (qp%table(i1,1)<bands(1).or.qp%table(i1,1)>bands(2).or.real(qp%E_bare(i1))>1.E-5) cycle
     Nqpv=Nqpv+1
     Dy(Nqpv)=real(qp%E(i1)-Ev)
     Dx(Nqpv)=real(qp%E_bare(i1)-Eov)
   enddo
   if (Nqpv>1) then
     call POL_LK_fit(Nqpv,Dx(:Nqpv),Dy(:Nqpv),fit_order,TCv,EPS,0._SP)
     if (fit_order==0) then
       call warning(' Fit order on energies [v] reduced to 0')
       TCv(2)=Tt_def(2)
     endif
     QP_action(i_ACTION)%CTL(i_spin)%E_err(2)=EPS
   endif
   !
   ! Storing
   !
   QP_action(i_ACTION)%CTL(i_spin)%db_scissor=(Ec-Eoc)-(Ev-Eov)
   QP_action(i_ACTION)%CTL(i_spin)%fit_scissor=TCc(1)-TCv(1)
   QP_action(i_ACTION)%CTL(i_spin)%E=(/TCc(1)+Ec-Eoc,TCc(2),TCv(1)+Ev-Eov,TCv(2)/)
   !
 enddo
 !
end subroutine
