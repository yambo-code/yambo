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
subroutine QP_apply_fit(band_range,qp,en,qpfit,what)
 !
 use pars,          ONLY:SP,schlen,DP
 use com,           ONLY:warning
 use stderr,        ONLY:intc
 use QP_m,          ONLY:QP_t
 use QP_CTL,        ONLY:qp_fit_ctl
 use electrons,     ONLY:levels
 implicit none
 !
 type(levels)    ::en
 type(QP_t)      ::qp
 type(qp_fit_ctl)::qpfit
 integer         ::band_range(2)
 character(1)    ::what
 !
 ! Work Space
 !
 integer  :: i1,Nqpc,Nqpv,fit_order,fit_order_on_fly
 real(SP) :: Eoc,Eov,Ec,Ev,V_err,C_err
 real(DP) :: EPS,Dy(qp%n_states),Dx(qp%n_states),Tt_def(3)
 real(DP), allocatable :: TCv(:),TCc(:)
 character (schlen)    :: warn_msg
 !
 ! fit data setup
 !
 Eov=-1.E5
 Eoc=1.E5
 Ev=-1.E5
 Ec=1.E5
 do i1=1,qp%n_states
   if (qp%table(i1,1)<band_range(1).or.qp%table(i1,1)>band_range(2)) cycle
   if (real(qp%E_bare(i1))<=1.E-5) then
     Eov=max(Eov,real(qp%E_bare(i1)))
     Ev=max(Ev,real(qp%E(i1))) 
   else
     Eoc=min(Eoc,real(qp%E_bare(i1)))
     Ec=min(Ec,real(qp%E(i1))) 
   endif
 enddo
 !
 fit_order=1
 if (what=='W') fit_order=2
 allocate(TCv(fit_order+1),TCc(fit_order+1))
 Tt_def=0.d0
 if (what=='E') Tt_def(2)=1.
 TCv=Tt_def(:fit_order+1)
 TCc=Tt_def(:fit_order+1)
 !
 ! E>0 (conduction)
 !
 fit_order_on_fly=fit_order
 !
 Nqpc=0
 do i1=1,qp%n_states
   if (qp%table(i1,1)<band_range(1).or.qp%table(i1,1)>band_range(2)) cycle
   if (real(qp%E_bare(i1))>1.E-5) then
     Nqpc=Nqpc+1
     if (what=='E') Dy(Nqpc)=real(qp%E(i1)-Eoc)
     if (what=='W') Dy(Nqpc)=aimag(qp%E(i1))
     Dx(Nqpc)=real(qp%E_bare(i1)-Eoc)
   endif
 enddo
 if (Nqpc>fit_order) then
   call polfit(Nqpc,Dx(:Nqpc),Dy(:Nqpc),fit_order_on_fly,TCc,EPS,0.d0)
   if (fit_order_on_fly/=fit_order) then
     call warning('Fit order on '//trim(what)//' [c] reduced to '//&
&                  trim(intc(fit_order_on_fly)))
     TCc(fit_order_on_fly+2:fit_order)=Tt_def(fit_order_on_fly+2:fit_order)
   endif
   if (what=='E') qpfit%E_err(1)=EPS
   if (what=='W') qpfit%W_err(1)=EPS
 endif
 !
 ! E<0 (valence)
 !
 fit_order_on_fly=fit_order
 !
 Nqpv=0
 do i1=1,qp%n_states
   if (qp%table(i1,1)<band_range(1).or.qp%table(i1,1)>band_range(2)) cycle
   if (real(qp%E_bare(i1))<=1.E-5) then
     Nqpv=Nqpv+1
     if (what=='E') Dy(Nqpv)=real(qp%E(i1)-Eov)
     if (what=='W') Dy(Nqpv)=aimag(qp%E(i1))
     Dx(Nqpv)=real(qp%E_bare(i1)-Eov)
   endif
 enddo
 if (Nqpv>fit_order) then
   call polfit(Nqpv,Dx(:Nqpv),Dy(:Nqpv),fit_order_on_fly,TCv,EPS,0.d0)
   if (fit_order_on_fly/=fit_order) then
     call warning('Fit order on '//trim(what)//' [v] reduced to '//&
&                  trim(intc(fit_order_on_fly)))
     TCv(fit_order_on_fly+2:fit_order)=Tt_def(fit_order_on_fly+2:fit_order)
   endif
   if (what=='E') qpfit%E_err(2)=EPS
   if (what=='W') qpfit%W_err(2)=EPS
 endif
 !
 ! Storing
 !
 qpfit%db_scissor=(Ec-Ev)-(Eoc-Eov)
 if (what=='E') qpfit%fit_scissor=TCc(1)-TCv(1)-qpfit%db_scissor
 if (what=='E') qpfit%E=(/TCc(1),TCc(2),TCv(1),TCv(2)/)
 if (what=='W') qpfit%W=qpfit%W+(/TCc(1),TCc(2),TCc(3),TCv(1),TCv(2),TCv(3)/)
 !
 deallocate(TCc,TCv)
 !
end subroutine
