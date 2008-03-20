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
subroutine QP_apply_stretch(band_range,E,QP_ctl_from_DB,QP_ctl_add,&
&                           DB_corrected)
 !
 ! Here I apply the strecth for both QP_ctl_from_DB (respecting the 
 ! qp_done array) and QP_ctl_add (everywhere)
 !
 use pars,          ONLY:SP,lchlen,HARTREE
 use memory_m,      ONLY:mem_est
 use com,           ONLY:msg
 use electrons,     ONLY:levels,n_sp_pol
 use QP_CTL_m,      ONLY:QP_ctl_t
 use QP_m,          ONLY:QP_ctl_applied
 implicit none
 type(levels)    ::E
 type(QP_ctl_t)  ::QP_ctl_from_DB,QP_ctl_add
 integer         ::band_range(2),DB_corrected(E%nb,E%nk)
 !
 ! Work Space
 !
 integer  ::ib,ik
 real(SP) ::Eoc,Eov
 character(lchlen) ::ch
 logical           ::QP_apply_done,do_E,do_W,do_Z
 !
 ! Is there anything to do ?
 !
 do_E=.false.
 do_W=.false.
 do_Z=.false.
 !
 if (any(QP_ctl_from_DB%E/=(/0.,1.,0.,1./)).or.any(QP_ctl_add%E/=(/0.,1.,0.,1./))) then
   if (QP_apply_done(band_range,(/1,E%nk/),E,'E')) return
   do_E=.true.
 endif
 if (any((/QP_ctl_from_DB%W(:)/=0/)).or.any((/QP_ctl_add%W(:)/=0/))) then
   if (QP_apply_done(band_range,(/1,E%nk/),E,'W')) return
   do_W=.true.
   if (.not.associated(E%W)) then
     allocate(E%W(E%nb,E%nk,n_sp_pol))
     call mem_est("E-W",(/size(E%W)/),(/SP/))
     E%W=0.
   endif
 endif
 if (QP_ctl_from_DB%Z/=(1.,0.).or.QP_ctl_add%Z/=(1.,0.)) then
   if (QP_apply_done(band_range,(/1,E%nk/),E,'Z')) return
   do_Z=.true.
   if (.not.associated(E%Z)) then
     allocate(E%Z(E%nb,E%nk,n_sp_pol))
     call mem_est("E-Z",(/size(E%Z)/))
     E%Z=1.
   endif
 endif
 if (.not.any((/do_E,do_W,do_Z/))) return
 !
 if (.not.QP_ctl_applied) then
   QP_ctl_applied=.true.
   call section('p','QP Corrections extrapolation')
 endif
 !
 ! Backup (only once)
 !
 if (.not.associated(E%Eo)) then
   allocate(E%Eo(E%nb,E%nk,n_sp_pol))
   call mem_est("E-Eo",(/size(E%Eo)/),(/SP/))
   E%Eo=E%E
 endif
 !
 ! Band edges
 !
 Eov=-1.
 Eoc=1.E5
 do ib=1,E%nb
   do ik=1,E%nk
     if (E%Eo(ib,ik,1)<=1.E-5) Eov=max(Eov,E%Eo(ib,ik,1))
     if (E%Eo(ib,ik,1)>1.E-5)  Eoc=min(Eoc,E%Eo(ib,ik,1))
   enddo
 enddo
 !
 if (do_E) call do_fit_operation('E',2,QP_ctl_from_DB%E,QP_ctl_add%E)
 if (do_W) call do_fit_operation('W',3,QP_ctl_from_DB%W,QP_ctl_add%W)
 if (do_Z) call do_fit_operation('Z',1,(/real(QP_ctl_from_DB%Z),aimag(QP_ctl_from_DB%Z)/),&
&                                      (/real(QP_ctl_add%Z),aimag(QP_ctl_add%Z)/))
 !
 write (ch,'(4a)') '-- ',trim(QP_ctl_add%short_descr),&
&      ' Linear/Quadratic extrapolation ',repeat('-',19)
 call msg('r',trim(ch))
 call msg('r','=== QP/DB derived ===')
 call fit_report(QP_ctl_from_DB)
 call msg('r','=== Additional ===')
 call fit_report(QP_ctl_add)
 call msg('r',repeat('-',60))
 !
 contains
   !
   subroutine do_fit_operation(what,nM,Mr,Mf)
     character(1) :: what
     integer      :: nM
     real(SP)     :: Mr(2*nM),Mf(2*nM)
     !
     ! Work Space
     !
     integer  :: i1,i2,i3,iref,ik_r
     real(SP) :: delta,rref,Ml
     logical  :: QP_apply_done
     !
     do ib=band_range(1),band_range(2)
       !
       do ik=1,E%nk
         !
         if (QP_apply_done((/ib,ib/),(/ik,ik/),E,what)) cycle
         !
         if (E%Eo(ib,ik,1)> 1.E-5) iref=0
         if (E%Eo(ib,ik,1)<=1.E-5) iref=nM
         if (E%Eo(ib,ik,1)> 1.E-5) rref=Eoc
         if (E%Eo(ib,ik,1)<=1.E-5) rref=Eov
         !
         if (what=='Z') then
           !
           E%Z(ib,ik,1)=Mf(1)+(0.,1.)*Mf(2)
           if (DB_corrected(ib,ik)/=1) then
             if (Mr(1)/=1.) E%Z(ib,ik,1)=(Mf(1)+Mr(1))/2.+(0.,1.)*(Mf(2)+Mr(2))/2.
           endif
           E%qp_done(ib,ik)=E%qp_done(ib,ik)+4
           cycle
           !
         endif
         !
         ! Calculates the polinomial expansion (delta)
         ! using Mf and Mr coefficients
         !
         delta=Mf(iref+1)
         do i3=1,nM-1
           Ml=Mf(iref+1+i3)
           if (what=='E') then
             Ml=Ml-1.
             delta=delta+Ml*(E%Eo(ib,ik,1)-rref)**i3
           endif
           if (what=='W') delta=delta+Ml*(E%Eo(ib,ik,1)-rref)**(i3+1)
         enddo
         if (DB_corrected(ib,ik)/=1) then
           delta=delta+Mr(iref+1)
           do i3=1,nM-1
             Ml=Mr(iref+1+i3)
             if (what=='E') then
               Ml=Ml-1.
               delta=delta+Ml*(E%Eo(ib,ik,1)-rref)**i3
             endif
             if (what=='W') delta=delta+Ml*(E%Eo(ib,ik,1)-rref)**(i3+1)
           enddo
         endif
         !
         if (what=='E') E%E(ib,ik,1)=E%E(ib,ik,1)+delta
         !
         if (what=='W') then
           if (E%Eo(ib,ik,1)<=1.E-5) E%W(ib,ik,1)=E%W(ib,ik,1)+abs(delta)
           if (E%Eo(ib,ik,1)> 1.E-5) E%W(ib,ik,1)=E%W(ib,ik,1)-abs(delta)
         endif
         !
         if (what=='E') E%qp_done(ib,ik)=E%qp_done(ib,ik)+1
         if (what=='W') E%qp_done(ib,ik)=E%qp_done(ib,ik)+2
         !
       enddo
     enddo
   end subroutine
   !
   subroutine fit_report(qpfit)
     type(QP_ctl_t)::qpfit
     if (qpfit%db_scissor/=0.) &
&       call msg(' r',' Band edges  DB correction [ev]:',qpfit%db_scissor*HARTREE)
     if (qpfit%fit_scissor/=0.) &
&       call msg(' r',' Additional FIT correction [ev]:',qpfit%fit_scissor*HARTREE)
     if (any(qpfit%E/=(/0.,1.,0.,1./))) then
      call msg(' r',' Energies 0th order c/v [ev]:',(/qpfit%E(1),qpfit%E(3)/)*HARTREE)
      call msg(' r',' Energies 1st order         :',(/qpfit%E(2),qpfit%E(4)/))
      if (any((/qpfit%E_err/=0./))) call msg(' r','                       Error:',qpfit%E_err)
     endif
     if (any((/qpfit%W/=0./))) then
       call msg(' r',' Widths 0th order c/v [ev]:',(/qpfit%W(1),qpfit%W(4)/)*HARTREE)
       call msg(' r',' Widths 1st order         :',(/qpfit%W(2),qpfit%W(5)/))
       call msg(' r',' Widths 2st order         :',(/qpfit%W(3),qpfit%W(6)/))
       if (any((/qpfit%W_err/=0./))) call msg(' r','                       Error:',qpfit%W_err)
     endif
     if (qpfit%Z/=(1.,0.)) &
&       call msg(' r',' Renormalization      :',(/real(qpfit%Z),aimag(qpfit%Z)/))
     ! 
   end subroutine
   !
end subroutine
