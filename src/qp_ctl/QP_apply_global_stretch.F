!
!        Copyright (C) 2000-2014 the YAMBO team
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
subroutine QP_apply_global_stretch(band_range,E,QP_ctl_from_DB,QP_ctl_add,&
&                                  DB_corrected,apply_anyway)
 !
 ! Here I apply the strecth for both QP_ctl_from_DB (respecting the 
 ! qp_done array) and QP_ctl_add (everywhere)
 !
 use pars,          ONLY:SP,lchlen
 use units,         ONLY:HA2EV,HBAR_eVfs
 use memory_m,      ONLY:mem_est
 use com,           ONLY:msg
 use electrons,     ONLY:levels,n_sp_pol
 use QP_CTL_m,      ONLY:QP_ctl_t
 use QP_m,          ONLY:QP_ctl_applied
 implicit none
 type(levels)    ::E
 type(QP_ctl_t)  ::QP_ctl_from_DB(n_sp_pol),QP_ctl_add(n_sp_pol)
 integer         ::band_range(2),DB_corrected(E%nb,E%nk,n_sp_pol)
 logical         ::apply_anyway
 !
 ! Work Space
 !
 integer  ::ib,ik,i_sp,n_of_coeff,i_spin
 real(SP) ::Eoc,Eov,M_factors_base(6),M_factors_free(6)
 character(lchlen) ::ch
 logical           ::QP_check_if_corrected,do_E,do_W,do_Z
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
   !
   if (any(QP_ctl_from_DB(i_sp)%E/=(/0.,1.,0.,1./)).or.any(QP_ctl_add(i_sp)%E/=(/0.,1.,0.,1./))) then
     if (.not.apply_anyway) then
       if (QP_check_if_corrected(band_range,(/1,E%nk/),(/i_sp,i_sp/),E,'E')) return
     endif
     do_E=.true.
   endif
   if (any((/QP_ctl_from_DB(i_sp)%W(:)/=0/)).or.any((/QP_ctl_add(i_sp)%W(:)/=0/))) then
     if (.not.apply_anyway) then
       if (QP_check_if_corrected(band_range,(/1,E%nk/),(/i_sp,i_sp/),E,'W')) return
     endif
     do_W=.true.
     if (.not.associated(E%W)) then
       allocate(E%W(E%nb,E%nk,n_sp_pol))
       call mem_est("E-W",(/size(E%W)/),(/SP/))
       E%W=0.
     endif
   endif
   if (QP_ctl_from_DB(i_sp)%Z/=(1.,0.).or.QP_ctl_add(i_sp)%Z/=(1.,0.)) then
     if (.not.apply_anyway) then
       if (QP_check_if_corrected(band_range,(/1,E%nk/),(/i_sp,i_sp/),E,'Z')) return
     endif
     do_Z=.true.
     if (.not.associated(E%Z)) then
       allocate(E%Z(E%nb,E%nk,n_sp_pol))
       call mem_est("E-Z",(/size(E%Z)/))
       E%Z=1.
     endif
   endif
   if (.not.any((/do_E,do_W,do_Z/))) return
   !
   if (.not.QP_ctl_applied.and..not.apply_anyway) QP_ctl_applied=.true.
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
       if (E%Eo(ib,ik,i_sp)<=1.E-5) Eov=max(Eov,E%Eo(ib,ik,i_sp))
       if (E%Eo(ib,ik,i_sp)>1.E-5)  Eoc=min(Eoc,E%Eo(ib,ik,i_sp))
     enddo
   enddo
   !
   i_spin=i_sp
   if (do_E) then
     n_of_coeff=2
     M_factors_base(:4)=QP_ctl_from_DB(i_sp)%E
     M_factors_free(:4)=QP_ctl_add(i_sp)%E
     call do_fit_operation('E')
   endif
   if (do_W) then
     n_of_coeff=3
     M_factors_base(:6)=QP_ctl_from_DB(i_sp)%W
     M_factors_free(:6)=QP_ctl_add(i_sp)%W
     call do_fit_operation('W')
   endif
   if (do_Z) then
     n_of_coeff=1
     M_factors_base(:2)=(/real(QP_ctl_from_DB(i_sp)%Z),aimag(QP_ctl_from_DB(i_sp)%Z)/)
     M_factors_free(:2)=(/real(QP_ctl_add(i_sp)%Z),aimag(QP_ctl_add(i_sp)%Z)/)
     call do_fit_operation('Z')
   endif
   !
   write (ch,'(4a)') '-- ',trim(QP_ctl_add(i_sp)%short_descr),&
&        ' Linear/Quadratic extrapolation ',repeat('-',19)
   call msg('r',trim(ch))
   call msg('r','=== QP/DB derived ===')
   call fit_report(QP_ctl_from_DB(i_sp))
   call msg('r','=== Additional ===')
   call fit_report(QP_ctl_add(i_sp))
   call msg('r',repeat('-',60))
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
     integer  :: i1,i2,i3,iref,ik_r
     real(SP) :: delta,rref,M_factor_local
     logical  :: QP_check_if_corrected
     !
     do ib=band_range(1),band_range(2)
       !
       do ik=1,E%nk
         !
         if (.not.apply_anyway) then
           if (QP_check_if_corrected((/ib,ib/),(/ik,ik/),(/i_spin,i_spin/),E,what)) cycle
         endif
         !
         if (E%Eo(ib,ik,i_spin)> 1.E-5) iref=0
         if (E%Eo(ib,ik,i_spin)<=1.E-5) iref=n_of_coeff
         if (what=="W") then
           if (E%Eo(ib,ik,i_spin)> 1.E-5) rref=QP_ctl_add(i_sp)%W_E_ref(1)
           if (E%Eo(ib,ik,i_spin)<=1.E-5) rref=QP_ctl_add(i_sp)%W_E_ref(2)
         else
           if (E%Eo(ib,ik,i_spin)> 1.E-5) rref=Eoc
           if (E%Eo(ib,ik,i_spin)<=1.E-5) rref=Eov
         endif
         !
         if (what=='Z') then
           !
           E%Z(ib,ik,i_spin)=M_factors_free(1)+(0.,1.)*M_factors_free(2)
           if (DB_corrected(ib,ik,i_spin)/=1) then
             if (M_factors_base(1)/=1.) E%Z(ib,ik,i_spin)=(M_factors_free(1)+M_factors_base(1))/2.+&
&                                               (0.,1.)*(M_factors_free(2)+M_factors_base(2))/2.
           endif
           if (.not.apply_anyway) E%QP_corrected(ib,ik,i_spin)=E%QP_corrected(ib,ik,i_spin)+4
           cycle
           !
         endif
         !
         ! Calculates the polinomial expansion (delta)
         ! using M_factors_free and M_factors_base coefficients
         !
         delta=M_factors_free(iref+1)
         !
         do i3=1,n_of_coeff-1
           M_factor_local=M_factors_free(iref+1+i3)
           if (what=='E') M_factor_local=M_factor_local-1.
           if (what=='W'.and.E%Eo(ib,ik,i_spin)<rref.and.E%Eo(ib,ik,i_spin)> 1.E-5) cycle
           if (what=='W'.and.E%Eo(ib,ik,i_spin)>rref.and.E%Eo(ib,ik,i_spin)<=1.E-5) cycle
           delta=delta+M_factor_local*(E%Eo(ib,ik,i_spin)-rref)**i3
         enddo
         if (DB_corrected(ib,ik,i_spin)/=1.or.apply_anyway) then
           delta=delta+M_factors_base(iref+1)
           do i3=1,n_of_coeff-1
             M_factor_local=M_factors_base(iref+1+i3)
             if (what=='E') M_factor_local=M_factor_local-1.
             if (what=='W'.and.E%Eo(ib,ik,i_spin)<rref.and.E%Eo(ib,ik,i_spin)> 1.E-5) cycle
             if (what=='W'.and.E%Eo(ib,ik,i_spin)>rref.and.E%Eo(ib,ik,i_spin)<=1.E-5) cycle
             delta=delta+M_factor_local*(E%Eo(ib,ik,i_spin)-rref)**i3
           enddo
         endif
         !
         if (what=='E') E%E(ib,ik,i_spin)=E%E(ib,ik,i_spin)+delta
         !
         if (what=='W') then
           if (E%Eo(ib,ik,i_spin)<=1.E-5) E%W(ib,ik,i_spin)=E%W(ib,ik,i_spin)+abs(delta)
           if (E%Eo(ib,ik,i_spin)> 1.E-5) E%W(ib,ik,i_spin)=E%W(ib,ik,i_spin)-abs(delta)
         endif
         !
         if (.not.apply_anyway) then
           if (what=='E') E%QP_corrected(ib,ik,i_spin)=E%QP_corrected(ib,ik,i_spin)+1
           if (what=='W') E%QP_corrected(ib,ik,i_spin)=E%QP_corrected(ib,ik,i_spin)+2
         endif
         !
       enddo
     enddo
   end subroutine
   !
   subroutine fit_report(qpfit)
     type(QP_ctl_t)::qpfit
     if (qpfit%db_scissor/=0.) &
&       call msg(' r',' Gap correction (database) [ev]:',qpfit%db_scissor*HA2EV)
     if (qpfit%fit_scissor/=0.) &
&       call msg(' r','                     (FIT) [ev]:',qpfit%fit_scissor*HA2EV)
     if (any(qpfit%E/=(/0.,1.,0.,1./))) then
      call msg(' r',' Energies 0th order c/v [ev]:',(/qpfit%E(1),qpfit%E(3)/)*HA2EV)
      call msg(' r',' Energies 1st order         :',(/qpfit%E(2),qpfit%E(4)/))
      if (any((/qpfit%E_err/=0./))) call msg(' r','                       Error:',qpfit%E_err)
     endif
     if (any((/qpfit%W/=0./))) then
       call msg(' r',' Widths 0th order c/v [ev]:',(/qpfit%W(1),qpfit%W(4)/)*HA2EV)
       call msg(' r','                      [fs]:',(/1./qpfit%W(1),1./qpfit%W(4)/)*HBAR_eVfs/HA2EV)
       call msg(' r',' Widths 1st order         :',(/qpfit%W(2),qpfit%W(5)/))
       call msg(' r',' Widths 2st order         :',(/qpfit%W(3),qpfit%W(6)/)/HA2EV)
       if (any((/qpfit%W_err/=0./))) call msg(' r','                       Error:',qpfit%W_err)
     endif
     if (qpfit%Z/=(1.,0.)) &
&       call msg(' r',' Renormalization      :',(/real(qpfit%Z),aimag(qpfit%Z)/))
     ! 
   end subroutine
   !
end subroutine
