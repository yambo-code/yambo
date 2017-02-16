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
subroutine QP_apply_global_stretch(band_range,E,k,QP_ctl_from_DB,QP_ctl_add,DB_corrected,apply_anyway)
 !
 ! Here I apply the strecth for both QP_ctl_from_DB (respecting the qp_done array) and QP_ctl_add (everywhere)
 !
 ! When apply_anyway=.TRUE. the qp_done array is not used and both corrections are applied anywhere.
 !
 use pars,          ONLY:SP,lchlen,rZERO,cI,cONE
 use units,         ONLY:HA2EV,HBAR_eVfs
 use com,           ONLY:msg
 use electrons,     ONLY:levels,n_sp_pol,n_spin,n_spinor
 use R_lattice,     ONLY:bz_samp
 use QP_CTL_m,      ONLY:QP_ctl_t
 use QP_m,          ONLY:QP_ctl_applied
 use interfaces,    ONLY:el_density_of_states
 !
#include<memory.h>
 !
 type(levels)    ::E
 type(bz_samp)   ::k
 type(QP_ctl_t)  ::QP_ctl_from_DB(n_sp_pol),QP_ctl_add(n_sp_pol)
 integer         ::band_range(2),DB_corrected(E%nb,E%nk,n_sp_pol)
 logical         ::apply_anyway
 !
 ! Work Space
 !
 integer  ::ib,ik,i_sp,n_of_coeff,i_spin
 real(SP) ::M_factors_DBs(6),M_factors_CTL(6),dos_factor(n_spin)
 character(lchlen) :: ch
 logical           :: do_E_DBs,do_E_CTL,do_W_DBs,do_W_CTL,do_Z_DBs,do_Z_CTL
 logical, external :: QP_check_if_corrected
 !
 ! Loop on spins
 !
 do i_sp= 1,n_sp_pol
   !
   ! Is there anything to do ?
   !
   do_E_DBs=.false.
   do_E_CTL=.false.
   do_W_DBs=.false.
   do_W_CTL=.false.
   do_Z_DBs=.false.
   do_Z_CTL=.false.
   M_factors_DBs=rZERO
   M_factors_CTL=rZERO
   !
   if (any(QP_ctl_from_DB(i_sp)%E/=(/0.,1.,0.,1./)).or.any(QP_ctl_add(i_sp)%E/=(/0.,1.,0.,1./))) then
     if (.not.apply_anyway) then
       do_E_DBs=.not.QP_check_if_corrected(band_range,(/1,E%nk/),(/i_sp,i_sp/),E,'E')
     endif
     do_E_CTL=.true.
   endif
   if (any((/QP_ctl_from_DB(i_sp)%W(:)/=0/)).or.any((/QP_ctl_add(i_sp)%W(:)/=0/))) then
     if (.not.apply_anyway) then
       do_W_DBs=.not.QP_check_if_corrected(band_range,(/1,E%nk/),(/i_sp,i_sp/),E,'W')
     endif
     do_W_CTL=.true.
     if (.not.allocated(E%W)) then
       YAMBO_ALLOC(E%W,(E%nb,E%nk,n_sp_pol))
       E%W=rZERO
     endif
   endif
   if (QP_ctl_from_DB(i_sp)%Z/=cONE.or.QP_ctl_add(i_sp)%Z/=cONE) then
     if (.not.apply_anyway) then
       do_Z_DBs=.not.QP_check_if_corrected(band_range,(/1,E%nk/),(/i_sp,i_sp/),E,'Z')
     endif
     do_Z_CTL=.true.
     if (.not.allocated(E%Z)) then
       YAMBO_ALLOC(E%Z,(E%nb,E%nk,n_sp_pol))
       E%Z=1._SP
     endif
   endif
   if (.not.any((/do_E_DBs,do_E_CTL,do_W_DBs,do_W_CTL,do_Z_DBs,do_Z_CTL/))) return
   !
   if (.not.QP_ctl_applied.and..not.apply_anyway) QP_ctl_applied=.true.
   !
   ! Backup (only once)
   !
   if (.not.allocated(E%Eo)) then
     YAMBO_ALLOC(E%Eo,(E%nb,E%nk,n_sp_pol))
     E%Eo=E%E
   endif
   !
   i_spin=i_sp
   if (do_E_DBs.or.do_E_CTL) then
     n_of_coeff=2
     if (do_E_DBs) M_factors_DBs(:4)=QP_ctl_from_DB(i_sp)%E
     M_factors_CTL(:4)=QP_ctl_add(i_sp)%E
     call do_fit_operation('E')
   endif
   if (do_W_DBs.or.do_W_CTL) then
     n_of_coeff=3
     if (do_W_DBs) M_factors_DBs(:6)=QP_ctl_from_DB(i_sp)%W
     M_factors_CTL(:6)=QP_ctl_add(i_sp)%W
     call do_fit_operation('W')
   endif
   if (do_Z_DBs.or.do_Z_CTL) then
     n_of_coeff=1
     if (do_Z_DBs) M_factors_DBs(:2)=(/real(QP_ctl_from_DB(i_sp)%Z),aimag(QP_ctl_from_DB(i_sp)%Z)/)
     M_factors_CTL(:2)=(/real(QP_ctl_add(i_sp)%Z),aimag(QP_ctl_add(i_sp)%Z)/)
     call do_fit_operation('Z')
   endif
   !
   !
   write (ch,'(4a)') '-- ',trim(QP_ctl_add(i_sp)%short_descr),&
&          ' DOS/Linear/Quadratic extrapolation ',repeat('-',19)
   call msg('r',trim(ch))
   call msg('r','=== QP/DB derived ===')
   call fit_report(QP_ctl_from_DB(i_sp),'fit')
   call msg('r','=== Additional ===')
   call fit_report(QP_ctl_add(i_sp),'add')
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
         l_add_DB_corr = (DB_corrected(ib,ik,i_spin)/=1).or.apply_anyway
         !
         ! Energy
         !========
         if (what=='E') then
           ! Shissor correction
           delta=M_factors_CTL(iref+1)
           if(l_add_DB_corr) delta=delta+M_factors_DBs(iref+1)
           ! Stretching correction
           if (E%Eo(ib,ik,i_spin)> 1.E-5_SP) rref=E%E_CBm
           if (E%Eo(ib,ik,i_spin)<=1.E-5_SP) rref=E%E_VBM
           M_factor_local=(M_factors_CTL(iref+2)-1._SP)
           if(l_add_DB_corr) M_factor_local=M_factor_local+(M_factors_DBs(iref+2)-1._SP)
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
           delta=M_factors_CTL(iref+1)
           ! DOS proportional correction
           call el_density_of_states(k,E,E%E(ib,ik,i_spin),0.05_SP/HA2EV,(/1,E%nb/),dos_factor,.false.)
           M_factor_local=M_factors_CTL(iref+2)
           if(l_add_DB_corr) M_factor_local=M_factor_local+M_factors_DBs(iref+2)
           delta=delta+M_factor_local*dos_factor(i_spin)*real(n_spinor,SP)
           ! Quadratic correction
           if (E%Eo(ib,ik,i_spin)> 1.E-5_SP) rref=QP_ctl_add(i_sp)%W_E_ref(1)
           if (E%Eo(ib,ik,i_spin)<=1.E-5_SP) rref=QP_ctl_add(i_sp)%W_E_ref(2)
           if (E%Eo(ib,ik,i_spin)> max(1.E-5_SP,rref) .or. E%Eo(ib,ik,i_spin)<min(1.E-5_SP,rref)) &
&            delta=delta+M_factors_CTL(iref+3)*(E%Eo(ib,ik,i_spin)-rref)**2
           if (l_add_DB_corr) then
             if (E%Eo(ib,ik,i_spin)> 1.E-5_SP) rref=E%E_CBm+E%E_ind_gap(i_spin)
             if (E%Eo(ib,ik,i_spin)<=1.E-5_SP) rref=E%E_VBM+E%E_ind_gap(i_spin)
             if (E%Eo(ib,ik,i_spin)> max(1.E-5_SP,rref) .or. E%Eo(ib,ik,i_spin)<min(1.E-5_SP,rref)) &
&              delta=delta+M_factors_DBs(iref+3)*(E%Eo(ib,ik,i_spin)-rref)**2
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
           Z_tmp=M_factors_CTL(1)+cI*M_factors_CTL(2)
           if ( DB_corrected(ib,ik,i_spin)/=1 .and. M_factors_DBs(1)/=1._SP ) then
             Z_tmp=( Z_tmp+ (M_factors_DBs(1)+cI*M_factors_DBs(2)) )/2._SP
           endif
           !
           E%Z(ib,ik,i_spin)=Z_tmp
           !
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
   subroutine fit_report(qpfit,what)
     type(QP_ctl_t)::qpfit
     character(3)  ::what
     if (qpfit%db_scissor/=0._SP) &
&      call msg(' r',' Gap correction (database) [ev]:',qpfit%db_scissor*HA2EV)
     if (qpfit%fit_scissor/=0._SP) &
&      call msg(' r','                     (FIT) [ev]:',qpfit%fit_scissor*HA2EV)
     if (any(qpfit%E/=(/0._SP,1._SP,0._SP,1._SP/))) then
       call msg(' r',' Energies 0th order c/v [ev]:',(/qpfit%E(1),qpfit%E(3)/)*HA2EV)
       call msg(' r',' Energies 1st order         :',(/qpfit%E(2),qpfit%E(4)/))
       if (any((/qpfit%E_err/=0._SP/))) call msg(' r','                       Error:',qpfit%E_err)
     endif
     if (any((/qpfit%W/=0._SP/))) then
       if(what=="fit") call msg(' r',' Widths 0th DB derived fit is done but not used')
       call msg(' r',' Widths 0th order c/v [ev]:',(/      qpfit%W(1),      qpfit%W(4)/)          *HA2EV)
       call msg(' r','                      [fs]:',(/1._SP/qpfit%W(1),1._SP/qpfit%W(4)/)*HBAR_eVfs/HA2EV)
       if (any((/qpfit%W_err(1:2)/=0._SP/))) call msg(' r','                     Error:',qpfit%W_err(1:2)*HA2EV)
       call msg(' r',' Widths dos prop.     [eV]:',(/      qpfit%W(2),      qpfit%W(5)/)          *HA2EV)
       if (any((/qpfit%W_err(3:4)/=0._SP/))) call msg(' r','                     Error:',qpfit%W_err(3:4)*HA2EV)
       call msg(' r',' Widths 2st order   [1/eV]:',(/      qpfit%W(3),      qpfit%W(6)/)          /HA2EV)
       if (any((/qpfit%W_err(5:6)/=0._SP/))) call msg(' r','                     Error:',qpfit%W_err(5:6)/HA2EV)
     endif
     if (qpfit%Z/=cONE) &
&      call msg(' r',' Renormalization      :',(/real(qpfit%Z),aimag(qpfit%Z)/))
     !
   end subroutine
   !
end subroutine
