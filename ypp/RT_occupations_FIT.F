! 
! Copyright (C) 2000-2013 D. Sangalli and the YAMBO team
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
subroutine RT_occupations_FIT(k,E,RT_occ_distribution,time)
 ! 
 ! FIT the occupations with a Gauss–Newton algorithm
 !
 use units,          ONLY:HA2EV,HA2KEL,FS2AUT
 use YPP,            ONLY:Fermi_fit_points,RT_n_report
 use pars,           ONLY:SP,lchlen
 use SC,             ONLY:SC_bands,n_SC_descriptions,SC_description
 use QP_m,           ONLY:QP_table,QP_n_states,QP_state
 use R_lattice,      ONLY:bz_samp
 use D_lattice,      ONLY:Tel
 use electrons,      ONLY:levels,spin,spin_occ,n_spin
 use vec_operate,    ONLY:sort,degeneration_finder
 use functions,      ONLY:Fermi_fnc,Fermi_fnc_derivative,Fermi_fnc_T_derivative
 use com,            ONLY:msg,of_open_close,error
 use stderr,         ONLY:intc
 !
 implicit none
 !
 type(bz_samp) :: k
 type(levels)  :: E
 !
 real(SP)              :: RT_occ_distribution(QP_n_states,RT_n_report)
 real(SP)              :: time(RT_n_report)
 !
 integer, parameter    :: max_iter=1000,fit_dim=1000,nmax_file_report=16
 real(SP)              :: RT_occ_fitted(fit_dim,RT_n_report,2),Ei(fit_dim,2)
 !
 ! Work Space
 !
 logical               :: l_two_temperatures
 !
 real(SP)              :: x(QP_n_states),Jacob(QP_n_states,2)
 real(SP)              :: Avg_error(RT_n_report,2)
 real(SP)              :: Jacob_sq(2,2),Jacob_sq_m1(2,2),Fit_new(2)
 real(SP)              :: T_guess(2),T_fit(RT_n_report,2)
 real(SP)              :: Ef_guess(2),Ef_fit(RT_n_report,2)
 real(SP)              :: delta_E,E_shifted,E_start
 ! 
 integer  :: ifit,i_s,ik,ib,i1,i2,n1,n2,i_fp,i_spin
 integer  :: nfit,last_occupied,nfound(2)
 integer  :: E_sorted_index(QP_n_states)
 real(SP) :: E_sorted(QP_n_states)
 !
 character(lchlen) ::output_fname
 !
 if(n_spin>1) call error('Occupations FIT not implemented for n_spin>1')
 !
 l_two_temperatures=.true.
 !
 do i_s=1,QP_n_states
   ib    =QP_table(i_s,1)
   ik    =QP_table(i_s,3)
   i_spin=spin(QP_table(i_s,:))
   !
   E_sorted(i_s)=E%E(ib,ik,i_spin)
 enddo
 !
 call sort(E_sorted,indx=E_sorted_index)
 !
 do i_s=1,QP_n_states
   if(E_sorted(i_s)>0._SP) exit
   last_occupied=i_s
 enddo
 !
 nfit=1
 if(l_two_temperatures) nfit=2
 !
 do ifit=1,nfit
   if(.not.l_two_temperatures)        delta_E=(Fermi_fit_points(2)-Fermi_fit_points(1))
   if(ifit==1.and.l_two_temperatures) delta_E=(1._SP-Fermi_fit_points(1))
   if(ifit==2.and.l_two_temperatures) delta_E=(Fermi_fit_points(2)+1._SP)
   if(ifit==1) E_start=Fermi_fit_points(1)/HA2EV
   if(ifit==2) E_start=-1._SP/HA2EV
   delta_E=delta_E/real(fit_dim,SP)/HA2EV
   do i1=1,fit_dim
     Ei(i1,ifit)=E_start+delta_E*real(i1-1,SP)
   enddo
 enddo
 !
 T_guess=max(Tel,1.E-2)
 Ef_guess=(E_sorted(last_occupied+1)-E_sorted(last_occupied))
 if(l_two_temperatures) Ef_guess(2)=-Ef_guess(1)
 !
 T_fit=0._SP
 Ef_fit=0._SP
 RT_occ_fitted=0._SP
 nfound=0._SP
 !
 do i1=1,RT_n_report
   !
   ! Sort also RT_occupations
   x(:)=RT_occ_distribution(E_sorted_index(:),i1)
   RT_occ_distribution(:,i1)=x(:)
   !
   do ifit=1,nfit
     !
     do i2=1,max_iter
       Avg_error(i1,ifit)=0._SP
       x=0._SP
       Jacob=0._SP
       do i_s=1,QP_n_states
         if(i_s<=last_occupied.and.ifit==2.and.l_two_temperatures) cycle
         if(i_s> last_occupied.and.ifit==1.and.l_two_temperatures) cycle
         E_shifted=E_sorted(i_s)-Ef_guess(ifit)
         x(i_s)=RT_occ_distribution(i_s,i1)-Fermi_fnc(E_shifted,T_guess(ifit))*spin_occ
         Jacob(i_s,1)=-Fermi_fnc_T_derivative(E_shifted,T_guess(ifit))*spin_occ
         Jacob(i_s,2)=-Fermi_fnc_derivative(E_shifted,T_guess(ifit))*spin_occ
         Avg_error(i1,ifit)=Avg_error(i1,ifit)+x(i_s)**2._SP
       enddo
       !
       Avg_error(i1,ifit)=sqrt(Avg_error(i1,ifit))/real(QP_n_states,SP)
       !
       Jacob_sq=matmul(transpose(Jacob),Jacob)
       !
       Jacob_sq_m1(:,:)=1._SP/(Jacob_sq(1,1)*Jacob_sq(2,2)-Jacob_sq(1,2)*Jacob_sq(2,1))
       Jacob_sq_m1(1,1)= Jacob_sq_m1(1,1)*Jacob_sq(2,2)
       Jacob_sq_m1(2,2)= Jacob_sq_m1(2,2)*Jacob_sq(1,1)
       Jacob_sq_m1(1,2)=-Jacob_sq_m1(1,2)*Jacob_sq(1,2)
       Jacob_sq_m1(2,1)=-Jacob_sq_m1(2,1)*Jacob_sq(2,1)
       !
       Fit_new=matmul(Jacob_sq_m1,matmul(transpose(Jacob),x))
       !
       if(Fit_new(1)==Fit_new(1)+1) goto 1000
       !
       T_fit(i1,ifit)=T_guess(ifit)-Fit_new(1)
       Ef_fit(i1,ifit)=Ef_guess(ifit)-Fit_new(2)
       !
       if(abs(T_guess(ifit)-T_fit(i1,ifit))<1.E-6 .and. abs(Ef_guess(ifit)-Ef_fit(i1,ifit))<1.E-3 ) then
         nfound(ifit)=nfound(ifit)+1
         exit
       endif
       !
       T_guess=T_fit(i1,ifit)
       Ef_guess(ifit)=Ef_fit(i1,ifit)
       !
     enddo   
     !
     if(i2==max_iter+1) then
1000   continue
       T_fit(i1,ifit) =-1000._SP
       T_guess(ifit)=max(Tel,1.E-2)
       Ef_fit(i1,ifit)=-1000._SP
       Ef_guess(ifit)= 1._SP*(minval(E%E(E%nbf+1,:,:))-maxval(E%E(E%nbf,:,:)))
       Ef_guess(ifit)=-1._SP*(minval(E%E(E%nbf+1,:,:))-maxval(E%E(E%nbf,:,:)))
     endif     
     !
     if(i2<=max_iter) then
       do i2=1,fit_dim
         RT_occ_fitted(i2,i1,ifit)=Fermi_fnc(Ei(i2,ifit)-Ef_fit(i1,ifit),T_fit(i1,ifit))*spin_occ
       enddo
     endif
     !
   enddo
   !      
 enddo
 !
 if(l_two_temperatures) then
   call msg('s',":: Elec. fitting Temperatures found " &
&            //trim(intc(nfound(1)))//"/"//trim(intc(RT_n_report)))
   call msg('s',":: Holes fitting Temperatures found " &
&            //trim(intc(nfound(2)))//"/"//trim(intc(RT_n_report)))
 else
   call msg('s',":: Fitting Temperatures found "//trim(intc(nfound(1)))//"/"//trim(intc(RT_n_report)))
 endif
 !
 do i2=1,RT_n_report,nmax_file_report
   n1=i2
   n2=min(RT_n_report,i2+15)
   !
   output_fname='YPP-RT_occupation_distribution_DATA'
   if(RT_n_report>nmax_file_report) &
&    output_fname=trim(output_fname)//'_grp'//trim(intc(i2/nmax_file_report+1))
   !
   call of_open_close(trim(output_fname),'ot')
   !
   do i_fp=1,n_SC_descriptions
     call msg('o '//trim(output_fname),'#',trim(SC_description(i_fp)),INDENT=0)
   enddo
   !
   call msg('o '//trim(output_fname),'#')
   !
   do i_s=1,QP_n_states
     !
     ib    =QP_table(i_s,1)
     ik    =QP_table(i_s,3)
     i_spin=spin(QP_table(i_s,:))
     !
     if (ib<SC_bands(1)) cycle
     if (ib>SC_bands(2)) cycle
     if (ik>k%nibz) cycle
     !
     call msg('o '//trim(output_fname),'',(/E_sorted(i_s)*HA2EV,&
  &            RT_occ_distribution(i_s,n1:n2)/),INDENT=-2,USE_TABS=.true.)
     !
   enddo
   !
   call of_open_close(trim(output_fname))
   !
   do ifit=1,nfit
     !
     output_fname='YPP-RT_occupation_distribution_FIT'
     if(l_two_temperatures.and.ifit==1) output_fname=trim(output_fname)//'h'
     if(l_two_temperatures.and.ifit==2) output_fname=trim(output_fname)//'e'
     if(RT_n_report>nmax_file_report) &
&      output_fname=trim(output_fname)//'_grp'//trim(intc(i2/nmax_file_report+1))
     !
     call of_open_close(trim(output_fname),'ot')
     !
     do i_fp=1,n_SC_descriptions
       call msg('o '//trim(output_fname),'#',trim(SC_description(i_fp)),INDENT=0)
     enddo
     !
     call msg('o '//trim(output_fname),'# Result of the FIT: time (fs), T_fit [K], Ef_fit [eV], Avg_error')
     call msg('o '//trim(output_fname),'#',(/0._SP, time(n1:n2)/FS2AUT /),INDENT=-2,USE_TABS=.true.)
     call msg('o '//trim(output_fname),'#',(/0._SP, T_fit(n1:n2,ifit)*HA2KEL/),INDENT=-2,USE_TABS=.true.)
     call msg('o '//trim(output_fname),'#',(/0._SP, Ef_fit(n1:n2,ifit)*HA2EV/),INDENT=-2,USE_TABS=.true.)
     call msg('o '//trim(output_fname),'#',(/0._SP, Avg_error(n1:n2,ifit)/)   ,INDENT=-2,USE_TABS=.true.)
     call msg('o '//trim(output_fname),'#')
     !
     do i1=1,fit_dim
       call msg('o '//trim(output_fname),'',(/Ei(i1,ifit)*HA2EV,&
  &               RT_occ_fitted(i1,n1:n2,ifit)/),INDENT=-2,USE_TABS=.true.)
     enddo
     !
     call of_open_close(trim(output_fname))
     !
   enddo
 enddo
 !
end subroutine RT_occupations_FIT
