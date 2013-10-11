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
subroutine RT_occupations_FIT(E,RT_occ_DATA,time)
 ! 
 ! FIT the occupations with a Gauss–Newton algorithm
 !
 use units,          ONLY:HA2EV,HA2KEL,FS2AUT
 use YPP,            ONLY:RT_n_FIT
 use pars,           ONLY:SP,lchlen
 use SC,             ONLY:SC_bands,n_SC_descriptions,SC_description, &
&                         RT_nk,RT_nstates,RT_ibz_coo
 use QP_m,           ONLY:QP_table,QP_n_states
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels,spin,spin_occ,n_spin
 use vec_operate,    ONLY:sort
 use functions,      ONLY:Fermi_fnc,Fermi_fnc_derivative,Fermi_fnc_T_derivative
 use com,            ONLY:msg,of_open_close,error
 use stderr,         ONLY:intc,real2ch
 !
 implicit none
 !
 type(levels)  :: E
 !
 real(SP)              :: RT_occ_DATA(RT_nstates,RT_n_FIT)
 real(SP)              :: time(RT_n_FIT)
 !
 integer, parameter    :: max_iter=10000,fit_dim=1000,nmax_file_report=16
 real(SP)              :: RT_occ_fitted(fit_dim,RT_n_FIT,2),Ei(fit_dim,2),Fit_E_range(2)
 !
 ! Work Space
 !
 logical               :: l_two_temperatures,lfound(2)
 !
 real(SP)              :: x(RT_nstates),Jacob(RT_nstates,2)
 real(SP)              :: Avg_error(RT_n_FIT,2)
 real(SP)              :: Jacob_sq(2,2),Jacob_sq_m1(2,2),Fit_new(2)
 real(SP)              :: T_guess(2),T_fit(RT_n_FIT,2)
 real(SP)              :: Ef_guess(2),Ef_fit(RT_n_FIT,2)
 real(SP)              :: delta_E,E_shifted,E_start,E_shift
 ! 
 integer  :: ifit,i_s,i_RT,ik,ik_rim,ib,i1,i2,n1,n2,i_fp,i_spin
 integer  :: ntry,nfit,last_occupied,nfound(2)
 integer  :: E_sorted_index(RT_nstates)
 real(SP) :: E_sorted(RT_nstates)
 !
 character(lchlen) ::output_fname
 character(10)     ::titles(RT_n_FIT+1),data_ch(RT_n_FIT+1,4,2)
 !
 if(n_spin>1) call error('Occupations FIT not implemented for n_spin>1')
 !
 l_two_temperatures=.true.
 !
 i_RT=0
 do i_s=1,QP_n_states
   ib    =QP_table(i_s,1)
   ik    =QP_table(i_s,3)
   i_spin=spin(QP_table(i_s,:))
   !
   do ik_rim=RT_ibz_coo(ik,1),RT_ibz_coo(ik,2)
     !
     if (ib<SC_bands(1)) cycle
     if (ib>SC_bands(2)) cycle
     if (ik_rim>RT_nk) cycle
     i_RT=i_RT+1
     !
     E_sorted(i_RT)=E%E(ib,ik_rim,i_spin)
     !
   enddo
   !
 enddo
 !
 call sort(E_sorted,indx=E_sorted_index)
 !
 do i_RT=1,RT_nstates
   if(E_sorted(i_RT)>0._SP) exit
   last_occupied=i_RT
 enddo
 !
 nfit=1
 if(l_two_temperatures) nfit=2
 !
 E_shift=0.0_SP !1._SP/HA2EV
 Fit_E_range(1)=E_sorted(1)          -E_shift
 Fit_E_range(2)=E_sorted(RT_nstates) +E_shift
 !
 do ifit=1,nfit
   if(.not.l_two_temperatures)        delta_E=(Fit_E_range(2)-Fit_E_range(1))
   if(ifit==1.and.l_two_temperatures) delta_E=(E_shift-Fit_E_range(1))
   if(ifit==2.and.l_two_temperatures) delta_E=(Fit_E_range(2)+E_shift)
   if(ifit==1) E_start=Fit_E_range(1)
   if(ifit==2) E_start=-E_shift
   delta_E=delta_E/real(fit_dim,SP)
   do i1=1,fit_dim
     Ei(i1,ifit)=E_start+delta_E*real(i1-1,SP)
   enddo
 enddo
 !
 T_guess=1.E-3
 Ef_guess=E_sorted(last_occupied+1)
 if(l_two_temperatures) Ef_guess(2)=E_sorted(last_occupied)
 !
 T_fit=0._SP
 Ef_fit=0._SP
 RT_occ_fitted=0._SP
 nfound=0
 ntry=0
 !
 do i1=1,RT_n_FIT
   !
   ! Sort also RT_occupations
   x(:)=RT_occ_DATA(E_sorted_index(:),i1)
   RT_occ_DATA(:,i1)=x(:)
   lfound=.false.
   lfound=.false.
   !
   do ifit=1,nfit
     !
2000 ntry=ntry+1
     if(ntry>1) then
       T_guess(ifit)=1.E-3*10**real(ntry-1,SP)
       if(ifit==1) Ef_guess(ifit)=E_sorted(last_occupied+1)
       if(ifit==2) Ef_guess(ifit)=E_sorted(last_occupied)
     endif
     !
     do i2=1,max_iter
       Avg_error(i1,ifit)=0._SP
       x=0._SP
       Jacob=0._SP
       i_RT=0
       do i_s=1,QP_n_states
         ib= QP_table(i_s,1)
         ik= QP_table(i_s,3)
         !
         do ik_rim=RT_ibz_coo(ik,1),RT_ibz_coo(ik,2)
           !
           if (ib<SC_bands(1)) cycle
           if (ib>SC_bands(2)) cycle
           if (ik_rim>RT_nk) cycle
           !
           i_RT=i_RT+1
           !
           if(i_RT<=last_occupied.and.ifit==2.and.l_two_temperatures) cycle
           if(i_RT> last_occupied.and.ifit==1.and.l_two_temperatures) cycle
           E_shifted=E_sorted(i_RT)-Ef_guess(ifit)
           x(i_RT)=RT_occ_DATA(i_RT,i1)-Fermi_fnc(E_shifted,T_guess(ifit))*spin_occ
           Jacob(i_RT,1)=-Fermi_fnc_T_derivative(E_shifted,T_guess(ifit))*spin_occ
           Jacob(i_RT,2)=-Fermi_fnc_derivative(E_shifted,T_guess(ifit))*spin_occ
           Avg_error(i1,ifit)=Avg_error(i1,ifit)+x(i_RT)**2._SP
         enddo
         !
       enddo
       !
       Avg_error(i1,ifit)=sqrt(Avg_error(i1,ifit))/real(RT_nstates,SP)
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
       if(any(isnan(Fit_new(:)))) exit
       if(any(Fit_new(:)==Fit_new(:)+1._SP)) exit
       !
       T_fit(i1,ifit)=T_guess(ifit)-Fit_new(1)
       Ef_fit(i1,ifit)=Ef_guess(ifit)-Fit_new(2)
       !
       if(abs(T_guess(ifit)-T_fit(i1,ifit))<1.E-6 .and. abs(Ef_guess(ifit)-Ef_fit(i1,ifit))<1.E-3 ) then
         nfound(ifit)=nfound(ifit)+1
         lfound(ifit)=.true.
         ntry=0
         exit
       endif
       !
       T_guess(ifit)=T_fit(i1,ifit)
       Ef_guess(ifit)=Ef_fit(i1,ifit)
       !
     enddo   
     !
     if(.not.lfound(ifit).and.ntry< 5) goto 2000
     !
     if(.not.lfound(ifit).and.ntry>=5) then
       ntry=0
       T_fit(i1,ifit) =0._SP
       T_guess(ifit)=1.E-3
       Ef_fit(i1,ifit)=0._SP
       if(ifit==1) Ef_guess(ifit)= E_sorted(last_occupied+1)
       if(ifit==2) Ef_guess(ifit)= E_sorted(last_occupied)
     endif     
     !
     if(lfound(ifit)) then
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
   call msg('s',":: Holes fitting Temperatures found " &
&            //trim(intc(nfound(1)))//"/"//trim(intc(RT_n_FIT)))
   call msg('s',":: Elec. fitting Temperatures found " &
&            //trim(intc(nfound(2)))//"/"//trim(intc(RT_n_FIT)))
 else
   call msg('s',":: Fitting Temperatures found "//trim(intc(nfound(1)))//"/"//trim(intc(RT_n_FIT)))
 endif
 !
 titles(1)='   [eV]   '
 data_ch(1,1,:)=' t    [fs]'
 if(l_two_temperatures) then
   data_ch(1,2,1)=' T(h) [K]'
   data_ch(1,2,2)=' T(e) [K]'
 else
   data_ch(1,2,:)=' T    [K]'
 endif
 data_ch(1,3,:)=' Ef   [eV]'
 data_ch(1,4,:)=' error'
 !
 do i1=1,RT_n_FIT
   titles(i1+1)=' f@'//trim(real2ch( time(i1)/FS2AUT ))
   data_ch(i1+1,1,:)= trim(real2ch( time(i1)/FS2AUT ))
   do ifit=1,nfit
     data_ch(i1+1,2,ifit)=   trim(real2ch( T_fit(i1,ifit)*HA2KEL ))
     data_ch(i1+1,3,ifit)=   trim(real2ch( Ef_fit(i1,ifit)*HA2EV ))
     data_ch(i1+1,4,ifit)=   trim(real2ch( Avg_error(i1,ifit) ))
   enddo
 enddo
 !
 do i2=1,RT_n_FIT,nmax_file_report
   n1=i2
   n2=min(RT_n_FIT,i2+15)
   !
   output_fname='YPP-RT_occupation_distribution_DATA'
   if(RT_n_FIT>nmax_file_report) &
&    output_fname=trim(output_fname)//'_grp'//trim(intc(i2/nmax_file_report+1))
   !
   call of_open_close(trim(output_fname),'ot')
   !
   do i_fp=1,n_SC_descriptions
     call msg('o '//trim(output_fname),'#',trim(SC_description(i_fp)),INDENT=0)
   enddo
   !
   call msg('o '//trim(output_fname),'#')
   call msg('o '//trim(output_fname),'# Occupations distribution at different times (f@ time)')
   call msg('o '//trim(output_fname),'#')
   call msg('o '//trim(output_fname),'#',(/titles(1),titles(n1+1:n2+1)/),INDENT=-2,USE_TABS=.true.)
   call msg('o '//trim(output_fname),'#')
   !
   i_RT=0
   do i_s=1,QP_n_states
     !
     ib    =QP_table(i_s,1)
     ik    =QP_table(i_s,3)
     i_spin=spin(QP_table(i_s,:))
     !
     do ik_rim=RT_ibz_coo(ik,1),RT_ibz_coo(ik,2)
       !
       if (ib<SC_bands(1)) cycle
       if (ib>SC_bands(2)) cycle
       if (ik_rim>RT_nk) cycle
       !
       i_RT=i_RT+1
       !
       call msg('o '//trim(output_fname),'',(/E_sorted(i_RT)*HA2EV,&
    &            RT_occ_DATA(i_RT,n1:n2)/),INDENT=-2,USE_TABS=.true.)
       !
     enddo
     !
   enddo
   !
   call of_open_close(trim(output_fname))
   !
   do ifit=1,nfit
     !
     output_fname='YPP-RT_occupation_distribution_FIT'
     if(l_two_temperatures.and.ifit==1) output_fname=trim(output_fname)//'_hole'
     if(l_two_temperatures.and.ifit==2) output_fname=trim(output_fname)//'_electrons'
     if(RT_n_FIT>nmax_file_report) &
&      output_fname=trim(output_fname)//'_grp'//trim(intc(i2/nmax_file_report+1))
     !
     call of_open_close(trim(output_fname),'ot')
     !
     do i_fp=1,n_SC_descriptions
       call msg('o '//trim(output_fname),'#',trim(SC_description(i_fp)),INDENT=0)
     enddo
     !
     call msg('o '//trim(output_fname),'#')
     call msg('o '//trim(output_fname),'# Results of the FIT')
     do i1=1,4
       call msg('o '//trim(output_fname),'#',(/data_ch(1,i1,ifit),data_ch(n1+1:n2+1,i1,ifit)/),&
&                                                                          INDENT=-2,USE_TABS=.true.)
     enddo
     !     
     call msg('o '//trim(output_fname),'#')
     call msg('o '//trim(output_fname),'# Fit functions at different time (f@ time)')
     call msg('o '//trim(output_fname),'#')
     call msg('o '//trim(output_fname),'#',(/titles(1),titles(n1+1:n2+1)/),INDENT=-2,USE_TABS=.true.)
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
