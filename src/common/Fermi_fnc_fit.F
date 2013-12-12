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
integer function Fermi_fnc_fit(RT_E,RT_occ,RT_E_fit,RT_occ_fit,T_fit,Ef_fit,err_fit,l_T_max)
 ! 
 ! FIT the occupations with a Gauss–Newton algorithm
 !
 use pars,           ONLY:SP,lchlen
 use SC,             ONLY:SC_bands,RT_nk,RT_nstates,RT_ibz_coo
 use QP_m,           ONLY:QP_table,QP_n_states
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels,spin,spin_occ,n_spin
 use functions,      ONLY:Fermi_fnc,Fermi_fnc_derivative,Fermi_fnc_T_derivative
 use com,            ONLY:msg,of_open_close,error
 use stderr,         ONLY:intc,real2ch
 use units
 !
 implicit none
 !
 real(SP),intent(in)   :: RT_E(RT_nstates)
 real(SP),intent(in)   :: RT_occ(RT_nstates)
 !
 integer, parameter    :: fit_dim=1000
 real(SP), intent(out) :: RT_occ_fit(fit_dim,2)
 real(SP), intent(out) :: RT_E_fit(fit_dim,2)
 logical,  intent(in)  :: l_T_max
 !
 real(SP), intent(out) :: T_fit(2)
 real(SP), intent(out) :: Ef_fit(2)
 real(SP), intent(out) :: err_fit(4)
 !
 ! Work Space
 !
 integer, parameter    :: max_iter=10000, ntry_max=10
 real(SP)              :: Fit_E_range(2),T_guess(2),Ef_guess(2),T_now(2)
 !
 logical               :: l_two_temperatures,lfound(2)
 !
 real(SP)              :: x(RT_nstates),Jacob(RT_nstates,2)
 real(SP)              :: Jacob_sq(2,2),Jacob_sq_m1(2,2),Fit_new(2)
 real(SP)              :: delta_E,E_shifted,E_start,E_shift
 ! 
 integer  :: ifit,i_s,i_RT,ik,ik_rim,ib,i1,i2
 integer  :: ntry,nfit,last_occupied,nfound(2)
 !
 if(n_spin>1) call error('Occupations FIT not implemented for n_spin>1')
 !
 l_two_temperatures=.true.
 !
 do i_RT=1,RT_nstates
   if(RT_E(i_RT)>0._SP) exit
 enddo
 last_occupied=i_RT-1
 !
 nfit=1
 if(l_two_temperatures) nfit=2
 !
 E_shift=0.0_SP
 Fit_E_range(1)=RT_E(1)          -E_shift
 Fit_E_range(2)=RT_E(RT_nstates) +E_shift
 !
 do ifit=1,nfit
   if(.not.l_two_temperatures)        delta_E=(Fit_E_range(2)-Fit_E_range(1))
   if(ifit==1.and.l_two_temperatures) delta_E=(E_shift-Fit_E_range(1))
   if(ifit==2.and.l_two_temperatures) delta_E=(Fit_E_range(2)+E_shift)
   if(ifit==1) E_start=Fit_E_range(1)
   if(ifit==2) E_start=-E_shift
   delta_E=delta_E/real(fit_dim,SP)
   do i1=1,fit_dim
     RT_E_fit(i1,ifit)=E_start+delta_E*real(i1-1,SP)
   enddo
 enddo
 !
 do ifit=1,nfit
   if(T_fit(ifit) ==0._SP) T_guess(ifit)=1.E-3
   if(Ef_fit(ifit)==0._SP) Ef_guess(ifit)=RT_E(last_occupied+(ifit-1))
   if(T_fit(ifit) /=0._SP) T_guess(ifit)=T_fit(ifit)
   if(Ef_fit(ifit)/=0._SP) Ef_guess(ifit)=Ef_fit(ifit)
 enddo
 !
 ! DEBUG<
 !write(*,*) ' '
 !write(*,*) T_guess*HA2KEL,Ef_guess*HA2EV
 ! DEBUG>
 !
 ntry=0
 T_fit=0._SP
 Ef_fit=0._SP
 RT_occ_fit=0._SP
 !
 lfound=.false.
 !
 do ifit=1,nfit
   !
   ! I cannot fit over constant occupations
   if( ifit==1 .and. all(RT_occ(1:last_occupied )==RT_occ(1))          ) cycle
   if( ifit==2 .and. all(RT_occ(last_occupied+1:)==RT_occ(RT_nstates)) ) cycle
   !
2000 ntry=ntry+1
   !
   ! Starting point for high temperatures
   if(ntry>1) then 
     T_guess(ifit)=1.E-3_SP*10**real(ntry-1,SP)
     Ef_guess(ifit)= RT_E(last_occupied+1)*(-1._SP)**real(ifit,SP)
   endif
   !
   !DEBUG<
   !write(*,*) ifit,ntry,T_guess(ifit)*HA2KEL,Ef_guess(ifit)*HA2EV
   !DEBUG>
   !
   do i2=1,max_iter
     err_fit(ifit)=0._SP
     err_fit(ifit+2)=0._SP
     x=0._SP
     Jacob=0._SP
     i_RT=0
     do i_s=1,QP_n_states
       ib= QP_table(i_s,1)
       ik= QP_table(i_s,3)
       !
       if (QP_table(i_s,2)/=QP_table(i_s,1)) cycle
       !
       if (ib<SC_bands(1)) cycle
       if (ib>SC_bands(2)) cycle
       !
       do ik_rim=RT_ibz_coo(ik,1),RT_ibz_coo(ik,2)
         !
         if (ik_rim>RT_nk) cycle
         !
         i_RT=i_RT+1
         !
         if(i_RT<=last_occupied.and.ifit==2.and.l_two_temperatures) cycle
         if(i_RT> last_occupied.and.ifit==1.and.l_two_temperatures) cycle
         E_shifted=RT_E(i_RT)-Ef_guess(ifit)
         x(i_RT)=RT_occ(i_RT)-Fermi_fnc(E_shifted,T_guess(ifit))*spin_occ
         Jacob(i_RT,1)=-Fermi_fnc_T_derivative(E_shifted,T_guess(ifit))*spin_occ
         Jacob(i_RT,2)=-Fermi_fnc_derivative(E_shifted,T_guess(ifit))*spin_occ
         err_fit(ifit)  =err_fit(ifit)+(x(i_RT)/spin_occ)**2._SP
         err_fit(ifit+2)=max(err_fit(ifit+2),(x(i_RT)/spin_occ)**2._SP)
       enddo
       !
     enddo
     !
     err_fit(ifit)  =sqrt(err_fit(ifit))/real(RT_nstates,SP)
     err_fit(ifit+2)=sqrt(err_fit(ifit+2))
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
     !DEBUG<
     !write(*,*) i2,Jacob
     !write(*,*) Jacob_sq
     !write(*,*) Jacob_sq_m1
     !write(*,*) Fit_new
     !write(*,*) err_fit
     !DEBUG>
     !
     if(any(isnan(Fit_new(:)))) exit
     if(any(Fit_new(:)==Fit_new(:)+1._SP)) exit
     !if(all(Fit_new(:)==0._SP)) exit
     !
     T_fit(ifit)=T_guess(ifit)-Fit_new(1)
     Ef_fit(ifit)=Ef_guess(ifit)-Fit_new(2)
     !
     if(abs(T_guess(ifit)-T_fit(ifit))<1.E-6 .and. abs(Ef_guess(ifit)-Ef_fit(ifit))<1.E-3 ) then
       nfound(ifit)=nfound(ifit)+1
       lfound(ifit)=.true.
       ntry=0
       exit
     endif
     !
     T_guess(ifit)=T_fit(ifit)
     Ef_guess(ifit)=Ef_fit(ifit)
     !
   enddo
   !
   if(.not.lfound(ifit).and.ntry< ntry_max) goto 2000
   !
   if(.not.lfound(ifit).and.ntry>=ntry_max) then
     T_fit(ifit) =0._SP
     Ef_fit(ifit)=0._SP
     RT_occ_fit(:,ifit)=0._SP
     ntry=0
   endif     
 enddo
 !
 if(.not.lfound(1) .and. .not.lfound(2)) Fermi_fnc_fit=0
 if(     lfound(1) .and. .not.lfound(2)) Fermi_fnc_fit=1
 if(.not.lfound(1) .and.      lfound(2)) Fermi_fnc_fit=2
 if(     lfound(1) .and.      lfound(2)) Fermi_fnc_fit=3
 !
 T_now=T_fit
 if(l_T_max.and.Fermi_fnc_fit==3) T_now=maxval(T_fit)
 !
 do ifit=1,nfit
   !
   if(lfound(ifit)) then
     do i2=1,fit_dim
       RT_occ_fit(i2,ifit)=Fermi_fnc(RT_E_fit(i2,ifit)-Ef_fit(ifit),T_now(ifit))*spin_occ
     enddo
   endif
   !
 enddo
 !
end function Fermi_fnc_fit
