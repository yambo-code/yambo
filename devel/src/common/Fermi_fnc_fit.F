! 
!        Copyright (C) 2000-2014 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS
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
integer function Fermi_fnc_fit(RT_E,RT_occ,nstates,T_fit,Ef_fit,err_fit,RT_occ_fit_out,RT_E_fit_out)
 ! 
 ! FIT the occupations:
 ! - first try with a standard Gauss–Newton algorithm
 ! - if this fails try with a damped Gauss–Newton algorithm, with
 !   increasing damping. This is the Levenberg-Marquartd algorithm
 !
 use pars,           ONLY:SP,lchlen
 use SC,             ONLY:SC_bands
 use QP_m,           ONLY:QP_table,QP_n_states
 use R_lattice,      ONLY:bz_samp
 use real_time,      ONLY:RT_nk,RT_ibz_coo
 use electrons,      ONLY:levels,spin,spin_occ,n_sp_pol
 use functions,      ONLY:Fermi_fnc,Fermi_fnc_derivative,Fermi_fnc_T_derivative
 use com,            ONLY:msg,of_open_close,error
 use stderr,         ONLY:intc,real2ch
 use units
 !
 implicit none
 !
 integer, intent(in)   :: nstates
 real(SP),intent(in)   :: RT_E(nstates)
 real(SP),intent(in)   :: RT_occ(nstates)
 !
 real(SP), intent(out) :: T_fit(2)
 real(SP), intent(out) :: Ef_fit(2)
 real(SP), intent(out) :: err_fit(4)
 !
 real(SP), intent(out), optional :: RT_occ_fit_out(1000,2)
 real(SP), intent(out), optional :: RT_E_fit_out(1000,2)
 !
 ! Shadows to optionals
 !
 real(SP)              :: RT_occ_fit(1000,2)
 real(SP)              :: RT_E_fit(1000,2)
 !
 ! Work Space
 !
 integer, parameter    :: max_iter=1000, ntry_max=10, fit_dim=1000
 real(SP)              :: Fit_E_range(2),T_guess(2),Ef_guess(2),  &
&                         T_save(2),Ef_save(2)
 !
 logical               :: l_two_temperatures,lfound(2),l_dumping(2)
 !
 real(SP)              :: x(nstates),Jacob(nstates,2)
 real(SP)              :: Jacob_sq(2,2),Jacob_sq_m1(2,2),Fit_new(2)
 real(SP)              :: delta_E,E_shifted,E_start,E_shift
 ! 
 integer  :: ifit,i_s,i_RT,ik,ik_rim,ib,i1,i2
 integer  :: ntry,nfit,nstates_fitted(2),last_occupied,nfound(2)
 !
 if (n_sp_pol>1) call error('Occupations FIT not implemented for n_sp_pol>1')
 !
 l_two_temperatures=.true.
 !
 do i_RT=1,nstates
   if(RT_E(i_RT)>0._SP) exit
 enddo
 last_occupied=i_RT-1
 nstates_fitted(1)=last_occupied
 nstates_fitted(2)=nstates-last_occupied
 !
 nfit=1
 if(l_two_temperatures) nfit=2
 !
 E_shift=0.0_SP
 Fit_E_range(1)=RT_E(1)       -E_shift
 Fit_E_range(2)=RT_E(nstates) +E_shift
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
 T_save=T_fit
 Ef_save=Ef_fit
 !
 l_dumping=.false.
 !
9999 do ifit=1,nfit
   if(.not.l_dumping(ifit)) then
     ! Random guess seems more stable than previous
     ! fit guess in case of Gauss-Newton
     T_guess(ifit)=1.E-3
     Ef_guess(ifit)=RT_E(last_occupied+(ifit-1))
   else
     if(T_save(ifit) <=0._SP) T_guess(ifit)=1.E-3
     if(Ef_save(ifit)==0._SP) Ef_guess(ifit)=RT_E(last_occupied+(ifit-1))
     if(T_save(ifit) > 0._SP) T_guess(ifit)=T_save(ifit)
     if(Ef_save(ifit)/=0._SP) Ef_guess(ifit)=Ef_save(ifit)
   endif
 enddo
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
   if( ifit==1 .and. all(abs(RT_occ(1:last_occupied )-RT_occ(1))<1.E-3)        ) cycle
   if( ifit==2 .and. all(abs(RT_occ(last_occupied+1:)-RT_occ(nstates))<1.E-3 ) ) cycle
   !
2000 ntry=ntry+1
   !
   ! Starting point for high temperatures
   if(ntry>1) then 
     T_guess(ifit)=1.E-3_SP*10**real(ntry-1,SP)
     Ef_guess(ifit)= RT_E(last_occupied+1)*(-1._SP)**real(ifit,SP)
     if(l_dumping(ifit)) then
       if(T_save(ifit)>0._SP) T_guess(ifit)=T_save(ifit)
       if(Ef_save(ifit)/=0._SP) Ef_guess(ifit)=Ef_save(ifit)
     endif
   endif
   !
   !DEBUG<
   !write(100,*) ifit,ntry,T_guess(ifit)*HA2KEL,Ef_guess(ifit)*HA2EV
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
       i1=2
       if (RT_nk*(SC_bands(2)-SC_bands(1)+1)>nstates) i1=1
       do ik_rim=RT_ibz_coo(ik,1),RT_ibz_coo(ik,i1)
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
     err_fit(ifit)  =sqrt(err_fit(ifit))/real(nstates_fitted(ifit),SP)
     err_fit(ifit+2)=sqrt(err_fit(ifit+2))
     !
     Jacob_sq=matmul(transpose(Jacob),Jacob)
     !
     ! This shifts from the Gauss-Newton algorithm to the
     ! to the Levenberg-Marquartd algorithm  with a dumping
     if(l_dumping(ifit)) then
       Jacob_sq(1,1)=Jacob_sq(1,1)*(1._SP+10.**-real(4-ntry))
       Jacob_sq(2,2)=Jacob_sq(2,2)*(1._SP+10.**-real(4-ntry))
     endif
     !  Jacob_sq(1,1)=Jacob_sq(1,1)*(1._SP+1.E-3/real(i2))
     !  Jacob_sq(2,2)=Jacob_sq(2,2)*(1._SP+1.E-3/real(i2))
     !endif
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
     !write(100,*) i2,Jacob
     !write(100,*) i2,x
     !write(100,*) i2,RT_occ
     !write(100,*) i2,Jacob_sq
     !write(100,*) Jacob_sq_m1
     !write(100,*) 'T= ',(T_guess(ifit)+Fit_new(1))*HA2KEL,"Ef= ",(Ef_guess(ifit)-Fit_new(2))*HA2EV
     !write(100,*) err_fit
     !DEBUG>
     !
     if(any(isnan(Fit_new(:)))) exit
     if(any(Fit_new(:)==Fit_new(:)+1._SP)) exit
     !
     T_fit(ifit)=T_guess(ifit)-Fit_new(1)
     Ef_fit(ifit)=Ef_guess(ifit)-Fit_new(2)
     !
     if( ( abs(Fit_new(1))<1.E-6.and.abs(Fit_new(2))<1.E-3                                     ) .or.  &
       & ( abs(Fit_new(1))<1.E-5.and.abs(Fit_new(2))<1.E-2.and.i2>70.and.err_fit(ifit+2)<0.05) )  then
       !write(100,*) 'FIT done'
       !write(100,*) '  '
       nfound(ifit)=nfound(ifit)+1
       lfound(ifit)=.true.
       l_dumping(ifit)=.false.
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
     if(.not.l_dumping(ifit)) then
       l_dumping(ifit)=.true.
       goto 9999
     endif
     l_dumping(ifit)=.false.
     T_fit(ifit) =0._SP
     Ef_fit(ifit)=0._SP
     RT_occ_fit(:,ifit)=0._SP
     ntry=0
     err_fit(ifit)  =99._SP
     err_fit(ifit+2)=99._SP
   endif     
 enddo
 !
 if(.not.lfound(1) .and. .not.lfound(2)) Fermi_fnc_fit=0
 if(     lfound(1) .and. .not.lfound(2)) Fermi_fnc_fit=1
 if(.not.lfound(1) .and.      lfound(2)) Fermi_fnc_fit=2
 if(     lfound(1) .and.      lfound(2)) Fermi_fnc_fit=3
 !
 do ifit=1,nfit
   !
   if(lfound(ifit)) then
     do i2=1,fit_dim
       RT_occ_fit(i2,ifit)=Fermi_fnc(RT_E_fit(i2,ifit)-Ef_fit(ifit),T_fit(ifit))*spin_occ
     enddo
   endif
   !
 enddo
 !
 if(present(RT_occ_fit_out)) RT_occ_fit_out=RT_occ_fit
 if(present(RT_E_fit_out))   RT_E_fit_out=  RT_E_fit
 !
end function Fermi_fnc_fit
