!
!        Copyright (C) 2000-2018 the YAMBO team
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
subroutine RT_X_effective(en)
 !
 use pars,           ONLY:SP,lchlen,pi,schlen,cZERO
 use YPP_real_time,  ONLY:Pol%F_t,Pump_path,N_probes,Probe_path,RT_OBSERVABLES_IO,&
&                         max_n_of_paths,Use_FFT,RT_conf,N_pumps,N_probe_frequencies,X_order,&
&                         l_RT_X_inversion,l_skip_OBS_IO
 use units,          ONLY:HA2EV,SVCMm12VMm1,AU2VMm1,AUT2FS
 use electrons,      ONLY:levels
 use com,            ONLY:jobstr,msg,error
 use IO_m,           ONLY:NONE,LOG
 use fields,         ONLY:Efield,ext_field,Efield_duplicate,field_frequency,&
&                         EtoT,Divide_by_Field
 use RT_control,     ONLY:RT_output
 use real_time,      ONLY:RT_P
 use vec_operate,    ONLY:sort
 use LIVE_t,         ONLY:live_timing
 use stderr,         ONLY:intc,real2ch
 !
#include<memory.h>
 !
 type(levels)  :: en
 !
 character(lchlen) :: jobstr_save
 character(schlen) :: X_file_name
 integer           :: i_f,i_fp,f_indx(max_n_of_paths),i_order,min_X_order,i_c,i_t,i_conv,&
&                     i_p,N_periods,i_Pump,i_Probe
 real(SP)          :: Pump_Frequency(1),Unit_of_Measure
 complex(SP)       :: running_X(0:X_order,3),runn_err(0:X_order,3)
 logical           :: Probe_consistency_error,CONVERGENCE_is_reached,Iterative_analysis,dump_dP,&
&                     one_P_many_W,many_P_one_W,one_P_one_W,IO_error
 real(SP),    allocatable    :: RT_P_probe(:,:,:),RT_P_pump(:,:),Probe_Frequency(:),&
&                               Harmonic_Frequency(:,:),RT_P_save(:,:,:)
 complex(SP), allocatable    :: X_effective(:,:,:),Susceptibility(:,:,:)
 complex(SP), allocatable    :: X_eff_err(:,:,:),Susc_err(:,:,:)
 type(ext_field)   :: Efield_probe
 real(SP),parameter:: ZERO=1.E-15_SP
 !
 call section("=",'Effective Response function ')
 !===============================================
 !
 jobstr_save=jobstr
 !
 if (l_RT_X_inversion) then
   if (N_probe_frequencies>1) then
     min_X_order=1
     X_order=1
   else
     min_X_order=0
   endif
 else
   min_X_order=1
   if (X_order>1) min_X_order = 0 
 endif
 !
 Iterative_analysis=N_probe_frequencies==1
 one_P_many_W      =N_probe_frequencies> 1.and.N_probes==1
 many_P_one_W      =N_probe_frequencies==1.and.N_probes> 1
 one_P_one_W       =N_probe_frequencies==1.and.N_probes==1
 dump_dP           =one_P_one_W
 !
 N_probe_frequencies=max(N_probes,N_probe_frequencies) 
 !
 YAMBO_ALLOC(RT_P_probe,(RT_conf%T_n_steps,N_probes,3))
 YAMBO_ALLOC(RT_P_pump,(RT_conf%T_n_steps,3))
 YAMBO_ALLOC(X_effective,(0:X_order,N_probe_frequencies,3))
 YAMBO_ALLOC(Susceptibility,(0:X_order,N_probe_frequencies,3))
 YAMBO_ALLOC(Susc_err,(0:X_order,N_probe_frequencies,3))
 YAMBO_ALLOC(X_eff_err,(0:X_order,N_probe_frequencies,3))
 YAMBO_ALLOC(RT_P_save,(RT_conf%T_n_steps,N_probes,3))
 YAMBO_ALLOC(Probe_Frequency,(N_probe_frequencies))
 YAMBO_ALLOC(Harmonic_Frequency,(0:X_order,N_probe_frequencies))
 !
 X_effective=cZERO
 X_eff_err  =cZERO
 !
 ! Pump only
 !===========
 !
 RT_P_pump     =0._SP
 Susceptibility=cZERO
 Pump_Frequency=0._SP
 i_Pump=2
 !
 if (N_pumps>0) then
   jobstr=trim(Pump_path)
   call RT_OBSERVABLES_IO(what='JP',IO_error=IO_error,COM_=NONE,mesg=trim(Pump_path))
   RT_P_pump=real(Pol%F_t(:,1:3),SP)
   Pump_Frequency=Efield(i_Pump)%frequency(1)
   min_X_order=0
 endif
 !
 ! Probe Databases
 !=================
 !
 Probe_consistency_error=.FALSE.
 !
 RT_P_probe=0._SP
 i_Probe=1
 !
 do i_f=1,N_probes
   !
   jobstr=trim(Probe_path(i_f))
   if (i_f==1) call RT_OBSERVABLES_IO(what='JP',IO_error=IO_error,COM_=LOG, mesg=trim(jobstr))
   if (i_f> 1) call RT_OBSERVABLES_IO(what='JP',IO_error=IO_error,COM_=NONE,mesg=trim(jobstr))
   !
   if (IO_error) call error("J_and_P databases I/O error")
   !
   if (i_f==1) then
     call Efield_duplicate(Efield(i_Probe),Efield_probe)
   else
     if (Efield_probe%t_initial/=Efield(i_Probe)%t_initial) Probe_consistency_error=.TRUE.
     if (Efield_probe%ef_name/=Efield(i_Probe)%ef_name) Probe_consistency_error=.TRUE.
     if ( any( (/ abs(Efield_probe%width-Efield(i_Probe)%width)>1.E-5,&
&                 abs(Efield_probe%intensity-Efield(i_Probe)%intensity)>1.E-5/))) Probe_consistency_error=.TRUE.
   endif
   !
   RT_P_probe(:,i_f,:)=real(Pol%F_t(:,1:3),SP)-RT_P_pump(:,:)
   !
   do i_fp=1,Efield(i_Probe)%n_frequencies
     if (N_probes==1) Probe_Frequency(i_fp)=field_frequency(Efield(i_Probe),i_fp)
     if (N_probes>1 ) Probe_Frequency(i_f)=field_frequency(Efield(i_Probe),1)
   enddo
   !
 enddo
 !
 jobstr=jobstr_save
 !
 ! In the case of a PUMPED P(t) I rwite on file the difference
 ! P(t)-P_pump(t) to see the oscillations of the Probe induced component
 !
 if (N_pumps>0.and.N_probes==1.and..not.l_skip_OBS_IO) then
   !
   call RT_output("open P_minus")
   !
   call live_timing('P(t)-P_pump(t):',RT_conf%T_n_steps)
   !
   do i_t=1,RT_conf%T_n_steps
     RT_P=RT_P_probe(i_t,1,1:3)
     call RT_output('P_minus',TIME=(i_t-1)*RT_conf%delta_T*AUT2FS)
     call live_timing(steps=1)
   enddo
   !
   call live_timing()
   call RT_output("close P_minus")
   !
 endif
 !
 ! Save a copy of P 
 !
 If (dump_dP) RT_P_save=RT_P_probe
 !
 if (Probe_consistency_error) call error('Not all probes have consistent parameters')
 !
 if (trim(Efield_probe%ef_name)=='ANTIRES'.and.one_P_many_W) then
   if (N_pumps==1) then
     call msg('s',':: Pump  frequency range   [eV]:',(/minval(Pump_Frequency), maxval(Pump_Frequency)/)*HA2EV)
   else
     call msg('s',':: Probe                   [eV]:',(/minval(Probe_Frequency),maxval(Probe_Frequency)/)*HA2EV)
   endif
 endif
 !
 ! Basical Frequencies
 !=====================
 do i_order=min_X_order,X_order,1
   !
   ! FREQUENCIES
   !=============
   ! Depending on the order and if there is a steady pump applied
   ! the frequencies on which the Fourier analysis must be done are:
   !
   ! N_pumps=1
   !   |
   !   X_order=0 => Probe_Frequency(i_f)
   !   X_order=n => n*Pump_requency-Probe_Frequency(i_f)
   !
   ! N_pumps=0
   !   |
   !   X_order=n => n*Probe_Frequency(i_f)
   !
   if (N_pumps==1) then
     !
     if (i_order==0) then
       Harmonic_Frequency(i_order,:)=Probe_Frequency(:)
     else
       Harmonic_Frequency(i_order,:)=abs( real(i_order+1)*Pump_Frequency(1)-Probe_Frequency(:) )
     endif
     !
   else if (N_pumps==0) then
     !
     Harmonic_Frequency(i_order,:)=real(i_order,SP)*Probe_Frequency(:)
     !
   endif
   !
 enddo
 !
 if (N_pumps==1.or.N_pumps==0) then
   !
   if (one_P_one_W) then
     call msg('s',':: Harmonic frequencies    [eV]:',(/Harmonic_Frequency(0:,1)/)*HA2EV)
   else
     call msg('s',':: Harmonic frequencies    [eV]:',(/minval(Harmonic_Frequency),maxval(Harmonic_Frequency)/)*HA2EV)
   endif
   !
 endif
 !
 ! [A] If the Probe is DELTA I use RT_X_LRR_real_field to Fourier trasnform
 !     =====================
 ! the polarization purged of the pump part.
 !
 if ( trim( Efield_probe%ef_name ) == 'DELTA' ) then
   !
   ! Negative energies needed in FT. No FFT. 
   ! 
   Use_FFT=.FALSE.
   !
   RT_conf%W_range=RT_conf%W_range(2)*(/-1.,1./)
   !
   Pol%F_t(:,1:3)=RT_P_probe(:,1,:)
   !
   call RT_OBSERVABLES_damp_and_write()
   !
   call RT_X_LRR_real_field(en)
   !
   call clean_ALLOC()
   !
   return
   !
 endif
 !
 ! [B] The inversion method
 !
 if ( l_RT_X_inversion ) then
   !
   call RT_X_inversion(RT_P_probe,Susceptibility,Harmonic_Frequency,X_effective)
   !
   call plot_X_effective()
   !
   call clean_ALLOC()
   !
   return
   !
 endif
 !
 ! Energy step 
 !===============
 if (one_P_many_W) then
   Efield_Probe%W_step=1.
   do i_f=2,N_probe_frequencies
     Efield_Probe%W_step=min( Efield_Probe%W_step, abs(Probe_frequency(1) - Probe_frequency(i_f))) 
   enddo
 else if (one_P_one_W) then
   Efield_Probe%W_step=Probe_frequency(1)
 else if (many_P_one_W) then
   Efield_Probe%W_step=minval( Probe_frequency )
 endif
 !
 if (N_pumps==1) then
   Efield_Probe%W_step=minval( abs(Harmonic_Frequency) )
 else if (N_pumps==0) then
   Efield_Probe%W_step=abs(Harmonic_Frequency(1,1))
 endif
 !
 ! Time Range
 !
 if (RT_conf%T_range(2)<0.) RT_conf%T_range(2)= RT_conf%T_n_steps*RT_conf%delta_T
 N_periods=int( (RT_conf%T_range(2) - RT_conf%T_range(1))/EtoT(E=Efield_Probe%W_step) )
 RT_conf%T_range(1)= RT_conf%T_range(2)-N_periods*EtoT(E=Efield_Probe%W_step)
 !
 if ( RT_conf%T_range(1) < 0.) call error(&
&   'Simulation time not long enough. Period is'//trim(real2ch( EtoT(E=Efield_Probe%W_step)*AUT2FS ))//' fs')
 !
 ! Fix the Time range. Do not write the polarization
 !
 !DEBUG>
 !call RT_initialize()
 !DEBUG<
 !
 if (one_P_many_W) call msg('s','[RT] Energy step[eV]:',Efield_Probe%W_step*HA2EV)
 !
 do i_order=min_X_order,X_order,1
   !
   call live_timing('X effective@order '//trim(intc(i_order))//':',N_probe_frequencies)
   !
   do i_f=1,N_probe_frequencies
     !
     i_p=i_f
     !
     if (.not.one_P_many_W) Efield_Probe%W_step=Probe_Frequency(i_f)
     if (     one_P_many_W) i_p=1
     if (N_pumps==1       ) Efield_Probe%W_step=Harmonic_Frequency(i_order,1)
     !
     CONVERGENCE_is_reached=.FALSE.
     i_conv   =0
     running_X=cZERO
     runn_err =cZERO
     !
     do while (.not.CONVERGENCE_is_reached)
       !
       i_conv=i_conv+1
       !
       do i_c=1,3
         call RT_coefficients_Fourier(RT_conf%T_n_steps,RT_P_probe(:,i_p,i_c),1,(/Harmonic_Frequency(i_order,i_f)/),&
&                                     running_X(i_order,i_c),runn_err(i_order,i_c),Efield_Probe%W_step)
       enddo
       !
       call RT_split_Polarization(i_order+1,RT_conf%T_n_steps,Harmonic_Frequency(0:i_order,i_f),&
&                                 running_X(:i_order,:),RT_P_probe(:,i_p,:),"subtract")
       !
       X_effective(i_order,i_f,:)=X_effective(i_order,i_f,:)+running_X(i_order,:)
       X_eff_err(i_order,i_f,:)  =X_eff_err(i_order,i_f,:)+runn_err(i_order,:)
       !
       CONVERGENCE_is_reached=all((/abs(running_X(i_order,1))<ZERO,abs(running_X(i_order,2))<ZERO,&
&                                  abs(running_X(i_order,3)) <ZERO/)).or.i_conv==9.or..not.Iterative_analysis
       !
     enddo
     !
     call live_timing(steps=1) 
     !
   enddo
   !
   call live_timing() 
   !
 enddo
 !
 if (dump_dP) then
   !
   call RT_split_Polarization(X_order+1,RT_conf%T_n_steps,Harmonic_Frequency(0:,1),X_effective(0:,1,:),RT_P_save(:,1,:),"dump")
   !
 endif
 !
 do i_order=min_X_order,X_order,1
   !
   do i_f=1,N_probe_frequencies
     !
     if (N_pumps==0) X_effective(i_order,i_f,:)=4.*pi*X_effective(i_order,i_f,:)
     !
     Susceptibility(i_order,i_f,:)=X_effective(i_order,i_f,:)*Divide_by_Field(Efield_Probe,i_order)
     Susc_err(i_order,i_f,:)      =X_eff_err(i_order,i_f,:)*  Divide_by_Field(Efield_Probe,i_order)
     !
   enddo
   !
 enddo
 !
 call plot_X_effective()
 !
 call clean_ALLOC()
 !
 contains
   !
   subroutine clean_ALLOC()
     YAMBO_FREE(RT_P_probe)
     YAMBO_FREE(RT_P_pump)
     YAMBO_FREE(X_effective)
     YAMBO_FREE(Susceptibility)
     YAMBO_FREE(Probe_Frequency)
     YAMBO_FREE(Harmonic_Frequency)
     YAMBO_FREE(RT_P_save)
   end subroutine
   !
   subroutine plot_X_effective()
     !
     character(2) :: chart_string
     !
     ! ... ordering and frequency step (defines the P(t) periodicity)
     !
     Harmonic_Frequency(1,:)=Probe_Frequency(:)
     call sort(Harmonic_Frequency(1,:),indx=f_indx(:N_probe_frequencies))
     !
     do i_order=min_X_order,X_order,1
       !
       do i_c=1,3
         !
         if (i_c==1) chart_string="_x"
         if (i_c==2) chart_string="_y"
         if (i_c==3) chart_string="_z"
         !
         X_file_name="X_probe_order_"//trim(intc(i_order))//chart_string
         if (N_pumps==0.and.i_order==1) X_file_name="eps"//chart_string
         !
         ! Note that the X are plotted as a function of the DETUNING=W_probe-W_pump
         !
         call RT_output(what="open "//trim(X_file_name) )
         !
         do i_fp=1,N_probe_frequencies
           !
           i_f=f_indx(i_fp)
           !
           Unit_of_Measure = 1._SP
           !
           if (N_pumps==0) then
             if (i_order==0) Unit_of_Measure = SVCMm12VMm1/AU2VMm1
             if (i_order> 1) Unit_of_Measure = (SVCMm12VMm1/AU2VMm1)**(i_order-1)  
           else if (N_pumps==1) then
             if (i_order> 1) Unit_of_Measure = (SVCMm12VMm1/AU2VMm1)**(i_order+1)  
           endif
           !
           call RT_output(trim(X_file_name),VALUEs=(/aimag(Susceptibility(i_order,i_f,i_c))*Unit_of_Measure,&
&                                                real( Susceptibility(i_order,i_f,i_c))*Unit_of_Measure,&
&                                                abs(aimag(Susc_err(i_order,i_f,i_c))*Unit_of_Measure),&
&                                                abs(real( Susc_err(i_order,i_f,i_c))*Unit_of_Measure)/),&
&                                                E=Probe_Frequency(i_f)*HA2EV)
           !
         enddo
         call msg('o '//trim(X_file_name),'')
         call msg('o '//trim(X_file_name),'')
       enddo 
       !
     enddo
   end subroutine
   !
 end subroutine
