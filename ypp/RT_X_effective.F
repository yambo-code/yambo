!
! Copyright (C) 2000-2010 A. Marini and the YAMBO team 
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
subroutine RT_X_effective(en,l_RT_X_inversion)
 !
 use pars,           ONLY:SP,lchlen,pi,schlen
 use YPP,            ONLY:RT_P_t,Pump_path,N_probes,Probe_path,RT_databases,RT_initialize,&
&                         max_n_of_paths,Use_FFT,X_rt,N_pumps,N_probe_frequencies,X_order
 use units,          ONLY:HARTREE,sVcm2Vmm,AU2Vmm,FS2AUT
 use electrons,      ONLY:levels
 use com,            ONLY:jobstr,msg,error
 use IO_m,           ONLY:NONE,LOG
 use fields,         ONLY:Efield,i_Probe,ext_field,Efield_duplicate,field_frequency,&
&                         i_Pump,EtoT,Divide_by_Field
 use real_time,      ONLY:NE_steps,RT_output,RT_step,RT_P
 use vec_operate,    ONLY:sort
 use timing,         ONLY:live_timing
 use stderr,         ONLY:intc,real2ch
 implicit none
 !
 type(levels)  :: en
 logical       :: l_RT_X_inversion
 !
 character(lchlen) :: jobstr_save
 character(schlen) :: X_file_name
 integer           :: i_f,i_fp,f_indx(max_n_of_paths),i_order,min_X_order,i_c,i_t,i_conv
 real(SP)          :: Pump_Frequency(1),Unit_of_Measure
 complex(SP)       :: running_X(X_order+1,3)
 logical           :: Probe_consistency_error,CONVERGENCE_is_reached,Iterative_analysis,dump_dP,&
&                     one_P_many_W
 real(SP),    allocatable    :: RT_P_probe(:,:,:),RT_P_pump(:,:),Probe_Frequency(:),&
&                               Fourier_Frequency(:),RT_P_save(:,:,:)
 complex(SP), allocatable    :: X_effective(:,:,:),Susceptibility(:,:)
 type(ext_field)   :: Efield_probe
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
 dump_dP           =N_probe_frequencies==1.and.N_probes==1
 one_P_many_W      =N_probe_frequencies> 1.and.N_probes==1
 !
 N_probe_frequencies=max(N_probes,N_probe_frequencies) 
 !
 allocate(RT_P_probe(NE_steps,N_probes,3),RT_P_pump(NE_steps,3),X_effective(X_order+1,N_probe_frequencies,3))
 allocate(Susceptibility(X_order+1,N_probe_frequencies))
 if (dump_dP) allocate(RT_P_save(NE_steps,N_probes,3))
 allocate(Probe_Frequency(N_probe_frequencies),Fourier_Frequency(N_probe_frequencies))
 !
 X_effective=(0._SP,0._SP)
 !
 ! Pump only
 !===========
 !
 RT_P_pump     =0.
 Susceptibility=(0.,0.)
 Pump_Frequency=0.
 !
 if (N_pumps>0) then
   jobstr=trim(Pump_path)
   call RT_databases(en,COM_=NONE,mesg=trim(Pump_path))
   RT_P_pump=real(RT_P_t)
   Pump_Frequency=Efield(i_Pump)%frequency(1)
   min_X_order=0
 endif
 !
 ! Probe Databases
 !=================
 !
 Probe_consistency_error=.FALSE.
 !
 RT_P_probe=(0.,0.)
 !
 do i_f=1,N_probes
   !
   jobstr=trim(Probe_path(i_f))
   if (i_f==1) call RT_databases(en,COM_=LOG, mesg=trim(jobstr))
   if (i_f> 1) call RT_databases(en,COM_=NONE,mesg=trim(jobstr))
   !
   if (i_f==1) then
     call Efield_duplicate(Efield(i_Probe),Efield_probe)
   else
     if (Efield_probe%t_initial/=Efield(i_Probe)%t_initial) Probe_consistency_error=.TRUE.
     if (Efield_probe%ef_name/=Efield(i_Probe)%ef_name) Probe_consistency_error=.TRUE.
     if ( any( (/ abs(Efield_probe%damping-Efield(i_Probe)%damping)>1.E-5,&
&                 abs(Efield_probe%intensity-Efield(i_Probe)%intensity)>1.E-5/))) Probe_consistency_error=.TRUE.
   endif
   !
   RT_P_probe(:,i_f,:)=real(RT_P_t(:,:))-RT_P_pump(:,:)
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
 if (N_pumps>0.and.N_probes==1) then
   !
   call RT_output("open P_minus")
   !
   call live_timing('P(t)-P_pump(t):',NE_steps)
   !
   do i_t=1,NE_steps
     RT_P=RT_P_probe(i_t,1,1:3)
     call RT_output('P_minus',TIME=(i_t-1)*RT_step/FS2AUT)
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
   call msg('s',':: Pump  frequency range   [eV]:',(/minval(Pump_Frequency),maxval(Pump_Frequency)/)*HARTREE)
   call msg('s',':: Probe                   [eV]:',(/minval(Probe_Frequency),maxval(Probe_Frequency)/)*HARTREE)
 endif
 !
 ! [A] If the Probe is DELTA I use RT_X_one to Fourier trasnform
 !     =====================
 ! the polarization purged of the pump part.
 !
 if ( trim( Efield_probe%ef_name ) == 'DELTA' ) then
   !
   ! Negative energies needed in FT. No FFT. 
   ! 
   Use_FFT=.FALSE.
   !
   X_rt%energy_range=X_rt%energy_range(2)*(/-1.,1./)
   !
   RT_P_t(:,:)=RT_P_probe(:,1,:)
   !
   call RT_initialize(dump_and_write=.TRUE.)
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
   call RT_initialize(dump_and_write=.FALSE.)
   !
   call RT_X_inversion(RT_P_probe,Susceptibility,Probe_Frequency)
   !
   call plot_X_effective()
   !
   call clean_ALLOC()
   !
   return
   !
 endif
 !
 ! Time step & Time Range
 !========================
 if (N_probes==1) then
   !
   if (N_probe_frequencies>1) then
     Efield_Probe%W_step=1.
     do i_f=2,N_probe_frequencies
      Efield_Probe%W_step=min( Efield_Probe%W_step, abs(Probe_frequency(1) - Probe_frequency(i_f))) 
     enddo
   else
     Efield_Probe%W_step=Probe_frequency(1)
   endif
   !
 else
   !
   Efield_Probe%W_step=minval(Probe_frequency(:))
   !
 endif
 !
 ! Time Range
 !
 X_rt%time_range(2)= X_rt%time_range(1)+EtoT(E=Efield_Probe%W_step)
 !
 if ( X_rt%time_range(2)> NE_steps*RT_step) then
   X_rt%time_range(2)= NE_steps*RT_step
   X_rt%time_range(1)= X_rt%time_range(2)-EtoT(E=Efield_Probe%W_step)
 endif
 if ( X_rt%time_range(1) < 0.) call error(&
&   'Simulation time not long enough. Period is'//trim(real2ch( EtoT(E=Efield_Probe%W_step)/FS2AUT ))//' fs')
 !
 ! Fix the Time range. Do not write the polarization
 !
 call RT_initialize(dump_and_write=.FALSE.)
 !
 if (one_P_many_W) call msg('s','[RT] Energy step[eV]:',Efield_Probe%W_step*HARTREE)
 !
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
   !   X_order=n => (n+1)*Pump_requency-Probe_Frequency(i_f)
   !
   ! N_pumps=0
   !   |
   !   X_order=n => n*Probe_Frequency(i_f)
   !
   if (N_pumps==1) then
     !if (i_order==0) Fourier_Frequency(:)=Probe_Frequency(:)
     !if (i_order >0) Fourier_Frequency(:)=(i_order+1)*Pump_Frequency(1)-Probe_Frequency(:)
     if (i_order==0) Fourier_Frequency(:)=0.
     if (i_order >0) Fourier_Frequency(:)=real(i_order,SP)*Probe_Frequency(:)
   else if (N_pumps==0) then
     if (i_order==0) Fourier_Frequency(:)=0.
     if (i_order >0) Fourier_Frequency(:)=real(i_order,SP)*Probe_Frequency(:)
   endif
   !
   ! [C] X effective (several Probes, 1 frequency each)
   !     ===========
   !
   if (N_probes>1) then
     !
     call live_timing('X effective@order '//trim(intc(i_order))//':',N_probe_frequencies)
     !
     do i_f=1,N_probe_frequencies
       !
       CONVERGENCE_is_reached=.FALSE.
       i_conv=0
       running_X=(0.,0.)
       Efield_Probe%W_step=Probe_Frequency(i_f)
       !
       do while (.not.CONVERGENCE_is_reached)
         !
         i_conv=i_conv+1
         !
         do i_c=1,3
           call RT_coefficients_Fourier(NE_steps,RT_P_probe(:,i_f,i_c),1,(/Fourier_Frequency(i_f)/),&
&                                       running_X(i_order+1,i_c),Efield_Probe%W_step)
         enddo
         !
         call RT_split_Polarization(i_order+1,NE_steps,Probe_Frequency(i_f),running_X(:i_order+1,:),RT_P_probe(:,i_f,:),"subtract")
         !
         X_effective(i_order+1,i_f,:)=X_effective(i_order+1,i_f,:)+running_X(i_order+1,:)
         !
         CONVERGENCE_is_reached=all((/abs(running_X(i_order+1,1))<1.E-12,abs(running_X(i_order+1,2))<1.E-12,&
&                                  abs(running_X(i_order+1,3))<1.E-12/)).or.i_conv==9.or..not.Iterative_analysis
         !
       enddo
       !
       call live_timing(steps=1)
       ! 
     enddo
     !
     call live_timing()
     !
   else
     !
     ! [D] X effective (1 Probs, several frequencies)
     !     ===========
     CONVERGENCE_is_reached=.FALSE.
     i_conv=0
     running_X=(0.,0.)
     !
     do while (.not.CONVERGENCE_is_reached)
       !
       i_conv=i_conv+1
       !
       do i_c=1,3
         call RT_coefficients_Fourier(NE_steps,RT_P_probe(:,1,i_c),N_probe_frequencies,&
&                                     Fourier_Frequency,running_X(i_order+1,i_c),Efield_Probe%W_step)
       enddo
       !
       call RT_split_Polarization(i_order+1,NE_steps,Probe_Frequency(1),running_X(:i_order+1,:),RT_P_probe(:,1,:),"subtract")
       !
       X_effective(i_order+1,1,:)=X_effective(i_order+1,1,:)+running_X(i_order+1,:)
       !
       CONVERGENCE_is_reached=all((/abs(running_X(i_order+1,1))<1.E-12,abs(running_X(i_order+1,2))<1.E-12,&
&                                   abs(running_X(i_order+1,3))<1.E-12/)).or.i_conv==9.or..not.Iterative_analysis
       !
     enddo
     !
   endif
   !
 enddo
 !
 if (dump_dP) then
   !
   call RT_split_Polarization(X_order+1,NE_steps,Probe_Frequency(1),X_effective(:,1,:),RT_P_save(:,1,:),"dump")
   !
 endif
 !
 do i_order=min_X_order,X_order,1
   !
   do i_f=1,N_probe_frequencies
     !
     if (N_pumps==0) X_effective(i_order+1,i_f,:)=4.*pi*X_effective(i_order+1,i_f,:)
     !
     Susceptibility(i_order+1,i_f)=dot_product( Efield_Probe%versor(:),X_effective(i_order+1,i_f,:) )*&
&                                  Divide_by_Field(Efield_Probe,i_order)
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
     deallocate(RT_P_probe,RT_P_pump,X_effective,Susceptibility)
     deallocate(Probe_Frequency,Fourier_Frequency)
     if (dump_dP) deallocate(RT_P_save)
   end subroutine
   !
   subroutine plot_X_effective()
     !
     ! ... ordering and frequency step (defines the P(t) periodicity)
     !
     Fourier_Frequency(:)=Probe_Frequency(:)
     call sort(Fourier_Frequency,indx=f_indx(:N_probe_frequencies))
     !
     do i_order=min_X_order,X_order,1
       !
       X_file_name="X_probe_order_"//trim(intc(i_order))
       if (N_pumps==0.and.i_order==1) X_file_name="eps"
       !
       ! Note that the X are plotted as a function of the DETUNING=W_probe-W_pump
       !
       call RT_output("open "//trim(X_file_name) )
       !
       do i_fp=1,N_probe_frequencies
         !
         i_f=f_indx(i_fp)
         !
         Unit_of_Measure = 1._SP
         if (i_order==0) Unit_of_Measure = sVcm2Vmm/AU2Vmm
         if (i_order> 1) Unit_of_Measure = (sVcm2Vmm/AU2Vmm)**(i_order-1)  
         !
         call RT_output(trim(X_file_name),VALUEs=(/aimag(Susceptibility(i_order+1,i_f))*Unit_of_Measure,&
&                                                  real( Susceptibility(i_order+1,i_f))*Unit_of_Measure/),&
&                                                  E=(Probe_Frequency(i_f)-Pump_Frequency(1))*HARTREE)
         !
       enddo
       !
       call RT_output("close "//trim(X_file_name))
       !
     enddo
   end subroutine
   !
 end subroutine
