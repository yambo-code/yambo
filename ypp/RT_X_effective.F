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
subroutine RT_X_effective(en)
 !
 use pars,           ONLY:SP,lchlen,pi,schlen
 use YPP,            ONLY:RT_P_t,Pump_path,N_probes,Probe_path,RT_databases,RT_initialize,&
&                         max_n_of_paths,Use_FFT,X_rt,N_pumps,N_probe_frequencies,X_order
 use units,          ONLY:HARTREE,sVcm2Vmm,AU2Vmm 
 use electrons,      ONLY:levels
 use com,            ONLY:jobstr,msg,error
 use IO_m,           ONLY:NONE,LOG
 use fields,         ONLY:Efield,i_Probe,ext_field,Efield_duplicate,field_frequency,&
&                         i_Pump,EtoT,Divide_by_Field
 use real_time,      ONLY:NE_steps,RT_output,RT_step
 use vec_operate,    ONLY:sort
 use timing,         ONLY:live_timing
 use stderr,         ONLY:intc
 implicit none
 !
 type(levels)  :: en
 !
 character(lchlen) :: jobstr_save
 character(schlen) :: X_file_name
 integer           :: i_f,i_fp,f_indx(max_n_of_paths),i_order,min_X_order
 real(SP)          :: Pump_Frequency(1),Unit_of_Measure
 logical           :: Probe_consistency_error
 real(SP),    allocatable    :: RT_P_probe(:,:),RT_P_pump(:,:),Probe_Frequency(:),Fourier_Frequency(:)
 complex(SP), allocatable    :: X_effective(:)
 type(ext_field)   :: Efield_probe
 !
 call section("=",'Effective Response function ')
 !===============================================
 !
 jobstr_save=jobstr
 !
 N_probe_frequencies=max(N_probes,N_probe_frequencies) 
 !
 allocate(RT_P_probe(NE_steps,N_probes),RT_P_pump(NE_steps,3),X_effective(N_probe_frequencies))
 allocate(Probe_Frequency(N_probe_frequencies),Fourier_Frequency(N_probe_frequencies))
 !
 ! Pump only
 !===========
 !
 RT_P_pump=0.
 Pump_Frequency=0.
 min_X_order=1
 if (X_order==2) min_X_order = 0 
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
   RT_P_probe(:,i_f)=real(RT_P_t(:,1))-RT_P_pump(:,1)
   !
   do i_fp=1,Efield(i_Probe)%n_frequencies
     if (N_probes==1) Probe_Frequency(i_fp)=field_frequency(Efield(i_Probe),i_fp)
     if (N_probes>1 ) Probe_Frequency(i_f)=field_frequency(Efield(i_Probe),1)
   enddo
   !
 enddo
 !
 if (Probe_consistency_error) call error('Not all probes have consistent parameters')
 !
 if (trim( Efield_probe%ef_name ) == 'ANTIRES'.and.N_probes>1) then
   call msg('s',':: Pump  frequency range   [eV]:',(/minval(Pump_Frequency),maxval(Pump_Frequency)/)*HARTREE)
   call msg('s',':: Probe                   [eV]:',(/minval(Probe_Frequency),maxval(Probe_Frequency)/)*HARTREE)
 endif
 !
 jobstr=jobstr_save
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
   RT_P_t(:,1)=RT_P_probe(:,1)
   !
   call RT_initialize(dump_and_write=.TRUE.)
   !
   call RT_X_LRR_real_field(en)
   !
   deallocate(RT_P_probe,Probe_Frequency,RT_P_pump,X_effective)
   !
   return
   !
 endif
 !
 ! Time step & Time Range
 !========================
 if (Efield_Probe%W_step<epsilon(1._SP)) then
   Efield_Probe%W_step=1.
   do i_f=2,N_probe_frequencies
     Efield_Probe%W_step=min( Efield_Probe%W_step, abs(Probe_frequency(1) - Probe_frequency(i_f))) 
   enddo
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
 !
 call RT_initialize(dump_and_write=.FALSE.)
 call msg('s','[RT] Energy step[eV]:',Efield_Probe%W_step*HARTREE)
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
     if (i_order==0) Fourier_Frequency(:)=Probe_Frequency(:)
     if (i_order >0) Fourier_Frequency(:)=(i_order+1)*Pump_Frequency(1)-Probe_Frequency(:)
   else if (N_pumps==0) then
     Fourier_Frequency(:)=real(i_order,SP)*Probe_Frequency(:)
   endif
   !
   ! [B] X effective (several Probes, 1 frequency each)
   !     ===========
   !
   if (N_probes>1) then
     !
     call live_timing('X effective@order '//trim(intc(i_order))//':',N_probe_frequencies)
     !
     do i_f=1,N_probe_frequencies
       !
       call RT_Fourier_Coefficients(NE_steps,RT_P_probe(:,i_f),1,(/Fourier_Frequency(i_f)/),X_effective(i_f),Efield_Probe%W_step)
       !
       call live_timing(steps=1)
       ! 
     enddo
     !
     call live_timing()
     !
   else
     !
     ! [C] X effective (1 Probs, several frequencies)
     !     ===========
     !
     call RT_Fourier_Coefficients(NE_steps,RT_P_probe(:,1),N_probe_frequencies,Fourier_Frequency,X_effective,Efield_Probe%W_step)
     !
   endif
   !
   ! ... ordering and frequency step (defines the P(t) periodicity)
   !
   Fourier_Frequency(:)=Probe_Frequency(:)
   call sort(Fourier_Frequency,indx=f_indx(:N_probe_frequencies))
   !
   do i_fp=1,N_probe_frequencies
     !
     i_f=f_indx(i_fp)
     !
     if (N_pumps==0) X_effective(i_f)=4.*pi*X_effective(i_f)
     !
     X_effective(i_f)=X_effective(i_f)*Divide_by_Field(Efield_Probe,i_order)
     ! 
     Unit_of_Measure = 1._SP
     if (N_pumps==0.and.i_order.ne.1) Unit_of_Measure = sVcm2Vmm/AU2Vmm ! NB Just for SHG and OptRect, orders>2 need generalization
     call RT_output(trim(X_file_name),VALUEs=(/aimag(X_effective(i_f))*Unit_of_Measure,real(X_effective(i_f)*Unit_of_Measure)/),&
&                   E=(Probe_Frequency(i_f)-Pump_Frequency(1))*HARTREE)
     !
   enddo
   !
   call RT_output("close "//trim(X_file_name))
   !
 enddo
 !
 deallocate(RT_P_probe,Probe_Frequency,RT_P_pump,X_effective,Fourier_Frequency)
 !
end subroutine
