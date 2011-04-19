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
 use pars,           ONLY:SP,lchlen,pi
 use YPP,            ONLY:RT_P_t,Pump_path,N_probes,Probe_path,RT_databases,RT_init_X_one,&
&                         max_n_of_paths,Use_FFT,X_rt,N_pumps,N_probe_frequencies
 use units,          ONLY:HARTREE
 use electrons,      ONLY:levels
 use com,            ONLY:jobstr,msg,error
 use IO_m,           ONLY:NONE,LOG
 use fields,         ONLY:Efield,i_Probe,ext_field,Efield_duplicate
 use real_time,      ONLY:NE_steps,RT_output
 use vec_operate,    ONLY:sort
 use timing,         ONLY:live_timing
 implicit none
 !
 type(levels)  :: en
 !
 character(lchlen) :: jobstr_save
 integer           :: i_t,i_f,i_fp,f_indx(max_n_of_paths)
 real(SP)          :: deltaW
 complex(SP)       :: EPS
 logical           :: Probe_consistency_error
 real(SP),    allocatable    :: RT_P_probe(:,:),RT_P_pump(:,:),Frequency(:)
 complex(SP), allocatable    :: X_effective(:)
 type(ext_field)   :: Efield_probe
 !
 call section("=",'Effective Response function ')
 !===============================================
 !
 jobstr_save=jobstr
 !
 allocate(RT_P_probe(NE_steps,N_probes),RT_P_pump(NE_steps,3))
 allocate(X_effective(max(N_probes,N_probe_frequencies)))
 allocate(Frequency(max(N_probes,N_probe_frequencies)))
 !
 ! Pump only
 !===========
 !
 if (N_pumps>0) then
   jobstr=trim(Pump_path)
   call RT_databases(en,COM_=NONE,mesg="pump")
   RT_P_pump=real(RT_P_t)
 else
   RT_P_pump=0.
 endif
 !
 ! Probe Databases
 !=================
 !
 Probe_consistency_error=.FALSE.
 !
 RT_P_probe=(0.,0.)
 deltaW=0.
 !
 do i_f=1,N_probes
   !
   jobstr=trim(Probe_path(i_f))
   if (i_f==1) call RT_databases(en,COM_=LOG,mesg="probe")
   if (i_f> 1) call RT_databases(en,COM_=NONE,mesg="probe")
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
   !DEBUG>
   !RT_P_probe(:,1)=RT_P_probe(:,1)+real(RT_P_t(:,1))-RT_P_pump(:,1)
   !DEBUG<
   if (N_probe_frequencies >1 ) then  
     deltaW=(Efield(i_Probe)%frequency(2)-Efield(i_Probe)%frequency(1))/(Efield(i_Probe)%freq_steps-1)
     do i_fp=1,N_probe_frequencies
       Frequency(i_fp)=Efield(i_Probe)%frequency(1)+deltaW*(i_fp-1)
     enddo
   else
     Frequency(i_f)   =Efield(i_Probe)%frequency(1)
   endif
   !
!DEBUG>
!   do i_t=1,NE_steps
!     write (100+i_f,*) i_t,RT_P_probe(i_t,i_f)
!   enddo
!DEBUG<
 enddo
 !
 if (Probe_consistency_error) call error('Not all probes have consistent parameters')
 !
 if (trim( Efield_probe%ef_name ) == 'ANTIRES'.and.N_probes>1) then
   call msg('s',':: Frequency range   [eV]:',(/minval(Frequency),maxval(Frequency)/)*HARTREE)
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
   call RT_init_X_one(dump_and_write=.TRUE.)
   !
   call RT_X_one(en)
   !
   deallocate(RT_P_probe,Frequency,RT_P_pump,X_effective)
   !
   return
   !
 endif
 !
 call RT_output("open eps")
 !
 call RT_init_X_one(dump_and_write=.FALSE.)
 !
 ! [B] X effective (several Probes, 1 frequency each)
 !     ===========
 !
 if (N_probes>1) then
   !
   call sort(Frequency,indx=f_indx(:N_probes))
   !
   call live_timing('X effective:',N_probes)
   !
   !DEBUG>
   !call RT_Fourier_Coefficients(NE_steps,RT_P_probe(i_t,1),N_probes,Frequency,X_effective)
   !DEBUG<
   !
   do i_fp=1,N_probes
     !
     i_f=f_indx(i_fp)
     !
     call RT_Fourier_Coefficients(NE_steps,RT_P_probe(i_t,i_f),1,(/Frequency(i_fp)/),X_effective(i_fp))
     !
     EPS=1.+4.*pi*X_effective(i_fp)
     !
     call RT_output('eps',VALUEs=(/aimag(EPS),real(EPS),0._SP,0._SP/),E=Frequency(i_fp)*HARTREE)
     !
     call live_timing(steps=1)
     ! 
   enddo
   !
   call live_timing()
   !
 else
   !
   call RT_Fourier_Coefficients(NE_steps,RT_P_probe(i_t,1),N_probe_frequencies,Frequency,X_effective)
   !
   do i_f=1,N_probe_frequencies
     !
     EPS=1.+4.*pi*X_effective(i_f)
     !
     call RT_output('eps',VALUEs=(/aimag(EPS),real(EPS),0._SP,0._SP/),E=Frequency(i_f)*HARTREE)
     !
   enddo
   !
 endif
 !
 call RT_output("close eps")
 !
 deallocate(RT_P_probe,Frequency,RT_P_pump,X_effective)
 !
end subroutine
