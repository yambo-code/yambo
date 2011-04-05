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
 use pars,           ONLY:SP,lchlen,cI,schlen,pi
 use YPP,            ONLY:RT_P_t,i_t_start,i_t_end,i_t_jump,Pump_path,Probe_Path,N_probes,&
&                         RT_databases,RT_init_X_one,max_n_of_paths
 use units,          ONLY:HARTREE,FS2AUT 
 use electrons,      ONLY:levels
 use com,            ONLY:jobstr,msg
 use IO_m,           ONLY:NONE
 use fields,         ONLY:Efield,i_Probe
 use real_time,      ONLY:NE_steps,RT_step,RT_output
 use vec_operate,    ONLY:sort
 use timing,         ONLY:live_timing
 implicit none
 !
 type(levels)  :: en
 !
 character(lchlen) :: jobstr_save
 integer           :: nf,N_freqs,i_t,i_f,i_fp,f_indx(max_n_of_paths)
 real(SP)          :: TIME
 complex(SP)       :: EPS,RT_P_freq_derivative
 logical           :: IO_error
 real(SP),    allocatable    :: RT_P_freq(:,:),Frequency(:)
 complex(SP), allocatable    :: X_effective(:)
 !
 call section("=",'Effective Response function ')
 !===============================================
 !
 jobstr_save=jobstr
 !
 ! Pump+Probe Databases
 !======================
 !
 do i_f=1,N_probes
   !
   jobstr=trim(Probe_path(i_f))
   call RT_databases(en,COM_=NONE)
   !
   if (i_f==1) then
     !
     !  Initialization 
     !
     call RT_init_X_one(dump_and_write=.FALSE.)
     !
     allocate(RT_P_freq(NE_steps,N_probes),X_effective(N_probes),Frequency(N_probes))
     !
   endif
   !
   Frequency(i_f)=Efield(i_Probe)%frequency
   !
   RT_P_freq(:,i_f)=real(RT_P_t(:,1))
   !
 enddo
 !
 call msg('s',':: Frequency range   [eV]:',(/minval(Frequency),maxval(Frequency)/)*HARTREE)
 !
 ! Pump only
 !===========
 !
 jobstr=trim(Pump_path)
 call RT_databases(en,COM_=NONE)
 !
 do i_f=1,N_probes
   RT_P_freq(:,i_f)=RT_P_freq(:,i_f)-real(RT_P_t(:,1))
 enddo
 !
 jobstr=jobstr_save
 !
 call sort(Frequency,indx=f_indx(:N_probes))
 !
 call RT_output("open X_effective eps")
 !
 call live_timing('Output:',(i_t_end-i_t_start+1)/(i_t_jump+1)+1)
 !
 do i_t=i_t_start,i_t_end,i_t_jump+1
   !
   TIME=(i_t-1)*RT_step
   !
   do i_fp=1,N_probes
     !
     i_f=f_indx(i_fp)
     !
     RT_P_freq_derivative=(0.,0.)
     if (i_t<i_t_end) RT_P_freq_derivative=(RT_P_freq(i_t+1,i_f)-RT_P_freq(i_t,i_f))/RT_step
     !
     X_effective(i_fp)=exp(-cI*Frequency(i_fp)*TIME)/2./Efield(i_Probe)%amplitude*&
&                      ( RT_P_freq(i_t,i_f)-cI/Frequency(i_fp)*RT_P_freq_derivative)
     !
     EPS=1.+4.*pi*X_effective(i_fp)
     !
     call RT_output('X_effective',VALUEs=(/aimag(EPS),real(EPS)/),E=real(Frequency(i_fp))*HARTREE,TIME=TIME/FS2AUT)
     !
     if (i_t+i_t_jump>i_t_end) call RT_output('eps',VALUEs=(/aimag(EPS),real(EPS),0._SP,0._SP/),E=real(Frequency(i_fp))*HARTREE)
     !
   enddo
   !
   call msg('o X_effective',' ',' ',INDENT=0)
   !
   call live_timing(steps=1)
   ! eps
 enddo
 !
 call live_timing()
 !
 call RT_output("close X_effective eps")
 !
 deallocate(RT_P_freq,X_effective,Frequency)
 !
end subroutine
