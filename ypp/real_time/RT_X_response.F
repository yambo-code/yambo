!
!        Copyright (C) 2000-2022 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM CA
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
subroutine RT_X_response(en,k,q)
 !
 use YPP_real_time,  ONLY:Use_FFT,Pol,Curr,N_probe_frequencies,&
&                         N_probes,N_pumps,X_order,l_RT_pol,OBS_el_free,&
&                         l_RT_X_inversion,Probe_path,Pump_path,N_probes,RT_conf,l_skip_OBS_IO
 use YPP_interfaces, ONLY:RT_OBSERVABLES_IO
 use RT_control,     ONLY:OBS_RT_IO_t
 use pars,           ONLY:schlen
 use stderr,         ONLY:STRING_split,STRING_same
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels
 use fields,         ONLY:Efield
 use parser_m,       ONLY:parser
 !
#include<memory.h>
 !
 type(levels)  :: en
 type(bz_samp) :: k,q
 !
 ! Work Space
 !
 integer           :: Ndefs,i_o
 character(schlen) :: probe_defs(3)
 logical           :: l_RT_LRR_real_field,l_RT_X_effective,l_RT_SHG_qssin,IO_error
 !
 call RT_OBSERVABLES_IO_paths('JP',en)
 !
 ! Here only the first Field is analyzed
 !=======================================
 call STRING_split(Efield(1)%ef_name,probe_defs,n_non_empty_strings=Ndefs)
 !
 l_RT_LRR_real_field= all( (/ Ndefs==1 ,N_pumps<=1, N_probes==1, X_order==1/) )
 !
 l_RT_SHG_qssin     = all( (/ trim(probe_defs(1))=="QSSIN" , N_pumps==0, N_probes==1, X_order==2/) )
 !
 l_RT_X_effective   = all((/ trim(probe_defs(1))=="SIN" ,X_order>=0/))
 !
 if (l_RT_X_effective) l_RT_X_inversion=all((/ X_order>=0, N_probe_frequencies==1, N_pumps==0/))
 !
 if ( .not.any((/l_RT_LRR_real_field,l_RT_SHG_qssin,l_RT_X_effective/)) ) return
 !
 ! Time Initialization
 !====================
 call RT_time_configuration_setup('CURRENT',OBS_RT_IO_t)
 !
 ! Field-Dependent actions
 !=========================
 if ( any((/l_RT_LRR_real_field,l_RT_SHG_qssin/)) ) then
   !
   call section("=",'Pump and Probe(s) databases I/O')
   !--------------------------------------------------
   ! Database
   !
   allocate(Pol(N_pumps+N_probes))
   allocate(Curr(N_pumps+N_probes))
   !
   call IO_temporary_jobdir_and_strings("ALL","SAVE")
   !
   if (N_pumps==0) then
     Pol(1)%title ="polarization"
     Curr(1)%title="current"
   else
     Pol(1)%title ="probe-polarization"
     Curr(1)%title="probe-current"
     Pol(2)%title ="pump-polarization"
     Curr(2)%title="pump-current"
     if (.not.STRING_same(Pump_path,"default")) call IO_temporary_jobdir_and_strings("JOBSTR",Pump_path)
     call RT_OBSERVABLES_IO( what='JP',J=Curr(2),P=Pol(2),IO_error=IO_error)
     if (.not.allocated(Curr(2)%F_t)) call error('Pump databases missing/corrupt')
   endif
   !
   if (     STRING_same(Probe_path(1),"default")) call IO_temporary_jobdir_and_strings("ALL","RESTORE")
   if (.not.STRING_same(Probe_path(1),"default")) call IO_temporary_jobdir_and_strings("JOBSTR",Probe_path(1))
   call RT_OBSERVABLES_IO( what='JP',J=Curr(1),P=Pol(1),IO_error=IO_error)
   if (.not.allocated(Curr(1)%F_t)) call error('Probe databases missing/corrupt')
   !
   call IO_temporary_jobdir_and_strings("ALL","RESTORE")
   !
   !  Damping and output 
   !---------------------
   call RT_OBSERVABLES_damp_and_write(Pol(1),RT_conf,l_skip_OBS_IO)
   call RT_OBSERVABLES_damp_and_write(Curr(1),RT_conf,l_skip_OBS_IO)
   if (N_pumps>0) then
     call RT_OBSERVABLES_damp_and_write(Pol(2),RT_conf,l_skip_OBS_IO)
     call RT_OBSERVABLES_damp_and_write(Curr(2),RT_conf,l_skip_OBS_IO)
   endif
   !
   if ( l_RT_pol ) then
     !
     call RT_Polarization(en,k,q) 
     !
   else if ( l_RT_LRR_real_field ) then
     !
     ! CORE Logicals
     !---------------
     ! 
     call parser('UseFFT',Use_FFT)
     !
     ! OPTICS
     !--------
     !
     call RT_X_LRR_real_field(en)
     !
   else if ( l_RT_SHG_qssin ) then
     !
     ! DEBUG>
     !call RT_X_QSSIN(en)
     ! DEBUG<
     !
   endif
   !
 else if ( l_RT_X_effective ) then
   !
   ! DEBUG>
   !call RT_X_effective(en)
   ! DEBUG<
   !
 endif
 !
 ! CLEAN
 !
 do i_o=1,N_probes+N_pumps
   call OBS_el_free(Pol(i_o))
   call OBS_el_free(Curr(i_o))
 enddo
 deallocate(Pol)
 deallocate(Curr)
 !
end subroutine
