!
!        Copyright (C) 2000-2017 the YAMBO team
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
 use YPP_real_time,  ONLY:Use_FFT,RT_P_t,RT_J_t,N_probe_frequencies,&
&                         N_probes,N_pumps,RT_IO_J_and_P,X_order,&
&                         l_RT_X_inversion,l_skip_pol_and_curr_IO,l_force_min_damp,&
&                         Probe_path,Pump_path,N_probes
 use pars,           ONLY:lchlen
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels
 use parser_m,       ONLY:parser
 use fields,         ONLY:Efield
 use com,            ONLY:jobstr,error
 !
#include<memory.h>
 !
 type(levels)  :: en
 type(bz_samp) :: k,q
 !
 ! Work Space
 !
 integer           :: i_f,i_Probe
 character(lchlen) :: jobstr_save
 logical           :: l_RT_LRR_real_field,l_RT_X_effective,l_RT_SHG_qssin
 !
 call RT_IO_J_and_P_paths( )
 !
 ! LOGICALS
 !----------
 !
 call parser('SkipJP_IO',l_skip_pol_and_curr_IO)
 call parser('FrMinDamp',l_force_min_damp)
 !
 i_Probe=1
 !
 l_RT_LRR_real_field= all((/&
&   .not.any((/trim(Efield(i_Probe)%ef_name)=="ANTIRES",trim(Efield(i_Probe)%ef_name)=="RES",&
&   trim(Efield(i_Probe)%ef_name)=="SIN"/)),N_pumps==0, N_probes==1, X_order==1/) )
 !
 l_RT_SHG_qssin     = all( (/ trim(Efield(i_Probe)%ef_name)=="QSSIN" , N_pumps==0, N_probes==1, X_order==2/) )
 !
 l_RT_X_effective   = all((/any((/trim(Efield(i_Probe)%ef_name)=="ANTIRES",trim(Efield(i_Probe)%ef_name)=="SIN"/)),X_order>=0/))
 !
 if (l_RT_X_effective) l_RT_X_inversion=all((/ X_order>=0, N_probe_frequencies==1, N_pumps==0/))
 !
 if ( .not.any((/l_RT_LRR_real_field,l_RT_SHG_qssin,l_RT_X_effective/)) ) return
 !
 ! Time Initialization
 !====================
 call RT_time_configuration_setup('CURRENT')
 !
 ! Field-Dependent actions
 !=========================
 if ( any((/l_RT_LRR_real_field,l_RT_SHG_qssin/)) ) then
   !
   ! Database
   !
   ! Search the database in JOBSTR,Pump_path and Probe_path
   !
   call RT_IO_J_and_P( )
   !
   jobstr_save=jobstr
   if (.not.allocated(RT_J_t)) then
     jobstr=trim(Pump_path)
     call RT_IO_J_and_P(mesg=trim(Pump_path))
   endif
   if (.not.allocated(RT_J_t)) then
     do i_f=1,N_probes
       jobstr=trim(Probe_path(i_f))
       call RT_IO_J_and_P(mesg=trim(Pump_path))
       if (allocated(RT_J_t)) exit
     enddo
   endif
   jobstr=trim(jobstr_save)
   !
   if (.not.allocated(RT_J_t)) call error('J and P databases not found')
   !
   !  Initialization 
   !
   if ( l_RT_LRR_real_field.or. l_RT_SHG_qssin) call RT_dump_and_write_JP()
   !
   if ( l_RT_LRR_real_field ) then
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
 YAMBO_FREE(RT_P_t)
 YAMBO_FREE(RT_J_t)
 !
end subroutine
