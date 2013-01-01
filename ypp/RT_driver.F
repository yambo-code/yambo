!
! Copyright (C) 2000-2013 C. Attaccalite, A. Marini and the YAMBO team 
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
subroutine RT_driver(en,k)
 !
 use YPP,            ONLY:l_fix_syms,Use_FFT,RT_P_t,RT_J_t,N_probe_frequencies,&
&                         N_probes,N_pumps,RT_initialize,RT_databases,X_order,&
&                         l_RT_X_inversion,l_skip_pol_and_curr_IO,l_force_min_damp
 use R_lattice,      ONLY:bz_samp
 use memory_m,       ONLY:mem_est
 use electrons,      ONLY:levels
 use parser_m,       ONLY:parser
 use fields,         ONLY:Efield,i_probe
 use parser_m,       ONLY:parser
 implicit none
 !
 type(levels)  :: en
 type(bz_samp) :: k
 !
 ! Work Space
 !
 logical       :: l_RT_LRR_real_field,l_RT_X_effective,l_RT_SHG_qssin
 !
 if (l_fix_syms) then
   !
   call section("*",'RT Pre Processing')
   !
   ! SYMMERIES removal
   !-------------------
   !
   call RT_fix_symmetries(en,k)
   return
 endif
 !
 call section("*",'RT Post Processing')
 !
 call RT_paths(en)
 !
 ! LOGICALS
 !----------
 !
 call parser('SkipJP_IO',l_skip_pol_and_curr_IO)
 call parser('FrMinDamp',l_force_min_damp)
 !
 l_RT_LRR_real_field= all((/&
&   .not.any((/trim(Efield(i_probe)%ef_name)=="ANTIRES",trim(Efield(i_probe)%ef_name)=="RES",&
&   trim(Efield(i_probe)%ef_name)=="SIN"/)),N_pumps==0, N_probes==1, X_order==1/) )
 l_RT_SHG_qssin     = all( (/ trim(Efield(i_probe)%ef_name)=="QSSIN" , N_pumps==0, N_probes==1, X_order==2/) )
 !
 l_RT_X_effective   = all((/any((/trim(Efield(i_probe)%ef_name)=="ANTIRES",trim(Efield(i_probe)%ef_name)=="SIN"/)),X_order>=0/))
 !
 if (l_RT_X_effective) l_RT_X_inversion=all((/ X_order>=0, N_probe_frequencies==1, N_pumps==0/))
 !
 if ( .not.any((/l_RT_LRR_real_field,l_RT_SHG_qssin,l_RT_X_effective/)) ) return
 !
 if ( any((/l_RT_LRR_real_field,l_RT_SHG_qssin/)) ) then
   !
   ! Database
   !
   call RT_databases(en)
   !
   !  Initialization 
   !
   if ( l_RT_LRR_real_field.or. l_RT_SHG_qssin) call RT_initialize(dump_and_write=.TRUE.)
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
     call RT_X_QSSIN(en)
     !
   endif
   !
 else if ( l_RT_X_effective ) then
   !
   call RT_X_effective(en)
   !
 endif
 !
 ! CLEAN
 !
 if (allocated(RT_P_t)) deallocate(RT_P_t)
 if (allocated(RT_J_t)) deallocate(RT_J_t)
 call mem_est("RT_P_t RT_J_t")
 !
end subroutine
