!
!        Copyright (C) 2000-2021 the YAMBO team
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
subroutine K_output_init( )
 !
 use BS_solvers,   ONLY:run_Haydock,run_inversion,run_Diago,BSS_mode
 use BS,           ONLY:BS_dip_size,&
&                       l_BS_anomalous_Hall,l_BS_abs,l_BS_jdos,l_BS_trace,BSE_prop,&
&                       l_BS_esort,l_BS_kerr,l_BS_magnons,l_BS_dichroism,l_BS_anomalous_Hall,l_BS_photolum,&
&                       l_BS_optics,BS_dip_size,l_BS_magnons,l_BS_photolum,l_BS_ares_from_res,&
&                       eval_beta,eval_eels,eval_moke,eval_reflectivity
 use drivers,      ONLY:l_col_cut,l_rpa_IP
 use parser_m,     ONLY:parser
 use stderr,       ONLY:STRING_match
 use electrons,    ONLY:levels,n_sp_pol,n_spin
 use X_m,          ONLY:alpha_dim,eval_alpha
 !
 implicit none
 !
 type(levels)  ::Ken
 !
 ! Work Space
 logical :: eval_alpha_loc
 !
 ! What...
 !=========
 l_BS_abs       = STRING_match(BSE_prop,'abs')
 l_BS_jdos      = STRING_match(BSE_prop,'jdos').or.STRING_match(BSS_mode,'d').or.STRING_match(BSS_mode,'s').or.l_rpa_IP
 l_BS_dichroism = STRING_match(BSE_prop,'dich')
 l_BS_kerr      = STRING_match(BSE_prop,'kerr')
 l_BS_photolum  = STRING_match(BSE_prop,'photolum')
 l_BS_magnons   = STRING_match(BSE_prop,'magn') .and.n_spin>1
 !
 ! How ...
 !---------
 l_BS_esort     = STRING_match(BSE_prop,'esrt').and.(STRING_match(BSS_mode,'d').or.STRING_match(BSS_mode,'s').or.l_rpa_IP)
 !
 ! How to treat dipoles...
 !------------------------
 l_BS_trace     = STRING_match(BSE_prop,'trace')
 !
 ! Consequences which overwrites the kernel_init definitions...
 !--------------------------------------------------------------
 if (l_BS_photolum)                                l_BS_ares_from_res=.FALSE.
 if (l_BS_magnons.and.n_sp_pol==2)                 l_BS_ares_from_res=.FALSE.
 !
 if (l_BS_magnons.and.n_sp_pol==2) then
   l_BS_abs    = .false.
   l_BS_kerr   = .false.
   l_BS_trace  = .false.
   call warning(" n_sp_pol=2 and magnons requested. Spin flip BSE only is computed")
 endif
 if (l_BS_kerr) then
   call parser('AnHall',l_BS_anomalous_Hall)
   BS_dip_size=2
   l_BS_abs=.TRUE.
 endif
 if (l_BS_photolum) l_BS_trace  = .false.
 if (l_BS_trace   ) BS_dip_size = 3
 !
 l_BS_optics=l_BS_abs.or.l_BS_kerr.or.l_BS_dichroism.or.l_BS_photolum
 !
 ! Solver Logicals
 !=================
 run_Diago     = index(BSS_mode,'d')/=0
 run_Haydock   = index(BSS_mode,'h')/=0
 run_inversion = index(BSS_mode,'i')/=0
#if defined _SLEPC && !defined _NL
 run_Slepc     = index(BSS_mode,'s')/=0
#endif
 !
 if (l_BS_photolum.and.run_Haydock) then
   call warning('PL currently implemented only using the diagonalization/inversion solver')
   run_Haydock  =.false.
 endif
 !
 if (run_Haydock.and.allocated(Ken%W)) then
   call warning('K with QP widths not supported in the Haydock solver')
   run_Haydock=.false.
 endif
 !
 if (l_BS_abs) then
   call parser('Reflectivity',eval_reflectivity)
   eval_eels=.true.
   eval_alpha_loc=eval_alpha .and. ((l_col_cut) .or. (eval_eels) .or. trim(alpha_dim)=="volume" )
   eval_reflectivity=eval_reflectivity .and. .not. eval_alpha
 else
   eval_eels=.false.
   eval_alpha_loc=.false.
   eval_reflectivity=.false.
 endif
 !
 eval_beta =      eval_alpha .and. l_BS_kerr
 eval_beta = eval_alpha .and. (l_col_cut .or. trim(alpha_dim)=="volume" )
 eval_moke = .not.eval_alpha .and. l_BS_kerr
 !
 ! OUPUT(s)
 !==========
! do iq=BS_q(1),BS_q(2)
!   !
!   keys_string=' '
!   if (l_rpa_IP) keys_string=' IP'
!   if (BS_K_is_ALDA) keys_string=trim(keys_string)//' alda'
!   if (.not.l_rpa_IP.and..not.BS_K_is_ALDA) keys_string=' bse'
!   if (                       BS_K_is_ALDA) keys_string=' alda bse'
!   call OUTPUT_init("keys ALL",IQ=iq,keys=trim(keys_string))
!   !
! enddo
 !
end subroutine
