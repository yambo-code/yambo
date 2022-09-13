!
!        Copyright (C) 2000-2022 the YAMBO team
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
subroutine QP_logicals( )
 !
 use pars,          ONLY:SP
 use parser_m,      ONLY:parser
 use drivers,       ONLY:l_life
 use QP_m,          ONLY:QP_dSc_test,On_Mass_Shell_approx,l_QP_Expand,COHSEX_use_empties,l_QP_symmetrize,&
&                        use_GreenF_to_eval_QP,l_extended_output,use_GreenF_Zoom,QP_G_Zoom_treshold,&
&                        GW_terminator_Kind,l_GW_terminator,l_QP_eval_GF,l_QP_eval_Z,QP_solver
 use stderr,        ONLY:STRING_same
#if defined _PHEL
 use PHEL,          ONLY:PH_Self_Energy_mode
#endif
 use ELPH,          ONLY:l_GKKP_hosts_bare_dV
 !
 implicit none
 !
 ! OMS...
 call parser('OnMassShell',On_Mass_Shell_approx)
 !
 ! Netwon and Beta...
 call parser('NewtDchk',   QP_dSc_test)
 l_QP_eval_Z=STRING_same(QP_solver,"n").and..not.On_Mass_Shell_approx
 if (On_Mass_Shell_approx) QP_dSc_test=.FALSE.
 !
 ! Empty states...
 call parser('UseEbands',  COHSEX_use_empties)
 l_GW_terminator=GW_terminator_Kind/='none'
 !
 ! Reporting and Post-Processing
 call parser('QPExpand'   ,l_QP_Expand)
 call parser('QPsymmtrz',  l_QP_symmetrize)
 call parser('ExtendOut',  l_extended_output)
 !
 ! GF...
 l_QP_eval_GF=STRING_same(QP_solver,"g")
 call parser('GreenF2QP',  use_GreenF_to_eval_QP)
 use_GreenF_Zoom=QP_G_Zoom_treshold>0._SP
 !
 ! Lifetimes
 if (l_life) QP_solver="none"
 !
 ! Ph-El...
#if defined _PHEL
 !
 ! ... Kind
 if ( index(PH_Self_Energy_mode,"bare")>0 ) l_GKKP_hosts_bare_dV = .true.
 !
#endif
 !
end subroutine
