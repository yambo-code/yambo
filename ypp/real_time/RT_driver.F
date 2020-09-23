!
!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine RT_driver(en,k,q)
 !
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels
 use YPP_real_time,  ONLY:l_RT_X,l_RT_occupations,l_RT_lifetimes,l_RT_density,&
&                         l_RT_G_two_times,l_skip_OBS_IO,l_force_min_damp
 use YPPm,           ONLY:PROJECT_mode,l_PROJECT_atom,l_PROJECT_line,l_PROJECT_plane,&
&                         l_add_EQ_occ,l_separate_eh,l_skip_occ
 use interpolate,    ONLY:l_integral_respect
 use parser_m,       ONLY:parser
 use parallel_m,     ONLY:PAR_G_k_range
 !
 implicit none
 !
 type(levels)  :: en
 type(bz_samp) :: k,q
 !
 ! LOGICALS
 !----------
 call parser('SkipOBS_IO',l_skip_OBS_IO)
 call parser('FrMinDamp',l_force_min_damp)
 call parser('NormN',l_integral_respect)
 call parser('SeparateEH',l_separate_eh)
 call parser('IncludeEQocc',l_add_EQ_occ)
 call parser('NoOcc',l_skip_occ)
 call parser('Proj_DOS',l_PROJECT_atom)
 !
 l_PROJECT_atom =trim(PROJECT_mode)=="ATOM"
 l_PROJECT_line =trim(PROJECT_mode)=="LINE"
 l_PROJECT_plane=trim(PROJECT_mode)=="PLANE"
 !
 call section("*",'== Real-Time Post-Processing ==')
 !
 PAR_G_k_range=(/1,k%nibz/)
 !
 ! Output files
 !==============
 !
 call RT_output_INIT( )
 !
 ! GPL_EXCLUDE_START
 !
 ! Real-Time occupations and lifetimes
 !=====================================
 if ( l_RT_occupations.or.l_RT_lifetimes ) call RT_occupations_driver(k,en)
 !
 ! Plotting 
 !===========
 if (l_RT_density)                         call RT_density(en,k,q)
 !
 ! G lesser 
 !===========
 if (l_RT_G_two_times)                     call RT_G_two_times_driver(en,k,q)
 !
 ! GPL_EXCLUDE_END
 !
 ! Response Functions
 !====================
 if ( l_RT_X )                             call RT_X_response(en,k,q)
 !
end subroutine
