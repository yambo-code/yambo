!
! License-Identifier: GPL
!
! Copyright (C) 2015 The Yambo Team
!
! Authors (see AUTHORS file for details): AM CA
!
subroutine RT_ypp_driver(en,k,q)
 !
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels
 use YPP_real_time,  ONLY:l_RT_X,l_RT_occupations,l_RT_lifetimes,l_RT_density,&
&                         l_RT_G_two_times,l_skip_OBS_IO,l_force_min_damp,l_RT_pol,l_RT_fields
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
 call RT_output_INIT( )
 !
 ! Real-Time occupations and lifetimes
 !=====================================
 if ( l_RT_occupations.or.l_RT_lifetimes ) call RT_occupations_driver(k,q,en)
 !
 ! Plotting 
 !===========
 if (l_RT_density)                         call RT_density(en,k,q)
 !
 ! G lesser 
 !===========
 if (l_RT_G_two_times)                     call RT_G_two_times_driver(en,k,q)
 !
 ! Response Functions
 !====================
 if ( l_RT_X .or. l_RT_pol )               call RT_X_response(en,k,q)
 !
 ! Response Functions
 !====================
 if ( l_RT_fields )                        call RT_fields()
 !
end subroutine
