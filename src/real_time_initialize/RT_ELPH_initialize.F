!
!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine RT_ELPH_initialize(E,k,q)
 ! 
 use parser_m,       ONLY:parser
 use real_time,      ONLY:RT_bands
 use electrons,      ONLY:levels
 use drivers,        ONLY:l_elph_scatt,l_phel_scatt
 use R_lattice,      ONLY:bz_samp
 use IO_m,           ONLY:io_control,OP_RD_CL,VERIFY,NONE
 use ELPH,           ONLY:PH_freqs_sq,PH_W_debye
 !
#include<memory.h>
 !
 type(levels)             :: E
 type(bz_samp),intent(in) :: k,q
 !
 ! Work Space
 !
 integer             ::ID,i_err
 integer, external   ::io_RT_components,ELPH_expanded_databases_check
 logical             ::l_use_Debye_E
 !
 call section('=','El-Ph matrix elements')
 !                 ---------------------
 ! Restart
 !==========
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=VERIFY,ID=ID) 
 i_err=io_RT_components('REF',ID)
 !
 ! Logicals
 !==========
 if (.not.l_elph_scatt.and..not.l_phel_scatt) return
 !
 call parser('UseDebyeE'   , l_use_Debye_E)
 !
 ! El-Ph databases check
 !=======================
 i_err=ELPH_expanded_databases_check(RT_bands(2))
 if (i_err<0) then
   call warning('ELPH database not found and/or incompatible OR QP widths not provided. E-P scattering switched off')
   l_elph_scatt   =.FALSE.
   return
 endif
 !
 ! Q/K grid remapping
 !====================
 call ELPH_Q_databases_map(k,q)
 !
 ! El-Ph databases I/O
 !=====================
 if (l_elph_scatt) call ELPH_expanded_databases_load(E,k,q,RT_bands,"BZ square")
 !
 ! Pj-El databases I/O
 !=====================
 if (l_phel_scatt) call ELPH_expanded_databases_load(E,k,q,RT_bands,"IBZ")
 !
 ! Test of the effect of using a single optical mode
 !                               ===================
 !
 if (l_use_Debye_E) PH_freqs_sq=PH_W_debye**2
 !
 ! Q weights 
 !===========
 !
 call section('=','El-Ph BZ integrals')
 !                 -------------------
 call ELPH_acoustic_phonon_properties(q)
 !
 call RT_ELPH_Q_weights(q)
 !
 ! THETA matrix indexes
 !======================
#if defined _ELPH_ITERATIVE
 call RT_ELPH_iterative_init( )
#endif
 !
end subroutine RT_ELPH_initialize
