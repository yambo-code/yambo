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
 use pars,           ONLY:cZERO
 use parser_m,       ONLY:parser
 use real_time,      ONLY:THETA_matrix,THETA_matrix_index,l_RT_CCA_Kernel,RT_bands
 use electrons,      ONLY:levels
 use drivers,        ONLY:l_elph_scatt
 use R_lattice,      ONLY:bz_samp,nXkibz
 use IO_m,           ONLY:io_control,OP_RD_CL,VERIFY,NONE
 use ELPH,           ONLY:ph_freqs_sq,ph_modes,W_debye
 !
#include<memory.h>
 !
 type(levels)             :: E
 type(bz_samp),intent(in) :: k,q
 !
 ! Work Space
 !
 integer             ::iq,ID,i_counter,il,i_err,i_s
 integer, external   ::io_RT_components,ELPH_expanded_databases_check
 logical             ::l_use_Debye_E
 !
 ! Restart
 !==========
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=VERIFY,ID=ID) 
 i_err=io_RT_components('REF',ID)
 !
 ! Logicals
 !==========
 if (.not.l_elph_scatt) return
 !
 call parser('UseDebyeE'   , l_use_Debye_E)
 !
 ! El-Ph databases check
 !=======================
 i_err=ELPH_expanded_databases_check(RT_bands(2))
 if (i_err<0) then
   call warning('ELPH database not found and/or incompatible OR QP widths not provided. E-P scattering switched off')
   l_elph_scatt   =.FALSE.
   l_RT_CCA_Kernel=.FALSE.
   return
 endif
 !
 ! Q/K grid remapping
 !====================
 call ELPH_Q_databases_map(k,q)
 !
 ! El-Ph databases I/O
 !=====================
 call ELPH_expanded_databases_load(E,k,q,RT_bands,"BZ")
 !
 ! Test of the effect of using a single optical mode
 !                               ===================
 !
 if (l_use_Debye_E) ph_freqs_sq=W_debye**2
 !
 ! Q weights 
 !===========
 !
 call ELPH_Q_weight_times_Bose(q)
 !
 ! THETA matrix indexes
 !======================
 if (l_RT_CCA_Kernel) return
 !
 YAMBO_ALLOC(THETA_matrix,(2*q%nbz*ph_modes,RT_bands(2)-RT_bands(1)+1,RT_bands(2)-RT_bands(1)+1,nXkibz))
 THETA_matrix=cZERO
 YAMBO_ALLOC(THETA_matrix_index,(q%nbz,ph_modes,2))
 !
 i_counter=0
 !
 do iq=1,q%nbz
   do il=1,ph_modes
     do i_s=1,2
       !
       i_counter=i_counter+1
       !
       THETA_matrix_index(iq,il,i_s)=i_counter
       !
     enddo
   enddo
 enddo
 !
end subroutine RT_ELPH_initialize
