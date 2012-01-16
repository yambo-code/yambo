!
! Copyright (C) 2000-2011 D. Sangalli and the YAMBO team 
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
subroutine K_kerr_output()
 !
 use pars,          ONLY:SP
 use memory_m,      ONLY:mem_est
 use units,         ONLY:DEG2RAD
 use KERR,          ONLY:KERR_shadow,BSS_rhoq0_kerr
 use X_m,           ONLY:X_epsilon
 use BS,            ONLY:BSS_n_freqs
 !
 implicit none
 !
 integer           :: iw,i1
 !
 ! Kerr parameters
 !
 X_epsilon(6,:)=-X_epsilon(4,:)/( (X_epsilon(3,:)-1.0_SP)*sqrt(X_epsilon(3,:)) ) /DEG2RAD
 X_epsilon(7,:)=-X_epsilon(5,:)/( (X_epsilon(2,:)-1.0_SP)*sqrt(X_epsilon(2,:)) ) /DEG2RAD
 !
 ! I write the output file 
 ! 1 - Headers
 call KERR_shadow('def_files')
 call KERR_shadow('open_files')
 call KERR_shadow('files_headers')
 call KERR_shadow('close_files')
 !
 ! 2 - Data
 call KERR_shadow('open_files')
 do iw=1,BSS_n_freqs 
   call KERR_shadow('write_data',l_flag=.false.,n1=iw,n2=1,c1=X_epsilon(1,:))
 enddo
 call KERR_shadow('close_files')
 !
 ! CLEAN
 !
 call KERR_shadow('Dip_dealloc')
 !
 deallocate(BSS_rhoq0_kerr)
 call mem_est("BSS_rhoq0_kerr")
 !
end subroutine
