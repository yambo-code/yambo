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
subroutine PH_of(q)
 !
 use pars,           ONLY:SP
 use OUTPUT_simple,  ONLY:OUTPUT_add_column
 use RT_lifetimes,   ONLY:RT_PE_life,PE_linewidth
 use ELPH,           ONLY:PH_freqs_sq,elph_branches,PHEL_desc,ELPH_desc,l_GKKP_expanded_DB_exists,l_GKKP_DB_exists
 use R_lattice,      ONLY:nqibz,bz_samp
 use interfaces,     ONLY:DESC_write
 !
 implicit none
 !
 type(bz_samp),     intent(in) :: q
 !
 ! Work Space
 !
 integer :: iq,il
 real(SP):: ph_freq
 !
 call OUTPUT_add_column(what="reset",name="ph")
 call OUTPUT_add_column(what="open",name="ph")
 !
 call DESC_write('o ph','#',PHEL_desc)
 call DESC_write('o ph','#',ELPH_desc)
 !
 do iq=1,nqibz
   do il=elph_branches(1),elph_branches(2)
     call OUTPUT_add_column(TITLES=(/"Q-point","Branch "/),I_VALUES=(/iq,il/))
     if (l_GKKP_expanded_DB_exists) ph_freq=abs(sqrt(PH_freqs_sq(q%k_table(iq,1),il)))
     if (l_GKKP_DB_exists         ) ph_freq=abs(sqrt(PH_freqs_sq(iq,il)))
     call OUTPUT_add_column(TITLES=(/"Energy"/),R_VALUES=(/ph_freq/),UNIT="meV")
     call OUTPUT_add_column(TITLES=(/"Width(emit)"/),R_VALUES=(/RT_PE_life%emit(il,iq)/),UNIT="meV")
     call OUTPUT_add_column(TITLES=(/"Width(abs)"/),R_VALUES=(/RT_PE_life%abs(il,iq)/),UNIT="meV")
     !DEBUG> (FP)
     call OUTPUT_add_column(TITLES=(/"Width(retarded)"/),R_VALUES=(/PE_linewidth(il,iq)/),UNIT="meV")
     !DEBUG<
     call OUTPUT_add_column(ORDER=(/"Q-point","Branch ","Energy "/))
     call OUTPUT_add_column(ORDER=(/"Width(emit)","Width(abs) "/))
     !DEBUG>
     call OUTPUT_add_column(ORDER=(/"Width(retarded)"/))
     !DEBUG<
     call OUTPUT_add_column(what="write")
   enddo
 enddo
 !
 call OUTPUT_add_column(what="close")
 !
end subroutine
