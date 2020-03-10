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
logical function INIT_read_command_line_potentials(string,potential_user_string) 
 !
 use stderr,         ONLY:STRING_remove
 use hamiltonian,    ONLY:H_potential
 !
 implicit none
 !
 character(*) :: string,potential_user_string
 !
 INIT_read_command_line_potentials=.FALSE.
 !
 if ( index(string,trim(potential_user_string)) == 0 ) return
 !
 if ( len_trim(H_potential) > 0) H_potential=trim(H_potential)//"+"
 !
 select case(trim(potential_user_string))
   case('srpa')
     H_potential=trim(H_potential)//"SRPA"
   case('exx')
     H_potential=trim(H_potential)//"EXX"
   case('exxc')
     H_potential=trim(H_potential)//"EXXC"
   case('h')
     H_potential=trim(H_potential)//"HARTREE"
   case('f')
     H_potential=trim(H_potential)//"FOCK"
   case('coh')
     H_potential=trim(H_potential)//"COH"
   case('sex')
     H_potential=trim(H_potential)//"SEX"
   case('ldax')
     H_potential=trim(H_potential)//"LDA_X"
   case('pz')
     H_potential=trim(H_potential)//"PZ"
   case('gs')
     H_potential=trim(H_potential)//"GS_XC"
   case('d')
     H_potential=trim(H_potential)//"DEFAULT"
   case('ip')
     H_potential=trim(H_potential)//"IP"
 end select
 !
 string=STRING_remove(string,trim(potential_user_string),replace=" ")
 !
 INIT_read_command_line_potentials=.TRUE.
 !
end function INIT_read_command_line_potentials
