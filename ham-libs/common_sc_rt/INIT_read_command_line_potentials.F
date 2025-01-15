!
! License-Identifier: GPL
!
! Copyright (C) 2019 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
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
   case('cvonly')
     H_potential=trim(H_potential)//"CVONLY"
 end select
 !
 string=STRING_remove(string,trim(potential_user_string),replace=" ")
 !
 INIT_read_command_line_potentials=.TRUE.
 !
end function INIT_read_command_line_potentials
