!
! License-Identifier: GPL
!
! Copyright (C) 2009 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine acknowledge_yambo()
 !
 use com,        ONLY:msg
 use LIVE_t,     ONLY:LIVE_message
 !
 call msg('r','.-ACKNOWLEDGMENT')
 call msg('r','|')
 call msg('r','| The users of YAMBO have little formal obligations with respect to')
 call msg('r','| the YAMBO group (those specified in the GNU General Public')
! License-Identifier: GPL
!
! Copyright (C) 2009 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
 call msg('r','| common practice in the scientific literature, to acknowledge the')
 call msg('r','| efforts of people that have made the research possible. In this  ')
 call msg('r','| spirit, please find below the reference we kindly ask you to use    ')
 call msg('r','| in order to acknowledge YAMBO')
 call msg('r','|')
 call msg('r','| Many-body perturbation theory calculations using the yambo code')
 call msg('r','| D. Sangalli, A. Ferretti, H. Miranda, C. Attaccalite, I. Marri, E. Cannuccia, P. Melo,')
 call msg('r','| M Marsili, F Paleari, A Marrazzo, G Prandini, P Bonfà, M O Atambo, F Affinito,')
 call msg('r','| M Palummo, A Molina-Sánchez, C Hogan, M Grüning, D Varsano and A Marini.')
 call msg('r','| J. Phys.: Condens. Matter 31, 325902 (2019).')
 call msg('r','|')
 call msg('r','| Yambo: An ab initio tool for excited state calculations')
 call msg('r','| A. Marini, C. Hogan, M. Grüning, D. Varsano')
 call msg('r','| Computer Physics Communications 180, 1392 (2009).')
 call msg('r','|')
 call msg('r',' ')
 !
 call LIVE_message("","","n","%s",CPU_TIME=.false.)
 !
end subroutine
