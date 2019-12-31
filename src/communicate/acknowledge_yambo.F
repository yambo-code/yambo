!
!        Copyright (C) 2000-2019 the YAMBO team
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
subroutine acknowledge_yambo()
 !
 use com,        ONLY:msg
 use LIVE_t,     ONLY:LIVE_message
 !
 call msg('r','.-ACKNOWLEDGMENT')
 call msg('r','|')
 call msg('r','| The users of YAMBO have little formal obligations with respect to')
 call msg('r','| the YAMBO group (those specified in the GNU General Public')
 call msg('r','| License, http://www.gnu.org/copyleft/gpl.txt). However, it is')
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
