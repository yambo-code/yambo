!
!        Copyright (C) 2000-2018 the YAMBO team
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
subroutine RT_Lifetimes_merge(E,k,LIFE,SAVE_index)
 !
 use pars,           ONLY:SP
 use electrons,      ONLY:levels
 use R_lattice,      ONLY:bz_samp
 use parallel_m,     ONLY:PP_redux_wait,PAR_COM_PLASMA_INDEX,PAR_COM_Q_INDEX
 use real_time,      ONLY:REF_lifetime,l_initial_equilibrium,RT_nk,RT_bands
#if defined _TIMING
 use timing_m,       ONLY:timing
#endif
 !
 implicit none
 !
 type(levels),  intent(in)    :: E
 type(bz_samp), intent(in)    :: k
 integer,       intent(in)    :: SAVE_index
 real(SP),      intent(inout) :: LIFE(RT_bands(1):RT_bands(2),RT_nk)
 !
 ! Work Space
 !
 integer          :: ik
 !
#if defined _TIMING
   call timing('RT life merge',OPR='start')
#endif
 !
 !===========
 ! ALL 2 ALL
 !
 do ik=1,RT_nk
   !
   if (SAVE_index>2) LIFE(:,ik)=LIFE(:,ik)/real(PAR_COM_PLASMA_INDEX%n_CPU)
   if (SAVE_index>6) LIFE(:,ik)=LIFE(:,ik)/real(PAR_COM_Q_INDEX%n_CPU)
   !
   call PP_redux_wait(LIFE(:,ik))
   !
 enddo
 !
 if (l_initial_equilibrium) REF_lifetime(:,:,SAVE_index)=LIFE
 !
#if defined _TIMING
   call timing('RT life merge',OPR='stop')
#endif
 !
end subroutine RT_Lifetimes_merge
