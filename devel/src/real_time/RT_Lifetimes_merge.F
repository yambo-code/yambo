!
! Copyright (C) 2000-2014 A. Marini and the YAMBO team 
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
subroutine RT_Lifetimes_merge(LIFE,SAVE_index)
 !
 use pars,           ONLY:SP
 use R_lattice,      ONLY:nkibz
 use SC,             ONLY:SC_bands,RT_nk,RT_ibz_coo
 use parallel_m,     ONLY:PP_redux_wait,PAR_COM_Plasma_INDEX
 use real_time,      ONLY:REF_lifetime,l_equilibrium_lifetimes
 !
 implicit none
 !
 integer,       intent(in)    :: SAVE_index
 real(SP),      intent(inout) :: LIFE(SC_bands(1):SC_bands(2),RT_nk)
 !
 ! Work Space
 !
 integer          :: ib,ik,ik_rim,Nk_ee
 !
 Nk_ee=nkibz
#if defined _RT_EE_RIM
 Nk_ee=RT_nk
#endif
 !
 !===========
 ! ALL 2 ALL
 !
 if (SAVE_index<=2) then
   do ik=1,Nk_ee
#if defined _RT_EE_RIM
     ik_rim=ik
#else
     ik_rim=RT_ibz_coo(ik,1)
#endif
     call PP_redux_wait(LIFE(:,ik_rim))
   enddo
#if !defined _RT_EE_RIM
   if (SAVE_index==2) call RT_Coulomb_interpolation(E,k)
#endif
 else
   do ik=1,RT_nk
     LIFE(:,ik)=LIFE(:,ik)/real(PAR_COM_Plasma_INDEX%n_CPU)
     call PP_redux_wait(LIFE(:,ik))
   enddo
 endif
 !
 if (l_equilibrium_lifetimes) then
   REF_lifetime(:,:,SAVE_index)=LIFE
   return
 else
   LIFE=LIFE+REF_lifetime(:,:,SAVE_index)
 endif
 !
end subroutine RT_Lifetimes_merge
