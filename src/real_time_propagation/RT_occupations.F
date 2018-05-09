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
subroutine RT_occupations(dG)
 !
 use pars,           ONLY:SP
 use parallel_m,     ONLY:PP_redux_wait,PAR_IND_Xk_ibz,PAR_COM_Xk_ibz_INDEX
 use real_time,      ONLY:RT_all2ibz,l_RT_eval_lifetimes,RT_E_occupations,&
&                         RT_H_occupations,RT_delta_occupations,RT_bands,RT_nk,G_lesser_reference
 use RT_control,     ONLY:RT_do_it
 use electrons,      ONLY:spin_occ
 !
 implicit none
 !
 complex(SP), intent(in) :: dG(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),RT_nk)
 !
 ! Work Space
 !
 integer                 :: ik,ib
 real(SP)                :: df_H
 !
 if (l_RT_eval_lifetimes.or.RT_do_it('OUT')) then
   !OBS_rotate(R,O,N,direction)
 endif
 !
 ! Update the occupations:
 !  RT_delta_occupations are always sensitive to small changes 
 !  for Gref~spin_occ RT_H_occ is also sensitive
 !  for Gref~0        RT_E_occ is also sensitive
 !
 RT_E_occupations=0._SP
 RT_H_occupations=0._SP
 RT_delta_occupations=0._SP
 !
 do ik=1,RT_nk
   !
   if (.not.PAR_IND_Xk_ibz%element_1D(RT_all2ibz(ik)) ) cycle
   !
   do ib=RT_bands(1),RT_bands(2)
     !
     RT_E_occupations(ib,ik)= aimag(dG(ib,ib,ik))+aimag(G_lesser_reference(ib,ib,ik))
     !
     ! AM, July 2015. Intel compilers nest in ordered way the mathematical parenthesis. By using 
     ! df_H this problem is solved.
     !
     df_H                   =spin_occ-aimag(G_lesser_reference(ib,ib,ik))
     RT_H_occupations(ib,ik)=-aimag(dG(ib,ib,ik))+ df_H
     !
     RT_delta_occupations(ib,ik)=aimag(dG(ib,ib,ik))
     !
   enddo
 enddo
 !
 call PP_redux_wait(RT_E_occupations,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
 call PP_redux_wait(RT_H_occupations,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
 call PP_redux_wait(RT_delta_occupations,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
 !
end subroutine RT_occupations
