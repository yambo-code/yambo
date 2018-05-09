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
subroutine RT_Glesser_rotate(dG)
 !
 use pars,           ONLY:SP
 use parallel_m,     ONLY:PP_redux_wait,PAR_COM_Xk_ibz_INDEX,HEAD_k_CPU
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
 do ik=1,RT_nk
   !
 enddo
 !if (.not.l_RT_eval_lifetimes.and..not..RT_do_it('OUT')) return
 !
 !call OBS_rotate(R,O,N,direction)
 !
end subroutine RT_Glesser_rotate
