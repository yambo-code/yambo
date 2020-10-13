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
subroutine RT_occupations_update( )
 !
 use parallel_m,     ONLY:PAR_COM_Xk_ibz_INDEX,PAR_G_k_range,PAR_COM_Xk_ibz_INDEX,PP_redux_wait 
 use real_time,      ONLY:dG_lesser,G_lesser_reference,RT_bands
 use RT_occupations, ONLY:RT_el_occ,RT_ho_occ,RT_life_occ
 !
 ! Work Space
 !
 integer :: ib,Nk(2)
 !
 ! El/Ho Occupations are defined from G_lesser and G_lesser_reference
 !===================
 Nk=PAR_G_k_range
 !
 do ib=RT_bands(1),RT_bands(2)
   !
   RT_el_occ%N_ref(ib,:) =          aimag(G_lesser_reference(ib,ib,:))
   RT_ho_occ%N_ref(ib,:) = spin_occ-aimag(G_lesser_reference(ib,ib,:))
   !
   RT_el_occ%dN(ib,Nk(1):Nk(2))   = aimag(dG_lesser(ib,ib,Nk(1):Nk(2),1))
   RT_life_occ%dN(ib,Nk(1):Nk(2)) = aimag(dG_lesser(ib,ib,Nk(1):Nk(2),1))
   RT_el_occ%N(ib,Nk(1):Nk(2)) = RT_el_occ%N_ref(ib,Nk(1):Nk(2))+RT_el_occ%dN(ib,Nk(1):Nk(2))
   RT_ho_occ%N(ib,Nk(1):Nk(2)) = RT_ho_occ%N_ref(ib,Nk(1):Nk(2))-RT_el_occ%dN(ib,Nk(1):Nk(2))
   !
 enddo
 !
 call PP_redux_wait(RT_el_occ%N,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
 call PP_redux_wait(RT_ho_occ%N,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
 call PP_redux_wait(RT_el_occ%dN,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
 call PP_redux_wait(RT_life_occ%dN,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
 !
end subroutine RT_occupations_update
