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
subroutine RT_G_lesser_init( )
 !
 use pars,           ONLY:SP,rZERO
 use RT_occupations, ONLY:RT_el_occ
 use parallel_m,     ONLY:PAR_G_k_range
 use real_time,      ONLY:dG_lesser,G_lesser_reference,RTibz,RT_bands,G_lesser
 use R_lattice,      ONLY:nkibz
 !
 ! Work Space
 !
 integer :: ik,ik_RT,ib
 !
 do ib=RT_bands(1),RT_bands(2)
   do ik=1,nkibz
     do ik_RT=RTibz%k_range(ik,1),RTibz%k_range(ik,2)
       !
       G_lesser_reference(ib,ib,ik_RT) = cmplx(rZERO,RT_el_occ%N_ref(ib,ik_RT),SP)
       !
       if( ik_RT_DbGd<PAR_G_k_range(1) .or. ik_RT_DbGd>PAR_G_k_range(2) ) cycle
       !
       G_lesser(ib,ib,ik_RT,1)  = cmplx(rZERO,RT_el_occ%N(ib,ik_RT),SP)
       dG_lesser(ib,ib,ik_RT,1) = cmplx(rZERO,RT_el_occ%N(ib,ik_RT)-RT_el_occ%N_ref(ib,ik_RT),SP)
       !
     enddo
   enddo
 enddo
 !
end subroutine RT_G_lesser_init
