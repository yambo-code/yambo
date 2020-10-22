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
subroutine RT_Lifetimes_merge_intra_k(LIFE)
 !
 use parallel_m,     ONLY:PAR_COM_PLASMA_INDEX,PAR_COM_Q_INDEX,PP_redux_wait,&
&                         PAR_COM_Xk_ibz_A2A,PAR_IND_Xk_ibz
 use real_time,      ONLY:RTibz
 use R_lattice,      ONLY:nkibz
 use RT_lifetimes,   ONLY:RT_lifetime
 !
 implicit none
 !
 type(RT_lifetime), intent(inout) :: LIFE
 !
 ! Work Space
 !
 integer          :: NC,ik,COMM
 !
 if (PAR_COM_Xk_ibz_A2A%n_CPU==1) return
 if (.not.LIFE%active           ) return
 !
 ! Prefactor
 !
 NC=1
 if (LIFE%KIND=="el-ph_abs".or.LIFE%KIND=="el-ph_emit") then
    NC=PAR_COM_PLASMA_INDEX%n_CPU
 endif
 if (LIFE%KIND=="el-photon")  NC=PAR_COM_Q_INDEX%n_CPU*PAR_COM_PLASMA_INDEX%n_CPU
 !
 if (NC>1) then
   LIFE%emit=LIFE%emit/NC
   LIFE%abs=LIFE%abs/NC
 endif
 !
 do ik=1,nkibz
   !
   if (.not.PAR_IND_Xk_ibz%element_1D(ik)) cycle
   !
   call PP_redux_wait(LIFE%emit(:,RTibz%k_range(ik,1):RTibz%k_range(ik,2)),COMM=PAR_COM_Xk_ibz_A2A%COMM)
   call PP_redux_wait(LIFE%abs(:,RTibz%k_range(ik,1):RTibz%k_range(ik,2)),COMM=PAR_COM_Xk_ibz_A2A%COMM)
   !
 enddo 
 !
end subroutine RT_Lifetimes_merge_intra_k
