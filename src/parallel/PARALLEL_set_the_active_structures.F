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
subroutine PARALLEL_set_the_active_structures(n_structures)
 !
 ! The CPU's /= 1 are active and stored in PARALLEL_CPU_used/_role. Moreover if "k" and "q" fields
 ! are not present among the N_chains the CPU's are set to 1.
 !
 use parallel_m,    ONLY:ncpu,CPU_structure,PARALLEL_CPU_used,PARALLEL_CPU_used,&
&                        PARALLEL_CPU_role,i_PAR_structure
 !
 implicit none
 !
 integer ::  n_structures
 !
 ! Work Space
 !
 integer :: i_c
 !
 if (ncpu==1) return
 !
 n_structures=0
 !
 do i_c=1,CPU_structure(i_PAR_structure)%N_chains
   !
   if (CPU_structure(i_PAR_structure)%CPU(i_c)==1.and.&
&      .not.any(trim(CPU_structure(i_PAR_structure)%ROLE(i_c))==(/"k","q"/))) cycle
   !
   n_structures=n_structures+1
   !
   PARALLEL_CPU_used(n_structures)=CPU_structure(i_PAR_structure)%CPU(i_c)
   PARALLEL_CPU_role(n_structures)=CPU_structure(i_PAR_structure)%ROLE(i_c)
   !
 enddo
 !
end subroutine
