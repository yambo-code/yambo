!
! Copyright (C) 2000-2013 A. Marini and the YAMBO team
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
subroutine PARALLEL_get_user_structure(ENVIRONMENT)
 !
 use pars,         ONLY:schlen
 use parallel_m,   ONLY:CPU_structure,n_CPU_str_max,PARALLEL_CPU_role,PARALLEL_CPU_used,&
&                       PARALLEL_n_structures_active,ncpu
 use stderr,       ONLY:string_split
 !
 implicit none
 !
 character(*) :: ENVIRONMENT
 !
 ! Work Space
 !
 integer          ::i_s,i_selected,i_c
 logical          ::q_found,k_found
 character(schlen)::c_strings(10),r_strings(10),CPU_string_,ROLE_string_
 !
 i_selected=-1
 !
 q_found   =.FALSE.
 k_found   =.FALSE.
 !
 do i_s=1,n_CPU_str_max
   !
   if (ncpu==1) then
     CPU_structure(i_s)%CPU_string=" "
     CPU_structure(i_s)%ROLE_string=" "
   endif
   !
   if (trim(CPU_structure(i_s)%Long_Description)==ENVIRONMENT) then
     i_selected=i_s
     CPU_string_=CPU_structure(i_selected)%CPU_string
     ROLE_string_=CPU_structure(i_selected)%ROLE_string
   endif
   !
 enddo
 !
 do i_c=1,len_trim(CPU_string_)
   if (CPU_string_(i_c:i_c)==".") CPU_string_(i_c:i_c)=" "
 enddo
 !
 do i_c=1,len_trim(ROLE_string_)
   if (ROLE_string_(i_c:i_c)==".") ROLE_string_(i_c:i_c)=" "
 enddo
 !
 call string_split(CPU_string_,c_strings,n_non_empty_strings=CPU_structure(i_selected)%N_chains)
 call string_split(ROLE_string_,r_strings)
 !
 PARALLEL_n_structures_active=0
 !
 do i_c=1,CPU_structure(i_selected)%N_chains
   !
   read(c_strings(i_c),'(i3)') CPU_structure(i_selected)%CPU(i_c)
   read(r_strings(i_c),'(a )') CPU_structure(i_selected)%ROLE(i_c)
   !
   if (CPU_structure(i_selected)%CPU(i_c)==1.and.trim(CPU_structure(i_selected)%ROLE(i_c))=="v" ) cycle
   if (CPU_structure(i_selected)%CPU(i_c)==1.and.trim(CPU_structure(i_selected)%ROLE(i_c))=="c" ) cycle
   if (CPU_structure(i_selected)%CPU(i_c)==1.and.trim(CPU_structure(i_selected)%ROLE(i_c))=="b" ) cycle
   if (CPU_structure(i_selected)%CPU(i_c)==1.and.trim(CPU_structure(i_selected)%ROLE(i_c))=="qp") cycle
   !
   PARALLEL_n_structures_active=PARALLEL_n_structures_active+1
   !
   PARALLEL_CPU_used(PARALLEL_n_structures_active)=CPU_structure(i_selected)%CPU(i_c)
   PARALLEL_CPU_role(PARALLEL_n_structures_active)=CPU_structure(i_selected)%ROLE(i_c)
   !
   if (trim(PARALLEL_CPU_role(PARALLEL_n_structures_active))=="k" ) k_found=.TRUE.
   if (trim(PARALLEL_CPU_role(PARALLEL_n_structures_active))=="q" ) q_found=.TRUE.
   !
   PARALLEL_CPU_used(PARALLEL_n_structures_active)=2**(log(real(PARALLEL_CPU_used(PARALLEL_n_structures_active)))/log(2.))
   !
 enddo
 !
 if (ncpu==1) then
   CPU_structure(i_selected)%nCPU_diagonalization=1
   CPU_structure(i_selected)%nCPU_inversion=1
 endif
 !
 if (CPU_structure(i_selected)%nCPU_diagonalization/=1.and.CPU_structure(i_selected)%nCPU_diagonalization/=ncpu) then
   CPU_structure(i_selected)%nCPU_diagonalization=ncpu
 endif
 !
 if (CPU_structure(i_selected)%nCPU_inversion/=1.and.CPU_structure(i_selected)%nCPU_inversion/=ncpu) then
   CPU_structure(i_selected)%nCPU_inversion=ncpu
 endif
 !
 ! The two top fields ("q" and "k") must be defined anyway. But only these two!
 !
 if (.not.q_found.or.ncpu==1) then
   PARALLEL_n_structures_active=PARALLEL_n_structures_active+1
   PARALLEL_CPU_used(PARALLEL_n_structures_active)=1
   PARALLEL_CPU_role(PARALLEL_n_structures_active)="q"
 endif
 !
 if (.not.k_found.or.ncpu==1) then
   PARALLEL_n_structures_active=PARALLEL_n_structures_active+1
   PARALLEL_CPU_used(PARALLEL_n_structures_active)=1
   PARALLEL_CPU_role(PARALLEL_n_structures_active)="k"
 endif
 !
 if (ncpu==1) PARALLEL_n_structures_active=0
 !
end subroutine PARALLEL_get_user_structure
