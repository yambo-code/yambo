!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, DS
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
subroutine PARALLEL_global_Response_IO( )
 !
 ! io_RESPONSE check
 !
 ! No X I/O is possible ONLY when the number of cpu's allocated with the "q" field
 ! in the response function string is the same of the SE/RT/SC...string
 !  
 use IO_m,          ONLY:io_RESPONSE
 use parallel_m,    ONLY:n_CPU_str_max,CPU_structure,PARALLEL_CPU_role,PARALLEL_CPU_used,&
&                       PARALLEL_n_structures_active
 use com,           ONLY:warning
 !
 implicit none
 !
 integer  :: i,i_s_se,N_q_SE,N_q_current
 !
 if (io_RESPONSE) return
 !
 call PARALLEL_get_user_structure("Self_Energy",.FALSE.)
 !
 do i=1,n_CPU_str_max
   if (trim(CPU_structure(i)%Long_Description)=="Self_Energy") then
     i_s_se=i
   endif
 enddo
 !
 do i=1,CPU_structure(i_s_se)%N_chains
   if (trim(CPU_structure(i_s_se)%ROLE(i))=="q") N_q_SE=CPU_structure(i_s_se)%CPU(i)
 enddo
 !
 do i=1,PARALLEL_n_structures_active
   if (trim(PARALLEL_CPU_role(i))=="q") N_q_current=PARALLEL_CPU_used(i)
 enddo
 !
 if (N_q_SE/=N_q_current) then
   call warning('Response function I/O forced. Different CPU for the "q" field in Response and Self-Energy and SE')
   io_RESPONSE=.TRUE.
 endif
 !
end subroutine PARALLEL_global_Response_IO
