!
! Copyright (C) 2000-2011 A. Marini and the YAMBO team 
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
subroutine DephMatrix_setup()
 !
 use pars,          ONLY:SP
 use units,         ONLY:FS2AUT
 use com,           ONLY:error,msg,warning
 use memory_m,      ONLY:mem_est
 use electrons,     ONLY:levels
 use parser_m,      ONLY:parser
 use SC,            ONLY:SC_bands
 use QP_m,          ONLY:QP_nk
 use real_time,     ONLY:RT_Deph_Matrix,RT_ave_dephasing,RT_is_dumped
 implicit none
 !
 ! Work Space
 !
 integer :: dm_rng_lines,ik,ib(2),i1
 real(SP):: rcheck
 real(SP), allocatable :: DephM_r_limits(:,:)
 !
 ! %DephMatrix fields
 !
 dm_rng_lines=1
 rcheck=1.
 do while(rcheck>0.)
   if (allocated(DephM_r_limits)) deallocate(DephM_r_limits)
   allocate(DephM_r_limits(dm_rng_lines,4))
   DephM_r_limits(dm_rng_lines,4)=-1.
   call parser('DephMatrix',DephM_r_limits)
   rcheck=DephM_r_limits(dm_rng_lines,4)
   dm_rng_lines=dm_rng_lines+1
 enddo
 !
 dm_rng_lines=dm_rng_lines-2
 !
 ! Non null %DephMatrix fields
 !
 if(dm_rng_lines/=0.and.RT_ave_dephasing/=0._SP) then
   !
   ! Dephasing Matrix
   !
   call msg('s','[RT] Apply External Dephasing')
   !
   RT_is_dumped=.true.
   !
   allocate(RT_Deph_Matrix(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),QP_nk))
   call mem_est("DephMatrix",(/size(RT_Deph_Matrix)/))
   RT_Deph_Matrix=RT_ave_dephasing
   !
   do i1=1,dm_rng_lines
     ib(1)=int(DephM_r_limits(i1,1))
     ib(2)=int(DephM_r_limits(i1,2))
     ik=int(DephM_r_limits(i1,3))
     if(any(ib(:)>SC_bands(2)).or.any(ib(:)<SC_bands(1))) &
&                          call error("DephMatrix indexes out of the range! Wrong band assignment") 
     if(ik<=0.or.ik>QP_nk) call error("DephMatrix indexes out of the range! Wrong kpoint assignment") 
     RT_Deph_Matrix(ib(1),ib(2),ik)  =DephM_r_limits(i1,4)*FS2AUT
     RT_Deph_Matrix(ib(2),ib(1),ik)  =DephM_r_limits(i1,4)*FS2AUT
   enddo
   !
   deallocate(DephM_r_limits)
   !
 endif
 !
end subroutine
