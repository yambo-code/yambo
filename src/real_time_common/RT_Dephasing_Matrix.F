!
!        Copyright (C) 2000-2019 the YAMBO team
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
subroutine RT_Dephasing_Matrix(E)
 !
 use pars,          ONLY:SP
 use units,         ONLY:FS2AUT
 use com,           ONLY:warning,msg
 use electrons,     ONLY:levels
 use R_lattice,     ONLY:nXkibz
 use real_time,     ONLY:RT_Deph_Matrix,RT_is_statically_dephased,Phase_LifeTime,&
&                        RAD_LifeTime,RT_bands,RT_INPUT_Deph_limits
 !
#include<memory.h>
 !
 type(levels), intent(in) :: E
 !
 ! Work Space
 !
 integer :: ik,ib_1,ib_2,i1
 real(SP), parameter   :: tresh=1.E-5_SP
 real(SP), parameter   :: deg_tresh=1.E-5_SP
 !
 ! %DephMatrix fields
 !
 if (.not.allocated(RT_INPUT_Deph_limits).or.RAD_LifeTime > tresh.or.Phase_LifeTime > tresh.or.allocated(E%W)) then
   !
   YAMBO_ALLOC(RT_Deph_Matrix,(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),nXkibz))
   !
   RT_Deph_Matrix=(0._SP,0._SP)
   !
   RT_is_statically_dephased=.TRUE.
   !
 else
   !
   return
   !
 endif
 !
 ! DIAGONAL 
 !==========
 !
 if (RAD_LifeTime > tresh) then
   !
   call msg('rs','[RT] External scalar Radiative Scattering lifetime applied')
   !
   do ik=1,nXkibz
     do ib_1=RT_bands(1),RT_bands(2)
       do ib_2=ib_1,RT_bands(2)
         if ( abs(E%E(ib_1,ik,1)-E%E(ib_2,ik,1))>=deg_tresh ) cycle
         RT_Deph_Matrix(ib_1,ib_2,ik)=1._SP/RAD_LifeTime
         RT_Deph_Matrix(ib_2,ib_1,ik)=1._SP/RAD_LifeTime
       enddo
     enddo
   enddo
   !
 endif
 !
 ! OFF-DIAGONAL
 !==============
 !
 if (Phase_LifeTime > tresh) then
   !
   ! Scalar Phase value
   !--------------------
   !
   call msg('rs','[RT] External scalar Dephasing applied')
   !
   do ik=1,nXkibz
     do ib_1=RT_bands(1),RT_bands(2)
       do ib_2=ib_1+1,RT_bands(2)
         if ( abs(E%E(ib_1,ik,1)-E%E(ib_2,ik,1))<deg_tresh ) cycle
         !if ( ib_1> E%nbf )  cycle
         !if ( ib_2<=E%nbf )  cycle
         RT_Deph_Matrix(ib_1,ib_2,ik)=1._SP/Phase_LifeTime
         RT_Deph_Matrix(ib_2,ib_1,ik)=1._SP/Phase_LifeTime
       enddo
     enddo
   enddo
   !
 else if (allocated(RT_INPUT_Deph_limits)) then
   !
   ! Non null %DephMatrix fields
   !------------------------------
   call msg('rs','[RT] External Matricial Dephasing applied')
   !
   do i1=1,size(RT_INPUT_Deph_limits,1)
     ib_1=int(RT_INPUT_Deph_limits(i1,1))
     ib_2=int(RT_INPUT_Deph_limits(i1,2))
     ik=int(RT_INPUT_Deph_limits(i1,3))
     if(any((/ib_1,ib_2/)>RT_bands(2)).or.any((/ib_1,ib_2/)<RT_bands(1))) then
       call warning("DephMatrix indexes out of the range. Ignoring wrong band   assignment") 
       cycle
     endif
     if(ik<=0.or.ik>nXkibz) then
       call warning("DephMatrix indexes out of the range. Ignoring wrong kpoint assignment") 
       cycle
     endif
     if ( abs(E%E(ib_1,ik,1)-E%E(ib_2,ik,1))<deg_tresh ) cycle
     RT_Deph_Matrix(ib_1,ib_2,ik)  =1._SP/RT_INPUT_Deph_limits(i1,4)/FS2AUT
     RT_Deph_Matrix(ib_2,ib_1,ik)  =1._SP/RT_INPUT_Deph_limits(i1,4)/FS2AUT
   enddo
   !
   YAMBO_FREE(RT_INPUT_Deph_limits)
   !
 else if (allocated(E%W)) then
   !
   ! QP database 
   !-------------
   !
   call msg('rs','[RT] Quasiparticle Dephasing applied')
   !
   do ik=1,nXkibz
     do ib_1=RT_bands(1),RT_bands(2)
       do ib_2=ib_1+1,RT_bands(2)
         if ( abs(E%E(ib_1,ik,1)-E%E(ib_2,ik,1))<deg_tresh ) cycle
         !if ( ib_1> E%nbf )  cycle
         !if ( ib_2<=E%nbf )  cycle
         RT_Deph_Matrix(ib_1,ib_2,ik)=(abs(E%W(ib_1,ik,1))+abs(E%W(ib_2,ik,1)))
         RT_Deph_Matrix(ib_2,ib_1,ik)=(abs(E%W(ib_1,ik,1))+abs(E%W(ib_2,ik,1)))
       enddo     
     enddo
   enddo
   !
 endif
 !
end subroutine
