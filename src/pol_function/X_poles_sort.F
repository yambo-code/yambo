!
! Copyright (C) 2000-2012 C. Attaccalite and the YAMBO team 
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
subroutine X_poles_sort(cg_npts,bg_npts,n_poles,Xen,X,Xw)
 !
 !  Sort poles in Xo in such a way to improve the memory distribution
 !  this subroutine assumes force_bare_X_G=.true.
 !
 use pars,            ONLY:SP,schlen
 use electrons,       ONLY:levels
 use R_lattice,       ONLY:nkibz,qindx_X    
 use par_proc_m,      ONLY:pp_indexes,pp_indexes_reset,myid,pp_redux_wait,ncpu
 use par_indexes_m,   ONLY:par_indexes
 use R_lattice,       ONLY:qindx_S,nqbz
 use X_m,             ONLY:X_poles_tab,X_t
 use WF_distribute,   ONLY:Distributed_Memory
 use com,             ONLY:msg,error
 use frequency,       ONLY:w_samp
 !
 implicit none
 !
 type(X_t)   ,  intent(in) :: X
 type(levels),  intent(in) :: Xen
 type(w_samp),  intent(in) :: Xw
 integer, intent(in)    :: cg_npts,bg_npts(*)
 integer, intent(in)    :: n_poles
 !
 ! Work Space
 !
 integer :: i1,i2,ik,ikp,ikbz,ic,ic2
 integer :: n_cond,n_cond_x_proc,cpu_id
 integer :: tab_tmp(4)
 integer :: poles_cpu_map(n_poles) 
 type(pp_indexes)  :: px
 !
 ! Electron-Hole poles distribution 
 !
 !
 allocate(px%weight_1D(cg_npts))
 px%weight_1D(1:cg_npts)=bg_npts(1:cg_npts)+Xw%n(2)
 call par_indexes(px,(/cg_npts/))
 !
 n_cond=X%ib(2)-Xen%nbf
 !
 ! Distribute conduction bands among processors
 !
 n_cond_x_proc=int(n_cond/real(ncpu))
 !
 poles_cpu_map=0
 do i1 = 1,n_poles
   if(.not.px%element_1D(i1)) cycle
   poles_cpu_map(i1)=myid
 enddo
 call pp_redux_wait(poles_cpu_map)
 !
 do i1=1,n_poles 
   ! 
   ic=X_poles_tab(i1,3)
   cpu_id=mod(ic-Xen%nbf-1,ncpu)
   !
   if(cpu_id/=poles_cpu_map(i1)) then
     do i2=i1+1,n_poles 
       if(cpu_id==poles_cpu_map(i2)) then
         tab_tmp(:)       =X_poles_tab(i1,:)
         X_poles_tab(i1,:)=X_poles_tab(i2,:)
         X_poles_tab(i2,:)=tab_tmp(:)
         continue
       endif
     enddo
   endif
   !
 enddo
 !
 call pp_indexes_reset(px)
 !
end subroutine X_poles_sort
