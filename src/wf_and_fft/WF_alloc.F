!
!        Copyright (C) 2000-2023 the YAMBO team
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
subroutine WF_alloc(WF,k_extrema_only)
 !
 use pars,          ONLY:cZERO,SP
 use wave_func,     ONLY:WAVEs,states_to_load,wf_ng,wf_ncx,rho_map_size
 use electrons,     ONLY:n_spinor
 use parallel_int,  ONLY:PARALLEL_live_message
 use cuda_m,        ONLY:have_cuda
 !
#include<memory.h>
#include<yambo_wfs.h>
 !
 type(WAVEs)           :: WF
 logical, intent(in)   :: k_extrema_only
 !
 ! Work Space
 !
 integer :: ik,ib,i_sp_pol,NK,N_loaded,N_total,wf_grid_size
 !
 ! Distributed allocation
 !
 if(allocated(states_to_load)) then
   !
   WF%N=0
   N_loaded=0
   N_total=0
   do i_sp_pol=WF%sp_pol(1),WF%sp_pol(2)
     do ik=WF%k(1),WF%k(2)
       do ib=WF%b(1),WF%b(2)
         N_total=N_total+1
         if (states_to_load(ib,ik,i_sp_pol)) then
           N_loaded=N_loaded+1
           if (.not.k_extrema_only) WF%N=WF%N+1
           if (     k_extrema_only.and.(ik==WF%k(1).or.ik==WF%k(2))) WF%N=WF%N+1
	 endif
       enddo
     enddo
   enddo
   !
 else
   !
   NK=(WF%k(2)-WF%k(1)+1)
   if (k_extrema_only) NK=min(NK,2)
   WF%N=(WF%b(2)-WF%b(1)+1)*NK*(WF%sp_pol(2)-WF%sp_pol(1)+1)
   N_loaded=WF%N
   N_total=WF%N
   !
 endif
 !
 call PARALLEL_live_message("Wave-Function states",LOADED=N_loaded,TOTAL=N_total)
 !
 if (WF%space=='R')  wf_grid_size=rho_map_size
 if (WF%space=='G')  wf_grid_size=wf_ng
 if (WF%space=='C')  wf_grid_size=wf_ncx
 if (WF%space=='B')  wf_grid_size=wf_ncx
 !
 if (WF%space=='R')  then
   YAMBO_ALLOC(WF%r,(wf_grid_size,n_spinor,WF%N))
   WF%r=wfcmplx(cZERO,kind=SP)
   if (have_cuda) then
     YAMBO_ALLOC_MOLD(WF%r_d,WF%r)
     WF%r_d=wfcmplx(cZERO,kind=SP)
   endif
 else
   YAMBO_ALLOC(WF%c,(wf_grid_size,n_spinor,WF%N))
   WF%c=cZERO
   if (have_cuda) then
     YAMBO_ALLOC_MOLD(WF%c_d,WF%c)
     WF%c_d=cZERO
   endif
 endif
 !
 YAMBO_ALLOC(WF%index,(WF%b(2),WF%k(2),WF%sp_pol(2)))
 !AMBO_ALLOC(WF%index,(WF%b(1):WF%b(2),WF%k(1):WF%k(2),WF%sp_pol(1):WF%sp_pol(2)))
 WF%index=0
 !
end subroutine
