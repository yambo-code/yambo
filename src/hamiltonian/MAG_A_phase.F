!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS
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
subroutine MAG_A_phase()
 !
 use pars,           ONLY:SP
 use units,          ONLY:SPEED_OF_LIGHT
 use D_lattice,      ONLY:x_cc
 use com,            ONLY:msg
 use stderr,         ONLY:intc
 use magnetic,       ONLY:Bz,MAG_radius,n_loops
 use wave_func,      ONLY:WF
 use FFT_m,          ONLY:fft_size
 use interfaces,     ONLY:WF_load
 use wrapper,        ONLY:Vstar_dot_V
 !
 implicit none
 !
 real(SP)    :: r2_ref,r2_cyl(fft_size)
 complex(SP) :: wf_r2(fft_size)
 integer     :: ib_ref,ik_ref,is_ref,iwf
 !
 n_loops=0
 r2_cyl(:)=x_cc(:,1)**2+x_cc(:,2)**2
 !
 if(MAG_radius>1.E-5) then
   n_loops=int(MAG_radius**2*Bz/(2*SPEED_OF_LIGHT)+0.5)
   if(n_loops==0) call msg('rs','[MAG] Phase trick: check done, not needed')
   if(n_loops> 0) call msg('rs','[MAG] Applying phase trick. Nloops='//trim(intc(n_loops))//' ')
   return
 endif 
 !
 ik_ref=1
 ib_ref=1
 is_ref=1
 !
 call WF_load(WF,0,1,(/ib_ref,ib_ref/),(/ik_ref,ik_ref/),(/is_ref,is_ref/),title='-MAG_GAUGE',impose_free_and_alloc=.TRUE.)
 !
 iwf=WF%index(ib_ref,ik_ref,is_ref)
 wf_r2(:)=r2_cyl(:)*WF%c(:,is_ref,iwf)
 r2_ref=real(Vstar_dot_V(fft_size,WF%c(:,is_ref,iwf),wf_r2))
 !
 n_loops=int(r2_ref*Bz/(2*SPEED_OF_LIGHT)+0.5)
 !
 call WF_free(WF)
 !
 if(n_loops==0) call msg('rs','[MAG] Phase trick: check done, not needed')
 if(n_loops> 0) call msg('rs','[MAG] Applying phase trick. Nloops='//trim(intc(n_loops))//' ')
 !
end subroutine
