!
! Copyright (C) 2000-2010 D. Sangalli and the YAMBO team 
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
subroutine MAG_A_phase()
 !
 use pars,           ONLY:SP,pi
 use units,          ONLY:SPEED_OF_LIGHT,AU2Tesla,BOHR
 use R_lattice,      ONLY:nkibz
 use SC,             ONLY:SC_bands
 use com,            ONLY:msg,error
 use stderr,         ONLY:intc
 use magnetic,       ONLY:Bz,x,MAG_radius,phase_radius,n_loops
 use wave_func,      ONLY:wf,WF_load,WF_free,wf_state
 use FFT_m,          ONLY:fft_size
 use electrons,      ONLY:default_nel,n_sp_pol
 !
 implicit none
 !
 real(SP)    :: r2_ref,r2_cyl(fft_size)
 complex(SP) :: wf_r2(fft_size)
 integer     :: ib_ref,ik_ref,is_ref,iwf
 !
#if defined _DOUBLE
 complex(SP)  :: zdotc
#else
 complex(SP)  :: cdotc
#endif
 !
 n_loops=0
 r2_cyl(:)=x(:,1)**2+x(:,2)**2
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
 call WF_load(0,1,(/ib_ref,ib_ref/),(/ik_ref,ik_ref/),title='-MAG_GAUGE',impose_free_and_alloc=.TRUE.)
 !
 iwf=wf_state(ib_ref,ik_ref,is_ref)
 wf_r2(:)=r2_cyl(:)*wf(:,iwf)
#if defined _DOUBLE
 r2_ref=real(zdotc(fft_size, wf(:,iwf),1,wf_r2,1))
#else
 r2_ref=real(cdotc(fft_size, wf(:,iwf),1,wf_r2,1))
#endif
 !
 n_loops=int(r2_ref*Bz/(2*SPEED_OF_LIGHT)+0.5)
 !
 call WF_free()
 !
 if(n_loops==0) call msg('rs','[MAG] Phase trick: check done, not needed')
 if(n_loops> 0) call msg('rs','[MAG] Applying phase trick. Nloops='//trim(intc(n_loops))//' ')
 !
end subroutine
