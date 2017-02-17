!
!        Copyright (C) 2000-2016 the YAMBO team
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
subroutine WF_alloc(WF)
 !
 use pars,          ONLY:cZERO
 use com,           ONLY:error
 use wave_func,     ONLY:WAVEs,states_to_load,wf_ng,wf_ncx,QUIET_alloc
 use memory_m,      ONLY:mem_est
 use FFT_m,         ONLY:fft_size
#if defined _FFTW
 use FFT_m,         ONLY:fftw_plan
#endif
 use interfaces,    ONLY:PARALLEL_live_message
 implicit none
 !
 type(WAVEs)           :: WF
 !
 ! Work Space
 !
 integer :: err,ik,ib,is,N_total
 !
 ! Distributed allocation
 !
 if(allocated(states_to_load)) then
   !
   WF%N=0
   N_total=0
   do ib=WF%b(1),WF%b(2)
     do ik=WF%k(1),WF%k(2)
       do is=WF%spin(1),WF%spin(2)
         N_total=N_total+1
         if (states_to_load(ib,ik,is)) WF%N=WF%N+1
       enddo
     enddo
   enddo
   !
 else
   !
   WF%N=(WF%b(2)-WF%b(1)+1)*(WF%k(2)-WF%k(1)+1)*(WF%spin(2)-WF%spin(1)+1)
   N_total=WF%N
   !
 endif
 !
 if (WF%space=="R") allocate(WF%c(fft_size,WF%N),stat=err)
 if (WF%space=="G") allocate(WF%c(wf_ng,   WF%N),stat=err)
 if (WF%space=="C") allocate(WF%c(wf_ncx,  WF%N),stat=err)
 if (WF%space=="B") allocate(WF%c(wf_ncx,  WF%N),stat=err)
 !
 if (err/=0) call error(' Error allocating WFs')
 !
 if (WF%space/="B") call mem_est('WF'       ,(/size(WF%c)/),errors=(/err/),quiet=QUIET_alloc)
 if (WF%space=="B") call mem_est('WF_buffer',(/size(WF%c)/),errors=(/err/),quiet=QUIET_alloc)
 !
 call PARALLEL_live_message("Wave-Function states",LOADED=WF%N,TOTAL=N_total)
 !
 allocate(WF%index(WF%b(2),WF%k(2),WF%spin(2)),stat=err)
 !
 WF%c=cZERO
 WF%index=0
#if defined _FFTW
 fftw_plan=0
#endif
 !
end subroutine
