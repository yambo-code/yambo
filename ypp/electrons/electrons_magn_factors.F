!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details):
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
subroutine electrons_magn_factors(kpts,bands,magn_factors)
 !
 use pars,           ONLY:SP
 use wave_func,      ONLY:WF
 use FFT_m,          ONLY:fft_size
 use LIVE_t,         ONLY:live_timing
 use interfaces,     ONLY:PARALLEL_index,WF_load,WF_free
 use parallel_m,     ONLY:PP_indexes,myid,PP_redux_wait,PP_indexes_reset
 use wrapper_omp,    ONLY:Vstar_dot_V_omp
 !
 implicit none
 !
 integer,       intent(in)  :: kpts(2),bands(2)
 real(SP),      intent(out) :: magn_factors(bands(1):bands(2),kpts(1):kpts(2),3)
 !
 ! Work Space
 ! 
 integer               :: ik,ib,i_wf(2)
 type(PP_indexes)      :: px
 !
 call PP_indexes_reset(px)
 call PARALLEL_index(px,(/kpts(2)-kpts(1)+1,bands(2)-bands(1)+1/))
 !
 call live_timing('Magn-factor',px%n_of_elements(myid+1))
 !
 magn_factors=0._SP
 !
 do ik=kpts(1),kpts(2)
   if( .not.any(px%element_2D(ik-kpts(1)+1,:)) ) cycle
   call WF_load(WF,0,1,bands,(/ik,ik/),title='-Magn-factor',impose_free_and_alloc=.TRUE.)
   do ib=bands(1),bands(2)
     if( .not. px%element_2D(ik-kpts(1)+1,ib-bands(1)+1) ) cycle
     i_wf=WF%index(ib,ik,1)
     ! x
     magn_factors(ib,ik,1)=real(Vstar_dot_V_omp( fft_size, WF%c(:, 1,i_wf), WF%c(:,2,i_wf) ),SP) &
&                         +real(Vstar_dot_V_omp( fft_size, WF%c(:, 2,i_wf), WF%c(:,1,i_wf) ),SP)
     ! y
     magn_factors(ib,ik,2)=aimag(Vstar_dot_V_omp( fft_size, WF%c(:,1,i_wf), WF%c(:,2,i_wf) )) &
&                         -aimag(Vstar_dot_V_omp( fft_size, WF%c(:,2,i_wf), WF%c(:,1,i_wf) ))
     ! z
     magn_factors(ib,ik,3)=real(Vstar_dot_V_omp( fft_size, WF%c(:,1,i_wf), WF%c(:,1,i_wf) ),SP) &
&                         -real(Vstar_dot_V_omp( fft_size, WF%c(:,2,i_wf), WF%c(:,2,i_wf) ),SP)
     !
     call live_timing(steps=1)
   enddo
   call WF_free(WF)
 enddo
 call live_timing()
 !
 call PP_redux_wait(magn_factors)
 !
end subroutine
