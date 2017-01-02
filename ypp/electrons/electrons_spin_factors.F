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
subroutine electrons_spin_factors(kpts,bands,spin_factors)
 !
 use pars,           ONLY:SP
 use wave_func,      ONLY:WF
 use YPP,            ONLY:WF_ref
 use FFT_m,          ONLY:fft_size
 use LIVE_t,         ONLY:live_timing
 use interfaces,     ONLY:PARALLEL_index,WF_load
 use parallel_m,     ONLY:PP_indexes,myid,PP_redux_wait,PP_indexes_reset
 use wrapper_omp,    ONLY:Vstar_dot_V_omp
 !
 implicit none
 !
 integer,       intent(in)  :: kpts(2),bands(2)
 real(SP),      intent(out) :: spin_factors(bands(1):bands(2),kpts(1):kpts(2),2)
 !
 ! Work Space
 ! 
 integer               :: ik,ib,i_wf(2),ib_ref,ik_ref
 real(SP)              :: ref_dir(2,2),v(2)
 type(PP_indexes)      :: px
 !
 ref_dir(:,1)=(/1._SP,0._SP/)
 ref_dir(:,2)=(/0._SP,1._SP/)
 !
 ib_ref=WF_ref(1)
 ik_ref=WF_ref(2)
 !
 if(ib_ref>0.and.ik_ref>0) then
   !
   call WF_load(WF,0,1,(/ib_ref,ib_ref/),(/ik_ref,ik_ref/),title='-Spinor Factor',impose_free_and_alloc=.TRUE.)
   !
   i_wf(1)=WF%index(ib_ref,ik_ref,1)
   i_wf(2)=WF%index(ib_ref,ik_ref,2)
   v(1)=real(Vstar_dot_V_omp( fft_size, WF%c(:, i_wf(1) ), WF%c(:,i_wf(1)) ),SP)
   v(2)=real(Vstar_dot_V_omp( fft_size, WF%c(:, i_wf(2) ), WF%c(:,i_wf(2)) ),SP)
   !
   call WF_free(WF)
   !
   ref_dir(:,1)=v(:)/sqrt(dot_product(v,v))
   !
   v=ref_dir(:,1)*dot_product(ref_dir(:,1),ref_dir(:,2))
   !
   if(sqrt(dot_product(v,v))>1.E-5) then
     v(:)=-v(:)+ref_dir(:,2)
     ref_dir(:,2)=v(:)/sqrt(dot_product(v,v))
   endif
   !
 endif
 !
 call PP_indexes_reset(px)
 call PARALLEL_index(px,(/kpts(2)-kpts(1)+1,bands(2)-bands(1)+1/))
 !
 call live_timing('Spinor Components',px%n_of_elements(myid+1))
 !
 spin_factors=0._SP
 !
 do ik=kpts(1),kpts(2)
   if( .not.any(px%element_2D(ik-kpts(1)+1,:)) ) cycle
   call WF_load(WF,0,1,bands,(/ik,ik/),title='-Spinor-factor',impose_free_and_alloc=.TRUE.)
   do ib=bands(1),bands(2)
     if( .not. px%element_2D(ik-kpts(1)+1,ib-bands(1)+1) ) cycle
     i_wf(:)=WF%index(ib,ik,:)
     v(1)=real(Vstar_dot_V_omp( fft_size, WF%c(:, i_wf(1) ), WF%c(:,i_wf(1)) ),SP)
     v(2)=real(Vstar_dot_V_omp( fft_size, WF%c(:, i_wf(2) ), WF%c(:,i_wf(2)) ),SP)
     spin_factors(ib,ik,1)=abs(dot_product(v,ref_dir(:,1)))
     spin_factors(ib,ik,2)=abs(dot_product(v,ref_dir(:,2)))
     call live_timing(steps=1)
   enddo
   call WF_free(WF)
 enddo
 call live_timing()
 !
 call PP_redux_wait(spin_factors)
 !
end subroutine
