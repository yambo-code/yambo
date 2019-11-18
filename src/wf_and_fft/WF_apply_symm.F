!
!        Copyright (C) 2000-2019 the YAMBO team
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
subroutine WF_apply_symm(isc,WF_symm)
 !
 use pars,          ONLY:SP
 use wave_func,     ONLY:WF
 use FFT_m,         ONLY:fft_rot_r,fft_size
 use stderr,        ONLY:intc
 use parallel_m,    ONLY:myid
 use electrons,     ONLY:n_spinor
 use D_lattice,     ONLY:nsym,spin_sop,i_time_rev,idt_index
 !
 implicit none
 !
 integer,     intent(in)  :: isc(4)
 complex(SP), intent(out) :: WF_symm(fft_size,n_spinor)
 !
 integer     :: i_wf
#if defined _OPENMP
 integer     :: ifft, i_spinor
#endif
 !
 i_wf=WF%index(isc(1),isc(2),isc(4))
#if defined _MPI
 if (i_wf==0) call error('EMPTY WF index @ b:'//trim(intc(isc(1)))//' k:'//&
&                          trim(intc(isc(2)))//' spin:'//trim(intc(isc(4)))//&
&                          ' ID:'//trim(intc(myid)))
#endif
 !
#if defined _OPENMP
 if(isc(3)==idt_index) then
!$omp parallel do default(shared), private(ifft,i_spinor)
   do i_spinor=1,n_spinor
     do ifft = 1, fft_size
       WF_symm(ifft,i_spinor)=WF%c(ifft,i_spinor,i_wf)
     enddo
   enddo
!$omp end parallel do
   return
 endif
 !
 if (n_spinor==1) then
   !
!$omp parallel do default(shared), private(ifft)
   do ifft = 1, fft_size
     WF_symm(ifft,1)=WF%c(fft_rot_r(ifft,isc(3)),1,i_wf)
   enddo
!$omp end parallel do
   !
 endif
 !
 if (n_spinor==2) then
!$omp parallel default(shared), private(ifft)
!$omp do
   do ifft = 1, fft_size
     WF_symm(ifft,1)=spin_sop(1,1,isc(3))*WF%c(fft_rot_r(ifft,isc(3)),1,i_wf)+ &
                     spin_sop(1,2,isc(3))*WF%c(fft_rot_r(ifft,isc(3)),2,i_wf)
   enddo
!$omp end do
!$omp do
   do ifft = 1, fft_size
     WF_symm(ifft,2)=spin_sop(2,1,isc(3))*WF%c(fft_rot_r(ifft,isc(3)),1,i_wf)+ &
                     spin_sop(2,2,isc(3))*WF%c(fft_rot_r(ifft,isc(3)),2,i_wf)
   enddo
!$omp end do
!$omp end parallel
 endif
 !
 if(isc(3)>nsym/(i_time_rev+1)) then
!$omp parallel do default(shared), private(ifft,i_spinor)
   do i_spinor = 1, n_spinor
     do ifft = 1, fft_size
       WF_symm(ifft,i_spinor)=conjg(WF_symm(ifft,i_spinor))
     enddo
   enddo
!$omp end parallel do
 endif
 !
 ! ELSE on if defined OPENMP
 !
#else
 !
 if(isc(3)==idt_index) then
   WF_symm(:,1)=WF%c(:,1,i_wf)
   if(n_spinor==2) WF_symm(:,2)=WF%c(:,2,i_wf)
   return
 endif
 if (n_spinor==1) WF_symm(:,1)=WF%c(fft_rot_r(:,isc(3)),1,i_wf)
 if (n_spinor==2) then
   WF_symm(:,1)=spin_sop(1,1,isc(3))*WF%c(fft_rot_r(:,isc(3)),1,i_wf)+ &
&                    spin_sop(1,2,isc(3))*WF%c(fft_rot_r(:,isc(3)),2,i_wf)
   WF_symm(:,2)=spin_sop(2,1,isc(3))*WF%c(fft_rot_r(:,isc(3)),1,i_wf)+ &
&                    spin_sop(2,2,isc(3))*WF%c(fft_rot_r(:,isc(3)),2,i_wf)
 endif
 !
 if(isc(3)>nsym/(i_time_rev+1)) WF_symm=conjg(WF_symm)
 !
#endif
 !
end subroutine
