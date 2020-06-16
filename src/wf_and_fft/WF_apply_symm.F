!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS, AF
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
!#include<dev_defs.h>
 !
subroutine DEV_SUB_ALT(WF_apply_symm)(isc,WF_symm)
 !
 use pars,          ONLY:SP
 use wave_func,     ONLY:WF
 use FFT_m,         ONLY:DEV_VAR(fft_rot_r),fft_size
 use stderr,        ONLY:intc
 use parallel_m,    ONLY:myid
 use electrons,     ONLY:n_spinor
 use D_lattice,     ONLY:nsym,DEV_VAR(spin_sop),i_time_rev,idt_index
 use deviceXlib_m
 !
 implicit none
 !
 integer,     intent(in)  :: isc(4)
 complex(SP), intent(out) DEV_ATTR :: WF_symm(fft_size,n_spinor)
 !
 complex(SP), pointer     DEV_ATTR :: WF_p(:,:,:)
 integer  :: i_wf
 integer  :: ifft,i_spinor,isymm

 i_wf=WF%index(isc(1),isc(2),isc(4))
 !
#if defined _MPI
 if (i_wf==0) call error('EMPTY WF index @ b'//trim(intc(isc(1)))//' k'//&
&                         trim(intc(isc(2)))//' spin'//trim(intc(isc(4)))//&
&                         ' ID'//trim(intc(myid)))
#endif
 !
 WF_p => DEV_VAR(WF%c)
 isymm = isc(3)
 !
 if(isc(3)==idt_index) then
   !
   call dev_memcpy(WF_symm,DEV_VAR(WF%c)(:,:,i_wf))
   !
 endif
 !
 if(n_spinor==1) then
   !
#ifdef _CUDA_LOC   
   !$cuf kernel do(1) <<<*,*>>>
#else
   !$omp parallel do default(shared), private(ifft)
#endif
   do ifft = 1, fft_size
     WF_symm(ifft,1)=WF_p(DEV_VAR(fft_rot_r)(ifft,isymm),1,i_wf)
   enddo
   !
 endif
 !
 if (n_spinor==2) then
   !
#ifdef _CUDA_LOC   
   !$cuf kernel do(1) <<<*,*>>>
#else
   !$omp parallel do default(shared), private(ifft)
#endif
   do ifft = 1, fft_size
     WF_symm(ifft,1)=DEV_VAR(spin_sop)(1,1,isymm)*WF_p(DEV_VAR(fft_rot_r)(ifft,isymm),1,i_wf)+ &
                     DEV_VAR(spin_sop)(1,2,isymm)*WF_p(DEV_VAR(fft_rot_r)(ifft,isymm),2,i_wf)
     WF_symm(ifft,2)=DEV_VAR(spin_sop)(2,1,isymm)*WF_p(DEV_VAR(fft_rot_r)(ifft,isymm),1,i_wf)+ &
                     DEV_VAR(spin_sop)(2,2,isymm)*WF_p(DEV_VAR(fft_rot_r)(ifft,isymm),2,i_wf)
   enddo
   !
 endif
 !
 ! take care of time reversal if needed
 !
 if(isc(3)>nsym/(i_time_rev+1)) call dev_conjg(WF_symm)
 !
end subroutine DEV_SUB_ALT(WF_apply_symm)

