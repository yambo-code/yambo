!
! License-Identifier: GPL
!
! Copyright (C) 2013 The Yambo Team
!
! Authors (see AUTHORS file for details): DS
!
function WF_symm(ifft,isc)
 !
 use pars,          ONLY:SP
 use wave_func,     ONLY:WF
 use FFT_m,         ONLY:fft_rot_r
 use electrons,     ONLY:n_spinor
 use D_lattice,     ONLY:nsym,spin_sop,i_time_rev,idt_index
 !
 implicit none
 !
 complex(SP), dimension(n_spinor) :: WF_symm
 !
 integer     :: isc(4)
 !
 integer     :: ifft
 integer     :: i_wf
 !
 i_wf=WF%index(isc(1),isc(2),isc(4))
 !
 if(isc(3)==idt_index) then
   WF_symm(:)=WF%c(ifft,:,i_wf)
   return
 endif
 !
 if(n_spinor==1) WF_symm(1)=WF%c(fft_rot_r(ifft,isc(3)),1,i_wf)
 !
 if (n_spinor==2) then
   WF_symm(1)=spin_sop(1,1,isc(3))*WF%c(fft_rot_r(ifft,isc(3)),1,i_wf)+ &
&                    spin_sop(1,2,isc(3))*WF%c(fft_rot_r(ifft,isc(3)),2,i_wf)
   WF_symm(2)=spin_sop(2,1,isc(3))*WF%c(fft_rot_r(ifft,isc(3)),1,i_wf)+ &
&                    spin_sop(2,2,isc(3))*WF%c(fft_rot_r(ifft,isc(3)),2,i_wf)
 endif
 !
 if(isc(3)>nsym/(i_time_rev+1)) WF_symm=conjg(WF_symm)
 !
end function
