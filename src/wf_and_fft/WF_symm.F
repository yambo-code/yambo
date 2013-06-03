!
! Copyright (C) 2000-2013 D. Sangalli and the YAMBO team 
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
function WF_symm(ifft,isc)
 !
 use pars,          ONLY:SP
 use wave_func,     ONLY:wf,wf_state
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
 integer     :: i_wf(n_spinor)
 !
 i_wf(1)=wf_state(isc(1),isc(2),isc(4))
 if(n_spinor==2)  i_wf(2)=wf_state(isc(1),isc(2),2)
 !
 if(isc(3)==idt_index) then
   WF_symm(1)=wf(ifft,i_wf(1))
   if(n_spinor==2) WF_symm(2)=wf(ifft,i_wf(2))
   return
 endif
 !
 if(n_spinor==1) WF_symm(1)=wf(fft_rot_r(isc(3),ifft),i_wf(1))
 !
 if (n_spinor==2) then
   WF_symm(1)=spin_sop(1,1,isc(3))*wf(fft_rot_r(isc(3),ifft),i_wf(1))+ &
&                    spin_sop(1,2,isc(3))*wf(fft_rot_r(isc(3),ifft),i_wf(2))
   WF_symm(2)=spin_sop(2,1,isc(3))*wf(fft_rot_r(isc(3),ifft),i_wf(1))+ &
&                    spin_sop(2,2,isc(3))*wf(fft_rot_r(isc(3),ifft),i_wf(2))
 endif
 !
 if(isc(3)>nsym/(i_time_rev+1)) WF_symm=conjg(WF_symm)
 !
end function
