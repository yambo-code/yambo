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
subroutine WF_free(WF)
 !
 use wave_func,ONLY:WAVEs,QUIET_free
 use memory_m, ONLY:mem_est
 use FFT_m,    ONLY:fft_size,fft_dim,fft_dim_loaded,fft_g_table,&
&                    fft_rot_r,fft_rot_r_inv,fft_multiplier
#if defined _FFTW
 use FFT_m,    ONLY:fftw_plan
#endif
 type(WAVEs)      ::WF
 !
 if (.not.associated(WF%c)) return
 !
 deallocate(WF%c)
 if (WF%space/="B") call mem_est('WF'       ,quiet=QUIET_free)
 if (WF%space=="B") call mem_est('WF_buffer',quiet=QUIET_free)
 nullify(WF%c)
 !
 if (associated(WF%state)) then
   deallocate(WF%state)
   nullify(WF%state)
 endif 
 !
 if (WF%space=="R") then
   fft_size=0
   fft_dim=0
   fft_dim_loaded=0
   fft_multiplier=1
   if (allocated(fft_g_table)) then
     deallocate(fft_g_table,fft_rot_r,fft_rot_r_inv)
     call mem_est('FFT_g_tab FFT_rot')
   endif
#if defined _FFTW
   call dfftw_destroy_plan(fftw_plan)
   fftw_plan=0
#endif
 endif
 WF%k=0
 WF%b=0
 WF%spin=0
 WF%space=' '
 WF%to_load=.TRUE.
 !
end subroutine
