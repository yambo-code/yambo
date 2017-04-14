!
!        Copyright (C) 2000-2017 the YAMBO team
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
subroutine WF_free(WF,keep_fft)
 !
 use wave_func,ONLY:WAVEs,QUIET_free
 use memory_m, ONLY:mem_est
 !
 implicit none
 !
 type(WAVEs)       :: WF
 logical, optional :: keep_fft 
 !
 ! Work-space
 !
 logical           :: keep_fft_
 !
 keep_fft_=.false.
 if (present(keep_fft)) keep_fft_=keep_fft
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
 if(.not.keep_fft_.and.WF%space=="R") call fft_free()
 !
 WF%k=0
 WF%b=0
 WF%spin=0
 WF%space=' '
 WF%to_load=.TRUE.
 !
end subroutine
