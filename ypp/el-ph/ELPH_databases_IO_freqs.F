!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, FP
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
function interger ELPH_databases_IO_freqs(ph_freqs_file,q_pt,ph_freq,n_q)
 !
 use pars,           ONLY:SP 
 use D_lattice,      ONLY:n_atoms
 !
 implicit none
 !
 integer :: n_modes = n_atoms*3
 integer,  optional, intent(in)  :: n_q
 real(SP), optional, intent(out) :: q_pt(n_q,3)
 real(SP), optional, intent(out) :: ph_freq(n_q,n_modes)
 !
 ! Work Space
 !
 integer :: io_unit=99
 integer :: i1
 !
 ELPH_databases_IO_freqs=0
 !
 if(.not.present(n_q)) then
   !
   ! Return the number of q-points
   !
   open(unit=io_unit,file=trim(ph_freqs_file))
   read(io_unit,*,end=100)
   do 
     read(io_unit,*,end=100)  ! read q-point
     do i1=1,ph_modes,6
       read(io_unit,*,end=100)  ! frequencies
     enddo
   enddo
   !
   100 close(io_unit)
   return ELPH_databases_IO_freq
   !
 else
   !
   ! Fill q_pt and ph_freq without any conversion or check
   !
   open(unit=io_unit,file=trim(ph_freqs_file))
   read(io_unit,*,end=100)
   do i_q=1,n_q
     read(io_unit,*,end=100)    q_pt(i_q,:) ! read q-point
     do i1=1,1+ceiling(real(n_modes)/6.)
       read(io_unit,*,end=100)  ph_freq(i_q,i1:min(i1+5,n_modes)  ! frequencies
     enddo
   enddo
   !
 endif
 !
end subroutine
