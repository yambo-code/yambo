!
! License-Identifier: GPL
!
! Copyright (C) 2021 The Yambo Team
!
! Authors (see AUTHORS file for details): AM FP
!
subroutine ELPH_databases_IO_freqs(ph_file,n_q,q_pt,ph_freqs)
 !
 use pars,           ONLY:schlen,SP 
 use D_lattice,      ONLY:n_atoms
 use units,          ONLY:HA2CMm1
 !
 implicit none
 !
 character(schlen), intent(in)   :: ph_file
 integer,      intent(inout)     :: n_q
 real(SP), optional, intent(out) :: q_pt(n_q,3)
 real(SP), optional, intent(out) :: ph_freqs(3*n_atoms,n_q)
 !
 ! Work Space
 !
 integer :: io_unit=99
 integer :: i1,i_q,ph_modes
 !
 ph_modes=3*n_atoms
 !
 if(.not.present(q_pt).and..not.present(ph_freqs)) then
   !
   ! Return the number of q-points
   !
   open(unit=io_unit,file=trim(ph_file))
   read(io_unit,*,end=100)
   n_q=0
   do 
     read(io_unit,*,end=100)  ! read q-point
     do i1=1,ph_modes,6
       read(io_unit,*,end=100)  ! frequencies
     enddo
     n_q=n_q+1
   enddo
   !
   100 close(io_unit)
   !
 else
   !
   ! Fill q_pt and ph_freq without any conversion or check
   !
   open(unit=io_unit,file=trim(ph_file))
   read(io_unit,*)
   do i_q=1,n_q
     read(io_unit,*)  q_pt(i_q,:) ! read q-point
     do i1=1,ph_modes,6
       read(io_unit,*)  ph_freqs(i1:min(i1+5,ph_modes),i_q)  ! frequencies
     enddo
   enddo
   !
   ph_freqs=ph_freqs/HA2CMm1
   !
   close(io_unit)
   !
 endif
 !
end subroutine
