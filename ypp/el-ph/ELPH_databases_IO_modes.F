!
! License-Identifier: GPL
!
! Copyright (C) 2021 The Yambo Team
!
! Authors (see AUTHORS file for details): CA AM FP
!
subroutine ELPH_databases_IO_modes(ph_file,n_q,q_pt,ph_freqs,ph_pol)
 !
 use pars,           ONLY:schlen,SP 
 use D_lattice,      ONLY:n_atoms
 use units,          ONLY:HA2THZ
 use stderr,         ONLY:intc
 use com,            ONLY:msg
 !
 implicit none
 !
 character(schlen), intent(in)   :: ph_file
 integer,     intent(inout)  :: n_q
 real(SP),    optional,   intent(out) :: q_pt(n_q,3)
 real(SP),    optional,   intent(out) :: ph_freqs(3*n_atoms,n_q)
 complex(SP), optional,   intent(out) :: ph_pol(n_q,3*n_atoms,n_atoms,3)
 !
 ! Work Space
 !
 integer :: io_unit=99
 integer :: i1,iq,ic,ia,ph_modes
 character(1)      :: ws_ch(2)
 real(SP)          :: w_vec_pot(6),ph_q(3)
 logical, external  ::file_exists
 !
 ph_modes=3*n_atoms
 !
 ! Read polarization vectors
 !
 if(file_exists(trim(ph_file))) then
   open(unit=io_unit,file=trim(ph_file))
 else
   call error('File '//trim(ph_file)//' not found ')
 endif
 !
 if(.not.present(q_pt)) then
   !
   n_q=0   
   !
   do
     !
     read(io_unit,*,end=100)
     read(io_unit,*,end=100)
     read(io_unit,*,end=100) ws_ch(1),ws_ch(2),ph_q(:)
     read(io_unit,*,end=100)
     !
     do i1=1,ph_modes
       read(io_unit,*,end=100)
       do ia=1,n_atoms
         read(io_unit,*,end=100) ws_ch(1),w_vec_pot
       enddo
     enddo
     !
     n_q=n_q+1
     !
   enddo
   !
100 call msg('sr','Number of q-points found :'//intc(n_q))
   !
 else
   !
   do iq=1,n_q
     !
     read(io_unit,*)
     read(io_unit,*)
     read(io_unit,*) ws_ch(1),ws_ch(2),q_pt(iq,:)
     read(io_unit,*)
     !
     do i1=1,ph_modes
       ! 
       read(io_unit, '(25x,f10.6)') ph_freqs(i1,iq)     ! Reads the eigenfreq in THz using format     
       !
       do ia=1,n_atoms
         read(io_unit,*) ws_ch(1),w_vec_pot
         forall(ic=1:3) ph_pol(iq,i1,ia,ic)=cmplx(w_vec_pot(2*(ic-1)+1),w_vec_pot(2*ic),SP)
       enddo
       !
     enddo
     !
   enddo
   !
   ph_freqs = ph_freqs/HA2THZ
   !
 endif
 !
 close(io_unit)
 !
end subroutine
