!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): CA, AM, FP
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
subroutine ELPH_databases_IO_pol(ph_pol_file,n_q,q_pt,ph_pol)
 !
 use pars,           ONLY:schlen,SP 
 use D_lattice,      ONLY:n_atoms
 use units,          ONLY:HA2CMm1
 use vec_operate,    ONLY:v_is_zero
 !
 implicit none
 !
 character(schlen), intent(in)   :: ph_pol_file
 integer,       intent(in)  :: n_q
 real(SP),      intent(in) :: q_pt(n_q,3)
 real(SP),      intent(out) :: ph_pol(n_q,3*n_atoms,n_atoms,3)
 !
 ! Work Space
 !
 integer :: io_unit=99
 integer :: i1,iq,ic,ia,ph_modes
 character(1)      :: ws_ch(2)
 real(SP)          :: w_vec_pot(6),ph_q(3)
 !
 ph_modes=3*n_atoms
 !
 ! Read polarization vectors
 !
 open(unit=io_unit,file=trim(ph_pol_file))
 !
 do iq=1,n_q
   !
   read(io_unit,*)
   read(io_unit,*)
   read(io_unit, *) ws_ch(1),ws_ch(2),ph_q(:)
   !
   ! Q-points should be the same of the frequency file
   !
   if(.not.v_is_zero(q_pt(iq,:)-ph_q(:)))  call error("Incorrect q-point in "//trim(ph_pol_file))
   !
   read(io_unit,*)
   !
   do i1=1,ph_modes
     read(io_unit,*) 
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
 close(io_unit)
 !
end subroutine
