!
! Copyright (C) 2000-2005 A. Marini and the SELF team
!         http://www.fisica.uniroma2.it/~self
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
module ELPH
 !
 use pars, ONLY:SP
 !
 implicit none
 integer   :: ph_modes
 integer   :: elph_nb
 integer   :: elph_nq
 integer   :: elph_nq_used
 real(SP)  :: max_ph_freq
 logical   :: elph_use_q_grid
 real(SP),    allocatable :: ph_qpt(:,:)
 real(SP),    allocatable :: ph_freqs_sq(:,:)
 real(SP),    allocatable :: elph_DW(:,:,:,:)
 real(SP),    allocatable :: E_k_plus_q(:,:,:)
 real(SP),    allocatable :: f_k_plus_q(:,:,:)
 complex(SP), allocatable :: elph_gkkp(:,:,:,:)
 !
 ! el-ph Self-Energy
 !
 integer   :: QP_PH_n_G_bands
 real(SP)  :: PH_damping
 real(SP)  :: PH_null_E_rgn
 !
 contains
   !
   subroutine elph_gkkp_alloc()
     use memory_m,  ONLY:mem_est
     use R_lattice, ONLY:nkbz,nqibz
     implicit none
     integer     ::alloc_err(5)
     if (allocated(ph_freqs_sq)) return
     if (elph_use_q_grid) then
       allocate(ph_freqs_sq(nqibz,ph_modes),stat=alloc_err(1))
     else
       allocate(ph_freqs_sq(elph_nq,ph_modes),stat=alloc_err(1))
     endif
     allocate(elph_gkkp(nkbz,ph_modes,elph_nb,elph_nb),stat=alloc_err(2))
     allocate(elph_DW(nkbz,ph_modes,elph_nb,elph_nb),stat=alloc_err(3))
     allocate(E_k_plus_q(elph_nb,nkbz,1),stat=alloc_err(4))
     allocate(f_k_plus_q(elph_nb,nkbz,1),stat=alloc_err(5))
     call mem_est("ph_freqs_sq elph_gkkp elph_DW E_k_plus_q f_k_plus_q",&
&                 (/size(ph_freqs_sq),size(elph_gkkp),size(elph_DW),&
&                   size(E_k_plus_q), size(f_k_plus_q)/),&
&                 (/SP, 2*SP,SP,SP,SP/),errors=alloc_err)
     if (.not.allocated(ph_qpt)) then
       allocate(ph_qpt(elph_nq,3))
       call mem_est("ph_qpt")
     endif
   end subroutine
   !
   subroutine elph_gkkp_free()
     use memory_m,  ONLY:mem_est
     implicit none
     deallocate(ph_freqs_sq,elph_gkkp,elph_DW,E_k_plus_q,f_k_plus_q,ph_qpt)
     call mem_est("ph_freqs_sq elph_gkkp elph_DW E_k_plus_q f_k_plus_q ph_qpt")
   end subroutine
   !
   subroutine setup_k_plus_q_levels(E)
     !
     use electrons,  ONLY:spin_occ
     use D_lattice,  ONLY:Tel
     use R_lattice,  ONLY:nkbz
     use functions,  ONLY:Fermi_fnc
     !
     real(SP)    ::E
     integer     ::ib,ik
     do ib=1,elph_nb
       do ik=1,nkbz
         E_k_plus_q(ib,ik,1)=E_k_plus_q(ib,ik,1)-E
         f_k_plus_q(ib,ik,1)=spin_occ*Fermi_fnc(E_k_plus_q(ib,ik,1),Tel)
       enddo 
     enddo
     !
   end subroutine
   !
end module ELPH
