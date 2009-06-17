!
! Copyright (C) 2000-2008 A. Marini and the YAMBO team
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
 ! gFsq matrices
 !
 integer              :: gsqF_energy_steps
 real(SP),allocatable :: gsqF_fan(:,:,:,:)
 real(SP),allocatable :: gsqF_dw(:,:,:)
 real(SP),allocatable :: gsqF_ca_corr(:,:,:,:)
 real(SP),allocatable :: gsqF_life_bose(:,:,:,:)
 real(SP),allocatable :: gsqF_life_f(:,:,:,:)
 !
 ! el-ph Self-Energy
 !
 integer   :: QP_PH_n_G_bands
 real(SP)  :: PH_null_E_rgn
 !
 contains
   !
   subroutine elph_global_alloc(what)
     use memory_m,  ONLY:mem_est
     use QP_m,      ONLY:QP_n_states
     use R_lattice, ONLY:nkbz,nqibz
     character(*)      ::what
     integer           ::alloc_err(5)
     !
     if (what=='gFsq'.and..not.allocated(gsqF_fan)) then
       allocate(gsqF_fan(QP_n_states,elph_nq_used,ph_modes,gsqF_energy_steps))
       allocate(gsqF_dw(QP_n_states,elph_nq_used,ph_modes))
       allocate(gsqF_ca_corr(QP_n_states,elph_nq_used,ph_modes,gsqF_energy_steps))
       allocate(gsqF_life_bose(QP_n_states,elph_nq_used,ph_modes,gsqF_energy_steps))
       allocate(gsqF_life_f(QP_n_states,elph_nq_used,ph_modes,gsqF_energy_steps))
       gsqF_fan=0.
       gsqF_dw=0.
       gsqF_ca_corr=0.
       gsqF_life_bose=0.
       gsqF_life_f=0.
       call mem_est('gsqF_residuals',(/4*size(gsqF_fan),size(gsqF_dw)/),(/SP,SP/))
     endif
     if (what=='gkkp'.and..not.allocated(elph_gkkp)) then
       allocate(elph_gkkp(nkbz,ph_modes,elph_nb,elph_nb),stat=alloc_err(1))
       allocate(elph_DW(nkbz,ph_modes,elph_nb,elph_nb),stat=alloc_err(2))
       allocate(f_k_plus_q(elph_nb,nkbz,1),stat=alloc_err(3))
       call mem_est("elph_gkkp elph_DW f_k_plus_q",&
&                   (/size(elph_gkkp),size(elph_DW),&
&                     size(f_k_plus_q)/),&
&                   (/2*SP,SP/),errors=alloc_err)
     endif
     !
     if (.not.allocated(ph_freqs_sq)) then
       if (elph_use_q_grid) then
         allocate(ph_freqs_sq(nqibz,ph_modes))
       else
         allocate(ph_freqs_sq(elph_nq,ph_modes))
       endif
       allocate(E_k_plus_q(elph_nb,nkbz,1))
       call mem_est("ph_freqs_sq E_k_plus_q",(/size(ph_freqs_sq),size(E_k_plus_q)/),&
&                                            (/SP,SP/))
     endif
     !
     if (.not.allocated(ph_qpt)) then
       allocate(ph_qpt(elph_nq,3))
       call mem_est("ph_qpt",(/size(ph_qpt)/),(/SP/))
     endif
     !
   end subroutine
   !
   subroutine elph_global_free()
     use memory_m,  ONLY:mem_est
     if (allocated(gsqF_fan)) then
       deallocate(gsqF_fan,gsqF_dw,gsqF_ca_corr,gsqF_life_bose,gsqF_life_f)
       call mem_est("gsqF_residuals")
     endif
     if (allocated(elph_gkkp)) then
       deallocate(elph_gkkp,elph_DW,f_k_plus_q)
       call mem_est("elph_gkkp elph_DW f_k_plus_q")
     endif
     if (allocated(ph_freqs_sq)) then
       deallocate(ph_freqs_sq,E_k_plus_q)
       call mem_est("ph_freqs_sq f_k_plus_q")
     endif
     if (allocated(ph_qpt)) then
       deallocate(ph_qpt)
       call mem_est("ph_qpt")
     endif
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
