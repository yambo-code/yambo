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
module ELPH
 !
 use pars,       ONLY:SP
 use electrons,  ONLY:levels
 use D_lattice,  ONLY:n_atoms_species
 !
 implicit none
 integer   :: ph_modes
 integer   :: elph_nb
 integer   :: elph_nk_bz
 integer   :: elph_nDBs
 integer   :: elph_nDBs_used
 integer   :: elph_branches(2)
 real(SP)  :: W_debye
 logical   :: elph_use_q_grid
 logical   :: elph_grids_are_expanded=.FALSE.
 real(SP),    allocatable :: QP_OMS_Fan(:)
 real(SP),    allocatable :: QP_DW(:)
 real(SP),    allocatable :: ph_kpt_bz(:,:)
 real(SP),    allocatable :: ph_qpt(:,:)
 real(SP),    allocatable :: ph_freqs_sq(:,:)
 real(SP),    allocatable :: elph_DW(:,:,:,:)
 real(SP),    allocatable :: E_k_plus_q(:,:,:)
 real(SP),    allocatable :: f_k_plus_q(:,:,:)
 complex(SP), allocatable :: elph_gkkp(:,:,:,:)
 complex(SP), allocatable :: pol_vector(:,:,:)
 type(levels), SAVE       :: E_k_p_q
 !
 ! El_h Hamiltonian
 !
 integer   :: elph_Ham_bands(2)
 integer   :: elph_Ham_ik
 real(SP)  :: RES_tresh
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
 logical   :: eval_G_using_KK ! Perform KK of the spectral function
 real(SP)  :: FAN_deltaE_treshold
 real(SP)  :: DW_deltaE_treshold
 real(SP)  :: MAX_gkkp=0._SP
 !
 contains
   !
   subroutine elph_global_alloc(what)
     use memory_m,  ONLY:mem_est
     use parallel_m,ONLY:PAR_nQ
     use QP_m,      ONLY:QP_n_states
     use R_lattice, ONLY:nkbz,nqibz
     character(*)      ::what
     integer           ::alloc_err
     !
     select case (trim(what))
       !
       case ('gFsq')
         !
         if (.not.allocated(gsqF_fan)) then
           if (.not.eval_G_using_KK) then
             allocate(gsqF_fan(QP_n_states,PAR_nQ,ph_modes,gsqF_energy_steps),stat=alloc_err)
             call mem_est('gsqF_fan',(/size(gsqF_fan)/),(/SP/),errors=(/alloc_err/))
             allocate(gsqF_ca_corr(QP_n_states,PAR_nQ,ph_modes,gsqF_energy_steps),stat=alloc_err)
             call mem_est('gsqF_ca_corr',(/size(gsqF_ca_corr)/),(/SP/),errors=(/alloc_err/))
             gsqF_fan=0.
             gsqF_ca_corr=0.
           endif
           allocate(gsqF_dw(QP_n_states,PAR_nQ,ph_modes),stat=alloc_err)
           call mem_est('gsqF_dw',(/size(gsqF_dw)/),(/SP/),errors=(/alloc_err/))
           allocate(gsqF_life_bose(QP_n_states,PAR_nQ,ph_modes,gsqF_energy_steps),stat=alloc_err)
           call mem_est('gsqF_life_bose',(/size(gsqF_life_bose)/),(/SP/),errors=(/alloc_err/))
           allocate(gsqF_life_f(QP_n_states,PAR_nQ,ph_modes,gsqF_energy_steps),stat=alloc_err)
           call mem_est('gsqF_life_f',(/size(gsqF_life_f)/),(/SP/),errors=(/alloc_err/))
           gsqF_dw=0.
           gsqF_life_bose=0.
           gsqF_life_f=0.
         endif
         !
       case ('gkkp')
         !
         if (.not.allocated(elph_gkkp)) then
           allocate(elph_gkkp(nkbz,ph_modes,elph_nb,elph_nb),stat=alloc_err)
           call mem_est("GKKP",(/size(elph_gkkp)/),(/2*SP/),errors=(/alloc_err/))
           allocate(elph_DW(nkbz,ph_modes,elph_nb,elph_nb),stat=alloc_err)
           call mem_est("GKKP_DW",(/size(elph_DW)/),(/SP/),errors=(/alloc_err/))
           allocate(f_k_plus_q(elph_nb,nkbz,1))
         endif
         !
     end select
     !
     if (.not.allocated(ph_freqs_sq)) then
       allocate(ph_freqs_sq(elph_nDBs,ph_modes))
       ph_freqs_sq=0._SP
       allocate(E_k_plus_q(elph_nb,nkbz,1))
       call mem_est("ph_freqs_sq E_k_plus_q",(/size(ph_freqs_sq),size(E_k_plus_q)/),&
&                                            (/SP,SP/))
       !
       allocate(pol_vector(ph_modes,sum(n_atoms_species),3))
       call mem_est("pol_vector",(/size(pol_vector)/))
       !
     endif
     !
     if (.not.allocated(ph_qpt)) then
       allocate(ph_qpt(elph_nDBs,3))
       call mem_est("ph_qpt",(/size(ph_qpt)/),(/SP/))
       allocate(ph_kpt_bz(nkbz,3))
       call mem_est("ph_kpt_bz",(/size(ph_kpt_bz)/),(/SP/))
     endif
     !
   end subroutine
   !
   subroutine elph_global_free()
     use memory_m,  ONLY:mem_est
     if (allocated(gsqF_fan)) then
       deallocate(gsqF_fan,gsqF_dw,gsqF_ca_corr)
       call mem_est("gsqF_fan gsqF_dw gsqF_ca_corr")
     endif
     if (allocated(gsqF_life_f)) then
       deallocate(gsqF_life_bose,gsqF_life_f)
       call mem_est("gsqF_life_bose gsqF_life_f")
     endif
     if (allocated(elph_gkkp)) then
       deallocate(elph_gkkp,elph_DW,f_k_plus_q)
       call mem_est("elph_gkkp elph_DW f_k_plus_q")
     endif
     if (allocated(ph_freqs_sq)) then
       deallocate(ph_freqs_sq,E_k_plus_q)
       call mem_est("ph_freqs_sq f_k_plus_q")
     endif
     if (allocated(pol_vector)) then
       deallocate(pol_vector)
       call mem_est("pol_vector")
     endif
     if (allocated(ph_qpt)) then
       deallocate(ph_qpt,ph_kpt_bz)
       call mem_est("ph_qpt ph_kpt_bz")
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
