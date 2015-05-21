!
!        Copyright (C) 2000-2015 the YAMBO team
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
module electrons
 !
 use pars,  ONLY:SP,IP
 implicit none
 !
 real(SP) :: nel
 real(SP) :: default_nel
 real(SP) :: spin_occ
 real(SP) :: filled_tresh
 integer  :: n_bands
 integer  :: n_met_bands
 integer  :: n_full_bands
 !
 ! Spin support
 !
 !     n_spin n_sp_pol n_spinor n_spin_den 
 !       1        1        1       1              
 !       2        1        2       1          non collinear, v[rho] 
 !       2        1        2       4          non collinear, v[rho_{\sigma,\tau}] 
 !       2        2        1       2          collinear,     v[rho_{\sigma}]
 !
 ! Note that in the non collinear case n_spin is an index
 ! for the spinor components. It is not a Physical quantity.
 !
 integer  :: n_spin
 integer  :: n_sp_pol
 integer  :: n_spinor
 integer  :: n_spin_den
 !
 logical  :: l_spin_orbit
 !
 ! Average magnetization density
 !
 logical                  :: eval_magn
 real(SP)                 :: Total_magn(3)
 !
 type levels
   integer         :: nb
   integer         :: nk
   integer         :: nbf
   integer         :: nbm
   integer         :: bf
   integer         :: kf
   integer         :: sf
   real(SP)        :: E_Fermi
   real(SP)        :: E_dir_gap(2) ! 1-2 refers to spin polarizations
   real(SP)        :: E_ind_gap(2)
   real(SP)        :: E_VBM        ! Valence    Band Maximum
   real(SP)        :: E_CBm        ! Conduction Band minimum
   !
   ! QP corrections
   !
   integer,pointer :: QP_corrected(:,:,:)  => null()
   real(SP),   pointer :: E(:,:,:)         => null()
   real(SP),   pointer :: Eo(:,:,:)        => null()
   real(SP),   pointer :: E_dip(:,:,:)     => null()
   real(SP),   pointer :: f(:,:,:)         => null()
   real(SP),   pointer :: fo(:,:,:)        => null()
   real(SP),   pointer :: W(:,:,:)         => null()
   complex(SP),pointer :: Z(:,:,:)         => null()
   !
   ! Green Functions
   !
   logical             :: GreenF_is_causal
   integer             :: GreenF_n_E_steps
   complex(SP),pointer :: GreenF_W(:,:,:,:)   => null()
   complex(SP),pointer :: GreenF(:,:,:,:)     => null()
   ! 
   ! Fine energy grid
   !   
   real(SP),   pointer :: E_RIM(:,:,:)        => null()
   real(SP),   pointer :: W_RIM(:,:,:)        => null()
   real(SP),   pointer :: f_RIM(:,:,:)        => null()
   !
 end type levels
 ! 
 ! Fine energy grid Tables
 !   
 integer  :: BZ_RIM_max_filling = 1
 integer  :: BZ_RIM_tot_nkpts
 integer  :: BZ_ASSOCIATED_nkpts
 integer  :: BZ_RIM_nbands
 logical  :: RIM_is_symmetry_expanded=.FALSE.
 integer ,allocatable :: BZ_RIM_nkpt(:)
 integer ,allocatable :: BZ_RIM_table(:,:)
 real(SP),allocatable :: BZ_RIM_ipol_weight(:)
 !
 contains
   !
   subroutine E_duplicate(Ei,Eo)
     use memory_m,  ONLY:mem_est
     type(levels):: Ei,Eo
     !
     integer :: BOUNDS(2)
     logical :: naive
     !
     naive=Eo%nk==0
     Eo%nb=Ei%nb
     if (naive) then
       Eo%nk=Ei%nk
       Eo%nbf=Ei%nbf
       Eo%nbm=Ei%nbm
       Eo%bf=Ei%bf
       Eo%kf=Ei%kf
       Eo%sf=Ei%sf
       Eo%E_Fermi=Ei%E_Fermi
       Eo%E_VBM=Ei%E_VBM
       Eo%E_CBm=Ei%E_CBm
       Eo%E_dir_gap=Ei%E_dir_gap
     endif
     allocate(Eo%E(Eo%nb,Eo%nk,n_sp_pol),Eo%f(Eo%nb,Eo%nk,n_sp_pol))
     call mem_est("E-E E-f",(/size(Eo%E),size(Eo%f)/),(/SP,SP/))
     Eo%E=0.
     Eo%f=0.
     !
     ! This line is beacause in _RT calculations the actual first size of %Eo may differ
     ! from %nb as it is limited by SC_bands. To be checked after implementatio of E_dip
     !
     BOUNDS(1)=size(Ei%Eo,1)
     BOUNDS(2)=BOUNDS(1)
     if (associated(Ei%E_dip)) BOUNDS(2)=size(Ei%E_dip,1)
     !
     if (naive) then
       Eo%f(:,:,:)=Ei%f(:,:,:)
       Eo%E(:,:,:)=Ei%E(:,:,:)
       if (associated(Ei%Eo)) then
         allocate(Eo%Eo(BOUNDS(1),Eo%nk,n_sp_pol))
         call mem_est("E-Eo",(/size(Eo%Eo)/),(/SP/))
         Eo%Eo(:BOUNDS(1),:,:)=Ei%Eo(:BOUNDS(1),:,:)
       endif
       if (associated(Ei%E_dip)) then
         allocate(Eo%E_dip(BOUNDS(2),Eo%nk,n_sp_pol))
         call mem_est("E-E_dip",(/size(Eo%E_dip)/),(/SP/))
         Eo%E_dip(:BOUNDS(2),:,:)=Ei%E_dip(:BOUNDS(2),:,:)
       endif
       if (associated(Ei%fo)) then
         allocate(Eo%fo(BOUNDS(2),Eo%nk,n_sp_pol))
         call mem_est("E-fo",(/size(Eo%fo)/),(/SP/))
         Eo%fo(:BOUNDS(2),:,:)=Ei%fo(:BOUNDS(2),:,:)
       endif
       if (associated(Ei%W)) then
         allocate(Eo%W(BOUNDS(2),Eo%nk,n_sp_pol))
         call mem_est("E-W",(/size(Eo%W)/),(/SP/))
         Eo%W(:BOUNDS(2),:,:)=Ei%W(:BOUNDS(2),:,:)
       endif
       if (associated(Ei%Z)) then
         allocate(Eo%Z(BOUNDS(2),Eo%nk,n_sp_pol))
         call mem_est("E-Z",(/size(Eo%Z)/))
         Eo%Z(:BOUNDS(2),:,:)=Ei%Z(:BOUNDS(2),:,:)
       endif
       if (associated(Ei%QP_corrected)) then
         BOUNDS(1)=size(Ei%QP_corrected,1)
         BOUNDS(2)=size(Ei%QP_corrected,2)
         allocate(Eo%QP_corrected(BOUNDS(1),BOUNDS(2),n_sp_pol))
         call mem_est("E-QP_corrected",(/size(Eo%QP_corrected)/),(/IP/))
         Eo%QP_corrected=Ei%QP_corrected
       endif
     else
       Eo%E(:,:,:)=Ei%E(:,:Eo%nk,:)
     endif
     !
     ! E RIM
     !
     if (BZ_RIM_nbands/=0.and.Ei%nk==Eo%nk) then
       allocate(Eo%E_RIM(BZ_RIM_nbands,BZ_RIM_tot_nkpts,n_sp_pol))
       allocate(Eo%W_RIM(BZ_RIM_nbands,BZ_RIM_tot_nkpts,n_sp_pol))
       allocate(Eo%f_RIM(BZ_RIM_nbands,BZ_RIM_tot_nkpts,n_sp_pol))
       call mem_est("E_RIM W_RIM f_RIM",(/size(Eo%E_RIM),size(Eo%W_RIM),size(Eo%f_RIM)/),(/SP,SP,SP/))
       Eo%E_RIM=Ei%E_RIM
       Eo%W_RIM=Ei%W_RIM
       Eo%f_RIM=Ei%f_RIM
     endif
     !
   end subroutine
   !
   subroutine E_reset(E)
     use memory_m, ONLY:mem_est
     type(levels)::E
     E%E_dir_gap=0.
     E%E_Fermi=0.
     E%E_VBM=0.
     E%E_CBm=0.
     E%nb=0
     E%nk=0
     E%nbf=0 
     E%nbm=0
     E%bf=0
     E%kf=0 
     E%GreenF_n_E_steps=0 
     E%GreenF_is_causal=.FALSE. 
     if(associated(E%E))              deallocate(E%E)
     if(associated(E%Eo))             deallocate(E%Eo)
     if(associated(E%E_dip))          deallocate(E%E_dip)
     if(associated(E%Z))              deallocate(E%Z)
     if(associated(E%W))              deallocate(E%W)
     if(associated(E%f))              deallocate(E%f)
     if(associated(E%fo))             deallocate(E%fo)
     if(associated(E%QP_corrected))   deallocate(E%QP_corrected)
     if(associated(E%GreenF))         deallocate(E%GreenF)
     if(associated(E%GreenF_W))       deallocate(E%GreenF_W)
     nullify(E%E,E%Eo,E%E_dip,E%Z,E%W,E%f,E%fo,E%QP_corrected,E%GreenF,E%GreenF_W)
     call mem_est("E-E E-Eo E-E_dip E-f E-W E-Z E-QP_corrected E-GreenF E-GreenF_W")
     !
     call E_RIM_components_reset(E,.FALSE.)
     !
   end subroutine
   !
   subroutine E_RIM_components_reset(E,FULL_RESET)
     !
     use memory_m, ONLY:mem_est
     !
     type(levels)::E
     logical     ::FULL_RESET
     !
     if(associated(E%E_RIM))      deallocate(E%E_RIM)
     if(associated(E%W_RIM))      deallocate(E%W_RIM)
     if(associated(E%f_RIM))      deallocate(E%f_RIM)
     !
     nullify(E%E_RIM,E%W_RIM,E%f_RIM)
     call mem_est("E_RIM W_RIM f_RIM")
     !
     if (.not.FULL_RESET) return
     !
     BZ_RIM_tot_nkpts=0
     BZ_ASSOCIATED_nkpts=0
     BZ_RIM_nbands=0
     !
     if(allocated(BZ_RIM_nkpt))        deallocate(BZ_RIM_nkpt)
     if(allocated(BZ_RIM_table))       deallocate(BZ_RIM_table)
     if(allocated(BZ_RIM_ipol_weight)) deallocate(BZ_RIM_ipol_weight)
     !
   end subroutine
   !
   integer function spin(v)
     integer :: v(:)
     !
     spin=1
     if (n_sp_pol==2) spin=v(4)
     !
   end function
   !
end module electrons
