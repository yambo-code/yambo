!
! Copyright (C) 2000-2013 A. Marini and the YAMBO team 
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
module electrons
 !
 use pars
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
 ! Boltzmann occupations
 !
 real(SP)          :: Boltz_Efield(3)
 real(SP)          :: Boltz_tau
 real(SP), pointer :: Boltz_f(:,:,:,:)
 real(SP), pointer :: Boltz_vel(:,:,:,:)
 !
 type levels
   integer         :: nb
   integer         :: nk
   integer         :: nbf
   integer         :: nbm
   integer         :: bf
   integer         :: kf
   integer         :: sf
   real(SP)        :: Efermi(3)
   !
   ! QP corrections
   !
   integer,pointer :: QP_corrected(:,:,:)  => null()
   real(SP),   pointer :: E(:,:,:)         => null()
   real(SP),   pointer :: Eo(:,:,:)        => null()
   real(SP),   pointer :: f(:,:,:)         => null()
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
   real(SP),   pointer :: f_RIM(:,:,:)        => null()
   !
 end type levels
 ! 
 ! Fine energy grid Tables
 !   
 integer  :: BZ_RIM_max_filling
 integer  :: BZ_RIM_tot_nkpts
 integer  :: BZ_RIM_nbands
 integer ,allocatable :: BZ_RIM_nkpt(:)
 integer ,allocatable :: BZ_RIM_table(:,:)
 real(SP), allocatable :: BZ_RIM_ipol_weight(:)
 !
 contains
   !
   subroutine E_duplicate(Ei,Eo)
     use com,       ONLY:warning
     use memory_m,  ONLY:mem_est
     type(levels):: Ei,Eo
     !
     integer :: QP_corrected_bounds(2)
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
       Eo%Efermi=Ei%Efermi
     endif
     allocate(Eo%E(Eo%nb,Eo%nk,n_sp_pol),Eo%f(Eo%nb,Eo%nk,n_sp_pol))
     call mem_est("E-E E-f",(/size(Eo%E),size(Eo%f)/),(/SP,SP/))
     Eo%E=0.
     Eo%f=0.
     if (naive) then
       Eo%f(:,:,:)=Ei%f(:,:,:)
       Eo%E(:,:,:)=Ei%E(:,:,:)
       if (associated(Ei%Eo)) then
         allocate(Eo%Eo(Eo%nb,Eo%nk,n_sp_pol))
         call mem_est("E-Eo",(/size(Eo%E)/),(/SP/))
         Eo%Eo=Ei%Eo
       endif
       if (associated(Ei%W)) then
         allocate(Eo%W(Eo%nb,Eo%nk,n_sp_pol))
         call mem_est("E-W",(/size(Eo%W)/),(/SP/))
         Eo%W=Ei%W
       endif
       if (associated(Ei%Z)) then
         allocate(Eo%Z(Eo%nb,Eo%nk,n_sp_pol))
         call mem_est("E-Z",(/size(Eo%Z)/))
         Eo%Z=Ei%Z
       endif
       if (associated(Ei%QP_corrected)) then
         QP_corrected_bounds(1)=size(Ei%QP_corrected,1)
         QP_corrected_bounds(2)=size(Ei%QP_corrected,2)
         allocate(Eo%QP_corrected(QP_corrected_bounds(1),QP_corrected_bounds(2),n_sp_pol))
         call mem_est("E-QP_corrected",(/size(Eo%QP_corrected)/),(/IP/))
         Eo%QP_corrected=Ei%QP_corrected
         call warning(' QP corrections transferred in energy levels duplication')
       endif
     else
       Eo%E(:,:,:)=Ei%E(:,:Eo%nk,:)
     endif
     !
     ! E RIM
     !
     if (BZ_RIM_nbands/=0.and.Ei%nk==Eo%nk) then
       allocate(Eo%E_RIM(BZ_RIM_nbands,BZ_RIM_tot_nkpts,n_sp_pol))
       allocate(Eo%f_RIM(BZ_RIM_nbands,BZ_RIM_tot_nkpts,n_sp_pol))
       call mem_est("E_RIM f_RIM",(/size(Eo%E_RIM),size(Eo%f_RIM)/),(/SP,SP/))
       Eo%E_RIM=Ei%E_RIM
       Eo%f_RIM=Ei%f_RIM
     endif
     !
   end subroutine
   !
   subroutine E_reset(E)
     use memory_m, ONLY:mem_est
     type(levels)::E
     E%Efermi=0.
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
     if(associated(E%Z))              deallocate(E%Z)
     if(associated(E%W))              deallocate(E%W)
     if(associated(E%f))              deallocate(E%f)
     if(associated(E%QP_corrected))   deallocate(E%QP_corrected)
     if(associated(E%GreenF))         deallocate(E%GreenF)
     if(associated(E%GreenF_W))       deallocate(E%GreenF_W)
     nullify(E%E,E%Eo,E%Z,E%W,E%f,E%QP_corrected,E%GreenF,E%GreenF_W)
     call mem_est("E-E E-Eo E-f E-W E-Z E-QP_corrected E-GreenF E-GreenF_W")
     !
     if(associated(E%E_RIM))      deallocate(E%E_RIM)
     if(associated(E%f_RIM))      deallocate(E%f_RIM)
     nullify(E%E_RIM,E%f_RIM)
     call mem_est("E_RIM f_RIM")
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
