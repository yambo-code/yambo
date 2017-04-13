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
   real(SP),   pointer :: f(:,:,:)         => null()
   real(SP),   pointer :: fo(:,:,:)        => null()
   real(SP),   pointer :: df(:,:,:)        => null()
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
   ! Double energy and k-points grid
   !   
   real(SP),   pointer :: E_DbGd(:,:,:)        => null()
   real(SP),   pointer :: W_DbGd(:,:,:)        => null()
   real(SP),   pointer :: f_DbGd(:,:,:)        => null()
   real(SP),   pointer :: df_DbGd(:,:,:)       => null()
   !
   ! Perturbative SOC
   !
   real(SP),   pointer :: E_SOC(:,:)           => null()
   !
 end type levels
 ! 
 ! Fine energy grid Tables
 !   
 integer  :: BZ_DbGd_max_filling = 1
 integer  :: BZ_DbGd_nkpts
 integer  :: BZ_DbGd_tot_nkpts
 integer  :: BZ_ASSOCIATED_nkpts
 integer  :: BZ_DbGd_nbands
 logical  :: DbGd_is_symmetry_expanded=.FALSE.
 integer ,allocatable :: BZ_DbGd_nkpt(:)
 integer ,allocatable :: BZ_DbGd_table(:,:,:)
 real(SP),allocatable :: BZ_DbGd_ipol_weight(:)
 real(SP),allocatable :: BZ_DbGd_kpts(:,:)
 !
 ! Perturbative SOC table
 !
 integer :: E_SOC_nbands
 integer, allocatable :: E_SOC_table(:,:,:)
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
       Eo%E_ind_gap=Ei%E_ind_gap
     endif
     allocate(Eo%E(Eo%nb,Eo%nk,n_sp_pol),Eo%f(Eo%nb,Eo%nk,n_sp_pol))
     call mem_est("E-E E-f",(/size(Eo%E),size(Eo%f)/),(/SP,SP/))
     Eo%E=0._SP
     Eo%f=0._SP
     !
     ! This line is beacause in _RT calculations the actual first size of %Eo may differ
     ! from %nb as it is limited by SC_bands.
     !
     BOUNDS(1)=Ei%nb
     if (associated(Ei%Eo)) then
       BOUNDS(1)=size(Ei%Eo,1)
     endif
     BOUNDS(2)=BOUNDS(1)
     !
     if (naive) then
       ! %E
       Eo%E(:,:,:)=Ei%E(:,:,:)
       if (associated(Ei%Eo)) then
         allocate(Eo%Eo(BOUNDS(1),Eo%nk,n_sp_pol))
         call mem_est("E-Eo",(/size(Eo%Eo)/),(/SP/))
         Eo%Eo(:BOUNDS(1),:,:)=Ei%Eo(:BOUNDS(1),:,:)
       endif
       ! %f
       Eo%f(:,:,:)=Ei%f(:,:,:) 
       if (associated(Ei%fo)) then
         allocate(Eo%fo(BOUNDS(1),Eo%nk,n_sp_pol))
         call mem_est("E-fo",(/size(Eo%fo)/),(/SP/))
         Eo%fo(:BOUNDS(1),:,:)=Ei%fo(:BOUNDS(1),:,:)
       endif
       if (associated(Ei%df)) then
         allocate(Eo%df(BOUNDS(1),Eo%nk,n_sp_pol))
         call mem_est("E-df",(/size(Eo%df)/),(/SP/))
         Eo%df(:BOUNDS(1),:,:)=Ei%df(:BOUNDS(1),:,:)
       endif
       ! %W
       if (associated(Ei%W)) then
         allocate(Eo%W(BOUNDS(2),Eo%nk,n_sp_pol))
         call mem_est("E-W",(/size(Eo%W)/),(/SP/))
         Eo%W(:BOUNDS(2),:,:)=Ei%W(:BOUNDS(2),:,:)
       endif
       ! %Z
       if (associated(Ei%Z)) then
         allocate(Eo%Z(BOUNDS(2),Eo%nk,n_sp_pol))
         call mem_est("E-Z",(/size(Eo%Z)/))
         Eo%Z(:BOUNDS(2),:,:)=Ei%Z(:BOUNDS(2),:,:)
       endif
       ! %QP_corrected
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
     ! E Double Grid
     !
     if (BZ_DbGd_nbands/=0.and.Ei%nk==Eo%nk) then
       allocate(Eo%E_DbGd(BZ_DbGd_nbands,BZ_DbGd_tot_nkpts,n_sp_pol))
       allocate(Eo%W_DbGd(BZ_DbGd_nbands,BZ_DbGd_tot_nkpts,n_sp_pol))
       allocate(Eo%f_DbGd(BZ_DbGd_nbands,BZ_DbGd_tot_nkpts,n_sp_pol))
       call mem_est("E_DbGd W_DbGd f_DbGd",(/size(Eo%E_DbGd),size(Eo%W_DbGd),size(Eo%f_DbGd)/),(/SP,SP,SP/))
       Eo%E_DbGd=Ei%E_DbGd
       Eo%W_DbGd=Ei%W_DbGd
       Eo%f_DbGd=Ei%f_DbGd
       if(associated(Ei%df_DbGd)) then
         allocate(Eo%df_DbGd(BZ_DbGd_nbands,BZ_DbGd_tot_nkpts,n_sp_pol))
         call mem_est("df_DbGd",(/size(Eo%df_DbGd)/),(/SP/))
         Eo%df_DbGd=Ei%df_DbGd
       endif
     endif
     !
     ! E_SOC
     !
     if (E_SOC_nbands/=0.and.Ei%nk==Eo%nk) then
       allocate(Eo%E_SOC(E_SOC_nbands,Eo%nk))
       call mem_est("E_SOC",(/size(Eo%E_SOC)/),(/SP/))
       Eo%E_SOC=Ei%E_SOC
     endif
     !
   end subroutine
   !
   subroutine E_reset(E)
     use memory_m, ONLY:mem_est
     type(levels)::E
     E%E_dir_gap=0._SP
     E%E_ind_gap=0._SP
     E%E_Fermi=0._SP
     E%E_VBM=0._SP
     E%E_CBm=0._SP
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
     if(associated(E%fo))             deallocate(E%fo)
     if(associated(E%df))             deallocate(E%df)
     if(associated(E%QP_corrected))   deallocate(E%QP_corrected)
     if(associated(E%GreenF))         deallocate(E%GreenF)
     if(associated(E%GreenF_W))       deallocate(E%GreenF_W)
     nullify(E%E,E%Eo,E%Z,E%W,E%f,E%QP_corrected,E%GreenF,E%GreenF_W)
     call mem_est("E-E E-Eo E-f E-fo E-df E-W E-Z E-QP_corrected E-GreenF E-GreenF_W")
     !
     call E_DbGd_components_reset(E,.FALSE.)
     !
     if(associated(E%E_SOC))          deallocate(E%E_SOC)
     !
   end subroutine
   !
   subroutine E_DbGd_components_reset(E,FULL_RESET)
     !
     use memory_m, ONLY:mem_est
     !
     type(levels)::E
     logical     ::FULL_RESET
     !
     if(associated(E%E_DbGd) )      deallocate(E%E_DbGd )
     if(associated(E%W_DbGd) )      deallocate(E%W_DbGd )
     if(associated(E%f_DbGd) )      deallocate(E%f_DbGd )
     if(associated(E%df_DbGd))      deallocate(E%df_DbGd)
     !
     nullify(E%E_DbGd,E%W_DbGd,E%f_DbGd,E%df_DbGd)
     call mem_est("E_DbGd W_DbGd f_DbGd df_DbGd")
     !
     if (.not.FULL_RESET) return
     !
     BZ_DbGd_nkpts=0
     BZ_DbGd_tot_nkpts=0
     BZ_ASSOCIATED_nkpts=0
     BZ_DbGd_nbands=0
     !
     if(allocated(BZ_DbGd_nkpt))        deallocate(BZ_DbGd_nkpt)
     if(allocated(BZ_DbGd_table))       deallocate(BZ_DbGd_table)
     if(allocated(BZ_DbGd_ipol_weight)) deallocate(BZ_DbGd_ipol_weight)
     if(allocated(BZ_DbGd_kpts))        deallocate(BZ_DbGd_kpts)
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
