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
#include<memory.h>
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
 logical  :: eval_magn
 real(SP) :: Total_magn(3)
 !
 ! Minimum of the electronic density
 !
 logical :: eval_rho_max
 logical :: rho_max_is_def
 integer :: ir_rho_max
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
   real(SP),   allocatable :: E(:,:,:)         
   real(SP),   allocatable :: Eo(:,:,:)        
   real(SP),   allocatable :: f(:,:,:)         
   real(SP),   allocatable :: fo(:,:,:)        
   real(SP),   allocatable :: df(:,:,:)        
   real(SP),   allocatable :: W(:,:,:)         
   complex(SP),allocatable :: Z(:,:,:)         
   !
   ! Green Functions
   !
   logical                 :: GreenF_is_causal
   integer                 :: GreenF_n_E_steps
   complex(SP),allocatable :: GreenF_W(:,:,:,:)   
   complex(SP),allocatable :: GreenF(:,:,:,:)     
   ! 
   ! Double energy and k-points grid
   !   
   real(SP),   allocatable :: E_DbGd(:,:,:)        
   real(SP),   allocatable :: W_DbGd(:,:,:)        
   real(SP),   allocatable :: f_DbGd(:,:,:)        
   real(SP),   allocatable :: df_DbGd(:,:,:)       
   !
   ! Perturbative SOC
   !
   real(SP),   allocatable :: E_SOC(:,:)           
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
 real(SP),allocatable :: BZ_DbGd_weight(:)
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
     YAMBO_ALLOC(Eo%E,(Eo%nb,Eo%nk,n_sp_pol))
     YAMBO_ALLOC(Eo%f,(Eo%nb,Eo%nk,n_sp_pol))
     Eo%E=0._SP
     Eo%f=0._SP
     !
     ! This line is beacause in _RT calculations the actual first size of %Eo may differ
     ! from %nb as it is limited by SC_bands.
     !
     BOUNDS(1)=Ei%nb
     if (allocated(Ei%Eo)) then
       BOUNDS(1)=size(Ei%Eo,1)
     endif
     BOUNDS(2)=BOUNDS(1)
     !
     if (naive) then
       ! %E
       Eo%E(:,:,:)=Ei%E(:,:,:)
       if (allocated(Ei%Eo)) then
         YAMBO_ALLOC(Eo%Eo,(BOUNDS(1),Eo%nk,n_sp_pol))
         Eo%Eo(:BOUNDS(1),:,:)=Ei%Eo(:BOUNDS(1),:,:)
       endif
       ! %f
       Eo%f(:,:,:)=Ei%f(:,:,:) 
       if (allocated(Ei%fo)) then
         YAMBO_ALLOC(Eo%fo,(BOUNDS(1),Eo%nk,n_sp_pol))
         Eo%fo(:BOUNDS(1),:,:)=Ei%fo(:BOUNDS(1),:,:)
       endif
       if (allocated(Ei%df)) then
         YAMBO_ALLOC(Eo%df,(BOUNDS(1),Eo%nk,n_sp_pol))
         Eo%df(:BOUNDS(1),:,:)=Ei%df(:BOUNDS(1),:,:)
       endif
       ! %W
       if (allocated(Ei%W)) then
         YAMBO_ALLOC(Eo%W,(BOUNDS(2),Eo%nk,n_sp_pol))
         Eo%W(:BOUNDS(2),:,:)=Ei%W(:BOUNDS(2),:,:)
       endif
       ! %Z
       if (allocated(Ei%Z)) then
         YAMBO_ALLOC(Eo%Z,(BOUNDS(2),Eo%nk,n_sp_pol))
         Eo%Z(:BOUNDS(2),:,:)=Ei%Z(:BOUNDS(2),:,:)
       endif
     else
       Eo%E(:,:,:)=Ei%E(:,:Eo%nk,:)
     endif
     !
     ! E Double Grid
     !
     if (BZ_DbGd_nbands/=0.and.Ei%nk==Eo%nk) then
       YAMBO_ALLOC(Eo%E_DbGd,(BZ_DbGd_nbands,BZ_DbGd_tot_nkpts,n_sp_pol))
       YAMBO_ALLOC(Eo%W_DbGd,(BZ_DbGd_nbands,BZ_DbGd_tot_nkpts,n_sp_pol))
       YAMBO_ALLOC(Eo%f_DbGd,(BZ_DbGd_nbands,BZ_DbGd_tot_nkpts,n_sp_pol))
       Eo%E_DbGd=Ei%E_DbGd
       Eo%W_DbGd=Ei%W_DbGd
       Eo%f_DbGd=Ei%f_DbGd
       if(allocated(Ei%df_DbGd)) then
         YAMBO_ALLOC(Eo%df_DbGd,(BZ_DbGd_nbands,BZ_DbGd_tot_nkpts,n_sp_pol))
         Eo%df_DbGd=Ei%df_DbGd
       endif
     endif
     !
     ! E_SOC
     !
     if (E_SOC_nbands/=0.and.Ei%nk==Eo%nk) then
       YAMBO_ALLOC(Eo%E_SOC,(E_SOC_nbands,Eo%nk))
       Eo%E_SOC=Ei%E_SOC
     endif
     !
   end subroutine
   !
   subroutine E_reset(E)
     type(levels)::E
     !
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
     YAMBO_FREE(E%E)
     YAMBO_FREE(E%Eo)
     YAMBO_FREE(E%Z)
     YAMBO_FREE(E%W)
     YAMBO_FREE(E%f)
     YAMBO_FREE(E%fo)
     YAMBO_FREE(E%df)
     YAMBO_FREE(E%GreenF)
     YAMBO_FREE(E%GreenF_W)
     !
     call E_DbGd_components_reset(E,.FALSE.)
     !
     YAMBO_FREE(E%E_SOC)
     !
   end subroutine
   !
   subroutine E_DbGd_components_reset(E,FULL_RESET)
     !
     type(levels)::E
     logical     ::FULL_RESET
     !
     YAMBO_FREE(E%E_DbGd)
     YAMBO_FREE(E%W_DbGd)
     YAMBO_FREE(E%f_DbGd)
     YAMBO_FREE(E%df_DbGd)
     !
     !
     if (.not.FULL_RESET) return
     !
     BZ_DbGd_nkpts=0
     BZ_DbGd_tot_nkpts=0
     BZ_ASSOCIATED_nkpts=0
     BZ_DbGd_nbands=0
     !
     YAMBO_FREE(BZ_DbGd_nkpt)
     YAMBO_FREE(BZ_DbGd_table)
     YAMBO_FREE(BZ_DbGd_weight)
     YAMBO_FREE(BZ_DbGd_kpts)
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
