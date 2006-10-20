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
 module electrons
!================
   use pars
   real(SP) :: nel,default_nel,spin_occ,filled_tresh
   integer  :: n_bands,n_met_bands,n_full_bands
!
! Spin support
!
   integer  :: n_spin,n_sp_pol,n_spinor
!
   type levels
     integer         :: nb
     integer         :: nk
     integer         :: nbf
     integer         :: nbm
     integer         :: bf
     integer         :: kf
     integer         :: sf
     integer,pointer :: qp_done(:,:)
     real(SP)            :: Efermi(3)
     real(SP),   pointer :: E(:,:,:)
     real(SP),   pointer :: Eo(:,:,:)
     real(SP),   pointer :: f(:,:,:)
     real(SP),   pointer :: W(:,:)
     complex(SP),pointer :: Z(:,:)
! 
! Fine energy grid
!  
     real(SP),   pointer :: E_RIM(:,:,:)
     real(SP),   pointer :: f_RIM(:,:,:)
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
!
 contains
!
 subroutine E_duplicate(Ei,Eo)
 use memory_m,  ONLY:mem_est
 implicit none
 type(levels):: Ei,Eo
 logical :: naive
 naive=Eo%nk==0
 Eo%nb=Ei%nb
 if (naive) then
   Eo%nk=Ei%nk
   Eo%nbf=Ei%nbf
   Eo%nbm=Ei%nbm
   Eo%bf=Ei%bf
   Eo%kf=Ei%kf
   Eo%Efermi=Ei%Efermi
 endif
 allocate(Eo%E(Eo%nb,Eo%nk,n_spin),Eo%f(Eo%nb,Eo%nk,n_spin),&
&         Eo%W(Eo%nb,Eo%nk),Eo%Z(Eo%nb,Eo%nk))
 call mem_est("E-E E-f E-W E-Z",&
&             (/size(Eo%E),size(Eo%f),size(Eo%W),size(Eo%Z)/),&
&             (/SP,        SP,        SP,        2*SP/) )
 Eo%E=0.
 Eo%W=0.
 Eo%f=0.
 Eo%Z=(0.,0.)
 if (naive) then
   Eo%f(:,:,:)=Ei%f(:,:,:)
   Eo%E(:,:,:)=Ei%E(:,:,:)
   if (associated(Ei%Eo)) Eo%E(:,:,:)=Ei%Eo(:,:,:)
 else
   Eo%E(:,:,:)=Ei%E(:,:Eo%nk,:)
 endif
!
!E RIM
!
 if (BZ_RIM_nbands/=0.and.Ei%nk==Eo%nk) then
   allocate(Eo%E_RIM(BZ_RIM_nbands,BZ_RIM_tot_nkpts,n_spin))
   allocate(Eo%f_RIM(BZ_RIM_nbands,BZ_RIM_tot_nkpts,n_spin))
   call mem_est("E_RIM f_RIM",(/size(Eo%E_RIM),size(Eo%f_RIM)/),(/SP,SP/))
   Eo%E_RIM=Ei%E_RIM
   Eo%f_RIM=Ei%f_RIM
 endif
 end subroutine

 subroutine E_reset(E)
 use memory_m, ONLY:mem_est
 implicit none
 type(levels)::E
 E%Efermi=0.
 E%nb=0
 E%nk=0
 E%nbf=0 
 E%nbm=0
 E%bf=0
 E%kf=0 
 nullify(E%E,E%Eo,E%Z,E%W,E%f,E%qp_done,E%E_RIM,E%f_RIM)
 call mem_est("E-E E-Eo E-f E-W E-Z E-qp_done E_RIM f_RIM")
 end subroutine


 end module electrons
