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
 !     n_spin n_sp_pol n_spinor  
 !       1        1        1              
 !       2        1        2    non collinear
 !       2        2        1        collinear
 !
 ! Note that in the non collinear case n_spin is an index
 ! for the spinor components. It is not a Physical quantity.
 !
 integer  :: n_spin
 integer  :: n_sp_pol
 integer  :: n_spinor
 logical  :: spin_orbit
 !
 type levels
   integer         :: nb
   integer         :: nk
   integer         :: nbf
   integer         :: nbm
   integer         :: bf
   integer         :: kf
   integer         :: sf
   integer,pointer :: QP_corrected(:,:,:)
   real(SP)            :: Efermi(3)
   real(SP),   pointer :: E(:,:,:)
   real(SP),   pointer :: Eo(:,:,:)
   real(SP),   pointer :: f(:,:,:)
   real(SP),   pointer :: W(:,:,:)
   complex(SP),pointer :: Z(:,:,:)
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
#if defined _DIPOLE_RIM
 logical  :: l_dipole_RIM
#endif
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
     nullify(E%E,E%Eo,E%Z,E%W,E%f,E%QP_corrected)
     call mem_est("E-E E-Eo E-f E-W E-Z E-QP_corrected")
     !
     nullify(E%E_RIM,E%f_RIM)
     call mem_est("E_RIM f_RIM")
     !
   end subroutine
   !
   integer function spin(v)
     integer :: v(:)
     !
     spin=1
     !
     ! GPL_EXCLUDE_START SPIN_IGNORE
     !
     if (n_sp_pol==2) spin=v(4)
     !
     ! GPL_EXCLUDE_END SPIN_IGNORE
     !
   end function
   !
   subroutine el_density(en,Xk,rho,force_si,G_lesser)
     !
     !Electronic density.
     !
     !Note that here the identity
     !
     ! \sum_{S_k} F(S_k r) = 1/R_k \sum_{S} F(S r) (1)
     ! 
     !where S_k is a symm. op. in the star of k, S is a generic symm. op.
     !and R_k is the rank of the small group at k.
     !
     !Using (1) the density is calculated in two steps
     !
     ! rho(r) = 1/nkibz \sum_{ n k S_k } f_{n k} |wf_{n k}(S_k^-1 r)|^2=
     !        = 1/nkibz \sum_{S} \sum_{n k} f_{n k}/R_k |wf_{n k}(S^-1 r)|^2 =
     !        = 1/nsym \sum_{S} ( \sum_{n k} f_{n k} w_k |wf_{n k}(S^-1 r)|^2 )
     !
     !where we have used the relation
     !
     ! w_k = nsym/(nkibz * rank(k))
     !
     !
     use pars,          ONLY:SP
     use com,           ONLY:warning
     use R_lattice,     ONLY:bz_samp
     use D_lattice,     ONLY:nsym,i_time_rev
     use FFT_m,         ONLY:fft_size,fft_rot_r
     use wave_func,     ONLY:wf_state,wf
     !
     implicit none
     type(levels) ::en       
     type(bz_samp)::Xk
     real(SP)     ::rho(fft_size)
     logical,     intent(in), optional :: force_si
     complex(SP), intent(in), optional :: G_lesser(en%nbm,en%nbm,Xk%nibz)
     !
     ! Work Space 
     !
     integer :: i1,i2,ik,i_spin,ifft,rho_syms,ifft2
     real(SP):: rho_no_sym(fft_size),f_occ
     logical :: warn_done
     !
     rho=0.
     rho_no_sym=0.
     warn_done=.false.
     !
     do i1=1,en%nbm
       do i2=1,Xk%nibz
         !
         i_spin=1
         !
         ! GPL_EXCLUDE_START SPIN_IGNORE
         !
         do i_spin=1,n_spin
           !
           if (size(wf_state,3)<i_spin) cycle
           !
           ! GPL_EXCLUDE_END SPIN_IGNORE
           !
           if (size(wf_state,1)<i1) cycle
           if (size(wf_state,2)<i2) cycle
           !
           ifft=wf_state(i1,i2,i_spin)
           !
           if (ifft==0) then
             if (.not.warn_done) call warning('Not enough states to calculate rho')
             warn_done=.true.
             cycle
           endif
           !
           f_occ=en%f(i1,i2,1)
           !
           ! GPL_EXCLUDE_START SPIN_IGNORE
           !
           if (n_sp_pol==2) f_occ=en%f(i1,i2,i_spin)
           !
           ! GPL_EXCLUDE_END SPIN_IGNORE
           !
           rho_no_sym(:)=rho_no_sym(:)+f_occ*Xk%weights(i2)*abs(wf(:,ifft))**2.
           !
           ! GPL_EXCLUDE_START SPIN_IGNORE
           !
         enddo
         !
         ! GPL_EXCLUDE_END SPIN_IGNORE
         !
       enddo
     enddo
     !
#if defined _SC
     !
     ! If present G_lesser add the Off-diagonal contributions
     !
     if(present(G_lesser)) then
       !      
       do i1=1,en%nbm
         do i2=i1+1,en%nbm
           !      
           do ik=1,Xk%nibz
             !
             ifft=wf_state(i1,ik,1)
             ifft2=wf_state(i2,ik,1)
             !
             if(ifft==0.or.ifft2==0) cycle
             !
             rho_no_sym(:)=rho_no_sym(:)+Xk%weights(ik)*2._SP*real(wf(:,ifft) &
&                      *conjg(wf(:,ifft2))*(0._SP,-1._SP)*G_lesser(i1,i2,ik))
             !
           enddo
           !
         enddo
       enddo
       !
     endif
     !
#endif
     !
     ! Simmetrization
     !
     rho_syms=nsym/(i_time_rev+1)
     if (present(force_si)) rho_syms=nsym
     do i1=1,rho_syms
       rho(:)=rho(:)+real(rho_no_sym(fft_rot_r(i1,:)))/real(nsym)
     enddo
     !
     if (.not.present(force_si)) rho=rho*(1+i_time_rev)
     !
   end subroutine
   !
end module electrons
