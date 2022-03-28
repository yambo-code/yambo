!
!        Copyright (C) 2000-2021 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): MG IA
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
subroutine FL_diagonalization(E,k,FL_V_bands)
 !
 use pars,           ONLY:SP,cONE,cZERO
 use electrons,      ONLY:levels,n_sp_pol
 use R_lattice,      ONLY:bz_samp
 use QP_m,           ONLY:QP_nk
 use fields,         ONLY:Efield
 use linear_algebra, ONLY:DIAGO
 use interfaces,     ONLY:LINEAR_ALGEBRA_driver
 use wrapper,        ONLY:M_by_V,V_plus_alpha_V,Vstar_dot_V
 use nl_optics,      ONLY:NL_bands,E_full
 use fl_optics,      ONLY:max_fl_mode,n_tot_fl_modes,FL_space_dim,FL_S_plus,FL_S_minus,&
&                         FL_states,FL_Ho_plus_Sigma,FL_QE
 !
 implicit none
 !
 type(levels),  intent(in)     :: E
 type(bz_samp), intent(in)     :: k
 complex(SP),   intent(inout)  :: FL_V_bands(NL_bands(2),n_tot_fl_modes,E%nbf,QP_nk,n_sp_pol)
 !
 ! Work Space
 !
 real(SP)    :: FL_eigenvalues(FL_states(1):FL_states(2),QP_nk,n_sp_pol)
!real(SP)    :: FL_QE(NL_bands(1):NL_bands(2),QP_nk,n_sp_pol)
 real(SP)    :: min_projection,trial_projection,trial_val,trial_proj
 real(SP)    :: energy_difference(FL_states(1):FL_states(2))
 complex(SP) :: FL_eigenvectors(FL_states(1):FL_states(2),FL_states(1):FL_states(2))
 integer     :: ik_bz,i_sp_pol,is,size_Hamiltonian,i_m
 integer     :: i1,i2,i1_fl,i2_fl,i1_fks,i2_fks,i3_fks
 integer     :: FL_chosen_bands(NL_bands(1):E%nbf)
 character(4) :: filename
 integer     :: line_counter
 complex(SP) :: Vtmp(FL_states(1):FL_states(2)),Vzero(FL_states(1):FL_states(2))
 complex(SP) :: ref_vec(FL_states(1):FL_states(2)),trial_vec(FL_states(1):FL_states(2))
 complex(SP) :: Mtmp(FL_states(1):FL_states(2),FL_states(1):FL_states(2))
 !
 FL_eigenvalues = 0._SP
 !FL_QE = 0._SP !aim137 this was wrong, they are initialised as E_full
 !
 do ik_bz=1,QP_nk
   !
   !print*, '------ k points=',ik_bz
   !if(.not.PAR_IND_Xk_bz%element_1D(ik_bz)) cycle
   !
   do is=1,n_sp_pol
     !
     ! Diagonalization
     ! ===============
     !
      FL_eigenvectors(FL_states(1):FL_states(2),FL_states(1):FL_states(2)) = &
&    FL_Ho_plus_Sigma(FL_states(1):FL_states(2),FL_states(1):FL_states(2),ik_bz,is)
     call LINEAR_ALGEBRA_driver(DIAGO,&
                              & M=FL_eigenvectors(FL_states(1):FL_states(2),FL_states(1):FL_states(2)),&
                              & E_real=FL_eigenvalues(FL_states(1):FL_states(2),ik_bz,is)) 
     !aim137 overwrites matrix M with eigenvectors as columns
     !
     FL_chosen_bands = 0
     !
     ! Populate FL_V_bands
     ! ===================
     !
     do i_m=NL_bands(1),E%nbf ! band I want to update
       !print*, '--- band =',i_m
       !
       ! ref_vec is FL_V_band from previous iteration
       !
      !print*, ''
      !print*, '------',i_m,ik_bz,is
       !
       !
       ! try with quasi-energy
       do i1_fks=FL_states(1),FL_states(2)
       energy_difference(i1_fks) = &
                    &   abs(FL_eigenvalues(i1_fks,ik_bz,is) &
                    & - E_full(i_m,ik_bz,is))
       enddo
       !
       if (minval(energy_difference) < 1.E-3) then
         !
         FL_chosen_bands(i_m) = minloc(energy_difference(FL_states(1):FL_states(2)),dim=1)
         trial_vec(FL_states(1):FL_states(2)) = &
                      & FL_eigenvectors(FL_states(1):FL_states(2),FL_chosen_bands(i_m))
         trial_val = FL_eigenvalues(FL_chosen_bands(i_m),ik_bz,is)
         !
       else 
         !
         ref_vec = cZERO
         !
         do i1=NL_bands(1),NL_bands(2)
           do i1_fl=1,n_tot_fl_modes
             !
             i1_fks = i1_fl + (i1-1)*n_tot_fl_modes !don't need SP here, it's an integer
             ref_vec(i1_fks) = FL_V_bands(i1,i1_fl,i_m,ik_bz,is)
             !
           enddo
        !  print*, FL_V_bands(i1,:,i_m,ik_bz,is)
         enddo
         !
         ! aim137 - prelim
         ! aim137: podria directamente multiplicar toda la matriz 
         ! aim137  FL_eigenvectors aca por el valor que anula la fase
         ! aim137  y ahi tomar el eigenvector que tenga un 1 en el
         ! aim137  lugar correcto... luego chequeo con un dot prod y
         ! aim137  si no da ~1, ahi entro al loop este...
         ! Find maximum inner product with ref_vec
         !
         min_projection = 1._SP
         trial_vec = cZERO
         trial_val = 0._SP
         do i2_fks=FL_states(1),FL_states(2)
           !
           trial_projection = Vstar_dot_V(FL_states(2)-FL_states(1)+1,&
                           &  ref_vec(FL_states(1):FL_states(2)),&
                           &  FL_eigenvectors(FL_states(1):FL_states(2),i2_fks))
          !trial_projection = dot_product(&
          !            &      conjg(ref_vec(FL_states(1):FL_states(2))),&
          !            &    FL_eigenvectors(FL_states(1):FL_states(2),i2_fks))
           if (real(trial_projection) < 0) then
                   trial_projection = trial_projection*(-1._SP)
               !aim137 check - I don't know whether I need to do this or not
               !aim137 it did not change anything anyway
               !   FL_eigenvectors(FL_states(1):FL_states(2),i2_fks) = &
               ! & FL_eigenvectors(FL_states(1):FL_states(2),i2_fks)*(-1._SP)
           endif
           !
           if ((1-real(trial_projection)) < min_projection) then
             min_projection = 1-real(trial_projection)
             trial_vec(FL_states(1):FL_states(2)) = &
                     & FL_eigenvectors(FL_states(1):FL_states(2),i2_fks)
             trial_val = FL_eigenvalues(i2_fks,ik_bz,is)
             FL_chosen_bands(i_m) = i2_fks
             trial_proj = real(trial_projection)
           endif
           !
         enddo
         !
         !aim137 debug
         !print*, '-------------------'
         !print*, trial_proj
         !print*, ''
         !print*, ref_vec(FL_states(1):FL_states(2))
         !print*, ''
         !print*, trial_vec(FL_states(1):FL_states(2))
         !call error('paro aca')
         !aim137 debug
       endif
       !
       !aim137 check < 
       !aim137 is it correct to do this? why don't I just leave this phase?
       i1_fks = (i_m-1)*n_tot_fl_modes + (max_fl_mode+1)
       trial_vec = trial_vec * conjg(trial_vec(i1_fks))
       !aim137 check >
       !
       !aim137 DEBUG <
      !i1_fks = (i_m-1)*n_tot_fl_modes + (max_fl_mode+1)
      !if (ik_bz == 15 .and. i_m ==4) then
      !  print*, 'chosen band', FL_chosen_bands(i_m), 'central component', i1_fks
      !  print*,  abs(trial_vec(i1_fks)),abs(trial_vec(i1_fks+n_tot_fl_modes+1))
      !  print*,  FL_eigenvalues(i1_fks,ik_bz,is)-E_full(i_m,ik_bz,is), &
      !          &FL_eigenvalues(i1_fks+n_tot_fl_modes+1,ik_bz,is)-E_full(i_m+1,ik_bz,is)
      !endif
       !aim137 DEBUG >
       !
       ! Overwrite FL_V_band with new result
       !
       !aim137 check < which band has it chosen?
       !if (ik_bz == 4 .and. i_m ==4 ) then
       !print*, FL_chosen_bands(i_m), (i_m-1)*n_tot_fl_modes + (max_fl_mode+1)
       !print*, 'energy diff with KS energy =', trial_val - E_full(i_m,ik_bz,is)
       !print*, 'eigenvector highest component (abs)',abs(trial_vec((i_m-1)*n_tot_fl_modes + (max_fl_mode+1)))
       !print*, trial_vec(FL_chosen_bands(i_m)),trial_vec((i_m-1)*n_tot_fl_modes + (max_fl_mode+1))
       !endif
       !aim137 check >
       !
       do i1=NL_bands(1),NL_bands(2)
         do i1_fl=1,n_tot_fl_modes
           !
           i1_fks = i1_fl + (i1-1)*n_tot_fl_modes
           FL_V_bands(i1,i1_fl,i_m,ik_bz,is) = trial_vec(i1_fks)
           ! aim137 try mixing beta here
           ! aim137 mixing beta - it did dampen the oscillations
           ! in the accuracy, but not even 0.95 solved it
           !   FL_V_bands(i1,i1_fl,i_m,ik_bz,is) = &
           !&  FL_V_bands(i1,i1_fl,i_m,ik_bz,is) * 0.9_SP +&
           !&  trial_vec(i1_fks) * 0.1_SP
           !
         enddo
           !print*, FL_V_bands(i1,:,i_m,ik_bz,is)
       enddo
       FL_QE(i_m,ik_bz,is) = trial_val
       !aim137 try mixing beta here
       !FL_QE(i_m,ik_bz,is) = FL_QE(i_m,ik_bz,is) * 0.9_SP + trial_val * 0.1_SP
       !
       !aim137 debug - avoid flipping
       !if (FL_QE(i_m,ik_bz,is)-E_full(i_m,ik_bz,is) < 0) &
       !        & FL_QE(i_m,ik_bz,is) = E_full(i_m,ik_bz,is) - (trial_val-E_full(i_m,ik_bz,is))
       !aim137 debug
       !
    !  ! Optional: Check eigenvectors
    !  ! =============================
    !  !
    !  Vzero=cZERO
    !  size_Hamiltonian = FL_states(2) - FL_states(1) + 1
    !  call M_by_V('N',size_Hamiltonian,size_Hamiltonian,cONE,&
    !          &   FL_Ho_plus_Sigma(FL_states(1):FL_states(2),FL_states(1):FL_states(2),ik_bz,is),&
    !          &   size_Hamiltonian,FL_eigenvectors(FL_states(1):FL_states(2),FL_chosen_bands(i_m)),&
    !          &   1,cONE,Vzero(FL_states(1):FL_states(2)),1)
    !  !
    !  call V_plus_alpha_V(size_Hamiltonian, -FL_QE(i_m,ik_bz,is)*cONE, &
    !          &           FL_eigenvectors(FL_states(1):FL_states(2),FL_chosen_bands(i_m)), Vzero(FL_states(1):FL_states(2)))
    !  !
    !  ! Check that Vzero is actually zero
    !  if (sum(real(Vzero)) > 0.001) call error('Diagonalization in FL-KS space failed')
    !  if (sum(aimag(Vzero)) > 0.001) call error('Diagonalization in FL-KS space failed...')
    !  !
    !  ! end Optional
       ! ------------
       !
     enddo ! loop i_m
     !
   enddo
   !
 enddo
 !
!!aim137 DEBUG
!do ik_bz=1,QP_nk
!do is=1,n_sp_pol
!write(filename,'(i0)') ik_bz
!open(111,file='FL_eigenvectors'//trim(adjustl(filename))//'.dat')
!do i_m=NL_bands(1),E%nbf
!write(111,*) 'BAND ',i_m,', eigenvalue =',FL_QE(i_m,ik_bz,is)
!write(111,*) 'Real and imaginary parts of eigenvector:'
!do i1_fks=FL_states(1),FL_states(2)
!write(111,'(2(1f20.17))') FL_eigenvectors(i1_fks,FL_chosen_bands(i_m))
!enddo
!write(111,*) ''
!enddo
!close(111)
!enddo
!enddo
!print*, FL_V_bands(2,:,2,36,1), FL_QE(2,36,1)
!!aim137 END DEBUG
 !
end subroutine
