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
subroutine FL_diagonalization(E,k,FL_V_bands,FL_QE,iter_num)
 !
 use pars,           ONLY:SP,cONE,cZERO,cI,pi
 use units,          ONLY:HA2EV
 use com,            ONLY:msg
 use electrons,      ONLY:levels,n_sp_pol
 use R_lattice,      ONLY:bz_samp
 use QP_m,           ONLY:QP_nk
 use fields,         ONLY:Efield
 use linear_algebra, ONLY:DIAGO
 use interfaces,     ONLY:LINEAR_ALGEBRA_driver
 use wrapper,        ONLY:M_by_V,V_plus_alpha_V,Vstar_dot_V
 use nl_optics,      ONLY:NL_bands,E_full,NL_damping
 use fl_optics,      ONLY:max_fl_mode,n_tot_fl_modes,FL_space_dim,FL_S_plus,FL_S_minus,&
&                         FL_states,FL_Ho_plus_Sigma,FL_QE_hist,FL_ac_phase,&
&                         FL_eigenvectors,FL_eigenvalues,FL_beta,l_FL_avEnergy
 use parallel_m,     ONLY:PAR_IND_Xk_ibz,PAR_COM_Xk_ibz_INDEX,PAR_Xk_ibz_index,myid
 use parallel_int,   ONLY:PP_redux_wait
 !
 implicit none
 !
 type(levels),  intent(in)     :: E
 type(bz_samp), intent(in)     :: k
 complex(SP),   intent(inout)  :: FL_V_bands(NL_bands(2),n_tot_fl_modes,maxval(E%nbf),QP_nk,n_sp_pol)
 real(SP),      intent(inout)  :: FL_QE(NL_bands(2),QP_nk,n_sp_pol)
 integer,       intent(in)     :: iter_num
 !
 ! Work Space
 !
 real(SP)    :: min_projection,trial_projection,trial_val,trial_proj
 real(SP)    :: energy_difference(FL_states(1):FL_states(2))
 integer     :: ik_bz,i_sp_pol,is,size_Hamiltonian,i_m,ik_mem
 integer     :: i1,i2,i1_fl,i2_fl,i1_fks,i2_fks,i3_fks
 integer     :: FL_chosen_bands(NL_bands(1):maxval(E%nbf)),i1_fks_eigenvec
 character(4) :: filename,filename2
 integer     :: line_counter,i_central_fks,trial_chosen_band_QE
 complex(SP) :: Vtmp(FL_states(1):FL_states(2)),Vzero(FL_states(1):FL_states(2))
 complex(SP) :: ref_vec(FL_states(1):FL_states(2)),trial_vec(FL_states(1):FL_states(2))
 complex(SP) :: Mtmp(FL_states(1):FL_states(2),FL_states(1):FL_states(2))
 complex(SP) :: NHM_Mat(FL_states(1):FL_states(2),FL_states(1):FL_states(2))
 complex(SP) :: NHM_Left_Vec(FL_states(1):FL_states(2),FL_states(1):FL_states(2))
 complex(SP) :: NHM_Right_Vec(FL_states(1):FL_states(2),FL_states(1):FL_states(2))
 complex(SP) :: NHM_Cmpl_Val(FL_states(1):FL_states(2))
 complex(SP) :: FL_V_bands_hist(NL_bands(2),n_tot_fl_modes,maxval(E%nbf),QP_nk,n_sp_pol)
 logical     :: l_assume_Hermitian
 real(SP)    :: energy_crossing,modul,beta
 !
 FL_eigenvalues = 0._SP
 energy_difference = 0._SP
 l_assume_Hermitian = .false.
 !
 FL_V_bands_hist = FL_V_bands
 !
 do ik_bz=1,QP_nk
   !
   if(.not.PAR_IND_Xk_ibz%element_1D(ik_bz)) then
     !
     !aim137 to avoid summing previous values in PP_redux_wait, I set
     !aim137 to zero the ones that will not be calculated in this core
     !
     FL_QE(:,ik_bz,:) = 0._SP
     FL_V_bands(:,:,:,ik_bz,:) = cZERO
     cycle
     !
   endif
   !
   ik_mem=PAR_Xk_ibz_index(ik_bz)
   !
   do is=1,n_sp_pol
     !
     if (l_assume_Hermitian) then
       ! Diagonalization
       ! ===============
       !
       FL_eigenvectors(FL_states(1):FL_states(2),FL_states(1):FL_states(2),ik_mem,is) = &
&      FL_Ho_plus_Sigma(FL_states(1):FL_states(2),FL_states(1):FL_states(2),ik_mem,is)
       call LINEAR_ALGEBRA_driver(DIAGO,&
       & M=FL_eigenvectors(FL_states(1):FL_states(2),FL_states(1):FL_states(2),ik_mem,is),&
       & E_real=FL_eigenvalues(FL_states(1):FL_states(2),ik_bz,is)) 
       !aim137 overwrites matrix M with eigenvectors as columns
       !
     else
       ! Non-Hermitian Diagonalization
       ! =============================
       !
       NHM_Left_Vec = cZERO
       NHM_Right_Vec = cZERO
       NHM_Cmpl_Val = cZERO
       !
       NHM_Mat(FL_states(1):FL_states(2),FL_states(1):FL_states(2)) = &
&      FL_Ho_plus_Sigma(FL_states(1):FL_states(2),FL_states(1):FL_states(2),ik_mem,is)
       call LINEAR_ALGEBRA_driver(DIAGO,&
                                & M=NHM_Mat(FL_states(1):FL_states(2),FL_states(1):FL_states(2)),&
                                & V_left=NHM_Left_Vec(FL_states(1):FL_states(2),FL_states(1):FL_states(2)),&
                                & V_right=NHM_Right_Vec(FL_states(1):FL_states(2),FL_states(1):FL_states(2)),&
                                & E_cmpl=NHM_Cmpl_Val(FL_states(1):FL_states(2)))

       ! discard imaginary part of eigenvalue
       FL_eigenvalues(FL_states(1):FL_states(2),ik_bz,is) = real(NHM_Cmpl_Val(FL_states(1):FL_states(2)))
       ! take right eigenvectors
       FL_eigenvectors(FL_states(1):FL_states(2),FL_states(1):FL_states(2),ik_mem,is)=&
&      NHM_Right_Vec(FL_states(1):FL_states(2),FL_states(1):FL_states(2))
       !
     endif
     !
     !aim137 DEBUG<
      !print*, ' =========================================== kpt = ',ik_bz
      !print*, 'eigenvalues:'
      !print*, FL_eigenvalues(FL_states(1):FL_states(2),ik_bz,is)*HA2EV
    !write(filename,'(i0)') ik_bz
    !open(111,file='FL_QEeigensolution_kpt'//trim(adjustl(filename))//'.dat')
    !write(111,*) 'Quasi-energies [eV]'
    !write(111,'(100(1f8.3))') FL_eigenvalues(FL_states(1):FL_states(2),ik_bz,is)*HA2EV
    !write(111,*) 'Eigenvectors'
    !do i1_fks=FL_states(1),FL_states(2)
    !  !
    !  write(111,*) 'evec ',i1_fks
    !  write(111,'(100(1f7.4))') abs(FL_eigenvectors(FL_states(1):FL_states(2),i1_fks,ik_mem,is))
    !  !
    !enddo
    !close(111)
     !aim137 DEBUG>
     !
     FL_chosen_bands = 0
     !
     ! Populate FL_V_bands
     ! ===================
     !
     do i_m=NL_bands(1),E%nbf(is) ! band I want to update
       !
       ! select eigenvector - 1. highest central component
       !
       i_central_fks = (i_m-1)*n_tot_fl_modes + (max_fl_mode+1)
       !
       FL_chosen_bands(i_m) = FL_states(1) - 1 + &
&      maxloc(abs(FL_eigenvectors(i_central_fks,FL_states(1):FL_states(2),ik_mem,is)),dim=1)
       !
       ! finding closest quasi-energy to check
       !energy_difference = FL_eigenvalues(:,ik_bz,is)- E%E(i_m,ik_bz,1)
       !i3_fks = FL_states(1) - 1 +  minloc(abs(FL_eigenvalues(FL_states(1):FL_states(2),ik_bz,is)),dim=1)
       !if (i3_fks /= FL_chosen_bands(i_m)) &
       !&call warning (' the criteria for eigenpair selection are not giving the same result')
       !
       trial_vec(FL_states(1):FL_states(2)) = &
                    & FL_eigenvectors(FL_states(1):FL_states(2),FL_chosen_bands(i_m),ik_mem,is)
       trial_val = FL_eigenvalues(FL_chosen_bands(i_m),ik_bz,is)
       !
       !aim137 DEBUG <
       !print*, 'kpt: ',ik_bz,' band: ',i_m,' complex quasienergy: ', NHM_Cmpl_Val(FL_chosen_bands(i_m))
       !print*, 'real quasienergy: ',trial_val
       !aim137 DEBUG >
       !
       ! Set phase zero for central component of the eigenvector
       ! we are not doing this in the end
       !
       !i1_fks = (i_m-1)*n_tot_fl_modes + (max_fl_mode+1)
       !trial_vec = trial_vec * conjg(trial_vec(i1_fks)) / abs(trial_vec(i1_fks))
       !
       ! Average Energy eigenproblem
       !
       if (l_FL_avEnergy) call FL_average_energy(&
       &    FL_Ho_plus_Sigma(FL_states(1):FL_states(2),FL_states(1):FL_states(2),ik_mem,is),&
       &     FL_eigenvectors(FL_states(1):FL_states(2),FL_states(1):FL_states(2),ik_mem,is),&
       &      FL_eigenvalues(FL_states(1):FL_states(2),ik_bz,is),&
       &           trial_vec(FL_states(1):FL_states(2)),&
       &           FL_chosen_bands(i_m))
       !
       !print*, 'AE_vec',abs(trial_vec(FL_states(1):FL_states(2)))
       ! Detect population inversion
       !
       i_central_fks = (i_m-1)*n_tot_fl_modes + (max_fl_mode+1)
       !
       do i1_fks=FL_states(1),FL_states(2)
         !
         if (abs(trial_vec(i1_fks)) > 10.005_SP .and. i1_fks /= i_central_fks) then !aim137 critical BORRAR ESTE 10.005 for DEBUG
           !
           i1 = nint(real(i1_fks/n_tot_fl_modes,SP))+1
           i1_fl = mod(i1_fks,n_tot_fl_modes)
           if (i1_fl == 0) then
             i1_fl = n_tot_fl_modes - (max_fl_mode+1)
             i1 = i1 - 1
           else
             i1_fl = i1_fl - (max_fl_mode+1)
           endif
           !
           i1_fks_eigenvec = FL_states(1) - 1 + &
&          maxloc(abs(FL_eigenvectors(i1_fks,FL_states(1):FL_states(2),ik_mem,is)),dim=1)
           !
           print*, 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx  kpt = ',ik_bz, &
                   &' band = ',i_m,'(',E%E(i_m,ik_bz,1)*HA2EV,' eV) -- &
                   & band2 = ',i1, '(',E%E(i1 ,ik_bz,1)*HA2EV,' eV)'
!          print*, i_central_fks,i_central_fks,FL_Ho_plus_Sigma(i_central_fks,i_central_fks,ik_mem,1)
!          print*, i_central_fks,i1_fks,FL_Ho_plus_Sigma(i_central_fks,i1_fks,ik_mem,1)
!          print*, i1_fks,i_central_fks,FL_Ho_plus_Sigma(i1_fks,i_central_fks,ik_mem,1)
!          print*, i1_fks,i1_fks,FL_Ho_plus_Sigma(i1_fks,i1_fks,ik_mem,1)
           write(*,'(100(1f7.4))') trial_val*HA2EV,abs(trial_vec)
           write(*,'(100(1f7.4))') FL_eigenvalues(i1_fks_eigenvec,ik_bz,1)*HA2EV,&
                   &               abs(FL_eigenvectors(FL_states(1):FL_states(2),i1_fks_eigenvec,ik_mem,is))
          !call msg('r','Detected population inversion at k-point: ',ik_bz)
          !call msg('r','involving band: ',i_m)
          !call msg('r','and band: ',i1)
          !call msg('r','Floquet mode on second band: ',i1_fl)
          !call msg('r','Population on second band: ',abs(trial_vec(i1_fks)))
          !call warning ('  [FL] There was a large population inversion in the system')
          !
          !aim137 DEBUG<
         !write(filename,'(i0)') ik_bz
         !open(111,file='FL_population_inversion'//trim(adjustl(filename))//'.dat')
         !write(111,*) 'BAND ',i_m,', eigenvalue =',FL_QE(i_m,ik_bz,is)
         !!write(111,*) 'Real and imaginary parts of eigenvector:'
         !write(111,*) trial_vec
         !write(111,*) FL_eigenvectors(FL_states(1):FL_states(2),i1_fks_eigenvec,ik_mem,is)
         !close(111)
          !aim137 DEBUG>
           !
         endif
         !
       enddo
       !
      !! Mix with KS time-independent bands
      !! 
      !beta = 0.1_SP
      !trial_vec = trial_vec * beta
      !i_central_fks = (i_m-1)*n_tot_fl_modes + (max_fl_mode+1)
      !trial_vec(i_central_fks) = trial_vec(i_central_fks) + 1._SP * (1._SP-beta)
      !modul=sqrt(sum(conjg(trial_vec(:))*trial_vec(:)))      
      !trial_vec = trial_vec / modul
       !
       ! Check normalizaton
       !
       modul=sqrt(sum(conjg(trial_vec(:))*trial_vec(:)))      
       if (abs(modul-1)>1.E-12_SP) then
         call error (' FL some eigenvectors were not normalized')
       endif
       !
       ! Overwrite FL_V_band with new result
       !
       do i1=NL_bands(1),NL_bands(2)
         do i1_fl=1,n_tot_fl_modes
           i1_fks = i1_fl + (i1-1)*n_tot_fl_modes
           FL_V_bands(i1,i1_fl,i_m,ik_bz,is) = trial_vec(i1_fks)
         enddo
       enddo
       FL_QE(i_m,ik_bz,is) = trial_val
       !
       !aim137 DEBUG <
       !central component
       !print*, atan(aimag(FL_V_bands(4,max_fl_mode+1,4,ik_bz,is))/real(FL_V_bands(4,max_fl_mode+1,4,ik_bz,is)))
       !coupling component
       !print*, atan(aimag(FL_V_bands(5,max_fl_mode+2,4,ik_bz,is))/real(FL_V_bands(5,max_fl_mode+2,4,ik_bz,is)))
       !aim137 DEBUG >
       !
       !aim137 DEBUG AVERAGE ENERGY<
  !!!  write(filename ,'(i0)') ik_bz
  !!!  write(filename2,'(i0)') i_m
  !!!  open(111,file='FL_near_degeneracies_band_'//trim(adjustl(filename2))//'_kpt_'//trim(adjustl(filename))//'.dat')
  !!!  write(111,*) 'BAND ',i_m
  !!!  do i2_fks=FL_states(1),FL_states(2)
  !!!  if (abs(trial_val - FL_eigenvalues(i2_fks,ik_bz,is)) < 0.1/HA2EV) then
  !!!  write(111,*) FL_eigenvalues(i2_fks,ik_bz,is)*HA2EV
  !!!  write(111,*) FL_eigenvectors(FL_states(1):FL_states(2),i2_fks,ik_mem,is)
  !!!  endif
  !!!  enddo
  !!!  close(111)
       !aim137 DEBUG AVERAGE ENERGY>
       !
     enddo ! loop i_m
     !
 !!! !aim137 DEBUG AVERAGE ENERGY<
 !!! write(filename,'(i0)') ik_bz
 !!! open(111,file='FL_Hamiltonian'//trim(adjustl(filename))//'.dat')
 !!! do i2_fks=FL_states(1),FL_states(2)
 !!! write(111,*) FL_Ho_plus_Sigma(i2_fks,FL_states(1):FL_states(2),ik_mem,is)*HA2EV
 !!! enddo
 !!! close(111)
 !!! !aim137 DEBUG AVERAGE ENERGY>
     !
     !call FL_fix_band_phases(FL_V_bands(NL_bands(1):NL_bands(2),:,:,ik_bz,is),E%nbf(1),iter_num)
     !
   enddo
   !
 enddo
 !
 call PP_redux_wait(FL_QE, COMM=PAR_COM_Xk_ibz_INDEX%COMM )
 call PP_redux_wait(FL_V_bands, COMM=PAR_COM_Xk_ibz_INDEX%COMM )
 call PP_redux_wait(FL_eigenvalues, COMM=PAR_COM_Xk_ibz_INDEX%COMM )
 !
 !Mixing of eigenvectors
 !FL_V_bands = FL_V_bands * FL_beta + FL_V_bands_hist * (1._SP - FL_beta)
 !
 !aim137 DEBUG
!do ik_bz=1,QP_nk
!do is=1,n_sp_pol
!write(filename,'(i0)') ik_bz
!open(111,file='FL_eigenvectors'//trim(adjustl(filename))//'.dat')
!do i_m=NL_bands(1),E%nbf(1)
!write(111,*) 'BAND ',i_m,', eigenvalue =',FL_QE(i_m,ik_bz,is)
!!write(111,*) 'Real and imaginary parts of eigenvector:'
!do i1=NL_bands(1),NL_bands(2)
!write(111,'(100(2(1f20.14),A1))') ( FL_V_bands(i1,i1_fl,i_m,ik_bz,is),',', i1_fl=1,n_tot_fl_modes )
!!write(111,'(100(1f20.14))') atan(real(FL_V_bands(i1,:,i_m,ik_bz,is)),aimag(FL_V_bands(i1,:,i_m,ik_bz,is)))
!enddo
!write(111,*) ''
!enddo
!close(111)
!enddo
!enddo
 !aim137 END DEBUG
 !
end subroutine
