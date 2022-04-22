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
 use nl_optics,      ONLY:NL_bands,E_full
 use fl_optics,      ONLY:max_fl_mode,n_tot_fl_modes,FL_space_dim,FL_S_plus,FL_S_minus,&
&                         FL_states,FL_Ho_plus_Sigma,FL_QE_hist,FL_V_bands_hist,FL_ac_phase
 use parallel_m,     ONLY:PAR_IND_Xk_ibz,PAR_COM_Xk_ibz_INDEX,PAR_Xk_ibz_index,myid
 use parallel_int,   ONLY:PP_redux_wait
 !
 implicit none
 !
 type(levels),  intent(in)     :: E
 type(bz_samp), intent(in)     :: k
 integer,       intent(in)     :: iter_num
 complex(SP),   intent(inout)  :: FL_V_bands(NL_bands(2),n_tot_fl_modes,E%nbf,QP_nk,n_sp_pol)
 real(SP),      intent(inout)  :: FL_QE(NL_bands(2),QP_nk,n_sp_pol)
 !
 ! Work Space
 !
 real(SP)    :: FL_eigenvalues(FL_states(1):FL_states(2),QP_nk,n_sp_pol)
 real(SP)    :: min_projection,trial_projection,trial_val,trial_proj
 real(SP)    :: energy_difference(FL_states(1):FL_states(2))
 complex(SP) :: FL_eigenvectors(FL_states(1):FL_states(2),FL_states(1):FL_states(2))
 integer     :: ik_bz,i_sp_pol,is,size_Hamiltonian,i_m,ik_mem
 integer     :: i1,i2,i1_fl,i2_fl,i1_fks,i2_fks,i3_fks
 integer     :: FL_chosen_bands(NL_bands(1):E%nbf)
 character(4) :: filename
 integer     :: line_counter,i_central_fks,trial_chosen_band_QE
 complex(SP) :: Vtmp(FL_states(1):FL_states(2)),Vzero(FL_states(1):FL_states(2))
 complex(SP) :: ref_vec(FL_states(1):FL_states(2)),trial_vec(FL_states(1):FL_states(2))
 complex(SP) :: Mtmp(FL_states(1):FL_states(2),FL_states(1):FL_states(2))
 complex(SP) :: NHM_Mat(FL_states(1):FL_states(2),FL_states(1):FL_states(2))
 complex(SP) :: NHM_Left_Vec(FL_states(1):FL_states(2),FL_states(1):FL_states(2))
 complex(SP) :: NHM_Right_Vec(FL_states(1):FL_states(2),FL_states(1):FL_states(2))
 complex(SP) :: NHM_Cmpl_Val(FL_states(1):FL_states(2))
 logical     :: l_assume_Hermitian
 real(SP)    :: energy_crossing
 !
 FL_eigenvalues = 0._SP
 energy_difference = 0._SP
 l_assume_Hermitian = .false.
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
        FL_eigenvectors(FL_states(1):FL_states(2),FL_states(1):FL_states(2)) = &
&      FL_Ho_plus_Sigma(FL_states(1):FL_states(2),FL_states(1):FL_states(2),ik_mem,is)
       call LINEAR_ALGEBRA_driver(DIAGO,&
                                & M=FL_eigenvectors(FL_states(1):FL_states(2),FL_states(1):FL_states(2)),&
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
       FL_eigenvectors(FL_states(1):FL_states(2),FL_states(1):FL_states(2))=&
&      NHM_Right_Vec(FL_states(1):FL_states(2),FL_states(1):FL_states(2))
       !
     endif
     !
     FL_chosen_bands = 0
     !
     ! Populate FL_V_bands
     ! ===================
     !
     do i_m=NL_bands(1),E%nbf ! band I want to update
       !
       ! select eigenvector - 1. highest central component
       !
       i_central_fks = (i_m-1)*n_tot_fl_modes + (max_fl_mode+1)
       !
       FL_chosen_bands(i_m) = FL_states(1) - 1 + &
&      maxloc(abs(FL_eigenvectors(i_central_fks,FL_states(1):FL_states(2))),dim=1)
       !
       ! aim137 prelim - I could confirm this choice with a dot product between
       ! the chosen eigenvector and the band from the previous iteration (ref_vec)
       !
       trial_vec(FL_states(1):FL_states(2)) = &
                    & FL_eigenvectors(FL_states(1):FL_states(2),FL_chosen_bands(i_m))
       trial_val = FL_eigenvalues(FL_chosen_bands(i_m),ik_bz,is)
       !
       ! select eigenvector - 2. by Quasi-Energy
       !
       do i1_fks=FL_states(1),FL_states(2)
       energy_difference(i1_fks) = &
                    &   abs(FL_eigenvalues(i1_fks,ik_bz,is) &
                    &     - E_full(i_m,ik_bz,is))
       enddo
       !
       trial_chosen_band_QE = FL_states(1) - 1 + &
&      minloc(energy_difference(FL_states(1):FL_states(2)),dim=1)
       !
       if (FL_chosen_bands(i_m) /= trial_chosen_band_QE) then
         !
         energy_crossing = abs( &
&        energy_difference(FL_chosen_bands(i_m)) - &
&        energy_difference(trial_chosen_band_QE))
         !
         !call warning ('   There are Quasi-Energy crossings in the system')
         !
         if (energy_crossing > 1.E-4_SP) then
           !
           ! ref_vec is FL_V_band from previous iteration
           !
           call error('  went through eigenvector route') !aim137 critical
           ref_vec = cZERO
           !
           do i1=NL_bands(1),NL_bands(2)
             do i1_fl=1,n_tot_fl_modes
               !
               i1_fks = i1_fl + (i1-1)*n_tot_fl_modes !don't need SP here, it's an integer
               ref_vec(i1_fks) = FL_V_bands(i1,i1_fl,i_m,ik_bz,is) 
               !aim137 this is now ok as the ik_bz'th component of FL_V_bands was not set
               ! to zero if the code has reached this point in that ik_bz.
               !
             enddo
           !
           enddo
           !
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
         endif ! eigenvector route
         !
       endif
       !
       !
       !
       ! Set phase zero for central component of the eigenvector
       ! 
       ! we are not doing this in the end
       !
       !i1_fks = (i_m-1)*n_tot_fl_modes + (max_fl_mode+1)
       !trial_vec = trial_vec * conjg(trial_vec(i1_fks)) / abs(trial_vec(i1_fks))
       !
       ! Detect population inversion
       !
       i_central_fks = (i_m-1)*n_tot_fl_modes + (max_fl_mode+1)
       do i1_fks=FL_states(1),FL_states(2)
         !
         if (abs(trial_vec(i1_fks)) > 0.5_SP .and. i1_fks /= i_central_fks) then
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
           call msg('r','Detected population inversion at k-point: ',ik_bz)
           call msg('r','involving band: ',i_m)
           call msg('r','and band: ',i1)
           call msg('r','Floquet mode on second band: ',i1_fl)
           call msg('r','Population on second band: ',abs(trial_vec(i1_fks)))
           call error ('  [FL] There was a large population inversion in the system')
           !
         endif
         !
       enddo
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
       !
     enddo ! loop i_m
     !
   enddo
   !
 enddo
 !
 call PP_redux_wait(FL_QE, COMM=PAR_COM_Xk_ibz_INDEX%COMM )
 call PP_redux_wait(FL_V_bands, COMM=PAR_COM_Xk_ibz_INDEX%COMM )
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
