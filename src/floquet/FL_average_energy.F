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
subroutine FL_average_energy(FL_Hamiltonian,eigenvectors,eigenvalues,AE_vec,i_chosen_fks)
 !
 use pars,           ONLY:SP,cONE,cZERO,cI,pi,rZERO
 use units,          ONLY:HA2EV
 use com,            ONLY:msg
 use QP_m,           ONLY:QP_nk
 use fields,         ONLY:Efield
 use linear_algebra, ONLY:DIAGO
 use interfaces,     ONLY:LINEAR_ALGEBRA_driver
 use wrapper,        ONLY:M_by_V,Vstar_dot_V
 use nl_optics,      ONLY:NL_damping,NL_bands
 use fl_optics,      ONLY:max_fl_mode,n_tot_fl_modes,FL_states,l_FL_foldBZ
 !
 implicit none
 !
 complex(SP),   intent(in)     :: FL_Hamiltonian(FL_states(1):FL_states(2),FL_states(1):FL_states(2))
 complex(SP),   intent(in)     :: eigenvectors(FL_states(1):FL_states(2),FL_states(1):FL_states(2))
 real(SP),      intent(in)     :: eigenvalues(FL_states(1):FL_states(2))
 complex(SP),   intent(out)    :: AE_vec(FL_states(1):FL_states(2))
 integer,       intent(in)     :: i_chosen_fks
 !
 ! Work Space
 !
 integer     :: i1,i2,i1_fl,i2_fl,i1_fks,i2_fks,i3_fks,i1_deg,i2_deg,i_central_fks
 integer     :: subspace_dim,i_shifted_fl,Ham_dim,i_av_energy,ref_interval
 logical     :: selected_vectors(FL_states(1):FL_states(2))
 real(SP)    :: w0,AE_vec_module,qe_in_wBZ(FL_states(1):FL_states(2))
 real(SP)    :: qe_near_deg_max,qe_near_deg_min,phase_orig,phase_new
 integer     :: qe_shift(FL_states(1):FL_states(2))
 complex(SP) :: tmp_vec(FL_states(1):FL_states(2))
 complex(SP) :: Hamiltonian(FL_states(1):FL_states(2),FL_states(1):FL_states(2))
 logical     :: l_assume_Hermitian
 complex(SP),allocatable :: av_Hamiltonian(:,:),subspace_vecs(:,:)
 real(SP),allocatable    :: av_energies(:)
 complex(SP),allocatable :: NHM_Left_Vec(:,:),NHM_Right_Vec(:,:),NHM_Cmpl_Val(:)
 !
 selected_vectors = .false.
 Hamiltonian = cZERO
 AE_vec = cZERO
 qe_shift = 0
 !
 ! Set QE to interval [0,\omega)
 !
 w0 = Efield(1)%frequency(1)
 i_central_fks = (NL_bands(1)-1)*n_tot_fl_modes + (max_fl_mode+1)
 !
 if (l_FL_foldBZ) then
   ref_interval = floor(eigenvalues(i_chosen_fks)/w0)
   do i1_fks=FL_states(1),FL_states(2)
     qe_shift(i1_fks) = floor(eigenvalues(i1_fks)/w0)-ref_interval
     qe_in_wBZ(i1_fks) = eigenvalues(i1_fks) - qe_shift(i1_fks) * w0
     if (qe_in_wBZ(i1_fks) < (qe_in_wBZ(i_chosen_fks) - w0/2._SP)) then
       qe_in_wBZ(i1_fks) = qe_in_wBZ(i1_fks) + w0
       qe_shift(i1_fks) = qe_shift(i1_fks) - 1
     endif
     if (qe_in_wBZ(i1_fks) > (qe_in_wBZ(i_chosen_fks) + w0/2._SP)) then
       qe_in_wBZ(i1_fks) = qe_in_wBZ(i1_fks) - w0
       qe_shift(i1_fks) = qe_shift(i1_fks) + 1
     endif
   enddo
 else
   qe_in_wBZ = eigenvalues
 endif
 !
 ! Select degenerate subspace
 !
 qe_near_deg_max = qe_in_wBZ(i_chosen_fks)
 qe_near_deg_min = qe_in_wBZ(i_chosen_fks)
 !
 do i1_fks=FL_states(1),FL_states(2)
   !
   if (abs(qe_in_wBZ(i_chosen_fks)-qe_in_wBZ(i1_fks)) < 0.002_SP * NL_damping) then
     !
     selected_vectors(i1_fks) = .true.
     if (qe_in_wBZ(i1_fks) > qe_near_deg_max) qe_near_deg_max = qe_in_wBZ(i1_fks)
     if (qe_in_wBZ(i1_fks) < qe_near_deg_min) qe_near_deg_min = qe_in_wBZ(i1_fks)
     !
   endif
   !
 enddo
 !
 subspace_dim = count(selected_vectors)
 !
 !if (subspace_dim < 2 .or. abs(eigenvectors(i_central_fks,i_chosen_fks)) > 0.999995) then
 if (subspace_dim < 2 ) then
   !
   AE_vec(FL_states(1):FL_states(2)) = eigenvectors(FL_states(1):FL_states(2),i_chosen_fks)
   return
   !
 endif
 print*, 'dimension =',subspace_dim,' energy range=',(qe_near_deg_max-qe_near_deg_min)*HA2EV
 print*, 'chosen qe =',eigenvalues(i_chosen_fks)*HA2EV
 print*, '-- eigenvalues and main eigenvector'
 write(*,'(100(1f8.3))') eigenvalues*HA2EV
 write(*,'(100(1f12.6))') abs(eigenvectors(FL_states(1):FL_states(2),i_chosen_fks))
 !
 if (l_FL_foldBZ) then
   print*, '-- reset eigenvalues around qe =',qe_in_wBZ(i_chosen_fks)*HA2EV
   write(*,'(100(1f8.3))') qe_in_wBZ*HA2EV
 endif
 !
 ! Allocation
 !
 allocate(av_Hamiltonian(subspace_dim,subspace_dim))
 allocate(subspace_vecs(FL_states(1):FL_states(2),subspace_dim))
 allocate(av_energies(subspace_dim))
 !
 av_Hamiltonian = cZERO
 subspace_vecs  = cZERO
 av_energies    = rZERO
 !
 ! Build vector subspace
 !
 i1_deg = 1
 do i1_fks=FL_states(1),FL_states(2)
   !
   if (selected_vectors(i1_fks)) then
     !
     subspace_vecs(FL_states(1):FL_states(2),i1_deg) = eigenvectors(FL_states(1):FL_states(2),i1_fks)
     write(*,'(A5,100(1f12.6))') 'old: ',abs(subspace_vecs(FL_states(1):FL_states(2),i1_deg))
     if (l_FL_foldBZ) subspace_vecs(FL_states(1):FL_states(2),i1_deg) =&
     &    shifted_vec(eigenvectors(FL_states(1):FL_states(2),i1_fks),qe_shift(i1_fks))
     write(*,'(A5,100(1f12.6))') 'new: ',abs(subspace_vecs(FL_states(1):FL_states(2),i1_deg))
     i1_deg = i1_deg + 1
     !
   endif
   !
 enddo
 !
 ! Remove time derivative
 !
 Hamiltonian = FL_Hamiltonian
 !
 do i1=NL_bands(1),NL_bands(2)
   do i1_fl=1,n_tot_fl_modes
     i1_fks = i1_fl + (i1-1)*n_tot_fl_modes
     i_shifted_fl = i1_fl - (max_fl_mode+1)
     !
     Hamiltonian(i1_fks,i1_fks) = &
&    Hamiltonian(i1_fks,i1_fks) + real(i_shifted_fl,SP) * w0 
     !
   enddo
 enddo
 !
 ! Build average Hamiltonian
 !
 Ham_dim = FL_states(2)-FL_states(1)+1
 do i1_deg=1,subspace_dim
   do i2_deg=1,subspace_dim
   !
   tmp_vec = cZERO
   call M_by_V('N',Ham_dim,Ham_dim,cONE,&
   &    Hamiltonian(FL_states(1):FL_states(2),FL_states(1):FL_states(2)),&
   &    Ham_dim,subspace_vecs(FL_states(1):FL_states(2),i2_deg),1,cZERO,&
   &    tmp_vec(FL_states(1):FL_states(2)),1)
   !
   av_Hamiltonian(i1_deg,i2_deg) = Vstar_dot_V(Ham_dim,&
   &                               subspace_vecs(FL_states(1):FL_states(2),i1_deg),&
   &                               tmp_vec(FL_states(1):FL_states(2)))

   !
   enddo
 enddo
 !
 ! Diagonalisation
 !
 l_assume_Hermitian = .true.
 !
 if (l_assume_Hermitian) then
   !
   print*, '-- Hermitian Average Energy'
   ! option Hermitian
   call LINEAR_ALGEBRA_driver(DIAGO,M=av_Hamiltonian,E_real=av_energies)
   !aim137 overwrites matrix M with eigenvectors as columns
   !
 else
   !
   print*, '-- non-Hermitian Average Energy'
   allocate(NHM_Left_Vec(subspace_dim,subspace_dim))
   allocate(NHM_Right_Vec(subspace_dim,subspace_dim))
   allocate(NHM_Cmpl_Val(subspace_dim))
   NHM_Left_Vec   = cZERO
   NHM_Right_Vec  = cZERO
   NHM_Cmpl_Val   = cZERO
   !
   ! option non-Hermitian
   !
   call LINEAR_ALGEBRA_driver(DIAGO,M=av_Hamiltonian,&
                                  & V_left=NHM_Left_Vec,&
                                  & V_right=NHM_Right_Vec,&
                                  & E_cmpl=NHM_Cmpl_Val)
   !
   av_energies = real(NHM_Cmpl_val,SP)
   av_Hamiltonian = NHM_Right_Vec
   !
 endif
 !
 ! Choose vector
 !
 i_av_energy = minloc(av_energies,dim=1)
 !
 AE_vec = cZERO
 do i1_deg=1,subspace_dim
   !
   AE_vec = AE_vec + av_Hamiltonian(i1_deg,i_av_energy) * subspace_vecs(FL_states(1):FL_states(2),i1_deg)
   !
 enddo
 !
 ! Normalisation
 !
 AE_vec_module = real(sqrt(sum(conjg(AE_vec(:))*AE_vec(:))),SP)
 AE_vec = AE_vec / AE_vec_module
 !
 i_central_fks = (NL_bands(1)-1)*n_tot_fl_modes + (max_fl_mode+1)
 phase_orig = atan(real(eigenvectors(i_central_fks,i_chosen_fks)),aimag(eigenvectors(i_central_fks,i_chosen_fks)))
 phase_new = atan(real(AE_vec(i_central_fks)),aimag(AE_vec(i_central_fks)))
 !print*, eigenvectors(i_central_fks,i_chosen_fks),'orig phase = ',phase_orig
 AE_vec = AE_vec * conjg(AE_vec(i_central_fks))/abs(AE_vec(i_central_fks))
 !print*, AE_vec(i_central_fks),'aven phase = ', phase_new
 !
 write(*,'(A5,100(1f8.3))') 'avE= ',av_energies*HA2EV
 print*, i_av_energy
 write(*,'(A5,100(1f12.6))') 'vec= ',abs(AE_vec)
 ! Deallocation
 !
 deallocate(av_Hamiltonian)
 deallocate(subspace_vecs)
 deallocate(av_energies)
 !
 contains
  !
  function shifted_vec(vec,shift)
    !
    complex(SP) :: vec(FL_states(1):FL_states(2))
    complex(SP) :: shifted_vec(FL_states(1):FL_states(2))
    integer     :: shift,i_fks
    !
    shifted_vec = cZERO
    !
    do i_fks=FL_states(1),FL_states(2)
      !
      if ((i_fks - shift)<FL_states(1)) cycle
      if ((i_fks - shift)>FL_states(2)) cycle
      !
      shifted_vec(i_fks) = vec(i_fks-shift)
      !
    enddo
    !
  end function shifted_vec
end subroutine
