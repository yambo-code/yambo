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
subroutine FL_average_energy(FL_Hamiltonian,eigenvectors,eigenvalues,AE_vec,i_central_fks)
 !
 use pars,           ONLY:SP,cONE,cZERO,cI,pi
 use units,          ONLY:HA2EV
 use com,            ONLY:msg
 use QP_m,           ONLY:QP_nk
 use fields,         ONLY:Efield
 use linear_algebra, ONLY:DIAGO
 use interfaces,     ONLY:LINEAR_ALGEBRA_driver
 use wrapper,        ONLY:M_by_V,Vstar_dot_V
 use nl_optics,      ONLY:NL_damping,NL_bands
 use fl_optics,      ONLY:max_fl_mode,n_tot_fl_modes,FL_states
 !
 implicit none
 !
 complex(SP),   intent(in)     :: FL_Hamiltonian(FL_states(1):FL_states(2),FL_states(1):FL_states(2))
 complex(SP),   intent(in)     :: eigenvectors(FL_states(1):FL_states(2),FL_states(1):FL_states(2))
 real(SP),      intent(in)     :: eigenvalues(FL_states(1):FL_states(2))
 complex(SP),   intent(out)    :: AE_vec(FL_states(1):FL_states(2))
 integer,       intent(in)     :: i_central_fks
 !
 ! Work Space
 !
 integer     :: i1,i2,i1_fl,i2_fl,i1_fks,i2_fks,i3_fks,i1_deg,i2_deg
 integer     :: subspace_dim,i_shifted_fl,Ham_dim,i_av_energy
 logical     :: selected_vectors(FL_states(1):FL_states(2))
 real(SP)    :: w0,AE_vec_module
 complex(SP) :: tmp_vec(FL_states(1):FL_states(2))
 complex(SP) :: Hamiltonian(FL_states(1):FL_states(2),FL_states(1):FL_states(2))
 complex(SP),allocatable :: av_Hamiltonian(:,:),subspace_vecs(:,:)
 real(SP),allocatable    :: av_energies(:)
 !
 selected_vectors = .false.
 Hamiltonian = cZERO
 AE_vec = cZERO
 !
 ! Set QE to interval [0,\omega)
 !
 ! COMPLETAR
 !
 ! Select degenerate subspace
 !
 do i1_fks=FL_states(1),FL_states(2)
   !
   if (abs(eigenvalues(i_central_fks)-eigenvalues(i1_fks)) < 2*NL_damping) then
     !
     selected_vectors(i1_fks) = .true.
     !
   endif
   !
 enddo
 !
 subspace_dim = count(selected_vectors)
 !
 if (subspace_dim < 2) then
   !
   AE_vec(FL_states(1):FL_states(2)) = eigenvectors(FL_states(1):FL_states(2),i_central_fks)
   return
   !
 endif
 !
 ! Allocation
 !
 allocate(av_Hamiltonian(subspace_dim,subspace_dim))
 allocate(subspace_vecs(FL_states(1):FL_states(2),subspace_dim))
 allocate(av_energies(subspace_dim))
 av_Hamiltonian = cZERO
 subspace_vecs = cZERO
 av_energies = cZERO
 !
 ! Build vector subspace
 !
 i1_deg = 1
 do i1_fks=FL_states(1),FL_states(2)
   !
   if (selected_vectors(i1_fks)) then
     !
     subspace_vecs(FL_states(1):FL_states(2),i1_deg) = eigenvectors(FL_states(1):FL_states(2),i1_fks)
     i1_deg = i1_deg + 1
     !
   endif
   !
 enddo
 !
 ! Remove time derivative
 !
 w0 = Efield(1)%frequency(1)
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
 call LINEAR_ALGEBRA_driver(DIAGO,M=av_Hamiltonian,E_real=av_energies)
 !aim137 overwrites matrix M with eigenvectors as columns
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
 ! Deallocation
 !
 deallocate(av_Hamiltonian)
 deallocate(subspace_vecs)
 deallocate(av_energies)
 !
end subroutine
