!
!        Copyright (C) 2000-2020 the YAMBO team
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
module mod_com2y
 ! 
 use pars,          ONLY:SP,PI,lchlen,schlen,DP
 !
 implicit none
 !
 logical :: use_SI_instead_of_TR
 logical :: force_noWFs
 logical :: force_noSYMM
 logical :: artificial_spin_pol
 logical :: verboseIO
 !
 integer :: ng_vec_abinit, wf_nb_io_user
 !
 real(SP):: alat_mult_factor
 !
 contains
   !
   subroutine interface_presets(in_string)
     !
     use stderr,         ONLY:STRING_split
     use LIVE_t,         ONLY:live_timing_is_on
     use parallel_m,     ONLY:master_cpu
     !
     implicit none
     !
     character(*)   :: in_string
     !
     character(schlen)::str_piece(100)
     integer          ::i1
     !
     live_timing_is_on=master_cpu
     !
     ! Split the string in pieces
     !
     call STRING_split(in_string,str_piece)
     !
     use_SI_instead_of_TR =index(in_string,'notr')>0
     force_noSYMM         =index(in_string,'nosy')>0
     force_noWFs          =index(in_string,'nowf')>0
     artificial_spin_pol  =index(in_string,'dupl')>0
     verboseIO            =index(in_string,'verb')>0
     !
     alat_mult_factor=1.
     wf_nb_io_user=0
     do i1=1,100
       if ( trim(str_piece(i1))=='alat_f')  read(str_piece(i1+1),*) alat_mult_factor
       if ( trim(str_piece(i1))=='fragnb')  read(str_piece(i1+1),*) wf_nb_io_user
     enddo
     !
   end subroutine
   !
   subroutine symmetries_check_and_load(int_sop,nsym_,trevsym,t_rev)
     !
     use com,       ONLY:msg
     use electrons, ONLY:l_spin_orbit,n_spin_den
     use D_lattice, ONLY:nsym,dl_sop,i_time_rev,a,mag_syms,inv_index,&
&                        atoms_spatial_inversion,i_space_inv,alat
     use R_lattice, ONLY:b,rl_sop
     !
     implicit none
     !
     integer :: nsym_
     integer :: int_sop(3,3,nsym_) ! Symmetries in RLU
     logical, optional :: trevsym
     integer, optional :: t_rev(nsym_)
     !
     integer :: is,is_inv,is_spatial,is_trev,is_yambo,i2,i3
     logical :: l_identity,l_inversion
     logical :: double_symmetries
     real(SP):: sop_mat(3,3)
     !
     mag_syms=.false.
     inv_index=-1
     if(present(t_rev).and.present(trevsym)) then
       i_time_rev=0
       if(trevsym) i_time_rev=1
       if(any(t_rev(:)==1)) then
         mag_syms=.true.
         i_time_rev=1
       endif
       double_symmetries=.not.mag_syms
     else
       i_time_rev=1
       if(l_spin_orbit.and.n_spin_den==4) i_time_rev=0
       double_symmetries=(i_time_rev==1)
     endif
     !
     ! Space Invertion (possible only for v >= 3.0.4)
     !
     call atoms_spatial_inversion()
     !
     if (i_space_inv==0) call msg("l","[SI no]...")
     if (i_space_inv==1) call msg("l","[SI yes]...")
     !
     ! Search for identity and inversion symmetries.
     !
     l_identity=.false.
     l_inversion=.false.
     !
     ! Force not to read symmetries and set the first
     ! symmetry equal to the identity
     !
     if(force_noSYMM) then
       nsym_=1
       int_sop(:,:,1)=0
       int_sop(1,1,1)=1
       int_sop(2,2,1)=1
       int_sop(3,3,1)=1
     endif
     !
     do is=1,nsym_
       if (int_sop(1,1,is)==1.and.int_sop(1,2,is)==0.and.int_sop(1,3,is)==0.and.&
&          int_sop(2,1,is)==0.and.int_sop(2,2,is)==1.and.int_sop(2,3,is)==0.and.&
&          int_sop(3,1,is)==0.and.int_sop(3,2,is)==0.and.int_sop(3,3,is)==1) &
&          l_identity=.true.
       if (int_sop(1,1,is)==-1.and.int_sop(1,2,is)==0.and.int_sop(1,3,is)==0.and.&
&          int_sop(2,1,is)==0.and.int_sop(2,2,is)==-1.and.int_sop(2,3,is)==0.and.&
&          int_sop(3,1,is)==0.and.int_sop(3,2,is)==0.and.int_sop(3,3,is)==-1) then
         l_inversion=.true.
         is_inv=is
       endif 
     enddo
     !
     if (.not.l_identity) call msg("l","[I no]...")
     if (l_identity)      call msg("l","[I yes]...")
     !
     ! Special case where I,-I present: no TR used.
     !
     if (l_identity.and.l_inversion) then
       !
       ! If -I is a space inversion I keep both I and -I
       ! and switch off TR if there is no SO coupling
       !
       if (i_space_inv==1.and..not.mag_syms) i_time_rev=0
       if (i_space_inv==0) call error('-I is not a proper symmetry operation')
     endif
     !
     ! Identity was stripped in outkss: swap all
     ! symmetries with their inversions
     !
     if (.not.l_identity.and.l_inversion) then
       call msg('s','[SYMS] Identity not found: swapping all symmetries')
       int_sop=-int_sop
     endif
     !
     if (force_noSYMM) i_time_rev=0
     ! 
     ! when the TR is imposed to be removed from the user it must be replaced
     ! by the spatial inversion. Otherwise it cannot be removed.
     !
     if (i_time_rev==0) double_symmetries=.false.
     if (i_time_rev==1.and.use_SI_instead_of_TR.and.i_space_inv==1.and..not.mag_syms) then 
       i_time_rev=0
       double_symmetries=.not.l_inversion
     endif
     !
     if (.not.l_inversion) call msg("l","[-I no]...")
     if (l_inversion)      call msg("l","[-I yes]...")
     !
     if (i_time_rev==1) call msg("l","[TR yes]")
     if (i_time_rev==0) call msg("l","[TR no]")
     !
     ! Double the simmetries if TR or SI is added
     !
     if (mag_syms) call msg('s','[MAG-SYMS] T-rev with (A) Magnetic field or (B) SOC and magnetization')
     !
     nsym=nsym_
     if (double_symmetries) nsym=2*nsym_
     !
     !From abinit help ...
     !
     !The relation between the above symmetry matrices symrel,
     !expressed in the basis of primitive translations, and the same symmetry
     !matrices expressed in cartesian coordinates, is as follows.
     !Denote the matrix whose columns are the primitive
     !translations as A, and denote the cartesian symmetry matrix as R. Then
     !R_rlu = A(inverse) * R * A
     !
     !In my case A=transpose(a)
     !
     !R= transpose(a) R_rlu inverse[transpose(a)]
     !
     !but inverse[transpose(a)]= b/2./pi
     !
     allocate(dl_sop(3,3,nsym))
     is_spatial=0
     is_trev=0
     do is=1,nsym_
       if(mag_syms) then
         ! PWscf: I need to reorder the symmetries
         if(t_rev(is)==0) then
           is_spatial=is_spatial+1
           is_yambo=is_spatial
         else
           is_trev=is_trev+1
           is_yambo=nsym_/2+is_trev
         endif
       else
         is_yambo=is
       endif
       sop_mat=matmul(transpose(a),int_sop(:,:,is))
       dl_sop(:,:,is_yambo)=matmul(sop_mat,b)/2._SP/pi
       if (double_symmetries) dl_sop(:,:,is_yambo+nsym_)=-dl_sop(:,:,is_yambo)
       if (mag_syms.and.(is_yambo>nsym/(1+i_time_rev))) dl_sop(:,:,is_yambo)=-dl_sop(:,:,is_yambo)
     enddo
     !
     allocate(rl_sop(3,3,nsym))
     do is=1,nsym
       forall (i2=1:3,i3=1:3) rl_sop(i2,i3,is)=dl_sop(i2,i3,is)*alat(i2)/alat(i3)
     enddo
     !
   end subroutine
   !
   subroutine print_interface_dimensions(en,k)
     use com,                   ONLY : msg
     use electrons,             ONLY : levels, default_nel, n_spin, n_sp_pol, &
&                                      n_spinor, l_spin_orbit
     use R_lattice,             ONLY : bz_samp, ng_vec, g_vec
     use D_lattice,             ONLY : nsym, i_time_rev, i_space_inv, dl_sop, &
&                                      DL_vol, a, alat, input_GS_Tel, &
&                                      n_atoms_species_max,n_atomic_species,n_atoms_species, atom_pos, &
&                                      Z_species
     use wave_func,             ONLY : wf_nc_k, wf_ncx, wf_igk, wf_ng,real_wavefunctions
     use xc_functionals,        ONLY : GS_xc_KIND,GS_xc_FUNCTIONAL,xc_string
     use vec_operate,           ONLY : v_is_zero
     !
     implicit none
     !
     type(levels),     intent(in)   :: en     ! Energies
     type(bz_samp),    intent(in)   :: k      ! K/Q points
     ! 
     call msg('s',' Electrons             ',default_nel)
     call msg('s',' Temperature           ',input_GS_Tel,"[eV]")
     call msg('s',' Lattice factors       ',alat,"[a.u.]")
     call msg('s',' K-points              ',k%nibz)
     call msg('s',' Bands                 ',en%nb)
     call msg('s',' Spinor components     ',n_spinor)
     call msg('s',' Spin polarizations    ',n_sp_pol)
     call msg('s',' Spin orbit coupling    ',l_spin_orbit)
     call msg('s',' Symmetries   [spatial]',nsym/(i_time_rev+1))
     call msg('s','                [T-rev] ',i_time_rev==1)
     call msg('s',' Max WF components     ',wf_ncx)
     call msg('s',' RL vectors        (WF)',wf_ng)
     call msg('s',' RL vectors    (CHARGE)',ng_vec)
     call msg('s',' XC potential           ',xc_string(GS_xc_FUNCTIONAL))
     call msg('s',' Atomic species        ',n_atomic_species)
     call msg('s',' Max atoms/species     ',n_atoms_species_max)
     !
     ! Check if real wavefunction are possible (gamma point only).
     !
     real_wavefunctions=k%nibz==1.and.v_is_zero(k%pt(1,:))
     !
     return
     !
   end subroutine print_interface_dimensions
   !
end module
