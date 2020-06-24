!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DV
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
subroutine c2y_db1(en,k,file_name_)
 !
 use pars,                  ONLY: SP, DP,pi
 use com,                   ONLY: msg,warning,error
 use electrons,             ONLY: levels, default_nel,n_bands,n_spin,n_sp_pol,&
&                                 n_spinor 
 use vec_operate,           ONLY: define_b_and_DL_vol
 use R_lattice,             ONLY: b, bz_samp, ng_vec, g_vec,nkibz
 use D_lattice,             ONLY: a, alat, input_GS_Tel,&
&                                 n_atomic_species,n_atoms_species,&
&                                 n_atoms_species_max,atom_pos,Z_species,lattice
 use wave_func,             ONLY: wf_nc_k, wf_igk,wf_ncx,wf_ng
 use xc_functionals,        ONLY: GS_xc_KIND,GS_xc_FUNCTIONAL
 use xc_functionals,        ONLY: XC_EXCHANGE_CORRELATION,XC_LDA_C_PW
 use mod_com2y,             ONLY: print_interface_dimensions,symmetries_check_and_load,&
&                                 alat_mult_factor
#include<memory.h>
 character(*)                   :: file_name_
 type(levels),     intent(out)  :: en
 type(bz_samp),    intent(out)  :: k  
 !
 !Work Space
 !
 character*80                   :: str
 character*2                    :: zel(99)
 integer                        :: irecord,nat,idum
 REAL(DP)                       :: rdum,adum(3,3)
 REAL(DP), allocatable          :: atom_pos_dp(:,:,:)
 integer                        :: i1, ik, i2, i3, inel,i_spin,ig
 logical                        :: ldum
 integer                        :: int_sop(3,3,1) ! Symmetries in RLU
 !
 !
 integer,  allocatable          :: rel_gvec(:,:)
 real(DP), allocatable          :: rstr(:)
 !
 call msg('s','KSS Header...')
 !
 ! CPMD KSS FILE
 !
 open(unit=11,file=file_name_,form='unformatted')
 !
 !     ==  Section  1: Header                                          ==
 !
 read(11) str
 call msg('s', str)
 !
 !     ==  Section  2: Symmetry and Cell info                          ==
 !
 call msg('s',":: Symmetry and Cell info...") 
 read(11) irecord
 read(11) idum,idum
 read(11) (rdum,i1=1,6),adum
 a = adum
 !
 alat(1) = maxval(abs(a(1,:)))*alat_mult_factor
 alat(2) = maxval(abs(a(2,:)))*alat_mult_factor
 alat(3) = maxval(abs(a(3,:)))*alat_mult_factor
 call crystal_lattice()
 call define_b_and_DL_vol()
 !
 !     ==  Section  3: Number of atomic species and atoms per species  ==
 !
 call msg('s',":: Atom's information ...")
 read(11) irecord
 read(11) n_atomic_species    ! Number of atomic species 
 YAMBO_ALLOC(n_atoms_species,(n_atomic_species))
 YAMBO_ALLOC(Z_species,(n_atomic_species))
 read(11) (n_atoms_species(i1),i1=1,n_atomic_species),(zel(i1),i1=1,n_atomic_species)
 do i1=1,n_atomic_species
   call assign_z_spec(zel(i1),Z_species(i1))
 enddo
 !atoms per species
 n_atoms_species_max=maxval(n_atoms_species)
 YAMBO_ALLOC(atom_pos,(3,n_atoms_species_max,n_atomic_species))
 YAMBO_ALLOC(atom_pos_dp,(3,n_atoms_species_max,n_atomic_species))
 !
 !     ==  Section  4: Atomic coordinates                              ==
 !
 nat = 0 
 read (11) irecord
 do i1=1,n_atomic_species
   do i2=1,n_atoms_species(i1) 
      read(11) (atom_pos_dp(i3,i2,i1), i3=1,3)
      nat=nat+1
   enddo
 enddo
 atom_pos = atom_pos_dp
 YAMBO_FREE(atom_pos_dp)
!
!      ==  Section  5: Atomic velocities (Skipped)                     ==
!
 read (11) irecord
 call fskip(irecord)
!
!C     ==  Section  6: Initial atomic coordinates                      ==
!
 read (11) irecord
 call fskip(irecord) 
!
!C     ==  Section  7: Cutoff, # of electrons, grid                    ==
!
  call msg('s',":: # of electrons, states, plane-waves")
  read (11) irecord
  if (irecord.eq.1) then
    read (11) rdum,rdum,ldum,inel,idum,idum,idum
    default_nel=inel
  else
    write(*,'(A)') 'C2Y: WARNING! CANNOT read SECTION 8 '
  endif
!
!C     ==  Section  8: States, dual, plane waves                       ==
!
  read (11) irecord
  if(irecord.eq.1) then
    read (11) n_bands,nkibz,wf_ng,idum,idum,idum
    if (nkibz==0) k%nibz = 1 
    nkibz =  k%nibz
    en%nb=n_bands
    wf_ng = 2*wf_ng-1  ! This is for the time-reversal
  else
    write(*,'(A)') 'C2Y:  WARNING! CANNOT read SECTION 8 '
  endif
!
!C     ==  Section  9: PW Coefficients                                 ==
!
! Here I skip the wfs, they will be read later.
 read(11) irecord
 irecord = abs(irecord) 
 call fskip(irecord) 
!
!C     ==  Section 10: PW Velocities (skipped)                          ==
!
 read(11) irecord
 irecord = abs(irecord)
 call fskip(irecord)
!
!C     ==  Section 11: Accumulators                                    ==
!
 read(11) irecord
 irecord = abs(irecord)
 call fskip(irecord)
!
!C     ==  Section 12: Nose thermostats general info                   ==
!
 read(11) irecord
 irecord = abs(irecord)
 call fskip(irecord)
!
!C     ==  Section 13: Nose thermostats electrons                      ==
!
 read(11) irecord
 irecord = abs(irecord)
 call fskip(irecord)
!
!C     ==  Section 14: Nose thermostats ions                           ==
!
 read(11) irecord
 irecord = abs(irecord)
 call fskip(irecord)
!
!C     ==  Section 15: Nose thermostats ions (ULTRA)                   ==
!
 read(11) irecord
 irecord = abs(irecord)
 call fskip(irecord)
!
!C     ==  Section 16: Nose thermostats ions (MASSIVE)                 ==
!
 read(11) irecord
 irecord = abs(irecord)
 call fskip(irecord)
!
!C     ==  Section 17: Nose thermostats cell                           ==
!
 read(11) irecord
 irecord = abs(irecord)
 call fskip(irecord)
!
!C     ==  Section 18: Potential                                       ==
!
 read(11) irecord
 irecord = abs(irecord)
 call fskip(irecord)
!
!C     ==  Section 19: PW single states                                ==
!
 read(11) irecord
 irecord = abs(irecord)
 call fskip(irecord)
!
!C     ==  Section 20: H Matrices (cell parameters)                    ==
!
 read(11) irecord
 irecord = abs(irecord)
 call fskip(irecord)
!
!C     ==  Section 21: K-Points                                        ==
!
!     If only gamma, in cpmd it looks nkpt=0 ! Now set to gamma, then look how to read the k-sampling
 YAMBO_ALLOC(k%pt,(k%nibz,3))
 k%pt = 0.
 read(11) irecord
 irecord = abs(irecord)
 call fskip(irecord)
!
!C     ==  Section 22: Electron density                                ==
!
 read(11) irecord
 irecord = abs(irecord)
 call fskip(irecord)
!
!C     ==  Section 23: Occupation numbers                              ==
!
 read(11) irecord
 irecord = abs(irecord)
 call fskip(irecord)
!
!C     ==  Section 24: Fermi energy and eigenvalues                    ==
!
 read(11) irecord
 if(irecord/=0) then
! Here read the KS eigenvalues
   call msg('s',':: Energies...')
   read(11) rdum
   i_spin = 1 
   YAMBO_ALLOC(rstr,(en%nb))
   YAMBO_ALLOC(en%E,(en%nb,k%nibz,n_spin))
   do ik=1,k%nibz
     read (11)  (rstr(i1),i1=1,en%nb) 
     en%E(:,ik,i_spin)=rstr(:)
   enddo
   YAMBO_FREE(rstr)
 else
   write(*,'(A)') 'C2Y: ERROR ! CANNOT read EIGENVALUES'
   stop
 endif
 close (11) 
!
!C     ==  Section 25: Classical Particles (coordinates and velocities)==
!C     ==  Section 26: LinRes PW Coefficients                          ==
!C     ==  Section 27: LinRes PW Velocities                            ==
!C     ==  Section 28: Partial Hessian (for microiterative TS search)  ==
!C     ==  Section 29: P-RFO status and work arrays                    ==
!C     ==  Section 30: L-BFGS history and status                       ==
!C     ==  Section 31: Adaptive tolerance status                       ==
!C     ==  Section 32: Constraints values                              ==
!C     ==  Section 33: Cell translation vector in QM/MM runs           ==
!C     ==  Section 34 - 99 : empty                                     ==
 !
 ! XC KIND/FUNCTIONAL
 !===================
 !
 call warning ('GGA not supported: switching to LDA PW')
 GS_xc_FUNCTIONAL=XC_LDA_C_PW
 GS_xc_KIND=XC_EXCHANGE_CORRELATION
 !
 ! Spin assignement, for the moment only spin-unpolarized
 !
 n_spinor=1
 n_sp_pol=1
 !
 n_spin=max(n_sp_pol,n_spinor)
 !
 input_GS_Tel = 0 
 !
 call msg('s','Symmetries...')
 !============================
 !
 int_sop = 0
 do i1=1,3
   int_sop(i1,i1,1) = 1 
 enddo
 !
 call warning ('Symmetries not supported for the moment')
 call symmetries_check_and_load(int_sop,1) 
 !
 call msg('s','RL vectors...')
 !============================
 !
 ! Here for the moment I read Gvec from external file (fort.40), next decide if 
 ! is better to rebuild the Gvec table from scratch. Anyway the time-reversal is not
 ! included. I re-build it here below.  
 open (unit=40,file='fort.40', status='old')
 read (40,*) ng_vec
 !
 ! ng_vec = wf_ng
 !
 ! time-reversal
 !
 ! ALLOCATION
 !
 YAMBO_ALLOC(wf_nc_k,(k%nibz))
 YAMBO_ALLOC(g_vec,(ng_vec,3))
 YAMBO_ALLOC(rel_gvec,(3,wf_ng))
 ig=1
 read(40,*) (rel_gvec(i2,ig),i2=1,3)
 do i1= 1,(wf_ng-1)/2
   ig=2*i1
   read(40,*) (rel_gvec(i2,ig),i2=1,3)
   ig=ig+1
   rel_gvec(:,ig)=-rel_gvec(:,ig-1)
 enddo
 close(40)
 do i1=1,wf_ng
   g_vec(i1,:)=matmul(transpose(b),rel_gvec(:,i1))*alat(:)/2./pi
 enddo
 wf_nc_k=wf_ng
 wf_ncx=maxval(wf_nc_k)
 wf_ng=wf_ncx
 YAMBO_ALLOC(wf_igk,(wf_ncx,k%nibz))
 forall(i1=1:wf_ng) wf_igk(i1,:)=i1
 !
 YAMBO_FREE(rel_gvec)
 !
 !
 call msg('l','done')
 !
 call msg('s','Report:')
 !======================
 !
 call print_interface_dimensions(en,k)
 !
end subroutine
!
subroutine fskip(nrec)
 !
 implicit none
 integer nrec,i1
 !
 if (nrec.lt.0) then
   write(*,*) ' FSKIP! NREC=',nrec
 endif
 do i1=1,nrec
    read(11,END=20,ERR=30)
 enddo
 !
 return
 !
20   continue
 write(*,*) "FSKIP, END OF FILE"
30   continue
 write(*,*) "FSKIP, read error"
end subroutine
!
subroutine assign_z_spec(what,Z)
 !
 implicit none
 character*2  what 
 integer Z 
 !
 select case(trim(what))
      case(' H')
        Z=1
      case(' C')
        Z=6
      case(' N')
        Z=7
      case(' O')
        Z=8
      case DEFAULT
        write(*,*) "c2y_db1: Atom ", what ," not found, please add it in assign_z_spec routine"
 end select
end subroutine

