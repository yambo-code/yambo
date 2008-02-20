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
integer function ioDB1(E,k,ID)
 !
 use pars,             ONLY:HARTREE,SP,IP
 use memory_m,         ONLY:mem_est
 use com,              ONLY:code_version
 use electrons,        ONLY:levels,default_nel,n_spin,n_bands,nel,n_sp_pol,&
&                           n_spinor
 use D_lattice,        ONLY:a, alat, nsym, i_time_rev, default_Tel, dl_sop, &
&                           Tel, n_atoms_max,n_atom_species,n_atoms,atom_pos, &
&                           znucl
 use R_lattice,        ONLY:ng_vec, g_vec, bz_samp,nkibz,&
&                           qp_states_k,nXkibz,ng_closed
 use wave_func,        ONLY:wf_ncx, wf_nc_k, wf_igk,wf_ng
 use IO_m,             ONLY:io_connect, io_disconnect, io_sec, &
&                           io_elemental, io_status, io_bulk,read_is_on,&
&                           serial_number,io_serial_number,&
&                           DUMP,io_mode,write_is_on,io_code_version,ver_is_gt_or_eq
 use QP_m,             ONLY:QP_nk,QP_nb,QP_n_G_bands
 use BS,               ONLY:BS_bands
 use xc_functionals,   ONLY:GS_xc_KIND,GS_xc_FUNCTIONAL 
 use global_XC,        ONLY:setup_global_XC
#if defined _SC
 use SC,               ONLY:SC_bands
#endif
 !
 implicit none
 !
 type(levels)  :: E
 type(bz_samp) :: k
 integer       :: ID,code_version_local(3)
 !
 ioDB1 =io_connect(desc="db1",type=0,ID=ID)
 if (ioDB1/=0) goto 1
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   ! Dimensions
   !
   call io_elemental(ID, VAR="DIMENSIONS",VAR_SZ=16)
   !
   ! Meaning of DIMENSIONS tag in ioDB1 file:
   !
   !  1 Serial Number 
   !  2 Major version   3 Minor version   4 Patchlevel
   !  5 Bands                   E%nb
   !  6 k-points                k%nibz
   !  7 G-vectors (total)       ng_vec
   !  8 G-vectors (wfc/shell)   wf_ncx
   !  9 Time reversal flag      i_time_rev
   ! 10 Symmetries              nsym
   ! 11 Spinor cmpts            n_spinor
   ! 12 Spins                   n_sp_pol
   ! 13 Temperature             default_Tel
   ! 14 Electrons               default_nel
   ! 15 XC kind                 GS_xc_KIND
   ! 16 XC functional           GS_xc_FUNCTIONAL 
   !
   ! Added dimensions: (still in SEC=1)
   !
   !------------ 3.0.3 ------------------
   !    G-vectors (wfc/igk)     ng_wf
   !------------ 3.0.4 ------------------
   !    Max atoms/species       max_atom_species
   !    No. of atom species     n_atom_species
   !
   ! ONLY DB1 can define the serial_number 
   !
   call io_elemental(ID,I0=serial_number,DB_I0=serial_number)
   io_serial_number(ID)=serial_number
   !
   ! Code_version here and not in io_header because io_header
   ! writes the serial_number as well.
   !
   if (write_is_on(ID)) code_version_local=code_version
   call io_elemental(ID,DB_I1=io_code_version(ID,:),I1=code_version_local)
   ! 
   call io_elemental(ID,I0=E%nb,CHECK=.true.,OP=(/"=="/),&
&                    VAR=' Bands                           :')
   call io_elemental(ID,I0=k%nibz,CHECK=.true.,OP=(/"=="/),&
&                    VAR=' K-points                        :')
   call io_elemental(ID,I0=ng_vec,CHECK=.true.,OP=(/"=="/),&
&                    VAR=' G-vectors             [RL space]:')
   call io_elemental(ID,I0=wf_ncx,CHECK=.true.,OP=(/"=="/),&
&                    VAR=' Components       [wavefunctions]:')
   call io_elemental(ID,I0=i_time_rev,CHECK=.true.,OP=(/"=="/))
   !
   if (i_time_rev==1) then
     call io_elemental(ID,I0=nsym,CHECK=.true.,OP=(/"=="/),&
&                      VAR=' Symmetries       [spatial+T-rev]:')
   else
     call io_elemental(ID,I0=nsym,CHECK=.true.,OP=(/"=="/),&
&                      VAR=' Symmetries             [spatial]:')
   endif
   !
   call io_elemental(ID,I0=n_spinor,CHECK=.true.,OP=(/"=="/),&
&                    VAR=' Spinor components               :')
   call io_elemental(ID,I0=n_sp_pol,CHECK=.true.,OP=(/"=="/),&
&                    VAR=' Spin polarizations              :')
   !
   ! Spin Components
   !
   n_spin=max(n_sp_pol,n_spinor,1)
   !
   call io_elemental(ID, UNIT=HARTREE,R0=default_Tel,CHECK=.true.,OP=(/"=="/),&
&                    VAR=' Temperature                 [ev]:')
   call io_elemental(ID, R0=default_nel,CHECK=.true.,OP=(/"=="/),&
&                    VAR=' Electrons                       :')
   call io_elemental(ID, I0=GS_xc_KIND)
   call io_elemental(ID, I0=GS_xc_FUNCTIONAL)
   call io_elemental(ID, VAR="",VAR_SZ=0,MENU=0) 
   !
   !=------------ DIMENSIONS END -----------------=
   !
   ! Wavefunction G-vector components
   !---------------------------------
   !
   ! This IO entry has been added only in ver. 3.0.3
   ! to correctly handle PW use of igk indexes.
   ! Note the difference between wf_ncx, wf_nc_k and wf_ng. In PW in general
   ! wf_ng = maxval(wf_igk) > wf_ncx
   !
   if (ver_is_gt_or_eq(ID,(/3,0,3/))) then
     !
     call io_elemental(ID, VAR="WF_G_COMPONENTS",VAR_SZ=1,MENU=0)
     call io_elemental(ID,I0=wf_ng,CHECK=.true.,OP=(/"=="/),&
&                      VAR=' WF G-vectors        (ver.>3.0.3):')
   endif
   !
   ! Atomic data
   !
   if (ver_is_gt_or_eq(ID,(/3,0,4/))) then 
     call io_elemental(ID, VAR="MAX_ATOMS",VAR_SZ=1,MENU=0)
     call io_elemental(ID,I0=n_atoms_max,   CHECK=.true.,OP=(/"=="/),&
&                      VAR=' Max atoms/species   (ver.>3.0.4):')
     !
     call io_elemental(ID, VAR="number_of_atom_species",VAR_SZ=1,MENU=0)
     call io_elemental(ID,I0=n_atom_species,CHECK=.true.,OP=(/"=="/),&
&                      VAR=' No. of atom species (ver.>3.0.4):')
   endif
   call io_elemental(ID, VAR="",VAR_SZ=0)
   !
   ! Exporting E%nb/k%nibz when in DUMP
   !
   if (io_mode(ID)==DUMP.or.write_is_on(ID)) then
     !
     nkibz =k%nibz
     nXkibz=k%nibz
     QP_nk =k%nibz
     E%nk =k%nibz
     qp_states_k=(/1,k%nibz/)
     !
     n_bands=E%nb
     QP_nb=E%nb
     QP_n_G_bands=(/1,E%nb/)
     BS_bands=(/1,E%nb/)
#if defined _SC
     SC_bands=E%nb
#endif
     !
     Tel=default_Tel
     nel=default_nel
     !
     ng_closed=ng_vec
     if (.not.ver_is_gt_or_eq(ID,(/3,0,3/))) wf_ng=wf_ncx
     !
     call setup_global_XC('preset',GS_xc_KIND,xc_functional=GS_xc_functional)
     !
   endif
   !
   ioDB1=io_status(ID)
   if (ioDB1/=0) goto 1
   !
 endif
 !
 if (.not.any((/io_sec(ID,:)==2/))) goto 1
 !
 if (read_is_on(ID)) then
   ! 
   allocate(E%E(E%nb,E%nk,n_sp_pol),E%f(E%nb,E%nk,n_sp_pol))
   call mem_est("E-E E-f",(/size(E%E),size(E%f)/),(/SP,SP/))
   E%f=0._SP
   !
   allocate(g_vec(ng_vec,3),k%pt(k%nibz,3))
   allocate(dl_sop(3,3,nsym),wf_nc_k(k%nibz))
   allocate(wf_igk(wf_ncx,k%nibz))
   !
   call mem_est("g_vec k_pt dl_sop wf_nc_k wf_igk",&
&               (/size(g_vec),size(k%pt),size(dl_sop),size(wf_nc_k),size(wf_igk)/),&
&               (/SP,        SP,        SP,           IP,         IP/) )
   !
   if (ver_is_gt_or_eq(ID,(/3,0,4/))) then 
     allocate(n_atoms(n_atom_species))
     allocate(znucl(n_atom_species))
     allocate(atom_pos(3,n_atoms_max,n_atom_species))
     call mem_est("n_atoms atom_pos znucl",&
&                 (/n_atom_species,size(atom_pos),n_atom_species/),&
&                 (/IP,SP,IP/))
   endif
   !
 endif
 !
 ! Cell
 !
 call io_bulk(ID,VAR="LATTICE_PARAMETER",VAR_SZ=(/3/))
 call io_bulk(ID,R1=alat)

 call io_bulk(ID,VAR="LATTICE_VECTORS",VAR_SZ=(/3,3/))
 call io_bulk(ID,R2=a)
 !
 ! Atoms
 !
 if (ver_is_gt_or_eq(ID,(/3,0,4/))) then 
   call io_bulk(ID,VAR="N_ATOMS",VAR_SZ=(/n_atom_species/))
   call io_bulk(ID,I1=n_atoms)
   call io_bulk(ID,VAR="ATOM_POS",VAR_SZ=shape(atom_pos))
   call io_bulk(ID,R3=atom_pos)
   call io_bulk(ID,VAR="atomic_numbers",VAR_SZ=(/n_atom_species/))
   call io_bulk(ID,I1=znucl)
 endif
 !
 ! Symmetry
 !
 call io_bulk(ID, VAR="SYMMETRY", VAR_SZ=shape(dl_sop) )
 call io_bulk(ID, R3=dl_sop)
 !
 ! G-vectors
 !
 call io_bulk(ID, VAR="G-VECTORS", VAR_SZ=shape(g_vec) )
 call io_bulk(ID, R2=g_vec)
 !
 ! K-point grid
 !
 call io_bulk(ID, VAR="K-POINTS", VAR_SZ=shape(k%pt) )
 call io_bulk(ID, R2=k%pt)
 !
 ! Eigenvalues
 !
 call io_bulk(ID, VAR="EIGENVALUES", VAR_SZ=shape(E%E) )
 call io_bulk(ID, R3=E%E )
 !
 ! Wfc grids
 !
 call io_bulk(ID, VAR="WFC_NG", VAR_SZ=shape(wf_nc_k) )
 call io_bulk(ID, I1=wf_nc_k )
 !
 call io_bulk(ID, VAR="WFC_GRID", VAR_SZ=shape(wf_igk) )
 call io_bulk(ID, I2=wf_igk )
 !
1 call io_disconnect(ID=ID)
 !
end function ioDB1
