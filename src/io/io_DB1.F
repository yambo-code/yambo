!
!        Copyright (C) 2000-2019 the YAMBO team
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
integer function io_DB1(E,k,ID)
 !
 use pars,             ONLY:SP
 use units,            ONLY:HA2EV
 use com,              ONLY:error,msg
 use C_driver,         ONLY:code_version,code_revision,code_GPL_revision
 use electrons,        ONLY:levels,default_nel,n_spin,n_bands,nel,n_sp_pol,&
&                           n_spinor
 use D_lattice,        ONLY:a, alat, nsym, i_time_rev, input_GS_Tel, dl_sop, &
&                           Tel, n_atoms_species_max,n_atomic_species,n_atoms_species,atom_pos, &
&                           Z_species, n_atoms, mag_syms
 use R_lattice,        ONLY:ng_vec, g_vec, bz_samp,nkibz,&
&                           QP_states_k,nXkibz,ng_closed
 use wave_func,        ONLY:wf_ncx, wf_nc_k, wf_igk,wf_ng,wf_nb_io
 use LIVE_t,           ONLY:live_timing_is_on
 use IO_m,             ONLY:io_connect, io_disconnect, io_sec,variable_exist, &
&                           io_elemental, io_status, io_bulk,read_is_on,&
&                           serial_number,io_serial_number,io_code_revision,&
&                           DUMP,io_mode,write_is_on,io_code_version,ver_is_gt_or_eq
 use QP_m,             ONLY:QP_nk,QP_nb,QP_n_G_bands
 use BS,               ONLY:BS_bands
 use xc_functionals,   ONLY:GS_xc_KIND,GS_xc_FUNCTIONAL,GS_exx_FRACTION,GS_exx_SCREENING
 use global_XC,        ONLY:setup_global_XC,EXT_NONE
#if defined _SC || defined _RT
 use collision_ext,    ONLY:COLL_bands
 use hamiltonian,      ONLY:H_ref_bands
#endif
#if defined _SC
 use SC,               ONLY:SC_bands
#endif
#if defined _NL
 use nl_optics,        ONLY:NL_bands
#endif
#if defined _RT
 use real_time,        ONLY:RT_bands
#endif
#include<memory.h>
 !
 type(levels)  :: E
 type(bz_samp) :: k
 integer       :: ID
 !
 ! Work Space
 !
 integer :: code_version_local(3),code_revision_local,MENU
 !
 io_DB1 =io_connect(desc="db1",type=0,ID=ID)
 if (io_DB1/=0) goto 1
 !
 MENU=1
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   ! Dimensions
   !
   call io_elemental(ID, VAR="DIMENSIONS",VAR_SZ=18)
   !
   ! Davide: WARNING, VAR_SZ is set to 18 instead of 17
   !         I'm not changing it to avoid compatibility problems
   !
   ! Meaning of DIMENSIONS tag in ioDB1 file:
   !
   !  1 Serial Number 
   !  2 Major version   3 Minor version   4 Patchlevel
   !  5 Revision                code_revision    	
   !  6 Bands                   E%nb
   !  7 k-points                k%nibz
   !  8 G-vectors (total)       ng_vec
   !  9 G-vectors (wfc/shell)   wf_ncx
   ! 10 Time reversal flag      i_time_rev
   ! 11 Symmetries              nsym
   ! 12 Spinor cmpts            n_spinor
   ! 13 Spins                   n_sp_pol
   ! 14 Temperature             default_Tel
   ! 15 Electrons               default_nel
   ! 16 XC kind                 GS_xc_KIND
   ! 17 XC functional           GS_xc_FUNCTIONAL 
   ! 
   ! Added dimensions: (still in SEC=1)
   !
   !------------ 3.0.3 ------------------
   !    G-vectors (wfc/igk)     ng_wf
   !------------ 3.0.4 ------------------
   !    Max atoms/species       max_atom_species
   !    No. of atom species     n_atom_species
   !------------ ?.?.? ------------------
   !    EXX fraction            GS_exx_FRACTION
   !    EXX screening           GS_exx_SCREENING 
   !------------ r. 4351 ----------------
   !    code_GPL_revision
   !
   ! ONLY DB1 can define the serial_number 
   !
   call io_elemental(ID,I0=serial_number,DB_I0=serial_number)
   io_serial_number(ID)=serial_number
   !
   ! Code_version/code_version here and not in io_header because io_header
   ! writes the serial_number as well.
   !
   if (write_is_on(ID)) code_version_local=code_version
   call io_elemental(ID,DB_I1=io_code_version(ID,:),I1=code_version_local)
   !
   if (write_is_on(ID)) code_revision_local=code_revision
   call io_elemental(ID,DB_I0=io_code_revision(ID,1),I0=code_revision_local)
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
   call io_elemental(ID, UNIT=HA2EV,R0=input_GS_Tel,CHECK=.true.,OP=(/"=="/),&
&                    VAR=' Temperature                 [ev]:')
   call io_elemental(ID, R0=default_nel,CHECK=.true.,OP=(/"=="/),&
&                    VAR=' Electrons                       :')
   call io_elemental(ID, I0=GS_xc_KIND)
   call io_elemental(ID, I0=GS_xc_FUNCTIONAL)
   call io_elemental(ID, VAR="",VAR_SZ=0,MENU=0) 
   !
   !=------------ DIMENSIONS END -----------------=
   !
   ! Check for old databases not compatible with libxc
   !
   if (.not.ver_is_gt_or_eq(ID,(/3,2,2/))) call compatibility_message( )
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
&                      VAR=' WF G-vectors                    :')
   endif
   !
   ! Atomic data
   !
   if (ver_is_gt_or_eq(ID,(/3,0,4/))) then 
     call io_elemental(ID, VAR="MAX_ATOMS",VAR_SZ=1,MENU=0)
     call io_elemental(ID,I0=n_atoms_species_max,   CHECK=.true.,OP=(/"=="/),&
&                      VAR=' Max atoms/species               :')
     !
     call io_elemental(ID, VAR="number_of_atom_species",VAR_SZ=1,MENU=0)
     call io_elemental(ID,I0=n_atomic_species,CHECK=.true.,OP=(/"=="/),&
&                      VAR=' No. of atom species             :')
   endif
   !
   ! Hybrids stuff
   !	
   if (ver_is_gt_or_eq(ID,(/4,3,0/))) then
     if (variable_exist(ID,"EXX_FRACTION").or.write_is_on(ID)) then
       call io_elemental(ID, VAR="EXX_FRACTION",VAR_SZ=1,MENU=0)
       call io_elemental(ID,R0=GS_exx_FRACTION,   CHECK=.true.,OP=(/"=="/),&
&                           VAR=' Exact exchange fraction in XC   :')
     endif 
     if (variable_exist(ID,"EXX_SCREENING").or.write_is_on(ID)) then
       call io_elemental(ID, VAR="EXX_SCREENING",VAR_SZ=1,MENU=0)
       call io_elemental(ID,R0=GS_exx_SCREENING,   CHECK=.true.,OP=(/"=="/),&
&                              VAR=' Exact exchange screening in XC  :')
     endif
   endif
   !
   if (ver_is_gt_or_eq(ID,revision=1240).and.ver_is_gt_or_eq(ID,(/3,2,5/))) then
     call io_elemental(ID, VAR="mag_syms",VAR_SZ=1,MENU=0)
     call io_elemental(ID, L0=mag_syms,VAR=' Magnetic symmetries             : ')
   endif
   !
   if (ver_is_gt_or_eq(ID,revision=4351)) then
     call io_elemental(ID, VAR="GPL_REVISION",VAR_SZ=1,MENU=0)
     if (write_is_on(ID)) code_revision_local=code_GPL_revision
     call io_elemental(ID,DB_I0=io_code_revision(ID,2),I0=code_revision_local)
   else
     io_code_revision(ID,2)=code_GPL_revision
   endif
   !
   call io_elemental(ID, VAR="",VAR_SZ=0,MENU=MENU)
   MENU=0
   !
   ! Exporting E%nb/k%nibz when in DUMP
   !
   if (io_mode(ID)==DUMP.or.write_is_on(ID)) then
     !
     nkibz =k%nibz
     nXkibz=k%nibz
     QP_nk =k%nibz
     E%nk =k%nibz
     QP_states_k=(/1,k%nibz/)
     !
     n_bands=E%nb
     wf_nb_io=E%nb
     QP_nb=E%nb
     QP_n_G_bands=(/1,E%nb/)
     !
     if(any(BS_bands(:)==0)) BS_bands=(/1,E%nb/)
#if defined _SC || defined _RT
     if(any(COLL_bands(:)==0)) COLL_bands=(/1,E%nb/)
     if(any(H_ref_bands(:)==0)) H_ref_bands=(/1,E%nb/)
#endif
#if defined _SC
     if(any(SC_bands(:)==0)) SC_bands=(/1,E%nb/)
#endif
#if defined _RT
     if(any(RT_bands(:)==0)) RT_bands=(/1,E%nb/)
#endif
#if defined _NL
     if(any(NL_bands(:)==0)) NL_bands=(/1,E%nb/)
#endif
     !
     Tel=input_GS_Tel
     nel=default_nel
     !
     ng_closed=ng_vec
     if (.not.ver_is_gt_or_eq(ID,(/3,0,3/))) wf_ng=wf_ncx
     !
     call setup_global_XC('preset',GS_xc_KIND,xc_functional=GS_xc_functional,perturbation=EXT_NONE,&
                          exx_fraction=GS_exx_FRACTION,exx_screening=GS_exx_SCREENING)
     !
   endif
   !
   io_DB1=io_status(ID)
   if (io_DB1/=0) goto 1
   !
 endif
 !
 if (.not.any((/io_sec(ID,:)==2/))) goto 1
 !
 if (read_is_on(ID)) then
   ! 
   YAMBO_ALLOC(E%E,(E%nb,E%nk,n_sp_pol))
   YAMBO_ALLOC(E%f,(E%nb,E%nk,n_sp_pol))
   E%f=0._SP
   !
   YAMBO_ALLOC(g_vec,(ng_vec,3))
   YAMBO_ALLOC(dl_sop,(3,3,nsym))
   YAMBO_ALLOC(k%pt,(k%nibz,3))
   YAMBO_ALLOC(wf_nc_k,(k%nibz))
   YAMBO_ALLOC(wf_igk,(wf_ncx,k%nibz))
   !
   if (ver_is_gt_or_eq(ID,(/3,0,4/))) then 
     YAMBO_ALLOC(n_atoms_species,(n_atomic_species))
     YAMBO_ALLOC(Z_species,(n_atomic_species))
     YAMBO_ALLOC(atom_pos,(3,n_atoms_species_max,n_atomic_species))
     !
   endif
   !
 endif
 !
 ! Cell
 !
 call io_bulk(ID,VAR="LATTICE_PARAMETER",VAR_SZ=(/3/))
 call io_bulk(ID,R1=alat)
 !
 call io_bulk(ID,VAR="LATTICE_VECTORS",VAR_SZ=(/3,3/))
 call io_bulk(ID,R2=a)
 !
 ! Atoms
 !
 if (ver_is_gt_or_eq(ID,(/3,0,4/))) then 
   call io_bulk(ID,VAR="N_ATOMS",VAR_SZ=(/n_atomic_species/))
   call io_bulk(ID,I1=n_atoms_species)
   call io_bulk(ID,VAR="ATOM_POS",VAR_SZ=shape(atom_pos))
   call io_bulk(ID,R3=atom_pos)
   call io_bulk(ID,VAR="atomic_numbers",VAR_SZ=(/n_atomic_species/))
   call io_bulk(ID,I1=Z_species)
   !
   if (io_mode(ID)==DUMP.or.write_is_on(ID))  n_atoms=sum(n_atoms_species) 
   !
 endif
 !
 ! Symmetry
 !
 call io_bulk(ID, VAR="SYMMETRY", VAR_SZ=shape(dl_sop) )
 call io_bulk(ID, R3=dl_sop)
 !
 ! G-vectors
 !
 ! Andrea (May 2010): Note here that in p2y g_vec may be allocated bigger then
 ! ng_vec*3 due to the larger charge RL vectors.
 !
 call io_bulk(ID, VAR="G-VECTORS", VAR_SZ=(/ng_vec,3/) )
 call io_bulk(ID, R2=g_vec(:ng_vec,:))
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
 contains
   !
   subroutine compatibility_message( )
     !
     live_timing_is_on=.FALSE.
     !
     call msg("s","The present DB1 database is NOT compatible with the current Yambo version." )
     call msg("s","Re-create the DB1 databases with a more recent version of the code")
     call msg("s"," ")
     !
     call error("I/O failed.")
     !
   end subroutine
   !
end function io_DB1
