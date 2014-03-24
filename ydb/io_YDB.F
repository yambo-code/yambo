!
! Copyright (C) 2000-2014 A. Marini and the YAMBO team 
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
integer function io_YDB(PATH,ID)
 !
 use pars,             ONLY:SP,IP,schlen
 use units,            ONLY:HA2EV
 use com,              ONLY:error
 use memory_m,         ONLY:mem_est
 use LOGO,             ONLY:code_version,code_revision
 use electrons,        ONLY:levels,default_nel,n_spin,n_bands,nel,n_sp_pol,&
&                           n_spinor
 use D_lattice,        ONLY:a, alat, nsym, i_time_rev, input_GS_Tel, dl_sop, &
&                           Tel, n_atoms_species_max,n_atomic_species,n_atoms_species,atom_pos, &
&                           Z_species, n_atoms, mag_syms
 use R_lattice,        ONLY:ng_vec, g_vec, bz_samp,nkibz,&
&                           qp_states_k,nXkibz,ng_closed
 use wave_func,        ONLY:wf_ncx, wf_nc_k, wf_igk,wf_ng,wf_nb_io
 use IO_m,             ONLY:io_connect, io_disconnect, io_sec, &
&                           io_elemental, io_status, io_bulk,read_is_on,&
&                           serial_number,io_serial_number,io_code_revision,&
&                           DUMP,io_mode,write_is_on,io_code_version,ver_is_gt_or_eq
 use QP_m,             ONLY:QP_nk,QP_nb,QP_n_G_bands
 use BS,               ONLY:BS_bands
 use xc_functionals,   ONLY:GS_xc_KIND,GS_xc_FUNCTIONAL
 use global_XC,        ONLY:setup_global_XC,EXT_NONE
 !
 implicit none
 !
 integer       :: ID
 character(*)  :: PATH
 !

 write (*,*) trim(PATH),'<'
 io_YDB=io_connect(desc=PATH,type=-2,ID=ID)
 write (*,*) trim(PATH),'>'
 !
 call io_disconnect(ID=ID)
 !
end function io_YDB
