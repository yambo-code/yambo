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
!==============================
 use pars,             ONLY:HARTREE,SP,IP
 use memory_m,         ONLY:mem_est
 use com,              ONLY:code_version
 use electrons,        ONLY:levels,default_nel,n_spin,n_bands,nel,n_sp_pol,n_spinor
 use D_lattice,        ONLY:a, alat, nsym, i_time_rev, default_Tel, dl_sop,Tel
 use R_lattice,        ONLY:ng_vec, g_vec, bz_samp,nkibz,&
&                           qp_states_k,nXkibz,ng_closed
 use wave_func,        ONLY:wf_ng_disk, ng_wf_k, wf_igk,wf_ng
 use IO_m,             ONLY:io_connect, io_disconnect, io_sec, &
&                           io_elemental, io_status, io_bulk,read_is_on,&
&                           serial_number,io_header,io_serial_number,&
&                           DUMP,io_mode,write_is_on,io_code_version
 use QP_m,             ONLY:QP_nk,QP_nb,QP_n_G_bands
 use BS,               ONLY:BS_bands
 use xc_functionals,   ONLY:GS_xc_KIND,GS_xc_FUNCTIONAL
 implicit none
 type(levels)  :: E
 type(bz_samp) :: k
 integer       :: ID

 ioDB1 =io_connect(desc="db1",type=0,ID=ID)
 if (ioDB1/=0) goto 1

 if (any((/io_sec(ID,:)==1/))) then
!
! Dimensions
!
   call io_elemental(ID, VAR="DIMENSIONS",VAR_SZ=16)
!
! ONLY DB1 can define the serial_number 
!
   call io_elemental(ID,I0=serial_number,DB_I0=serial_number)
   io_serial_number(ID)=serial_number

   call io_elemental(ID,DB_I1=io_code_version(ID,:),I1=code_version)
  
   call io_elemental(ID,I0=E%nb,CHECK=.true.,OP=(/"=="/),&
&                    VAR=' Bands                           :')
   call io_elemental(ID,I0=k%nibz,CHECK=.true.,OP=(/"=="/),&
&                    VAR=' K-points                        :')
   call io_elemental(ID,I0=ng_vec,CHECK=.true.,OP=(/"=="/),&
&                    VAR=' G-vectors             [RL space]:')
   call io_elemental(ID,I0=wf_ng_disk,CHECK=.true.,OP=(/"=="/),&
&                    VAR='                  [wavefunctions]:')
   call io_elemental(ID,I0=i_time_rev,CHECK=.true.,OP=(/"=="/))
   if (i_time_rev==1) then
     call io_elemental(ID,I0=nsym,CHECK=.true.,OP=(/"=="/),&
&                      VAR=' Symmetries       [spatial+T-rev]:')
   else
     call io_elemental(ID,I0=nsym,CHECK=.true.,OP=(/"=="/),&
&                      VAR=' Symmetries             [spatial]:')
   endif
   call io_elemental(ID,I0=n_spinor,CHECK=.true.,OP=(/"=="/),&
&                    VAR=' Spinor components               :')
   call io_elemental(ID,I0=n_sp_pol,CHECK=.true.,OP=(/"=="/),&
&                    VAR=' Spin polarizations              :')
!
! Spin Components
!-----------------
   n_spin=max(n_sp_pol,n_spinor,1)

   call io_elemental(ID, UNIT=HARTREE,R0=default_Tel,CHECK=.true.,OP=(/"=="/),&
&                    VAR=' Temperature                 [ev]:')
   call io_elemental(ID, R0=default_nel,CHECK=.true.,OP=(/"=="/),&
&                    VAR=' Electrons                       :')
   call io_elemental(ID, I0=GS_xc_KIND)
   call io_elemental(ID, I0=GS_xc_FUNCTIONAL)

   call io_elemental(ID, VAR="",VAR_SZ=0)
!
! Exporting E%nb/k%nibz when in DUMP
!-------------------------------------
!
   if (io_mode(ID)==DUMP.or.write_is_on(ID)) then
   
     nkibz =k%nibz
     nXkibz=k%nibz
     QP_nk =k%nibz
     E%nk =k%nibz
     qp_states_k=(/1,k%nibz/)

     n_bands=E%nb
     QP_nb=E%nb
     QP_n_G_bands=(/1,E%nb/)
     BS_bands=(/1,E%nb/)

     Tel=default_Tel
     nel=default_nel

     ng_closed=ng_vec
     wf_ng=wf_ng_disk

   endif

   ioDB1=io_status(ID)
   if (ioDB1/=0) goto 1

 endif

 if (.not.any((/io_sec(ID,:)==2/))) goto 1
!
 if (read_is_on(ID)) then
  
   allocate(E%E(E%nb,E%nk,n_spin),E%f(E%nb,E%nk,n_spin),&
&           E%W(E%nb,E%nk),E%Z(E%nb,E%nk))
   call mem_est("E-E E-f E-W E-Z",&
&               (/size(E%E),size(E%f),size(E%W),size(E%Z)/),&
&               (/SP,        SP,        SP,        2*SP/) )
   E%f=0._SP
   E%W=0._SP
   E%Z=(0._SP,0._SP)

   allocate(g_vec(ng_vec,3),k%pt(k%nibz,3))
   allocate(dl_sop(3,3,nsym),ng_wf_k(k%nibz))
   allocate(wf_igk(wf_ng_disk,k%nibz))

   call mem_est("g_vec k_pt dl_sop ng_wf_k wf_igk",&
&               (/size(g_vec),size(k%pt),size(dl_sop),size(ng_wf_k),size(wf_igk)/),&
&               (/SP,        SP,        SP,           IP,         IP/) )

 endif
!
! Cell
!
 call io_bulk(ID,VAR="LATTICE_PARAMETER",VAR_SZ=(/3/))
 call io_bulk(ID,R1=alat)

 call io_bulk(ID,VAR="LATTICE_VECTORS",VAR_SZ=(/3,3/))
 call io_bulk(ID,R2=a)
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
  call io_bulk(ID, VAR="WFC_NG", VAR_SZ=shape(ng_wf_k) )
  call io_bulk(ID, I1=ng_wf_k )

  call io_bulk(ID, VAR="WFC_GRID", VAR_SZ=shape(wf_igk) )
  call io_bulk(ID, I2=wf_igk )

1 call io_disconnect(ID=ID)

 end function ioDB1
