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
integer function io_Double_Grid(E,Xk,ID)
 !
 use electrons,      ONLY:levels,BZ_DbGd_nkpt,BZ_DbGd_table,BZ_DbGd_max_filling,&
&                         BZ_DbGd_nbands,BZ_DbGd_tot_nk_levels,n_sp_pol,BZ_DbGd_weight,&
&                         DbGd_is_symmetry_expanded,BZ_ASSOCIATED_nkpts,BZ_DbGd_kpts,&
&                         BZ_DbGd_tot_nk
 use R_lattice,      ONLY:nXkbz,bz_samp
 use IO_m,           ONLY:io_connect,io_disconnect,io_elemental,read_is_on,&
&                         io_bulk,io_header,ver_is_gt_or_eq,file_is_present,write_is_on,&
&                         IO_OUTDATED_DB
 use timing_m,       ONLY:timing
#include<memory.h>
 !
 type(levels) :: E
 type(bz_samp):: Xk
 integer      :: ID 
 !
 ! Work Space
 !
 integer :: DG_format
 !
 call timing('io_Double_Grid',OPR='start')
 !
 io_Double_Grid=-1
 !
 if (file_is_present('Double_Grid').or.write_is_on(ID)) then
   io_Double_Grid=io_connect(desc='Double_Grid',type=2,ID=ID)
   DG_format=3
 endif
 if (file_is_present('RIM_E_and_k').and.io_Double_Grid<0) then
   io_Double_Grid=io_connect(desc='RIM_E_and_k',type=2,ID=ID)
   DG_format=2
 endif
 if (file_is_present('E_RIM').and.io_Double_Grid<0) then
   io_Double_Grid=io_connect(desc='E_RIM',type=2,ID=ID)
   DG_format=1
 endif
 !
 if (io_Double_Grid/=0) goto 1
 !
 io_Double_Grid=io_header(ID)
 !
 if (.not.ver_is_gt_or_eq(ID,revision=3560)) io_Double_Grid=IO_OUTDATED_DB
 !
 if (io_Double_Grid/=0) goto 1
 !
 if (DG_format==1) then
   call io_elemental(ID,VAR="PARS",VAR_SZ=4,MENU=1)
 else
   call io_elemental(ID,VAR="PARS",VAR_SZ=6,MENU=1)
 endif
 !
 call io_elemental(ID,I0=BZ_DbGd_nbands,        VAR=' Bands                           :')
 call io_elemental(ID,I0=BZ_DbGd_tot_nk)
 call io_elemental(ID,I0=BZ_DbGd_tot_nk_levels, VAR=' Fine grid number of points      :')
 ! 
 if (DG_format==3) then
   call io_elemental(ID,I0=BZ_ASSOCIATED_nkpts, VAR=' BZ ASSOCIATED k-points          :')
 else
   BZ_ASSOCIATED_nkpts=nXkbz
 endif
 !
 call io_elemental(ID,I0=BZ_DbGd_max_filling,VAR=' BZ Blocks max filling           :')
 !
 if (DG_format==3) then
   call io_elemental(ID,L0=DbGd_is_symmetry_expanded,VAR=' Points expanded in the BZ       :')
 else
   DbGd_is_symmetry_expanded=.TRUE.
 endif
 call io_elemental(ID,VAR="",VAR_SZ=0,MENU=1)
 !
 if (read_is_on(ID)) then
   YAMBO_ALLOC(E%E_DbGd,(BZ_DbGd_nbands,BZ_DbGd_tot_nk_levels,n_sp_pol))
   YAMBO_ALLOC(E%W_DbGd,(BZ_DbGd_nbands,BZ_DbGd_tot_nk_levels,n_sp_pol))
   YAMBO_ALLOC(E%f_DbGd,(BZ_DbGd_nbands,BZ_DbGd_tot_nk_levels,n_sp_pol))
   YAMBO_ALLOC(BZ_DbGd_kpts,(BZ_DbGd_tot_nk,3))
   YAMBO_ALLOC(BZ_DbGd_nkpt,(BZ_ASSOCIATED_nkpts))
   YAMBO_ALLOC(BZ_DbGd_table,(BZ_ASSOCIATED_nkpts,BZ_DbGd_max_filling,2))
   YAMBO_ALLOC(BZ_DbGd_weight,(BZ_DbGd_max_filling))
 endif
 !
 ! Eigenvalues
 !
 if (DG_format==3) then
   call io_bulk(ID, VAR="EIGENVALUES", VAR_SZ=shape(E%E_DbGd) ) 
 else
   call io_bulk(ID, VAR="RIM_EIGENVALUES", VAR_SZ=shape(E%E_DbGd) ) 
 endif
 call io_bulk(ID, R3=E%E_DbGd )
 !
 ! Kpts
 !
 if (DG_format>1) then
   if (DG_format==3) then
     call io_bulk(ID, VAR="KPTS", VAR_SZ=shape(BZ_DbGd_kpts) ) 
   else
     call io_bulk(ID, VAR="RIM_KPTS", VAR_SZ=shape(BZ_DbGd_kpts) ) 
   endif
   call io_bulk(ID, R2=BZ_DbGd_kpts )
 endif
 !
 ! Block filling
 !
 if (DG_format==3) then
   call io_bulk(ID, VAR="BLOCK_FILLING", VAR_SZ=shape(BZ_DbGd_nkpt) ) 
 else
   call io_bulk(ID, VAR="RIM_BLOCK_FILLING", VAR_SZ=shape(BZ_DbGd_nkpt) ) 
 endif
 call io_bulk(ID, I1=BZ_DbGd_nkpt )
 !
 ! Block table
 !
 if (DG_format==3) then
   call io_bulk(ID, VAR="BLOCK_TABLE", VAR_SZ=shape(BZ_DbGd_table) ) 
 else
   call io_bulk(ID, VAR="RIM_BLOCK_TABLE", VAR_SZ=shape(BZ_DbGd_table) ) 
 endif
 call io_bulk(ID, I3=BZ_DbGd_table )
 !
 ! Double grid interpolation weights of each element in table
 !
 if (DG_format==3) then
   call io_bulk(ID, VAR="INTERPOL_WEIGHT", VAR_SZ=shape(BZ_DbGd_weight) ) 
 else
   call io_bulk(ID, VAR="RIM_INTERPOL_WEIGHT", VAR_SZ=shape(BZ_DbGd_weight) ) 
 endif
 call io_bulk(ID, R1=BZ_DbGd_weight)
 !
1 call io_disconnect(ID=ID)
 !
 call timing('io_Double_Grid',OPR='stop')
 !
end function
