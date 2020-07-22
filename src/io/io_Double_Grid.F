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
 use electrons,      ONLY:levels,E_FineGd_components_alloc
 use R_lattice,      ONLY:bz_samp,BZ_FineGd_mode,bz_samp_FineGd_alloc
 use IO_m,           ONLY:io_connect,io_disconnect,io_elemental,read_is_on,&
&                         io_bulk,io_header,ver_is_gt_or_eq,write_is_on,&
&                         IO_OUTDATED_DB,variable_exist
 use timing_m,       ONLY:timing
 !
#include<memory.h>
 !
 type(levels) :: E
 type(bz_samp):: Xk
 integer      :: ID 
 !
 ! Work Space
 !
 logical :: l_R_LATT
 integer :: VAR_SZ
 !
 call timing('io_Double_Grid',OPR='start')
 !
 io_Double_Grid=-1
 !
 io_Double_Grid=io_connect(desc='Double_Grid',type=2,ID=ID)
 !
 if (io_Double_Grid/=0) goto 1
 !
 l_R_LATT=(write_is_on(ID).or.variable_exist(ID,"HEAD_R_LATT"))
 io_Double_Grid=io_header(ID,R_LATT=l_R_LATT,IMPOSE=l_R_LATT,NOWARN_SER=.true.)
 !
 if (.not.ver_is_gt_or_eq(ID,revision=3560)) io_Double_Grid=IO_OUTDATED_DB
 !
 if (variable_exist(ID,'KPTS'))              io_Double_Grid=IO_OUTDATED_DB 
 !
 if (io_Double_Grid/=0) goto 1
 !
 if (ver_is_gt_or_eq(ID,revision=18605)) then
   call io_elemental(ID,VAR='FineGd_mode',CH0="",VAR_SZ=1,MENU=0)
   call io_elemental(ID,CH0=BZ_FineGd_mode,   VAR=' Brillouin Symmetry mode         ')
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
 endif
 call io_elemental(ID,VAR="PARS",VAR_SZ=6,MENU=0)
 !
 call io_elemental(ID,I0=E%nb_FineGd,         VAR=' Bands                           ')
 call io_elemental(ID,I0=E%nk_FineGd,         VAR=' Number of points (fine+coarse)  in the IBZ (E)')
 call io_elemental(ID,I0=Xk%FGibz%N,          VAR=' Number                          in the IBZ (k)')
 call io_elemental(ID,I0=Xk%FGbz%N,           VAR=' Number                          in the  BZ (k)')
 call io_elemental(ID,I0=Xk%FGibz%N_duplicate,VAR=' Number of duplic. pts (fine)        in the IBZ')
 call io_elemental(ID,I0=Xk%FGbz%N_duplicate, VAR=' Number                              in the  BZ')
 ! 
 call io_elemental(ID,VAR="",VAR_SZ=0,MENU=1)
 !
 ! Eigenvalues
 !
 if (read_is_on(ID)) then
   call E_FineGd_components_alloc(E)
 endif
 !
 call io_bulk(ID, VAR="EIGENVALUES", VAR_SZ=shape(E%E_FineGd) ) 
 call io_bulk(ID, R3=E%E_FineGd )
 !
 call io_bulk(ID, VAR="E_K_MAP", VAR_SZ=shape(E%k_map_FineGd) ) 
 call io_bulk(ID, I1=E%k_map_FineGd )
 !
 call io_bulk(ID, VAR="E_K_RANGE", VAR_SZ=shape(E%k_range_FineGd) ) 
 call io_bulk(ID, I2=E%k_range_FineGd )
 !
 ! Kpts
 !
 if (read_is_on(ID)) then
   call bz_samp_FineGd_alloc(Xk%FGibz,Xk%nibz)
   call bz_samp_FineGd_alloc(Xk%FGbz,Xk%nbz)
 endif
 !
 call io_bulk(ID, VAR="KPTS_IBZ", VAR_SZ=shape(Xk%FGibz%pt) ) 
 call io_bulk(ID, R2=Xk%FGibz%pt )
 !
 call io_bulk(ID, VAR="WEIGHTS_IBZ", VAR_SZ=shape(Xk%FGibz%weights) ) 
 call io_bulk(ID, R1=Xk%FGibz%weights )
 !
 call io_bulk(ID, VAR="KPTS_BZ", VAR_SZ=shape(Xk%FGbz%pt) ) 
 call io_bulk(ID, R2=Xk%FGbz%pt )
 !
 call io_bulk(ID, VAR="WEIGHTS_BZ", VAR_SZ=shape(Xk%FGbz%weights) ) 
 call io_bulk(ID, R1=Xk%FGbz%weights )
 !
 call io_bulk(ID, VAR="IBZ_E_MAP", VAR_SZ=(/Xk%FGibz%N/) )
 call io_bulk(ID, I1=Xk%FGibz%E_map )
 !
 call io_bulk(ID, VAR="BZ_E_MAP", VAR_SZ=(/Xk%FGbz%N/) )
 call io_bulk(ID, I1=Xk%FGbz%E_map )
 !
 ! Block filling
 !
 call io_bulk(ID, VAR="BLOCK_FILLING_IBZ", VAR_SZ=(/Xk%nibz/) ) 
 call io_bulk(ID, I1=Xk%FGibz%N_mapped )
 !
 call io_bulk(ID, VAR="BLOCK_FILLING_BZ", VAR_SZ=(/Xk%nbz/) ) 
 call io_bulk(ID, I1=Xk%FGbz%N_mapped )
 !
 ! Block table
 !
 call io_bulk(ID, VAR="K_RANGE_IBZ", VAR_SZ=shape(Xk%FGibz%k_range) ) 
 call io_bulk(ID, I2=Xk%FGibz%k_range )
 !
 call io_bulk(ID, VAR="K_RANGE_BZ", VAR_SZ=shape(Xk%FGbz%k_range) ) 
 call io_bulk(ID, I2=Xk%FGbz%k_range )
 !
1 call io_disconnect(ID=ID)
 !
 call timing('io_Double_Grid',OPR='stop')
 !
end function
