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
 use electrons,      ONLY:levels,n_sp_pol
 use R_lattice,      ONLY:bz_samp
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
 call io_elemental(ID,VAR="PARS",VAR_SZ=6,MENU=0)
 !
 call io_elemental(ID,I0=E%nb_FineGd,        VAR=' Bands                           ')
 call io_elemental(ID,I0=E%nk_FineGd,        VAR=' Number of points  in the IBZ (E)')
 call io_elemental(ID,I0=Xk%FGibz%N,     VAR=' Number of points  in the IBZ (k)')
 call io_elemental(ID,I0=Xk%FGbz%N,      VAR=' Number of points  in the  BZ (k)')
 call io_elemental(ID,I0=Xk%FGibz%N_duplicate,  VAR=' Number of duplic. pts in the IBZ')
 call io_elemental(ID,I0=Xk%FGbz%N_duplicate,   VAR=' Number of duplic. pts in the  BZ')
 ! 
 call io_elemental(ID,VAR="",VAR_SZ=0,MENU=1)
 !
 ! Eigenvalues
 !
 if (read_is_on(ID)) then
   YAMBO_ALLOC(E%E_FineGd,(E%nb_FineGd,E%nk_FineGd,n_sp_pol))
   YAMBO_ALLOC(E%W_FineGd,(E%nb_FineGd,E%nk_FineGd,n_sp_pol))
   YAMBO_ALLOC(E%f_FineGd,(E%nb_FineGd,E%nk_FineGd,n_sp_pol))
 endif
 !
 call io_bulk(ID, VAR="EIGENVALUES", VAR_SZ=shape(E%E_FineGd) ) 
 call io_bulk(ID, R3=E%E_FineGd )
 !
 ! Kpts
 !
 if (read_is_on(ID)) then
   YAMBO_ALLOC(Xk%FGibz%pt,(Xk%FGibz%N,3))
   YAMBO_ALLOC(Xk%weights_FineGd,(Xk%FGibz%N))
   YAMBO_ALLOC(Xk%FGbz%pt,(Xk%FGbz%N,3))
   YAMBO_ALLOC(Xk%sstar_FineGd,(Xk%FGbz%N))
 endif
 !
 call io_bulk(ID, VAR="KPTS_IBZ", VAR_SZ=shape(Xk%FGibz%pt) ) 
 call io_bulk(ID, R2=Xk%FGibz%pt )
 !
 call io_bulk(ID, VAR="KPTS_WEIGHTS", VAR_SZ=shape(Xk%weights_FineGd) ) 
 call io_bulk(ID, R1=Xk%weights_FineGd )
 !
 call io_bulk(ID, VAR="KPTS_BZ", VAR_SZ=shape(Xk%FGbz%pt) ) 
 call io_bulk(ID, R2=Xk%FGbz%pt )
 !
 call io_bulk(ID, VAR="IBZ_TO_BZ_MAP", VAR_SZ=(/Xk%FGbz%N/) )
 call io_bulk(ID, I1=Xk%sstar_FineGd )
 !
 ! Block filling
 !
 if (read_is_on(ID)) then
   YAMBO_ALLOC(Xk%FGibz%N_mapped,(Xk%nibz))
   YAMBO_ALLOC(Xk%FGbz%N_mapped,(Xk%nbz))
 endif
 !
 call io_bulk(ID, VAR="BLOCK_FILLING_IBZ", VAR_SZ=(/Xk%nibz/) ) 
 call io_bulk(ID, I1=Xk%FGibz%N_mapped )
 !
 call io_bulk(ID, VAR="BLOCK_FILLING_BZ", VAR_SZ=(/Xk%nbz/) ) 
 call io_bulk(ID, I1=Xk%FGbz%N_mapped )
 !
 ! Block table
 !
 if (read_is_on(ID)) then
   YAMBO_ALLOC(Xk%FGibz%map,(2,Xk%nibz))
   YAMBO_ALLOC(Xk%FGbz%map,(2,Xk%nbz))
 endif
 !
 call io_bulk(ID, VAR="BLOCK_TABLE_IBZ", VAR_SZ=shape(Xk%FGibz%map) ) 
 call io_bulk(ID, I2=Xk%FGibz%map )
 !
 call io_bulk(ID, VAR="BLOCK_TABLE_BZ", VAR_SZ=shape(Xk%FGbz%map) ) 
 call io_bulk(ID, I2=Xk%FGbz%map )
 !
1 call io_disconnect(ID=ID)
 !
 call timing('io_Double_Grid',OPR='stop')
 !
end function
