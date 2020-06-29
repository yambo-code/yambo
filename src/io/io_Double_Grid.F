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
 if (write_is_on(ID).or.variable_exist(ID,"HEAD_R_LATT")) then
   io_Double_Grid=io_header(ID,R_LATT=.TRUE.,IMPOSE=.TRUE.)
 else
   io_Double_Grid=io_header(ID)
 endif
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
 call io_elemental(ID,I0=Xk%nibz_FineGd,     VAR=' Number of points  in the IBZ (k)')
 call io_elemental(ID,I0=Xk%nbz_FineGd,      VAR=' Number of points  in the  BZ (k)')
 call io_elemental(ID,I0=Xk%nibz_duplicate,  VAR=' Number of duplic. pts in the IBZ')
 call io_elemental(ID,I0=Xk%nbz_duplicate,   VAR=' Number of duplic. pts in the  BZ')
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
   YAMBO_ALLOC(Xk%pt_FineGd,(Xk%nibz_FineGd,3))
   YAMBO_ALLOC(Xk%weights_FineGd,(Xk%nibz_FineGd))
   YAMBO_ALLOC(Xk%ptbz_FineGd,(Xk%nbz_FineGd,3))
   YAMBO_ALLOC(Xk%sstar_FineGd,(Xk%nbz_FineGd))
 endif
 !
 call io_bulk(ID, VAR="KPTS_IBZ", VAR_SZ=shape(Xk%pt_FineGd) ) 
 call io_bulk(ID, R2=Xk%pt_FineGd )
 !
 call io_bulk(ID, VAR="KPTS_WEIGHTS", VAR_SZ=shape(Xk%weights_FineGd) ) 
 call io_bulk(ID, R1=Xk%weights_FineGd )
 !
 call io_bulk(ID, VAR="KPTS_BZ", VAR_SZ=shape(Xk%ptbz_FineGd) ) 
 call io_bulk(ID, R2=Xk%ptbz_FineGd )
 !
 call io_bulk(ID, VAR="IBZ_TO_BZ_MAP", VAR_SZ=(/Xk%nbz_FineGd/) )
 call io_bulk(ID, I1=Xk%sstar_FineGd )
 !
 ! Block filling
 !
 if (read_is_on(ID)) then
   YAMBO_ALLOC(Xk%nibz_mapped,(Xk%nibz))
   YAMBO_ALLOC(Xk%nbz_mapped,(Xk%nbz))
 endif
 !
 call io_bulk(ID, VAR="BLOCK_FILLING_IBZ", VAR_SZ=(/Xk%nibz/) ) 
 call io_bulk(ID, I1=Xk%nibz_mapped )
 !
 call io_bulk(ID, VAR="BLOCK_FILLING_BZ", VAR_SZ=(/Xk%nbz/) ) 
 call io_bulk(ID, I1=Xk%nbz_mapped )
 !
 ! Block table
 !
 if (read_is_on(ID)) then
   YAMBO_ALLOC(Xk%ibz_DbGd_map,(2,Xk%nibz))
   YAMBO_ALLOC(Xk%bz_DbGd_map,(2,Xk%nbz))
 endif
 !
 call io_bulk(ID, VAR="BLOCK_TABLE_IBZ", VAR_SZ=shape(Xk%ibz_DbGd_map) ) 
 call io_bulk(ID, I2=Xk%ibz_DbGd_map )
 !
 call io_bulk(ID, VAR="BLOCK_TABLE_BZ", VAR_SZ=shape(Xk%bz_DbGd_map) ) 
 call io_bulk(ID, I2=Xk%bz_DbGd_map )
 !
 ! Double grid weights
 !
 if (read_is_on(ID)) then
   YAMBO_ALLOC(Xk%weights_DbGd,(Xk%nibz))
 endif
 !
 call io_bulk(ID, VAR="KPTS_WEIGHT", VAR_SZ=(/Xk%nibz/) ) 
 call io_bulk(ID, R1=Xk%weights_DbGd)
 !
1 call io_disconnect(ID=ID)
 !
 call timing('io_Double_Grid',OPR='stop')
 !
end function
