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
 use electrons,      ONLY:levels,FineGd_E_components_alloc
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
   call io_elemental(ID,VAR='MAPPING_mode',CH0="",VAR_SZ=1,MENU=0)
   call io_elemental(ID,CH0=BZ_FineGd_mode,   VAR=' Brillouin Symmetry  mode        ')
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   call io_elemental(ID,VAR='FGbz_mode',CH0="",VAR_SZ=1,MENU=0)
   call io_elemental(ID,CH0=Xk%FGbz%FG_mode,  VAR=' Fine Grid  BZ mapped mode       ')
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   call io_elemental(ID,VAR='FGibz_mode',CH0="",VAR_SZ=1,MENU=0)
   call io_elemental(ID,CH0=Xk%FGibz%FG_mode, VAR=' Fine Grid IBZ mapped mode       ')
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
 endif
 !
 call io_elemental(ID,VAR="PARS",VAR_SZ=5,MENU=0)
 call io_elemental(ID,I0=E%FG%nb,         VAR=' Bands                           ')
 call io_elemental(ID,I0=E%FG%nk,         VAR=' Number of points (fine)         in the IBZ (E)')
 call io_elemental(ID,I0=Xk%FGbare%N)
 call io_elemental(ID,I0=Xk%FGibz%N,          VAR=' Number of points (fine+coarse)  in the IBZ (k)')
 call io_elemental(ID,I0=Xk%FGbz%N,           VAR=' Number                          in the  BZ (k)')
 call io_elemental(ID,VAR="",VAR_SZ=0,MENU=1)
 !
 ! Eigenvalues
 !
 if (read_is_on(ID)) then
   call FineGd_E_components_alloc(E)
   call bz_samp_FineGd_alloc(Xk%FGbare,Xk%nibz)
   call bz_samp_FineGd_alloc(Xk%FGibz,Xk%nibz)
   call bz_samp_FineGd_alloc(Xk%FGbz,Xk%nbz)
 endif
 !
 call io_bulk(ID, VAR="EIGENVALUES", VAR_SZ=shape(E%FG%E) ) 
 call io_bulk(ID, R3=E%FG%E )
 !
 ! K map
 !
 call elemental_IO(Xk%FGbare,"K_MAP","BARE")
 !
 ! KPTs
 !
 call elemental_IO(Xk%FGbare,"KPTS","BARE")
 call elemental_IO(Xk%FGibz,"KPTS","IBZ")
 call elemental_IO(Xk%FGbz,"KPTS","BZ")
 !
 ! Weights
 !
 call elemental_IO(Xk%FGibz,"WEIGHTS","IBZ")
 call elemental_IO(Xk%FGbz,"WEIGHTS","BZ")
 !
 ! E map
 !
 call io_bulk(ID, VAR="IBZ_E_MAP", VAR_SZ=(/Xk%FGibz%N/) )
 call io_bulk(ID, I1=Xk%FGibz%E_map )
 !
 call io_bulk(ID, VAR="BZ_E_MAP", VAR_SZ=(/Xk%FGbz%N/) )
 call io_bulk(ID, I1=Xk%FGbz%E_map )
 !
 ! Block filling
 !
 call elemental_IO(Xk%FGibz,"N_MAPPED","IBZ")
 call elemental_IO(Xk%FGbz,"N_MAPPED","BZ")
 !
 ! K_RANGE
 !
 call elemental_IO(Xk%FGbare,"K_RANGE","BARE")
 call elemental_IO(Xk%FGibz,"K_RANGE","IBZ")
 call elemental_IO(Xk%FGbz,"K_RANGE","BZ")
 !
1 call io_disconnect(ID=ID)
 !
 call timing('io_Double_Grid',OPR='stop')
 !
 contains
   !
   subroutine elemental_IO(FG,what,kind)
     !
     use pars,       ONLY:schlen
     use R_lattice,  ONLY:bz_fine_grid
     !
     character(*)       ::what,kind
     character(schlen)  ::VAR_name
     type(bz_fine_grid) ::FG
     !
     VAR_NAME=kind//"_"//what
     !
     if (what=="N_MAPPED") then
       call io_bulk(ID, VAR=trim(VAR_NAME), VAR_SZ=shape(FG%N_mapped) )
       call io_bulk(ID, I1=FG%N_mapped )
     endif
     if (what=="K_RANGE") then
       call io_bulk(ID, VAR=trim(VAR_NAME), VAR_SZ=shape(FG%k_range) ) 
       call io_bulk(ID, I2=FG%k_range )
     endif
     if (what=="K_MAP") then
       call io_bulk(ID, VAR=trim(VAR_NAME), VAR_SZ=shape(FG%k_map))
       call io_bulk(ID, I1=FG%k_map )
     endif
     if (what=="WEIGHTS") then
       call io_bulk(ID, VAR=trim(VAR_NAME), VAR_SZ=shape(FG%weights))
       call io_bulk(ID, R1=FG%weights )
     endif
     if (what=="KPTS") then
       call io_bulk(ID, VAR=trim(VAR_NAME), VAR_SZ=shape(FG%pt))
       call io_bulk(ID, R2=FG%pt )
     endif
     if (what=="E_MAP") then
       call io_bulk(ID, VAR=trim(VAR_NAME), VAR_SZ=(/FG%N/))
       call io_bulk(ID, I1=FG%E_map )
     endif
   end subroutine
 !
end function
