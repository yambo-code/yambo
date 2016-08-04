!
!        Copyright (C) 2000-2016 the YAMBO team
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
 use pars,           ONLY:SP,IP
 use electrons,      ONLY:levels,n_sp_pol
 use R_lattice,      ONLY:nXkbz,bz_samp
 use memory_m,       ONLY:mem_est
 use IO_m,           ONLY:io_connect,io_disconnect,io_elemental,read_is_on,&
&                         io_bulk,io_header,ver_is_gt_or_eq,file_is_present,write_is_on,&
&                         IO_OUTDATED_DB
 implicit none
 !
 type(levels) :: E
 type(bz_samp):: Xk
 integer      :: ID 
 !
 ! Work Space
 !
 integer :: DG_format
 !
 io_Double_Grid=-1
 !
 io_Double_Grid=io_connect(desc='Double_Grid',type=2,ID=ID)
 !
 if (io_Double_Grid/=0) goto 1
 !
 io_Double_Grid=io_header(ID)
 !
 if (.not.ver_is_gt_or_eq(ID,revision=3560)) io_Double_Grid=IO_OUTDATED_DB
 !
 if (io_Double_Grid/=0) goto 1
 !
 call io_elemental(ID,VAR="PARS",VAR_SZ=5,MENU=1)
 !
 call io_elemental(ID,I0=E%nb_DbGd,        VAR=' Bands                           :')
 call io_elemental(ID,I0=E%nk_DbGd,        VAR=' Number of points  in the IBZ (E):')
 call io_elemental(ID,I0=Xk%nibz_DbGd,     VAR=' Number of points  in the IBZ (k):')
 call io_elemental(ID,I0=Xk%nbz_DbGd,      VAR=' Number of points  in the  BZ (k):')
 ! 
 call io_elemental(ID,VAR="",VAR_SZ=0,MENU=1)
 !
 !
 ! Eigenvalues
 !
 if (read_is_on(ID)) then
   allocate(E%E_DbGd(E%nb_DbGd,E%nk_DbGd,n_sp_pol))
   allocate(E%W_DbGd(E%nb_DbGd,E%nk_DbGd,n_sp_pol))
   allocate(E%f_DbGd(E%nb_DbGd,E%nk_DbGd,n_sp_pol))
 endif
 !
 call io_bulk(ID, VAR="EIGENVALUES", VAR_SZ=shape(E%E_DbGd) ) 
 call io_bulk(ID, R3=E%E_DbGd )
 !
 !
 ! Kpts
 !
 if (read_is_on(ID)) then
   allocate(Xk%pt_DbGd(Xk%nibz_DbGd,3))
   allocate(Xk%ptbz_DbGd(Xk%nbz_DbGd,3))
   allocate(Xk%bz_to_ibz_DbGd(Xk%nbz_DbGd))
 endif
 !
 call io_bulk(ID, VAR="KPTS_IBZ", VAR_SZ=shape(Xk%pt_DbGd) ) 
 call io_bulk(ID, R2=Xk%pt_DbGd )
 !
 call io_bulk(ID, VAR="KPTS_BZ", VAR_SZ=shape(Xk%ptbz_DbGd) ) 
 call io_bulk(ID, R2=Xk%ptbz_DbGd )
 !
 call io_bulk(ID, VAR="IBZ_TO_BZ_MAP", VAR_SZ=(/Xk%nbz_DbGd/) )
 call io_bulk(ID, I1=Xk%bz_to_ibz_DbGd )
 !
 !
 ! Block filling
 !
 if (read_is_on(ID)) allocate(Xk%nibz_mapped(Xk%nibz),Xk%nbz_mapped(Xk%nbz))
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
   allocate(Xk%ibz_DbGd_map(Xk%nibz,maxval(Xk%nibz_mapped)))
   allocate(Xk%bz_DbGd_map(Xk%nbz,maxval(Xk%nbz_mapped)))
   Xk%ibz_DbGd_map=0
   Xk%bz_DbGd_map=0
 endif
 !
 call io_bulk(ID, VAR="BLOCK_TABLE (IBZ)", VAR_SZ=shape(Xk%ibz_DbGd_map) ) 
 call io_bulk(ID, I2=Xk%ibz_DbGd_map )
 !
 call io_bulk(ID, VAR="BLOCK_TABLE ( BZ)", VAR_SZ=shape(Xk%bz_DbGd_map) ) 
 call io_bulk(ID, I2=Xk%bz_DbGd_map )
 !
 ! Double grid weights
 !
 if (read_is_on(ID)) allocate(Xk%weights_DbGd(Xk%nibz))
 !
 call io_bulk(ID, VAR="KPTS_WEIGHT", VAR_SZ=(/Xk%nibz/) ) 
 call io_bulk(ID, R1=Xk%weights_DbGd)
 !
 ! REMOVED INTERPOLATION WEIGHTS (to check)
 !
1 call io_disconnect(ID=ID)
 !
end function
