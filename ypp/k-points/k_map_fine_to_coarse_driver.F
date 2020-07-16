!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM DS
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
subroutine k_map_fine_to_coarse_driver(Xk,Xen)
 !
 use stderr,              ONLY:intc
 use R_lattice,           ONLY:bz_samp,nkibz,bz_samp_reset
 use com,                 ONLY:msg
 use YPPm,                ONLY:n_FineGd_DBs
 use electrons,           ONLY:levels,E_reset
 use IO_m,                ONLY:io_control,LOG,OP_WR_CL
 !
#include<memory.h>
 !
 type(bz_samp) :: Xk
 type(levels)  :: Xen
 !
 ! Work Space
 !
 type(levels)         :: FineGd_E
 type(bz_samp)        :: FineGd_k
 !
 integer, allocatable :: Reordering_map(:)
 !
 integer           :: i_err,ID
 integer, external :: io_Double_Grid
 !
 if (n_FineGd_DBs==0) then
   call msg("s",'DbGd: no database secified. Nothing to do.')
   return
 endif
 !
 if (Xk%FGbz%N>0) then
   call msg("s",'DbGd: database components already allocated. Nothing to do.')
   return
 endif
 !
 call section('*','Double Grid DB(s)')
 !
 ! Initial checks
 !================
 call section('+','Double Grid initialization')
 call k_map_fine_to_coarse_init(Xk,Xen,FineGd_k,FineGd_E)
 !
 YAMBO_ALLOC(Reordering_map,(FineGd_k%nibz))
 !
 ! E Dimensions
 !==============
 Xen%nb_FineGd=FineGd_E%nb
 Xen%nk_FineGd=FineGd_E%nk
 !
 ! Expand to the BZ 
 !==================
 !
 ! Xk (IBZ->BZ)
 !
 call k_build_up_BZ_tables(Xk)
 call k_ibz2bz(Xk,'i',.true.)
 !
 ! k_DbGd (IBZ->BZ)
 !
 call section('=','Double Grid K-points expansion ...')
 call k_ibz2bz(FineGd_k,'i',.true.)
 call msg('l','found '//trim(intc(FineGd_k%nbz))//' ('//trim(intc(FineGd_k%nibz))//') points in the BZ (IBZ)')
 !
 ! First map IBZ to IBZ
 !=======================
 call section('=','Double Grid IBZ mapping')
 call k_map_fine_to_coarse_ibz(Xk,Xen,FineGd_k,FineGd_E,Reordering_map)
 ! 
 ! call k_map_fine_to_coarse(
 !
 ! Then map BZ to BZ
 !===================
 call section('=','Double Grid BZ mapping')
 call k_map_fine_to_coarse_bz(Xk,Xen,FineGd_k,FineGd_E,Reordering_map)
 !
 YAMBO_FREE(Reordering_map)
 !
 ! Final I/O
 !============
 ! .. restoring of built-in value of nkibz to be used in io_header 
 nkibz=Xk%nibz
 !
 call io_control(ACTION=OP_WR_CL,SEC=(/1/),COM=LOG,ID=ID)
 i_err=io_Double_Grid(Xen,Xk,ID)
 !
 ! CLEAN
 !========
 !
 call E_reset(FineGd_E)
 call bz_samp_reset(FineGd_k)
 call k_ibz2bz(Xk,'d',.true.)
 !...Xk/Xen FineGd components
 deallocate(Xen%E_FineGd,Xk%ibz_DbGd_map,Xk%bz_DbGd_map,Xk%weights_DbGd,Xk%nibz_mapped,Xk%nbz_mapped)
 !
end subroutine
