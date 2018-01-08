!
!        Copyright (C) 2000-2018 the YAMBO team
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
 use pars,                ONLY:SP
 use stderr,              ONLY:intc
 use R_lattice,           ONLY:bz_samp
 use com,                 ONLY:msg
 use YPP,                 ONLY:n_FineGd_DBs
 use electrons,           ONLY:levels
 use IO_m,                ONLY:io_control,LOG,OP_WR_CL
 !
 implicit none
 !
 type(bz_samp) :: Xk
 type(levels)  :: Xen
 !
 ! Work Space
 !
 type(levels)         :: FineGd_E
 type(bz_samp)        :: FineGd_k
 !
 integer           :: i_err,ID
 integer, external :: io_Double_Grid
 !
 if (n_FineGd_DBs==0) then
   call msg("s",'DbGd: no database secified. Nothing to do.')
   return
 endif
 !
 if (Xk%nbz_FineGd>0) then
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
 ! First map IBZ to IBZ
 !=======================
 call section('=','Double Grid IBZ mapping')
 call k_map_fine_to_coarse_ibz(Xk,Xen,FineGd_k,FineGd_E)
 !
 ! Expand to the BZ 
 !==================
 call section('=','Double Grid K-points expansion ...')
 !
 ! Xk (IBZ->BZ)
 !
 call k_build_up_BZ_tables(Xk)
 call k_ibz2bz(Xk,'i',.true.)
 !
 ! k_DbGd (IBZ->BZ)
 !
 call k_ibz2bz(FineGd_k,'i',.true.)
 call msg('l','found '//trim(intc(FineGd_k%nbz))//' ('//trim(intc(FineGd_k%nibz))//') points in the BZ (IBZ)')
 !
 ! Then map BZ to BZ
 !===================
 call section('=','Double Grid BZ mapping')
 call k_map_fine_to_coarse_bz(Xk,Xen,FineGd_k,FineGd_E)
 !
 ! Final I/O
 !============
 call io_control(ACTION=OP_WR_CL,SEC=(/1/),COM=LOG,ID=ID)
 i_err=io_Double_Grid(Xen,Xk,ID)
 !
 ! CLEAN
 !========
 deallocate(FineGd_E%E)
 deallocate(FineGd_k%pt,FineGd_k%ptbz)
 deallocate(Xen%E_FineGd)
 deallocate(Xk%pt,Xk%ptbz,Xk%ibz_DbGd_map,Xk%bz_DbGd_map,Xk%weights_DbGd,Xk%nibz_mapped,Xk%nbz_mapped)
 !
end subroutine
