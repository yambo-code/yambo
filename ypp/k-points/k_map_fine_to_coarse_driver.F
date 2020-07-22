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
 use pars,                ONLY:SP,schlen
 use stderr,              ONLY:intc,STRING_same
 use R_lattice,           ONLY:bz_samp,nkibz,bz_samp_reset,BZ_FineGd_mode,bz_samp_FineGd_reset
 use com,                 ONLY:msg
 use YPPm,                ONLY:n_FineGd_DBs
 use electrons,           ONLY:levels,FineGd_E_components_reset
 use IO_m,                ONLY:io_control,REP,OP_WR_CL
 !
#include<memory.h>
 !
 type(bz_samp) :: Xk
 type(levels)  :: Xen
 !
 ! Work Space
 !
 type(bz_samp)        :: FineGd_k
 character(schlen)    :: FG_zone(2)
 integer              :: i_err,ID,i_fg
 integer, external    :: io_Double_Grid
 logical              :: l_mix,l_expand,l_unexpand
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
 l_mix     =STRING_same(BZ_FineGd_mode,"mixed")
 l_expand  =STRING_same(BZ_FineGd_mode,"expanded")
 l_unexpand=STRING_same(BZ_FineGd_mode,"unexpanded")
 !
 ! Initial checks
 !================
 call section('+','Double Grid initialization')
 call k_map_fine_to_coarse_init(Xk,Xen,FineGd_k)
 !
 ! Expand the Coarse Grid 
 !========================
 call k_build_up_BZ_tables(Xk)
 call k_ibz2bz(Xk,'i',.true.)
 !
 ! FineGd E to IBZ map
 !=======================
 call section('=','Double Grid Energies-IBZ mapping and Fine Grid re-ordering')
 call k_map_fine_to_coarse('Energies-IBZ',Xk,Xk%FGibz,FineGd_k,Xen)
 !
 ! Expand the Fine Grid 
 !========================
 !
 if (l_mix.or.l_expand) then
   !
   ! k_DbGd (IBZ->BZ)
   !
   call section('=','Double Grid K-points expansion ...')
   call k_ibz2bz(FineGd_k,'i',.true.)
   call msg('l','found '//trim(intc(FineGd_k%nbz))//' ('//trim(intc(FineGd_k%nibz))//') points in the BZ (IBZ)')
   call k_expand(FineGd_k)
   !
   FG_zone(1)="IBZ"
   FG_zone(2)="BZ"
   !
 endif
 !
 if (l_expand) then
   !
   ! BZ->IBZ
   !
   YAMBO_FREE(FineGd_k%pt)
   YAMBO_ALLOC(FineGd_k%pt,(FineGd_k%nibz,3))
   FineGd_k%nibz=FineGd_k%nbz
   FineGd_k%pt=FineGd_k%ptbz
   !
   FG_zone="BZ"
   !
 endif
 !
 if (l_unexpand) then
   !
   ! IBZ->BZ
   !
   FineGd_k%nbz=FineGd_k%nibz
   YAMBO_ALLOC(FineGd_k%ptbz,(FineGd_k%nbz,3))
   YAMBO_ALLOC(FineGd_k%weights,(FineGd_k%nibz))
   YAMBO_ALLOC(FineGd_k%sstar,(FineGd_k%nibz,2))
   FineGd_k%ptbz=FineGd_k%pt
   FineGd_k%weights=1./real(FineGd_k%nibz,SP)
   do i_fg=1,FineGd_k%nibz
     FineGd_k%sstar(i_fg,:)=(/i_fg,1/)
   enddo
   !
   FG_zone="IBZ"
   !
 endif
 !
 ! FG_zone(1) to IBZ map
 !=======================
 call section('=','Double Grid '//trim(FG_zone(1))//'-IBZ mapping')
 call k_map_fine_to_coarse(trim(FG_zone(1))//'-IBZ',Xk,Xk%FGibz,FineGd_k,Xen)
 !
 ! FG_zone(2) to BZ map
 !======================
 call section('=','Double Grid '//trim(FG_zone(2))//'-BZ mapping')
 call k_map_fine_to_coarse(trim(FG_zone(2))//'-BZ',Xk,Xk%FGbz,FineGd_k,Xen)
 !
 ! Final I/O
 !===========
 ! .. restoring of built-in value of nkibz to be used in io_header 
 nkibz=Xk%nibz
 !
 call io_control(ACTION=OP_WR_CL,SEC=(/1/),COM=REP,ID=ID)
 i_err=io_Double_Grid(Xen,Xk,ID)
 !
 ! CLEAN
 !========
 !
 call bz_samp_reset(FineGd_k)
 call k_ibz2bz(Xk,'d',.true.)
 call FineGd_E_components_reset(Xen,.TRUE.)
 call bz_samp_FineGd_reset(Xk%FGbz)
 call bz_samp_FineGd_reset(Xk%FGibz)
 !
end subroutine
