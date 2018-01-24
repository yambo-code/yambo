!
!        Copyright (C) 2000-2018 the YAMBO team
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
subroutine ELPH_format_converter(E,Xk,q)
 !
 use pars,                ONLY:SP,schlen
 use electrons,           ONLY:levels
 use ELPH,                ONLY:elph_global_free,elph_global_alloc,&
&                              elph_gkkp,elph_DW,elph_nDBs
 use ELPH_old,            ONLY:elph_global_free_old,elph_global_alloc_old,&
&                              elph_gkkp_old,elph_DW_old
 use com,                 ONLY:msg,warning,file_exists,error,core_io_path,more_io_path
 use R_lattice,           ONLY:bz_samp
 use IO_m,                ONLY:io_control,OP_APP_WR_CL,OP_WR_CL,OP_RD_CL,DUMP,IO_and_Messaging_switch
 use LIVE_t,              ONLY:live_timing
 !
 implicit none
 !
 type(bz_samp) :: Xk,q
 type(levels)  :: E
 !
 !Work Space...
 !
 !... I/O 
 !
 integer            ::idb,iq,ik,io_elph_err,ID_RD,ID_WR
 integer, external  ::io_ELPH,io_ELPH_old
 character(schlen)  ::db_name,core_io_path_save
 !
 core_io_path_save=core_io_path
 !
 call k_ibz2bz(Xk,'i',.false.)
 !
 elph_nDBs=q%nibz
 db_name='gkkp'
 !
 call io_control(ACTION=OP_RD_CL,SEC=(/1/),mode=DUMP,ID=ID_RD)
 io_elph_err=io_ELPH_old(ID_RD,trim(db_name))
 !
 if (io_elph_err/=0) then
   !
   elph_nDBs=q%nbz
   db_name='gkkp_expanded'
   !
   call io_control(ACTION=OP_RD_CL,SEC=(/1/),mode=DUMP,ID=ID_RD)
   io_elph_err=io_ELPH_old(ID_RD,trim(db_name))
   !
 endif
 !
 if (io_elph_err/=0) return
 !
 call section("*","== Electron-Phonon DBs convertion ==")
 !
 core_io_path=more_io_path
 !
 call io_control(ACTION=OP_WR_CL,SEC=(/1/),ID=ID_WR)
 io_elph_err=io_ELPH(ID_WR,trim(db_name))
 !
 ! Allocation...
 !
 call elph_global_alloc('gkkp')
 call elph_global_alloc_old('gkkp')
 !
 ! Force DB fragmentation
 !
 call IO_and_Messaging_switch("+DB_is_fragmented")
 !
 call live_timing('Conversion',elph_nDBs)
 !
 do idb=1,elph_nDBs
   !
   iq=idb
   !
   core_io_path=core_io_path_save
   !
   call io_control(ACTION=OP_RD_CL,SEC=(/iq+1/),ID=ID_RD)
   io_elph_err=io_ELPH_old(ID_RD,trim(db_name))
   !
   do ik=1,Xk%nbz
     elph_gkkp(:,:,:,ik)=elph_gkkp_old(ik,:,:,:)
     elph_DW(:,:,:,ik)  =elph_DW_old(ik,:,:,:)
   enddo
   !
   core_io_path=more_io_path
   !
   call io_control(ACTION=OP_APP_WR_CL,SEC=(/iq+1/),ID=ID_WR)
   io_elph_err=io_ELPH(ID_WR,trim(db_name))
   !
   call live_timing(steps=1)
   !
 enddo
 !
 call live_timing()
 !
 call elph_global_free()
 call elph_global_free_old()
 !
 call IO_and_Messaging_switch("-DB_is_fragmented")
 !
end subroutine
