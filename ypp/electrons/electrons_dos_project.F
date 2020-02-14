! 
!        Copyright (C) 2000-2019 the YAMBO team
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
subroutine electrons_dos_project( E , dos_E )
 !
 use pars,           ONLY:SP,schlen,lchlen
 use vec_operate,    ONLY:c2a,v_norm
 use units,          ONLY:HA2EV
 use com,            ONLY:msg,of_open_close
 use R_lattice,      ONLY:bz_samp,bz_samp_reset
 use LIVE_t,         ONLY:live_timing
 use YPP_interfaces, ONLY:electrons_dos_elemental
 use YPPm,           ONLY:DOS_bands,DOS_E_steps,output_fname,PROJECT_grid,&
&                         l_PROJECT_line,CIRCUIT_k_label,PROJECT_threshold
 use interpolate,    ONLY:GRID_k
 use electrons,      ONLY:levels,E_reset,n_sp_pol,n_spin
 use stderr,         ONLY:set_real_printed_length
 !
#include<memory.h>
 !
 type(levels)       :: E
 real(SP)           :: dos_E(DOS_E_steps)
 !
 ! Work Space
 !
 character(schlen)  :: titles(5)
 character(lchlen)  :: of_string
 integer       :: ik,i_E,is,ik_path,dos_unit
 real(SP)      :: O(3),k(3),kdist(GRID_k%nbz),max_dos
 type(bz_samp) :: SLICE_k
 type(levels)  :: SLICE_E
 real(SP),allocatable        :: el_dos(:,:,:)
 integer         ,external   :: find_the_unit
 real(SP),pointer,contiguous :: PDOS_fac_p(:,:,:) => null()
 real(SP),pointer,contiguous :: SPIN_fac_p(:,:,:) => null()
 !
 call bz_samp_reset(SLICE_k)
 call E_reset(SLICE_E)
 !
 ! Allocation
 !============
 YAMBO_ALLOC(el_dos,(GRID_k%nbz/PROJECT_grid(2),DOS_E_steps,n_spin))
 allocate(SLICE_k%weights(PROJECT_grid(2)))
 allocate(SLICE_k%sstar(PROJECT_grid(2),2))
 allocate(SLICE_E%E(DOS_bands(1):DOS_bands(2),PROJECT_grid(2),n_sp_pol))
 allocate(SLICE_E%f(DOS_bands(1):DOS_bands(2),PROJECT_grid(2),n_sp_pol))
 SLICE_k%nibz=PROJECT_grid(2)
 SLICE_k%nbz =PROJECT_grid(2)
 SLICE_k%weights=1./float(PROJECT_grid(2))
 do ik=1,PROJECT_grid(2)
   SLICE_k%sstar(ik,1)=ik
 enddo
 !
 call set_real_printed_length(f_length=10,g_length=10)
 !
 if (l_PROJECT_line) then
   !================
   !
   ! |k| distance along the path
   !-----------------------------
   kdist=0._SP
   O=GRID_k%pt(1,:)
   do ik=2,GRID_k%nbz
     ik_path=k_on_the_path(ik)
     if (ik_path==0) cycle
     k=GRID_k%pt(ik,:)-O
     call c2a(v_in=k,mode='ki2c')
     kdist(ik_path)=kdist(ik_path)+v_norm(k)
     if (len_trim( CIRCUIT_k_label(ik_path))>0) then
       if (ik_path>1) kdist(ik_path+1:)=kdist(ik_path)
       O=GRID_k%pt(ik,:)
     endif
   enddo
   !
   call live_timing('Projecting the DOS',GRID_k%nbz/PROJECT_grid(2))
   !
   do ik=1,GRID_k%nbz
     !
     ik_path=k_on_the_path(ik)
     if (ik_path==0) cycle
     !
     SLICE_E%E=E%E(DOS_bands(1):DOS_bands(2),PROJECT_grid(2)*(ik_path-1)+1:PROJECT_grid(2)*ik_path,:)
     SLICE_E%f=E%f(DOS_bands(1):DOS_bands(2),PROJECT_grid(2)*(ik_path-1)+1:PROJECT_grid(2)*ik_path,:)
     !
     call electrons_dos_elemental(SLICE_k,SLICE_E,DOS_bands,el_dos(ik_path,:,:),dos_E,&
 &                                SPIN_fac_p,PDOS_fac_p,USE_the_DbGd=.FALSE.,QUIET=.TRUE.)
     call live_timing(steps=1)
     !
   enddo
   !
   call live_timing( )
   !
   call live_timing('Printing the projected DOS',GRID_k%nbz/PROJECT_grid(2))
   !
   output_fname='el_dos_projected_LINE'
   !
   call of_open_close(trim(output_fname),'ot')
   titles(1)='|k| (a.u.)'
   !
   if (n_spin==1) then
     titles(2)='E [eV]'
     titles(3)='DOS'
     call msg('o dos','#',titles(:3),INDENT=0,USE_TABS=.true.)    
   else
     titles(2)='DOS [up]'
     titles(3)='DOS [up]'
     titles(4)='DOS [dn]'
     titles(5)='DOS [up+dn]'
     call msg('o dos','#',titles(:5),INDENT=0,USE_TABS=.true.)    
   endif
   call msg('o dos',"#")
   !
   max_dos= maxval( el_dos)
   !
   dos_unit=find_the_unit('dos')
   !
   do ik=1,GRID_k%nbz
     !
     ik_path=k_on_the_path(ik)
     if (ik_path==0) cycle
     !
     do i_E=1,DOS_E_steps
       !
       if (.not.any((/el_dos(ik_path,i_E,:)> max_dos*PROJECT_threshold/100./))) cycle
       !
       if (n_spin==1) then
         call msg('o dos','',(/kdist(ik_path),dos_E(i_E)*HA2EV,el_dos(ik_path,i_E,1)/),INDENT=-2,USE_TABS=.true.,&
&                 formatted_msg=of_string)
       else if (n_spin==2) then
         call msg('o dos','',(/kdist(ik_path),dos_E(i_E)*HA2EV,el_dos(ik_path,i_E,:),  &
&                             el_dos(ik_path,i_E,1)+el_dos(ik_path,i_E,2)/),INDENT=-2,USE_TABS=.true.)
       endif
       if (len_trim(CIRCUIT_k_label(ik_path))>0) of_string=trim(of_string)//"  ["//trim(CIRCUIT_k_label(ik_path))//"]"
       call msg_deliver_fast(dos_unit,of_string)
       !
     enddo
     !
     call live_timing(steps=1)
     !
   enddo
   !
   call live_timing()
   !
 endif
 !
 call set_real_printed_length()
 !
 ! Close & Clean
 !
 call of_open_close(trim(output_fname))
 call bz_samp_reset(SLICE_k)
 call E_reset(SLICE_E)
 YAMBO_FREE(el_dos)
 !
 contains
   !
   integer function k_on_the_path(ik_in)
     integer :: ik_in
     if (mod(ik_in,PROJECT_grid(2))/=1) then
       k_on_the_path=0
       return
     endif
     k_on_the_path=ik_in/PROJECT_grid(2)+1
   end function
   !
end subroutine
