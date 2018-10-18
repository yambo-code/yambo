! 
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, DS
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
subroutine electrons_dos_project( E , dos_E , el_dos )
 !
 ! SLICED structure
 ! 
 ! Y
 ! |   *
 ! |   *
 ! |   *
 ! |   *
 ! |   *
 ! |   *
 ! O---*-----------------X
 !
 ! The sliced dos is calcualated by integrating along **** line of the BZ.
 !
 use pars,           ONLY:SP,schlen
 use vec_operate,    ONLY:c2a,v_norm
 use units,          ONLY:HA2EV
 use com,            ONLY:msg,of_open_close
 use R_lattice,      ONLY:bz_samp,bz_samp_reset
 use LIVE_t,         ONLY:live_timing
 use YPP,            ONLY:INTERP_grid,DOS_bands,DOS_E_steps,electrons_dos_elemental,output_fname
 use interpolate,    ONLY:GRID_k
 use electrons,      ONLY:levels,E_reset,n_sp_pol,n_spin
 !
 implicit none
 !
 type(levels)  :: E
 real(SP)      :: el_dos(DOS_E_steps,n_spin,2),dos_E(DOS_E_steps)
 character(schlen)  :: titles(5)
 !
 ! Work Space
 !
 integer       :: ik,i_E,Nk_slice,is
 real(SP)      :: O(3),k(3),kOdist(INTERP_grid(1)+1)
 type(bz_samp) :: SLICE_k
 type(levels)  :: SLICE_E
 !
 call section('=','DOS projection')    
 !
 call bz_samp_reset(SLICE_k)
 call E_reset(SLICE_E)
 !
 Nk_slice=INTERP_grid(1)+1
 !
 allocate(SLICE_k%weights(Nk_slice))
 allocate(SLICE_k%sstar(Nk_slice,2))
 allocate(SLICE_E%E(DOS_bands(1):DOS_bands(2),Nk_slice,n_sp_pol))
 allocate(SLICE_E%f(DOS_bands(1):DOS_bands(2),Nk_slice,n_sp_pol))
 SLICE_k%nibz=Nk_slice
 SLICE_k%nbz =Nk_slice
 SLICE_k%weights=1./float(Nk_slice)
 !
 do ik=1,Nk_slice
   SLICE_k%sstar(ik,1)=ik
 enddo
 !
 call live_timing('Projected DOS',Nk_slice)
 !
 O=GRID_k%pt(1,:)
 call c2a(v_in=O,mode='ki2c')
 do ik=1,Nk_slice
   k=GRID_k%pt(Nk_slice*(ik-1)+1,:)
   call c2a(v_in=k,mode='ki2c')
   kOdist(ik)=v_norm(k-O)
 enddo
 !
 output_fname='el_projected_dos'
 !
 call of_open_close(trim(output_fname),'ot')
 titles(1)='O-X [a.u.]'
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
!   !
!   ! ...data
!   !
!   do i_E=1,DOS_E_steps
!   enddo
!   !
 !
 do ik=1,Nk_slice
   !
   SLICE_E%E=E%E(DOS_bands(1):DOS_bands(2),Nk_slice*(ik-1)+1:Nk_slice*ik,:)
   SLICE_E%f=E%f(DOS_bands(1):DOS_bands(2),Nk_slice*(ik-1)+1:Nk_slice*ik,:)
   !
   !write (*,*) GRID_k%pt(Nk_slice*(ik-1)+1,:),"-",GRID_k%pt(Nk_slice*ik,:)
   !
   call electrons_dos_elemental(SLICE_k,SLICE_E,DOS_bands,el_dos(:,:,1),dos_E,USE_the_DbGd=.FALSE.,QUIET=.TRUE.)
   !
   do i_E=1,DOS_E_steps
     !
     if (n_spin==1) call msg('o dos','',(/kOdist(ik),dos_E(i_E)*HA2EV,el_dos(i_E,1,1)/),INDENT=-2,USE_TABS=.true.)
     if (n_spin==2) call msg('o dos','',(/kOdist(ik),dos_E(i_E)*HA2EV,el_dos(i_E,:,1),  &
&                                         el_dos(i_E,1,1)+el_dos(i_E,2,2)/),INDENT=-2,USE_TABS=.true.)
     !
   enddo
   !
   write (100,*) 
   !
   call live_timing(steps=1)
   !
 enddo
 !
 call live_timing()
 !
 ! Close & Clean
 !
 call of_open_close(trim(output_fname))
 call bz_samp_reset(SLICE_k)
 call E_reset(SLICE_E)
 !
end subroutine
