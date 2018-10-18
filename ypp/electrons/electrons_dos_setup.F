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
subroutine electrons_dos_setup( E, dos_E , bands )
 !
 use pars,           ONLY:SP
 use YPP,            ONLY:DOS_E_step,DOS_E_range,DOS_E_steps,DOS_broadening,DOS_is_projected,&
&                         INTERP_path,INTERP_grid,n_path_pts,PtsPath,CIRCUIT_k,CIRCUIT_k_label
 use R_lattice,      ONLY:bz_samp_reset
 use D_lattice,      ONLY:lattice
 use electrons,      ONLY:levels
 use interpolate,    ONLY:GRID_k
 use com,            ONLY:msg
 !
 implicit none
 !
 type(levels),  intent(in) ::E
 integer,       intent(in) ::bands(2)
 real(SP),      intent(out)::dos_E(DOS_E_steps)
 !
 ! Work Space
 ! 
 integer               :: i_E,ikx,iky,nk,iX,iY,iO
 real(SP)              :: dk(3)
 !
 ! Energy Range
 !==============
 if (DOS_E_range(1)>DOS_E_range(2)) then
   DOS_E_range(1)=minval(E%E(bands(1):bands(2),:,:))-5._SP*DOS_broadening
   DOS_E_range(2)=maxval(E%E(bands(1):bands(2),:,:))+5._SP*DOS_broadening
 endif
 DOS_E_step=(DOS_E_range(2)-DOS_E_range(1))/DOS_E_steps
 !
 do i_E=1,DOS_E_steps
   dos_E(i_E)=DOS_E_range(1)+real(i_E,SP)*DOS_E_step
 enddo
 !
 ! Projection
 !============
 !
 if ( any((/INTERP_grid<0/)) ) return
 if (len_trim(INTERP_path)==0) return
 !
 call section('+',"Loading special Points for the "//trim(lattice)//" lattice")
 !=============================================================================
 !
 PtsPath=INTERP_path
 n_path_pts=INTERP_grid(1)
 !
 call k_special(.FALSE.)
 !
 DOS_is_projected=CIRCUIT_k%nbz>0
 !
 if (.not.DOS_is_projected) return
 !
 call bz_samp_reset(GRID_k)
 GRID_k%description="INTERP_PLANE"        
 !
 GRID_k%nibz=(n_path_pts+1)*(n_path_pts+1)
 GRID_k%nbz =GRID_k%nibz
 allocate(GRID_k%ptbz(GRID_k%nbz,3)) 
 allocate(GRID_k%pt(GRID_k%nbz,3)) 
 allocate(GRID_k%weights(GRID_k%nbz)) 
 !
 ! GRID_k
 ! 
 ! 2*n_path_pts+1 Y
 !                |   *
 !                |   *
 !                |   *
 !           nk-> 4   *
 !                |   *
 ! n_path_pts+2-> |   *
 !           nk-> 1---*-----------------X     
 !                n_path_pts+1          1
 !
 iX=1
 iY=2*n_path_pts+1
 iO=n_path_pts+1
 !
 nk=0
 !
 do ikx=iO,iX,-1
   do iky=iO,iY
     dk=CIRCUIT_k%pt(iky,:3)-CIRCUIT_k%pt(n_path_pts+1,:3)
     nk=nk+1
     GRID_k%ptbz(nk,:)=CIRCUIT_k%pt(ikx,:3)+dk
   enddo
 enddo
 GRID_k%pt=GRID_k%ptbz
 GRID_k%weights=1./GRID_k%nibz
 !
 call msg('s','[GRID_PLANE] Points              : ',GRID_k%nibz)
 call msg('s','             Origin         - '//trim(CIRCUIT_k_label(iO))//' [iku]: ',CIRCUIT_k%pt(iO,:3))
 call msg('s','             X              - '//trim(CIRCUIT_k_label(iX))//' [iku]: ',CIRCUIT_k%pt(iX,:3))
 call msg('s','             Y (integrated) - '//trim(CIRCUIT_k_label(iY))//' [iku]: ',CIRCUIT_k%pt(iY,:3))
 !
end subroutine
