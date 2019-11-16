! 
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS
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
subroutine electrons_WFs_proj_interpolate(Xen,SEED_k,OUT_k,BANDS_range,proj_fac,ID_proj)
 !
 use pars,           ONLY:SP,cZERO
 use wave_func,      ONLY:WF
 use electrons,      ONLY:levels
 use electrons,      ONLY:n_spinor
 use R_lattice,      ONLY:bz_samp
 use interpolate,    ONLY:INTERPOLATION_driver_seed,INTERPOLATION_driver_do
 use SC,             ONLY:load_SC_components,SC_bands
 use hamiltonian,    ONLY:H_rotation,H_rotation_ref
 use IO_m,           ONLY:DUMP,NONE
 use wrapper,        ONLY:M_by_M
 !
 implicit none
 !
 type(levels),  intent(in)    :: Xen
 type(bz_samp), intent(in)    :: SEED_k
 integer      , intent(in)    :: BANDS_range(2)
 integer      , intent(out)   :: ID_proj
 type(bz_samp), intent(inout) :: OUT_k
 !
 ! Work space
 !
 real(SP)     :: proj_fac(BANDS_range(1):BANDS_range(2),SEED_k%nibz,2)
 complex(SP)  :: H_tmp(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2))
 integer      :: ik,ib,ibp
 !
 proj_fac=cZERO
 !
 call load_SC_components('WF',COM_=NONE,MODE_=DUMP)
 !
 do ik=1,SEED_k%nibz
   !
   H_tmp=cZERO
   call M_by_M('c','n',SC_bands(2)-SC_bands(1)+1,H_rotation_ref(:,:,ik,1),H_rotation(:,:,ik,1),H_tmp)
   !
   do ib=BANDS_range(1),BANDS_range(2)
     do ibp=SC_bands(1),SC_bands(2)
       !if(ibp<=4) proj_fac(ib,ik,1)=proj_fac(ib,ik,1)+H_tmp(ib,ibp)*conjg(H_tmp(ib,ibp))
       !if(ibp> 4) proj_fac(ib,ik,2)=proj_fac(ib,ik,2)+H_tmp(ib,ibp)*conjg(H_tmp(ib,ibp))
       if(ibp<=4) proj_fac(ib,ik,1)=proj_fac(ib,ik,1)+H_tmp(ibp,ib)*conjg(H_tmp(ibp,ib))
       if(ibp> 4) proj_fac(ib,ik,2)=proj_fac(ib,ik,2)+H_tmp(ibp,ib)*conjg(H_tmp(ibp,ib))
     enddo
   enddo
   !
 enddo
 !
 call load_SC_components('WF_clean')
 !
 call INTERPOLATION_driver_seed("SCporj",ID_proj,BANDS_range,SEED_k,SEED_R3=proj_fac)
 call INTERPOLATION_driver_do("SCproj",ID_proj,OUT_k)
 !
end subroutine electrons_WFs_proj_interpolate
