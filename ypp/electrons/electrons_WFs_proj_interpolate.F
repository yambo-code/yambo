! 
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): DS
!
subroutine electrons_WFs_proj_interpolate(Xen,SEED_k,OUT_k,BANDS_range,proj_fac,ID_proj)
 !
 use pars,           ONLY:SP,cZERO
 use wave_func,      ONLY:WF
 use electrons,      ONLY:levels
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
