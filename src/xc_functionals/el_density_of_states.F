! 
!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine el_density_of_states(Xk,Xen,dos_E,DOS_broadening,DOS_bands,dos_value,&
&                               USE_the_DbGd,USE_Eo,USE_occupations,WF_fac,PDOS_fac)
 !
 use pars,           ONLY:SP
 use electrons,      ONLY:levels
 use R_lattice,      ONLY:bz_samp
 use D_lattice,      ONLY:DL_vol
 use functions,      ONLY:Fermi_fnc_derivative
 use electrons,      ONLY:spin,n_sp_pol,n_spinor,n_spin,spin_occ
 !
 implicit none
 !
 type(bz_samp), intent(in) ::Xk
 type(levels),  intent(in) ::Xen
 !
 real(SP),   intent(in)  :: dos_E,DOS_broadening
 integer,    intent(in)  :: DOS_bands(2)
 real(SP),   intent(out) :: dos_value(n_spin)
 logical,  optional, intent(in) :: USE_Eo,USE_the_DbGd,USE_occupations
 real(SP), optional, pointer, intent(in) :: WF_fac(:,:,:)
 real(SP), optional, pointer, intent(in) :: PDOS_fac(:,:,:)
 !
 ! Work space
 !
 integer   :: i_sp_pol,ib,ik_bz,ik,ik_fg,ik_weight
 real(SP)  :: el_dos_tmp(n_sp_pol),weight,f_occ,E,proj_fac(n_spin),DOS_Broadening_
 logical   :: ik_ibz_done(Xk%nibz),USE_Eo_,USE_the_DbGd_,USE_occ,USE_WFs,USE_PDOS
 !
 dos_value=0._SP
 !
 USE_Eo_=.FALSE.
 if (present(USE_Eo))          USE_Eo_=USE_Eo
 !
 USE_occ=.FALSE.
 if (present(USE_occupations)) USE_occ=USE_occupations
 !
 USE_the_DbGd_=.FALSE.
 if (present(USE_the_DbGd))    USE_the_DbGd_=USE_the_DbGd
 !
 USE_WFs=.FALSE.
 if (present(WF_fac))          USE_WFs=associated(WF_fac)
 !
 USE_PDOS=.FALSE.
 if (present(PDOS_fac))        USE_PDOS=associated(PDOS_fac)
 !
 DOS_broadening_=DOS_broadening
 !
 if(.not.allocated(Xen%Eo)) USE_Eo_=.FALSE.
 if (USE_the_DbGd)          USE_Eo_=.FALSE.
 !
 do i_sp_pol=1,n_sp_pol
   !
   ik_ibz_done=.false.
   !
   do ik=1,Xk%nibz
     !
     do ib=DOS_bands(1),DOS_bands(2)
       !
       el_dos_tmp=0._SP
       f_occ=spin_occ
       !
       if (.not.USE_the_DbGd_) weight=Xk%weights(ik)
       if (     USE_the_DbGd_) weight=Xk%weights_DbGd(1,ik)
       !
       if (.not.USE_Eo_) E=Xen%E(ib,ik,i_sp_pol)
       if (     USE_Eo_) E=Xen%Eo(ib,ik,i_sp_pol)
       !
       if ( abs(E-dos_E)>20._SP*DOS_broadening ) cycle
       if (USE_occ) f_occ=Xen%f(ib,ik,i_sp_pol)
       if (allocated(Xen%W)) DOS_broadening_=DOS_broadening+abs(Xen%W(ib,ik,i_sp_pol))
       el_dos_tmp(i_sp_pol)=f_occ*weight*Fermi_fnc_derivative(E-dos_E,DOS_broadening)
       !
       if (USE_the_DbGd_) then
         !
         do ik_fg=Xk%ibz_DbGd_map(1,ik),Xk%ibz_DbGd_map(2,ik)
           ik_weight=ik_fg-Xk%ibz_DbGd_map(1,ik)+2
           weight=Xk%weights_DbGd(ik_weight,ik)
           if (USE_occ) f_occ=Xen%f_FineGd(ib,ik_fg,i_sp_pol)
           if (allocated(Xen%W_FineGd)) DOS_broadening_=DOS_broadening+abs(Xen%W_FineGd(ib,ik_fg,i_sp_pol))
           el_dos_tmp(i_sp_pol)=el_dos_tmp(i_sp_pol)+f_occ*weight*&
           &                             Fermi_fnc_derivative(Xen%E_FineGd(ib,ik_fg,i_sp_pol)-dos_E,DOS_broadening)
         enddo
         !
       endif
       !
       ! Projections
       !
       proj_fac=1._SP
       !
       ! Project on SOC coefficients
       !
       if(n_spinor==2) then
         if(USE_WFs) then
           proj_fac(1)=proj_fac(1)*WF_fac(ib,ik,1)
           proj_fac(2)=proj_fac(2)*WF_fac(ib,ik,2)
         else
           proj_fac   =proj_fac*0.5_SP
         endif
       endif
       !
       ! Project on PSOC coefficients
       !
       if(USE_PDOS) proj_fac=proj_fac*PDOS_fac(ib,ik,i_sp_pol)
       !
       if(n_spinor==1) dos_value(i_sp_pol)=dos_value(i_sp_pol)+el_dos_tmp(i_sp_pol)*proj_fac(i_sp_pol)
       !
       if(n_spinor==2) dos_value          =dos_value          +el_dos_tmp(1)       *proj_fac
       !
     enddo  ! ib
     !
   enddo    ! ik
 enddo      ! i_sp_pol
 !
 dos_value=dos_value/DL_vol
 !
end subroutine el_density_of_states
