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
subroutine el_density_of_states(Xk,Xen,dos_E,DOS_broadening,DOS_bands,dos_value,&
&                               USE_the_DbGd,USE_Eo,WF_fac,USE_occupations)
 !
 use pars,           ONLY:SP
 use electrons,      ONLY:levels
 use R_lattice,      ONLY:bz_samp
 use D_lattice,      ONLY:DL_vol
 use functions,      ONLY:Fermi_fnc_derivative
 use electrons,      ONLY:spin,n_sp_pol,n_spinor,n_spin,spin_occ, &
&                         BZ_DbGd_nkpt,BZ_DbGd_table
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
 real(SP), optional, intent(in) :: WF_fac(DOS_bands(1):DOS_bands(2),Xk%nibz,2)
 !
 ! Work space
 !
 integer   :: i_sp_pol,ik_bz,ik,ik_rand,i1,ib
 real(SP)  :: el_dos_tmp(n_sp_pol),f_occ,dos_norm,E
 logical   :: ik_ibz_done(Xk%nibz),USE_Eo_,USE_the_DbGd_,USE_occ
 !
 dos_value=0._SP
 !
 USE_Eo_=.FALSE.
 if (present(USE_Eo)) USE_Eo_=USE_Eo
 !
 USE_occ=.FALSE.
 if (present(USE_occupations)) USE_occ=USE_occupations
 !
 USE_the_DbGd_=.FALSE.
 if (present(USE_the_DbGd)) USE_the_DbGd_=USE_the_DbGd
 !
 do i_sp_pol=1,n_sp_pol
   !
   ik_ibz_done=.false.
   !
   do ik_bz=1,Xk%nbz
     !
     ik=Xk%sstar(ik_bz,1)
     if (.not.USE_the_DbGd_) then
       if (ik_ibz_done(ik)) cycle
       ik_ibz_done(ik)=.true.
     endif
     !
     do ib=DOS_bands(1),DOS_bands(2)
       !
       el_dos_tmp=0._SP
       f_occ=spin_occ
       !
       if (USE_the_DbGd_) then
         !
         do i1=1,BZ_DbGd_nkpt(ik_bz)
           ik_rand=BZ_DbGd_table(ik_bz,i1,1)
           if (USE_occ) f_occ=Xen%f_DbGd(ib,ik_rand,i_sp_pol)
           el_dos_tmp(i_sp_pol)=el_dos_tmp(i_sp_pol)+f_occ*&
&                      Fermi_fnc_derivative(Xen%E_DbGd(ib,ik_rand,i_sp_pol)-dos_E,DOS_broadening)
         enddo
         ! 
       else
         !
         E=Xen%E(ib,ik,i_sp_pol)
         if (USE_Eo_.and.allocated(Xen%Eo)) then
           E=Xen%Eo(ib,ik,i_sp_pol)
         endif
         !
         if (USE_occ ) f_occ=Xen%f(ib,ik,i_sp_pol)
         el_dos_tmp(i_sp_pol)=el_dos_tmp(i_sp_pol)+f_occ*Xk%weights(ik)*Fermi_fnc_derivative(E-dos_E,DOS_broadening)
         !
       endif
       !
       ! Project on SOC coefficients
       !
       if(n_spinor==2) then
         if(present(WF_fac)) then
           dos_value(1)=dos_value(1)+el_dos_tmp(1)*WF_fac(ib,ik,1)
           dos_value(2)=dos_value(2)+el_dos_tmp(1)*WF_fac(ib,ik,2)
         else
           dos_value(:)=dos_value(:)+el_dos_tmp(1)*0.5_SP
         endif
       else
         dos_value=dos_value+el_dos_tmp
       endif
     enddo  ! ib
     !
   enddo    ! ik
 enddo      ! i_sp_pol
 !
 dos_norm=1._SP/DL_vol
 if (USE_the_DbGd_) dos_norm=dos_norm/real(sum(BZ_DbGd_nkpt))
 !
 dos_value=dos_value*dos_norm
 !
end subroutine el_density_of_states
