! 
!        Copyright (C) 2000-2017 the YAMBO team
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
subroutine el_density_of_states(Xk,Xen,dos_E,dos_broadening,dos_bands,dos_value,l_DbGd,WF_fac)
 !
 use pars,           ONLY:SP
 use electrons,      ONLY:levels
 use R_lattice,      ONLY:bz_samp,nkbz
 use D_lattice,      ONLY:DL_vol
 use functions,      ONLY:Fermi_fnc_derivative
 use parser_m,       ONLY:parser
 use electrons,      ONLY:spin,n_sp_pol,n_spinor,n_spin,spin_occ, &
&                         BZ_DbGd_nkpt,BZ_DbGd_table
 !
 implicit none
 !
 type(bz_samp), intent(in) ::Xk
 type(levels),  intent(in) ::Xen
 !
 real(SP),   intent(in)  :: dos_E,dos_broadening
 integer,    intent(in)  :: dos_bands(2)
 real(SP),   intent(out) :: dos_value(n_spin)
 logical,    intent(in)  :: l_DbGd
 !
 real(SP), optional, intent(in) :: WF_fac(dos_bands(1):dos_bands(2),Xk%nibz,2)
 !
 ! Work space
 !
 integer   :: i_sp_pol,ik_bz,ik,ik_rand,i1,ib
 real(SP)  :: el_dos_tmp(n_sp_pol),f_occ,dos_norm
 logical   :: ik_ibz_done(Xk%nibz),l_prt_occ_only
 !
 call parser('PrtOccOnly',l_prt_occ_only)
 !
 dos_value=0._SP
 !
 do i_sp_pol=1,n_sp_pol
   !
   ik_ibz_done=.false.
   !
   do ik_bz=1,nkbz
     !
     ik=Xk%sstar(ik_bz,1)
     if (.not.l_DbGd) then
       if(ik_ibz_done(ik)) cycle
       ik_ibz_done(ik)=.true.
     endif
     !
     do ib=dos_bands(1),dos_bands(2)
       !
       el_dos_tmp=0._SP
       f_occ=spin_occ
       !
       if (l_DbGd) then
         !
         do i1=1,BZ_DbGd_nkpt(ik_bz)
           ik_rand=BZ_DbGd_table(ik_bz,i1,1)
           if ( abs(Xen%E_DbGd(ib,ik_rand,i_sp_pol)-dos_E)>20._SP*dos_broadening ) cycle
           if (l_prt_occ_only) f_occ=Xen%f_DbGd(ib,ik_rand,i_sp_pol)
           el_dos_tmp(i_sp_pol)=el_dos_tmp(i_sp_pol)+f_occ*&
&                      Fermi_fnc_derivative(Xen%E_DbGd(ib,ik_rand,i_sp_pol)-dos_E,dos_broadening)
         enddo
         ! 
       else
         !
         if ( abs(Xen%E(ib,ik,i_sp_pol)-dos_E)>20._SP*dos_broadening ) cycle
         if (l_prt_occ_only ) f_occ=Xen%f(ib,ik,i_sp_pol)
         el_dos_tmp(i_sp_pol)=el_dos_tmp(i_sp_pol)+f_occ*Xk%weights(ik)*&
&                      Fermi_fnc_derivative(Xen%E(ib,ik,i_sp_pol)          -dos_E,dos_broadening)

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
 if (l_DbGd) dos_norm=dos_norm/real(sum(BZ_DbGd_nkpt))
 !
 dos_value=dos_value*dos_norm
 !
end subroutine el_density_of_states
