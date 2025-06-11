!
!        Copyright (C) 2000-2022 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS AM
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
subroutine NL_carriers_number(en,k,Ncarr)
 !
 use pars,           ONLY:SP
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels,n_sp_pol
 use nl_optics,      ONLY:NL_bands
 use real_time,      ONLY:dG_lesser
 !
 implicit none
 !
 type(levels),  intent(in) :: en
 type(bz_samp), intent(in) :: k
 real(SP),      intent(out):: Ncarr(3)
 !
 ! Workspace
 !
 integer     :: ib,ik,i_sp_pol
 !
 ! Electrons and holes number
 !
 Ncarr = 0._SP
 !
 do i_sp_pol=1,n_sp_pol
 do ik=1,en%nk
   do ib=NL_bands(1),NL_bands(2)
     if ( en%E(ib,ik,i_sp_pol)<=0._SP ) Ncarr(2)=Ncarr(2)-aimag(dG_lesser(ib,ib,ik,i_sp_pol,1))*k%weights(ik)
     if ( en%E(ib,ik,i_sp_pol)> 0._SP ) Ncarr(3)=Ncarr(3)+aimag(dG_lesser(ib,ib,ik,i_sp_pol,1))*k%weights(ik)
   enddo
 enddo
 enddo
 !
 Ncarr(1)=Ncarr(2)-Ncarr(3)
 !
end subroutine NL_carriers_number
