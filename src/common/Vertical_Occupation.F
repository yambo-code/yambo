!
!        Copyright (C) 2000-2017 Claudio Attaccalite
!
! Authors (see AUTHORS file for details): CA
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
subroutine Vertical_Occupation(E,K,mode,ik_exc)
 !
 use pars,          ONLY:SP,DP
 use units,         ONLY:HA2EV
 use electrons,     ONLY:levels,nel_cond,nel,E_duplicate,E_reset
 use R_lattice,     ONLY:bz_samp
 use IO_m,          ONLY:IO_and_Messaging_switch
 use com,           ONLY:error,msg
 !
 implicit none
 !
 type(levels) ::E
 type(bz_samp)::K
 integer      ::mode
 integer, intent(in) :: ik_exc
 !
 ! Work Space
 !
 real(SP)     :: E_range=0.026/HA2EV !eV = 300 Kelvin
 real(SP)     :: n_remove,n_add
 integer :: n_bnd_hole,n_bnd_elect
 integer :: ib
 !
 n_bnd_hole =0
 n_bnd_elect=0
 !
 do ib=1,E%nbf
   if(abs(E%E(ib,ik_exc,1)-E%E(E%nbf,ik_exc,1))<E_range)   n_bnd_hole=n_bnd_hole+1
 enddo
 do ib=E%nbf+1,E%nbf+10
   if(abs(E%E(ib,ik_exc,1)-E%E(E%nbf+1,ik_exc,1))<E_range) n_bnd_elect=n_bnd_elect+1
 enddo
 !
 if(n_bnd_elect==10) call error("Vertical Excitation: recompile with more bands in conduction!!")
 !
 n_remove=nel_cond/dble(n_bnd_hole)
 n_add   =nel_cond/dble(n_bnd_elect)
 !
 do ib=1,E%nbf
   if(abs(E%E(ib,ik_exc,1)-E%E(E%nbf,ik_exc,1))<E_range)  E%f(ib,ik_exc,1)=E%f(ib,ik_exc,1)-n_remove 
 enddo
 do ib=E%nbf+1,E%nbf+10
   if(abs(E%E(ib,ik_exc,1)-E%E(E%nbf+1,ik_exc,1))<E_range) E%f(ib,ik_exc,1)=E%f(ib,ik_exc,1)+n_add
 enddo
 !
 E%nbm=E%nbf+n_bnd_elect
 E%nbf=E%nbf-n_bnd_hole
 !
 call REPORT_all_Occupations(E,k,'E')
 !
end subroutine
