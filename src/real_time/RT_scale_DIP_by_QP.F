!
! Copyright (C) 2000-2011 M. Gruening and the YAMBO team 
!              http://www.yambo-code.org
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
subroutine RT_scale_DIP_by_QP(Xen)
!
 use pars,           ONLY:SP
 use X_m,            ONLY:DIP_iR
 use electrons,      ONLY:levels
 use R_lattice,      ONLY:nkibz
 use SC,             ONLY:SC_bands
 use com,            ONLY:msg
!
 type(levels),  intent(in)    :: Xen
 !
 real(SP)                     :: Ev_m_Ec,Eov_m_Eoc
!
 if (.not.associated(Xen%Eo)) return
 call msg('snr','[QP] Rescaling of the dipole by LDA/QP')
  do ik=1,nkibz
    do iv=1,Xen%nbm
     do ic=Xen%nbf+1,SC_bands(2)
       Ev_m_Ec=Xen%E(iv,ik,1)-Xen%E(ic,ik,1)
       Eov_m_Eoc=Xen%Eo(iv,ik,1)-Xen%Eo(ic,ik,1)
       DIP_iR(:,ic,iv,ik,1) = DIP_iR(:,ic,iv,ik,1)*Eov_m_Eoc/Ev_m_Ec 
     end do
   end do
 end do
! 
end subroutine RT_scale_DIP_by_QP
