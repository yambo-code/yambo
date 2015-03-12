! 
!        Copyright (C) 2000-2014 the YAMBO team
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
subroutine RT_check_RIM(Xk,Xen)
 !
 use electrons,        ONLY:levels,BZ_RIM_tot_Nkpts
 use R_lattice,        ONLY:bz_samp
 use real_time,        ONLY:RIM_ibz_coo,RIM_bz_coo,l_RT_uses_E_RIM,&
&                           RT_ibz_coo,RT_all2ibz,RT_Nk_ibz,RT_Nk
 use com,              ONLY:error
 !
 implicit none
 !
 type(bz_samp), intent(inout)  :: Xk
 type(levels),  intent(in)     :: Xen
 !
 type(bz_samp) :: Xq
 !
 integer           :: Nk,ik
 integer, external :: RT_k_grid
 !
 ! Check if RIM is used: the value BZ_RIM_tot_Nkpts is obtained from variables_SC.F
 !
 if(BZ_RIM_tot_Nkpts==0) then
   !
   RT_Nk=Xk%nibz
   !
   l_RT_uses_E_rim=.false.
   !
   allocate(RIM_ibz_coo(Xk%nibz,2))
   allocate(RIM_bz_coo(Xk%nbz,2))
   allocate(RT_ibz_coo(Xk%nibz,2))
   allocate(RT_Nk_ibz(Xk%nibz))
   !
   RT_Nk_ibz=1
   allocate(RT_all2ibz(Xk%nibz))
   do ik=1,Xk%nibz
     RIM_ibz_coo(ik,:)=ik
     RT_ibz_coo(ik,:)=ik
     RT_all2ibz(ik)=ik
   enddo
   !
   return
   !
 endif
 !
 Nk=RT_k_grid(Xen,Xk,Xq)
 !
 l_RT_uses_E_RIM=.true.
 !
 ! Shouldn't this check be done by variables_SC when reading BZ_RIM_tot_Nkpts ???
 !
 if(Nk==0) call error(' No ndb.RIM_E_and_k file found while reading from an SC DB with RIM points')
 !
 if(Nk/=RT_nk) call error(' Wrong RT k-grid, likely your ndb.RIM_E_and_k is changed')
 !
 allocate(Xk%pt_RIM(Nk,3))
 do ik=1,Xk%nibz
   Xk%pt_RIM(RT_ibz_coo(ik,1):RT_ibz_coo(ik,2),:)=Xk%ptbz_RIM(RIM_ibz_coo(ik,1):RIM_ibz_coo(ik,2),:)
 enddo
 !
end subroutine RT_check_RIM
