!
! Copyright (C) 2000-2013 A. Marini and the YAMBO team 
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
subroutine K_exchange(divide_by_qpg,iq,ik,Xk,Ox,X_dim)
 !
 use pars,           ONLY:SP
 use BS,             ONLY:BS_n_g_exch,BS_eh_table,BS_blk_dim,BS_bands
 use D_lattice,      ONLY:nsym,i_time_rev,sop_inv
 use R_lattice,      ONLY:g_rot,qindx_X,bz_samp,bare_qpg
 use collision,      ONLY:ggwinfo,collision_reset
 use electrons,      ONLY:n_sp_pol,spin
 !
 implicit none
 type(bz_samp) ::Xk
 integer       :: ik,iq,X_dim
 complex(SP)   :: Ox(BS_n_g_exch,X_dim)
 logical       :: divide_by_qpg
 !
 ! Work Space
 !
 type(ggwinfo)::isc
 integer :: i1,i2,i3,ikp,ikpbz,ikbz,iv1,ic1,is,isp,Hi,b_ref,i_sp
 integer :: nnst(BS_bands(2)-BS_bands(1)+1,BS_bands(2)-BS_bands(1)+1,nsym,n_sp_pol)
 !
 call collision_reset(isc)
 !
 b_ref=BS_bands(1)-1
 isc%ngrho=BS_n_g_exch
 allocate(isc%rhotw(BS_n_g_exch))
 !
 nnst=0
 !
 Hi=sum(BS_blk_dim(:ik-1))
 do i1=1,BS_blk_dim(ik)
   ikbz=BS_eh_table(i1+Hi,1)
   iv1=BS_eh_table(i1+Hi,2)
   ic1=BS_eh_table(i1+Hi,3)
   i_sp=spin(BS_eh_table(i1+Hi,:))
   ikpbz=qindx_X(iq,ikbz,1)
   is= Xk%sstar(ikbz,2)
   ikp=Xk%sstar(ikpbz,1)
   isp=Xk%sstar(ikpbz,2)
   isc%is=(/ic1,ik,is,i_sp/)
   isc%os=(/iv1,ikp,isp,i_sp/)
   isc%qs=(/qindx_X(iq,ikbz,2),iq,1/)
   select case (iq)
     case(1)
       if (is==1) then
         isc%is(3)=1
         isc%os(3)=1
         call scatterBamp(isc)
         if (divide_by_qpg) then
           forall(i2=1:BS_n_g_exch) Ox(i2,i1)=isc%rhotw(i2)/bare_qpg(iq,i2)
         else
           forall(i2=1:BS_n_g_exch) Ox(i2,i1)=isc%rhotw(i2)
         endif
         nnst(ic1-b_ref,iv1-b_ref,1,i_sp)=i1
       else
         do i2=1,BS_n_g_exch
           i3=g_rot(sop_inv(is),i2)
           Ox(i2,i1)=Ox(i3,nnst(ic1-b_ref,iv1-b_ref,1,i_sp))
         enddo
         if (is>nsym/(i_time_rev+1))  Ox(:,i1)=conjg(Ox(:,i1))
       endif
     case(2:)
       call scatterBamp(isc)
       if (divide_by_qpg) then
         forall(i2=1:BS_n_g_exch) Ox(i2,i1)=isc%rhotw(i2)/bare_qpg(iq,i2)
       else
         forall(i2=1:BS_n_g_exch) Ox(i2,i1)=isc%rhotw(i2)
       endif
   end select
 enddo
 !
 call collision_reset(isc)
 !
end subroutine
