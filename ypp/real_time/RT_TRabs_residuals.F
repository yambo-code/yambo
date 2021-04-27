!
!        Copyright (C) 2000-2021 the YAMBO team
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
subroutine RT_TRabs_residuals(what,N_trans,DIP_kerr_dir,DIP_dir,Xen,Xk)
 !
 use pars,            ONLY:SP,pi,cZERO,cI
 use R_lattice,       ONLY:bz_samp
 use electrons,       ONLY:levels,n_sp_pol,spin,spin_occ
 use DIPOLES,         ONLY:DIP_rotated
 use YPP_real_time,   ONLY:TRabs_RES_left,TRabs_RES_right,TRabs_E
 use YPPm,            ONLY:BS_E
 use BS,              ONLY:BS_H_dim
 use BS_solvers,      ONLY:BSS_eh_table,BS_mat
 use real_time,       ONLY:RT_bands
 !
#include<memory.h>
 !
 character(*)      :: what
 integer           :: N_trans
 type(levels)      :: Xen
 type(bz_samp)     :: Xk
 real(SP)          :: DIP_dir(3),DIP_kerr_dir(3)
 !
 ! Work space
 !
 ! Old...
 !
 integer              :: i_lp,i_v,i_c,i_kbz,i_kibz,i_spin,i_l,bands(2),i_n,i_np
 real(SP)             :: f_eh,delta_f_eh
 complex(SP)          :: dipole
 !
 TRabs_RES_right =cZERO
 TRabs_RES_left =cZERO
 !
 ! BSE residuals
 ! -------------
 if (index(what,"BSE")>0) then
   !
   do i_l=1,BS_H_dim
     !
     do i_lp = 1,BS_H_dim
       !
       i_kbz   = BSS_eh_table(i_lp,1)
       i_kibz  = Xk%sstar(i_kbz,1)
       i_v     = BSS_eh_table(i_lp,2)
       i_c     = BSS_eh_table(i_lp,3)
       i_spin  = spin(BSS_eh_table(i_lp,:))
       !
       if (i_v<RT_bands(1) .or. i_c>RT_bands(2) ) cycle
       !
       f_eh=      (Xen%fo(i_v,i_kibz,i_spin)-Xen%fo(i_c,i_kibz,i_spin))/spin_occ
       delta_f_eh=(Xen%df(i_v,i_kibz,i_spin)-Xen%df(i_c,i_kibz,i_spin))/spin_occ
       !
       dipole=elemental_dipole(i_c,i_v,i_kbz,i_spin)
       !
       TRabs_RES_right(i_l)= TRabs_RES_right(i_l)-dipole*BS_mat(i_lp,i_l)*sqrt(cmplx(f_eh,0.))
       TRabs_RES_left(i_l) = TRabs_RES_left(i_l)-dipole*BS_mat(i_lp,i_l)*delta_f_eh/sqrt(cmplx(f_eh,0.))
       TRabs_E(i_l)        = BS_E(i_l)
       !
     enddo
     !
   enddo
   !
   N_trans=BS_H_dim
   !
   return
   !
 endif
 !
 ! IP residuals
 ! -------------
 if (index(what,"IP")>0) then
   if (index(what,"vv")>0) bands=(/RT_bands(1),Xen%nbf/)
   if (index(what,"cc")>0) bands=(/Xen%nbf+1,RT_bands(2)/)
   do i_kbz=1,Xk%nbz
     i_kibz=Xk%sstar(i_kbz,1)
     do i_spin=1,n_sp_pol
       do i_n=bands(1),bands(2)
         do i_np=bands(1),bands(2)
           if (i_n==i_np) cycle
           if  (index(what,"vv")>0) f_eh=-Xen%f(i_n,i_kibz,i_spin)*(Xen%df(i_np,i_kibz,i_spin))/spin_occ/spin_occ
           if  (index(what,"cc")>0) f_eh= Xen%df(i_np,i_kibz,i_spin)*(spin_occ-Xen%f(i_n,i_kibz,i_spin))/spin_occ/spin_occ
           N_trans=N_trans+1
           dipole=elemental_dipole(i_np,i_n,i_kbz,i_spin)
           TRabs_RES_right(N_trans)=-conjg(dipole)*sqrt(f_eh)
           TRabs_E(N_trans)=Xen%E(i_np,i_kibz,i_spin)-Xen%E(i_n,i_kibz,i_spin)
         enddo
       enddo
     enddo
   enddo
 endif
 !
 contains
   !
   function elemental_dipole(ib,ibp,ik,i_spin)
     real(SP) :: elemental_dipole,dip(3)
     integer  :: ib,ibp,i_spin,ik
     dip=DIP_rotated(ib,ibp,i_kbz,i_spin,"DIP_iR",Xk)
     if (index(what,"KERR")>0) then
       elemental_dipole=sqrt(0.5_SP)*(dot_product(DIP_dir,dip)+cI*dot_product(DIP_kerr_dir,dip))
     endif
     if (index(what,"ABS")>0) then
       elemental_dipole=dot_product(DIP_dir,dip)
     endif
   end function
   !
end subroutine RT_TRabs_residuals
