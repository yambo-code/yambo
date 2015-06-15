!
! Copyright (C) 2000-2015 M. Gruning, C. Attaccalite and the YAMBO team 
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
subroutine DIPOLE_build_covariants(Xen,Xk,X)
 !
 ! Build the covariant dipole operator at zero field
 ! in PRB 69, 085106 (2004)
 !
 use LIVE_t,         ONLY:live_timing
 use pars,           ONLY:SP,cI,cZERO,pi
 use electrons,      ONLY:levels,n_sp_pol
 use X_m,            ONLY:X_t,DIP_S,DIP_cov
 use R_lattice,      ONLY:bz_samp,k_map
 use vec_operate,    ONLY:k_periodic_idx
 use matrix_operate, ONLY:hermitian
 use D_lattice,      ONLY:a
 !
 implicit none
 !
 type(levels),           intent(in)     :: Xen
 type(bz_samp),          intent(in)     :: Xk
 type(X_t),              intent(in)     :: X      
 !
 ! Work Space
 !
 integer     :: ik,iv,ic,i_spin,ic_min,iv_max,ikm1,id_red,id,idx(3),i1,i2
 integer     :: ikbz,ik2bz
 complex(SP) :: Sm1_plus (Xen%nbf,Xen%nbf,3,n_sp_pol)
 complex(SP) :: Sm1_minus(Xen%nbf,Xen%nbf,3,n_sp_pol)
 complex(SP) :: S_det(3,n_sp_pol)
 complex(SP) :: Wop(1:X%ib(2),1:X%ib(2))
 complex(SP) :: S_tmp(X%ib(2),X%ib(2))
 !
 DIP_cov=cZERO
 !
 call live_timing('Covariant Dipoles:',Xk%nibz)
 !
 ! Notice that DIP_cov are defined only in the IBZ
 ! while the overlaps are in the BZ 
 !
 do ik=1,Xk%nibz 
   !
   ikbz=Xk%sstar(ik,1)
   !
   call Build_Overlaps_Det(Xen,ikbz,X,Sm1_plus,Sm1_minus,S_det)
   !
   do i_spin=1,n_sp_pol
     !
     do id_red=1,3  ! loop on reciprocal lattice
       !
       if(k_map%max_kdir(id_red)/=1) then
         !
         idx=k_map%k_map_inv(ikbz,:)
         idx(id_red)=idx(id_red)-1
         idx=k_periodic_idx(idx,k_map)
         ikm1=k_map%k_map_dir(idx(1),idx(2),idx(3))
         !
         S_tmp=hermitian(DIP_S(:,:,id_red,ikm1,i_spin))
         !
         Wop=cZERO
         !
         forall(i1=1:X%ib(2),i2=1:Xen%nbf) 
           Wop(i1,i2)=sum(DIP_S(i1,1:Xen%nbf,id_red,ikbz,i_spin)*Sm1_plus (1:Xen%nbf,i2,id_red,i_spin)) &
&                    -sum(S_tmp(i1,1:Xen%nbf)                   *Sm1_minus(1:Xen%nbf,i2,id_red,i_spin))
         end forall
         !
         Wop=Wop*cI*k_map%max_kdir(id_red)/(4._SP*pi)
         !
       else
         !
         ! Single point Berry phase not implemented yet
         !
         forall(i1=1:X%ib(2)) Wop(i1,i1)=cZERO 
         !
       endif
       ! 
       ! Transform in cartesian coordinates 
       !
       do id=1,3
         DIP_cov(id,:,:,ik,i_spin)=DIP_cov(id,:,:,ik,i_spin)+transpose(Wop(:,:))*a(id_red,id)
       enddo
       !
     enddo 
     !
     do id=1,3
       DIP_cov(id,:,:,ik,i_spin)=(DIP_cov(id,:,:,ik,i_spin)+hermitian(DIP_cov(id,:,:,ik,i_spin)))/cI
     enddo
     !
   enddo ! spin loop
   !
   call live_timing(steps=1)
   !
   DIP_cov(2,5,4,1,1)=1._SP
   DIP_cov(2,4,5,1,1)=1._SP
   !
 enddo
 !
 call live_timing
 !   
 return
 !
end subroutine DIPOLE_build_covariants
