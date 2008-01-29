!
! Copyright (C) 2000-2005 A. Marini and the SELF team 
!         http://www.fisica.uniroma2.it/~self
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
subroutine k_lattice(k,Xk)
 !
 ! Output(s)
 !
 !  k_grid_b k_grid_dim k_grid k_grid_uc_vol
 !  eps_2_alpha alpha_dim
 !
 use pars,           ONLY:SP,schlen
 use X_m,            ONLY:eps_2_alpha,alpha_dim
 use memory_m,       ONLY:mem_est
 use vec_operate,    ONLY:v_norm,v_is_zero,cross_product,&
&                         c2a,sort,ak2bz
 use com,            ONLY:msg,error
 use matrix_operate, ONLY:m3det
 use D_lattice,      ONLY:DL_vol,a
 use R_lattice,      ONLY:b,k_grid_b,k_grid_dim,k_grid,RL_vol,&
&                         k_grid_uc_vol,nkibz,nkbz,nXkibz,nXkbz,d3k_factor,&
&                         bz_samp
 use zeros,          ONLY:k_rlu_zero
 !
 implicit none
 type(bz_samp)::k,Xk
 !
 ! Work Space
 !
 integer  :: i1,i2,i3,iv(3),i_dim_coord(3)
 real(SP) :: v0(3),v1(3),v0_ibz_mod
 real(SP),allocatable ::idist(:)
 integer, allocatable ::order(:)
 character(schlen)    ::ch
 !
 ! First I define the Xk type from the first nXkibz kpts of k
 !
 k%description ='k'
 Xk%description='Xk'
 Xk%nibz       =nXkibz
 !
 allocate(Xk%pt(Xk%nibz,3))
 call mem_est("Xk-pt",(/Xk%nibz*3/),(/SP/))
 Xk%pt(:,:)=k%pt(:Xk%nibz,:)
 !
 ! IBZ -> BZ
 !
 call k_expand(k)
 call k_expand(Xk)
 call k_ibz2bz(Xk,'a',.true.)
 !
 nkibz     =k%nibz
 nkbz      =k%nbz
 nXkbz     =Xk%nbz
 d3k_factor=RL_vol/real(Xk%nbz)
 !
 allocate(idist(Xk%nbz),order(Xk%nbz))
 do i1=1,Xk%nbz
   v0=Xk%ptbz(i1,:)-Xk%ptbz(1,:) 
   idist(i1)=ak2bz(v0)
   order(i1)=i1
 enddo
 if (Xk%nbz>1) call sort(idist,indx=order)
 deallocate(idist)
 !
 k_grid_dim=0
 k_grid_b=0.
 do i1=1,Xk%nbz
   v0=Xk%ptbz(order(i1),:)-Xk%ptbz(1,:)
   v0_ibz_mod=ak2bz(v0)
   if (v_norm(v0)<=1.E-5) cycle
   select case(k_grid_dim)
     case(0)
       k_grid_dim=1
       k_grid_b(1,:)=v0
       cycle
     case(1)
       v1=v0-proj(v0,k_grid_b(1,:))*k_grid_b(1,:)
       if (v_is_zero(v1,zero_=k_rlu_zero)) cycle
       k_grid_dim=2
       k_grid_b(2,:)=v0
       cycle
     case(2)
       k_grid_b(3,:)=v0
       !
       ! Here I check the vectors are independent.
       ! I use k_grid_b/v_module(k_grid_b(1,:)) to avoid small numbers
       ! when a large set of k-points is used
       !
       if (abs(m3det(k_grid_b))/v_norm(k_grid_b(1,:))<1.E-5)then
         k_grid_b(3,:)=0.
         cycle
       endif
       k_grid_dim=3
       cycle
     case(3)
       exit
    end select
 enddo
 deallocate(order)
 k_grid=0
 k_grid(k_grid_dim+1:)=1
 !
 if (k_grid_dim/=3) then
   !
   ! If K-grid dim/=3 I need to define the dim-1 k_grid_b to be
   ! the corresponding RL b (For example in a 2D grid in the
   ! x,y plane k_grid_b(3,:)\proto(0,0,1)
   !
   i3=0
   do i1=1,3
     v1(1)=0.
     do i2=1,k_grid_dim
       v1(1)=v1(1)+abs(proj(b(i1,:),k_grid_b(i2,:)))
     enddo
     if (abs(v1(1))<=1.E-5) then
       i3=i3+1
       if (i3+k_grid_dim>3) then
         ch='The grid is not 3D but SELF cannot find the _|_ vector(s)'
         goto 1
       endif
       !
       ! Note that k_grid_b here is rlu (as output is cc) because I am using 
       ! the K components to build it.
       !
       call c2a(b,b(i1,:),k_grid_b(k_grid_dim+i3,:),'kc2a')
     endif
   enddo
 endif
 !
 if (k_grid_dim==0.and.Xk%nbz/=1) then
   ch='The grid is 0D but I have more than 1 kpt.'
   goto 1
 endif 
 !
 ! Check Net size to store all BZ KPTS in rlu coordinates
 !
 iv=1
 i2=0
 if (k_grid_dim<3) iv(k_grid_dim+1:)=0
 do while (any(iv==1))
   do i1=1,k_grid_dim
     v0=i2*k_grid_b(i1,:)
     if (any((/v0(1)>1.-1.E-5,-1.+1.E-5>v0(1),&
&              v0(2)>1.-1.E-5,-1.+1.E-5>v0(2),&
&              v0(3)>1.-1.E-5,-1.+1.E-5>v0(3)/))) iv(i1)=0
     if (iv(i1)>0) k_grid(i1)=i2+1
   enddo
   i2=i2+1
 enddo
 if (product(k_grid)<Xk%nbz) then
   ch='Impossible to verify the K-grid dimension'
   goto 1
 endif
 !
 ! Reporting
 !
 write (ch,'(i1,a)') k_grid_dim,'D'
 call msg('r','Compatible Grid is ',trim(ch))
 if (k_grid_dim>0) call msg('r','B1 [rlu]=',k_grid_b(1,:))
 if (k_grid_dim>1) call msg('r','B2      =',k_grid_b(2,:))
 if (k_grid_dim>2) call msg('r','B3      =',k_grid_b(3,:))
 !
 ! Note that k_grid_b are Cartesian.
 !
 call c2a(b_in=b,v_in=k_grid_b(1,:),mode='ka2c')
 call c2a(b_in=b,v_in=k_grid_b(2,:),mode='ka2c')
 call c2a(b_in=b,v_in=k_grid_b(3,:),mode='ka2c')
 k_grid_uc_vol=0.
 v0=cross_product(k_grid_b(2,:),k_grid_b(3,:))
 do i1=1,3
   k_grid_uc_vol=k_grid_uc_vol+k_grid_b(1,i1)*v0(i1)
 enddo
 k_grid_uc_vol=abs(k_grid_uc_vol)
 !
 if (k_grid_dim>0) call msg('r','Grid dimensions               :',k_grid(:k_grid_dim))
 call msg('r',                  'K lattice UC volume       [au]:',k_grid_uc_vol)
 !
 ! Polarizability convertion factor
 !
 i_dim_coord=1
 if (all((/Xk%pt(:,1)==0./))) i_dim_coord(1)=0
 if (all((/Xk%pt(:,2)==0./))) i_dim_coord(2)=0
 if (all((/Xk%pt(:,3)==0./))) i_dim_coord(3)=0
 if (sum(i_dim_coord)==k_grid_dim) then
   select case(k_grid_dim)
     case(2)
       if (i_dim_coord(1)==0) then
         eps_2_alpha=sqrt(dot_product(a(1,:),a(1,:)))
       else if (i_dim_coord(2)==0) then
         eps_2_alpha=sqrt(dot_product(a(2,:),a(2,:)))
       else if (i_dim_coord(3)==0) then
         eps_2_alpha=sqrt(dot_product(a(3,:),a(3,:)))
       endif
       alpha_dim='length'
     case(1)
       if (i_dim_coord(1)==1) then
         eps_2_alpha=DL_vol/sqrt(dot_product(a(1,:),a(1,:)))
       else if (i_dim_coord(2)==1) then
         eps_2_alpha=DL_vol/sqrt(dot_product(a(2,:),a(2,:)))
       else if (i_dim_coord(3)==1) then
         eps_2_alpha=DL_vol/sqrt(dot_product(a(3,:),a(3,:)))
       endif
       alpha_dim='area'
     case(0)
       alpha_dim='volume'
       eps_2_alpha=DL_vol
   end select
 endif
 !
 ! CLEAN
 !
 call k_ibz2bz(Xk,"d",.false.)
 return
 !
1 if (len_trim(ch)>0) call error(ch)
 !
 contains
   !
   function proj(v1,v2)
   !
   implicit none
   real(SP) :: v1(3),v2(3),proj
   proj=0.
   if (v_is_zero(v2,zero_=k_rlu_zero)) return
   proj=dot_product(v1,v2)/v_norm(v2)**2.
   end function
   !
end subroutine
