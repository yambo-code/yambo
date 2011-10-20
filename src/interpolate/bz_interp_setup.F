!
! Copyright (C) 2000-2011 C. Attaccalite and the YAMBO team 
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
subroutine bz_interp_setup(Xk)
 !
 ! Code taken from BolzTraP
 ! http://www.icams.de/content/departments/ams/madsen/boltztrap.html
 !
 use pars,           ONLY:SP,pi
 use interpolate,    ONLY:nwave,lattice_vectors,lpfac,STERN1,int_sop
 use D_lattice,      ONLY:nsym,DL_vol,a,dl_sop
 use R_lattice,      ONLY:bz_samp,b
 use vec_operate,    ONLY:sort
 use matrix_operate, ONLY:m3inv
 use timing,         ONLY:live_timing
 !
 implicit none
 type(bz_samp) :: Xk
 !
 !Work Space
 !
 real(SP)                  :: sphere_radius,metten(3,3),a_inv(3,3),b_inv(3,3)
 real(SP), POINTER         :: absk(:)
 real(SP)                  :: absm,m2(3),kdum(3),stg(3,nsym)
 integer                   :: i,j,i1,i2,i3,is
 integer                   :: ind,nradm,irad,istart,islut
 integer                   :: kmax1(3)
 integer                   :: m(3),nst
 real(SP), allocatable     :: radii(:),tmp_vec(:,:)
 integer,  allocatable     :: icnt(:),ihkl1(:,:),ihkl2(:,:)
 logical                   :: addvec,kdelta
 !
 sphere_radius=(Xk%nibz*nsym*lpfac*3.*DL_vol/4./pi)**(1./3.)
 !
 allocate(int_sop(3,3,nsym))
 !
 do i1=1,3
   kmax1(i1)=sphere_radius*sqrt(dot_product(a(i1,:),a(i1,:)))+1
 enddo
 !
 call m3inv(transpose(a),a_inv)
 call m3inv(b,b_inv)
 !
 do is=1,nsym
   !
   int_sop(:,:,is)=dl_sop(:,:,is)*(2._SP*pi)
   int_sop(:,:,is)=matmul(a_inv,int_sop(:,:,is))
   int_sop(:,:,is)=matmul(int_sop(:,:,is),b_inv)
   !
 enddo
 !
 metten=matmul(a,transpose(a))
 nradm=product(2*kmax1+1)
 allocate(icnt(nradm),radii(nradm),ihkl1(3,nradm))
 irad=0
 call live_timing('BZ-INT',kmax1(3)*2+1)
 do i3=-kmax1(3),kmax1(3)
   M(3)=i3
   do i2=-kmax1(2),kmax1(2)
     M(2)=i2
     do i1=-kmax1(1),kmax1(1)
       M(1)=i1
       m2=m
       absm=dot_product(m2(1:3),matmul(m2(1:3),metten(1:3,1:3)))
       absm=sqrt(absm)
       if(absm>sphere_radius) cycle
       irad=irad+1
       icnt(irad)=irad
       radii(irad)=absm
       ihkl1(1:3,irad)=m(1:3)
     enddo
   enddo
   call live_timing(steps=1)
 enddo
 call live_timing
 ! 
 ! Quicksort over radii
 !
 call sort(arrin=radii(1:irad),indx=icnt(1:irad))
 !
 !     Now re-order equal radii
 !     Copy a sorted hkl list from ihkl1 to ihkl2
 !
 allocate (ihkl2(3,irad))
 do j=1,irad
   i=icnt(j)
   ihkl2(1:3,j)=ihkl1(1:3,i)
 enddo
 deallocate (ihkl1)
 !
 allocate(lattice_vectors(3,irad), absk(irad))
 absk=0._SP
 nwave=1
 m2(1:3)=ihkl2(1:3,1)
 lattice_vectors(1:3,nwave)=ihkl2(1:3,1)
 absk(nwave)=radii(1)
 do j=2,irad      
   m2(1:3)=ihkl2(1:3,j)
   absm=radii(j)
   addvec=.false.
!  new vector of diffent length
   if(absm-absk(nwave)>0.0001) then
     istart=nwave+1
     addvec=.true.
   endif
! .... NEW VECTOR EQU. OLD ONE
   if(.not.addvec) then
     call stern1(m2,nsym,int_sop,nst,stg)
     do i=istart,islut
       kdum(1:3)=lattice_vectors(1:3,I)
       if(kdelta(kdum,nst,stg)) goto 1
     enddo
     addvec=.TRUE.
1  continue
   endif
! .... PUT NEW VECTOR IN LIST
   if(addvec) then
     nwave=nwave+1
     lattice_vectors(1:3,nwave)=m2
     absk(nwave)=absm
     islut=nwave
   endif
 enddo
 !    
 deallocate(absk,icnt,radii,ihkl2)
 !
 allocate(tmp_vec(3,nwave))
 tmp_vec(1:3,1:nwave)=lattice_vectors(1:3,1:nwave)
 deallocate(lattice_vectors)
 allocate(lattice_vectors(3,nwave))
 lattice_vectors(1:3,1:nwave)=tmp_vec(1:3,1:nwave)
 deallocate(tmp_vec)
 !
end subroutine

logical function kdelta(k,nst,stg)
  !.... test, if k is in star of g  (generated in stern)                  
  use pars,         only:sp
  use vec_operate,  only:v_is_zero
  implicit none
  integer          :: nst
  real(sp)         :: k(3),stg(3,nst)
  integer          :: i
  !---------------------------------------------------------------------  
  kdelta=.false.
  do i=1,nst
     if(v_is_zero(stg(:,i)-k(:))) kdelta=.true.
  enddo
end function kdelta
