!
! Copyright (C) 2000-2012 C. Attaccalite and the YAMBO team 
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
subroutine fourier_interpolation(USER_k,VALUES,engre,ndim)
  !
  use pars,           ONLY:SP,pi
  use interpolate,    ONLY:nwave,lattice_vectors,lpfac,stern1,int_sop
  use R_lattice,      ONLY:bz_samp
  use D_lattice,      ONLY:nsym
  use vec_operate,    ONLY:c2a
  implicit none
  !
  integer, intent(in)       :: ndim
  type(bz_samp), intent(in) :: USER_k
  real(SP), intent(out)     :: VALUES(ndim,USER_k%nibz)
  real(SP), intent(in)      :: engre(nwave,ndim)
  !
  ! Work Space
  !
  real(SP)                 :: arg,tempc
  integer                  :: ikpt,nw,iband,i1
  real(SP)                 :: Rdum(3),stg(3,nsym)
  integer, allocatable     :: nstv(:)
  real(SP), allocatable    :: spwre(:)
  real(SP), allocatable    :: vec(:,:,:)
  !
  allocate (nstv(nwave))
  allocate (spwre(nwave))
  allocate (vec(3,nsym,nwave))
  !
  !precalc vec 
  !
  do nw=1,nwave
     rdum(1:3)=lattice_vectors(1:3,nw)
     call stern1(rdum,nsym,int_sop,nstv(nw),stg)
     forall(i1=1:nstv(nw)) vec(1:3,i1,nw)=stg(1:3,i1)
  enddo
  !
  do ikpt=1,user_k%nibz
     spwre=0._SP
     do nw=1,nwave
       do i1=1,nstv(nw)     
         arg=sum(2._SP*pi*user_k%pt(ikpt,1:3)*vec(1:3,i1,nw))      
         tempc=cos(arg)
         spwre(nw)=spwre(nw)+tempc
       enddo
     enddo
     !
     spwre(1:nwave)=spwre(1:nwave)/nstv(1:nwave)
     !
     forall(iband=1:ndim) VALUES(iband,ikpt) = sum(spwre(1:nwave)*engre(1:nwave,iband))
     !
  enddo
  !
  deallocate (nstv)
  deallocate (spwre)
  deallocate (vec)
end subroutine fourier_interpolation
