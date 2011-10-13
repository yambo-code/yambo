!
! Copyright (C) 2000-2010 C. Attaccalite and the YAMBO team 
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
subroutine bz_interpolation(USER_k,VALUES,icut1,icut2)
  !
  use pars,           ONLY:SP,pi
  use interpolate,    ONLY:nwave,lattice_vectors,lpfac,STERN1,engre,int_sop
  use R_lattice,      ONLY:bz_samp
  use D_lattice,      ONLY:nsym
  use vec_operate,    ONLY:c2a
  IMPLICIT NONE
  !
  INTEGER,INTENT(IN)        :: icut1,icut2
  type(bz_samp), intent(in) :: USER_k
  real(SP), intent(out)     :: VALUES(icut1:icut2,USER_k%nibz)
  !
  ! Work Space
  !
  REAL(SP)                 :: arg,tempc
  INTEGER                  :: ikpt,i,nw
  INTEGER                  :: iband
  REAL(SP)                 :: Rdum(3)
  REAL(SP)                 :: stg(3,nsym)
  integer, allocatable     :: nstv(:)
  REAL(SP), allocatable    :: spwre(:)
  REAL(SP), allocatable    :: vec(:,:,:)
  !
  allocate (nstv(nwave))
  allocate (spwre(nwave))
  allocate (vec(3,nsym,nwave))
  !
  !precalc vec 
  !
  DO nw=1,nwave
     Rdum(1:3)=lattice_vectors(1:3,nw)
     CALL stern1(Rdum,nsym,int_sop,nstv(nw),stg)
     forall(i=1:nstv(nw)) vec(1:3,i,nw)=stg(1:3,i)
  ENDDO
  !
  DO ikpt=1,USER_k%nibz
     DO nw=1,nwave
       spwre(nw)=sum(cos(2._SP*pi*(USER_k%pt(ikpt,1)*vec(1,1:nstv(nw),nw) &
&                                 +USER_k%pt(ikpt,2)*vec(2,1:nstv(nw),nw) &
&                                 +USER_k%pt(ikpt,3)*vec(2,1:nstv(nw),nw))))
     ENDDO
     !
     spwre(1:nwave)=spwre(1:nwave)/nstv(1:nwave)
     !
     forall(iband=icut1:icut2) VALUES(iband,ikpt) = sum(spwre(1:nwave)*engre(1:nwave,iband))
     !
  ENDDO
  !
  deallocate (nstv)
  deallocate (spwre)
  deallocate (vec)
END SUBROUTINE bz_interpolation
