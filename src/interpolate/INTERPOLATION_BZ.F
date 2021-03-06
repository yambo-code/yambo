!
!        Copyright (C) 2000-2021 the YAMBO team
!              http://www.yambo-code.org
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
subroutine INTERPOLATION_BZ(K,NK,ID,R1D,R2D,C1D,C2D,E)
 use pars,        ONLY:SP
 use R_lattice,   ONLY:bz_samp
 use electrons,   ONLY:levels
 use interpolate, ONLY:interpls,REAL1D,REAL2D,CMPLX1D,CMPLX2D
 use timing_m,    ONLY:timing
 !
#include<memory.h>
 !
 type(bz_samp), intent(in)              :: K
 integer,       intent(in)              :: ID,NK
 real(SP),      intent(out), optional   :: R1D(:,:),R2D(:,:,:)
 complex(SP),   intent(out), optional   :: C1D(:,:),C2D(:,:,:)
 type(levels),  intent(inout), optional :: E
 !
 ! Work Space
 !
 real(SP), allocatable :: RDATA(:,:)
 integer               :: outdim(2),ik,i1,i2
 !
 if(interpls(ID)%ndim==0) call error("Interpolation coefficients not present!")
 !
 if(interpls(ID)%interp_type==REAL1D.and.present(R1D)) then
   call INTERPOLATION_fourier(K,R1D,interpls(ID)%engre,interpls(ID)%ndim,NK,ID)
   return
 endif
 !
 YAMBO_ALLOC(RDATA,(interpls(ID)%ndim,NK))
 !
 call INTERPOLATION_fourier(K,RDATA,interpls(ID)%engre,interpls(ID)%ndim,NK,ID)
 !
 call timing('INTERPOLATE bz',OPR='start')
 !
 if(interpls(ID)%interp_type==CMPLX1D.and.present(C1D)) then
   !   
   outdim(1)=interpls(ID)%ndim/2
   !
   do ik=1,NK
     C1D(1:outdim(1),ik)=CMPLX(RDATA(1:outdim(1),ik),RDATA(outdim(1)+1:2*outdim(1),ik))
   enddo
   !
 elseif(interpls(ID)%interp_type==REAL2D.and.present(R2D)) then
   !
   outdim(1)               =size(R2D(:,1,1))
   outdim(2)               =size(R2D(1,1,:))
   do i1=1,outdim(2)
     R2D(1:outdim(1),1:NK,i1)=RDATA((i1-1)*outdim(1)+1:i1*outdim(1),1:NK)
   enddo
 elseif(interpls(ID)%interp_type==REAL2D.and.present(E)) then
   !
   outdim(1)               =E%nb
   outdim(2)               =size( E%E(1,1,:) )
   do i1=1,outdim(2)
     E%E(1:outdim(1),1:NK,i1)=RDATA((i1-1)*outdim(1)+1:i1*outdim(1),1:NK)
   enddo
   !
 elseif(interpls(ID)%interp_type==CMPLX2D.and.present(C2D)) then
   !
   outdim(1)               =size(C2D(:,1,1))
   outdim(2)               =size(C2D(1,:,1))
   !
   do i1=1,outdim(2)
     i2=i1+outdim(2)  
     C2D(1:outdim(1),i1,1:NK)=cmplx(RDATA((i1-1)*outdim(1)+1:i1*outdim(1),1:NK),& 
&                                     RDATA((i2-1)*outdim(1)+1:i2*outdim(1),1:NK)) 
   enddo
   !
 endif
 !
 YAMBO_FREE(RDATA)
 !
 call timing('INTERPOLATE bz',OPR='stop')
 !
end subroutine INTERPOLATION_BZ
