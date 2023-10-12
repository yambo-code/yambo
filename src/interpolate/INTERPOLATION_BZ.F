!
! License-Identifier: GPL
!
! Copyright (C) 2018 The Yambo Team
!
! Authors (see AUTHORS file for details): CA
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
