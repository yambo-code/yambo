!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): CA DS
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
subroutine INTERPOLATION_coefficients(R1D,R2D,C1D,C2D,E,k,Nk,ID,ID_obj)
 !
 ! Notice that input/output k-points for the interpolation are in iku units
 !
 ! Also note that K index is the the second in R2D
 !
 use pars,              ONLY:SP
 use R_lattice,         ONLY:bz_samp
 use electrons,         ONLY:levels
 use interpolate,       ONLY:interpls,REAL1D,REAL2D,CMPLX1D,CMPLX2D,nshells
 use interpolate_tools, ONLY:get_ID
#if defined _RT
 use real_time,         ONLY:RT_ibz_coo
#endif
 !
#include<memory.h>
 !
 type(bz_samp), intent(in)           :: k
 real(SP),      intent(in), optional :: R1D(:,:),R2D(:,:,:)
 complex(SP),   intent(in), optional :: C1D(:,:),C2D(:,:,:)
 type(levels),  intent(in), optional :: E
 integer,       intent(in), optional :: Nk
 integer,       intent(in)           :: ID_obj
 integer,       intent(inout)        :: ID
 !
 ! Work Space
 !
 real(SP), allocatable :: RDATA(:,:)
 integer               :: inpdim(2),i1,i2,ik,Nk_local
 !
 call get_ID(ID)
 !
 if(     present(Nk)) Nk_local=Nk
 if(.not.present(Nk)) Nk_local=k%nibz
 !
 ! Set dimensions
 !
 if(present(R1D)) then
   !
   interpls(ID)%interp_type=REAL1D
   interpls(ID)%ndim       =size(R1D(:,1))
   !
 else if(present(C1D)) then
   !
   interpls(ID)%interp_type=CMPLX1D
   inpdim(1)               =size(C1D(:,1))
   interpls(ID)%ndim       =2*inpdim(1)
   !
 elseif(present(E)) then
   !
   interpls(ID)%interp_type=REAL2D
   inpdim(1)               =E%nb
   inpdim(2)               =size( E%E(1,1,:) )
   interpls(ID)%ndim       =inpdim(1)*inpdim(2)
   !
 elseif(present(R2D)) then
   !
   interpls(ID)%interp_type=REAL2D
   inpdim(1)               =size(R2D(:,1,1))
   inpdim(2)               =size(R2D(1,1,:))
   interpls(ID)%ndim       =inpdim(1)*inpdim(2)
   !
 elseif(present(C2D)) then
   !
   interpls(ID)%interp_type=CMPLX2D
   inpdim(1)               =size(C2D(:,1,1))
   inpdim(2)               =size(C2D(1,:,1))
   interpls(ID)%ndim       =2*inpdim(1)*inpdim(2)
   !
 endif
 !
 ! Allocations
 !
 YAMBO_ALLOC(interpls(ID)%engre,(nshells,interpls(ID)%ndim))
 if(.not.present(R1D)) then
   YAMBO_ALLOC(RDATA,(interpls(ID)%ndim,Nk_local))
 endif
 !
 ! Transfer to tmp arrays
 !
 if(present(C1D)) then
   !
   RDATA(1:inpdim(1),1:Nk_local)            = real(C1D(1:inpdim(1),1:Nk_local))
   RDATA(inpdim(1)+1:2*inpdim(1),1:Nk_local)=aimag(C1D(1:inpdim(1),1:Nk_local))
   !
 elseif(present(E)) then
   !
#if defined _RT
    if(allocated(E%E_FineGd)) then
      do i1=1,inpdim(2)
        do ik=1,k%nibz
          RDATA((i1-1)*inpdim(1)+1:i1*inpdim(1),RT_ibz_coo(ik,1):RT_ibz_coo(ik,2)) =&
          &             E%E_FineGd(1:inpdim(1),k%ibz_DbGd_map(ik,1):k%ibz_DbGd_map(ik,2),i1)
        enddo
      enddo 
    else
#endif
     do i1=1,inpdim(2)
       RDATA((i1-1)*inpdim(1)+1:i1*inpdim(1),1:Nk_local) = E%E(1:inpdim(1),1:Nk_local,i1)
     enddo
#if defined _RT
   endif
#endif
 elseif(present(R2D)) then
   !
   do i1=1,inpdim(2)
     RDATA((i1-1)*inpdim(1)+1:i1*inpdim(1),1:Nk_local)            = R2D(1:inpdim(1),1:Nk_local,i1)
   enddo
   !
 elseif(present(C2D)) then
   !
   do i1=1,inpdim(2)
     i2=i1+inpdim(2)
     RDATA((i1-1)*inpdim(1)+1:i1*inpdim(1),1:Nk_local) =  real(C2D(1:inpdim(1),i1,1:Nk_local))
     RDATA((i2-1)*inpdim(1)+1:i2*inpdim(1),1:Nk_local) = aimag(C2D(1:inpdim(1),i1,1:Nk_local))
   enddo
   !
 endif
 !
 ! Actual interpolation 
 !  
 if(     present(R1D)) call INTERPOLATION_BZ_coeff(k,R1D  ,interpls(ID)%engre,interpls(ID)%ndim,Nk_local,ID_obj)
 if(.not.present(R1D)) call INTERPOLATION_BZ_coeff(k,RDATA,interpls(ID)%engre,interpls(ID)%ndim,Nk_local,ID_obj)
 !
 ! Free tmp array
 !
 if(.not.present(R1D)) then
   YAMBO_FREE(RDATA)
 endif
 !
end subroutine  INTERPOLATION_coefficients
