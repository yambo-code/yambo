!
!        Copyright (C) 2000-2017 the YAMBO team
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
subroutine eval_interpolation_coeff(R1D,R2D,C1D,C2D,E,k,Nk,ID)
 !
 ! Notice that input/output k-points for the interpolation 
 ! are in iku units
 !
 use pars,        ONLY:SP
 use R_lattice,   ONLY:bz_samp
 use electrons,   ONLY:levels,n_sp_pol
 use interpolate, ONLY:interpls,REAL1D,REAL2D,CMPLX1D,CMPLX2D,nshells,get_ID
#if defined _RT
 use real_time,   ONLY:DbGd_ibz_coo,RT_ibz_coo
#endif
 !
 implicit none
 !
 type(bz_samp), intent(in)           :: k
 real(SP),      intent(in), optional :: R1D(:,:),R2D(:,:,:)
 complex(SP),   intent(in), optional :: C1D(:,:),C2D(:,:,:)
 type(levels),  intent(in), optional :: E
 integer,       intent(in), optional :: Nk
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
 if(present(R1D)) then
   !
   interpls(ID)%interp_type=REAL1D
   interpls(ID)%ndim       =size(R1D(:,1))
   allocate(interpls(ID)%engre(nshells,interpls(ID)%ndim))
   call bz_interp_coeff(k,R1D,interpls(ID)%engre,interpls(ID)%ndim,Nk_local)
   return
   !
 endif
 !
 if(present(C1D)) then
   !
   interpls(ID)%interp_type=CMPLX1D
   inpdim(1)               =size(C1D(:,1))
   interpls(ID)%ndim       =2*inpdim(1)
   allocate(interpls(ID)%engre(nshells,interpls(ID)%ndim))
   allocate(RDATA(interpls(ID)%ndim,Nk_local))
   RDATA(1:inpdim(1),1:Nk_local)            = real(C1D(1:inpdim(1),1:Nk_local))
   RDATA(inpdim(1)+1:2*inpdim(1),1:Nk_local)=aimag(C1D(1:inpdim(1),1:Nk_local))
   !
 elseif(present(E)) then
   !
   interpls(ID)%interp_type=REAL2D
   inpdim(1)               =E%nb
   inpdim(2)               =n_sp_pol
   interpls(ID)%ndim       =inpdim(1)*inpdim(2)
   allocate(interpls(ID)%engre(nshells,interpls(ID)%ndim))
   allocate(RDATA(interpls(ID)%ndim,Nk_local))
#if defined _RT
   if(associated(E%E_DbGd)) then
     do i1=1,inpdim(2)
       do ik=1,k%nibz
         RDATA((i1-1)*inpdim(1)+1:i1*inpdim(1),RT_ibz_coo(ik,1):RT_ibz_coo(ik,2)) =&
         &             E%E_DbGd(1:inpdim(1),DbGd_ibz_coo(ik,1):DbGd_ibz_coo(ik,2),i1)
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
   interpls(ID)%interp_type=REAL2D
   inpdim(1)               =size(R2D(:,1,1))
   inpdim(2)               =size(R2D(1,:,1))
   interpls(ID)%ndim       =inpdim(1)*inpdim(2)
   allocate(interpls(ID)%engre(nshells,interpls(ID)%ndim))
   allocate(RDATA(interpls(ID)%ndim,Nk_local))
   do i1=1,inpdim(2)
     RDATA((i1-1)*inpdim(1)+1:i1*inpdim(1),1:Nk_local)            = R2D(1:inpdim(1),i1,1:Nk_local)
   enddo
   !
 elseif(present(C2D)) then
   !
   interpls(ID)%interp_type=CMPLX2D
   inpdim(1)               =size(C2D(:,1,1))
   inpdim(2)               =size(C2D(1,:,1))
   interpls(ID)%ndim       =2*inpdim(1)*inpdim(2)
   allocate(interpls(ID)%engre(nshells,interpls(ID)%ndim))
   allocate(RDATA(interpls(ID)%ndim,Nk_local))
   do i1=1,inpdim(2)
     i2=i1+inpdim(2)
     RDATA((i1-1)*inpdim(1)+1:i1*inpdim(1),1:Nk_local) =  real(C2D(1:inpdim(1),i1,1:Nk_local))
     RDATA((i2-1)*inpdim(1)+1:i2*inpdim(1),1:Nk_local) = aimag(C2D(1:inpdim(1),i1,1:Nk_local))
   enddo
   !
 endif
 !  
 call bz_interp_coeff(k,RDATA,interpls(ID)%engre,interpls(ID)%ndim,Nk_local)
 deallocate(RDATA)
 !
end subroutine  eval_interpolation_coeff
