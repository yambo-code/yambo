!
! License-Identifier: GPL
!
! Copyright (C) 2017 The Yambo Team
!
! Authors (see AUTHORS file for details): AM CA DS
!
subroutine RT_G_symmetrization(ik,G)
 !
 use pars,           ONLY:SP
 use real_time,      ONLY:RT_bands,G_lesser_reference
 use electrons,      ONLY:spin_occ
 !
 implicit none
 !
 integer,     intent(in)    :: ik
 complex(SP), intent(inout) :: G(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 !
 integer                :: ib,ibp
 real(SP)               :: E_occ_tmp,H_occ_tmp
 complex(SP)            :: tmp
 !
 do ib=RT_bands(1),RT_bands(2)
   !
   G(ib,ib)=cmplx(0._SP,aimag(G(ib,ib)))
   !
   E_occ_tmp= aimag(G(ib,ib))+            aimag(G_lesser_reference(ib,ib,ik))
   H_occ_tmp=-aimag(G(ib,ib))+ ( spin_occ-aimag(G_lesser_reference(ib,ib,ik)) )
   !
   if ( E_occ_tmp<0._SP .or. H_occ_tmp>spin_occ ) G(ib,ib)=                      G_lesser_reference(ib,ib,ik)
   if ( H_occ_tmp<0._SP .or. E_occ_tmp>spin_occ ) G(ib,ib)=cmplx(0._SP,spin_occ)-G_lesser_reference(ib,ib,ik)
   !
   do ibp=ib+1,RT_bands(2)
     !
     tmp=( G(ib,ibp)-conjg(G(ibp,ib)) )/2._SP
     !
     G(ib,ibp)=       tmp
     G(ibp,ib)=-conjg(tmp)
     !
   enddo
   !
 enddo
 !
end subroutine
