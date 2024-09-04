!
! License-Identifier: GPL
!
! Copyright (C) 2014 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine RT_rotating_frame(ik,rotation_isign,O,T)
 !
 ! isign=+1 normal -> RF
 ! isign=-1 RF     -> normal
 !
 use pars,           ONLY:SP,cI
 use real_time,      ONLY:RT_Deph_Matrix,RT_deph_range,RTibz,&
&                         l_RT_EXP,RT_is_statically_dephased,RT_bands,NE_time,H_RF
 !
 implicit none
 !
 integer,      intent(in)     :: ik,rotation_isign
 real(SP),     intent(in)     :: T
 complex(SP),  intent(inout)  :: O(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 !
 ! Work space
 !
 O(:,:) = O(:,:)*exp(rotation_isign*cI*H_RF(:,:,ik,1)*T)
 !
end subroutine RT_rotating_frame
