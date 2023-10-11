!
! License-Identifier: GPL
!
! Copyright (C) 2012 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine RT_ELPH_eq_kernel(ik,i_map,ik_m_q,N_q,E,TIME,ph_freq,Gkkp_sq,S_mat)
 !  
 use pars,           ONLY:SP,cI,pi,cZERO
 use electrons,      ONLY:levels,spin_occ
 use real_time,      ONLY:RT_bands
 !
 implicit none
 !
 type(levels),  intent(in)  :: E
 integer,       intent(in)  :: ik,i_map(-1:1),ik_m_q
 real(SP),      intent(in)  :: TIME,ph_freq,N_q(2)
 real(SP),      intent(in)  :: Gkkp_sq(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 complex(SP),   intent(out) :: S_mat(2,RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 !
 ! Work Space ...
 !
 integer  :: i_n,i_np,i_e,i_s
 real(SP) :: DELTA,GAMMA_,OCCUPATIONS
 !
 S_mat=cZERO
 !
 ! Dimensions
 !============
 !
 do i_s=-1,1,2
   !
   do i_np=RT_bands(1),RT_bands(2)
     !
     do i_e=RT_bands(1),RT_bands(2)
       !
       OCCUPATIONS = (N_q(i_map(-i_s))*(1._SP-E%f(i_e ,ik_m_q,1)/spin_occ)*E%f(i_np,ik    ,1)  -&
&                     N_q(i_map( i_s))*(1._SP-E%f(i_np,ik    ,1)/spin_occ)*E%f(i_e ,ik_m_q,1))/spin_occ
       DELTA       = E%E(i_e,ik_m_q,1)-E%E(i_np,ik,1)
       GAMMA_      = abs(E%W(i_e,ik_m_q,1))+abs(E%W(i_np,ik,1))
       !
       do i_n=RT_bands(1),RT_bands(2)
         !
         if (i_n/=i_np) cycle
         !
         S_mat(i_map(i_s),i_n,i_np)=S_mat(i_map(i_s),i_n,i_np)+&
&                                   Gkkp_sq(i_e,i_n)*OCCUPATIONS*&
&                                   exp(-(cI*DELTA+GAMMA_)*TIME)/(GAMMA_+cI*(DELTA-i_s*ph_freq))
         !
       enddo
       !
     enddo
     !
   enddo
   !
 enddo
 !
end subroutine RT_ELPH_eq_kernel
