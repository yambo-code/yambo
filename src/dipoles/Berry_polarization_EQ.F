!
! License-Identifier: GPL
!
! Copyright (C) 2019 The Yambo Team
!
! Authors (see AUTHORS file for details): MG CA
!                  
subroutine Berry_polarization_EQ(P_Berry_Red,All_S_det,en,Xk,k_map)
 !
 ! Evalute polarization using Berry phase formulation
 ! from the determinants (All_S_det) of the valence bands overlaps
 ! Berry phase is in reduced (crystalline) coordinates
 ! 
 ! Reference:
 ! R.D. King-Smith and D. Vanderbilt, "Theory of polarization of          
 ! crystaline solids", Phys Rev B 47, 1651 (1993).      
 !
 use pars,           ONLY:DP,SP,cZERO,pi,cONE
 use electrons,      ONLY:levels,spin_occ
 use R_lattice,      ONLY:bz_samp,bz_map
 use D_lattice,      ONLY:a
 use vec_operate,    ONLY:get_id_perpendicular
 use electric,       ONLY:l_P_periodic
 !
 implicit none           
 !
 type(levels),  intent(in)  :: en
 type(bz_samp), intent(in)  :: Xk
 type(bz_map),  intent(in)  :: k_map
 complex(SP),   intent(in)  :: All_S_det(6,Xk%nbz)
 complex(SP),   intent(out) :: P_Berry_RED(3)  ! In reduced coordinates
 !
 ! Work Space
 !
 integer       :: id_in,id2,id3,Nperpend,i1,i2,i3,idx(3),ikbz,id
 complex(DP)   :: zeta
 real(DP)      :: imag_zeta
 !
 ! Polarization in terms of geomatric phase
 ! see eq. 49,50 and 52 of PRB 69, 085104 (2004)
 !
 P_Berry_RED  =cZERO
 !
 do id_in=1,3
   !
   call get_id_perpendicular(id_in,id2,id3)
   !
   Nperpend=k_map%max_kdir(id2)*k_map%max_kdir(id3)
   !
   do i2=1,k_map%max_kdir(id2)
     do i3=1,k_map%max_kdir(id3)
       !
       zeta=cONE
       !
       idx(id2)=i2
       idx(id3)=i3
       !
       ! Loop on the string
       !
       do i1=1,k_map%max_kdir(id_in)
         !
         idx(id_in)=i1
         ikbz=k_map%k_map_dir(idx(1),idx(2),idx(3))
         zeta=zeta*All_S_det(id_in,ikbz)
         !
       enddo
       !
       ! Map phases between -pi and pi for all k_perpendicular
       ! Do we miss a factor 2?
       !
       imag_zeta=aimag(log(zeta))
       if(l_P_periodic) imag_zeta=imag_zeta-1._DP*pi*nint(imag_zeta/(1._DP*pi))
       !
       P_Berry_RED(id_in)=P_Berry_RED(id_in)+imag_zeta
       !
     enddo
   enddo
   !
   P_Berry_RED(id_in)=-P_Berry_RED(id_in)/real(Nperpend,SP)
   !
 enddo
 !
 P_Berry_RED=P_Berry_RED/(2._SP*pi)*spin_occ
 !
 if(l_P_periodic) P_Berry_RED=real(P_Berry_RED,SP)-spin_occ*NINT(real(P_Berry_RED,SP)/spin_occ)
 !
end subroutine Berry_polarization_EQ
