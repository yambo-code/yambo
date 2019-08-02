!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): MG, CA
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
 use pars,           ONLY:DP,SP,cI,cZERO,pi,cONE
 use electrons,      ONLY:n_sp_pol,levels,spin_occ,n_spin
 use R_lattice,      ONLY:bz_samp,bz_map
 use D_lattice,      ONLY:a
 use vec_operate,    ONLY:get_id_perpendicular
 !
 implicit none           
 !
 type(levels),  intent(in)  :: en
 type(bz_samp), intent(in)  :: Xk
 type(bz_map),  intent(in)  :: k_map
 complex(SP),   intent(in)  :: All_S_det(6,Xk%nbz,n_sp_pol)
 complex(SP),   intent(out) :: P_Berry_RED(3)  ! In reduced coordinates
 !
 ! Work Space
 !
 integer       :: id_in,id2,id3,Nperpend,i1,i2,i3,idx(3),ikbz,id,id_red
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
         zeta=zeta*All_S_det(id_in,ikbz,1)
         !
       enddo
       !
       ! Map phases between -pi and pi for all k_perpendicular
       ! Do we miss a factor 2?
       !
       imag_zeta=aimag(log(zeta))
       imag_zeta=imag_zeta-1._DP*pi*nint(imag_zeta/(1._DP*pi))
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
 if(n_spin==1) then
   P_Berry_RED=real(P_Berry_RED,SP)-2._SP*NINT(real(P_Berry_RED,SP)/2._SP)
 else
   P_Berry_RED=real(P_Berry_RED,SP)-1._SP*NINT(real(P_Berry_RED,SP)/1._SP)
 endif
 !
end subroutine Berry_polarization_EQ
