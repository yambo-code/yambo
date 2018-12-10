!
!        Copyright (C) 2000-2018 the YAMBO team
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
subroutine Berry_polarization(P_Berry,S_det,en,Xk,k_map)
 !
 ! Evaluate polarization using Berry phase formulation
 ! from the determinants (S_det) of the valence bands overlaps
 !
 use pars,           ONLY:SP,rZERO,pi,cONE,DP
 use electrons,      ONLY:levels,spin_occ
 use R_lattice,      ONLY:bz_samp,bz_map
 use D_lattice,      ONLY:a
 use vec_operate,    ONLY:get_id_perpendicular
 !
 implicit none           
 !
 type(levels),  intent(in)  :: en
 type(bz_samp), intent(in)  :: Xk
 type(bz_map),  intent(in)  :: k_map
 complex(SP),   intent(in)  :: S_det(3,Xk%nbz)
 real(SP),      intent(out) :: P_Berry(3)
 !
 ! Work Space
 !
 integer       :: id_in,id2,id3,Nperpend,i1,i2,i3,idx(3),ik,id,id_red
 complex(DP)   :: zeta
 real(SP)      :: P_berry_red(3) ! Berry polarization along the reciprocal vectors
 !
 ! Polarization in terms of geomatric phase
 ! see eq. 49,50 and 52 of PRB 69, 085104 (2004)
 !
 P_Berry_red  =rZERO
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
         ik=k_map%k_map_dir(idx(1),idx(2),idx(3))
         !
         zeta=zeta*dcmplx(S_det(id_in,ik))
         !
       enddo
       !
       P_berry_red(id_in)=P_berry_red(id_in)+aimag(log(zeta))
       !
     enddo
   enddo
   !
   P_berry_red(id_in)=-P_berry_red(id_in)/real(Nperpend,SP)
   !
 enddo
 !
 P_Berry_red=P_Berry_red/(2._SP*pi)*spin_occ
 !
 ! The polarization is periodic  (for n_sp_pol/=1 the periodicity is different)
 !
 P_Berry_red=P_Berry_red-2._SP*NINT(P_Berry_red/2._SP)
 !
 ! Transform in cartesian coordinates
 !
 P_Berry=rZERO
 !
 do id=1,3
   do id_red=1,3
     P_Berry(id)=P_Berry(id)+P_Berry_red(id_red)*a(id_red,id)/DL_Vol
   enddo
 enddo
 !
end subroutine Berry_polarization
