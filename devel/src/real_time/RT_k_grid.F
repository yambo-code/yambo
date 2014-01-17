!
! Copyright (C) 2000-2013 A. Marini and the YAMBO team 
!            http://www.yambo-code.org
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
integer function RT_k_grid(E,k)
 !
 !       K(BZ)                                                    K(IBZ)
 !               E_RIM(RIM_bz_coo(ikbz,:)=RIM_ibz_coo(ik,:))
 !        ____                                                     ____
 !ikbz1  | 1  | RIM_bz_coo(ikbz1,:) = RIM_ibz_coo(ik1,:)      ik1 | 1  | RT_ibz_coo(ik1,:)     
 !       |____|                     <----------------------       |____|   
 !        ____                                                     ____
 !ikbz2  | 2  | RIM_bz_coo(ikbz2,:) = 1                       ik2 | 2  | RT_ibz_coo(ik2,:)     
 !       |____|                                             /     |____|   
 !        ____                                             /
 !ikbz3  | 3  | RIM_bz_coo(ikbz3,:) = RIM_ibz_coo(ik2,:)  /            
 !       |____|                      <-------------------/        
 ! 
 !
 use pars,           ONLY:SP
 use SC,             ONLY:RT_ibz_coo,RT_all2ibz,RT_Nk_ibz,SC_bands
 use electrons,      ONLY:levels,BZ_RIM_nkpt
 use real_time,      ONLY:RIM_bz_coo,RIM_ibz_coo,RIM_closest_E
 use R_lattice,      ONLY:bz_samp
 !
 implicit none
 !
 type(levels), intent(in) :: E
 type(bz_samp),intent(in) :: k
 !
 ! Work space
 !
 integer :: ik,ik_bz,ik_rim,iE_rim,ik_ref,i_n
 real(SP):: delta_E,delta_E_ref
 !
 ! Number of K-points
 !====================
 !
 RT_k_grid=0
 !
 if (.not.allocated(BZ_RIM_nkpt)) return
 !
 if (.not.allocated(RIM_ibz_coo)) then
   allocate(RIM_ibz_coo(k%nibz,2))
   allocate(RIM_bz_coo(k%nbz,2))
   allocate(RT_ibz_coo(k%nibz,2))
   allocate(RT_Nk_ibz(k%nibz))
 endif
 !
 do ik=1,k%nibz
   ik_bz=1+sum(k%nstar(:ik-1))
   RIM_ibz_coo(ik,:)=(/sum(BZ_RIM_nkpt(:ik_bz-1))+1,sum(BZ_RIM_nkpt(:ik_bz))/)
   RT_ibz_coo(ik,:)=(/RT_k_grid+1,RT_k_grid+BZ_RIM_nkpt(ik_bz)/)
   RT_k_grid=RT_k_grid+BZ_RIM_nkpt(ik_bz)
   RT_Nk_ibz(ik)=BZ_RIM_nkpt(ik_bz)
 enddo
 do ik=1,k%nbz
   RIM_bz_coo(ik,:)=(/sum(BZ_RIM_nkpt(:ik-1))+1,sum(BZ_RIM_nkpt(:ik))/)
 enddo
 allocate(RT_all2ibz(RT_k_grid))
 do ik=1,k%nibz
   do ik_rim=RT_ibz_coo(ik,1),RT_ibz_coo(ik,2)
     RT_all2ibz(ik_rim)=ik
   enddo
 enddo
 !
 if (.not.allocated(RIM_closest_E)) allocate(RIM_closest_E(SC_bands(1):SC_bands(2),RT_k_grid))
 !
 forall(ik=1:k%nibz) RIM_closest_E(:,RT_ibz_coo(ik,1))=ik
 !
 do ik=1,k%nibz
   do ik_rim=RT_ibz_coo(ik,1)+1,RT_ibz_coo(ik,2)
     iE_rim=ik_rim-RT_ibz_coo(ik,1)+RIM_ibz_coo(ik,1)
     do i_n=SC_bands(1),SC_bands(2)
       !
       delta_E_ref=100._SP
       do ik_ref=1,k%nibz
         delta_E=E%E(i_n,ik_ref,1)-E%E_RIM(i_n,iE_rim,1)
         if(abs(delta_E)<abs(delta_E_ref)) then
           delta_E_ref=delta_E
           RIM_closest_E(i_n,ik_rim)=ik_ref
         endif
       enddo
       !
     enddo
   enddo
 enddo
 !
end function RT_k_grid
