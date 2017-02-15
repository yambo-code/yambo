!
!        Copyright (C) 2000-2017 the YAMBO team
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
subroutine k_map_nearest_by_dir(Xk,k_map)
 !
 ! Map k-points on a regular grid along the b directions
 !  
 use pars,           ONLY:SP,IP,zero_dfl,schlen
 use vec_operate,    ONLY:c2a
 use R_lattice,      ONLY:bz_samp,bz_map,WF_shifts
 use com,            ONLY:msg,error
 use pars,           ONLY:zero_dfl
#include<memory.h>
 !
 type(bz_map), intent(inout) :: k_map
 type(bz_samp),intent(inout) :: Xk
 !
 ! Work Space
 !
 integer  :: id,ik,idx(3)
 real(SP) :: k_shift(3),dshift
 character(schlen) :: dump_ch
 !
 YAMBO_ALLOC(WF_shifts,(Xk%nbz,3))
 !
 ! Map k-points on a regular grid with information and the neighboars
 ! along the reciprocal lattice vectors 
 !
 call k_ibz2bz(Xk,'a',.false.)
 !
 ! Check the points out of the BZ before bring them inside
 !
 call check_shifts(Xk,WF_shifts) 
! do ikbz=1,Xk%nbz
!   write(100,*) Xk%ptbz(ikbz,:)
! enddo
 !
 Xk%ptbz(:,:)=Xk%ptbz(:,:)-nint(Xk%ptbz(:,:)) ! Bring in the BZ
 !  
 ! Bring k-points between [0,1)
 !
 where(Xk%ptbz(:,:)<-zero_dfl)
   Xk%ptbz(:,:)=1._SP+Xk%ptbz(:,:)     
 end where
 !
 ! Find the smallest q in the 3 direction 
 !
 call k_find_smallest_q(Xk%ptbz,k_map%q_step,Xk%nbz)
 !
 call k_find_smallest_g0(k_map%g0_idx,k_map%g0_length)                                 
 write(dump_ch,'(a,3f14.8)') " Lenght of G vectors in each direction: ",k_map%g0_length(1:3)
 call msg("r",dump_ch)
 !
 ! Check if it is a shifted grid
 !
 dshift=10.E10_SP
 do ik=1,Xk%nbz
   if(sqrt(sum(Xk%ptbz(ik,:)**2))<dshift) then
      dshift=sqrt(sum(Xk%ptbz(ik,:)**2))
      k_shift =Xk%ptbz(ik,:) 
   endif
 enddo
 !
 if(dshift>zero_dfl) then
   write(dump_ch,'(a,3f12.6)') " Shifted grid: ",k_shift
   call msg("sr",dump_ch)
   call error(' Shifted grids not implemented yet in covariant dipoles ')
 else
   k_shift=0._SP
 endif
 !
 do id=1,3
   k_map%max_kdir(id)=nint(maxval(Xk%ptbz(:,id)-k_shift(id))/k_map%q_step(id))+1
   if(k_map%q_step(id)==0._SP) k_map%max_kdir(id)=1
 enddo
 !
 write(dump_ch,'(a,3i4)') "Number of k-point in each direction: ",k_map%max_kdir
 call msg("s",dump_ch)
 !
 if(allocated(k_map%k_map_dir)) then
   YAMBO_FREE(k_map%k_map_dir)
   YAMBO_FREE(k_map%k_map_inv)
 endif
 YAMBO_ALLOC(k_map%k_map_dir,(k_map%max_kdir(1),k_map%max_kdir(2),k_map%max_kdir(3)))
 YAMBO_ALLOC(k_map%k_map_inv,(Xk%nbz,3))
 ! 
 !Find index	     
 ! 
 do ik=1,Xk%nbz
   do id=1,3
     if(abs(k_map%q_step(id))>zero_dfl) then
       idx(id)=nint((Xk%ptbz(ik,id)-k_shift(id))/k_map%q_step(id))+1
     else ! non periodic directions
       idx(id)=1
     endif
   enddo
   k_map%k_map_dir(idx(1),idx(2),idx(3))=ik
   k_map%k_map_inv(ik,:)=idx
 enddo
 !
end subroutine k_map_nearest_by_dir
 
subroutine k_find_smallest_q(ptbz,q_step,nbz)
 !
 ! Find the smallest q in the 3 direction 
 !
 use pars,           ONLY:SP,IP,zero_dfl,schlen
 use R_lattice,      ONLY:bz_samp
 use com,            ONLY:msg
 !
 implicit none
 !
 integer, intent(in)   :: nbz
 real(SP),intent(in)   :: ptbz(nbz,3)
 real(SP),intent(out)  :: q_step(3)
 !
 ! Work space
 !
 integer           :: i1,id
 real(SP)          :: dist
 character(schlen) :: dump_ch
 !
 q_step(:)=10000
 !
 do i1=1,nbz 
   do id=1,3
     dist=abs(ptbz(i1,id)-ptbz(1,id))
     if(dist<q_step(id).and.dist>zero_dfl) q_step(id)=dist
   enddo
 enddo
 !
 if(any(q_step==10000)) call msg("s"," System with reduced dimensionality ")
 !
 where(q_step==10000)
   q_step = 0._SP
 end where
 !
 call msg("s"," Smallest q vectors :")
 write(dump_ch,'(3f14.6)') q_step(:)
 call msg("s",dump_ch)
 !
end subroutine k_find_smallest_q

subroutine k_find_smallest_g0(g0_idx,g0_length)
 !
 ! Find the smallest G0 in each direction
 !
 use pars,           ONLY:SP,schlen
 use vec_operate,    ONLY:c2a
 use R_lattice,      ONLY:g_vec,ng_closed
 use com,            ONLY:msg                                                                                   
 use vec_operate,    ONLY:v_is_zero,v_norm
 use zeros,          ONLY:G_iku_zero
 !
 implicit none
 !
 integer, intent(out) :: g0_idx(3,2)
 real(SP),intent(out) :: g0_length(3)
 !
 ! Work Space
 !
 integer :: id,i1
 character(schlen)  :: dump_ch
 real(SP)           :: g0(3)
 !
 do id=1,3
   !  
   g0=0._SP
   g0(id)=1._SP ! in reduced coordinates
   !
   call c2a(v_in=g0,mode='ka2i')
   !
   do i1=1,ng_closed 
     if (v_is_zero(g0-g_vec(i1,:),zero_=G_iku_zero)) then
       g0_idx(id,1)=i1
       exit
     endif
   enddo
   !
   do i1=1,ng_closed 
     if (v_is_zero(-g0-g_vec(i1,:),zero_=G_iku_zero)) then
       g0_idx(id,2)=i1
       exit
     endif
   enddo 
   !
   g0=0._SP
   g0(id)=1._SP ! in reduced coordinates
   !
   call c2a(v_in=g0,mode='ka2c')
   !
   g0_length(id)=v_norm(g0)
   !
 enddo
 !
 write(dump_ch,'(a,3i3)') " Index g0 smallest positive vectors in each direction: ",g0_idx(:,1)
 call msg("s",dump_ch)
 write(dump_ch,'(a,3i3)') " Index g0 smallest negative vectors in each direction: ",g0_idx(:,2)
 call msg("s",dump_ch)
 !
end subroutine k_find_smallest_g0

subroutine check_shifts(Xk,WF_shifts)
 !
 use pars,                ONLY:SP,lchlen
 use R_lattice,           ONLY:bz_samp
 !
 implicit none
 !
 type(bz_samp),intent(in)   :: Xk
 integer,      intent(out)  :: WF_shifts(Xk%nbz,3)
 !
 ! Work Space
 !
 integer :: ik,id
 real(SP), parameter :: EPS=1.E-5
 !
 WF_shifts=0
 !
 do ik=1,Xk%nbz
   do id=1,3
     if(Xk%ptbz(ik,id)<-EPS) then
        WF_shifts(ik,id)=1
     elseif(Xk%ptbz(ik,id)>1._SP-EPS) then
        WF_shifts(ik,id)=2
     endif
   enddo
 enddo
 !
end subroutine check_shifts
