!
! Copyright (C) 2000-2012 C. Attaccalite, M. Gruening and the YAMBO team 
!              http://www.yambo-code.org
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
 use vec_operate,    ONLY:c2a,rlu_k2bz
 use R_lattice,      ONLY:bz_samp,rl_sop,ng_closed,bz_map
 use D_lattice,      ONLY:nsym
 use com,            ONLY:msg,warning,error
 use memory_m,       ONLY:mem_est
 use pars,           ONLY:zero_dfl
 !
 implicit none
 !
 type(bz_map), intent(inout) :: k_map
 type(bz_samp),intent(inout) :: Xk
 !
 ! Work Space
 !
 integer  :: id,ik,idx(3)
 real(SP) :: g_red(3),k_comp
 logical  :: found_all,found_dir(3)
 real(SP) :: k_shift(3),dshift
 character(schlen) :: dump_ch
 !
 ! This subroutine works without symm and in reciprocal lattice coordinates
 !
 call k_ibz2bz(Xk,'a',.true.)
 !
 ! Bring k-points between [0,1)
 !
 where(Xk%ptbz(:,:)<-zero_dfl)
   Xk%ptbz(:,:)=1._SP+Xk%ptbz(:,:)
 end where
 !
 ! Find G-vector shifts associated with symmetries
 !
 call find_symm_shifts(Xk)
 !
 ! Find the smallest q in the 3 direction 
 !
 call k_find_smallest_q(Xk%ptbz,k_map%q_step,Xk%nbz)
 !
 call k_find_smallest_g0(k_map)                                 
 write(dump_ch,'(a,3f14.8)') " Lenght of G vectors in each direction: ",k_map%g0_length(1:3,1)
 call msg("r",dump_ch)
 !
 call generate_G_idx(k_map)
 !
 ! Check if it is a shifted grid
 !
 dshift=10E10_SP
 do ik=1,Xk%nbz
   if(sqrt(sum(Xk%ptbz(ik,:)**2))<dshift) then
      dshift=sqrt(sum(Xk%ptbz(ik,:)**2))
      k_shift =Xk%ptbz(ik,:) 
   endif
 enddo
 !
 if(dshift>zero_dfl) then
   write(dump_ch,'(a,3f12.6)') " Shifted grid: ",k_shift
   call msg("r",dump_ch)
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
   deallocate(k_map%k_map_dir,k_map%k_map_inv)
   call mem_est("k_map k_map_inv")
 endif
 allocate(k_map%k_map_dir(k_map%max_kdir(1),k_map%max_kdir(2),k_map%max_kdir(3)))
 call mem_est("k_map",(/size(k_map%k_map_dir)/),(/IP/))
 allocate(k_map%k_map_inv(Xk%nbz,3))
 call mem_est("k_map_inv",(/size(k_map%k_map_inv)/),(/IP/))
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

subroutine k_find_smallest_g0(k_map)
 !
 ! Find the smallest G0 in each direction
 !
 use pars,           ONLY:SP,schlen
 use vec_operate,    ONLY:c2a
 use R_lattice,      ONLY:g_vec,ng_closed,bz_map
 use com,            ONLY:msg                                                                                   
 use vec_operate,    ONLY:v_is_zero,v_norm
 use zeros,          ONLY:G_iku_zero
 !
 implicit none
 !
 type(bz_map), intent(inout) :: k_map
 !
 ! Work Space
 !
 integer :: id,i1,ig
 character(schlen)  :: dump_ch
 real(SP)           :: g0(3)
 !
 do ig=1,2 ! Find the first two smaller G-vectors
   do id=1,3
     !  
     g0=0._SP
     g0(id)=real(ig,SP) ! in reduced coordinates
     !
     call c2a(v_in=g0,mode='ka2i')
     !
     do i1=1,ng_closed 
       if (v_is_zero(g0-g_vec(i1,:),zero_=G_iku_zero)) then
         k_map%g0_plus_idx(id,ig)=i1
         exit
       endif
     enddo
     !
     do i1=1,ng_closed 
       if (v_is_zero(-g0-g_vec(i1,:),zero_=G_iku_zero)) then
         k_map%g0_minus_idx(id,ig)=i1
         exit
       endif
     enddo 
     !
     g0=0._SP
     g0(id)=real(ig,SP) ! in reduced coordinates
     !
     call c2a(v_in=g0,mode='ka2c')
     !
     k_map%g0_length(id,ig)=v_norm(g0)
     !
   enddo
 enddo
 !
 write(dump_ch,'(a,3i3)') " Index g0 smallest positive vectors in each direction: ",k_map%g0_plus_idx(:,1)
 call msg("rs",dump_ch)
 write(dump_ch,'(a,3i3)') " Index g0 smallest negative vectors in each direction: ",k_map%g0_minus_idx(:,1)
 call msg("rs",dump_ch)
 !
end subroutine k_find_smallest_g0

subroutine generate_G_idx(k_map)
 !
 ! Find the smallest G vectors in each direction
 ! they are used to shift wavefunction by a G-vector
 !
 use pars,           ONLY:SP,schlen
 use vec_operate,    ONLY:c2a
 use R_lattice,      ONLY:g_vec,ng_closed,bz_map
 use com,            ONLY:msg                                                                                   
 use vec_operate,    ONLY:v_is_zero,v_norm
 use zeros,          ONLY:G_iku_zero
 !
 implicit none
 !
 type(bz_map), intent(inout) :: k_map
 !
 ! Work Space
 !
 integer :: ix,iy,iz,i1
 real(SP)           :: g0(3)
 !
 do ix=-3,3
   do iy=-3,3
     do iz=-3,3
     !
     g0=real((/ix,iy,iz/),SP) ! in reduced coordinates
     !
     call c2a(v_in=g0,mode='ka2i')
     !
     do i1=1,ng_closed 
       if (v_is_zero(g0-g_vec(i1,:),zero_=G_iku_zero)) then
         k_map%g_idx(ix,iy,iz)=i1
         exit
       endif
     enddo
     !
     enddo
   enddo
 enddo
 !
end subroutine generate_G_idx 


subroutine find_symm_shifts(Xk)
 !
 ! Find the shift associated to each symmetry to bring
 ! back k_points in the BZ mantaining the periodic gauge
 !
 use pars,           ONLY:SP,zero_dfl
 use R_lattice,      ONLY:bz_samp,rl_sop,symm_shift,nkbz
 use D_lattice,      ONLY:nsym,idt_index,sop_inv
 use vec_operate,    ONLY:c2a
 !
 implicit none
 !
 type(bz_samp), intent(in) :: Xk
 !
 ! Work space
 !
 integer  :: ikbz,ik,is,id
 real(SP) :: v_ibz(3),v_bz(3),v1(3),v2(3)
 !
 if(.not.allocated(symm_shift)) allocate(symm_shift(nkbz,3))
 !
 symm_shift=0  
 !
 do ikbz=1,Xk%nbz
   !
   ik = Xk%sstar(ikbz,1)
   is = Xk%sstar(ikbz,2)
   !
   v_ibz=Xk%pt(ik,:)
   v_bz =matmul(rl_sop(:,:,is),v_ibz)
   call c2a(v_in=v_ibz,mode='ki2a')
   call c2a(v_in=v_bz,mode='ki2a')
   !
   do id=1,3
     if(v_bz(id)<-zero_dfl) then
       symm_shift(ikbz,id)=1
       v_bz(id)=v_bz(id)+1._SP
     endif
     if(v_bz(id)>1._SP+zero_dfl) then
       symm_shift(ikbz,id)=-1
       v_bz(id)=v_bz(id)-1._SP
     endif
   enddo
   !
   v1=real(symm_shift(ikbz,:))
   call c2a(v_in=v1,v_out=v2,mode='ka2i')
   v1=matmul(rl_sop(:,:,sop_inv(is)),v2)
   call c2a(v_in=v1,mode='ki2a')
   symm_shift(ikbz,:)=nint(v1)
   !
 enddo
 !
 ! Checking
 !
! do ikbz=1,Xk%nbz
!   !
!   ik = Xk%sstar(ikbz,1)
!   is = Xk%sstar(ikbz,2)
!   !
!   v1=real(symm_shift(ikbz,:))
!   call c2a(v_in=v1,mode='ka2i')
!   v_ibz=Xk%pt(ik,:)+v1
!   v_bz =matmul(rl_sop(:,:,is),v_ibz)
!   call c2a(v_in=v_ibz,mode='ki2a')
!   call c2a(v_in=v_bz,mode='ki2a')
!   !
!   write(*,*)
!   write(*,'(3f16.8)') v_ibz
!   write(*,'(3f16.8)') v_bz
!   !
! enddo
 !
end subroutine find_symm_shifts
