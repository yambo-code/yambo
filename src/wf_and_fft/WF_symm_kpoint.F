!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): MG, CA, DS
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
subroutine WF_symm_kpoint_cpu(b_to_load,ikbz,Xk,WF_k_out)
 !
 use pars,          ONLY:SP,cZERO
 use electrons,     ONLY:n_sp_pol,n_spinor
 use wave_func,     ONLY:WF,wf_ng
 use R_lattice,     ONLY:bz_samp,g_rot
 use D_lattice,     ONLY:idt_index,spin_sop,i_time_rev,nsym
 !
 implicit none
 !
 integer,       intent(in)  :: ikbz,b_to_load(2)
 type(bz_samp), intent(in)  :: Xk
 complex(SP),   intent(out) :: WF_k_out(wf_ng,n_spinor,b_to_load(1):b_to_load(2),n_sp_pol)
 !
 ! Work space
 !
 integer :: ik,is,i_sp_pol,ib,iwf,ig
 !
 ik = Xk%sstar(ikbz,1)
 is = Xk%sstar(ikbz,2)
 !
 WF_k_out=cZERO
 !
 if(is==idt_index) then
   !  
   do i_sp_pol=1,n_sp_pol
     do ib=b_to_load(1),b_to_load(2)
       iwf=WF%index(ib,ik,i_sp_pol)
       WF_k_out(:,:,ib,i_sp_pol)=WF%c(:,:,iwf)
     enddo
   enddo
   !
 else
   !
   WF_k_out=cZERO
   !
   do i_sp_pol=1,n_sp_pol
     do ib=b_to_load(1),b_to_load(2)
       iwf=WF%index(ib,ik,i_sp_pol)
       WF_k_out(g_rot(:wf_ng,is),:,ib,i_sp_pol)=WF%c(:wf_ng,:,iwf)
       if(n_spinor==2) forall(ig=1:wf_ng) &
&        WF_k_out(ig,:,ib,i_sp_pol)=matmul(spin_sop(:,:,is),WF_k_out(ig,:,ib,i_sp_pol))
     enddo
     !
   enddo
   !
   if(is>nsym/(1+i_time_rev)) WF_k_out=conjg(WF_k_out)
   !
 endif
 !
end subroutine WF_symm_kpoint_cpu

#ifdef _CUDA
subroutine WF_symm_kpoint_gpu(b_to_load,ikbz,Xk,WF_k_out_d)
 !
 use pars,          ONLY:SP,cZERO
 use electrons,     ONLY:n_sp_pol,n_spinor
 use wave_func,     ONLY:WF,wf_ng
 use R_lattice,     ONLY:bz_samp,g_rot_d
 use D_lattice,     ONLY:idt_index,spin_sop_d,i_time_rev,nsym
 use deviceXlib_m,  ONLY:dev_conjg
 !
 implicit none
 !
 integer,       intent(in)  :: ikbz,b_to_load(2)
 type(bz_samp), intent(in)  :: Xk
 complex(SP), device, intent(out) :: WF_k_out_d(wf_ng,n_spinor,b_to_load(1):b_to_load(2),n_sp_pol)
 !
 ! Work space
 !
 complex(SP), device, pointer :: WF_c_d(:,:,:)
 integer :: ik,is,i_sp_pol,ib,iwf
 integer :: i_g,i_spinor
 !
 ik = Xk%sstar(ikbz,1)
 is = Xk%sstar(ikbz,2)
 !
 WF_k_out_d=cZERO
 !
 WF_c_d => WF%c_d
 !
 if(is==idt_index) then
   !  
   do i_sp_pol=1,n_sp_pol
     do ib=b_to_load(1),b_to_load(2)
       iwf=WF%index(ib,ik,i_sp_pol)
       !
       !$cuf kernel do(2) <<<*,*>>>
       do i_spinor=1,n_spinor
       do i_g = 1, wf_ng
          WF_k_out_d(i_g,i_spinor,ib,i_sp_pol)=WF_c_d(i_g,i_spinor,iwf)
       enddo
       enddo
       !
     enddo
   enddo
   !
 else
   !
   do i_sp_pol=1,n_sp_pol
     do ib=b_to_load(1),b_to_load(2)
       !
       iwf=WF%index(ib,ik,i_sp_pol)
       !
       if (n_spinor==1) then
         !
         !$cuf kernel do(2) <<<*,*>>>
         do i_spinor=1,n_spinor
         do i_g=1,wf_ng
           WF_k_out_d(g_rot_d(i_g,is),i_spinor,ib,i_sp_pol)=WF_c_d(i_g,i_spinor,iwf)
         enddo
         enddo
         !
       else
         !
         !$cuf kernel do(1) <<<*,*>>>
         do i_g=1,wf_ng
           WF_k_out_d(g_rot_d(i_g,is),1,ib,i_sp_pol)=spin_sop_d(1,1,is)*WF_c_d(i_g,1,iwf)+ &
&                                                    spin_sop_d(1,2,is)*WF_c_d(i_g,2,iwf)
           WF_k_out_d(g_rot_d(i_g,is),2,ib,i_sp_pol)=spin_sop_d(2,1,is)*WF_c_d(i_g,1,iwf)+ &
&                                                    spin_sop_d(2,2,is)*WF_c_d(i_g,2,iwf)
         enddo
       endif
       !
     enddo
   enddo
   !
   if(is>nsym/(1+i_time_rev)) then
     call dev_conjg(WF_k_out_d)
   endif
   !
 endif
 !
end subroutine WF_symm_kpoint_gpu
#endif
