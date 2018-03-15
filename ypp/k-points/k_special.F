!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
subroutine k_special()
 !
 use pars,                ONLY:SP,schlen,lchlen
 use units,               ONLY:pi
 use D_lattice,           ONLY:alat,lattice
 use com,                 ONLY:msg,of_open_close,msg_deliver,warning
 use vec_operate,         ONLY:c2a,v_norm
 use YPP,                 ONLY:n_path_pts,PtsPath,K_transform
 use stderr,              ONLY:string_split
 use LIVE_t,              ONLY:live_timing_is_on
 use stderr,              ONLY:intc
 !
#include<memory.h>
 !
 ! Work Space
 ! 
 integer          :: n_special_k,ik,n_total_k,il,n_lines,ip,k_order(50),i_str
 real(SP)         :: trial_b(3,3),special_k(10,3),K_norm
 character(1)     :: k_label(10)
 character(schlen):: Path_strings(50),dumb_ch,k_f_name
 character(lchlen):: of_string
 !
 real(SP),     allocatable :: K_path(:,:)
 character(1), allocatable :: K_path_label(:)
 !
 n_special_k=0
 n_total_k=0
 !
 ! Special K taken from http://cst-www.nrl.navy.mil/bind/kpts/index.html
 !
 select case (trim(lattice))
   !
   case('FCC')
     !
     trial_b(1,:)=(/-1., 1., 1./)*2.*pi/alat(1)
     trial_b(2,:)=(/ 1.,-1., 1./)*2.*pi/alat(1)
     trial_b(3,:)=(/ 1., 1.,-1./)*2.*pi/alat(1)
     !
     n_special_k=6
     !
     special_k(1,:)=(/0.,0.,0./)
     k_label(1)='G'
     special_k(2,:)=(/0.5,0.5,0./)
     k_label(2)='X'
     special_k(3,:)=(/0.5,0.5,0.5/)
     k_label(3)='L'
     special_k(4,:)=(/0.5,0.75,0.25/)
     k_label(4)='W'
     special_k(5,:)=(/0.375,0.375,0.750/)
     k_label(5)='K'
     special_k(6,:)=(/0.375,0.375,0.750/)
     k_label(6)='U'
     !
     do ik=1,n_special_k
       call c2a(b_in=trial_b,v_in=special_k(ik,:),mode='ka2c')
     enddo
     !
   case('BCC')
     !
     trial_b(1,:)=(/ 0., 1., 1./)*2.*pi/alat(1)
     trial_b(2,:)=(/ 1., 0., 1./)*2.*pi/alat(1)
     trial_b(3,:)=(/ 1., 0.,-1./)*2.*pi/alat(1)
     !
     n_special_k=4
     !
     special_k(1,:)=(/0.,0.,0./)
     k_label(1)='G'
     special_k(2,:)=(/0.5,0.5,0./)
     k_label(2)='N'
     special_k(3,:)=(/0.5,0.5,0.5/)
     k_label(3)='P'
     special_k(4,:)=(/0. ,1.  ,0.  /)
     k_label(4)='H'
     !
   case('CUB')
     !
     trial_b(1,:)=(/ 1., 0., 0./)*2.*pi/alat(1)
     trial_b(2,:)=(/ 0., 1., 0./)*2.*pi/alat(1)
     trial_b(3,:)=(/ 0., 0., 1./)*2.*pi/alat(1)
     !
     n_special_k=4
     !
     special_k(1,:)=(/0.,0.,0./)
     k_label(1)='G'
     special_k(2,:)=(/0.5,0.,0./)
     k_label(2)='X'
     special_k(3,:)=(/0.5,0.5,0./)
     k_label(3)='M'
     special_k(4,:)=(/0.5,0.5,0.5/)
     k_label(4)='R'
     !
     do ik=1,n_special_k
       call c2a(b_in=trial_b,v_in=special_k(ik,:),mode='ka2c')
     enddo
     !
   case('HCP')
     !
     trial_b(1,:)=(/ 1.,-1./sqrt(3.), 0./)*2.*pi/alat(1)
     trial_b(2,:)=(/ 1., 1./sqrt(3.), 0./)*2.*pi/alat(1)
     trial_b(3,:)=(/ 0., 0.,          1./)*2.*pi/alat(3)
     !
     n_special_k=6
     !
     special_k(1,:)=(/0.,0.,0./)
     k_label(1)='G'
     special_k(2,:)=(/0. ,0.5,0./)
     k_label(2)='M'
     special_k(3,:)=(/1./3.,1./3.,0./)
     k_label(3)='K'
     special_k(4,:)=(/0. ,0.  ,0.5/)
     k_label(4)='A'
     special_k(5,:)=(/0.,0.5,0.5/)
     k_label(5)='L'
     special_k(6,:)=(/1./3.,1./3.,0.5/)
     k_label(6)='H'
     !
     do ik=1,n_special_k
       call c2a(b_in=trial_b,v_in=special_k(ik,:),mode='ka2c')
     enddo
     !
 end select
 !
 if (n_special_k==0) then
   call warning('Unknown lattice unit cell')
   return
 endif
 !
 call string_split(PtsPath,Path_strings)
 !
 k_order=0
 n_lines=0
 do i_str=1,50
   do ik=1,n_special_k
     if (k_label(ik)==trim(Path_strings(i_str))) then
       n_lines=n_lines+1
       k_order(n_lines)=ik
     endif
   enddo
 enddo
 n_total_k=n_path_pts* ( n_lines -1 )
 !
 if (n_total_k==0) then
   live_timing_is_on=.false.
   do ik=1,n_special_k
     call K_transform(special_k(ik,:),'cc')
     write (dumb_ch,'(3f12.7,2x,a)') special_k(ik,:),k_label(ik)
     call msg("s",trim(dumb_ch))
   enddo
   live_timing_is_on=.true.
 else
   !
   call msg('s','Found '//trim(intc(n_lines-1))//' segments along the path')
   !
   YAMBO_ALLOC(K_path,(n_total_k+1,4))
   allocate(K_path_label(n_total_k+1))
   n_total_k=0
   K_norm=0.
   K_path_label=" "
   do il=1,n_lines-1
     do ip=1,n_path_pts
       n_total_k= n_total_k+1
       if (ip==1) K_path_label(n_total_k)=k_label( k_order(il) )
       K_path(n_total_k,:3)=special_k(k_order(il),:)+ ( special_k(k_order(il+1),:) -  &
&                           special_k(k_order(il),:) )/n_path_pts*(ip-1)
       if (n_total_k>1) K_norm=K_norm+v_norm(  K_path(n_total_k,:3) -  K_path(n_total_k-1,:3) )
       K_path(n_total_k,4)= K_norm
     enddo
   enddo
   n_total_k= n_total_k+1
   K_path(n_total_k,:3)=special_k(k_order(n_lines),:)
   K_path_label(n_total_k)=k_label( k_order(n_lines) )
   K_norm=K_norm+v_norm(  K_path(n_total_k,:3) -  K_path(n_total_k-1,:3) )
   K_path(n_total_k,4)= K_norm
   !
   k_f_name="path_points"
   call of_open_close(k_f_name,'ot')
   call msg('o path',"#",(/"c1  ","c2  ","c3  ","Norm"/),INDENT=0,USE_TABS=.true.)
   call msg('o path',"#"," ",INDENT=0)
   !
   do ik=1,n_total_k
     call K_transform(K_path(ik,:3),'cc')
     call msg('o path',"",K_path(ik,:),INDENT=0,USE_TABS=.true.,formatted_msg=of_string)
     dumb_ch=of_string
     if (len_trim(K_path_label(ik))>0) write (dumb_ch,'(a,5x,3a)') trim(of_string),'[', K_path_label(ik),']'
     call msg_deliver('path',dumb_ch)
   enddo
   !
   call of_open_close(k_f_name)
   !
   deallocate(K_path_label)
   YAMBO_FREE(K_path)
 endif
 !
end subroutine
