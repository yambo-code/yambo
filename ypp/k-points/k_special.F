!
!        Copyright (C) 2000-2017 the YAMBO team
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
 use R_lattice,           ONLY:bz_samp_reset
 use D_lattice,           ONLY:alat,lattice
 use com,                 ONLY:msg,of_open_close,msg_deliver,warning
 use vec_operate,         ONLY:c2a,v_norm
 use YPP,                 ONLY:n_path_pts,PtsPath,K_transform,l_high_sym_pts,&
&                              SPECIAL_k,n_SPECIAL_k_MAX,CIRCUIT_k,SPECIAL_k_label,coo_out
 use stderr,              ONLY:string_split
 use LIVE_t,              ONLY:live_timing_is_on
 use stderr,              ONLY:intc
 !
#include<memory.h>
 !
 ! Work Space
 ! 
 integer          :: ik,il,n_lines,ip,k_order(50),i_str
 real(SP)         :: trial_b(3,3),K_norm
 character(schlen):: Path_strings(50),dumb_ch,k_f_name(2)
 character(lchlen):: of_string
 character(1), allocatable :: K_path_label(:)
 !
 call bz_samp_reset(SPECIAL_k)
 call bz_samp_reset(CIRCUIT_k)
 !
 YAMBO_ALLOC(SPECIAL_k%pt,(n_SPECIAL_k_MAX,3))
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
     SPECIAL_k%nbz=6
     !
     SPECIAL_k%pt(1,:)=(/0.,0.,0./)
     SPECIAL_k_label(1)='G'
     SPECIAL_k%pt(2,:)=(/0.5,0.5,0./)
     SPECIAL_k_label(2)='X'
     SPECIAL_k%pt(3,:)=(/0.5,0.5,0.5/)
     SPECIAL_k_label(3)='L'
     SPECIAL_k%pt(4,:)=(/0.5,0.75,0.25/)
     SPECIAL_k_label(4)='W'
     SPECIAL_k%pt(5,:)=(/0.375,0.375,0.750/)
     SPECIAL_k_label(5)='K'
     SPECIAL_k%pt(6,:)=(/0.375,0.375,0.750/)
     SPECIAL_k_label(6)='U'
     !
     do ik=1,SPECIAL_k%nbz
       call c2a(b_in=trial_b,v_in=SPECIAL_k%pt(ik,:),mode='ka2c')
     enddo
     !
   case('BCC')
     !
     trial_b(1,:)=(/ 0., 1., 1./)*2.*pi/alat(1)
     trial_b(2,:)=(/ 1., 0., 1./)*2.*pi/alat(1)
     trial_b(3,:)=(/ 1., 0.,-1./)*2.*pi/alat(1)
     !
     SPECIAL_k%nbz=4
     !
     SPECIAL_k%pt(1,:)=(/0.,0.,0./)
     SPECIAL_k_label(1)='G'
     SPECIAL_k%pt(2,:)=(/0.5,0.5,0./)
     SPECIAL_k_label(2)='N'
     SPECIAL_k%pt(3,:)=(/0.5,0.5,0.5/)
     SPECIAL_k_label(3)='P'
     SPECIAL_k%pt(4,:)=(/0. ,1.  ,0.  /)
     SPECIAL_k_label(4)='H'
     !
   case('CUB')
     !
     trial_b(1,:)=(/ 1., 0., 0./)*2.*pi/alat(1)
     trial_b(2,:)=(/ 0., 1., 0./)*2.*pi/alat(1)
     trial_b(3,:)=(/ 0., 0., 1./)*2.*pi/alat(1)
     !
     SPECIAL_k%nbz=4
     !
     SPECIAL_k%pt(1,:)=(/0.,0.,0./)
     SPECIAL_k_label(1)='G'
     SPECIAL_k%pt(2,:)=(/0.5,0.,0./)
     SPECIAL_k_label(2)='X'
     SPECIAL_k%pt(3,:)=(/0.5,0.5,0./)
     SPECIAL_k_label(3)='M'
     SPECIAL_k%pt(4,:)=(/0.5,0.5,0.5/)
     SPECIAL_k_label(4)='R'
     !
     do ik=1,SPECIAL_k%nbz
       call c2a(b_in=trial_b,v_in=SPECIAL_k%pt(ik,:),mode='ka2c')
     enddo
     !
   case('HCP')
     !
     trial_b(1,:)=(/ 1.,-1./sqrt(3.), 0./)*2.*pi/alat(1)
     trial_b(2,:)=(/ 1., 1./sqrt(3.), 0./)*2.*pi/alat(1)
     trial_b(3,:)=(/ 0., 0.,          1./)*2.*pi/alat(3)
     !
     SPECIAL_k%nbz=6
     !
     SPECIAL_k%pt(1,:)=(/0.,0.,0./)
     SPECIAL_k_label(1)='G'
     SPECIAL_k%pt(2,:)=(/0. ,0.5,0./)
     SPECIAL_k_label(2)='M'
     SPECIAL_k%pt(3,:)=(/1./3.,1./3.,0./)
     SPECIAL_k_label(3)='K'
     SPECIAL_k%pt(4,:)=(/0. ,0.  ,0.5/)
     SPECIAL_k_label(4)='A'
     SPECIAL_k%pt(5,:)=(/0.,0.5,0.5/)
     SPECIAL_k_label(5)='L'
     SPECIAL_k%pt(6,:)=(/1./3.,1./3.,0.5/)
     SPECIAL_k_label(6)='H'
     !
     do ik=1,SPECIAL_k%nbz
       call c2a(b_in=trial_b,v_in=SPECIAL_k%pt(ik,:),mode='ka2c')
     enddo
     !
 end select
 !
 if (SPECIAL_k%nbz==0) then
   call warning('Unknown lattice unit cell')
   return
 endif
 !
 call string_split(PtsPath,Path_strings)
 !
 k_order=0
 n_lines=0
 do i_str=1,50
   do ik=1,SPECIAL_k%nbz
     if (SPECIAL_k_label(ik)==trim(Path_strings(i_str))) then
       n_lines=n_lines+1
       k_order(n_lines)=ik
     endif
   enddo
 enddo
 if (n_lines==0) n_lines=1
 CIRCUIT_k%nbz=n_path_pts* ( n_lines -1 )
 !
 if (CIRCUIT_k%nbz==0.and..not.l_high_sym_pts) return
 !
 if (CIRCUIT_k%nbz==0) then
   live_timing_is_on=.false.
   do ik=1,SPECIAL_k%nbz
     call K_transform(SPECIAL_k%pt(ik,:),'cc')
     write (dumb_ch,'(3f12.7,2x,a)') SPECIAL_k%pt(ik,:),SPECIAL_k_label(ik)
     call msg("s",trim(dumb_ch))
   enddo
   live_timing_is_on=.true.
 else
   !
   call msg('s','Found '//trim(intc(n_lines-1))//' segments and  '//&
&                trim(intc(CIRCUIT_k%nbz+1))//' points along the path: '//trim(PtsPath))
   !
   YAMBO_ALLOC(CIRCUIT_k%pt,(CIRCUIT_k%nbz+1,4))
   allocate(K_path_label(CIRCUIT_k%nbz+1))
   CIRCUIT_k%nbz=0
   K_norm=0.
   K_path_label=" "
   do il=1,n_lines-1
     do ip=1,n_path_pts
       CIRCUIT_k%nbz= CIRCUIT_k%nbz+1
       if (ip==1) K_path_label(CIRCUIT_k%nbz)=SPECIAL_k_label( k_order(il) )
       CIRCUIT_k%pt(CIRCUIT_k%nbz,:3)=SPECIAL_k%pt(k_order(il),:)+ ( SPECIAL_k%pt(k_order(il+1),:) -  &
&                                     SPECIAL_k%pt(k_order(il),:) )/n_path_pts*(ip-1)
       if (CIRCUIT_k%nbz>1) K_norm=K_norm+v_norm(  CIRCUIT_k%pt(CIRCUIT_k%nbz,:3) -  CIRCUIT_k%pt(CIRCUIT_k%nbz-1,:3) )
       CIRCUIT_k%pt(CIRCUIT_k%nbz,4)= K_norm
     enddo
   enddo
   CIRCUIT_k%nbz= CIRCUIT_k%nbz+1
   CIRCUIT_k%pt(CIRCUIT_k%nbz,:3)=SPECIAL_k%pt(k_order(n_lines),:)
   K_path_label(CIRCUIT_k%nbz)=SPECIAL_k_label( k_order(n_lines) )
   K_norm=K_norm+v_norm(  CIRCUIT_k%pt(CIRCUIT_k%nbz,:3) -  CIRCUIT_k%pt(CIRCUIT_k%nbz-1,:3) )
   CIRCUIT_k%pt(CIRCUIT_k%nbz,4)= K_norm
   !
   if (l_high_sym_pts) then
     k_f_name(1)="path_points"
     k_f_name(2)="PWSCF_points"
     call of_open_close(k_f_name(1),'ot')
     call of_open_close(k_f_name(2),'ot')
     call msg('o  path',"#",(/"c1    ","c2    ","c3    ","Norm  "/),INDENT=0,USE_TABS=.true.)
     call msg('o PWSCF',"#",(/"c1    ","c2    ","c3    ","Weight"/),INDENT=0,USE_TABS=.true.)
     call msg('o path PWSCF',"#"," ",INDENT=0)
   endif
   !
   do ik=1,CIRCUIT_k%nbz
     if (.not.l_high_sym_pts) coo_out="iku" 
     call K_transform(CIRCUIT_k%pt(ik,:3),'cc')
     if (l_high_sym_pts) then
       call msg('o  path',"",CIRCUIT_k%pt(ik,:),INDENT=0,USE_TABS=.true.,formatted_msg=of_string)
       call msg('o PWSCF',"",(/CIRCUIT_k%pt(ik,:3),1._SP/),INDENT=0,USE_TABS=.true.,formatted_msg=of_string)
       dumb_ch=of_string
       if (len_trim(K_path_label(ik))>0) write (dumb_ch,'(a,5x,3a)') trim(of_string),'[', K_path_label(ik),']'
       call msg_deliver(dumb_ch)
     endif
   enddo
   !
   if (l_high_sym_pts) then
     call of_open_close(k_f_name(1))
     call of_open_close(k_f_name(2))
   endif
   !
   deallocate(K_path_label)
   !
 endif
 !
end subroutine
