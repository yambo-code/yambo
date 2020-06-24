!
!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine k_special(LIST)
 !
 use pars,                ONLY:SP,schlen,lchlen
 use units,               ONLY:pi
 use R_lattice,           ONLY:bz_samp_reset,b
 use D_lattice,           ONLY:alat,lattice,a
 use com,                 ONLY:msg,of_open_close,msg_deliver,warning
 use vec_operate,         ONLY:c2a,v_norm
 use YPP,                 ONLY:n_path_pts,PtsPath,K_transform,l_high_sym_pts,&
&                              SPECIAL_k,n_SPECIAL_k_MAX,CIRCUIT_k,SPECIAL_k_label,&
&                              n_SPECIAL_k_MAX,CIRCUIT_k_label,coo_out
 use stderr,              ONLY:STRING_split,STRING_match
 use LIVE_t,              ONLY:live_timing_is_on
 use stderr,              ONLY:intc
 !
#include<memory.h>
 !
 logical :: LIST
 !
 ! Work Space
 ! 
 integer          :: ik,il,n_lines,ip,i_str,K_position(n_SPECIAL_k_MAX)
 real(SP)         :: trial_b(3,3),K_norm,ap,bp,cp,fp,ep,K_sign(n_SPECIAL_k_MAX),k_l(3),k_lp1(3)
 character(schlen):: Path_strings(50),dumb_ch,k_f_name(2)
 character(lchlen):: of_string
 !
 call bz_samp_reset(SPECIAL_k)
 call bz_samp_reset(CIRCUIT_k)
 !
 YAMBO_ALLOC(SPECIAL_k%pt,(n_SPECIAL_k_MAX,3))
 !
 ! Special K taken from 
 ! 
 !  http://cst-www.nrl.navy.mil/bind/kpts/index.html
 !  http://lamp.tu-graz.ac.at/
 !
 select case (trim(lattice))
   !
   case('FCC')
     !
     trial_b(1,:)=(/-1., 1., 1./)*2.*pi/alat(1)
     trial_b(2,:)=(/ 1.,-1., 1./)*2.*pi/alat(1)
     trial_b(3,:)=(/ 1., 1.,-1./)*2.*pi/alat(1)
     !
     ! 2019/02/01:
     ! If yambo "b" is different
     ! i.e. in case for example
     ! b(1,:)=(/-0.5, 0.5, 0.5/)*2.*pi/alat(1)
     ! b(2,:)=(/ 0.5,-0.5.,0.5/)*2.*pi/alat(1)
     ! b(3,:)=(/ 0.5, 0.5,-0.5/)*2.*pi/alat(1)
     ! to properly detect special k-points
     ! uncomment the following line
     !
     !trial_b=b
     !
     SPECIAL_k%nbz=6
     !
     SPECIAL_k%pt(1,:)=(/0._SP,0._SP,0._SP/)
     SPECIAL_k_label(1)='G'
     SPECIAL_k%pt(2,:)=(/0.5_SP,0.5_SP,0._SP/)
     SPECIAL_k_label(2)='X'
     SPECIAL_k%pt(3,:)=(/0.5_SP,0.5_SP,0.5_SP/)
     SPECIAL_k_label(3)='L'
     SPECIAL_k%pt(4,:)=(/0.5_SP,0.75_SP,0.25_SP/)
     SPECIAL_k_label(4)='W'
     SPECIAL_k%pt(5,:)=(/0.375_SP,0.375_SP,0.750_SP/)
     SPECIAL_k_label(5)='K'
     SPECIAL_k%pt(6,:)=(/0.375_SP,0.375_SP,0.750_SP/)
     SPECIAL_k_label(6)='U'
     !
     do ik=1,SPECIAL_k%nbz
       call c2a(b_in=trial_b,v_in=SPECIAL_k%pt(ik,:),mode='ka2c')
     enddo
     !
   case('BCC')
     !
     trial_b(1,:)=(/ 0._SP, 1._SP, 1._SP/)*2._SP*pi/alat(1)
     trial_b(2,:)=(/ 1._SP, 0._SP, 1._SP/)*2._SP*pi/alat(1)
     trial_b(3,:)=(/ 1._SP, 0._SP,-1._SP/)*2._SP*pi/alat(1)
     !
     SPECIAL_k%nbz=4
     !
     SPECIAL_k%pt(1,:)=(/0._SP,0._SP,0._SP/)
     SPECIAL_k_label(1)='G'
     SPECIAL_k%pt(2,:)=(/0.5_SP,0.5_SP,0._SP/)
     SPECIAL_k_label(2)='N'
     SPECIAL_k%pt(3,:)=(/0.5_SP,0.5_SP,0.5_SP/)
     SPECIAL_k_label(3)='P'
     SPECIAL_k%pt(4,:)=(/0._SP ,1._SP  ,0._SP  /)
     SPECIAL_k_label(4)='H'
     !
   case('FC-ORTHOROMBIC (b<a)')
     !
     ap=pi/a(1,1)
     bp=pi/a(1,2)
     cp=pi/a(3,3)
     !
     call orthorombic_lengths(bp,ap)
     !
     SPECIAL_k%nbz=10
     !
     SPECIAL_k%pt(1,:)=(/0._SP,0._SP,0._SP/)
     SPECIAL_k_label(1)='G'
     SPECIAL_k%pt(2,:)=SPECIAL_k%pt(1,:)+(/0._SP,0._SP,cp/)
     SPECIAL_k_label(2)='Z'
     !
     SPECIAL_k%pt(3,:)=(/ap,0._SP,0._SP/)
     SPECIAL_k_label(3)='X'
     SPECIAL_k%pt(4,:)=SPECIAL_k%pt(3,:)+(/0._SP,0._SP,cp/)
     SPECIAL_k_label(4)='A'
     !
     SPECIAL_k_label(5)='Y'
     SPECIAL_k%pt(5,:)=(/0._SP,fp,0._SP/)
     SPECIAL_k_label(6)='T'
     SPECIAL_k%pt(6,:)=SPECIAL_k%pt(5,:)+(/0._SP,0._SP,cp/)
     !
     SPECIAL_k_label(7)='S'
     SPECIAL_k%pt(7,:)=(/ap/2._SP,bp/2._SP,0._SP/)
     SPECIAL_k_label(8)='R'
     SPECIAL_k%pt(8,:)=SPECIAL_k%pt(7,:)+(/0._SP,0._SP,cp/)
     !
     SPECIAL_k_label(9)='X1'
     SPECIAL_k%pt(9,:)=(/ap,ep,0._SP/)
     SPECIAL_k_label(10)='A1'
     SPECIAL_k%pt(10,:)=SPECIAL_k%pt(9,:)+(/0._SP,0._SP,cp/)
     !
   case('FC-ORTHOROMBIC (b>a)')
     !
     ap=pi/a(1,1)
     bp=pi/a(1,2)
     cp=pi/a(3,3)
     !
     call orthorombic_lengths(ap,bp)
     !
     SPECIAL_k%nbz=10
     !
     SPECIAL_k%pt(1,:)=(/0._SP,0._SP,0._SP/)
     SPECIAL_k_label(1)='G'
     SPECIAL_k%pt(2,:)=SPECIAL_k%pt(1,:)+(/0._SP,0._SP,cp/)
     SPECIAL_k_label(2)='Z'
     !
     SPECIAL_k%pt(3,:)=(/fp,0._SP,0._SP/)
     SPECIAL_k_label(3)='X'
     SPECIAL_k%pt(4,:)=SPECIAL_k%pt(3,:)+(/0._SP,0._SP,cp/)
     SPECIAL_k_label(4)='A'
     !
     SPECIAL_k_label(5)='Y'
     SPECIAL_k%pt(5,:)=(/0._SP,bp,0._SP/)
     SPECIAL_k_label(6)='T'
     SPECIAL_k%pt(6,:)=SPECIAL_k%pt(5,:)+(/0._SP,0._SP,cp/)
     !
     SPECIAL_k_label(7)='S'
     SPECIAL_k%pt(7,:)=(/ap/2._SP,bp/2._SP,0._SP/)
     SPECIAL_k_label(8)='R'
     SPECIAL_k%pt(8,:)=SPECIAL_k%pt(7,:)+(/0._SP,0._SP,cp/)
     !
     SPECIAL_k_label(9)='X1'
     SPECIAL_k%pt(9,:)=(/ep,bp,0._SP/)
     SPECIAL_k_label(10)='A1'
     SPECIAL_k%pt(10,:)=SPECIAL_k%pt(9,:)+(/0._SP,0._SP,cp/)
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
     SPECIAL_k%nbz=7
     !
     SPECIAL_k%pt(1,:)=(/0.,0.,0./)
     SPECIAL_k_label(1)='G'
     SPECIAL_k%pt(2,:)=(/0. ,0.5,0./)
     SPECIAL_k_label(2)='M'
     SPECIAL_k%pt(3,:)=(/2./3.,0.,0./)
     SPECIAL_k%pt(3,:)=(/1./3.,1./3.,0./)
     SPECIAL_k_label(3)='K'
     SPECIAL_k%pt(4,:)=(/-1./3.,-1./3.,0./)
     SPECIAL_k_label(4)='Kp'
     SPECIAL_k%pt(5,:)=(/0. ,0.  ,0.5/)
     SPECIAL_k_label(5)='A'
     SPECIAL_k%pt(6,:)=(/0.,0.5,0.5/)
     SPECIAL_k_label(6)='L'
     SPECIAL_k%pt(7,:)=(/1./3.,1./3.,0.5/)
     SPECIAL_k_label(7)='H'
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
 ! Report 
 !
 if (l_high_sym_pts.or.LIST) then
   call section("p","Special K-points list ["//trim(coo_out)//"]")
   live_timing_is_on=.false.
   do ik=1,SPECIAL_k%nbz
     k_l=SPECIAL_k%pt(ik,:)
     call K_transform(k_l,'cc')
     write (dumb_ch,'(3f12.7,2x,a)') k_l,SPECIAL_k_label(ik)
     call msg("s",trim(dumb_ch))
   enddo
   live_timing_is_on=.true.
 endif
 !
 if (LIST) then
   YAMBO_FREE(SPECIAL_k%pt)
   return
 endif
 !
 ! Path builder
 !
 call STRING_split(PtsPath,Path_strings)
 !
 K_position=0
 K_sign    =1._SP
 n_lines=0
 do i_str=1,50
   do ik=1,SPECIAL_k%nbz
     if ( STRING_match(trim(Path_strings(i_str)),trim(SPECIAL_k_label(ik))) ) then
       n_lines=n_lines+1
       K_position(n_lines)=ik
     endif
     if ( STRING_match(trim(Path_strings(i_str)),trim("-"//SPECIAL_k_label(ik))) ) then
       n_lines=n_lines+1
       K_sign(n_lines)=-1.
       K_position(n_lines)=ik
     endif
   enddo
 enddo
 if (n_lines==0) n_lines=1
 !
 if (n_lines==1.and..not.l_high_sym_pts) then
   !
   do ik=1,SPECIAL_k%nbz
     call c2a(v_in=SPECIAL_k%pt(ik,:),mode='kc2i')
   enddo
   !
   return
   !
 endif
 !
 CIRCUIT_k%nbz=n_path_pts* ( n_lines -1 )
 !
 if (CIRCUIT_k%nbz>0) then
   !
   call msg('s','Found '//trim(intc(n_lines-1))//' segments and  '//&
&                trim(intc(CIRCUIT_k%nbz+1))//' points along the path: '//trim(PtsPath))
   !
   YAMBO_ALLOC(CIRCUIT_k%pt,(CIRCUIT_k%nbz+1,4))
   allocate(CIRCUIT_k_label(CIRCUIT_k%nbz+1))
   CIRCUIT_k%nbz=0
   K_norm=0.
   CIRCUIT_k_label=" "
   do il=1,n_lines-1
     k_l  =SPECIAL_k%pt( K_position(il)  ,:)*K_sign(il)
     k_lp1=SPECIAL_k%pt( K_position(il+1),:)*K_sign(il+1)
     do ip=1,n_path_pts
       CIRCUIT_k%nbz= CIRCUIT_k%nbz+1
       if (ip==1.and.K_sign(il)>0.) CIRCUIT_k_label(CIRCUIT_k%nbz)=SPECIAL_k_label( K_position(il) )
       if (ip==1.and.K_sign(il)<0.) CIRCUIT_k_label(CIRCUIT_k%nbz)="-"//SPECIAL_k_label( K_position(il) )
       CIRCUIT_k%pt(CIRCUIT_k%nbz,:3)=k_l+(k_lp1-k_l)/n_path_pts*(ip-1)
       if (CIRCUIT_k%nbz>1) K_norm=K_norm+v_norm(  CIRCUIT_k%pt(CIRCUIT_k%nbz,:3) -  CIRCUIT_k%pt(CIRCUIT_k%nbz-1,:3) )
       CIRCUIT_k%pt(CIRCUIT_k%nbz,4)= K_norm
     enddo
   enddo
   CIRCUIT_k%nbz= CIRCUIT_k%nbz+1
   CIRCUIT_k%pt(CIRCUIT_k%nbz,:3)=SPECIAL_k%pt(K_position(n_lines),:)*K_sign(n_lines)
   if (k_sign(n_lines)>0.) CIRCUIT_k_label(CIRCUIT_k%nbz)=SPECIAL_k_label( K_position(n_lines) )
   if (k_sign(n_lines)<0.) CIRCUIT_k_label(CIRCUIT_k%nbz)="-"//SPECIAL_k_label( K_position(n_lines) )
   K_norm=K_norm+v_norm(  CIRCUIT_k%pt(CIRCUIT_k%nbz,:3) -  CIRCUIT_k%pt(CIRCUIT_k%nbz-1,:3) )
   CIRCUIT_k%pt(CIRCUIT_k%nbz,4)= K_norm
   !
   do ik=1,CIRCUIT_k%nbz
     call c2a(v_in=CIRCUIT_k%pt(ik,:),mode='kc2i')
   enddo
   do ik=1,SPECIAL_k%nbz
     call c2a(v_in=SPECIAL_k%pt(ik,:),mode='kc2i')
   enddo
   !
   if (l_high_sym_pts) then
     !
     k_f_name(1)="path_points"
     k_f_name(2)="PWSCF_points"
     call of_open_close(k_f_name(1),'ot')
     call of_open_close(k_f_name(2),'ot')
     call msg('o  path',"#",(/"c1    ","c2    ","c3    ","Norm  "/),INDENT=0,USE_TABS=.true.)
     call msg('o PWSCF',"#",(/"c1    ","c2    ","c3    ","Weight"/),INDENT=0,USE_TABS=.true.)
     call msg('o path PWSCF',"#"," ",INDENT=0)
     !
     do ik=1,CIRCUIT_k%nbz
       !
       call K_transform(CIRCUIT_k%pt(ik,:3),'iku')
       !
       call msg('o  path',"",CIRCUIT_k%pt(ik,:),INDENT=0,USE_TABS=.true.,formatted_msg=of_string)
       if (len_trim(CIRCUIT_k_label(ik))>0) of_string= trim(of_string)//' ['// CIRCUIT_k_label(ik)//']'
       call msg_deliver('path',trim(of_string))
       call msg('o PWSCF',"",(/CIRCUIT_k%pt(ik,:3),1._SP/),INDENT=0,USE_TABS=.true.,formatted_msg=of_string)
       if (len_trim(CIRCUIT_k_label(ik))>0) of_string= trim(of_string)//' ['// CIRCUIT_k_label(ik)//']'
       call msg_deliver('PWSCF',trim(of_string))
     enddo
     !
     YAMBO_FREE(SPECIAL_k%pt)
     YAMBO_FREE(CIRCUIT_k%pt)
     !
     call of_open_close(k_f_name(1))
     call of_open_close(k_f_name(2))
     !
   endif
   !
 endif
 !
 contains
   !
   subroutine orthorombic_lengths(xp,yp)
    !
    real(SP) :: dp,xp,yp
    !
    dp=sqrt(xp**2 + yp**2)/2._SP
    fp=2*dp**2/xp
    ep=xp-fp
    !
   end subroutine
   !
end subroutine
