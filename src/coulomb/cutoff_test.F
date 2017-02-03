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
subroutine cutoff_test(cut_is_sphere,cut_is_box,cut_is_cyl,is_cut,q)
 !
 use pars,          ONLY:SP,schlen,pi
 use vec_operate,   ONLY:v_norm
 use D_lattice,     ONLY:alat,sop_inv,a,DL_vol
 use R_lattice,     ONLY:bz_samp,cyl_ph_radius,bare_qpg,cyl_length,box_length,&
&                        g_vec,g_rot
 use wave_func,     ONLY:wf_ng
 use com,           ONLY:msg,of_open_close
 use LIVE_t,        ONLY:live_timing
 use parallel_m,    ONLY:PP_redux_wait,PP_indexes,myid,PP_indexes_reset
 use interfaces,    ONLY:PARALLEL_index
 implicit none
 type(bz_samp):: q
 logical      :: cut_is_sphere,cut_is_box,cut_is_cyl,is_cut(3)
 !
 ! Work Space
 !
 integer, parameter :: n_dl_pts=24 ! NO EVEN NUMBERS HERE !
 real(SP),parameter :: plot_factor=1.2
 character(schlen)  :: of_name(4)
 real(SP):: pl_xy_pt(n_dl_pts**2,3),v_xy(n_dl_pts**2,2),&
&           pl_yz_pt(n_dl_pts**2,3),v_yz(n_dl_pts**2,2),&
&           pl_xz_pt(n_dl_pts**2,3),v_xz(n_dl_pts**2,2),&
&           dummy(3),max_val
 integer :: iqibz,iqbz,ig,ir1,ir2,ip,ig_r,is
 type(PP_indexes) ::px
 !
 ! CutOff Test
 !
 pl_xy_pt=0.
 pl_yz_pt=0.
 pl_xz_pt=0.
 v_xy=0.
 v_yz=0.
 v_xz=0.
 call PP_indexes_reset(px)
 !
 ! DL points along the 3 possible planes
 !
 dummy=(/v_norm(a(1,:)),v_norm(a(2,:)),v_norm(a(3,:))/)
 !
 if (cut_is_sphere) dummy=2.*plot_factor*cyl_ph_radius
 !
 if (cut_is_cyl) then
   dummy=(/v_norm(a(1,:)),v_norm(a(2,:)),v_norm(a(3,:))/)
   if (is_cut(1).and.cyl_length==0) dummy(1)=2._SP*plot_factor*dummy(1)*10
   if (is_cut(2).and.cyl_length==0) dummy(2)=2._SP*plot_factor*dummy(2)*10
   if (is_cut(3).and.cyl_length==0) dummy(3)=2._SP*plot_factor*dummy(3)*10
 endif
 if (cut_is_box) then
   if (is_cut(1)) dummy(1)=2._SP*plot_factor*box_length(1)
   if (is_cut(2)) dummy(2)=2._SP*plot_factor*box_length(2)
   if (is_cut(3)) dummy(3)=2._SP*plot_factor*box_length(3)
 endif
 !
 do ir1=1,n_dl_pts
   do ir2=1,n_dl_pts
     ip=n_dl_pts*(ir1-1)+ir2
     pl_xy_pt(ip,:)=-((/dummy(1)/2,dummy(2)/2,0._SP   /)-&
&      (/dummy(1)*(ir1-1)/(n_dl_pts-1),dummy(2)*(ir2-1)/(n_dl_pts-1),0._SP/))
     pl_yz_pt(ip,:)=-((/0._SP    ,dummy(2)/2.,dummy(3)/2./)-&
&      (/0._SP,dummy(2)*(ir1-1)/(n_dl_pts-1),dummy(3)*(ir2-1)/(n_dl_pts-1)/))
     pl_xz_pt(ip,:)=-((/dummy(1)/2.,0._SP    ,dummy(3)/2./)-&
&      (/dummy(1)*(ir1-1)/(n_dl_pts-1),0._SP,dummy(3)*(ir2-1)/(n_dl_pts-1)/))
    
     if (cut_is_sphere) then
       v_xy(ip,1)=V_in_sphere( pl_xy_pt(ip,:) )
       v_yz(ip,1)=V_in_sphere( pl_yz_pt(ip,:) )
       v_xz(ip,1)=V_in_sphere( pl_xz_pt(ip,:) )
     endif
     !
     if (cut_is_box) then
       v_xy(ip,1)=V_in_box( pl_xy_pt(ip,:) )
       v_yz(ip,1)=V_in_box( pl_yz_pt(ip,:) )
       v_xz(ip,1)=V_in_box( pl_xz_pt(ip,:) )
     endif
     if (cut_is_cyl) then
       v_xy(ip,1)=V_in_cyl( pl_xy_pt(ip,:) )
       v_yz(ip,1)=V_in_cyl( pl_yz_pt(ip,:) )
       v_xz(ip,1)=V_in_cyl( pl_xz_pt(ip,:) )
     endif
   enddo
 enddo
 !
 ! CutOffed potential using qpg_bare
 !
 call k_ibz2bz(q,'i',.true.)
 !
 call PARALLEL_index(px,(/n_dl_pts**2/))
 call live_timing('CutOff Test',px%n_of_elements(myid+1))
 !
 do ip=1,n_dl_pts**2
   !
   if (.not.px%element_1D(ip)) cycle 
   !
   do iqbz=1,q%nbz
     do ig=1,wf_ng
       !
       iqibz = q%sstar(iqbz,1)
       is    = q%sstar(iqbz,2)
       ig_r  = g_rot( sop_inv(is), ig )
       !
       dummy(1)=dot_product( (q%ptbz(iqbz,:)+g_vec(ig,:))*2.*pi/alat(:), pl_xy_pt(ip,:))
       dummy(2)=dot_product( (q%ptbz(iqbz,:)+g_vec(ig,:))*2.*pi/alat(:), pl_yz_pt(ip,:))
       dummy(3)=dot_product( (q%ptbz(iqbz,:)+g_vec(ig,:))*2.*pi/alat(:), pl_xz_pt(ip,:))
       !
       v_xy(ip,2)=v_xy(ip,2)+Exp( cmplx(0.,dummy(1)) )*4.*pi/bare_qpg(iqibz,ig_r)**2.
       v_yz(ip,2)=v_yz(ip,2)+Exp( cmplx(0.,dummy(2)) )*4.*pi/bare_qpg(iqibz,ig_r)**2.
       v_xz(ip,2)=v_xz(ip,2)+Exp( cmplx(0.,dummy(3)) )*4.*pi/bare_qpg(iqibz,ig_r)**2.
       !
     enddo
     !
   enddo
   !
   v_xy(ip,2)=v_xy(ip,2)/DL_vol/real(q%nbz)
   v_yz(ip,2)=v_yz(ip,2)/DL_vol/real(q%nbz)
   v_xz(ip,2)=v_xz(ip,2)/DL_vol/real(q%nbz)
   !
   call live_timing(steps=1)
   !
 enddo
 !
 call PP_redux_wait(v_xy(:,2))
 call PP_redux_wait(v_yz(:,2))
 call PP_redux_wait(v_xz(:,2))
 !
 call live_timing()
 !
 ! Output Files
 !
 of_name(1)='xy_plane'
 of_name(2)='yz_plane'
 of_name(3)='xz_plane'
 of_name(4)='RL_components'
 call of_open_close(of_name(1),'ot')
 call msg('o xy','#')
 call msg('o xy','#',(/'X [au]','Y [au]','V_Dlat','V_Rlat'/),INDENT=0,USE_TABS=.true.)
 call msg('o xy','#')
 call of_open_close(of_name(2),'ot')
 call msg('o yz','#')
 call msg('o yz','#',(/'Y [au]','Z [au]','V_Dlat','V_Rlat'/),INDENT=0,USE_TABS=.true.)
 call msg('o yz','#')
 call of_open_close(of_name(3),'ot')
 call msg('o xz','#')
 call msg('o xz','#',(/'X [au]','Z [au]','V_Dlat','V_Rlat'/),INDENT=0,USE_TABS=.true.)
 call msg('o xz','#')
 call of_open_close(of_name(4),'ot')
 call msg('o RL_','#')
 call msg('o RL_','#',(/'|q+G| [au]',' V_o  [au]',' V_c  [au]'/),INDENT=0,USE_TABS=.true.)
 call msg('o RL_','#')
 !
 do ip=1,n_dl_pts**2
   call msg('o xy','',(/pl_xy_pt(ip,1),pl_xy_pt(ip,2),v_xy(ip,:)/),INDENT=-2,USE_TABS=.true.)
   call msg('o xz','',(/pl_xz_pt(ip,1),pl_xz_pt(ip,3),v_xz(ip,:)/),INDENT=-2,USE_TABS=.true.)
   call msg('o yz','',(/pl_yz_pt(ip,2),pl_yz_pt(ip,3),v_yz(ip,:)/),INDENT=-2,USE_TABS=.true.)
 enddo
 !
 max_val=0.
 !
 do iqbz=1,q%nbz
   do ig=1,wf_ng
     !
     iqibz = q%sstar(iqbz,1)
     is    = q%sstar(iqbz,2)
     ig_r  = g_rot( sop_inv(is), ig )
     !
     dummy(1)=v_norm( (q%ptbz(iqbz,:)+g_vec(ig,:))*2.*pi/alat(:) )
     dummy(2)=0.
     if (iqbz/=1.or.ig/=1) dummy(2)=4.*pi/dummy(1)**2.
     dummy(3)=4.*pi/bare_qpg(iqibz,ig_r)**2.
     !
     if (dummy(2)>max_val) max_val=dummy(2)
     !
     if (dummy(2)<max_val/100) cycle
     !
     call msg('o RL_','',dummy,INDENT=-2,USE_TABS=.true.)
     !
   enddo
   !
 enddo
 !
 call of_open_close(of_name(1))
 call of_open_close(of_name(2))
 call of_open_close(of_name(3))
 call of_open_close(of_name(4))
 !
 call k_ibz2bz(q,'d',.false.)
 !
 contains
   !
   real(SP) function V_in_sphere(r)
     real(SP) :: r(3)
     V_in_sphere=0.
     if (v_norm(r)<cyl_ph_radius.and.v_norm(r)>0.1) V_in_sphere=1./v_norm(r)
   end function
   !
   real(SP) function V_in_box(r)
     real(SP) :: r(3)
     V_in_box=0.
     if ( (.not.is_cut(1).or.abs(r(1))<box_length(1)/2.).and.&
&       (.not.is_cut(2).or.abs(r(2))<box_length(2)/2.).and.&
&       (.not.is_cut(3).or.abs(r(3))<box_length(3)/2.)) V_in_box=1./v_norm(r)
   end function

   real(SP) function V_in_cyl(r)
     real(SP) :: r(3)
     V_in_cyl=0.
     if (cyl_length>0) then 
       if ( (is_cut(1).and.abs(r(1))<cyl_length.and.abs(r(2))**2+abs(r(3))**2<cyl_ph_radius**2).or.&
&           (is_cut(2).and.abs(r(2))<cyl_length.and.abs(r(1))**2+abs(r(3))**2<cyl_ph_radius**2).or.&
&           (is_cut(3).and.abs(r(3))<cyl_length.and.abs(r(1))**2+abs(r(2))**2<cyl_ph_radius**2) ) &
&       V_in_cyl=1./v_norm(r)
     else
       if ( (is_cut(1).and.abs(r(2))**2+abs(r(3))**2<cyl_ph_radius**2).or.&
&           (is_cut(2).and.abs(r(1))**2+abs(r(3))**2<cyl_ph_radius**2).or.&
&           (is_cut(3).and.abs(r(1))**2+abs(r(2))**2<cyl_ph_radius**2)) V_in_cyl=1./v_norm(r)
     endif
   end function
 !
end subroutine
