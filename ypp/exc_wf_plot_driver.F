!
! Copyright (C) 2000-2008 D. Varsano, A. Marini and the YAMBO team
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
subroutine exc_wf_plot_driver(check_only)
 !
 use pars,        ONLY:SP
 use YPP,         ONLY:p_format,p_dir
 use com,         ONLY:warning
 use D_lattice,   ONLY:a
 use vec_operate, ONLY:v_is_zero
 implicit none
 !
 logical :: check_only
 !
 ! Work Space
 !
 integer :: plot_dim
 real(SP):: dp(3)
 logical :: use_gnuplot,use_xcrysden,use_1d(3),use_2d(3),use_3d,&
&           switch_to_gnuplot,switch_to_xcrysden
 !
 ! Dot products to check orthogonality issues
 !
 dp(1)=dot_product(a(1,:),a(2,:))
 dp(2)=dot_product(a(1,:),a(3,:))
 dp(3)=dot_product(a(2,:),a(3,:))
 !
 use_gnuplot=p_format=='g'
 use_xcrysden=p_format=='x'
 switch_to_xcrysden=.false.
 switch_to_gnuplot=.false.
 !
 if (.not.use_gnuplot.and..not.use_xcrysden) use_gnuplot=.true.
 !
 use_1d(1) =p_dir(:1)=='1'
 use_1d(2) =p_dir(:1)=='2'
 use_1d(3) =p_dir(:1)=='3'
 use_2d(1) =p_dir(:2)=='12'.or.p_dir(:2)=='21'
 use_2d(2) =p_dir(:2)=='13'.or.p_dir(:2)=='31'
 use_2d(3) =p_dir(:2)=='23'.or.p_dir(:2)=='32'
 use_3d    =p_dir=='123'
 !
 if (use_3d) then
   plot_dim=3
   use_1d=.false.
   use_2d=.false.
   if (.not.v_is_zero(dp).and.use_gnuplot) then
     if (check_only) call warning('Cannot average along non orthogonal directions')
     switch_to_xcrysden=.true.
   endif
 else if (any(use_2d)) then
   plot_dim=2
   use_1d=.false.
   use_3d=.false.
   if (use_2d(1).and..not.v_is_zero((/0.,dp(2),dp(3)/)).and.use_gnuplot) then
     call warning('Cannot average along non orthogonal direction')
     switch_to_xcrysden=.true.
   endif
   if (use_2d(2).and..not.v_is_zero((/dp(1),0.,dp(3)/)).and.use_gnuplot) then
     call warning('Cannot average along non orthogonal direction')
     switch_to_xcrysden=.true.
   endif
   if (use_2d(3).and..not.v_is_zero((/dp(1),dp(2),0./)).and.use_gnuplot) then
     call warning('Cannot average along non orthogonal direction')
     switch_to_xcrysden=.true.
   endif
 else if (any(use_1d)) then
   plot_dim=1
   use_2d=.false.
   use_3d=.false.
 else
   plot_dim=3
   p_dir='123'
 endif
 !
 if (plot_dim==1.and.use_xcrysden) switch_to_gnuplot=.true.
 if (plot_dim==3.and.use_gnuplot)  switch_to_xcrysden=.true.
 !
 if (switch_to_xcrysden.and.check_only) call warning('Switching to XCrySDen')
 if (switch_to_gnuplot.and.check_only)  call warning('Switching to GnuPlot')
 !
 if (switch_to_xcrysden) p_format='x'
 if (switch_to_gnuplot) p_format='p'
 !
 if (check_only) return
 !
 if (use_gnuplot)  call exc_wf_gnuplot(use_1d,use_2d,plot_dim,.FALSE.)
 if (use_xcrysden) then
   if (plot_dim==2) call exc_wf_gnuplot(use_1d,use_2d,plot_dim,.TRUE.)
   call exc_wf_xcrysden(use_2d,plot_dim)
 endif
 !
end subroutine
