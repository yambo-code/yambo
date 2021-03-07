!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM DV
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
subroutine plot_check_and_launch(check_only)
 !
 use pars,        ONLY:SP
 use YPPm,        ONLY:p_format,p_dir,plot_dim,use_gnuplot,use_xcrysden,use_cube,&
&                      plot_is_1D,plot_is_2D,plot_is_3D
 use D_lattice,   ONLY:a
 use vec_operate, ONLY:v_is_zero
 implicit none
 !
 logical :: check_only
 !
 ! Work Space
 !
 real(SP):: dp(3)
 logical :: switch_to_gnuplot,switch_to_xcrysden
 !
 ! Dot products to check orthogonality issues
 !
 dp(1)=dot_product(a(1,:),a(2,:))
 dp(2)=dot_product(a(1,:),a(3,:))
 dp(3)=dot_product(a(2,:),a(3,:))
 !
 use_gnuplot = (p_format=='g')
 use_xcrysden= (p_format=='x')
 use_cube    = (p_format=='c')
 !
 switch_to_xcrysden =.false.
 switch_to_gnuplot  =.false.
 !
 if (.not.use_gnuplot.and..not.use_xcrysden.and..not.use_cube) then
   p_format='g'
   use_gnuplot=.true.
 endif
 !
 plot_is_1D(1) =p_dir(:1)=='1'
 plot_is_1D(2) =p_dir(:1)=='2'
 plot_is_1D(3) =p_dir(:1)=='3'
 plot_is_2D(1) =p_dir(:2)=='12'.or.p_dir(:2)=='21'
 plot_is_2D(2) =p_dir(:2)=='13'.or.p_dir(:2)=='31'
 plot_is_2D(3) =p_dir(:2)=='23'.or.p_dir(:2)=='32'
 plot_is_3D    =p_dir=='123'
 !
 if (plot_is_3D) then
   plot_dim=3
   plot_is_1D=.false.
   plot_is_2D=.false.
   if (.not.v_is_zero(dp).and.use_gnuplot) then
     if (check_only) call warning(' Cannot average along non orthogonal directions')
     switch_to_xcrysden=.true.
   endif
 else if (any(plot_is_2D)) then
   plot_dim=2
   plot_is_1D=.false.
   plot_is_3D=.false.
   if (plot_is_2D(1).and..not.v_is_zero((/0._SP,dp(2),dp(3)/)).and.use_gnuplot) then
     call warning(' Cannot average along non orthogonal direction')
     switch_to_xcrysden=.true.
   endif
   if (plot_is_2D(2).and..not.v_is_zero((/dp(1),0._SP,dp(3)/)).and.use_gnuplot) then
     call warning(' Cannot average along non orthogonal direction')
     switch_to_xcrysden=.true.
   endif
   if (plot_is_2D(3).and..not.v_is_zero((/dp(1),dp(2),0._SP/)).and.use_gnuplot) then
     call warning(' Cannot average along non orthogonal direction')
     switch_to_xcrysden=.true.
   endif
 else if (any(plot_is_1D)) then
   plot_dim=1
   plot_is_2D=.false.
   plot_is_3D=.false.
 else
   plot_dim=3
   p_dir='123'
 endif
 !
 if (plot_dim==1.and.use_xcrysden) switch_to_gnuplot =.true.
 if (plot_dim==1.and.use_cube)     switch_to_gnuplot =.true.
 if (plot_dim==2.and.use_cube)     switch_to_gnuplot =.true.
 if (plot_dim==3.and.use_gnuplot)  switch_to_xcrysden=.true.
 !
 if (switch_to_xcrysden) then
   if(check_only) call warning(' Switching to XCrySDen')
   p_format='x'
   use_xcrysden=.true.
   use_gnuplot =.false.
   use_cube    =.false.
 endif
 if (switch_to_gnuplot ) then
   if(check_only) call warning(' Switching to GnuPlot')
   p_format='g'
   use_xcrysden=.false.
   use_gnuplot =.true.
   use_cube    =.false.
 endif
 !
 if (check_only) return
 !
 if (use_gnuplot)   call plot_gnuplot(.FALSE.)
 if (use_cube)      call plot_cube()
 if (use_xcrysden) then
   if (plot_dim==2) call plot_gnuplot(.TRUE.)
   call plot_xcrysden()
 endif
 !
end subroutine
