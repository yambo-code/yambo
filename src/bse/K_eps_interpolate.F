!
! License-Identifier: GPL
!
! Copyright (C) 2007 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine K_eps_interpolate(nW,W,err)
 !
 use pars,         ONLY:SP,cI
 use vec_operate,  ONLY:sort
 use X_m,          ONLY:Epsilon_ii
 use POL_FIT_m,    ONLY:POL_LK_fit
 !
 implicit none
 !
 integer  :: nW,err(nW)
 real(SP) :: W(nW)
 !
 ! Work Space...
 !
 integer, parameter :: order_max=3,n_pts_max=5
 integer    ::iw,iw_fit,n_pts,order,i1,fit_pt(n_pts_max)
 real(SP)   ::EPS,Dy(n_pts_max),Dx(n_pts_max),Pol_coeff(order_max+1)
 !
 w_loop: do iw=1,nW
   if (err(iw)/=0) then
     n_pts=0
     fit_pt=0
     do iw_fit=iw-1,1,-1
       if (err(iw_fit)/=0) cycle
       n_pts=n_pts+1
       fit_pt(n_pts)=iw_fit
       if (n_pts==n_pts_max/2) exit
     enddo
     do iw_fit=iw+1,nW
       if (err(iw_fit)/=0) cycle
       n_pts=n_pts+1
       fit_pt(n_pts)=iw_fit
       if (n_pts==n_pts_max) exit
     enddo
     call sort(fit_pt(:n_pts))
     !
     do i1=1,2
       order=order_max
       if (n_pts<n_pts_max) order=n_pts-2
       if (order<=0) cycle w_loop
       !
       Dx=0._SP
       Dy=0._SP
       Pol_coeff=0._SP
       do iw_fit=1,n_pts
         Dx(iw_fit)=W( fit_pt(iw_fit) )-W(iw)
         if (i1==1) Dy(iw_fit)=aimag( Epsilon_ii(fit_pt(iw_fit),2) )
         if (i1==2) Dy(iw_fit)= real( Epsilon_ii(fit_pt(iw_fit),2) )
       enddo
       call POL_LK_fit(n_pts,Dx(:n_pts),Dy(:n_pts),order,Pol_coeff(:order+1),EPS,0._SP)
       !
       if (i1==1) Epsilon_ii(iw,2)=cI*pol_eval(0._SP)
       if (i1==2) Epsilon_ii(iw,2)=Epsilon_ii(iw,2)+pol_eval(0._SP)
       !
     enddo
     !
   endif
 enddo w_loop
 !
 contains
   !
   real(SP) function pol_eval(at_w)
     !
     real(SP) ::at_w
     integer  ::ip
     !
     pol_eval=0.
     do ip=1,order+1
       pol_eval=pol_eval+at_w**(ip-1)*Pol_coeff(ip)
     enddo
     !
   end function
   !
end subroutine
