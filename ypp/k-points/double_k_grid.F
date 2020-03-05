!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DK CA
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
function double_k_grid(Xk,nkpoint_f,order_ipol) result(err)
 !
 use pars,                ONLY:SP
 use R_lattice,           ONLY:bz_samp,k_grid
 use LIVE_t,              ONLY:live_timing
 use com,                 ONLY:msg
 use vec_operate,         ONLY:c2a
 !
#include<memory.h>
 integer                     :: err
 type(bz_samp),intent(inout) :: Xk
 integer,      intent(in)    :: nkpoint_f(3)
 integer,      intent(in)    :: order_ipol
 !
 ! Work Space
 ! 
 integer :: nstep_f(3)  ! all quantities on fine grid have f
 integer :: nkpoint_c(3)              ! on coarse have c
 real(SP) :: deltak_f(3), deltak_c(3)
 real(SP), allocatable :: kpts_c(:,:)
 real(SP) :: tmp_ipol_weight(3)
 integer :: nx, ny, nz, ii, ii_c
 integer :: npts_around_coarse, nx_c, ny_c, nz_c
 integer :: nx_period, ny_period, nz_period
 integer :: intvx, intvy, intvz, intvx_per, intvy_per, intvz_per
 integer :: stx, sty, stz
 integer :: pt_ipol
 integer :: nintervls, iintervls(2)
 !
 err = 1
 ! do tests on kgrid
 nkpoint_c = k_grid
 if(product(nkpoint_c) .ne. size(Xk%ptbz,1) .or. &
   product(nkpoint_c) .ne. Xk%nbz) then
   call msg("s",' ERROR kgrid') 
   err=2
   return
 endif
 !
 deltak_c = 1./nkpoint_c
 deltak_f = 1./nkpoint_f
 !
 ! the fine grid has to be an integer multiple of coarse grid
 do ii = 1, 3
   if(mod(nkpoint_f(ii),nkpoint_c(ii)) .ne. 0)then
     call msg('s','Fine grid points have to be an integer multiple of coarse grid.')
     err=2
     return
   end if
 end do
 !
 ! number of steps in fine grid between two coarse points
 do ii = 1, 3
   nstep_f(ii) =  int(nkpoint_f(ii)/nkpoint_c(ii)) 
 end do
 ! 
 ! 
 if(order_ipol + 1 .gt. minval(nkpoint_c))then
   call msg('s','Not enough knots to interpolate with order', order_ipol) 
   err=4
   return
 end if
 nintervls = order_ipol + 1
 !
 ! points on coarse grid, fine points included, have zero weight, unless the coarse 
 ! point around which the fine ones are sampled. 
 npts_around_coarse = product((nintervls*(nstep_f-1)+1))
 !
 ! start and end interval of interpolation knots relative to interval of point
 ! that has to be interpolated
 iintervls(1) = -ceiling(real(order_ipol)/2)
 iintervls(2) = iintervls(1) + order_ipol
 ! check of range of intervals
 do ii = 1,100
   if(abs(iintervls(1)) .eq. iintervls(2) + mod(order_ipol,2)) exit
   iintervls = iintervls + 1
 end do
 !
 ! bring (internal) coarse grid to interval [0,1)
 !
 YAMBO_ALLOC(kpts_c,(product(nkpoint_c),3))
 kpts_c=0
 ! conversion kpoints to reciprocal coord 
 !
 do ii=1,Xk%nbz
   call c2a(v_in=Xk%ptbz(ii,:),v_out=kpts_c(ii,:),mode='ki2a')
 enddo
 where(kpts_c .lt. 0.0) kpts_c = kpts_c + 1.0
 !
 ! prepare Double Grid quantities
 !
 YAMBO_ALLOC(Xk%bz_DbGd_map,(Xk%nbz, npts_around_coarse))
 Xk%bz_DbGd_map = 0
 YAMBO_ALLOC(Xk%weights_ipol,(npts_around_coarse))
 Xk%weights_ipol = 0.0_SP
 !
 !---------------------- SAMPLE POINTS (KNOTS) -----------------------------
 ! find kpoints of fine grid that correspond to those of coarse grid
 call live_timing('BZ Double Grid Tables',Xk%nbz*npts_around_coarse)
 !
 do ii_c = 1,Xk%nbz !size(kpts_c,1)
   ! indices of coarse points 
   nx_c = anint(kpts_c(ii_c,1)*nkpoint_c(1) + 1)
   ny_c = anint(kpts_c(ii_c,2)*nkpoint_c(2) + 1)
   nz_c = anint(kpts_c(ii_c,3)*nkpoint_c(3) + 1)
    
   ! all fine points around coarse point
   ii = 0
   do intvx = nx_c+iintervls(1), nx_c+iintervls(2)
     intvx_per = (modulo(intvx-1, nkpoint_c(1)))* nstep_f(1)
     if(intvx.eq.nx_c)then
       stx = 1
     else
       stx = 2
     end if
     do nx = stx,  nstep_f(1)
       nx_period = intvx_per + nx
       ! 
       do intvy = ny_c+iintervls(1), ny_c+iintervls(2)
         intvy_per = (modulo(intvy-1, nkpoint_c(2)))* nstep_f(2)
         if(intvy.eq.ny_c)then
           sty = 1
         else
           sty = 2
         end if
         do ny = sty,  nstep_f(2)
           ny_period = intvy_per + ny
           !
           do intvz = nz_c+iintervls(1), nz_c+iintervls(2)
             intvz_per = (modulo(intvz-1, nkpoint_c(3)))* nstep_f(3)
             if(intvz .eq. nz_c)then
               stz = 1
             else
               stz = 2
             end if
             do nz = stz,  nstep_f(3)
               nz_period = (modulo(intvz-1, nkpoint_c(3)))* nstep_f(3) + nz
               ! 
               ii = ii + 1
               Xk%bz_DbGd_map(ii_c,ii) = sub2ind(nkpoint_f, nx_period, ny_period, nz_period)
               call live_timing(steps=1)
             end do
           end do
         end do
       end do
     end do
   end do
   
   Xk%nbz_mapped(ii_c)=ii
 end do
 !
 call live_timing()  
 !
 !----------------- SAMPLE WEIGHTS ---------------------- 
 ! weight depends on relative distance of fine point to interpolated point 
 ! (ie. to coarse point around which fine points are sampled)
 ! choose intervals such that starting point is the first point on grid.  
 !
 call live_timing('BZ Double Grid ipol weights',npts_around_coarse)
 pt_ipol = 1 - iintervls(1)  
 ii = 0
 do intvx = 1, nintervls 
   if(intvx.eq.pt_ipol)then
     stx = 1
    else
     stx = 2
   end if
   do nx = stx,  nstep_f(1)
     tmp_ipol_weight(1) = polynomial_ipol_coeff(order_ipol,&
              intvx, pt_ipol, real(nx-1)/nstep_f(1))
     !
     do intvy = 1, nintervls 
       if (intvy .eq. pt_ipol) then
         sty = 1
       else
         sty = 2
       end if
       do ny = sty,  nstep_f(2)
         tmp_ipol_weight(2) = polynomial_ipol_coeff(order_ipol, &
                  intvy, pt_ipol, real(ny-1)/nstep_f(2))
         !
         do intvz = 1, nintervls 
           if(intvz .eq. pt_ipol)then
             stz = 1
           else
             stz = 2
           end if
           do nz = stz,  nstep_f(3)
             tmp_ipol_weight(3) = polynomial_ipol_coeff(order_ipol, &
                      intvz, pt_ipol, real(nz-1)/nstep_f(3))
             !
             ii = ii + 1
             Xk%weights_ipol(ii) = product(tmp_ipol_weight)
             !   
             call live_timing(steps=1)
             !
           end do
         end do
       end do
     end do
   end do
 end do
 !
 ! norm to define integral over product on coarse grid sum_m a_m*b_m*deltak_c
 ! later in construction of GreenF every contribution will be divided by npts_around_coarse,
 ! which is not wanted for dgrid here, has to be cured beforehand. 
 !
 Xk%weights_ipol = Xk%weights_ipol/product(nstep_f)*real(npts_around_coarse)
 !
 ! everything is ok 
 !
 err = 0
 !
 ! clean up
 !
 YAMBO_FREE(kpts_c)
 !
 contains
   !
   ! transforms subscript to linear index of 3D array
   function sub2ind(arrsize, ind1, ind2, ind3) result( linind)
    integer :: linind
    integer, intent(in) :: arrsize(3) 
    integer, intent(in) :: ind1, ind2, ind3

    linind = (ind1 - 1)*arrsize(2)*arrsize(3) + (ind2 - 1)*arrsize(3) + ind3
  
   end function sub2ind
   !
   ! based on regular grid where fine grid is integer multiple of coarse grid. 
   ! all positions are scaled by coarse grid step.
   function polynomial_ipol_coeff(order, jj, pos, knot) result(coeff)
    real :: coeff
    integer, intent(in) :: order
    integer, intent(in) :: jj
    integer, intent(in) :: pos
    real, intent(in) :: knot
    !
    integer :: ii
    !
    coeff = 1.0
    do ii = 1, order + 1 
      if (ii .ne. jj) then
        coeff = coeff*(pos - ii - knot)/(jj - ii)
      end if
    end do
    !
    end function polynomial_ipol_coeff

end function double_k_grid
