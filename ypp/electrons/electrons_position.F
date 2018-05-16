! 
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM DS
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
subroutine electrons_position(nkpt_pos,nband_pos,power,what,format)
 !
 use pars,           ONLY:SP,schlen,pi
 use stderr,         ONLY:intc
 use FFT_m,          ONLY:fft_dim,fft_size
 use D_lattice,      ONLY:a,x_cc
 use wave_func,      ONLY:WF
 use electrons,      ONLY:n_sp_pol
 use LIVE_t,         ONLY:live_timing
 use com,            ONLY:msg,of_open_close,error
 use YPP,            ONLY:output_fname
 use wrapper,        ONLY:Vstar_dot_V
 use stderr,         ONLY:intc
 use interfaces,     ONLY:WF_load,WF_free
 !
#include<memory.h>
 !
 ! Input variables
 integer                  :: nkpt_pos(2),nband_pos(2)
 real(SP)                 :: power
 character(11)            :: what                      ! cartesian,cylindrical,spherical
 character(2)             :: format                    ! 2d,3d
 !
 ! Internal variables
 complex(SP)              :: r_med,theta_med,psi_med   !    spher.   coord (r,theta,psi)
 complex(SP)              :: x_med(3)                  !    cart.    coord (x,y,z)
 complex(SP)              :: rperp_med,z_med,phi_med   !    cylindr. coord (r,z,phi)
 complex(SP)              :: AB_dev,norm               !    AB: 1-(r0/r)**2
 integer                  :: nkpt_tot,nband_tot
 logical                  :: l_cartesian,l_cylindrical,l_spherical
 logical                  :: l_3d,l_2d
 !
 ! Dummyes
 integer                  :: ifft,ib,ik,i_spin
 character(schlen)        :: titles(6)
 real(SP)                 :: tmp_var,x_zero(3)
 complex(SP),allocatable  :: wf_tmp_2d(:,:,:),rperp_tmp_2d(:,:,:),rperp_med_2d(:,:,:)
 complex(SP),allocatable  :: r_tmp(:),theta_tmp(:),psi_tmp(:)
 complex(SP),allocatable  :: x_tmp(:,:)
 complex(SP),allocatable  :: rperp_tmp(:),z_tmp(:),phi_tmp(:)
 integer                  :: ir,ir3,i1
 !
 if(.not.any((/l_cartesian,l_cylindrical,l_spherical/)) ) return
 !
 x_zero=(/0._SP,0._SP,0._SP/)
 YAMBO_ALLOC(x_cc,(fft_size,3))
 if(power<0) call load_cc(.true. ,x_zero,1)
 if(power>0) call load_cc(.false.,x_zero,1)
 call logical_init()
 call local_alloc()
 !
 ! Prepare files for the output
 !
 if(l_3d) then
   if(l_cartesian)   output_fname='cartesian_coord_3d'
   if(l_cylindrical) output_fname='cylindrical_coord_3d'
   if(l_spherical)   output_fname='spherical_coord_3d'
   call of_open_close(trim(output_fname),'ot')
   titles(1)='band'
   titles(2)='kpt'
   titles(3)='spin'
   if(l_cartesian)   titles(4:6)=(/'re(x1)','re(x2)'   ,'re(x3)' /)
   if(l_cylindrical) titles(4:6)=(/'re( r )' ,'re( z )'    ,'re(phi)'/)
   if(l_spherical)    titles(4:6)=(/'re(  r  )' ,'re(theta)','re( psi )' /)
   call msg('o '//output_fname,'#',titles,INDENT=0,USE_TABS=.true.)
   call msg('o '//output_fname,'#')
 endif
 !
 do ik=nkpt_pos(1),nkpt_pos(2)
   !
   call WF_load(WF,0,1,nband_pos,(/ik,ik/),title='-MAG',impose_free_and_alloc=.TRUE.)
   !
   if(ik==nkpt_pos(1)) then
     nkpt_tot   = nkpt_pos(2)-nkpt_pos(1)+1
     nband_tot =  nband_pos(2)-nband_pos(1)+1
     call live_timing("YPP: electrons position",nkpt_tot*nband_tot)        ! n_sp_pol*nband_tot)
   endif
   !
   if(l_cylindrical.and.l_2d) then
     output_fname='coord_rperp_2d_k'//trim(intc(ik))
     call of_open_close(trim(output_fname),'ot')
     titles(1)='From band'
     titles(2)=''
     titles(3)=trim(intc(nband_pos(1)))   
     titles(4)='to band'
     titles(5)=''
     titles(6)=trim(intc(nband_pos(2)))   
     call msg('o '//output_fname,'#',titles,INDENT=0,USE_TABS=.true.)
     call msg('o '//output_fname,'#')
   endif
   !
   do i_spin=1,n_sp_pol
     do ib=nband_pos(1),nband_pos(2)
       ifft=WF%index(ib,ik,i_spin)
       if(i_spin==2) cycle
       !
       ! *******************************
       ! ***  Cartesian coordinates  ***
       ! *******************************
       if(l_cartesian) then
         ! 1)  3D
         if(l_3d) then
           do i1=1,3
             x_tmp(:,i1)=x_cc(:,i1,1)**power*WF%c(:,1,ifft)
             x_med(i1) = Vstar_dot_V(fft_size,WF%c(:,1,ifft),x_tmp(:,i1))
             x_med(i1) = x_med(i1)**(1._SP/power)
           enddo
           !
           call msg('o '//output_fname,'',(/real(ib,SP),real(ik,SP),real(i_spin,SP),&
&             real(x_med(1)),real(x_med(2)),real(x_med(3))/),INDENT=-2,USE_TABS=.true.)
         endif
         !
       endif
       !
       ! ********************************
       ! *** Cylindrical coordinates  ***
       ! ********************************
       if(l_cylindrical) then
         ! 1)  3D
         if (l_3d) then
           do ir=1,fft_size
             tmp_var=sqrt((x_cc(ir,1,1)**2+x_cc(ir,2,1)**2))
             rperp_tmp(ir)=tmp_var**power*WF%c(ir,1,ifft)
             z_tmp(ir)=x_cc(ir,3,1)**power*WF%c(ir,1,ifft)
             if ( x_cc(ir,1,1)> 0 ) phi_tmp(ir)=(0.5*pi+atan(x_cc(ir,2,1)/x_cc(ir,1,1)))*WF%c(ir,1,ifft)
             if ( x_cc(ir,1,1)< 0 ) phi_tmp(ir)=(1.5*pi+atan(x_cc(ir,2,1)/x_cc(ir,1,1)))*WF%c(ir,1,ifft)
           enddo
           rperp_med = Vstar_dot_V(fft_size,WF%c(:,1,ifft),rperp_tmp(:))
           z_med     = Vstar_dot_V(fft_size,WF%c(:,1,ifft),    z_tmp(:))
           phi_med   = Vstar_dot_V(fft_size,WF%c(:,1,ifft),  phi_tmp(:))
           rperp_med = rperp_med**(1./power)
           z_med     = z_med**(1./power)
           AB_dev    = (1.-6.41**2*rperp_med**(-2))  ! 5.92 (8,0)  6.41 (5,5)  10.36  (14,0)
           !
           call msg('o '//output_fname,'',(/real(ib,SP),real(ik,SP),real(i_spin,SP),&
&                   real(rperp_med),real(z_med),real(phi_med),real(AB_dev)/),INDENT=-2,USE_TABS=.true.)
         endif
         ! 2)  2D
         if(l_2d) then
           wf_tmp_2d(:,:,:)=reshape(WF%c(:,1,ifft),(/fft_dim(1),fft_dim(2),fft_dim(3)/))
           rperp_tmp_2d(:,:,:)=reshape(sqrt((x_cc(:,1,1)**2+x_cc(:,2,1)**2))**power*WF%c(:,1,ifft),&
&                                   (/fft_dim(1),fft_dim(2),fft_dim(3)/))
           rperp_med=(0.,0.)
           do ir3 = 0, fft_dim(3)-1         ! z 
             rperp_med_2d(ib-nband_pos(1)+1,i_spin,ir3+1) =&
&                Vstar_dot_V(fft_dim(1)*fft_dim(2),wf_tmp_2d(:,:,ir3+1),rperp_tmp_2d(:,:,ir3+1))
             norm=Vstar_dot_V(fft_dim(1)*fft_dim(2),wf_tmp_2d(:,:,ir3+1),wf_tmp_2d(:,:,ir3+1))
             rperp_med=rperp_med+rperp_med_2d(nband_tot,1,ir3+1)
             rperp_med_2d(ib-nband_pos(1)+1,i_spin,ir3+1)=rperp_med_2d(ib-nband_pos(1)+1,i_spin,ir3+1)/norm
             if (ib==nband_pos(2))  call msg('o '//output_fname,'',(/real(x_cc(1+ir3*fft_dim(1)*fft_dim(2),3,1),SP),&
&                                            real(rperp_med_2d(:,1,ir3+1)),real(rperp_med) /),INDENT=-2,USE_TABS=.true.)
          
           enddo
         endif
         !
       endif
       !
       if(l_spherical) then
         do ir=1,fft_size
           tmp_var=sqrt((x_cc(ir,1,1)**2+x_cc(ir,2,1)**2+x_cc(ir,3,1)**2))
           r_tmp(ir)=tmp_var**power*WF%c(ir,1,ifft)
           tmp_var=sqrt(x_cc(ir,1)**2+x_cc(ir,2)**2)
           theta_tmp(ir)=atan(x_cc(ir,3)/tmp_var)*WF%c(ir,1,ifft)
           if ( x_cc(ir,1,1)> 0 ) psi_tmp(ir)=(0.5*pi+atan(x_cc(ir,2,1)/x_cc(ir,1,1)))*WF%c(ir,1,ifft)
           if ( x_cc(ir,1,1)< 0 ) psi_tmp(ir)=(1.5*pi+atan(x_cc(ir,2,1)/x_cc(ir,1,1)))*WF%c(ir,1,ifft)
         enddo
         r_med = Vstar_dot_V(fft_size,WF%c(:,1,ifft),rperp_tmp(:))
         theta_med = Vstar_dot_V(fft_size,WF%c(:,1,ifft),theta_tmp(:))
         psi_med   = Vstar_dot_V(fft_size,WF%c(:,1,ifft),  psi_tmp(:))
         r_med=r_med**(1./power)
       endif
       !
       if(l_spherical) call msg('o '//output_fname,'',(/real(ib,SP),real(ik,SP),real(i_spin,SP),&
&           real(r_med),real(theta_med),real(psi_med)/),INDENT=-2,USE_TABS=.true.)
       !
       call live_timing(steps=1)
       !
     enddo
   enddo
   !
   if(l_2d) call of_open_close(trim(output_fname))
   call WF_free(WF)
   !
 enddo
 !
 call live_timing() 
 !
 if(l_3d) call of_open_close(trim(output_fname))
 YAMBO_FREE(x_cc)
 !
 contains
   subroutine logical_init()
     l_cartesian=.false.
     l_cylindrical=.false.
     l_spherical=.false.
     if(what=='cartesian')   l_cartesian=.true.
     if(what=='cylindrical') l_cylindrical=.true.
     if(what=='spherical')   l_spherical=.true.
     !
     l_3d=.false.
     l_2d=.false.
     if(format=='3d') l_3d=.true.
     if(format=='2d') l_2d=.true.
     if(l_2d.and..not.l_cylindrical) call error('2d implemented only for cylind coord')
   end subroutine
   !
   subroutine local_alloc()
     if(l_spherical) then
       YAMBO_ALLOC(r_tmp,(fft_size))
       YAMBO_ALLOC(theta_tmp,(fft_size))
       YAMBO_ALLOC(psi_tmp,(fft_size))
       r_tmp=0.
       theta_tmp=0.
       psi_tmp=0.
     endif
     if(l_cartesian) then
       YAMBO_ALLOC(x_tmp,(fft_size,3))
       x_tmp=0.
     endif
     if(l_cylindrical.and.l_3d) then
       YAMBO_ALLOC(rperp_tmp,(fft_size))
       YAMBO_ALLOC(z_tmp,(fft_size))
       YAMBO_ALLOC(phi_tmp,(fft_size))
       rperp_tmp=0.
       z_tmp=0.
       phi_tmp=0.
     endif
     if(l_cylindrical.and.l_2d) then
       YAMBO_ALLOC(rperp_tmp_2d,(fft_dim(1),fft_dim(2),fft_dim(3)))
       YAMBO_ALLOC(rperp_med_2d,(nband_pos(2)-nband_pos(1)+1,1,fft_dim(3)))
       rperp_tmp_2d=(0.,0.)
       rperp_med_2d=(0.,0.)
     endif
     if (l_2d) then
       YAMBO_ALLOC(wf_tmp_2d,(fft_dim(1),fft_dim(2),fft_dim(3)))
     endif
   end subroutine 
   !
   subroutine local_free()
     if(l_spherical) then 
       YAMBO_FREE(r_tmp)
       YAMBO_FREE(theta_tmp)
       YAMBO_FREE(psi_tmp)
     endif
     if(l_cartesian) then
       YAMBO_FREE(x_tmp)
     endif
     if(l_cylindrical.and.l_3d) then
       YAMBO_FREE(rperp_tmp)
       YAMBO_FREE(z_tmp)
       YAMBO_FREE(phi_tmp)
     endif
     if(l_cylindrical.and.l_2d) then
       YAMBO_FREE(rperp_tmp_2d)
       YAMBO_FREE(rperp_med_2d)
     endif
     if(l_2d)then
       YAMBO_FREE(wf_tmp_2d)
     endif
   end subroutine
   !
end subroutine
