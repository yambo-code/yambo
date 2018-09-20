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
subroutine electrons_angular_momentum(nkpt_angular,nband_angular)
 !
 ! Important:
 ! We are in presence of a potential vector field, so
 ! L = r x p                  is the quantized canonical momentum
 ! M = r x (p+1/c*A) = r x mv     is the mechanical angula momentum,
 !                                which is gauge invariant
 !
 use pars,           ONLY:SP,lchlen,schlen,cZERO
 use magnetic,       ONLY:MAG_landau,MAG_pauli,             &
&                         A_magn_x,A_magn_y,A_magn_z,       &
&                         MAG_hamiltonian_type,Bx,By,Bz,    &
&                         MAG_gauge
 use FFT_m,          ONLY:fft_size
 use D_lattice,      ONLY:a,x_cc
 use wrapper,        ONLY:Vstar_dot_V
 use interfaces,     ONLY:WF_load,WF_free
 use wave_func,      ONLY:WF,wf_x,wf_y,wf_z,WF_derivative_free
 use SC,             ONLY:load_SC_components
 use electrons,      ONLY:n_sp_pol
 use LIVE_t,         ONLY:live_timing
 use units,          ONLY:SPEED_OF_LIGHT
 use com,            ONLY:msg,of_open_close
 !
#include<memory.h>
 !
 ! Input variables
 integer                  :: nkpt_angular(2), nband_angular(2)
 !
 ! Internal variables
 complex(SP), allocatable :: L_tmp(:,:),M_tmp(:,:)
 real(SP)   , allocatable :: L_tot(:,:),M_tot(:,:)
 complex(SP)              :: L(3),M(3)
 logical                  :: angular_dir(3),do_derivative(3)
 integer                  :: nkpt_tot,nband_tot
 !
 ! Dummyes
 integer                  :: ifft,ib,ik,i_spin,i1
 character(lchlen)        :: fname(6)
 character(schlen)        :: titles(5)
 !
 nkpt_tot  = nkpt_angular(2)-nkpt_angular(1)+1
 nband_tot = nband_angular(2)-nband_angular(1)+1
 !
 call load_SC_components('check_WF_DB',n_bands=nband_angular(2))
 !
 MAG_landau=trim(MAG_hamiltonian_type)=='landau' .or. &
&           trim(MAG_hamiltonian_type)=='all'
 MAG_pauli=trim(MAG_hamiltonian_type)=='pauli' .or. &
&          trim(MAG_hamiltonian_type)=='all'
 ! 
 ! Build the vector potential
 call MAG_common_build_A(.true.)
 call load_cc(.false.,.true.)
 ! 
 ! Choose which are the directions you are interested in
 angular_dir(:)=.false.
 if(Bx>epsilon(1.)) angular_dir(1)=.true.
 if(By>epsilon(1.)) angular_dir(2)=.true.
 if(Bz>epsilon(1.)) angular_dir(3)=.true.
 do_derivative(:)=.false.
 if(MAG_landau) then
   if(allocated(A_magn_x)) do_derivative(1)=.true.
   if(allocated(A_magn_y)) do_derivative(2)=.true.
   if(allocated(A_magn_z)) do_derivative(3)=.true.
 else
   ! Presently evaluate only the z component of the angular momentum if
   ! no landau term is not present
   do_derivative(1:2)=.true.
 endif
 !
 ! If ypp_magnetic is used on a database with no magnetic field
 ! then it evaluates Lz
 if(.not.any(angular_dir(:))) angular_dir(3)=.true.
 !
 ! Prepare files for the output
 fname=(/'el_angular_Lx','el_angular_Ly','el_angular_Lz','el_angular_Mx','el_angular_My','el_angular_Mz'/)
 titles(1)='band'
 titles(2)='kpt'
 titles(3)='spin'
 do i1=1,3
   if (.not.angular_dir(i1)) cycle
   call of_open_close(trim(fname(i1)),'ot')
   if(MAG_landau) call of_open_close(trim(fname(i1+3)),'ot')
   select case (i1)
     case(1)
       titles(4:5)=(/'re(L_x)','im(L_x)'/)
       call msg('o Lx','#',titles,INDENT=0,USE_TABS=.true.)    
       if(MAG_landau) then
         titles(4)='MAG_gauge:'
         titles(5)=MAG_gauge
         call msg('o Lx','#',titles(4:5),INDENT=0,USE_TABS=.true.)    
         titles(4:5)=(/'re(M_x)','im(M_x)'/)
         call msg('o Mx','#',titles,INDENT=0,USE_TABS=.true.)    
       endif
     case(2)
       titles(4:5)=(/'re(L_y)','im(L_y)'/)
       call msg('o Ly','#',titles,INDENT=0,USE_TABS=.true.)    
       if(MAG_landau) then
         titles(4)='MAG_gauge:'
         titles(5)=MAG_gauge
         call msg('o Ly','#',titles(4:5),INDENT=0,USE_TABS=.true.)    
         titles(4:5)=(/'re(M_y)','im(M_y)'/)
         call msg('o My','#',titles,INDENT=0,USE_TABS=.true.)    
       endif
     case(3)
       titles(4:5)=(/'re(L_z)','im(L_z)'/)
       call msg('o Lz','#',titles,INDENT=0,USE_TABS=.true.)    
       if(MAG_landau) then
         titles(4)='MAG_gauge:'
         titles(5)=MAG_gauge
         call msg('o Lz','#',titles(4:5),INDENT=0,USE_TABS=.true.)    
         titles(4:5)=(/'re(M_z)','im(M_z)'/)
         call msg('o Mz','#',titles,INDENT=0,USE_TABS=.true.)    
       endif
   end select
 enddo
 !
 call local_alloc()
 !
 do ik=nkpt_angular(1),nkpt_angular(2)
   !
   ! First wf_derivative (important)
   call WF_derivative(nband_angular,(/ik,ik/),do_derivative)
   call WF_load(WF,0,1,nband_angular,(/ik,ik/),title='-MAG',impose_free_and_alloc=.TRUE.)
   !
   if(ik==nkpt_angular(1)) then
     if(.not.MAG_landau) call live_timing("ypp: L",nkpt_tot*n_sp_pol*nband_tot)
     if(MAG_landau) call live_timing("ypp: L & M ",nkpt_tot*n_sp_pol*nband_tot)
   endif
   !
   do i_spin=1,n_sp_pol
     do ib=nband_angular(1),nband_angular(2)
       ifft=WF%index(ib,ik,i_spin)
       if(i_spin==2.and..not.MAG_pauli) cycle
       !
       ! Li =(r x p)_i for i=x,y,z
       ! These are gauge dependent if MAG_landau=.true.
       ! These are quantized if wfs are eigenstate
       if(angular_dir(1)) L_tmp(1,:)=x_cc(:,2)*wf_z(:,1,ifft)-x_cc(:,3)*wf_y(:,1,ifft)
       if(angular_dir(2)) L_tmp(2,:)=x_cc(:,3)*wf_x(:,1,ifft)-x_cc(:,1)*wf_z(:,1,ifft)
       if(angular_dir(3)) L_tmp(3,:)=x_cc(:,1)*wf_y(:,1,ifft)-x_cc(:,2)*wf_x(:,1,ifft)
       !
       if(MAG_landau) then
         ! Mi = [r x (p + 1/c*A)]_i = Li +1/c*(r x A)_i for i=x,y,z
         ! These are the mechanicals momenta and are gauge independent
         if(angular_dir(1)) M_tmp(1,:)=L_tmp(1,:)+1./SPEED_OF_LIGHT*(x_cc(:,2)*A_magn_z(:)- &
&                             x_cc(:,3)*A_magn_y(:))*WF%c(:,1,ifft)
         if(angular_dir(2)) M_tmp(2,:)=L_tmp(2,:)+1./SPEED_OF_LIGHT*(x_cc(:,3)*A_magn_x(:)- &
&                             x_cc(:,1)*A_magn_z(:))*WF%c(:,1,ifft)
         if(angular_dir(3)) M_tmp(3,:)=L_tmp(3,:)+1./SPEED_OF_LIGHT*(x_cc(:,1)*A_magn_y(:)- &
&                             x_cc(:,2)*A_magn_x(:))*WF%c(:,1,ifft)
       endif
       !
       do i1=1,3
         if(.not.angular_dir(i1)) cycle
         L(i1) = Vstar_dot_V(fft_size,WF%c(:,1,ifft),L_tmp(i1,:))
         if(MAG_landau) M(i1) = Vstar_dot_V(fft_size,WF%c(:,1,ifft),M_tmp(i1,:))
         L_tot(i1,ik)=L_tot(i1,ik)+L(i1)
         if(MAG_landau) M_tot(i1,ik)=M_tot(i1,ik)+M(i1)
       enddo
       !
       if(angular_dir(1)) call msg('o Lx','',(/real(ib,SP),real(ik,SP),real(i_spin,SP),&
&                                              real(L(1)),aimag(L(1))/),INDENT=-2,USE_TABS=.true.)
       if(angular_dir(2)) call msg('o Ly','',(/real(ib,SP),real(ik,SP),real(i_spin,SP),&
&                                              real(L(2)),aimag(L(2))/),INDENT=-2,USE_TABS=.true.)
       if(angular_dir(3)) call msg('o Lz','',(/real(ib,SP),real(ik,SP),real(i_spin,SP),&
&                                              real(L(3)),aimag(L(3))/),INDENT=-2,USE_TABS=.true.)
       if(MAG_landau) then
         if(angular_dir(1)) call msg('o Mx','',(/real(ib,SP),real(ik,SP),real(i_spin,SP),&
&                                                real(M(1)),aimag(M(1))/),INDENT=-2,USE_TABS=.true.)
         if(angular_dir(2)) call msg('o My','',(/real(ib,SP),real(ik,SP),real(i_spin,SP),&
&                                                real(M(2)),aimag(M(2))/),INDENT=-2,USE_TABS=.true.)
         if(angular_dir(3)) call msg('o Mz','',(/real(ib,SP),real(ik,SP),real(i_spin,SP),&
&                                                real(M(3)),aimag(M(3))/),INDENT=-2,USE_TABS=.true.)
       endif
       !
       call live_timing(steps=1)
       !
     enddo
   enddo
   !
   do i1=1,3
     if(.not.angular_dir(i1)) cycle
     L_tot(i1,nkpt_tot+1)=L_tot(i1,nkpt_tot+1)+L_tot(i1,ik)
     if(MAG_landau) M_tot(i1,nkpt_tot+1)=M_tot(i1,nkpt_tot+1)+M_tot(i1,ik)
   enddo
   !
   if(angular_dir(1)) call msg('o Lx','#',(/real(ik,SP),L_tot(1,ik)/),INDENT=-2,USE_TABS=.true.)
   if(angular_dir(2)) call msg('o Ly','#',(/real(ik,SP),L_tot(2,ik)/),INDENT=-2,USE_TABS=.true.)
   if(angular_dir(3)) call msg('o Lz','#',(/real(ik,SP),L_tot(3,ik)/),INDENT=-2,USE_TABS=.true.)
   if(MAG_landau) then
     if(angular_dir(1)) call msg('o Mx','#',(/real(ik,SP),M_tot(1,ik)/),INDENT=-2,USE_TABS=.true.)
     if(angular_dir(2)) call msg('o My','#',(/real(ik,SP),M_tot(2,ik)/),INDENT=-2,USE_TABS=.true.)
     if(angular_dir(3)) call msg('o Mz','#',(/real(ik,SP),M_tot(3,ik)/),INDENT=-2,USE_TABS=.true.)
   endif
   !
   call WF_derivative_free()
   call WF_free(WF)
   !
 enddo
 !
 call live_timing() 
 !
 if(angular_dir(1)) call msg('o Lx','#',L_tot(1,nkpt_tot+1),INDENT=-2,USE_TABS=.true.)
 if(angular_dir(2)) call msg('o Ly','#',L_tot(2,nkpt_tot+1),INDENT=-2,USE_TABS=.true.)
 if(angular_dir(3)) call msg('o Lz','#',L_tot(3,nkpt_tot+1),INDENT=-2,USE_TABS=.true.)
 if(MAG_landau) then
   if(angular_dir(1)) call msg('o Mx','#',M_tot(1,nkpt_tot+1),INDENT=-2,USE_TABS=.true.)
   if(angular_dir(2)) call msg('o My','#',M_tot(2,nkpt_tot+1),INDENT=-2,USE_TABS=.true.)
   if(angular_dir(3)) call msg('o Mz','#',M_tot(3,nkpt_tot+1),INDENT=-2,USE_TABS=.true.)
 endif
 !
 do i1=1,3
   if (.not.angular_dir(i1)) cycle
   call of_open_close(trim(fname(i1)))
   if(MAG_landau) call of_open_close(trim(fname(i1+3)))
 enddo
 !
 ! Free the vector potential
 call MAG_common_build_A(.false.)
 YAMBO_FREE(x_cc)
 !
 contains
   subroutine local_alloc()
     YAMBO_ALLOC(L_tmp,(3,fft_size))
     YAMBO_ALLOC(L_tot,(3,nkpt_tot+1))
     L_tmp=cZERO
     L_tot=0._SP
     L=cZERO
     if(MAG_landau) then
       YAMBO_ALLOC(M_tmp,(3,fft_size))
       YAMBO_ALLOC(M_tot,(3,nkpt_tot+1))
       M_tmp=cZERO
       M_tot=0._SP
       M=cZERO
     endif
   end subroutine 
   !
   subroutine local_free()
     YAMBO_FREE(L_tmp)
     YAMBO_FREE(L_tot)
     YAMBO_FREE(M_tmp)
     YAMBO_FREE(M_tot)
   end subroutine
   !
end subroutine
