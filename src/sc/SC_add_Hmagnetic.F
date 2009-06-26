!
! Copyright (C) 2000-2008 A. Marini, D. Sangalli and the YAMBO team 
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
subroutine SC_add_Hmagnetic(kpt,WF_G_max_in,WF_Go_indx_in)
 !
 ! This subroutine evaluate the magnetic Hamilnian in the space of 
 ! KS wave-functions. The field due to a single supercell is supposed to be:
 ! *******************************************************************************************
 !                               [  B_0    r<=r_0
 !    Bx=0   ,   By=0    ,  Bz = {                     ====> A; B=rot(A)
 !                               [   0     r> r_0  
 !
 !      [ -0.5*B_0 y                            [ 0.5*B_0 x
 ! Ax = {                                  Ay = {                                    A_z=0
 !      [ -0.5*B_0 (r_0**2)*y/(x**2+y**2)       [ 0.5*B_0 (r_0**2)*x/(x**2+y**2)
 ! *******************************************************************************************
 ! B_0 and r_0 are given in input
 ! If r_0 == 0 then Bz=B_0 everywhere and we decide A to be the same in each supercell
 ! (which is possible thanks to gauge invariance), that is Ax=-0.5*B_0 y ; Ay=0.5*B_0 x
 ! If r_0 /= 0 then we have the field inside a circle in each supercell.
 ! then to the expression of the vector potential in the supercell R=0 we have to add
 ! a contributoin due to all other cells. This is done summing over a number of supercell
 ! which can be controlled thanks to the input variable AB_cutoff. As the potential goes
 ! to zero as 1/r the number of supercells to be considered should be checked.
 !
 ! Here all magnetic effect are included except the Zeeman splitting, which could be implemented
 ! simlpy with a shift of the energy levels spin up Vs spin down...(at list for collinear systems)
 !
 use pars,           ONLY:SP,pi
 use SC,             ONLY:Ho
 use memory_m,       ONLY:mem_est
 use FFT_m,          ONLY:fft_size,fft_dim,fft_g_table
 use D_lattice,      ONLY:a,DL_vol,alat
 use R_lattice,      ONLY:g_vec
 use magnetic,       ONLY:r_0,B_0,AB_cutoff
 use QP_m,           ONLY:QP_nk,QP_nb
 use electrons,      ONLY:n_sp_pol
 use SC,             ONLY:SC_bands
 use wave_func,      ONLY:wf,wf_state,wf_load,wf_free
 ! 
 !
 ! Input variables
 integer   :: WF_G_max_in,WF_Go_indx_in
 real      :: kpt(QP_nk,3)
 !
 ! Dummies
 integer   :: i1,ir1,ir2,ir3,ig1
 integer   :: icell1,icell2
 integer   :: ik,i_spin,ib,ibp
 real, allocatable  :: r_cell_cc(:,:),r2(:)
 !
 complex, allocatable  :: H_magn_n2(:,:),H_paramagn_x(:),H_paramagn_y(:),&
&                         H_diamagn(:), H_zeeman(:,:)
 complex, allocatable  :: wf_derivative(:,:), wf_x(:,:), wf_y(:,:)
 real,    allocatable  :: A_magn_x(:),A_magn_y(:)
 !
 ! Data expexted to be received from input_file
 B_0=1
 r_0=1
 AB_cutoff=1
 !
 ! ******** Allocation ********
 allocate(A_magn_x(fft_size),A_magn_y(fft_size))
 call mem_est("A_magn_x A_magn_y",(/2*fft_size/))
 allocate(H_paramagn_x(fft_size),H_paramagn_y(fft_size))
 allocate(H_zeeman(fft_size,2),H_diamagn(fft_size))
 call mem_est("H_paramagn_x H_paramagn_y",(/fft_size*2/))
 call mem_est("H_zeeman H_diamagn",(/fft_size*2/))
 A_magn_x(:)=0.
 A_magn_y(:)=0.
 H_paramagn_x(:)=0.
 H_paramagn_y(:)=0.
 H_diamagn(:)=0.
 H_zeeman(:,:)=0.
 !
 allocate(r_cell_cc(fft_size,3),r2(fft_size))
 call mem_est("r_cell_cc r2",(/fft_size*4/))
 ! First we construct A(r)
 ! Ax , Ay  (Az=0)   
 do ir1 = 0, fft_dim(1)-1
   do ir2 = 0, fft_dim(2)-1
     do ir3 = 0, fft_dim(3)-1
       ir = 1 + ir1 + ir2*fft_dim(1) + ir3*fft_dim(1)*fft_dim(2)
       ! Definition: rcell_i=a1_i+a_2i+a3_i     i=x,y,z
       !             in cartesian coordinates
       r_cell_cc(ir,:) =  ir1*a(1,:)/fft_dim(1) +&
&                         ir2*a(2,:)/fft_dim(2) +&
&                         ir3*a(3,:)/fft_dim(3)
       ! Radius in the direction perpendicular to the magnetic
       ! field
       r2(ir)=r_cell_cc(ir,1)**2 + r_cell_cc(ir,2)**2
       if(r2(ir)<=r_0**2) then
         A_magn_x(ir)=-0.5*B_0*r_cell_cc(ir,2)
         A_magn_y(ir)= 0.5*B_0*r_cell_cc(ir,1)
         ! I evaluate here the H_zeeman term, here 1,2 are the spin components
         H_zeeman(ir,1)= B_0
         H_zeeman(ir,2)=-B_0
       else
         A_magn_x(ir)=-0.5*B_0*r_0**2*r_cell_cc(ir,2)/r2(ir)
         A_magn_y(ir)= 0.5*B_0*r_0**2*r_cell_cc(ir,1)/r2(ir)
         ! I evaluate here the H_zeeman
         H_zeeman(ir,1)=0 
         H_zeeman(ir,2)=0
       endif
     enddo
   enddo
 enddo
 !
 if (AB_cutoff/=0) then
   radius=(AB_cutoff*a(1,1)+AB_cutoff*a(2,1))**2
   do icell1 = 1, AB_cutoff
     do icell2 = 1, AB_cutoff
       shift_x=icell1*a(1,1)+icell2*a(2,1)
       shift_y=icell1*a(1,2)+icell2*a(2,2) 
       ! Here I set a cutoff on a number cycrcle of radius AB_cutoff**2
       if((shift_x**2+shift_y**2)>radius) cycle
       ! I add the contribution from the cells which are on the top-right
       r_cell_cc(:,1)=r_cell_cc(:,1)+shift_x
       r_cell_cc(:,2)=r_cell_cc(:,2)+shift_y
       r2(:)=r_cell_cc(:,1)**2 + r_cell_cc(:,2)**2
       A_magn_x(:)=A_magn_x(:)-0.5*B_0*r_0**2*r_cell_cc(:,2)/r2(:)
       A_magn_y(:)=A_magn_y(:)+0.5*B_0*r_0**2*r_cell_cc(:,1)/r2(:)
       ! I add the contribution from the cells which are on the bottom-left
       r_cell_cc(:,1)=r_cell_cc(:,1)-2*shift_x
       r_cell_cc(:,2)=r_cell_cc(:,2)-2*shift_y
       r2(:)=r_cell_cc(:,1)**2 + r_cell_cc(:,2)**2
       A_magn_x(:)=A_magn_x(:)-0.5*B_0*r_0**2*r_cell_cc(:,2)/r2(:)
       A_magn_y(:)=A_magn_y(:)+0.5*B_0*r_0**2*r_cell_cc(:,1)/r2(:)
       ! Set back r_cell_cc to its value in the cell R=0
       r_cell_cc(:,1)=r_cell_cc(:,1)+shift_x
       r_cell_cc(:,2)=r_cell_cc(:,2)+shift_y
     enddo
   enddo
   ! Here I'm doing an error which is of the order ~ B_0 r_0 / radius ...
 endif
 !
 ! 1) H_paramagn = Ax px + Ay py     (orbital component only of H_paramagnetic)
 ! 2) H_diamagn  = Ax**2 + A_y**2
 ! 3) H_zeeman   = \sigma \ddot rot^A   (spin component of H_paramagneti)
 ! 1)
 ! We will take into account the px(/py) component in the derivative of
 ! the wave function, in the next step, so now we have to set up an artificial
 ! distinction H_x and H_y
 H_paramagn_x(:)=A_magn_x(:) 
 H_paramagn_y(:)=A_magn_y(:)
 ! 2) H_diamagnetic
 H_diamagn(:)=A_magn_x(:)**2 + A_magn_y(:)**2
 ! 3) H_zeeman
 ! Already computed when A has been constructed
 deallocate(A_magn_x,A_magn_y)
 call mem_est("A_magn_x A_magn_y")
 deallocate(r_cell_cc,r2)
 call mem_est("r_cell_cc r2")
 !
 ! Load the wf in G space
 !  ------------>  ???????? max(SC_bands,E%nbm) ? QP_nb  <--------------------- ???????????
 !  ------------>  ???????? K%nibz ? QP_nk               <--------------------- ???????????
 ! call wf_load(WF_G_max_in,WF_Go_indx_in,(/1,max(SC_bands,E%nbm)/),(/1,k%nibz/),space='G',title=' ')
 call wf_load(WF_G_max_in,WF_Go_indx_in,(/1,QP_nb/),(/1,QP_nb/),space='G',title=' ')
 ! Allocate the wf_derivative and wf_x, wf_y
 allocate(wf_derivative(WF_G_max_in,2))                                                 ! This is in G space
 call mem_est("wf_derivative",(/WF_G_max_in/))
 allocate(wf_x(fft_size,QP_nk*n_sp_pol*QP_nb),wf_y(fft_size,QP_nk*n_sp_pol*QP_nb))   ! These are in real space
 call mem_est("wf_x wf_y",(/fft_size*QP_nk*n_sp_pol*QP_nb*2/))
 !
 do ik=1,QP_nk   
   do ispin=1,n_sp_pol   
     do ib=1,QP_nb   
       n1=wf_state(ib,ik,ispin)
       ! Evaluate the wf_derivative in G space
       ! wf_derivative(:,i)=-i grad_i wf(:)    i=x,y,z
       ! wf_derivative(G,i)= (k_i+G_i) wf(:)
       ! presently wf_z is not needed
       do i1=1,2
         wf_derivative(:,i1)=wf(:,n1)*(kpt(ik,i1)+g_vec(: WF_G_max_in ,i1))*2.0_SP*pi/alat(i1)
       enddo
       ! Here we FFT wf_derivative to real space --> wf_x,wf_y
       do ig1=1,WF_G_max_in
         wf_x(fft_g_table(ig1,1),n1)= wf_derivative(ig1-1,1)/(DL_vol*float(nkbz)) 
         wf_y(fft_g_table(ig1,1),n1)= wf_derivative(ig1-1,2)/(DL_vol*float(nkbz)) 
       enddo
#if defined _FFTW
       fftw_plan = 0
       call fft_3d(wf_x(:,n1),fft_dim,1,fftw_plan)
       fftw_plan = 0
       call fft_3d(wf_y(:,n1),fft_dim,1,fftw_plan)
#else
       call fft_3d(wf_x(:,n1),fft_dim,1)
       call fft_3d(wf_y(:,n1),fft_dim,1)
#endif
     enddo
   enddo
 enddo
 !
 deallocate(wf_derivative)
 call mem_est("wf_derivative")
 ! Free the wf in G space and load in real space
 call wf_free()
! call wf_load(WF_G_max_in,WF_Go_indx_in,(/1,max(SC_bands,E%nbm)/),(/1,k%nibz/),title=' ')
 call wf_load(WF_G_max_in,WF_Go_indx_in,(/1,QP_nb/),(/1,QP_nk/),title=' ')
 !
 ! Now we compute < n1| H_magn |n2 > = < n1| H_paramagn_x | n2x> + < n1 | H_paramagn_y | n2y> +
 !                                     + < n1| H_diamagnetic |n2 >
 ! were n2x=-i*px|n2 > ;  n2y=-i*py|n2 > 
 !
 allocate(H_magn_n2(fft_size,2))
 call mem_est("H_magn_n2",(/fft_size*2/)*2)
 H_magn_n2(:,:)=(0.,0.)
 !
 do ik=1,QP_nk   
   do ispin=1,n_sp_pol   
     do ib=1,QP_nb   
       do ibp=1,QP_nb
         !
         n1=wf_state(ib,ik,ispin)  
         n2=wf_state(ibp,ik,ispin) 
         !
         H_magn_n2(:,ispin)= H_paramagn_x(:)*wf_x(:,n2)+H_paramagn_y(:)*wf_y(:,n2)
         H_magn_n2(:,ispin)= H_magn_n2(:,ispin)+(H_diamagn(:)+H_zeeman(:,ispin))*wf(:,n2)
         !
#if defined _DOUBLE
         Ho(ib,ibp,ik,ispin) = zdotc(fft_size,wf(:,n1),1,H_magn_n2(:,ispin),1)
#else
         Ho(ib,ibp,ik,ispin) = cdotc(fft_size,wf(:,n1),1,H_magn_n2(:,ispin),1)
#endif
         H_magn_n2(:,:)=(0.,0.)
       enddo
     enddo
   enddo
 enddo
 !
 deallocate(H_magn_n2)
 call mem_est("H_mag_n2")
 deallocate(wf_x,wf_y)
 call mem_est("wf_x wf_y")
 deallocate(H_paramagn_x,H_paramagn_y,H_diamagn,H_zeeman)
 call mem_est("H_paramagn_x H_paramagn_y H_diamagn H_zeeman")
 !
end subroutine
