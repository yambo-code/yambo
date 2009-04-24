!
! Copyright (C) 2000-2008 C. Attaccalite and the YAMBO team 
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
subroutine Kinetic(Xen,Xk)
 !
 use pars,                ONLY:SP,pi
 use timing,              ONLY:live_timing
 use electrons,           ONLY:levels,n_spin, n_spinor, n_sp_pol
 use R_lattice,           ONLY:g_vec,bz_samp
 use D_lattice,           ONLY:alat,dl_sop,sop_inv 
 use X_m,                 ONLY:X_t
 use IO_m,                ONLY:io_control,VERIFY,REP,OP_RD,RD_CL,RD
 use memory_m,            ONLY:mem_est
 use wave_func,           ONLY:wf,wf_ng,wf_state
 use SC,                  ONLY:SC_bands,Ko
 use real_time,           ONLY:Grad
 !
 implicit none
 type(bz_samp), intent(in)    :: Xk
 type(levels),  intent(in)    :: Xen
 !
 ! Work Space
 !
 integer                      :: ik,i1,icfft,ivfft,ic,iv,i2
 integer                      :: is,ia,il,im,alloc_err,i_spin
 complex(SP), allocatable     :: wf_xyz_g2(:,:)
 complex(SP)                  :: rho(4),tot,cdotc
 !
 !I/O and external functions
 !
 integer                      :: ID,io_err
 !
 allocate(wf_xyz_g2(wf_ng,4))
 call live_timing('Kinetic Energy (T):',SC_bands*Xk%nibz*n_spin)
 !
 ! Main loop over k in IBZ
 !
 do ik=1,Xk%nibz
   ! 
   do i_spin=1,n_spin
     ! 
     do iv=1,SC_bands
       !
       do ic=1,SC_bands
         !
         ivfft=wf_state(iv,ik,i_spin)
         !
         ! wf_xyz(1:3) = -i grad fdo(iv,ik)
         !
         ! wf_xyz(4) = - nabla fdo(iv,ik)
         !
         do i1=1,3
           wf_xyz_g2(1:wf_ng,i1) = wf(1:wf_ng,ivfft)*(Xk%pt(ik,i1)+g_vec(1:wf_ng,i1))*2.0_SP*pi/alat(i1)
         enddo
         !
         wf_xyz_g2(1:wf_ng,4)  = wf(1:wf_ng,ivfft) *  & 
&                          ( (xK%pt(ik,1) + g_vec(1:wf_ng,1))**2 * (2.0_SP*pi/alat(1))**2  &
&                          + (xK%pt(ik,2) + g_vec(1:wf_ng,2))**2 * (2.0_SP*pi/alat(2))**2  &
&                          + (xK%pt(ik,3) + g_vec(1:wf_ng,3))**2 * (2.0_SP*pi/alat(3))**2 )
         !                                                   
         icfft=wf_state(ic,ik,i_spin)
         !
         ! rho(1:3) = wf_xyz^* fdo(ic,ik)= - i < fdo(iv,ik)| grad |  fdo(ic,ik) > =
         !          \sum_G -i u^*_{iv ik}(G) i ( k + G ) u_{ic ik}(G)
         !         =  \sum_G u^*_{iv ik}(G) ( k + G ) u_{ic ik}(G)
         !
         ! rho(4) =   \sum_G u^*_{iv ik}(G) ( k + G )^2 u_{ic ik}(G)  
         !
#if defined _DOUBLE
         call zgemv('C',wf_ng,4,(1._DP,0._DP),wf_xyz_g2,wf_ng,wf(:,icfft),1,(0._DP,0._DP),rho,1)
#else
         call cgemv('C',wf_ng,4,(1._SP,0._SP),wf_xyz_g2,wf_ng,wf(:,icfft),1,(0._SP,0._SP),rho,1)
#endif
         !
         ! GPL_EXCLUDE_START SPIN_IGNORE
         !
         Grad(1:3,ic,iv,ik,i_spin)=rho(1:3)
         Ko(ic,iv,ik,i_spin)      = 1._SP/2._SP * rho(4)
         !
       enddo    ! conduction band loop
       !
       call live_timing(steps=1)
       !
     enddo      ! valence band loop
     ! 
   enddo        ! spin loop
   ! 
 enddo          ! k-points
 !
 call live_timing
 !
 ! CLEAN
 !
 deallocate(wf_xyz_g2)
 return
 !
end subroutine Kinetic
!
! Copyright (C) 2000-2008 C. Attaccalite and the YAMBO team 
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
subroutine Kinetic(Xen,Xk)
 !
 use pars,                ONLY:SP,pi
 use timing,              ONLY:live_timing
 use electrons,           ONLY:levels,n_spin, n_spinor, n_sp_pol
 use R_lattice,           ONLY:g_vec,bz_samp
 use D_lattice,           ONLY:alat,dl_sop,sop_inv 
 use X_m,                 ONLY:X_t
 use IO_m,                ONLY:io_control,VERIFY,REP,OP_RD,RD_CL,RD
 use memory_m,            ONLY:mem_est
 use wave_func,           ONLY:wf,wf_ng,wf_state, wf_load
 use SC,                  ONLY:SC_bands,Ko
 use real_time,           ONLY:Grad
 !
 implicit none
 type(bz_samp), intent(in)    :: Xk
 type(levels),  intent(in)    :: Xen
 !
 ! Work Space
 !
 integer                      :: ik,i1,icfft,ivfft,ic,iv,i2
 integer                      :: is,ia,il,im,alloc_err,i_spin
 complex(SP), allocatable     :: wf_xyz_g2(:,:)
 complex(SP)                  :: rho(4),tot
 !
 !I/O and external functions
 !
 integer                      :: ID,io_err
 !
 allocate(wf_xyz_g2(wf_ng,4))
 call live_timing('Kinetic Energy (T):',SC_bands*Xk%nibz*n_spin)
 !
 ! Main loop over k in IBZ
 !
 do ik=1,Xk%nibz
   ! 
   do i_spin=1,n_spin
     ! 
     do iv=1,SC_bands
       !
       do ic=1,SC_bands
         !
         rho(:)=(0.,0.)
         !
         ivfft=wf_state(iv,ik,i_spin)
         !
         ! wf_xyz(1:3) = -i grad fdo(iv,ik)
         !
         ! wf_xyz(4) = - nabla fdo(iv,ik)
         !
         do i1=1,3
           wf_xyz_g2(1:wf_ng,i1) = wf(:,ivfft)*(Xk%pt(ik,i1)+g_vec(:wf_ng,i1))*2.0_SP*pi/alat(i1)
         enddo
         !
         wf_xyz_g2(1:wf_ng,4)  = wf(1:wf_ng,ivfft) *  & 
&                          ( (xK%pt(ik,1) + g_vec(1:wf_ng,1))**2 * (2.0_SP*pi/alat(1))**2  &
&                          + (xK%pt(ik,2) + g_vec(1:wf_ng,2))**2 * (2.0_SP*pi/alat(2))**2  &
&                          + (xK%pt(ik,3) + g_vec(1:wf_ng,3))**2 * (2.0_SP*pi/alat(3))**2 )
         !                                                   
         icfft=wf_state(ic,ik,i_spin)
         !
         ! rho(1:3) = wf_xyz^* fdo(ic,ik)= - i < fdo(iv,ik)| grad |  fdo(ic,ik) > =
         !          \sum_G -i u^*_{iv ik}(G) i ( k + G ) u_{ic ik}(G)
         !         =  \sum_G u^*_{iv ik}(G) ( k + G ) u_{ic ik}(G)
         !
         ! rho(4) =   \sum_G u^*_{iv ik}(G) ( k + G )^2 u_{ic ik}(G)  
         !
#if defined _DOUBLE
         call zgemv('C',wf_ng,4,(1._DP,0._DP),wf_xyz_g2,wf_ng,wf(:,icfft),1,(0._DP,0._DP),rho,1)
#else
         call cgemv('C',wf_ng,4,(1._SP,0._SP),wf_xyz_g2,wf_ng,wf(:,icfft),1,(0._SP,0._SP),rho,1)
#endif
         !
         ! GPL_EXCLUDE_START SPIN_IGNORE
         !
         Grad(1:3,ic,iv,ik,i_spin)=rho(1:3)
         Ko(ic,iv,ik,i_spin)      = 1._SP/2._SP * rho(4)
         !
       enddo    ! conduction band loop
       !
       call live_timing(steps=1)
       !
     enddo      ! valence band loop
     ! 
   enddo        ! spin loop
   ! 
 enddo          ! k-points
 !
 call live_timing
 !
 ! CLEAN
 !
 deallocate(wf_xyz_g2)
 return
 !
end subroutine Kinetic
