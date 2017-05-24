! 
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): CA, DS, AM
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
subroutine electrons_bands_interpolate(Xen,Xk,ID_interp,USER_k,INTERP_k,CIRCUIT_k,BANDS_range,bands_output)
 !
 use pars,           ONLY:SP
 use YPP,            ONLY:BANDS_steps,coo_in,coo_out,k_transform,interp_grid
 use electrons,      ONLY:levels,n_sp_pol
 use R_lattice,      ONLY:bz_samp,bz_samp_reset
 use com,            ONLY:msg,error
 use vec_operate,    ONLY:c2a,v_norm
 use parser_m,       ONLY:parser
 use interpolate,    ONLY:eval_interpolation_coeff,bz_interpolation
 !
#include<memory.h>
 !
 type(bz_samp), intent(in)  :: Xk
 type(levels),  intent(in)  :: Xen
 integer      , intent(in)  :: BANDS_range(2)
 integer      , intent(out) :: ID_interp
 type(bz_samp), intent(out) :: USER_K,CIRCUIT_K,INTERP_k
 real(SP)     , pointer     :: bands_output(:,:,:)
 !
 ! Work space
 !
 real(SP)              :: real_ctl,max_dist,v(3)
 real(SP), allocatable :: distances(:)
 real(SP)              :: bands_input(BANDS_range(1):BANDS_range(2),1:n_sp_pol,1:Xk%nibz)
 integer               :: i1,ic,ib,ik,i_sp_pol,i_sp
 integer,  allocatable :: int_distances(:)
 !
 ! Input file parsing
 !====================
 if(BANDS_steps>0) then
   !
   real_ctl=0._SP
   call bz_samp_reset(USER_K)
   call bz_samp_reset(CIRCUIT_K)
   !
   ! Read the kpts from input (extrema from the path)
   ! 
   USER_K%nibz=1
   !
   kgrid_main_loop: do while(real_ctl/=999._SP)
     YAMBO_FREE(USER_K%pt)
     YAMBO_ALLOC(USER_K%pt,(USER_K%nibz,3))
     USER_K%pt(USER_K%nibz,:)=(/0._SP,0._SP,999._SP/)
     call parser('BKpts',USER_K%pt)
     real_ctl=USER_K%pt(USER_K%nibz,3)
     if (real_ctl/=999._SP) USER_K%nibz=USER_K%nibz+1
   enddo kgrid_main_loop
   !
   USER_K%nibz=USER_K%nibz-1
   call msg("s",'Number of K-points in the circuit :',USER_K%nibz)
   !
   coo_out="iku"
   do ik=1,USER_K%nibz
     call k_transform(USER_K%pt(ik,:),coo_in)
   enddo
   !
 endif
 !
 ! Initialization
 !================
 if((BANDS_steps==0.and.USER_K%nibz==0).and.any(interp_grid<=0)) &
   call error(' Neither kpt path nor kpt grid are defined in input')
 !
 if (any(BANDS_range<=0))                call error(" Wrong bands range")
 !
 if (BANDS_range(2)-BANDS_range(1)+1>20) call error(" Ypp cannot interpolate more than 20 bands. Reduce bands range")
 !
 !if (BZ_DbGd_tot_Nkpts> 0) call error(" Interpolation and Double_Grid not yet implemented")
 !
 ! Define the path (CIRCUIT_K) from the user kpts if BANDS_steps>0
 !================================================================= 
 if( BANDS_steps> 0) then
   !
   YAMBO_ALLOC(distances,(USER_K%nibz-1))
   YAMBO_ALLOC(int_distances,(USER_K%nibz-1))
   !
   do ik=1,USER_K%nibz-1
     v=USER_K%pt(ik,:)-USER_K%pt(ik+1,:)
     call c2a(v_in=v,mode="ki2c")
     distances(ik)=v_norm(v)
   enddo
   !
   max_dist=maxval(distances)
   !
   do ik=1,USER_K%nibz-1
     int_distances(ik)=maxval((/nint(BANDS_steps*distances(ik)/max_dist),1/))
   enddo
   CIRCUIT_K%nibz=sum(int_distances)+1
   !
   YAMBO_ALLOC(CIRCUIT_K%pt,(CIRCUIT_K%nibz,3))
   YAMBO_ALLOC(CIRCUIT_K%weights,(CIRCUIT_K%nibz))
   !
   CIRCUIT_K%weights=1._SP/real(CIRCUIT_K%nibz,SP)
   !
   ic=1
   do ik=1,USER_K%nibz-1
     v=(USER_K%pt(ik+1,:)-USER_K%pt(ik,:))/int_distances(ik)
     do i1=1,int_distances(ik)
       CIRCUIT_K%pt(ic,:)= USER_K%pt(ik,:)+(i1-1._SP)*v
       ic=ic+1
     enddo
   enddo
   CIRCUIT_K%pt(CIRCUIT_K%nibz,:)=USER_K%pt(USER_K%nibz,:)
   !
   YAMBO_FREE(distances)
   YAMBO_FREE(int_distances)
   !
 endif
 !
 ! Define the regural grid for interpolation if (all(interp_grid>0))
 !===================================================================
 if(all(interp_grid>0)) then
   call bz_samp_reset(INTERP_k)
   INTERP_k%description="Interp"
   call grid_generator(INTERP_k,'i',interp_grid)
 endif
 !
 ! Brillouine zone setup
 !========================
 call bz_interp_setup(Xk)
 !
 ! Energies to be interpolated
 !=============================
 do i_sp_pol=1,n_sp_pol
   bands_input(BANDS_range(1):BANDS_range(2),i_sp_pol,1:Xk%nibz)=Xen%E(BANDS_range(1):BANDS_range(2),1:Xk%nibz,i_sp_pol)
 enddo
 call eval_interpolation_coeff(R2D=bands_input,k=Xk,Nk=Xk%nibz,ID=ID_interp)
 !
 ! Interpolation
 !===============
 ! 1: interpolate on the circuit      if (BANDS_steps>0)
 ! 2: interpolate on the regular grid if (interp_grid>0)
 !
 if (BANDS_steps> 0) then
   YAMBO_ALLOC_P(bands_output,(BANDS_range(1):BANDS_range(2),1:n_sp_pol,CIRCUIT_k%nibz))
   call bz_interpolation(USER_k=CIRCUIT_k,R2D=bands_output,ID=ID_interp)
 endif
 !
 if(all(interp_grid>0)) then
   YAMBO_ALLOC_P(bands_output,(BANDS_range(1):BANDS_range(2),1:n_sp_pol,INTERP_k%nibz))
   call bz_interpolation(USER_k=INTERP_k,R2D=bands_output,ID=ID_interp)
 endif
 !
end subroutine electrons_bands_interpolate