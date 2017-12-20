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
subroutine electrons_bands_interpolate(Xen,Xk,ID_interp,BANDS_range,CIRCUIT_bands,INTERP_bands)
 !
 use pars,           ONLY:SP
 use YPP,            ONLY:BANDS_steps,coo_in,coo_out,k_transform,INTERP_grid,&
&                         BANDS_Eo_DB,BANDS_Eo,USER_k,CIRCUIT_k,INTERP_k,CIRCUIT_made_of_SPECIAL_k
 use electrons,      ONLY:levels,n_sp_pol
 use D_lattice,      ONLY:alat,a
 use R_lattice,      ONLY:bz_samp,bz_samp_reset
 use com,            ONLY:msg,error,warning
 use vec_operate,    ONLY:c2a,v_norm
 use parser_m,       ONLY:parser
 use interpolate,    ONLY:eval_interpolation_coeff,bz_interpolation
 use interfaces,     ONLY:io_DB1_selective_scan
 !
#include<memory.h>
 !
 type(bz_samp), intent(in)  :: Xk
 type(levels),  intent(in)  :: Xen
 type(levels),  intent(out) :: CIRCUIT_bands,INTERP_bands
 integer      , intent(in)  :: BANDS_range(2)
 integer      , intent(out) :: ID_interp
 !
 ! Work space
 !
 real(SP)              :: real_ctl,max_dist,v(3),db_a(3,3),db_alat(3)
 real(SP)              :: bands_input(BANDS_range(1):BANDS_range(2),1:n_sp_pol,1:Xk%nibz)
 real(SP), allocatable :: distances(:)
 integer               :: i1,ic,ib,ik,i_sp_pol,i_sp,i_err
 integer,  allocatable :: int_distances(:)
 !
 ! Input file parsing
 !====================
 !
 real_ctl=0._SP
 call bz_samp_reset(USER_k)
 if (.not.CIRCUIT_made_of_SPECIAL_k) call bz_samp_reset(CIRCUIT_k)
 !
 if (BANDS_steps>0.and.trim(BANDS_Eo_DB)=="none".and..not.CIRCUIT_made_of_SPECIAL_k) then
   !
   ! Read the kpts from input (extrema from the path)
   ! 
   USER_k%nbz=1
   !
   kgrid_main_loop: do while(real_ctl/=999._SP)
     YAMBO_FREE(USER_k%pt)
     YAMBO_ALLOC(USER_k%pt,(USER_k%nbz,3))
     USER_k%pt(USER_k%nbz,:)=(/0._SP,0._SP,999._SP/)
     call parser('BANDS_kpts',USER_k%pt)
     real_ctl=USER_k%pt(USER_k%nbz,3)
     if (real_ctl/=999._SP) USER_k%nbz=USER_k%nbz+1
   enddo kgrid_main_loop
   !
   USER_k%nbz =USER_k%nbz-1
   USER_k%nibz=USER_k%nbz
   !
   coo_out="iku"
   do ik=1,USER_k%nbz
     call k_transform(USER_k%pt(ik,:),coo_in)
   enddo
   !
 endif
 !
 ! Initialization
 !================
 if((BANDS_steps==0.and.USER_k%nbz==0).and.any(INTERP_grid<=0).and.trim(BANDS_Eo_DB)=="none"&
&   .and..not.CIRCUIT_made_of_SPECIAL_k) then
   call error(' Neither KPT path nor KPT grid are defined in input')
 endif
 !
 if (any(BANDS_range<=0))                call error(" Wrong bands range")
 !
 if (BANDS_range(2)-BANDS_range(1)+1>20) call error(" YPP cannot interpolate more than 20 bands. Reduce bands range")
 !
 ! DEBUG>
 !if (BZ_DbGd_tot_Nkpts> 0) call error(" Interpolation and Double_Grid not yet implemented")
 ! DEBUG<
 !
 ! Define the path (CIRCUIT_K) from the user kpts if BANDS_steps>0
 !================================================================= 
 if( BANDS_steps> 0 .and. .not. CIRCUIT_made_of_SPECIAL_k ) then
   !
   if (trim(BANDS_Eo_DB)=="none") then
     !
     allocate(distances(USER_k%nbz-1),int_distances(USER_k%nbz-1))
     !
     do ik=1,USER_k%nbz-1
       v=USER_k%pt(ik,:)-USER_k%pt(ik+1,:)
       call c2a(v_in=v,mode="ki2c")
       distances(ik)=v_norm(v)
     enddo
     !
     max_dist=maxval(distances)
     !
     do ik=1,USER_k%nbz-1
       int_distances(ik)=maxval((/nint(BANDS_steps*distances(ik)/max_dist),1/))
     enddo
     CIRCUIT_k%nbz =sum(int_distances)+1
     CIRCUIT_k%nibz=CIRCUIT_k%nbz
     !
     call msg("s",'Number of K-points in the circuit :',CIRCUIT_k%nbz)
     !
     YAMBO_ALLOC(CIRCUIT_k%pt,(CIRCUIT_k%nbz,3))
     YAMBO_ALLOC(CIRCUIT_k%weights,(CIRCUIT_k%nbz))
     !
     CIRCUIT_k%weights=1._SP/real(CIRCUIT_k%nbz,SP)
     !
     ic=1
     do ik=1,USER_k%nbz-1
       v=(USER_k%pt(ik+1,:)-USER_k%pt(ik,:))/int_distances(ik)
       do i1=1,int_distances(ik)
         CIRCUIT_k%pt(ic,:)= USER_k%pt(ik,:)+(i1-1._SP)*v
         ic=ic+1
       enddo
     enddo
     CIRCUIT_k%pt(CIRCUIT_k%nbz,:)=USER_k%pt(USER_k%nbz,:)
     !
     deallocate(distances,int_distances)
     !
   else 
     !
     i_err=io_DB1_selective_scan("nk nb alat lattice_vectors",trim(BANDS_Eo_DB),BANDS_Eo,CIRCUIT_k,db1_a=db_a,db1_alat=db_alat)
     CIRCUIT_k%nbz=CIRCUIT_k%nibz
     if (any(alat/=db_alat).or.any(a/=db_a)) call warning("Lattice constants mismatch")
     call msg("s",'Eo DB bands/k-point:',(/BANDS_Eo%nb, CIRCUIT_k%nbz/))
     YAMBO_ALLOC(CIRCUIT_k%pt,(CIRCUIT_k%nbz,3))
     YAMBO_ALLOC(BANDS_Eo%E,(BANDS_Eo%nb,CIRCUIT_k%nbz,n_sp_pol))
     i_err=io_DB1_selective_scan('E K',trim(BANDS_Eo_DB),BANDS_Eo,CIRCUIT_k)
     if (i_err/=0) call error("Reading the "//trim(BANDS_Eo_DB)//" SAVE database")
     !
   endif
 endif
 !
 ! Define the regural grid for interpolation if (all(interp_grid>0))
 !===================================================================
 if(all(INTERP_grid>0)) then
   call bz_samp_reset(INTERP_k)
   INTERP_k%description="Interp"
   call grid_generator(INTERP_k,'i',INTERP_grid)
 endif
 !
 ! Brillouine zone setup
 !========================
 call bz_interp_setup(Xk)
 !
 ! Energies to be interpolated
 !=============================
 do i_sp_pol=1,n_sp_pol
   do ib=BANDS_range(1),BANDS_range(2)
     do ik=1,Xk%nibz
       bands_input(ib,i_sp_pol,ik)=Xen%E(ib,ik,i_sp_pol)
       !
       ! Interpolate only GW corrections?
       !
       if (allocated(Xen%Eo).and.allocated(BANDS_Eo%E)) then
         bands_input(ib,i_sp_pol,ik)=Xen%E(ib,ik,i_sp_pol)-Xen%Eo(ib,ik,i_sp_pol)
       endif
       !
     enddo
   enddo
   !
 enddo
 !
 call section('=',"Interpolation Coefficients")
 !==============================================
 call eval_interpolation_coeff(R2D=bands_input,k=Xk,Nk=Xk%nibz,ID=ID_interp)
 !
 ! Interpolation
 !===============
 ! 1: interpolate on the circuit      if (BANDS_steps>0)
 ! 2: interpolate on the regular grid if (interp_grid>0)
 !
 if (BANDS_steps> 0) then
   YAMBO_ALLOC(CIRCUIT_bands%E,(BANDS_range(1):BANDS_range(2),1:n_sp_pol,CIRCUIT_k%nbz))
   call bz_interpolation(K=CIRCUIT_k,R2D=CIRCUIT_bands%E,ID=ID_interp)
   YAMBO_ALLOC(CIRCUIT_k%weights,(CIRCUIT_k%nbz))
   CIRCUIT_k%weights=1./real( CIRCUIT_k%nbz )
 endif
 !
 if(all(INTERP_grid>0)) then
   YAMBO_ALLOC(INTERP_bands%E,(BANDS_range(1):BANDS_range(2),1:n_sp_pol,INTERP_k%nibz))
   call bz_interpolation(K=INTERP_k,R2D=INTERP_bands%E,ID=ID_interp)
 endif
 !
end subroutine electrons_bands_interpolate
