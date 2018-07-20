! 
!        Copyright (C) 2000-2018 the YAMBO team
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
subroutine electrons_bands_interpolate(Xen,Xk,BANDS_range,CIRCUIT_E,GRID_E,CIRCUIT_dE)
 !
 use pars,           ONLY:SP,schlen
 use YPP,            ONLY:BANDS_steps,coo_in,coo_out,K_transform,INTERP_grid,&
&                         CIRCUIT_E_DB_path,CIRCUIT_E_db,USER_k,CIRCUIT_k,&
&                         CIRCUIT_made_of_SPECIAL_k,BANDS_E_and_k_db,SPECIAL_k,&
&                         BANDS_path,CIRCUIT_k_label,SPECIAL_k_label
 use electrons,      ONLY:levels,n_sp_pol
 use D_lattice,      ONLY:alat,a
 use R_lattice,      ONLY:bz_samp,bz_samp_reset,k_the_nearest
 use com,            ONLY:msg,error,warning
 use vec_operate,    ONLY:c2a,v_norm
 use parser_m,       ONLY:parser
 use interpolate,    ONLY:INTERPOLATION_driver,GRID_k
 use interfaces,     ONLY:io_DB1_selective_scan
 !
#include<memory.h>
 !
 type(bz_samp), intent(in)  :: Xk
 type(levels),  intent(in)  :: Xen
 type(levels),  intent(out) :: CIRCUIT_E
 integer      , intent(in)  :: BANDS_range(2)
 type(levels),  intent(out), optional :: CIRCUIT_dE
 type(levels),  intent(out), optional :: GRID_E
 !
 ! Work space
 !
 real(SP)              :: real_ctl,max_dist,v(3),db_a(3,3),db_alat(3),delta_align
 real(SP), allocatable :: distances(:),E_in(:,:,:),dE_in(:,:,:)
 character(schlen)     :: coo_out_
 integer               :: i1,ic,ib,ik,i_sp_pol,i_err,ik_special
 integer,  allocatable :: int_distances(:)
 !
 ! Input file parsing
 !====================
 !
 real_ctl=0._SP
 call bz_samp_reset(USER_k)
 if (.not.CIRCUIT_made_of_SPECIAL_k) call bz_samp_reset(CIRCUIT_k)
 !
 if (BANDS_steps>0.and.trim(CIRCUIT_E_DB_path)=="none".and..not.CIRCUIT_made_of_SPECIAL_k) then
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
   if (USER_k%nbz==0) then
     call k_special(.TRUE.)
     call error("No points given in input")
   endif
   !
   coo_out_=coo_out
   coo_out ="iku"
   do ik=1,USER_k%nbz
     call K_transform(USER_k%pt(ik,:),coo_in)
   enddo
   coo_out =coo_out_
   !
 endif
 !
 ! Initialization
 !================
 if((BANDS_steps==0.and.USER_k%nbz==0).and.any(INTERP_grid<=0).and.trim(CIRCUIT_E_DB_path)=="none"&
&   .and..not.CIRCUIT_made_of_SPECIAL_k) then
   call error(' Neither KPT path nor KPT grid are defined in input')
 endif
 !
 if (any(BANDS_range<=0))                call error(" Wrong bands range")
 !
 if (BANDS_range(2)-BANDS_range(1)+1>20) call error(" YPP cannot interpolate more than 20 bands. Reduce bands range")
 !
 ! Define the path (CIRCUIT_K) from the user kpts if BANDS_steps>0
 !================================================================= 
 if(.not.CIRCUIT_made_of_SPECIAL_k ) then
   !
   if (trim(CIRCUIT_E_DB_path)=="none".and.BANDS_steps> 0) then
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
     i_err=io_DB1_selective_scan("nk nb alat lattice_vectors",trim(CIRCUIT_E_DB_path),&
&                                CIRCUIT_E_db,CIRCUIT_k,db1_a=db_a,db1_alat=db_alat)
     CIRCUIT_k%nbz=CIRCUIT_k%nibz
     if (any(alat/=db_alat).or.any(a/=db_a)) call warning("Lattice constants mismatch")
     call msg("s",'GS databases         : '//trim(CIRCUIT_E_DB_path))
     call msg("s",'bands/k-points       :',(/CIRCUIT_E_db%nb, CIRCUIT_k%nbz/))
     YAMBO_ALLOC(CIRCUIT_k%pt,(CIRCUIT_k%nbz,3))
     YAMBO_ALLOC(CIRCUIT_E_db%E,(CIRCUIT_E_db%nb,CIRCUIT_k%nbz,n_sp_pol))
     i_err=io_DB1_selective_scan('E K',trim(CIRCUIT_E_DB_path),CIRCUIT_E_db,CIRCUIT_k)
     if (i_err/=0) call error("Reading the "//trim(CIRCUIT_E_DB_path)//" SAVE database")
     BANDS_E_and_k_db=.TRUE.
     !
   endif
   !
   allocate(CIRCUIT_k_label(CIRCUIT_k%nbz))
   CIRCUIT_k_label=" "
   BANDS_path=" "
   if (SPECIAL_k%nbz>0) then
     do ik=1,CIRCUIT_k%nbz
       v=CIRCUIT_k%pt(ik,:)
       call c2a(v_in=v,mode="ki2c")
       ik_special=k_the_nearest(CIRCUIT_k%pt(ik,:),SPECIAL_k%pt(:SPECIAL_k%nbz,:),SPECIAL_k%nbz,ROTATE_k_ref=.TRUE.,i_err=i_err)  
       if (i_err==0) then
         CIRCUIT_k_label(ik)=trim(SPECIAL_k_label(ik_special))
         BANDS_path=trim(BANDS_path)//" "//trim(SPECIAL_k_label(ik_special))
       endif
     enddo
     if (len_trim(BANDS_path)>0) call msg("s",'Circuit path         :',trim(BANDS_path))
   endif
   !
 endif
 !
 ! Interpolation
 !===============
 ! 1: interpolate on the circuit      if (BANDS_steps>0)
 ! 2: interpolate on the regular grid if (interp_grid>0)
 !
 call INTERPOLATION_driver(operation="INIT",SEED_k=Xk)
 !
 if(all(INTERP_grid>0)) call INTERPOLATION_driver(operation="GRID")
 !
 if (allocated(Xen%Eo).and.BANDS_E_and_k_db) then
   call INTERPOLATION_driver(bands=BANDS_range,SEED_E=Xen,SEED_k=Xk,operation="dE_on_built_in")
 endif
 !
 call INTERPOLATION_driver(bands=BANDS_range,SEED_E=Xen,SEED_k=Xk,operation="E_on_built_in")
 !
 if (BANDS_steps> 0) then
   !
   call INTERPOLATION_driver(OUT_k=CIRCUIT_k,OUT_E=CIRCUIT_E,operation="E_on_built_in")
   !
   if (allocated(dE_in).and.present(CIRCUIT_dE)) then
    call INTERPOLATION_driver(OUT_k=CIRCUIT_k,OUT_E=CIRCUIT_dE,operation="dE_on_built_in")
   endif
   !
 endif
 !
 if(all(INTERP_grid>0)) then
   call INTERPOLATION_driver(OUT_k=GRID_k,OUT_E=GRID_E,operation="E_on_built_in")
 endif
 !
 call INTERPOLATION_driver(operation="END")
 !
 ! Alignement
 !============
 !
 if (allocated(CIRCUIT_E_db%E)) then
   do i_sp_pol=1,n_sp_pol
     delta_align=CIRCUIT_E%E(BANDS_range(1),1,i_sp_pol)-CIRCUIT_E_db%E(BANDS_range(1),1,i_sp_pol)
     do ib=BANDS_range(1),BANDS_range(2)
       do ik=1,CIRCUIT_k%nbz
         CIRCUIT_E_db%E(ib,ik,i_sp_pol)=CIRCUIT_E_db%E(ib,ik,i_sp_pol)+delta_align
       enddo
     enddo
   enddo
 endif
 !
 ! Free
 !======
 !
 YAMBO_FREE(E_in)
 YAMBO_FREE(dE_in)
 !
end subroutine electrons_bands_interpolate
