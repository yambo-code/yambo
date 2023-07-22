!
!        Copyright (C) 2000-2022 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
subroutine DIPOLE_ypp_driver(k,E,q)
 !
 use pars,           ONLY:SP,rZERO,schlen
 use com,            ONLY:msg
 use stderr,         ONLY:intc
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels,n_spin
 use YPPm,           ONLY:DIPs_C_bands,DIPs_V_bands,DIPs_E_range,DIPs_direction,K_transform,coo_out,INTERP_grid,&
&                         l_PROJECT_plane,l_PROJECT_line,PROJECT_N_par,DIPs,DIPs_mask,DIPs_kind
 use DIPOLES,        ONLY:DIPOLES_reset,DIP_alloc,DIP_iR,DIP_desc
 use parallel_m,     ONLY:PAR_IND_DIPk_ibz,PAR_IND_CON_BANDS_DIP,PAR_IND_VAL_BANDS_DIP,PAR_DIPk_ibz_index
 use parallel_int,   ONLY:PARALLEL_global_indexes,PP_redux_wait
 use QP_CTL_m,       ONLY:QP_apply
 use vec_operate,    ONLY:v_norm
 use interfaces,     ONLY:DESC_write
 use IO_m,           ONLY:deliver_IO_error_message
 use OUTPUT,         ONLY:OUTPUT_driver
 use YPP_interfaces, ONLY:PROJECTION_setup,PROJECTION_plot
 use parser_m,       ONLY:parser
 use interpolate,    ONLY:INTERPOLATION_driver_seed,INTERPOLATION_driver_do,GRID_k,INTERP_obj
 !
#include<memory.h>
 !
 type(bz_samp)  :: k,q
 type(levels)   :: E
 !
 ! Work Space
 !
 integer     :: ik,ic,iv,is,ikbz,ID,NK,ik_plane,ik_mem,BANDS_done(DIPs_C_bands(2)),io_err
 complex(SP) :: DIP_projected
 real(SP)    :: delta_E,v(3),DIP_accumulated(1,k%nibz,1)
 logical     :: BAND_fields_controls(2)
 character(schlen)     :: BANDS_str
 real(SP), allocatable :: DIP_plane(:,:,:)
 integer,  external    :: PROJECTION_k_on_the_path_or_plane
 !
 BAND_fields_controls=.FALSE.
 !
 if (trim(DIPs_kind)=="IP") then
   !
   call section('*','IP Dipole Properties')
   !=======================================================
   !
   ! ... Apply QP corrections
   call QP_apply((/DIPs_V_bands(1),DIPs_C_bands(2)/),E,k,'G',msg_fmt='rs')
   !
   !... PROJECTION setup
   call PROJECTION_setup("setup")
   if (l_PROJECT_line) then
     call warning("Line Projection not possible in DIPOLES mode. Switched off")
     l_PROJECT_line=.FALSE.
   endif
   if (l_PROJECT_plane.and.all((/INTERP_grid>0/))) then
     call warning("Plane Projection and Interpolation not simultaneously possible. Interpolation Switched off")
     INTERP_grid=-1
   endif
   !
   call PROJECTION_setup("DIPOLES",(/1,1/))
   !
   call parser('DIP_C_bands', BAND_fields_controls(1))
   call parser('DIP_V_bands', BAND_fields_controls(2))
   !
 endif
 !
 ! ... Dimensions
 if (all(BAND_fields_controls)) then
   DIPs%ib    =(/DIPs_V_bands(1),DIPs_C_bands(2)/)
   DIPs%ib_lim=(/DIPs_V_bands(2),DIPs_C_bands(1)/)
 else
   DIPs_V_bands=(/DIPs%ib(1),DIPs%ib_lim(1)/)
   DIPs_C_bands=(/DIPs%ib_lim(2),DIPs%ib(2)/)
 endif
 !
 ! ... PARALLEL setup
 call PARALLEL_global_indexes(E,k,q," ",RESET=.TRUE.)
 call PARALLEL_global_indexes(E,k,q,"DIPOLES",Dip=DIPs,Dip_limits_pre_defined=all(BAND_fields_controls))
 !
 ! ... IO
 call DIPOLE_IO(k,e,DIPs,'read ',io_err,'DIP')
 call deliver_IO_error_message(io_err,"Dipoles",.TRUE.)
 !
 if (trim(DIPs_kind)=="LOAD") return
 !
 DIPs_direction=DIPs_direction/v_norm(DIPs_direction)
 !
 ! ... Alloc
 if (trim(DIPs_kind)=="MASK") then
   YAMBO_ALLOC(DIPs_mask,(DIPs_V_bands(1):DIPs_V_bands(2),k%nibz,n_spin))
   DIPs_mask=rZERO
   BANDS_str=" "
   BANDS_done=0
 endif
 if (trim(DIPs_kind)=="IP") DIP_accumulated=rZERO
 !
 ! ... Loop
 do ik=1,k%nibz
   if (.not.PAR_IND_DIPk_ibz%element_1D(ik)) cycle
   ik_mem=PAR_DIPk_ibz_index(ik)
   do iv=DIPs_V_bands(1),DIPs_V_bands(2)
     if (.not.PAR_IND_VAL_BANDS_DIP%element_1D(iv)) cycle
     do ic=DIPs_C_bands(1),DIPs_C_bands(2)
       if (.not.PAR_IND_CON_BANDS_DIP%element_1D(ic)) cycle
       do is=1,n_spin
         delta_E=E%E(ic,ik,is)-E%E(iv,ik,is)
         if (DIPs_E_range(1)>0._SP .and. delta_E<DIPs_E_range(1)) cycle
         if (DIPs_E_range(2)>0._SP .and. delta_E>DIPs_E_range(2)) cycle
         DIP_projected=dot_product(DIPs_direction,DIP_iR(:,ic,iv,ik_mem,is))
         if (trim(DIPs_kind)=="IP") then
           DIP_accumulated(1,ik,1)=DIP_accumulated(1,ik,1)+abs(DIP_projected)**2
         endif
         if (trim(DIPs_kind)=="MASK") then
           DIPs_mask(iv,ik,is)=DIPs_mask(iv,ik,is)+abs(DIP_projected)**2
         endif
         if (BANDS_done(ic)==0) BANDS_str=trim(BANDS_str)//" "//trim(intc(ic))
         BANDS_done(ic)=1
       enddo
     enddo
   enddo
 enddo
 !
 if (trim(DIPs_kind)=="MASK") then
   call msg('s','Conduction band(s) contributing to the MASK',trim(BANDS_str))
   call PP_redux_wait( DIPs_mask )
 endif
 if (trim(DIPs_kind)=="IP") call PP_redux_wait( DIP_accumulated(1,:,1))
 if (.not.trim(DIPs_kind)=="IP") then
   call LOCAL_clean()
   return
 endif
 !
 !... Normalization
 DIP_accumulated=DIP_accumulated/maxval( DIP_accumulated )
 !
 ! ... INTERPOLATION
 if (all(INTERP_grid>0)) call INTERPOLATION_create_the_grid(INTERP_grid)
 if (allocated(GRID_k%pt)) then
   !
   call INTERPOLATION_driver_seed("DIPOLES",ID,(/1,1/),k,SEED_R3=DIP_accumulated)
   call INTERPOLATION_driver_do("DIPOLES",ID,GRID_k)
   !
   ! ... PROJECTION
   if (l_PROJECT_plane) then
     YAMBO_ALLOC(DIP_plane,(PROJECT_N_par,1,1))
     DIP_plane=rZERO
     do ikbz=1,GRID_k%nbz
       ik_plane=PROJECTION_k_on_the_path_or_plane(ikbz)
       if (ik_plane==0) cycle
       if (INTERP_obj(ID)%INTERP(1,ikbz,1)<0._SP) cycle
       DIP_plane(ik_plane,1,1)=DIP_plane(ik_plane,1,1)+INTERP_obj(ID)%INTERP(1,ikbz,1)
     enddo
   endif
   call PROJECTION_plot("DIPOLES", 1,1, (/1,1/), OBS_K=DIP_plane)
   !
 endif
 !
 if (.not.l_PROJECT_plane) then
   !
   ! ... Print
   call OUTPUT_driver("electronic_dipoles",action="reset")
   call OUTPUT_driver("electronic_dipoles",action="open")
   call DESC_write('o electronic_dipoles','#',DIP_desc)
   !
   NK=k%nbz
   if (allocated(GRID_k%ptbz)) NK=GRID_k%nbz
   !
   do ikbz=1,NK
     if (allocated(GRID_k%ptbz)) then
       v=GRID_k%ptbz(ikbz,:)
       ik=ikbz
     else
       v=k%ptbz(ikbz,:)
       ik=k%sstar(ikbz,1)
     endif
     call K_transform(v,'iku')
     call OUTPUT_driver("electronic_dipoles",TITLES=(/"k_x ("//trim(coo_out)//")",&
&                                    "k_y ("//trim(coo_out)//")","k_z ("//trim(coo_out)//")"/),R_VALUES=v)
     if (allocated(GRID_k%pt)) then
       call OUTPUT_driver("electronic_dipoles",TITLES=(/"|<v|R|c>.DIP_dir|^2"/),R_values=(/INTERP_obj(ID)%INTERP(1,ikbz,1)/))
     else
       call OUTPUT_driver("electronic_dipoles",TITLES=(/"|<v|R|c>.DIP_dir|^2"/),R_values=(/DIP_accumulated(1,ik,1)/))
     endif
     call OUTPUT_driver("electronic_dipoles",action="write")
   enddo
   call OUTPUT_driver("electronic_dipoles",action="close")
   !
 endif
 !
 !... Clean
 call LOCAL_clean( )
 YAMBO_FREE(DIP_plane)
 if (all(INTERP_grid>0)) call INTERPOLATION_driver_end(0)
 !
 contains 
   !
   subroutine LOCAL_clean
    call DIP_alloc('DIP_iR')
    call DIP_alloc('DIP_P')
    call DIP_alloc('DIP_v')
    call DIPOLES_reset(DIPs)
   end subroutine
   !
end subroutine
