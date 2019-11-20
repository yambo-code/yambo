! 
!        Copyright (C) 2000-2019 the YAMBO team
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
subroutine plot_interpolated_values(Xk,Xen,Ef,LOCAL_k,IDs,dims,nquantities,nfiles,basename,int_kind)
 !
 use units,          ONLY:HA2EV
 use pars,           ONLY:SP,schlen,lchlen
 use com,            ONLY:msg,of_open_close
 use electrons,      ONLY:levels,n_sp_pol,n_spinor
 use vec_operate,    ONLY:v_norm,iku_v_norm
 use R_lattice,      ONLY:bz_samp
 use stderr,         ONLY:intc,set_real_printed_length
 use YPPm,           ONLY:coo_out,K_transform,CIRCUIT_E_db,CIRCUIT_k_label
 use interpolate,    ONLY:INTERP_obj
 !
#include<memory.h>
 !
 type(bz_samp),    intent(in)  :: Xk,LOCAL_k
 type(levels),     intent(in)  :: Xen
 real(SP),         intent(in)  :: Ef
 integer,          intent(in)  :: IDs(nquantities),dims(2),nquantities,nfiles(nquantities)
 character(*),     intent(in)  :: basename,int_kind 
 !
 ! Local variables
 !
 integer               :: ik_local,ik_ibz_grid,ik_bz_grid,iqnt,i_dim,ifile,i_point,i_sp_pol,ivalue,&
&                         ndims,nval,n_points,n_points_max
 logical               :: PLOT_BANDS,PLOT_from_DB,PLOT_interpolated,PLOT_built_in,l_print_k_name
 character(lchlen)     :: of_string
 character(schlen)     :: headings_k(4),headings(dims(2)-dims(1)+1+4), &
&                         file_name(sum(nfiles)),k_name
 real(SP)              :: value_k,        values(dims(2)-dims(1)+1+4), &
&                         tmp_var(dims(1):dims(2),maxval(nfiles),nquantities),tmp_dist(3)
 !
 ! Allocs
 !
 integer,  allocatable :: Grid_to_path_map(:,:)
 real(SP), allocatable :: circuit_versor(:,:),circuit_dist(:,:)
 real(SP), allocatable :: auxiliary_var(:,:,:,:),tmp_k(:,:),tmp_k_dist(:,:)
 !
 call set_real_printed_length(f_length=10,g_length=10)
 !
 PLOT_interpolated = trim(basename)=="interpolated"
 PLOT_built_in     = trim(basename)=="built_in"    
 PLOT_from_DB      = trim(basename)=="from_DB"
 !
 PLOT_BANDS       = trim(int_kind)=="bands"
 !
 call msg("s","Generating output for "//trim(basename)//" "//trim(int_kind))
 !
 ndims=dims(2)-dims(1)+1
 n_points=1
 n_points_max=1
 !
 ! Define a map between k-points in grid and nearest k-point in circuit
 !
 if (PLOT_built_in) then
   !
   YAMBO_ALLOC(circuit_versor,(3,LOCAL_k%nbz))
   YAMBO_ALLOC(circuit_dist  ,(3,LOCAL_k%nbz))
   !
   YAMBO_ALLOC(Grid_to_path_map,(LOCAL_k%nbz,Xk%nbz))
   Grid_to_path_map =-1
   !
   do ik_local=1,LOCAL_k%nbz-1
     tmp_dist=LOCAL_k%pt(ik_local+1,:)-LOCAL_k%pt(ik_local,:)
     circuit_dist  (:,ik_local)=tmp_dist
     circuit_versor(:,ik_local)=tmp_dist/v_norm(tmp_dist)
   enddo
   circuit_versor(:,ik_local)=circuit_versor(:,ik_local-1)
   !
   n_points_max=0
   do ik_local=1,LOCAL_k%nbz
     !
     n_points=0
     do ik_bz_grid=1,Xk%nbz
       !
       tmp_dist=Xk%ptbz(ik_bz_grid,:)-LOCAL_k%pt(ik_local,:)
       !
       if ((abs(v_norm(tmp_dist)-dot_product(tmp_dist,circuit_versor(:,ik_local)))<1.E-5_SP).and.&
       &    iku_v_norm(tmp_dist)<=iku_v_norm(circuit_dist(:,ik_local))                            ) then
         Grid_to_path_map(ik_local,ik_bz_grid)=ik_bz_grid
         n_points=n_points+1
       endif
       n_points_max=max(n_points,n_points_max)
       !
     enddo
     !
   enddo
   !
   YAMBO_FREE(circuit_versor)
   YAMBO_FREE(circuit_dist  )
   !
   if(n_points_max>1) then
     YAMBO_ALLOC(auxiliary_var,(dims(1):dims(2),maxval(nfiles),nquantities,n_points_max-1))
   endif
   !
 endif
 !
 YAMBO_ALLOC(     tmp_k,(3,n_points_max))
 YAMBO_ALLOC(tmp_k_dist,(3,n_points_max))
 !
 ! Define filenames
 !
 file_name= " "
 !
 ifile=0
 file_name(1:nfiles(1))=trim(int_kind)//"_"//trim(basename)
 if(PLOT_BANDS) then
   do i_sp_pol=1,n_sp_pol
     ifile=ifile+1
     if (n_sp_pol>1.and.i_sp_pol==1) file_name(ifile)=trim(file_name(ifile))//"_spin-up"
     if (n_sp_pol>1.and.i_sp_pol==2) file_name(ifile)=trim(file_name(ifile))//"_spin-dn"
   enddo
 endif
 !
 if((PLOT_interpolated.or.PLOT_built_in).and.PLOT_BANDS.and.n_spinor==2) then
   file_name(ifile+1)="spin_factors_UP_"//trim(basename)
   file_name(ifile+2)="spin_factors_DN_"//trim(basename)
   file_name(ifile+3)="magnetization_x_"//trim(basename)
   file_name(ifile+4)="magnetization_y_"//trim(basename)
   file_name(ifile+5)="magnetization_z_"//trim(basename)
   ! TMP coding for back compatibility <
   if(PLOT_interpolated.and.PLOT_BANDS) then
     file_name(ifile+1)="spin_factors_UP"
     file_name(ifile+2)="spin_factors_DN"
     file_name(ifile+3)="magnetization_x"
     file_name(ifile+4)="magnetization_y"
     file_name(ifile+5)="magnetization_z"
   endif
   ! TMP coding for back compatibility >
   ifile=ifile+5
 endif
 !
 if(ifile /= sum(nfiles)) call error(" something went wrong")
 !
 ! Defined headers
 !
 headings_k(1)=" |k| (a.u.) "
 headings_k(2:4)=(/"k_x ("//trim(coo_out)//")","k_y ("//trim(coo_out)//")","k_z ("//trim(coo_out)//")"/)
 !
 nval=0
 if(PLOT_BANDS) then
   headings(1)=headings_k(1)
   headings(ndims+2:ndims+4)=headings_k(2:4)
   nval=ndims+4
 endif
 !
 do i_dim=dims(1),dims(2)
   if(PLOT_BANDS) headings(1+i_dim-dims(1)+1)=' b'//intc(i_dim)
 enddo
 !
 ! Open files and print headers
 !
 do ifile=1,sum(nfiles)
   call of_open_close(trim(file_name(ifile)),'ot')
   if(PLOT_BANDS) then
     call msg('o '//trim(file_name(ifile)),"#",headings,INDENT=0,USE_TABS=.true.)
     call msg('o '//trim(file_name(ifile)),"#")
   endif
 enddo
 !
 ! print values
 !
 value_k =0._SP
 tmp_k_dist=0._SP
 !
 do ik_local=1,LOCAL_k%nbz
   !
   l_print_k_name=.false.
   !
   if (PLOT_built_in) then
     !
     n_points=0
     do ik_bz_grid=1,Xk%nbz
       if( Grid_to_path_map(ik_local,ik_bz_grid)==-1 ) cycle
       n_points=n_points+1
       ik_ibz_grid=Xk%sstar(ik_bz_grid,1)
       !
       tmp_k_dist(:,n_points)=Xk%ptbz(ik_bz_grid,:)-LOCAL_k%pt(ik_local,:)
       tmp_k(:,n_points)=Xk%ptbz(ik_bz_grid,:)
       !
       do iqnt=1,nquantities
         if(n_points==1) tmp_var(:,1:nfiles(iqnt),iqnt)=INTERP_obj(IDs(iqnt))%SEED(dims(1):dims(2),ik_ibz_grid,:)
         if(n_points> 1) auxiliary_var(:,1:nfiles(iqnt),iqnt,n_points-1)=INTERP_obj(IDs(iqnt))%SEED(dims(1):dims(2),ik_ibz_grid,:)
       enddo
       !
     enddo
     !
   endif
   !
   if (PLOT_interpolated.or.PLOT_from_DB) then
     !
     tmp_k(:,1)=LOCAL_k%pt(ik_local,:)
     !
     if (len_trim(CIRCUIT_k_label(ik_local))>0) then
       k_name="  ["//trim(CIRCUIT_k_label(ik_local))//"]"
       l_print_k_name=.true.
     endif
     !
   endif
   !
   if (PLOT_interpolated) then
     !
     do iqnt=1,nquantities
       tmp_var(:,1:nfiles(iqnt),iqnt)=INTERP_obj(IDs(iqnt))%INTERP(dims(1):dims(2),ik_local,:)
     enddo
     !
     if( PLOT_BANDS) then
       if(allocated(Xen%Eo).and.allocated(CIRCUIT_E_db%E) ) then
         tmp_var(:,1:nfiles(1),1)=tmp_var(:,1:nfiles(1),1)+CIRCUIT_E_db%E(dims(1):dims(2),ik_local,:)
       endif
       tmp_var(:,1:nfiles(1),1)=tmp_var(:,1:nfiles(1),1)+Ef
     endif
     !
   endif
   !
   ! Specific for bands only
   !
   if (PLOT_from_DB.and.PLOT_BANDS) then
     !
     tmp_k(:,n_points)=LOCAL_k%pt(ik_local,:)
     tmp_var(:,1:n_sp_pol,1)=CIRCUIT_E_db%E(dims(1):dims(2),ik_local,:)+Ef
     !
   endif
   !
   do i_point=1,n_points
     !
     if (ik_local>1.and.i_point==1) value_k=value_k+iku_v_norm(LOCAL_k%pt(ik_local,:)-LOCAL_k%pt(ik_local-1,:))
     !
     call K_transform(tmp_k(:,i_point),'iku')
     !
     values(1)=value_k+iku_v_norm(tmp_k_dist(:,i_point))
     values(ndims+2:ndims+4)=tmp_k(1:3,i_point)
     !
     ivalue=0
     do iqnt=1,nquantities
       do ifile=1,nfiles(iqnt)
         ivalue=ivalue+1
         if(i_point==1) values(2:ndims+1)=      tmp_var(:,ifile,iqnt)
         if(i_point> 1) values(2:ndims+1)=auxiliary_var(:,ifile,iqnt,i_point-1)
         if(PLOT_BANDS .and. ivalue==1) values(2:ndims+1)=values(2:ndims+1)*HA2EV
         !
         if (l_print_k_name) then
           call msg('o '//trim(file_name(ivalue)),' ',values(:nval),INDENT=0,USE_TABS=.true.,formatted_msg=of_string)
           of_string=trim(of_string)//trim(k_name)
           call msg_deliver(trim(file_name(ivalue)),of_string)
         else
           call msg('o '//trim(file_name(ivalue)),' ',values(:nval),INDENT=0,USE_TABS=.true.)
         endif
       enddo
       !
     enddo
   enddo
   !
 enddo
 !
 ! Close files
 !
 do ifile=1,sum(nfiles)
   call of_open_close(trim(file_name(ifile)))
 enddo
 !
 if(n_points_max>1) then
   YAMBO_FREE(auxiliary_var)
 endif
 !
 call set_real_printed_length( )
 !
end subroutine plot_interpolated_values
