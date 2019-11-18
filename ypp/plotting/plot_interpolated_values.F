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
 use pars,           ONLY:SP,schlen
 use com,            ONLY:msg,of_open_close
 use electrons,      ONLY:levels,n_sp_pol,n_spinor
 use vec_operate,    ONLY:v_norm,iku_v_norm
 use R_lattice,      ONLY:bz_samp
 use stderr,         ONLY:intc,set_real_printed_length
 use YPPm,           ONLY:coo_out,K_transform,CIRCUIT_E_db
 use interpolate,    ONLY:INTERP_obj
#if defined _YPP_SC
 use SC,             ONLY:SC_neq_kind
#endif
#if defined _YPP_RT
 use YPP_real_time,  ONLY:GreenF_T_and_W
#endif
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
 integer               :: ndims,ik_local,ik_ibz_grid,ik_bz_grid,iqnt,i_dim,ifile,i_sp_pol,ivalue,nval
 logical               :: PLOT_BANDS,PLOT_G2,PLOT_E_and_k_DB,PLOT_interpolate,PLOT_internal
 character(schlen)     :: headings_k(4),headings(dims(2)-dims(1)+1+4), &
&                         file_name(maxval(nfiles))
 real(SP)              :: value_k(4),     values(dims(2)-dims(1)+1+4), &
&                         tmp_var(dims(1):dims(2),maxval(nfiles),nquantities),tmp_dist(3),tmp_k(3)
#if defined _YPP_SC
 logical               :: proj_wfc
#endif
 !
 ! Allocs
 !
 integer,  allocatable :: Grid_to_path_map(:)
 real(SP), allocatable :: circuit_versor(:,:),circuit_dist(:,:)
 !
 call set_real_printed_length(f_length=10,g_length=10)
 !
 PLOT_interpolate = trim(basename)=="interpolated"
 PLOT_internal    = trim(basename)=="built_in"    
 PLOT_E_and_k_DB  = trim(basename)=="from_DB"
 !
 PLOT_BANDS       = trim(int_kind)=="bands"
 PLOT_G2          = trim(int_kind)=="G_k_w"
 !
 ndims=dims(2)-dims(1)+1
 !
#if defined _YPP_SC
 proj_wfc=(trim(SC_neq_kind)=="matsubara")
#endif
 !
 ! Define a map between k-points in grid and nearest k-point in circuit
 !
 if (PLOT_internal   ) then
   !
   YAMBO_ALLOC(circuit_versor,(3,LOCAL_k%nbz-1))
   YAMBO_ALLOC(circuit_dist  ,(3,LOCAL_k%nbz-1))
   !
   YAMBO_ALLOC(Grid_to_path_map,(LOCAL_k%nbz))
   Grid_to_path_map =-1
   !
   do ik_local=1,LOCAL_k%nbz-1
     tmp_dist=LOCAL_k%pt(ik_local+1,:)-LOCAL_k%pt(ik_local,:)
     circuit_dist  (:,ik_local)=tmp_dist
     circuit_versor(:,ik_local)=tmp_dist/v_norm(tmp_dist)
   enddo
   !
   do ik_local=1,LOCAL_k%nbz-1
     do ik_bz_grid=1,Xk%nbz
       !
       tmp_dist=Xk%ptbz(ik_bz_grid,:)-LOCAL_k%pt(ik_local,:)
       !
       if ((abs(v_norm(tmp_dist)-dot_product(tmp_dist,circuit_versor(:,ik_local)))<1.E-5_SP).and.&
       &    iku_v_norm(tmp_dist)<=iku_v_norm(circuit_dist(:,ik_local))                            ) then
         Grid_to_path_map(ik_local)=ik_bz_grid
       endif
       !
     enddo
   enddo
   !
   YAMBO_FREE(circuit_versor)
   YAMBO_FREE(circuit_dist  )
   !
 endif
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
 if(PLOT_G2) then
   do iqnt=1,2
     ifile=ifile+1
     if(ifile==1) file_name(ifile)=trim(file_name(ifile))//"_Re"
     if(ifile==2) file_name(ifile)=trim(file_name(ifile))//"_Im"
   enddo
 endif
 !
 if(PLOT_INTERPOLATE.and.PLOT_BANDS.and.n_spinor==2) then
   file_name(ifile+1)="spin_factors_UP"//trim(basename)
   file_name(ifile+2)="spin_factors_DN"//trim(basename)
   file_name(ifile+3)="magnetization_x"//trim(basename)
   file_name(ifile+4)="magnetization_y"//trim(basename)
   file_name(ifile+5)="magnetization_z"//trim(basename)
   ifile=ifile+5
 endif
 !
#if defined _YPP_SC
 if(PLOT_INTERPOLATE.and.PLOT_BANDS.and.proj_wfc) then
   ifile=ifile+5
   file_name(ifile+1)="proj_val_"//trim(basename)
   file_name(ifile+2)="proj_con_"//trim(basename)
   ifile=ifile+2
 endif
#endif
 !
 if(ifile /= sum(nfiles)) call error(" something went wrong")
 !
 ! Defined headers
 !
 headings_k(1)=" |k| (a.u.) "
 headings_k(2:4)=(/"k_x ("//trim(coo_out)//")","k_y ("//trim(coo_out)//")","k_z ("//trim(coo_out)//")"/)
 !
 if(PLOT_BANDS) then
   headings(1)=" |k| (a.u.) "
   headings(ndims+2:ndims+4)=headings_k(2:4)
   nval=ndims+4
 endif
 if(PLOT_G2   ) then
   values(1)=0._SP
   nval=ndims+1
 endif
 !
 do i_dim=dims(1),dims(2)
   if(PLOT_BANDS) headings(1+i_dim-dims(1)+1)=' b'//intc(i_dim)
#if defined _YPP_RT
   if(PLOT_G2   ) values(1+i_dim-dims(1)+1)=real(GreenF_T_and_W%W(i_dim),SP)*HA2EV
#endif
 enddo
 !
 ! Open files and print headers
 !
 do ifile=1,sum(nfiles)
   call of_open_close(trim(file_name(ifile)),'ot')
   if(PLOT_BANDS) then
     call msg('o '//trim(file_name(ifile)),"#",headings,INDENT=0)
     call msg('o '//trim(file_name(ifile)),"#")
   endif
   if(PLOT_G2) then
     call msg('o '//trim(file_name(ifile))," ",values(:nval),INDENT=0)
   endif
 enddo
 !
 ! File with KPTs
 !
 !if(PLOT_G2   ) then
 !  !
 !  call of_open_close('KPT','ot')
 !  call msg('o KPT',"#",headings_k,INDENT=0,USE_TABS=.true.)
 !  call msg('o KPT',"#")
 !  !
 !endif
 !
 call msg('o path',"#"," ",INDENT=0)
 !
 ! print values
 !
 value_k(1)=0._SP
 tmp_dist  =0._SP
 !
 do ik_local=1,LOCAL_k%nbz
   !
   if (PLOT_internal) then
     !
     if(Grid_to_path_map(ik_local)==-1) cycle
     ik_bz_grid=Grid_to_path_map(ik_local)
     ik_ibz_grid=Xk%sstar(ik_bz_grid,1)
     !
     tmp_dist=Xk%ptbz(ik_bz_grid,:)-LOCAL_k%pt(ik_local,:)
     tmp_k=Xk%ptbz(ik_bz_grid,:)
     !
     do iqnt=1,nquantities
       tmp_var(:,1:nfiles(iqnt),iqnt)=INTERP_obj(IDs(iqnt))%SEED(dims(1):dims(2),ik_ibz_grid,:)
     enddo
     !
   endif
   !
   if (PLOT_interpolate) then
     !
     tmp_k=LOCAL_k%pt(ik_local,:)
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
   if (PLOT_E_and_k_DB.and.PLOT_BANDS) then
     !
     tmp_k=LOCAL_k%pt(ik_local,:)
     tmp_var(:,1:n_sp_pol,1)=CIRCUIT_E_db%E(dims(1):dims(2),ik_local,:)+Ef
     !
   endif
   !
   if (ik_local>1) value_k(1)=value_k(1)+iku_v_norm(LOCAL_k%pt(ik_local,:)-LOCAL_k%pt(ik_local-1,:))
   !
   call K_transform(tmp_k,'iku')
   value_k(2:4)=tmp_k
   !
   values(1)=value_k(1)+iku_v_norm(tmp_dist)
   values(ndims+2:ndims+4)=value_k(2:4)
   !
   ivalue=0
   !
   do iqnt=1,nquantities
     do ifile=1,nfiles(iqnt)
       ivalue=ivalue+1
       values(2:ndims+1)=tmp_var(:,ifile,iqnt)
       if(PLOT_BANDS .and. ivalue<=2) values=values*HA2EV
       !
       call msg('o '//trim(file_name(ivalue)),' ',values(:nval),INDENT=0)
       !
     enddo
   enddo
   !
   !if(PLOT_G2) call msg('o KPT',' ',value_k,INDENT=0)
   !
 enddo
 !
 ! Close files
 !
 do ifile=1,sum(nfiles)
   call of_open_close(trim(file_name(ifile)))
 enddo
 !if(PLOT_G2) call msg('o KPT',' ',value_k,INDENT=0)
 !
 call set_real_printed_length( )
 !
end subroutine plot_interpolated_values
