! 
! Copyright (C) 2000-2013 C. Attaccalite and the YAMBO team
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
subroutine electrons_bands(Xk,Xen)
 !
 use pars,           ONLY:SP,schlen
 use units,          ONLY:HA2EV
 use YPP,            ONLY:BANDS_steps,coo_in,coo_out,BANDS_range,k_transform,interp_grid
 use electrons,      ONLY:levels,n_sp_pol
 use R_lattice,      ONLY:bz_samp,bz_samp_reset
 use com,            ONLY:msg,of_open_close,error
 use vec_operate,    ONLY:c2a,v_norm
 use parser_m,       ONLY:parser
 use stderr,         ONLY:intc
 use interpolate,    ONLY:eval_interpolation_coeff,bz_interpolation,reset_interpolation
 use QP_CTL_m,       ONLY:QP_apply
 implicit none
 !
 type(bz_samp), intent(inout) ::Xk
 type(levels),  intent(in) ::Xen
 !
 ! Work Space
 !
 type(bz_samp)         :: USER_K,CIRCUIT_K,INTERP_K
 real(SP)              :: real_ctl,v(3),max_dist
 real(SP), allocatable :: distances(:),values(:),bands(:,:,:),tmp_bands(:,:,:)
 integer               :: i1,i2,ic,is,ID_interp,ik
 integer,  allocatable :: int_distances(:)
 character(schlen)     :: bands_f_name(n_sp_pol)
 character(schlen), allocatable :: headings(:)
 !
 !Input file parsing
 !
 real_ctl=0._SP
 call bz_samp_reset(USER_K)
 call bz_samp_reset(CIRCUIT_K)
 !
 USER_K%nibz=1
 !
 kgrid_main_loop: do while(real_ctl/=999._SP)
   if (associated(USER_K%pt)) deallocate(USER_K%pt)
     allocate(USER_K%pt(USER_K%nibz,3))
     USER_K%pt(USER_K%nibz,:)=(/0._SP,0._SP,999._SP/)
     call parser('BKpts',USER_K%pt)
     real_ctl=USER_K%pt(USER_K%nibz,3)
     if (real_ctl/=999._SP)  USER_K%nibz=USER_K%nibz+1
 enddo kgrid_main_loop
 !
 USER_K%nibz=USER_K%nibz-1
 !
 coo_out="iku"
 do i1=1,USER_K%nibz
   call k_transform(USER_K%pt(i1,:),coo_in)
 enddo
 !
 if((BANDS_steps==0.or.USER_K%nibz==0).and.any(interp_grid<=0)) return
 !
 call QP_apply(BANDS_range,Xen,Xk,'G',msg_fmt='s')
 !
 call bz_interp_setup(Xk)
 !
 allocate(tmp_bands(Xen%nb,n_sp_pol,Xk%nibz))
 tmp_bands=0._SP
 do is=1,n_sp_pol
   tmp_bands(BANDS_range(1):BANDS_range(2),is,1:Xk%nibz)=Xen%E(BANDS_range(1):BANDS_range(2),1:Xk%nibz,is)
 enddo
 call eval_interpolation_coeff(R2D=tmp_bands(BANDS_range(1):BANDS_range(2),1:n_sp_pol,1:Xk%nibz), &
&                              k=Xk,ID=ID_interp)
 deallocate(tmp_bands)
 !
 allocate(values(3+BANDS_range(2)-BANDS_range(1)+1),headings(3+BANDS_range(2)-BANDS_range(1)+1))
 !
 ! Interpolation along lines
 ! 
 if(BANDS_steps>0) then
   !     
   if(any(BANDS_range<=0)) call error("Wrong bands range")
   !
   allocate(distances(USER_K%nibz-1),int_distances(USER_K%nibz-1))
   !
   call msg("s",'Number of K-points in the circuit :',USER_K%nibz)
   !
   do i1=1,USER_K%nibz-1
     v=USER_K%pt(i1,:)-USER_K%pt(i1+1,:)
     call c2a(v_in=v,mode="ki2c")  
     distances(i1)=v_norm(v)
   enddo
   !
   max_dist=maxval(distances)
   !
   do i1=1,USER_K%nibz-1
     int_distances(i1)=maxval((/nint(BANDS_steps*distances(i1)/max_dist),1/))
   enddo
   !
   CIRCUIT_K%nibz=sum(int_distances)+1
   allocate(CIRCUIT_K%pt(CIRCUIT_K%nibz,3))
   !
   ic=1
   do i1=1,USER_K%nibz-1
     v=(USER_K%pt(i1+1,:)-USER_K%pt(i1,:))/int_distances(i1)
     do i2=1,int_distances(i1)
       CIRCUIT_K%pt(ic,:)= USER_K%pt(i1,:)+(i2-1._SP)*v
       ic=ic+1
     enddo
   enddo
   CIRCUIT_K%pt(CIRCUIT_K%nibz,:)=USER_K%pt(USER_K%nibz,:)
   !
   deallocate(distances,int_distances)
   !
   allocate(bands(BANDS_range(1):BANDS_range(2),1:n_sp_pol,CIRCUIT_k%nibz))
   !
   call bz_interpolation(USER_k=CIRCUIT_k,R2D=bands,ID=ID_interp)
   !
   do is=1,n_sp_pol
     bands_f_name(is)="bands-sp"//intc(is)
     call of_open_close(bands_f_name(is),'ot')
   enddo
   !
   headings(1:3)=(/" kx  "," ky  "," kz  "/)
   do i1=BANDS_range(1),BANDS_range(2)
     headings(3+i1-BANDS_range(1)+1)=' b'//intc(i1)
   enddo
   !
   do is=1,n_sp_pol
     call msg('o '//bands_f_name(is),"#",headings,INDENT=0,USE_TABS=.true.)
   enddo
   !
   call msg('o path',"#"," ",INDENT=0)
   !
   coo_out=coo_in
   !
   do i1=1,CIRCUIT_K%nibz
     !
     call k_transform(CIRCUIT_K%pt(i1,:),"iku")
     values(1:3)=CIRCUIT_K%pt(i1,:)
     do is=1,n_sp_pol
       values(3+1:3+BANDS_range(2)-BANDS_range(1)+1)=bands(BANDS_range(1):BANDS_range(2),is,i1)*HA2EV
       call msg('o '//bands_f_name(is),"",values,INDENT=0,USE_TABS=.true.)
     enddo
     !
   enddo
   ! 
   do is=1,n_sp_pol
     call of_open_close(bands_f_name(is))
   enddo
   !
   deallocate(bands)
   !
 endif
 !
 ! Interpolation on a regural grid
 !
 if(all(interp_grid>0)) then
   !
   call bz_samp_reset(INTERP_k)
   !
   INTERP_k%description="Interp"
   !
   call grid_generator(INTERP_k,interp_grid)
   allocate(bands(BANDS_range(1):BANDS_range(2),1:n_sp_pol,INTERP_k%nibz))
   call bz_interpolation(USER_k=INTERP_k,R2D=bands,ID=ID_interp)
   !
   do is=1,n_sp_pol
     bands_f_name(is)="grid-sp"//intc(is)
     call of_open_close(bands_f_name(is),'ot')
   enddo
   !
   headings(1:3)=(/" kx  "," ky  "," kz  "/)
   do i1=BANDS_range(1),BANDS_range(2)
     headings(3+i1-BANDS_range(1)+1)=' b'//intc(i1)
   enddo
   !
   do is=1,n_sp_pol
     call msg('o '//bands_f_name(is),"#",headings,INDENT=0,USE_TABS=.true.)
   enddo
   !
   coo_out=coo_in
   !
   do i1=1,INTERP_K%nbz
     !
     call k_transform(INTERP_K%ptbz(i1,:),"iku")
     values(1:3)=INTERP_K%ptbz(i1,:)
     !
     ik=INTERP_K%sstar(i1,1)
     !
     do is=1,n_sp_pol
       values(3+1:3+BANDS_range(2)-BANDS_range(1)+1)=bands(BANDS_range(1):BANDS_range(2),is,ik)*HA2EV
       call msg('o '//bands_f_name(is),"",values,INDENT=0,USE_TABS=.true.)
     enddo
     !
   enddo
   !
   do is=1,n_sp_pol
     call of_open_close(bands_f_name(is))
   enddo
   !
   deallocate(bands)
   !
 endif
 !
 call reset_interpolation(ID_interp)
 !
end subroutine
