! 
!        Copyright (C) 2000-2015 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): CA, DS
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
subroutine electrons_bands(Xk,Xen,BANDS_range)
 !
 use pars,           ONLY:SP,schlen
 use units,          ONLY:HA2EV
 use YPP,            ONLY:BANDS_steps,coo_in,coo_out,k_transform,interp_grid, &
&                         l_RT_occ_Bands,l_RT_life_Bands,l_bands
 use electrons,      ONLY:levels,n_sp_pol,BZ_RIM_tot_Nkpts
 use R_lattice,      ONLY:bz_samp,bz_samp_reset
 use com,            ONLY:msg,of_open_close,error
 use vec_operate,    ONLY:c2a,v_norm
 use parser_m,       ONLY:parser
 use stderr,         ONLY:intc
 use interpolate,    ONLY:eval_interpolation_coeff,bz_interpolation,reset_interpolation
 use QP_CTL_m,       ONLY:QP_apply
 !
 implicit none
 !
 type(bz_samp), intent(inout) :: Xk
 type(levels),  intent(in)    :: Xen
 integer,       intent(in)    :: BANDS_range(2)
 !
 ! Work Space
 !
 type(bz_samp)         :: USER_K,CIRCUIT_K,INTERP_K
 real(SP)              :: real_ctl,v(3),max_dist
 real(SP), allocatable :: distances(:),values(:)
 real(SP), allocatable :: bands_grid(:,:,:),bands_circuit(:,:,:)
 logical               :: l_g_space_interp,l_nearest_neig_interp
 integer               :: i1,i2,ic,is,ID_interp,ik,ikbz,ib,Nk
 integer,  allocatable :: int_distances(:)
 character(3)          :: message
 character(schlen)     :: file_name(n_sp_pol)
 character(schlen), allocatable :: headings(:)
 !
 if(any(BANDS_range<=0)) call error(" Wrong bands range")
 !
 !Input file parsing
 !
 real_ctl=0._SP
 call bz_samp_reset(USER_K)
 call bz_samp_reset(CIRCUIT_K)
 !
 ! Define the kind of interpolation
 call parser('NNInterp',l_nearest_neig_interp)
 l_g_space_interp=.not.l_nearest_neig_interp
 !
 if(l_RT_occ_Bands.or.l_RT_life_Bands) then
   !
   !
 else
   !
   if(BZ_RIM_tot_Nkpts> 0) call error(" Interpolation and RIM implemented only in the _RT project")
   if(l_nearest_neig_interp) call error(" NNInterp implemented only for occupations and lifetimes")
   Nk=Xk%nibz
   !
 endif
 !
 ! Read the kpts from input (extrema from the path)
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
 call msg("s",'Number of K-points in the circuit :',USER_K%nibz)
 !
 coo_out="iku"
 do ik=1,USER_K%nibz
   call k_transform(USER_K%pt(ik,:),coo_in)
 enddo
 !
 if((BANDS_steps==0.or.USER_K%nibz==0).and.any(interp_grid<=0)) return
 !
 ! Define the path (CIRCUIT_K) from the user kpts if BANDS_steps>0
 ! 
 if( BANDS_steps> 0) then
   allocate(distances(USER_K%nibz-1),int_distances(USER_K%nibz-1))
   !
   do ik=1,USER_K%nibz-1
     v=USER_K%pt(ik,:)-USER_K%pt(ik+1,:)
     call c2a(v_in=v,mode="ki2c")
     distances(ik)=v_norm(v)
   enddo
   !
   max_dist=maxval(distances)
   !
   !
   do ik=1,USER_K%nibz-1
     int_distances(ik)=maxval((/nint(BANDS_steps*distances(ik)/max_dist),1/))
   enddo
   CIRCUIT_K%nibz=sum(int_distances)+1
   !
   allocate(CIRCUIT_K%pt(CIRCUIT_K%nibz,3))
   allocate(CIRCUIT_K%weights(CIRCUIT_K%nibz))
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
   deallocate(distances,int_distances)
   !
 endif
 !
 ! Define the regural grid for interpolation if (all(interp_grid>0))
 !
 if(all(interp_grid>0)) then
   call bz_samp_reset(INTERP_k)
   INTERP_k%description="Interp"
   call grid_generator(INTERP_k,'i',interp_grid)
 endif
 !
 ! Apply QP corrections
 ! 
 call QP_apply(BANDS_range,Xen,Xk,'G',msg_fmt='s')
 !
 ! Interpolation: Initial setup
 !
 !
 call bz_interp_setup(Xk)
 !
 ! Energies interpolation.
 !===========================
 !
 ! STEP 1: define coefficients for bands
 !
 allocate(bands_grid(BANDS_range(1):BANDS_range(2),1:n_sp_pol,1:Nk))
 do is=1,n_sp_pol
   if(BZ_RIM_tot_Nkpts==0) bands_grid(BANDS_range(1):BANDS_range(2),is,1:Nk)=Xen%E(BANDS_range(1):BANDS_range(2),1:Nk,is)
 enddo
 call eval_interpolation_coeff(R2D=bands_grid,k=Xk,Nk=Nk,ID=ID_interp)
 deallocate(bands_grid)
 !
 ! STEP 2
 ! 1: interpolate on the circuit      if (BANDS_steps>0)
 ! 2: interpolate on the regular grid if (interp_grid>0)
 !
 if (BANDS_steps> 0) then
   allocate(bands_circuit(BANDS_range(1):BANDS_range(2),1:n_sp_pol,CIRCUIT_k%nibz))
   call bz_interpolation(USER_k=CIRCUIT_k,R2D=bands_circuit,ID=ID_interp)
 endif
 !
 if(all(interp_grid>0)) then
   allocate(bands_grid(BANDS_range(1):BANDS_range(2),1:n_sp_pol,INTERP_k%nibz))
   call bz_interpolation(USER_k=INTERP_k,R2D=bands_grid,ID=ID_interp)
 endif
 !
 call reset_interpolation(ID_interp)
 !
 !
 !
 ! Output
 ! 1: bands on circuit    if (BANDS_steps>0) ; 
 ! 2: dos on regular grid if (interp_grid>0) ; 
 !
 if (l_bands) then
   !
   allocate(values(4+BANDS_range(2)-BANDS_range(1)+1),headings(4+BANDS_range(2)-BANDS_range(1)+1))
   !
   headings(1)="Step"
   headings(2:4)=(/" kx  "," ky  "," kz  "/)
   do ib=BANDS_range(1),BANDS_range(2)
     headings(4+ib-BANDS_range(1)+1)=' b'//intc(ib)
   enddo
   !
   do is=1,n_sp_pol
     if(BANDS_steps> 0)     file_name(is)="bands-sp"//intc(is)
     if(all(interp_grid>0)) file_name(is)="grid-sp"//intc(is)
     call of_open_close(file_name(is),'ot')
   enddo
   !
   do is=1,n_sp_pol
     call msg('o '//file_name(is),"#",headings,INDENT=0,USE_TABS=.true.)
   enddo
   !
   call msg('o path',"#"," ",INDENT=0)
   !
   coo_out=coo_in
   !
   if(BANDS_steps> 0) then
     !
     do ik=1,CIRCUIT_K%nibz
       !
       call k_transform(CIRCUIT_K%pt(ik,:),"iku")
       values(1)=real(ik,SP)
       values(2:4)=CIRCUIT_K%pt(ik,:)
       do is=1,n_sp_pol
         values(4+1:4+BANDS_range(2)-BANDS_range(1)+1)=bands_circuit(BANDS_range(1):BANDS_range(2),is,ik)*HA2EV
         call msg('o '//file_name(is),' ',values,INDENT=0,USE_TABS=.true.)
       enddo
       !
     enddo
     ! 
     deallocate(bands_circuit)
     !
   endif
   !
   if(all(interp_grid>0)) then
     !
     do ikbz=1,INTERP_K%nbz
       !
       values(1)=real(ik,SP)
       call k_transform(INTERP_K%ptbz(ikbz,:),"iku")
       values(2:4)=INTERP_K%ptbz(ikbz,:)
       !
       ik=INTERP_K%sstar(ikbz,1)
       !
       do is=1,n_sp_pol
         values(4+1:4+BANDS_range(2)-BANDS_range(1)+1)=bands_grid(BANDS_range(1):BANDS_range(2),is,ik)*HA2EV
         call msg('o '//file_name(is),' ',values,INDENT=0,USE_TABS=.true.)
       enddo
       !
     enddo
     !
     deallocate(bands_grid)
     !
   endif
   !
   do is=1,n_sp_pol
     call of_open_close(file_name(is))
   enddo
   !
 endif
 !
 !
end subroutine
