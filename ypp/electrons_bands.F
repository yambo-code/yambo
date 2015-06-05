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
 use electrons,      ONLY:levels,n_sp_pol,BZ_DbGd_tot_Nkpts
 use R_lattice,      ONLY:bz_samp,bz_samp_reset
 use com,            ONLY:msg,of_open_close,error
 use vec_operate,    ONLY:c2a,v_norm
 use parser_m,       ONLY:parser
 use stderr,         ONLY:intc
 use interpolate,    ONLY:eval_interpolation_coeff,bz_interpolation,reset_interpolation
 use QP_CTL_m,       ONLY:QP_apply
#if defined _YPP_RT
 use YPP,            ONLY:Nel_fac
 use stderr,         ONLY:real2ch
 use units,          ONLY:FS2AUT
 use rt_ctl,         ONLY:OCC_T_range,OCC_deltaT,RT_carriers_object
 use real_time,      ONLY:DbGd_ibz_coo,RT_delta0_occupations,REF_lifetime,&
&                         RT_Nk,RT_ibz_coo,RT_Nk_ibz,RT_carriers
#endif
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
#if defined _YPP_RT
 real(SP), allocatable :: fit_on_circuit(:,:,:,:),variable_to_fit(:,:,:)
 real(SP)              :: io_Time,Nel,Nhole,Real_Nhole,Real_Nel,Max_occ
 logical               :: l_normalize_Nel
 integer               :: i_T,n_T_steps
#endif
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
#if defined _YPP_RT
   if(all(interp_grid>0).or.BANDS_steps<=0) call error(" Interpolation of occupations/lifetimes only on a band circuit")
   !n_T_steps=RT_IO_carriers_setup(Xen,Xk)
   !call RT_IO_check_Double_Grid(Xk,Xen)
   !call RT_carriers_object(RT_carriers,Xen,Xk,WHAT='allocate')
   if(l_RT_life_Bands) n_T_steps=1
   Nk=RT_Nk
#endif
   !
 else
   !
   if(BZ_DbGd_tot_Nkpts> 0) call error(" Interpolation and Double_Grid implemented only in the _RT project")
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
#if defined _YPP_RT
   if(BZ_DbGd_tot_Nkpts>0) then
     do ik=1,Xk%nibz
       bands_grid(BANDS_range(1):BANDS_range(2),is,RT_ibz_coo(ik,1):RT_ibz_coo(ik,2))= &
&         Xen%E_DbGd(BANDS_range(1):BANDS_range(2),DbGd_ibz_coo(ik,1):DbGd_ibz_coo(ik,2),is)
     enddo
   endif
#endif
   if(BZ_DbGd_tot_Nkpts==0) bands_grid(BANDS_range(1):BANDS_range(2),is,1:Nk)=Xen%E(BANDS_range(1):BANDS_range(2),1:Nk,is)
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
#if defined _YPP_RT
 !
 Max_occ=0.
 !
 ! Occupations interpolation.
 !===========================
 !
 if(l_RT_occ_Bands.or.l_RT_life_Bands) then
   !
   call parser('NormN',l_normalize_Nel)
   !
   if(n_sp_pol>1) call error('Occupations for spin polarized systems not implemented')
   !
   allocate(variable_to_fit(BANDS_range(1):BANDS_range(2),1:n_sp_pol,1:Nk))
   !
   allocate(fit_on_circuit(BANDS_range(1):BANDS_range(2),1:n_sp_pol,CIRCUIT_k%nibz,n_T_steps))
   !
   do i_T=1,n_T_steps
     !
     if(l_RT_occ_Bands) then
       !
       io_Time=min(OCC_T_range(1)+(i_T-1)*OCC_deltaT,OCC_T_range(2))
       !
       !call RT_IO_carriers(Xen,k=Xk,Time=io_Time)
       !
       call RT_carriers_object(RT_carriers,WHAT='fill')
       !
       Real_Nhole=0._SP
       Real_Nel=0._SP
       do ik=1,Xk%nibz
         Real_Nhole=Real_Nhole-&
&         sum((RT_delta0_occupations(BANDS_range(1):Xen%nbf,RT_ibz_coo(ik,1):RT_ibz_coo(ik,2))))*&
&                        Xk%weights(ik)/real(RT_Nk_ibz(ik))
          Real_Nel=  Real_Nel+&
&         sum((RT_delta0_occupations(Xen%nbf+1:BANDS_range(2),RT_ibz_coo(ik,1):RT_ibz_coo(ik,2))))*&
&                        Xk%weights(ik)/real(RT_Nk_ibz(ik))
       enddo
       Max_occ=maxval((/Max_occ,RT_delta0_occupations(:,:)/))
       !
     endif
     !
     if(l_g_space_interp) then
       !
       ! STEP 1: define coefficients for occupations/lifetimes
       !
       if(l_RT_occ_Bands) then
         do is=1,n_sp_pol
           do ib=BANDS_range(1),BANDS_range(2)
             variable_to_fit(ib,is,ik)=RT_delta0_occupations(ib,ik)
           enddo
         enddo
       else if (l_RT_life_Bands) then
         do is=1,n_sp_pol
           do ib=BANDS_range(1),BANDS_range(2)
             variable_to_fit(ib,is,ik)=sum(REF_lifetime(ib,ik,:))
           enddo
         enddo
       endif
       !
       call eval_interpolation_coeff(R2D=variable_to_fit,k=Xk,Nk=Nk,ID=ID_interp)
       !
       ! STEP 2
       ! 1: interpolate on the circuit      if (BANDS_steps>0)
       !
       if (BANDS_steps> 0) call bz_interpolation(USER_k=CIRCUIT_k,R2D=fit_on_circuit(:,:,:,i_T),ID=ID_interp)
       !
     elseif(l_nearest_neig_interp) then
       !
       message='RT '
       if(i_T==1) message='RT0'
       !
       if(l_RT_occ_Bands)  then
         call Nearest_kpt_interpolation(RT_carriers%nk,CIRCUIT_k%nibz,RT_carriers%nb,&
&                                       RT_carriers%nstates,1,1,RT_carriers%table,RT_carriers%kpt, &
&                                       CIRCUIT_k%pt,RT_carriers%k_weight,CIRCUIT_k%weights,&
&                                       RT_carriers%delta_f,fit_on_circuit(:,:,:,i_T),message,l_normalize_Nel)
       endif
       if(l_RT_life_Bands) then
         call Nearest_kpt_interpolation(RT_carriers%nk,CIRCUIT_k%nibz,&
&                                       RT_carriers%nb,RT_carriers%nstates,1,1,RT_carriers%table,RT_carriers%kpt, &
&                                       CIRCUIT_k%pt,RT_carriers%k_weight,CIRCUIT_k%weights,&
&                                       RT_carriers%GAMMA_bare,fit_on_circuit(:,:,:,i_T),message,.false.)
       endif
       !
     endif
     !
     ! Normilize the occupations
     !
     if(l_RT_occ_Bands) then
       Nhole=-sum(fit_on_circuit(BANDS_range(1):Xen%nbf,:,:,i_T))/real(CIRCUIT_k%nibz)
       Nel=   sum(fit_on_circuit(Xen%nbf+1:BANDS_range(2),:,:,i_T))/real(CIRCUIT_k%nibz)
       !
       if(.not.l_nearest_neig_interp) then
         if(Nhole>0.and.l_normalize_Nel) &
&          fit_on_circuit(BANDS_range(1):Xen%nbf,:,:,i_T)=&
&            fit_on_circuit(BANDS_range(1):Xen%nbf,:,:,i_T)/Nhole*Real_Nhole
         if(Nel>0  .and.l_normalize_Nel) &
&          fit_on_circuit(Xen%nbf+1:BANDS_range(2),:,:,i_T)=&
&            fit_on_circuit(Xen%nbf+1:BANDS_range(2),:,:,i_T)/Nel*Real_Nel
       endif
       !
     endif
     !
     if(l_g_space_interp) call reset_interpolation(ID_interp)
     !
   enddo 
   !
   call RT_carriers_object(RT_carriers,WHAT='free')
   !
   deallocate(variable_to_fit)
   !
 endif
 !
#endif
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
#if defined _YPP_RT
 !
 if(l_RT_occ_Bands.or.l_RT_life_Bands) then
   !
   allocate(values(3),headings(3))
   !
   headings(1)="Step"
   if(l_RT_occ_Bands)  headings(3)=' occ'
   if(l_RT_life_Bands) headings(3)='LIFE'
   !
   coo_out=coo_in
   !
   do i_T=1,n_T_steps
     !
     io_Time=min(OCC_T_range(1)+(i_T-1)*OCC_deltaT,OCC_T_range(2))
     !
     if (l_RT_occ_Bands ) file_name(1)="YPP-RT_occ_bands_iT"//intc(i_T)
     if (l_RT_life_Bands) file_name(1)="YPP-RT_REF_LIFE_bands"
     !
     call of_open_close(file_name(1),'ot')
     !
     call msg('o '//file_name(1)," "," ",INDENT=0)
     call msg('o '//file_name(1),"#",' t= '//trim(real2ch( io_Time/FS2AUT )),INDENT=0)
     !
     do ib=BANDS_range(1),BANDS_range(2)
       !
       call msg('o '//file_name(1),"#"," ",INDENT=0)
       !
       headings(2)=' b'//intc(ib) 
       !
       call msg('o '//file_name(1),"#",headings,INDENT=0,USE_TABS=.true.)
       call msg('o '//file_name(1),"#"," ",INDENT=0)
       !
       do ik=1,CIRCUIT_K%nibz
         !
         values(1)=real(ik,SP)
         values(2)=bands_circuit(ib,1,ik)*HA2EV
         if(l_RT_occ_Bands)  values(3)=fit_on_circuit(ib,1,ik,i_T)/Max_occ*Nel_fac
         if(l_RT_life_Bands) values(3)=fit_on_circuit(ib,1,ik,i_T)*HA2EV*1000.
         call msg('o '//file_name(1),' ',values,INDENT=0,USE_TABS=.true.)
         !
       enddo
       !
     enddo
     !
     call of_open_close(file_name(1))
     !
   enddo
   !
   deallocate(fit_on_circuit,values,headings)
   !
 endif
 !
#endif
 !
end subroutine
