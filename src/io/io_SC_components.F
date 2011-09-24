!
! Copyright (C) 2000-2010 A. Marini and the YAMBO team 
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
integer function io_SC_components(what,E_sc,ID)
 !
 ! what can be E WF V J_and_P G
 !
 use pars,           ONLY:SP,lchlen
 use R_lattice,      ONLY:nkibz
 use stderr,         ONLY:intc
 use SC,             ONLY:SC_bands,rho_convergence,E_convergence,&
&                         SC_R,V_xc_sc,local_V,H_nl_sc,RT_k,RT_nk,RT_occupations,&
&                         SC_xc_functional,V_hartree_sc,l_NSC_shot,SC_fft_size
 use IO_m,           ONLY:io_connect,io_disconnect,io_header,io_elemental,io_status,&
&                         io_sec,io_bulk,read_is_on,write_is_on,&
&                         manage_RD_WR_CL_IF_END,io_reset
 use fields,         ONLY:i_Probe,i_Pump
 use real_time,      ONLY:max_rt_steps_disk,NE_i_time
#if defined _RT
 use real_time,      ONLY:l_NE_with_fields,NE_steps,RT_P,J_and_P_cache_size, &
&                         G_lesser,NE_dynamics,RT_ind_J,J_and_P_cache,&
&                         G_lesser_reference,P_2levels_is_done,RT_P_2levels
 use fields,         ONLY:A_vecpot
 use QP_m,           ONLY:QP_nk
#endif                                
 use matrix_operate, ONLY:mat_c2r,mat_r2c
 use electrons,      ONLY:levels,n_sp_pol,n_spin
 use fragments,      ONLY:io_fragment
 implicit none
 !
 integer               :: ID
 type(levels)          :: E_sc
 character(*)          :: what
 !
 ! Work Space
 !
 integer, external     :: variables_SC
 character(lchlen)     :: local_description(100)
 !
 real(SP), allocatable :: R_H_G_disk(:,:,:),E_disk(:,:,:)
 integer               :: ik,bands_to_load,ib,i_spin,i1
 logical               :: l_wf,l_V,l_E,l_IMPOSE_SN,CHECK_,l_occupations,l_G,l_J_and_P
#if defined _RT		
 integer               :: file_number
 logical               :: IO_current
 real(SP)              :: A_vecpot_disk(3,3,2)
#endif
 !
 l_E =what=="E"
 l_wf=what=="WF"
 l_V =what=="V"
 l_G =.FALSE.
 l_J_and_P =.FALSE.
 l_occupations=what=="occupations"
#if defined _RT		
 l_J_and_P    =what=="J_and_P"
 l_G          =what=="G"
#endif
 !
 ! When the db.scV is used for single non-SC shot the number
 ! of bands and SC_mixing can be not respected. 
 !
 CHECK_=.TRUE.
 if (l_NSC_shot) CHECK_=.FALSE.
 !
 if (l_wf) io_SC_components=io_connect(desc="scWFs",type=2,ID=ID)
 if (l_E)  io_SC_components=io_connect(desc="scE",type=2,ID=ID)
 if (l_V)  io_SC_components=io_connect(desc="scV",type=2,ID=ID)
 !
#if defined _RT
 !
 ! Write only every max_rt_steps_disk
 !
 if (l_J_and_P) then
   !      
   file_number=(NE_i_time-1)/max_rt_steps_disk
   !      
   if(io_sec(ID,1)==1) then 
     io_SC_components=io_connect(desc="RT_J_and_P",type=2,ID=ID)
   else
     !      
     IO_current=.false.
     if(mod(NE_i_time,max_rt_steps_disk)  ==0.and.write_is_on(ID))  IO_current=.true.
     if(mod(NE_i_time-1,max_rt_steps_disk)==0.and.read_is_on(ID))   IO_current=.true.
     !
     if(IO_current) then
       io_SC_components=io_connect(desc="RT_J_and_P_"//trim(intc(file_number)),type=2,ID=ID)
     else
       io_SC_components=0
     endif
     !
   endif
   !
 endif
 !
 if(l_G.or.l_occupations) then
   if( (mod(NE_i_time,max_rt_steps_disk)/=0.and.NE_i_time/=1) .and.NE_dynamics.and.write_is_on(ID)) then
     io_SC_components=0
     call io_reset(ID)
     return
   endif
 endif
 if (l_G)           io_SC_components=io_connect(desc="RT_GreenF",type=2,ID=ID)
 !
#endif
 !
 if (l_occupations) io_SC_components=io_connect(desc="RT_occupations",type=2,ID=ID)
 !
 if (io_SC_components/=0) goto 1
 !
 ik = maxval(io_sec(ID,:))-1
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   l_IMPOSE_SN=.not.l_V 
#if defined _SC
   if (l_occupations) l_IMPOSE_SN=.FALSE.
#endif
   !
   if (any((/l_E,l_V,l_wf,l_occupations,&
&            l_J_and_P,l_G/)))  io_SC_components=io_header(ID,IMPOSE_SN=l_IMPOSE_SN,CUTOFF=.true.)
   !
   if (io_SC_components/=0) goto 1
   !
   io_SC_components=variables_SC(ID,local_description,what)
   !=======================================================
   if (io_SC_components/=0) goto 1
   !
   if(any((/l_E,l_V,l_wf/))) then
     !
     call io_elemental(ID,VAR="RHOCONV_ECONV",VAR_SZ=2,MENU=0) 
     call io_elemental(ID,R0=rho_convergence)
     call io_elemental(ID,R0=E_convergence)
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
     !
   endif
   !
   if (l_G.or.l_occupations) then
     call io_elemental(ID,VAR="Probe_and_Pump_index",VAR_SZ=2,MENU=0) 
     call io_elemental(ID,I0=i_Probe)
     call io_elemental(ID,I0=i_Pump)
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   endif
#if defined _RT
   if (l_J_and_P) then
     call io_elemental(ID,VAR="P2l_is_done",VAR_SZ=1,MENU=0) 
     call io_elemental(ID,L0=P_2levels_is_done)
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   endif
#endif
   !
   io_SC_components=io_status(ID)
   if (io_SC_components/=0) goto 1
   !
 endif
 ! 
#if defined _RT
 if(l_J_and_P) then
   call manage_RD_WR_CL_IF_END(ID,ik,NE_steps)
 else
#endif
   call manage_RD_WR_CL_IF_END(ID,ik,nkibz)
#if defined _RT
 endif
#endif
 !
 ! Potential
 !===========

 !
 if (l_V.and.local_V.and.ik==0.and.allocated(V_xc_sc)) then
   !
   call io_bulk(ID,VAR="V_xc_sc",VAR_SZ=(/SC_fft_size,n_spin/))
   call io_bulk(ID,R2=V_xc_sc)
   !
   call io_bulk(ID,VAR="V_hartree",VAR_SZ=(/SC_fft_size,n_sp_pol/))
   call io_bulk(ID,R2=V_hartree_sc)
   !
 endif
 !
 if ( l_E.and.associated(E_sc%E)) then
   !
   ! Energies
   !==========
   !
   call io_bulk(ID,VAR="E_sc",VAR_SZ=(/SC_bands(2),nkibz,n_sp_pol/))
   if (write_is_on(ID)) then
     call io_bulk(ID,R3=E_sc%E(:SC_bands(2),:,:))
   else
     allocate(E_disk(SC_bands(2),nkibz,n_sp_pol))
     call io_bulk(ID,R3=E_disk)
     !
     bands_to_load=size(E_sc%E,1)
     !
     if (bands_to_load>SC_bands(2)) then
       !
       ! For either the H_sc and WFs I first check to 
       ! have enough bands (in wf_load and X_O_strengths).
       ! For the energies instead I need to shift the levels > SC_bands(2) (if any).
       !
       forall (ib=SC_bands(2)+1:bands_to_load) E_sc%E(ib,:,:)=E_sc%E(ib,:,:)-E_sc%E(SC_bands(2),:,:)
       E_sc%E(:SC_bands(2),:,:)=E_disk(:SC_bands(2),:,:)
       forall (ib=SC_bands(2)+1:bands_to_load) E_sc%E(ib,:,:)=E_sc%E(ib,:,:)+E_sc%E(SC_bands(2),:,:)
       !
     else
       E_sc%E(:bands_to_load,:,:)=E_disk(:bands_to_load,:,:)
     endif
     deallocate(E_disk)
   endif
   !
 endif
 !
 if(any((/l_E,l_V,l_wf,l_G,l_occupations/))) then
   !
   if (ik==0) goto 1
   !
   ! Fragmentation
   !===============
   !
   call io_fragment(ID,i_fragment=ik)
   !
 endif
 !
 if (l_V.and..not.local_V) then
   !
   ! H_sc
   !======
   !
   allocate(R_H_G_disk(SC_bands(2),SC_bands(2),2))
   !
   do i_spin=1,n_sp_pol
     !
     call io_bulk(ID,VAR="H_nl_sc_K"//trim(intc(ik))//"_SPIN"//trim(intc(i_spin)),&
&                 VAR_SZ=(/SC_bands(2),SC_bands(2),2/))
     !
     if (write_is_on(ID)) call mat_c2r(H_nl_sc(:,:,i_spin),R_H_G_disk)
     call io_bulk(ID,R3=R_H_G_disk)
     if (read_is_on(ID))  call mat_r2c(R_H_G_disk,H_nl_sc(:,:,i_spin))
     !
   enddo
   !
   deallocate(R_H_G_disk)
   !
 endif
 !
 if ( l_WF ) then
   !
   ! Rotation matrix 
   !=================
   !
   allocate(R_H_G_disk(SC_bands(2),SC_bands(2),2))
   !
   do i_spin=1,n_sp_pol
     call io_bulk(ID,VAR="Rotation_Matrix_K"//trim(intc(ik))//"_SPIN"//trim(intc(i_spin)),&
&                 VAR_SZ=(/SC_bands(2),SC_bands(2),2/))
     !
     if (write_is_on(ID)) call mat_c2r(SC_R(:,:,ik,i_spin),R_H_G_disk)
     call io_bulk(ID,R3=R_H_G_disk)
     if (read_is_on(ID))  call mat_r2c(R_H_G_disk,SC_R(:,:,ik,i_spin))
     !
   enddo
   deallocate(R_H_G_disk)
 endif
 !
 if ( l_occupations ) then
   !
   ! lesser Green Function diagonal
   !===============================
   !
   if (io_SC_components/=0) goto 1  
   !
   if (NE_i_time==max_rt_steps_disk.or.read_is_on(ID)) then
     call io_bulk(ID,VAR="RT_Kpts",VAR_SZ=(/RT_nk,3/))
     call io_bulk(ID,R2=RT_k)
   endif
   !
   call io_bulk(ID,VAR="RT_occupations_TIME"//trim(intc(NE_i_time)),VAR_SZ=(/SC_bands(2),RT_nk/))
   call io_bulk(ID,R2=RT_occupations)
   !
 endif
 !
#if defined _RT
 !
 if ( l_G ) then
   !
   ! lesser Green Function
   !=======================
   !
   if (io_SC_components/=0) goto 1  
   !
   allocate(R_H_G_disk(SC_bands(2),SC_bands(2),2))
   !
   i_spin=1
   !
   call io_bulk(ID,VAR="G_lesser_K"//trim(intc(ik))//"_SPIN"//trim(intc(i_spin)),&
&       VAR_SZ=(/SC_bands(2),SC_bands(2),2/))
   !
   if (write_is_on(ID)) call mat_c2r(G_lesser(:,:,ik),R_H_G_disk)
   call io_bulk(ID,R3=R_H_G_disk)
   if (read_is_on(ID))  call mat_r2c(R_H_G_disk,G_lesser(:,:,ik))
   !
   call io_bulk(ID,VAR="G_lesser_ref_K"//trim(intc(ik))//"_SPIN"//trim(intc(i_spin)),&
&       VAR_SZ=(/SC_bands(2),SC_bands(2),2/))
   if (write_is_on(ID)) call mat_c2r(G_lesser_reference(:,:,ik),R_H_G_disk)
   call io_bulk(ID,R3=R_H_G_disk)
   if (read_is_on(ID))  call mat_r2c(R_H_G_disk,G_lesser_reference(:,:,ik))
   !
   if(l_NE_with_fields.and.ik==QP_nk) then
     call io_bulk(ID,VAR="A_vecpot",VAR_SZ=(/3,3,2/))
     if (write_is_on(ID)) then
       A_vecpot_disk(:,1,1)= real(A_vecpot%vecpot(:),SP)
       A_vecpot_disk(:,1,2)=aimag(A_vecpot%vecpot(:))
       A_vecpot_disk(:,2,1)= real(A_vecpot%vecpot_vel(:),SP)
       A_vecpot_disk(:,2,2)=aimag(A_vecpot%vecpot_vel(:))
       A_vecpot_disk(:,3,1)= real(A_vecpot%vecpot_acc(:),SP)
       A_vecpot_disk(:,3,2)=aimag(A_vecpot%vecpot_acc(:))
     endif
     call io_bulk(ID,R3=A_vecpot_disk)
     if (read_is_on(ID)) then 
       A_vecpot%vecpot(:)    =cmplx(A_vecpot_disk(:,1,1),A_vecpot_disk(:,1,2),SP)
       A_vecpot%vecpot_vel(:)=cmplx(A_vecpot_disk(:,2,1),A_vecpot_disk(:,2,2),SP)
       A_vecpot%vecpot_acc(:)=cmplx(A_vecpot_disk(:,3,1),A_vecpot_disk(:,3,2),SP)
     endif
   endif
   !
   deallocate(R_H_G_disk)
   !
 endif
 !
 if ( l_J_and_P ) then
   !
   ! Current and Vector Potential
   !==============================
   !        
   if(io_sec(ID,1)/=1) then
     !
     if(write_is_on(ID)) then
       !
       do i1=1,3
         J_and_P_cache( 2*i1-1,  mod(NE_i_time-1,max_rt_steps_disk)+1)= real(RT_ind_J(i1))
         J_and_P_cache( 2*i1,    mod(NE_i_time-1,max_rt_steps_disk)+1)=aimag(RT_ind_J(i1))
         J_and_P_cache(6+2*i1-1, mod(NE_i_time-1,max_rt_steps_disk)+1)= real(RT_P(i1))
         J_and_P_cache(6+2*i1,   mod(NE_i_time-1,max_rt_steps_disk)+1)=aimag(RT_P(i1))
         if (P_2levels_is_done) then
           J_and_P_cache(12+2*i1-1, mod(NE_i_time-1,max_rt_steps_disk)+1)= real(RT_P_2levels(i1))
           J_and_P_cache(12+2*i1,   mod(NE_i_time-1,max_rt_steps_disk)+1)=aimag(RT_P_2levels(i1))
         else
           J_and_P_cache(12+2*i1-1, mod(NE_i_time-1,max_rt_steps_disk)+1)= 0.
           J_and_P_cache(12+2*i1,   mod(NE_i_time-1,max_rt_steps_disk)+1)= 0.
         endif
       enddo
       !
     endif
     !
     if (IO_current) then
       !                                
       call io_bulk(ID,VAR="Current_Polarization_and_Potential_TIME"//trim(intc(file_number)), &
&                      VAR_SZ=(/J_and_P_cache_size,max_rt_steps_disk/))
       !
       call io_bulk(ID,R2=J_and_P_cache)
       !
       call io_disconnect(ID)
       !
     endif
     !
     if(read_is_on(ID)) then
       !
       ! Read from cache
       !
       !
       do i1=1,3
         RT_ind_J(i1)           =cmplx(J_and_P_cache(   2*i1-1,mod(NE_i_time-1,max_rt_steps_disk)+1), &
&                                      J_and_P_cache(   2*i1,  mod(NE_i_time-1,max_rt_steps_disk)+1),SP)
         RT_P(i1)               =cmplx(J_and_P_cache( 6+2*i1-1,mod(NE_i_time-1,max_rt_steps_disk)+1), &
&                                      J_and_P_cache( 6+2*i1  ,mod(NE_i_time-1,max_rt_steps_disk)+1),SP)
         RT_P_2levels(i1)       =cmplx(J_and_P_cache(12+2*i1-1,mod(NE_i_time-1,max_rt_steps_disk)+1), &
&                                      J_and_P_cache(12+2*i1  ,mod(NE_i_time-1,max_rt_steps_disk)+1),SP)
       enddo
       !
     endif 
     !
     if(.not.IO_current) call io_reset(ID) 
     !
   endif
   !
 endif       
 !
#endif
 !
1 call io_disconnect(ID=ID)
 !
end function
