!
!        Copyright (C) 2000-2016 the YAMBO team
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
integer function io_RT_components(what,ID)
 !
 ! What can be E WF V J_and_P G
 !
 use pars,           ONLY:SP,lchlen,schlen
 use drivers,        ONLY:l_elph_scatt
 use R_lattice,      ONLY:nkibz,nqbz
 use electrons,      ONLY:spin_occ
 use stderr,         ONLY:intc
 use SC,             ONLY:SC_xc_functional,V_hartree_sc,l_NSC_shot,SC_fft_size,V_mean
 use IO_m,           ONLY:io_connect,io_disconnect,io_header,io_elemental,io_status,        &
&                         io_sec,io_bulk,read_is_on,write_is_on,IO_NO_DATABASE,io_fragment, &
&                         manage_RD_WR_CL_IF_END,variable_is_found,&
&                         frag_RT
 use real_time,      ONLY:NE_i_time,REF_lifetime,RT_step,&
&                         RT_E_occupations_reference,RT_H_occupations_reference,&
&                         l_NE_with_fields,NE_steps,RT_nk, &
&                         dG_lesser,RT_carriers,RT_obj_alloc,&
&                         G_lesser_reference,G_MEM_steps,THETA_matrix,q_weight_RT,&
&                         RT_bands,RT_nbands,MEM_pointer
 use ELPH,           ONLY:ph_modes,elph_branches
 use RT_control,     ONLY:J_and_P_cache_size,J_and_P_cache,JPC_RT_IO_t,RT_carriers_object,RT_IO_t,&
&                         GF_RT_IO_t,TIME_string,J_and_P_cache_steps,J_and_P_cache_steps
 use fields,         ONLY:A_ext
 use matrix_operate, ONLY:mat_c2r,mat_r2c
#if defined _TIMING
 use timing_m,       ONLY:timing
#endif
 implicit none
 !
 integer,      intent(in) :: ID
 character(*), intent(in) :: what
 !
 ! Work Space
 !
 integer, external     :: variables_SC,variables_RT
 character(lchlen)     :: local_description(100),db_name
 character(schlen)     :: local_what,file_extension
 !
 real(SP), allocatable :: TMP_matrix(:,:,:)
 integer               :: ik,ib,i_spin,i_memory,db_type,i_Probe,i_Pump
 logical               :: l_V,l_IMPOSE_SN,CHECK_,l_G,l_J_and_P,l_T_EL,l_THETA,l_REF,l_carriers
 real(SP)              :: A_vecpot_disk(3,3,2)
 !
#if defined _TIMING
 call timing('io_RT_components',OPR='start')
#endif
 !
 l_carriers   =what=="carriers"
 l_THETA      =what=="THETA"
 l_J_and_P    =what=="J_and_P"
 l_G          =what=="G"
 l_REF        =what=="REF"
 !
 db_name="RT_"//trim(what)
 local_what=what
 db_type=2
 if (.not.any((/l_carriers,l_THETA,l_J_and_P,l_G,l_REF/))) then
   db_name=trim(what)
   local_what='carriers'
   l_carriers=.TRUE.
   db_type=-3
 endif
 !
 if (l_G) ik = maxval(io_sec(ID,:))-1
 !
 if (l_REF) db_name="RT_reference_components"
 !
 file_extension=TIME_string( (NE_i_time-1)*RT_step )
 !
 ! When the db.scV is used for single non-SC shot the number
 ! of bands and SC_mixing can be not respected. 
 !
 CHECK_=.TRUE.
 !
 ! Write only every RT_io_steps
 !
 if (l_J_and_P.or.l_carriers) then
   if (io_sec(ID,1)==1) then 
     io_RT_components=io_connect(desc=trim(db_name),type=db_type,ID=ID)
   else
     if (l_J_and_P)     io_RT_components=io_connect(desc=trim(db_name)//"_Time_"//trim(file_extension),type=db_type,ID=ID)
     if (l_carriers) then
       io_RT_components=io_connect(desc=trim(db_name)//"_Time_"//trim(file_extension),type=db_type,ID=ID,no_full_RESET=.TRUE.)
       if (read_is_on(ID).and.io_RT_components==IO_NO_DATABASE) then
         io_RT_components=io_connect(desc=trim(db_name),type=db_type,ID=ID)
       endif
     endif
   endif
 endif
 !
 if (l_G) then
   !
   io_RT_components=io_connect(desc=trim(db_name),type=db_type,ID=ID)
   !
 endif
 !
 if (l_THETA) io_RT_components=io_connect(desc="RT_THETA",type=db_type,ID=ID)
 !
 if (l_REF.and.io_sec(ID,1)==1) io_RT_components=io_connect(desc=trim(db_name),type=db_type,ID=ID)
 !
 if (io_RT_components/=0) goto 1
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   l_IMPOSE_SN=.not.l_V
   if (l_carriers) l_IMPOSE_SN=.FALSE.
   l_T_EL=.false.
#if defined _ELPH
   l_T_EL=.true.
#endif
   !
   if (any((/l_J_and_P,l_G,l_THETA,l_REF/))) then
     if (write_is_on(ID).or.variable_is_found(ID,"TEMPERATURES")/=0) then
       io_RT_components=io_header(ID,IMPOSE_SN=l_IMPOSE_SN,CUTOFF=.true.,T_EL=l_T_EL,FRAG=frag_RT)
     else
       io_RT_components=io_header(ID,IMPOSE_SN=l_IMPOSE_SN,CUTOFF=.true.,FRAG=frag_RT)
     endif
   endif
   !
   if (l_carriers) then
     if (write_is_on(ID).or.(read_is_on(ID).and.variable_is_found(ID,"TEMPERATURES")/=0) ) then
       io_RT_components=io_header(ID,IMPOSE_SN=l_IMPOSE_SN,CUTOFF=.true.,T_EL=l_T_EL,FRAG=frag_RT)
     endif
   endif
   !
   if (io_RT_components/=0) goto 1
   !
   io_RT_components=variables_SC(ID,local_description,trim(local_what))
   if (io_RT_components/=0) goto 1
   !
   io_RT_components=variables_RT(ID,local_description,trim(local_what))
   if (io_RT_components/=0) goto 1
   !
   if (l_G.or.l_carriers.or.l_THETA) then
     call io_elemental(ID,VAR="Probe_and_Pump_index",VAR_SZ=2,MENU=0) 
     call io_elemental(ID,I0=i_Probe)
     call io_elemental(ID,I0=i_Pump)
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   endif
   !
   if (l_THETA) then
     call io_bulk(ID,VAR="Q_weight",VAR_SZ=(/nqbz/))
     call io_bulk(ID,R1=q_weight_RT(:,1,1))
   endif
   !
   if (l_G) then
     call io_bulk(ID,VAR="MEMORY_TIME_POINTS",VAR_SZ=(/G_MEM_steps/))
     call io_bulk(ID,I1=MEM_pointer)
   endif
   !
   ! Here I dump all RELEVANT RT_carriers% components that make the RT_carriers databases
   ! independent
   !
   if (l_carriers) then
     call io_elemental(ID,VAR="RT_carriers_dimensions",VAR_SZ=4,MENU=0) 
     call io_elemental(ID,I0=RT_carriers%nk)
     call io_elemental(ID,I1=RT_carriers%nb)
     call io_elemental(ID,I0=RT_carriers%nstates)
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
     !
     if (read_is_on(ID)) call RT_obj_alloc(RT_carriers)
     !
     call io_bulk(ID,VAR="RT_kpt",VAR_SZ=(/RT_carriers%nk,3/))
     call io_bulk(ID,R2=RT_carriers%kpt)
     call io_bulk(ID,VAR="RT_k_weight",VAR_SZ=(/RT_carriers%nk/))
     call io_bulk(ID,R1=RT_carriers%k_weight)
     call io_bulk(ID,VAR="RT_table",VAR_SZ=(/RT_carriers%nstates,4/))
     call io_bulk(ID,I2=RT_carriers%table)
   endif
   !
   ! I/O time points
   !
   if (l_J_and_P.or.l_carriers) call LOCAL_IO(JPC_RT_IO_t)
   if (l_G)                     call LOCAL_IO(GF_RT_IO_t)
   !
   io_RT_components=io_status(ID)
   if (io_RT_components/=0) goto 1
   !
 endif
 ! 
 if(l_J_and_P.or.l_carriers) then
   call manage_RD_WR_CL_IF_END(ID,NE_i_time,NE_steps)
 else if (l_G) then
   call manage_RD_WR_CL_IF_END(ID,ik,RT_nk)
 else
   call manage_RD_WR_CL_IF_END(ID,ik,nkibz)
 endif
 !
 if(any((/l_G,l_THETA/))) then
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
 if (l_REF .and. ik==0) goto 1
 !
 if ( l_carriers ) then
   !
   ! Carrier components
   !====================
   !
   if (io_RT_components/=0) goto 1  
   !
   if (io_sec(ID,1)>1) then
     !
     call RT_carriers_object(RT_carriers,DB_id=ID,WHAT="input_output")
     !
     if (read_is_on(ID)) call RT_carriers_object(RT_carriers,WHAT="dump")
     !
   endif
   !
 endif
 !
 if ( l_THETA ) then
   !
   ! ELPH Kernel THETA Matrix 
   !==========================
   !
   if (io_RT_components/=0) goto 1  
   !
   allocate(TMP_matrix(2*nqbz*ph_modes,RT_bands(2)-RT_bands(1)+1,2))
   !
   do ib=1,RT_bands(2)-RT_bands(1)+1
     if (write_is_on(ID)) call mat_c2r(THETA_matrix(:,:,ib,ik),TMP_matrix)
     call io_bulk(ID,VAR="RT_THETA_B"//trim(intc(ib))//"_K"//trim(intc(ik)),VAR_SZ=(/2*nqbz*ph_modes,RT_bands(2)-RT_bands(1)+1,2/))
     call io_bulk(ID,R3=TMP_matrix)
     if (read_is_on(ID))  call mat_r2c(TMP_matrix,THETA_matrix(:,:,ib,ik))
   enddo
   !
   deallocate(TMP_matrix)
   !
 endif
 !
 if ( l_G ) then
   !
   ! lesser Green Function
   !=======================
   !
   if (io_RT_components/=0) goto 1  
   !
   allocate(TMP_matrix(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),2))
   !
   i_spin=1
   !
   do i_memory=1,G_MEM_steps
     !
     call io_bulk(ID,VAR="dG_lesser_K"//trim(intc(ik))//"_SPIN"//trim(intc(i_spin))//"_IMEMORY"//trim(intc(i_memory)),&
&                 VAR_SZ=(/RT_nbands,RT_nbands,2/))
     !
     if (write_is_on(ID)) call mat_c2r(dG_lesser(:,:,ik,i_memory),TMP_matrix)
     call io_bulk(ID,R3=TMP_matrix)
     if (read_is_on(ID))  call mat_r2c(TMP_matrix,dG_lesser(:,:,ik,i_memory))
     !
   enddo
   !
   if(l_NE_with_fields.and.ik==RT_nk) then
     call io_bulk(ID,VAR="A_vecpot",VAR_SZ=(/3,3,2/))
     if (write_is_on(ID)) then
       A_vecpot_disk(:,1,1)= real(A_ext%vecpot(:),SP)
       A_vecpot_disk(:,1,2)=aimag(A_ext%vecpot(:))
       A_vecpot_disk(:,2,1)= real(A_ext%vecpot_vel(:),SP)
       A_vecpot_disk(:,2,2)=aimag(A_ext%vecpot_vel(:))
       A_vecpot_disk(:,3,1)= real(A_ext%vecpot_acc(:),SP)
       A_vecpot_disk(:,3,2)=aimag(A_ext%vecpot_acc(:))
     endif
     call io_bulk(ID,R3=A_vecpot_disk)
     if (read_is_on(ID)) then 
       A_ext%vecpot(:)    =cmplx(A_vecpot_disk(:,1,1),A_vecpot_disk(:,1,2),SP)
       A_ext%vecpot_vel(:)=cmplx(A_vecpot_disk(:,2,1),A_vecpot_disk(:,2,2),SP)
       A_ext%vecpot_acc(:)=cmplx(A_vecpot_disk(:,3,1),A_vecpot_disk(:,3,2),SP)
     endif
   endif
   !
   deallocate(TMP_matrix)
   !
 endif
 !
 if ( l_REF ) then
   !
   ! REFERENCE Lifetimes
   !====================
   !
   if (io_RT_components/=0) goto 1  
   !
   if (l_elph_scatt) then
     !
     if (read_is_on(ID)) then
       if(allocated(q_weight_RT)) deallocate(q_weight_RT)
       allocate(q_weight_RT(nqbz,elph_branches(1):elph_branches(2),2))
     endif
     !
     call io_bulk(ID,VAR="Q_WEIGHT",VAR_SZ=(/nqbz,elph_branches(2)-elph_branches(1)+1,2/))
     call io_bulk(ID,R3=q_weight_RT(:,:,:))
     !
   endif
   !
   if (allocated(REF_lifetime)) then
     !
     call io_bulk(ID,VAR="RT_E_E_REF_Hole_lifetime",VAR_SZ=(/RT_bands(2)-RT_bands(1)+1,RT_nk/))
     call io_bulk(ID,R2=REF_lifetime(:,:,1))
     !
     call io_bulk(ID,VAR="RT_E_E_REF_Electron_lifetime",VAR_SZ=(/RT_bands(2)-RT_bands(1)+1,RT_nk/))
     call io_bulk(ID,R2=REF_lifetime(:,:,2))
     !
     call io_bulk(ID,VAR="RT_E_P_REF_Hole_lifetime",VAR_SZ=(/RT_bands(2)-RT_bands(1)+1,RT_nk/))
     call io_bulk(ID,R2=REF_lifetime(:,:,3))
     !
     call io_bulk(ID,VAR="RT_E_P_REF_Electron_lifetime",VAR_SZ=(/RT_bands(2)-RT_bands(1)+1,RT_nk/))
     call io_bulk(ID,R2=REF_lifetime(:,:,4))
     !
     call io_bulk(ID,VAR="RT_E_P_REF_Ho_abs_lifetime",VAR_SZ=(/RT_bands(2)-RT_bands(1)+1,RT_nk/))
     call io_bulk(ID,R2=REF_lifetime(:,:,5))
     !
     call io_bulk(ID,VAR="RT_E_P_REF_El_abs_lifetime",VAR_SZ=(/RT_bands(2)-RT_bands(1)+1,RT_nk/))
     call io_bulk(ID,R2=REF_lifetime(:,:,6))
     !
     call io_bulk(ID,VAR="RT_E_PHOT_REF_Hole_abs_lifetime",VAR_SZ=(/RT_bands(2)-RT_bands(1)+1,RT_nk/))
     call io_bulk(ID,R2=REF_lifetime(:,:,7))
     !
     call io_bulk(ID,VAR="RT_E_PHOT_REF_El_abs_lifetime",VAR_SZ=(/RT_bands(2)-RT_bands(1)+1,RT_nk/))
     call io_bulk(ID,R2=REF_lifetime(:,:,8))
     !
   endif
   !
   if (allocated(G_lesser_reference)) then
     !
     allocate(TMP_matrix(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),2))
     !
     i_spin=1
     !
     do ik=1,RT_nk
       call io_bulk(ID,VAR="G_lesser_ref_K"//trim(intc(ik))//"_SPIN"//trim(intc(i_spin)),&
&                      VAR_SZ=(/RT_nbands,RT_nbands,2/))
       if (write_is_on(ID)) call mat_c2r(G_lesser_reference(:,:,ik),TMP_matrix)
       call io_bulk(ID,R3=TMP_matrix)
       if (read_is_on(ID))  call mat_r2c(TMP_matrix,G_lesser_reference(:,:,ik))
     enddo
     !
     deallocate(TMP_matrix)
     !
     if (read_is_on(ID)) then
       do ib=RT_bands(1),RT_bands(2)
         RT_E_occupations_reference(ib,:)=         aimag(G_lesser_reference(ib,ib,:))
         RT_H_occupations_reference(ib,:)=spin_occ-aimag(G_lesser_reference(ib,ib,:))
       enddo
     endif
     !
   endif
   !
 endif
 !
 if ( l_J_and_P ) then
   !
   ! Current and Polarization 
   !==========================
   !        
   if (io_sec(ID,1)>1) then
     !
     call io_elemental(ID,VAR="J_and_P_CACHE_TIME_N_points",VAR_SZ=1,MENU=0) 
     call io_elemental(ID,I0=J_and_P_cache_steps)
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
     !
     if (.not.allocated(J_and_P_cache)) then    
       allocate(J_and_P_cache(J_and_P_cache_size,J_and_P_cache_steps))
     endif
     !
     call io_bulk(ID,VAR="Current_Polarization_and_Potential_TIME"//trim(file_extension), &
&                    VAR_SZ=(/J_and_P_cache_size,J_and_P_cache_steps/))
     !
     call io_bulk(ID,R2=J_and_P_cache)
     !
     call io_disconnect(ID)
     !                       
   endif
   !
 endif       
 !
1 call io_disconnect(ID=ID)
 !
2 continue
 !
#if defined _TIMING
 call timing('io_RT_components',OPR='stop')
#endif
 !
 contains
   !
   subroutine LOCAL_IO(TYP)
     type(RT_IO_t) :: TYP
     call io_elemental(ID,VAR="IO_TIME_N_points",VAR_SZ=1,MENU=0) 
     call io_elemental(ID,I0=TYP%N)
     if (.not.allocated(TYP%Time)) then    
       allocate(TYP%Time(NE_steps))
     endif
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
     call io_bulk(ID,VAR="IO_TIME_points",VAR_SZ=(/NE_steps/))
     call io_bulk(ID,R1=TYP%Time)
   end subroutine
   !
end function
