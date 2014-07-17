!
!        Copyright (C) 2000-2014 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM DS
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
subroutine OCCUPATIONS_IO_and_interpolate(E,what,k,Time,imposed_COM)
 !
 use units,          ONLY:BO2ANG,FS2AUT,HA2EV
 use pars,           ONLY:SP,lchlen
 use parser_m,       ONLY:parser
 use real_time,      ONLY:ID,G_lesser,NE_i_time,RT_IO_JPO_steps,RT_step,MEM_index,&
&                         RT_E_occupations,RT_H_occupations,G_MEM_steps,RT_IO_steps_DB,REF_lifetime,&
&                         G_lesser_reference,RT_delta0_occupations,E_P_Hole_lifetime,&
&                         E_P_Electron_lifetime,E_P_El_abs_lifetime,E_P_Ho_abs_lifetime,&
&                         E_E_Hole_lifetime,E_E_Electron_lifetime,&
&                         RT_E_occupations_reference,RT_H_occupations_reference
 use SC,             ONLY:SC_bands,RT_nk,l_RT_occupations_in_use
 use IO_m,           ONLY:io_control,NONE,OP_WR_CL,OP_RD,RD_CL,OP_RD_CL,DUMP,variable_is_found
 use electrons,      ONLY:levels,BZ_RIM_nbands,n_sp_pol,n_met_bands,&
&                         n_full_bands,nel,E_reset,BZ_RIM_tot_nkpts,spin_occ
 use com,            ONLY:jobstr,msg,file_exists,warning,error
 use QP_m,           ONLY:OCC_ctl
 use memory_m,       ONLY:mem_est
 use R_lattice,      ONLY:bz_samp,nXkibz,bz_samp_reset
 use D_lattice,      ONLY:DL_vol,input_Tel_is_negative
 use LIVE_t,         ONLY:what_is_running
 use stderr,         ONLY:intc
 use vec_operate,    ONLY:sort
 use interfaces,     ONLY:OCCUPATIONS_Gaps
 !
 implicit none
 !
 type(levels),       intent(inout) :: E
 character(1)                      :: what
 type(bz_samp),optional,intent(in) :: k
 real(SP), optional, intent(inout) :: Time
 integer , optional, intent(in)    :: imposed_COM
 !
 ! Work Space
 !
 integer           :: ib,i1
 real(SP)          :: PE_nel
 logical           :: is_def
 !
 ! I/O
 !
 integer           :: io_err_occ,io_err_E,COM_,file_number
 character(lchlen) :: jobstr_save
 type(levels)      :: RT_db1_E
 type(bz_samp)     :: RT_db1_k
 integer, external :: io_DB1_selective_scan,io_DB1
#if defined _RT
 integer, external :: io_RT_components
#endif
 !
 ! Interpolation
 !
 real(SP), allocatable :: RT_E(:)
 integer , allocatable :: RT_E_table(:,:),RT_E_indx(:)
 integer               :: ik,Nb_to_int
 !
 ! Real-Time occupations
 !=======================
 !
 if (.not.present(Time)) then
   !
   ! (a) Write
   !
#if defined _RT
   !
   call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1/),ID=ID(6)) 
   io_err_occ=io_RT_components('occupations',ID(6))
   call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/NE_i_time+1/),ID=ID(6))
   io_err_occ=io_RT_components('occupations',ID(6))
   ! 
#endif
   ! 
 else
   !
   ! (b) Load
   !
   jobstr_save=jobstr
   if (what=="X") jobstr=trim(OCC_ctl(1))
   if (what=="K") jobstr=trim(OCC_ctl(2))
   if (what=="G") jobstr=trim(OCC_ctl(3))
   call parser(what//'fnFdb',is_def)
   if (.not.is_def) jobstr=jobstr_save
   !  
   COM_=NONE 
   if (present(imposed_COM)) then
     COM_=imposed_COM
   endif
   ! 
   if (index(what_is_running,'YPP')==0) call section('p','NEGF Occupations I/O and interpolation ('//trim(what)//')')
   ! 
   ! OCCUPATIONS I/O (I)
   !====================
   !  
#if defined _RT
   call io_control(ACTION=OP_RD,COM=COM_,SEC=(/1/),MODE=DUMP,ID=ID(6))
   io_err_occ=io_RT_components('occupations',ID(6))
#endif
   !
   if (io_err_occ<0) then
#if !defined _YPP_RT 
     call warning ('Occupations database not found in folder: '//trim(jobstr))
#endif
     jobstr=jobstr_save
     return
   else
#if !defined _YPP_RT 
     call msg('s','Occupations database read from folder: '//trim(jobstr))
#endif
   endif
   ! 
   ! DB1 I/O 
   !=========
   !
   if (index(what_is_running,'YPP')==0) then
     ! 
     call E_reset(RT_db1_E) 
     call bz_samp_reset(RT_db1_k)
     !
     io_err_E=io_DB1_selective_scan(RT_db1_E,RT_db1_k,trim(jobstr),'nk')
     if (io_err_E<0) then
       call error ('DB1 database not found in '//trim(jobstr))
     else
       io_err_E=io_DB1_selective_scan(RT_db1_E,RT_db1_k,trim(jobstr),'nb')
       !
       allocate(RT_db1_E%E(RT_db1_E%nb,RT_db1_k%nibz,n_sp_pol), RT_db1_k%pt(RT_db1_k%nibz,3))
       !
       io_err_E=io_DB1_selective_scan(RT_db1_E,RT_db1_k,trim(jobstr),'E')
       io_err_E=io_DB1_selective_scan(RT_db1_E,RT_db1_k,trim(jobstr),'K')
     endif
     !
   endif
   !
   ! OCCUPATIONS I/O (II)
   !======================
   ! 
   ! NE_time=(NE_i_time-1)*RT_step but I/O only when NE_i_time= N * RT_IO_steps
   !
   NE_i_time= (Time+RT_step)/RT_step
   NE_i_time= NE_i_time-mod(NE_i_time-1,RT_IO_JPO_steps)
   !
   if (.not.allocated(RT_delta0_occupations)) then
     allocate(RT_delta0_occupations(SC_bands(1):SC_bands(2),RT_nk))
     call mem_est("RT_delta0_occupations",(/size(RT_delta0_occupations)/),(/SP/))
   endif
   !
#if defined _RT
   call io_control(ACTION=RD_CL,COM=NONE,SEC=(/1/),MODE=DUMP,ID=ID(6))
   io_err_occ=io_RT_components('occupations',ID(6))
   call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/NE_i_time+1/),MODE=DUMP,ID=ID(6))
   io_err_occ=io_RT_components('occupations',ID(6))
#endif
   ! 
   ! REFERENCE I/O
   !===============
   !  
#if defined _RT
   if (.not.allocated(REF_lifetime)) then
     allocate(REF_lifetime(SC_bands(1):SC_bands(2),RT_nk,6))
     call mem_est("REF_lifetime",(/size(REF_lifetime)/),(/SP/))
     allocate(G_lesser_reference(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),RT_nk))
     call mem_est("Gf_l_ref",(/size(G_lesser_reference)/),(/2*SP/))
     allocate(RT_E_occupations_reference(SC_bands(1):SC_bands(2),RT_nk))
     allocate(RT_H_occupations_reference(SC_bands(1):SC_bands(2),RT_nk))
     call mem_est("RT_E_occ_ref RT_H_occ_ref",&
     &  (/size(RT_E_occupations_reference),size(RT_H_occupations_reference)/),(/SP,SP/))
     call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1,2/),ID=ID(6))
     io_err_occ=io_RT_components('REF',ID(6))
   endif
#endif
   !
   ! Add to the read occupations/lifetimes the reference part
   !
   do ik=1,RT_nk
     do ib=SC_bands(1),SC_bands(2)
       if (allocated(RT_E_occupations)) then
         RT_E_occupations(ib,ik)= RT_delta0_occupations(ib,ik)+          aimag(G_lesser_reference(ib,ib,ik))
         RT_H_occupations(ib,ik)=-RT_delta0_occupations(ib,ik)+(spin_occ-aimag(G_lesser_reference(ib,ib,ik)))
       endif
       if (allocated(E_E_Hole_lifetime)) then
         E_E_Hole_lifetime(ib,ik)=E_E_Hole_lifetime(ib,ik)+REF_lifetime(ib,ik,1)  
         E_E_Electron_lifetime(ib,ik)=E_E_Electron_lifetime(ib,ik)+REF_lifetime(ib,ik,2)  
       endif
       if (allocated(E_P_Hole_lifetime)) then
         E_P_Hole_lifetime(ib,ik)=E_P_Hole_lifetime(ib,ik)+REF_lifetime(ib,ik,3)  
         E_P_Electron_lifetime(ib,ik)=E_P_Electron_lifetime(ib,ik)+REF_lifetime(ib,ik,4)  
         E_P_Ho_abs_lifetime(ib,ik)=E_P_Ho_abs_lifetime(ib,ik)+REF_lifetime(ib,ik,5)  
         E_P_El_abs_lifetime(ib,ik)=E_P_El_abs_lifetime(ib,ik)+REF_lifetime(ib,ik,6)  
       endif
     enddo
   enddo
   !
   jobstr=jobstr_save
   !
   if (index(what_is_running,'YPP')>0) return
   !
   ! Interpolation
   !===============
   !
   allocate(RT_E(SC_bands(2)*RT_db1_k%nibz),RT_E_table(SC_bands(2)*RT_db1_k%nibz,2))
   allocate(RT_E_indx(SC_bands(2)*RT_db1_k%nibz))
   i1=0
   do ik=1,RT_db1_k%nibz
     do ib=1,SC_bands(2)
       i1=i1+1
       RT_E(i1)= RT_db1_E%E(ib,ik,1)
       RT_E_table(i1,:)= (/ib,ik/)
     enddo
   enddo
   !
   call sort(arrin=RT_E,indx=RT_E_indx)
   !
   call interpolate(E%E(:SC_bands(2),:,1),E%f(:SC_bands(2),:,1),SC_bands(2),k%nibz)
   !
   if (associated(E%f_RIM)) then
     Nb_to_int=min(SC_bands(2),BZ_RIM_nbands)
     call interpolate(E%E_RIM(:Nb_to_int,:,1),E%f_RIM(:Nb_to_int,:,1),Nb_to_int,BZ_RIM_tot_nkpts)
   endif
   !
   call mem_est("RT_delta0_occupations")
   ! 
   call E_reset(RT_db1_E) 
   call bz_samp_reset(RT_db1_k)
   !
   PE_nel=nel-sum(E%f(:n_met_bands,:,:))/real(nXkibz)
   !
   call msg('nr','Snapshot time            [fs]:',Time/FS2AUT)
   call msg(' r','Photoexcited electrons       :',PE_nel)
   call msg(' r','                          [%]:',PE_nel/nel*100._SP)
   call msg(' r','                      [cm^-3]:',PE_nel/(DL_vol*BO2ANG**3*1.E-24))
   !
   l_RT_occupations_in_use=.TRUE.
   !
   call msg('r','')
   !
   ! I need to redefine here the met/filled number of bands related to the occupations
   ! using OCCUPATIONS_gaps
   !
   if (.not.input_Tel_is_negative) call OCCUPATIONS_Gaps(E)
   !
   call msg('r','Integarted N. of e-   [input]:',sum(RT_delta0_occupations(:,:))/real(RT_nk)+(SC_bands(1)-1)*2._SP)
   call msg('r','               [interpolated]:',sum(E%f(:,:,:))/real(nXkibz))
   if (associated(E%f_RIM)) then
      call msg('r','                        [RIM]:',sum(E%f_RIM(:,:,:))/real(BZ_RIM_tot_nkpts))
   endif
   !
   n_met_bands =E%nbm
   n_full_bands=E%nbf
   !
   do ik=1,k%nibz
     call REPORT_Energies(E%E(:SC_bands(2),:,:),k,k,SC_bands(2),(/ik,ik/),      'E  ',.TRUE.)
     call REPORT_Energies(E%f(:SC_bands(2),:,:)/HA2EV,k,k,SC_bands(2),(/ik,ik/),'occ',.FALSE.)
   enddo
   !
 endif
 !
 contains
   !
   subroutine interpolate(En,Fn,Nb,Nk)
     real(SP) :: En(Nb,Nk),Fn(Nb,Nk),E_diff,E_bottom
     integer  :: Nb,Nk,ib_occ(2),ik_occ(2)
     !
     E_bottom=minval(En(:,:))
     Fn(SC_bands(1)+1:,:)=0.
     !
     do ik=1,Nk
        do ib=SC_bands(1),min(SC_bands(2),Nb)
          !
          E_diff=1000.
          !
          do i1=1,size(RT_E)
            !
            if (RT_E_table(RT_E_indx(i1),1)<SC_bands(1)) cycle
            !
            if (abs( RT_E(i1)-En(ib,ik)-RT_E(1)+E_bottom ) < E_diff) then
              !
              Fn(ib,ik)=RT_delta0_occupations( RT_E_table(RT_E_indx(i1),1), RT_E_table(RT_E_indx(i1),2) )
              !
              E_diff= abs( RT_E(i1)-En(ib,ik)-RT_E(1)+E_bottom )
              !
            endif
            !
          enddo
          !
        enddo
      enddo
      !
   end subroutine
   !
end subroutine OCCUPATIONS_IO_and_interpolate
