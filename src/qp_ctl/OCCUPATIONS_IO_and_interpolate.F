!
! Copyright (C) 2000-2012 A. Marini and the YAMBO team 
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
subroutine OCCUPATIONS_IO_and_interpolate(E,what,k,Time,imposed_COM)
 !
 use units,          ONLY:BO2ANG,FS2AUT,HA2EV
 use pars,           ONLY:SP,lchlen
 use parser_m,       ONLY:parser
 use real_time,      ONLY:ID,G_lesser,NE_i_time,RT_IO_steps,RT_step,MEM_index
 use SC,             ONLY:SC_bands,RT_occupations,RT_nk,RT_k,l_RT_occupations_in_use
 use IO_m,           ONLY:io_control,NONE,OP_WR_CL,OP_RD,RD_CL,OP_RD_CL,DUMP,variable_is_found
 use electrons,      ONLY:levels,BZ_RIM_nbands,n_sp_pol,n_met_bands,&
&                         n_full_bands,nel,E_reset,BZ_RIM_tot_nkpts
 use com,            ONLY:jobstr,msg,file_exists,warning,error
 use QP_m,           ONLY:OCC_ctl
 use memory_m,       ONLY:mem_est
 use R_lattice,      ONLY:bz_samp,nXkibz,bz_samp_reset
 use D_lattice,      ONLY:DL_vol,input_Tel_is_negative
 use timing,         ONLY:what_is_running
 use stderr,         ONLY:intc
 use vec_operate,    ONLY:sort
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
 integer, external :: ioDB1_selective_scan,ioDB1,io_SC_components
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
   do ib=SC_bands(1),SC_bands(2)
     RT_occupations(ib,:)=-real((0._SP,1._SP)*G_lesser(ib,ib,:,MEM_index(NE_i_time)))
   enddo
   ! 
   call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1/),ID=ID(6)) 
   io_err_occ=io_SC_components('occupations',E,ID(6))
   call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/NE_i_time+1/),ID=ID(6))
   io_err_occ=io_SC_components('occupations',E,ID(6))
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
   if (.not.is_def) then
     jobstr=jobstr_save
     return
   endif
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
   call io_control(ACTION=OP_RD,COM=COM_,SEC=(/1/),MODE=DUMP,ID=ID(6))
   io_err_occ=io_SC_components('occupations',E,ID(6))
   !
   if (io_err_occ<0) then
     call warning ('Occupations database not found in folder: '//trim(jobstr))
     jobstr=jobstr_save
     return
   else
     call msg('s','Occupations database read from folder: '//trim(jobstr))
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
     io_err_E=ioDB1_selective_scan(RT_db1_E,RT_db1_k,trim(jobstr),'nk')
     if (io_err_E<0) then
       call error ('DB1 database not found in '//trim(jobstr))
     else
       io_err_E=ioDB1_selective_scan(RT_db1_E,RT_db1_k,trim(jobstr),'nb')
       !
       allocate(RT_db1_E%E(RT_db1_E%nb,RT_db1_k%nibz,n_sp_pol), RT_db1_k%pt(RT_db1_k%nibz,3))
       !
       io_err_E=ioDB1_selective_scan(RT_db1_E,RT_db1_k,trim(jobstr),'E')
       io_err_E=ioDB1_selective_scan(RT_db1_E,RT_db1_k,trim(jobstr),'K')
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
   NE_i_time= NE_i_time-mod(NE_i_time,RT_IO_steps)
   !
   if (NE_i_time==0       ) then
     NE_i_time=RT_IO_steps
   else
     do i1=NE_i_time,RT_IO_steps,-RT_IO_steps 
       !
       file_number=(i1-1)/RT_IO_steps
       !
       if ( variable_is_found(ID(6),'RT_occupations_TIME'//trim(intc(i1)))==1.or.&
&           file_exists(trim(jobstr)//"/ndb.RT_occupations_"//trim(intc(file_number))).or.&
&           file_exists(trim(jobstr)//"/db.RT_occupations_"//trim(intc(file_number))) ) then
         NE_i_time=i1
         exit
       endif
       !
     enddo
   endif
   !
   Time=(NE_i_time-1)*RT_step
   !
   if (.not.allocated(RT_k)) then
     allocate(RT_occupations(SC_bands(1):SC_bands(2),RT_nk),RT_k(RT_nk,3))
     call mem_est("RT_occupations",(/size(RT_occupations)/),(/SP/))
   endif
   !
   call io_control(ACTION=RD_CL,COM=NONE,SEC=(/1/),MODE=DUMP,ID=ID(6))
   io_err_occ=io_SC_components('occupations',E,ID(6))
   call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/NE_i_time+1/),MODE=DUMP,ID=ID(6))
   io_err_occ=io_SC_components('occupations',E,ID(6))
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
   call interpolate(E%E(:SC_bands(2),:,1),E%f(:SC_bands(2),:,1),SC_bands(2),k%nibz,.FALSE.)
   !
   if (associated(E%f_RIM)) then
     Nb_to_int=min(SC_bands(2),BZ_RIM_nbands)
     call interpolate(E%E_RIM(:Nb_to_int,:,1),E%f_RIM(:Nb_to_int,:,1),Nb_to_int,BZ_RIM_tot_nkpts,.TRUE.)
   endif
   !
   call mem_est("RT_occupations")
   ! 
   call E_reset(RT_db1_E) 
   call bz_samp_reset(RT_db1_k)
   !
   ! I need to redefine here several quantities related to the occupations
   ! using REPORT_Occupations
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
   if (.not.input_Tel_is_negative) call REPORT_Occupations(E,2,.TRUE.)
   call msg('r','Integarted N. of e-   [input]:',sum(RT_occupations(:,:))/real(RT_nk)+(SC_bands(1)-1)*2._SP)
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
   subroutine interpolate(En,Fn,Nb,Nk,REPORT)
     real(SP) :: En(Nb,Nk),Fn(Nb,Nk),E_diff,E_bottom
     integer  :: Nb,Nk,ib_occ(2),ik_occ(2)
     logical  :: REPORT
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
              Fn(ib,ik)=RT_occupations( RT_E_table(RT_E_indx(i1),1), RT_E_table(RT_E_indx(i1),2) )
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
