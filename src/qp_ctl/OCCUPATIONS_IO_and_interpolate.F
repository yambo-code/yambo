!
! Copyright (C) 2000-2013 A. Marini and the YAMBO team 
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
 use real_time,      ONLY:ID,G_lesser,NE_i_time,max_rt_steps_disk,RT_step,MEM_index
 use SC,             ONLY:SC_bands,RT_occupations,RT_nk,RT_k 
 use IO_m,           ONLY:io_control,NONE,OP_APP_WR_CL,OP_RD,RD_CL,DUMP
 use electrons,      ONLY:levels,BZ_RIM_nkpt,BZ_RIM_table,BZ_RIM_nbands,n_sp_pol,n_met_bands,&
&                         n_full_bands,nel,filled_tresh
 use com,            ONLY:jobstr,msg
 use QP_m,           ONLY:OCC_ctl
 use memory_m,       ONLY:mem_est
 use R_lattice,      ONLY:bz_samp,nXkibz
 use D_lattice,      ONLY:DL_vol
 use timing,         ONLY:what_is_running
 use stderr,         ONLY:set_real_printed_length
 use interfaces,     ONLY:OCCUPATIONS_gaps
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
 real(SP)          :: PE_nel
 integer           :: ib,io_err,COM_,ik,i_err,ik_near,is,ik_bz,ik_r,i1,i_spin
 integer, external :: io_SC_components
 character(lchlen) :: jobstr_save
 logical           :: is_def
 !
 integer, external :: k_the_nearest
 !
 ! Real-Time occupations
 !=======================
 !
 if (.not.present(Time)) then
   !
   ! (a) Write
   !
   do ib=1,SC_bands(1)-1
     RT_occupations(ib,:)=E%f(ib,:,1)
   enddo
   !
   do ib=SC_bands(1),SC_bands(2)
     RT_occupations(ib,:)=-real((0._SP,1._SP)*G_lesser(ib,ib,:,MEM_index(NE_i_time)))
   enddo
   ! 
   ! Note that here as well as in ndb.GreenF the SEC is used only to
   ! store the header + the descriptors. Therefore in order to write
   ! those informations I pass directly NE_i_time that starts from 1.
   !
   call io_control(ACTION=OP_APP_WR_CL,COM=NONE,SEC=(/NE_i_time/),ID=ID(6))
   io_err=io_SC_components('occupations',E,ID(6))
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
   call io_control(ACTION=OP_RD,COM=COM_,SEC=(/1/),MODE=DUMP,ID=ID(6))
   io_err=io_SC_components('occupations',E,ID(6))
   !
   if (io_err<0) then
     jobstr=jobstr_save
     return
   endif
   ! 
   ! NE_time=(NE_i_time-1)*RT_step
   !
   NE_i_time= int( real(Time/RT_step+1)/real(max_rt_steps_disk) ) * max_rt_steps_disk
   Time=(NE_i_time-1)*RT_step
   !
   if (.not.allocated(RT_k)) then
     allocate(RT_occupations(SC_bands(2),RT_nk),RT_k(RT_nk,3))
     call mem_est("RT_occupations",(/size(RT_occupations)/),(/SP/))
   endif
   !
   call io_control(ACTION=RD_CL,COM=COM_,SEC=(/NE_i_time/),MODE=DUMP,ID=ID(6))
   io_err=io_SC_components('occupations',E,ID(6))
   !
   jobstr=jobstr_save
   !
   if (index(what_is_running,'YPP')>0) return
   !
   ! Interpolation
   !===============
   !
   do ik=1,k%nibz
     ik_near=k_the_nearest(k%pt(ik,:),RT_k,RT_nk,.FALSE.,i_err)
     do ib=SC_bands(1),SC_bands(2)
       E%f(ib,ik,:)=RT_occupations(ib,ik_near)
       if ( E%f(ib,ik,1) < filled_tresh )  E%f(ib,ik,:)=0._SP
     enddo
   enddo
   !
   if (associated(E%f_RIM)) then
     do ik_bz=1,k%nbz
       ik=k%sstar(ik_bz,1)
       do i1=1,BZ_RIM_nkpt(ik_bz)
         ik_r=BZ_RIM_table(ik_bz,i1)
         forall (ib=1:BZ_RIM_nbands,i_spin=1:n_sp_pol) E%f_RIM(ib,ik_r,i_spin)=E%f(ib,ik,i_spin)
       enddo
     enddo
   endif
   !
   deallocate(RT_occupations,RT_k)
   call mem_est("RT_occupations")
   !
   ! I need to redefine here several quantities related to the occupations
   ! using OCCUPATIONS_gaps
   !
   PE_nel=nel-sum(E%f(:n_met_bands,:,:))/real(nXkibz)
   !
   call msg('nr','Snapshot time            [fs]:',Time/FS2AUT)
   call msg(' r','Photoexcited electrons       :',PE_nel)
   call msg(' r','                          [%]:',PE_nel/nel*100._SP)
   call msg(' r','                      [cm^-3]:',PE_nel/(DL_vol*BO2ANG**3*1.E-24))
   !
   call msg('r','')
   call OCCUPATIONS_gaps(E)
   call msg('r','Integarted N. of e-    :',sum(E%f(:,:,:))/real(nXkibz))
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
end subroutine OCCUPATIONS_IO_and_interpolate
