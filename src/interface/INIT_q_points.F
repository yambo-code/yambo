!
!        Copyright (C) 2000-2017 the YAMBO team
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
subroutine INIT_q_points(io_QINDX_err,l_WRITE)
 !
 ! This routine:
 !
 ! 1. reads the q-points from the input file
 ! 2. reads the q-points from the e-p DB's 
 !
 use pars,        ONLY:SP,schlen,DP
 use memory_m,    ONLY:mem_est
 use it_m,        ONLY:nflines,flines,infile,infile_verbosity,V_kpt,V_all
 use R_lattice,   ONLY:nqibz,q_pt,bz_samp,bz_samp_reset,q_source
#if defined _ELPH
 use ELPH,        ONLY:ph_qpt,elph_use_q_grid,elph_nDBs,elph_grids_are_expanded
#endif
 use vec_operate, ONLY:c2a
 implicit none
 !
 logical :: l_WRITE
 integer :: io_QINDX_err
 !
 ! Work Space
 !
 integer       :: i1,nflines_save
 type(bz_samp) :: q
 real(DP)      :: q_pt_db(3)
 !
 if (io_QINDX_err==0) return
 !
 if (l_WRITE.and. (infile_verbosity==V_kpt.or.infile_verbosity==V_all)) then
   nflines_save=nflines
   write (flines(nflines+1),'(a,t30,a)') '% QptCoord',&
&        '# [KPT] [iku] Q-points coordinates (compatibility)'
   nflines=nflines+1
   if (nqibz>0) then
     do i1=1,nqibz
       write (flines(nflines+1),'(3(f10.3,a))') q_pt(i1,1),'|',&
&             q_pt(i1,2),'|',q_pt(i1,3),'|'
       nflines=nflines+1
     enddo
   endif
   flines(nflines+1)='%'
   nflines=nflines+1
 else if (get_e_p_q_point(1,q_pt_db)==0) then
   q_source="Electron-Phonon databases"
   do i1=1,2
     nqibz=0
     do while(get_e_p_q_point(nqibz+1,q_pt_db)==0) 
       nqibz=nqibz+1
       if (i1==2) q_pt(nqibz,:)=-real(q_pt_db,SP)
     enddo
     if (i1==1.and..not.allocated(q_pt)) then
       allocate(q_pt(nqibz,3))
       call mem_est("q_pt",(/nqibz*3/),(/SP/))  
     endif
   enddo
   return
#if defined _ELPH
 else if (allocated(ph_qpt).and.elph_use_q_grid.and..not.elph_grids_are_expanded) then
   q_source="Electron-Phonon databases"
   nqibz=elph_nDBs
   allocate(q_pt(nqibz,3))
   call mem_est("q_pt",(/nqibz*3/),(/SP/))  
   q_pt(:,:)=-real(ph_qpt(:,:),SP)
   return
#endif
 else
   call bz_samp_reset(q)
   call init_parse_K_list(q,'QptCoord')
   nqibz=q%nibz
   if (nqibz>0)  then
     allocate(q_pt(nqibz,3))
     q_pt=q%pt
     call mem_est("q_pt",(/nqibz*3/),(/SP/))  
   endif
   call bz_samp_reset(q)
   return
 endif 
 !
 ! Write to input file
 !
 open(unit=12,file=trim(infile),position='append')
 do i1=nflines_save+1,nflines
   write (12,'(a)') trim(flines(i1))
 enddo 
 close(12)
 !
 contains 
   !
   integer function get_e_p_q_point(i_q,q_in)
     !
     use D_lattice,   ONLY:alat
     use IO_m,        ONLY:io_control,io_connect,io_unit,OP_RD_CL,&
&                          io_disconnect
     !
     integer :: i_q
     real(DP):: q_in(3)
     !
     integer           :: io_err,ID
     character(schlen) :: db_name
     real(DP)          :: alat_DP
     !
     call io_control(ACTION=OP_RD_CL,ID=ID)
     write (db_name,'(a,i6.6)') 'SAVE/s.dbph_',iabs(i_q)
     io_err=io_connect(desc=trim(db_name),type=-2,ID=ID)
     get_e_p_q_point=io_err
     !
     if (io_err==0) then
       read (io_unit(ID)) 
       read (io_unit(ID)) alat_DP,q_in
       q_in=q_in/alat_DP*alat(:)
     endif
     call io_disconnect(ID)
     !
   end function
   !
end subroutine
