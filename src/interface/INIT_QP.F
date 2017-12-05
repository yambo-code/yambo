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
subroutine INIT_QP(use_for_QP,Energy_range)
 !
 ! QP_state -> input file
 !
 use QP_m,      ONLY: QP_nk,QP_nb,QP_state
 use drivers,   ONLY: infile_editing
 use it_m,      ONLY: nflines,flines,infile,ADDITIONAL_flines,nADDITIONAL_flines
 implicit none
 !
 logical :: use_for_QP,Energy_range
 ! 
 ! Work Space
 !
 integer :: i1,state(4),nflines_save
 character(7) :: comment
 integer, external :: QP_state_extract
 !
 comment= ''
 if (use_for_QP) comment="[GW] QP"
 !
 nflines_save=nflines
 write (flines(nflines+1),'(a,t32,2a)') '%QPkrange',&
&      '# '//trim(comment)//' generalized Kpoint/Band indices'
 nflines=nflines+1
 if (.not.allocated(QP_state)) then 
   write (flines(nflines+1),'(4(i3,a))') 1,'|',QP_nk,'|',1,'|',QP_nb,'|'
   nflines=nflines+1
 else
   state=(/1,1,0,0/)
   do while(QP_state_extract(state)>0)
     write (flines(nflines+1),'(4(i3,a))') state(1),'|',state(2),'|',state(3),'|',state(4),'|'
     nflines=nflines+1
   enddo
 endif
 flines(nflines+1)='%'
 nflines=nflines+1
 !
 if (Energy_range) then
   write (flines(nflines+1),'(a,t32,a)') '%QPerange',&
&      '# '//trim(comment)//' generalized Kpoint/Energy indices'
   write (flines(nflines+2),'(2(i3,a),2(f4.1,a))') 1,'|',QP_nk,'|',0.,'|',-1.,'|'
   flines(nflines+3)='%'
   nflines=nflines+3
 endif
 !
 ! Write (eventually) to input file
 !
 ADDITIONAL_flines=" "
 !
 if (infile_editing) open(unit=12,file=trim(infile),position='append')
 do i1=nflines_save+1,nflines
   nADDITIONAL_flines=nADDITIONAL_flines+1
   ADDITIONAL_flines(nADDITIONAL_flines)=flines(i1)
   if (infile_editing) write (12,'(a)') trim(flines(i1))
 enddo 
 if (infile_editing) close(12)
 !
end subroutine
