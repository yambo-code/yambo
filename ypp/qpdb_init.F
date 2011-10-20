!
! Copyright (C) 2000-2011 C. Attaccalite and the YAMBO team 
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
subroutine qpdb_init()
 !
 ! QPDB_states -> input file
 !
 use QP_m,      ONLY: QP_nk,QP_nb,QP_state,QP_t,QP_reset
 use drivers,   ONLY: infile_editing
 use it_m,      ONLY: nflines,flines,infile
 use parser_m,  ONLY: parser       
 use IO_m,      ONLY:OP_RD_CL,DUMP,io_control,NONE
 use units,     ONLY:HA2EV
 implicit none
 ! 
 ! Work Space
 !
 type(QP_t) :: qp
 integer :: i1,nflines_save,state(4),io_QP,ID
 logical :: l_lines_in_input
 character(7) :: comment
 integer, external :: ioQP
 !
 call parser('QPDBkrange',l_lines_in_input)
 !
 !If there are already the QP lines or there's not the input file todo return
 !
 if (.not.infile_editing) return 
 !
 call QP_reset(qp)
 call io_control(ACTION=OP_RD_CL,SEC=(/1,2,3/),COM=NONE,MODE=DUMP,ID=ID)
 io_QP=ioQP("QP",qp,ID)
 !
 comment="[QPDB] QP correction"
 nflines_save=nflines
 !
 ! QP corrections
 ! 
 write (flines(nflines+1),'(a,t22,a)') '%QPkrange',&
&      '# '//trim(comment)//'QPDB energy correction( kp1| kp2| bnd1| bnd2| E-Eo[eV]| Img(E)[eV]| Z|)'
 if(io_QP==0) then
   do i1=1,qp%n_states
     nflines=nflines+1
     write (flines(nflines+1),'(4(i3,a),3(f12.6,a))') qp%table(i1,3),'|',qp%table(i1,3),'|',qp%table(i1,1),'|',qp%table(i1,1),'|' &
&                            ,real(qp%E(i1)-qp%E_bare(i1))*HA2EV,'|',aimag(qp%E(i1))*HA2EV,'|',real(qp%Z(i1)),'|'
   enddo
 else
   nflines=nflines+1
   write (flines(nflines+1),'(4(i3,a),3(f4.1,a))') -1,'|',-1,'|',-1,'|',-1,'|',0.0,'|',0.0,'|',0.0,'|'
 endif
 nflines=nflines+1
 flines(nflines+1)='%'
 nflines=nflines+1
 !
 ! Write to input file
 !
 open(unit=12,file=trim(infile),position='append')
 do i1=nflines_save+1,nflines
   write (12,'(a)') trim(flines(i1))
 enddo 
 close(12)
 return
 !
end subroutine
