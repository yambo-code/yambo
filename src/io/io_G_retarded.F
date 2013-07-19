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
integer function io_G_retarded(ID,what)
 !
 use pars,           ONLY:SP
 use units,          ONLY:FS2AUT,HA2EV
 use IO_m,           ONLY:io_connect,io_disconnect,io_header,io_sec,io_elemental,&
&                         write_is_on,io_bulk,io_status,ver_is_gt_or_eq
 use real_time,      ONLY:Gr,Gr_kind,Gr_bands,Gr_T_steps,Gr_T_window,Gr_E_steps,Gr_E_window,&
&                         Gr_peak_pos,Gr_E_ranges,Gr_T_step,Gr_QP_width,Gr_description,N_Gr_descs
 use R_lattice,      ONLY:nkibz
 use QP_m,           ONLY:QP_ctl_Wc_E_ref,QP_ctl_Wv_E_ref,QP_ctl_Wc,QP_ctl_Wv,QP_ctl_E
 use stderr,         ONLY:intc
 !
 implicit none
 character(*)::what
 integer     ::ID
 !
 ! Work Space
 !
 integer              :: i_k,i_s,T_steps_each_section,T_section(2),i_desc,i1
 real(SP),allocatable :: Gr_disk(:,:,:)
 !
 io_G_retarded=io_connect(desc=what,type=2,ID=ID)
 if (io_G_retarded/=0) goto 1
 !
 Gr_description=' '
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   io_G_retarded=io_header(ID,XC_KIND="G")
   !
   call io_elemental(ID,VAR='G_retarded_KIND',CH0="",VAR_SZ=1,MENU=0)
   i_desc=1
   call io_elemental(ID,CH0=Gr_kind,&
&       VAR=' [Gr] Retarded Green`s function  :',CHECK=.true.,&
&       OP=(/"=="/),DESCRIPTION=Gr_description(i_desc))
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   !
   if (ver_is_gt_or_eq(ID,revision=2580)) then
     call io_elemental(ID,VAR="PARAMETERS",VAR_SZ=21,MENU=0)
   else if (ver_is_gt_or_eq(ID,revision=2440)) then
     call io_elemental(ID,VAR="PARAMETERS",VAR_SZ=18,MENU=0)
   else
     call io_elemental(ID,VAR="PARAMETERS",VAR_SZ=10,MENU=0)
   endif
   !
   call io_elemental(ID,I1=Gr_bands,&
&       VAR=' Bands                           :',CHECK=.true.,OP=(/"==","=="/))
   !
   i_desc=i_desc+1
   call io_elemental(ID,R0=Gr_QP_width,&
&       VAR=' [Gr] QP damping             [eV]:',UNIT=HA2EV,CHECK=.true.,&
&       OP=(/"=="/),DESCRIPTION=Gr_description(i_desc))
   !
   if (ver_is_gt_or_eq(ID,revision=2580)) then
     !
     i_desc=i_desc+1
     call io_elemental(ID,R1=QP_ctl_E(3,:,1),&
&         VAR=' [Gr] QP stretching[eV,adim,adim]:',CHECK=.true.,&
&         OP=(/"==","==","=="/),DESCRIPTION=Gr_description(i_desc))
   endif
   !
   if (ver_is_gt_or_eq(ID,revision=2440)) then
     !
     i_desc=i_desc+1
     call io_elemental(ID,R0=QP_ctl_Wc_E_ref(3,1),&
&         VAR=' [Gr] Damping (c) Energy ref [eV]:',UNIT=HA2EV,CHECK=.true.,&
&         OP=(/"=="/),DESCRIPTION=Gr_description(i_desc))
     i_desc=i_desc+1
     call io_elemental(ID,R1=QP_ctl_Wc(3,:,1),&
&         VAR=' [Gr] Damping (c) [eV,adim,eV^-1]:',CHECK=.true.,&
&         OP=(/"==","==","=="/),DESCRIPTION=Gr_description(i_desc))
     i_desc=i_desc+1
     call io_elemental(ID,R0=QP_ctl_Wv_E_ref(3,1),&
&         VAR=' [Gr] Damping (v) Energy ref [eV]:',UNIT=HA2EV,CHECK=.true.,&
&         OP=(/"=="/),DESCRIPTION=Gr_description(i_desc))
     i_desc=i_desc+1
     call io_elemental(ID,R1=QP_ctl_Wv(3,:,1),&
&         VAR=' [Gr] Damping (v) [eV,adim,eV^-1]:',CHECK=.true.,&
&         OP=(/"==","==","=="/),DESCRIPTION=Gr_description(i_desc))
     !
   endif
   !
   i_desc=i_desc+1
   call io_elemental(ID,R0=Gr_T_window,&
&       VAR=' [Gr] Time range             [fs]:',UNIT=1./FS2AUT,CHECK=.true.,&
&       OP=(/"=="/),DESCRIPTION=Gr_description(i_desc))
   i_desc=i_desc+1
   call io_elemental(ID,R0=Gr_T_step,&
&       VAR=' [Gr] Time step              [fs]:',UNIT=1./FS2AUT,CHECK=.true.,&
&       OP=(/"=="/),DESCRIPTION=Gr_description(i_desc))
   i_desc=i_desc+1
   call io_elemental(ID,I0=Gr_T_steps,&
&       VAR=' [Gr] Time steps                 :',CHECK=.true.,&
&       OP=(/"=="/),DESCRIPTION=Gr_description(i_desc))
   !
   if (trim(Gr_kind)/="HC".and.trim(Gr_kind)/="QP") then
     !
     i_desc=i_desc+1
     call io_elemental(ID,R0=Gr_E_window,&
&         VAR=' [Gr] Energy range           [eV]:',UNIT=HA2EV,CHECK=.true.,&
&         OP=(/"=="/),DESCRIPTION=Gr_description(i_desc))
     !
     i_desc=i_desc+1
     call io_elemental(ID,I0=Gr_E_steps,&
&         VAR=' [Gr] Energy steps               :',CHECK=.true.,&
&         OP=(/"=="/),DESCRIPTION=Gr_description(i_desc))
     !
   endif
   !
   N_Gr_descs=i_desc
   !
   call io_elemental(ID,VAR=" ",VAR_SZ=0)
   !
   io_G_retarded=io_status(ID)
   !
   if (io_G_retarded/=0) goto 1
   !
 endif
 !
 if (.not.any((/io_sec(ID,:)==2/))) return
 !
 T_steps_each_section=Gr_T_steps/10
 !
 allocate(Gr_disk(Gr_bands(2),T_steps_each_section,2))
 !
 do i_k=1,nkibz
   !
   do i_s=1,10
     !
     T_section=(/(i_s-1)*T_steps_each_section+1,min( i_s*T_steps_each_section,Gr_T_steps )/)
     !
     call io_bulk(ID,VAR="G_retarded_k"//trim(intc(i_k))//"_Tsec"//trim(intc(i_s)),VAR_SZ=(/Gr_bands(2),T_steps_each_section,2/))
     if(write_is_on(ID)) then
       Gr_disk(:,1:T_section(2)-T_section(1)+1,1) =real(Gr(i_k,:,T_section(1):T_section(2)))
       Gr_disk(:,1:T_section(2)-T_section(1)+1,2)=aimag(Gr(i_k,:,T_section(1):T_section(2)))
       call io_bulk(ID,R3=Gr_disk)
     else
       call io_bulk(ID,R3=Gr_disk)
       Gr(i_k,:,T_section(1):T_section(2))=&
&                                  CMPLX(Gr_disk(:,1:T_section(2)-T_section(1)+1,1),Gr_disk(:,1:T_section(2)-T_section(1)+1,2))
     endif
     !
   enddo
   !
 enddo
 !
 deallocate(Gr_disk)
 !
 call io_bulk(ID,VAR="Gr_Peak_Position",VAR_SZ=(/nkibz,Gr_bands(2)/))
 call io_bulk(ID,R2=Gr_peak_pos)
 !
 call io_bulk(ID,VAR="Gr_Energy_Ranges",VAR_SZ=(/nkibz,Gr_bands(2),2/))
 call io_bulk(ID,R3=Gr_E_ranges)
 !
1 call io_disconnect(ID=ID)
 !
end function
