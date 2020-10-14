!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, DS
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
integer function io_DIPOLES(Dip,ID)
 !
 use pars,          ONLY:schlen,SP
 use DIPOLES,       ONLY:DIPOLE_t,DIP_iR,DIP_P,DIP_v,DIP_alloc
 use electrons,     ONLY:n_spin,n_sp_pol
 use R_lattice,     ONLY:nXkibz
 use IO_m,          ONLY:io_connect,io_disconnect,io_sec,frag_DIP,io_DIP,&
&                        io_variable_bulk,def_variable_bulk,read_is_on,io_header,io_extension,&
&                        IO_NOT_ALLOWED
#if defined _RT || defined _KERR
 use DIPOLES,       ONLY:DIP_spin,DIP_orbital,&
&                        compute_Orb_dipoles,compute_Spin_dipoles
 use electrons,     ONLY:n_spinor
#endif
#if defined _RT
 use DIPOLES,       ONLY:DIP_P_spinor
#endif
#if defined _SC || defined _RT
 use DIPOLES,       ONLY:P_square,compute_P2_dipoles
#endif
#if defined _KERR
 use BS,            ONLY:l_BS_magnons,l_BS_dichroism
#endif
 use parallel_m,    ONLY:PAR_K_scheme
 use timing_m,      ONLY:timing
 !
 implicit none
 !
 type(DIPOLE_t) :: Dip
 integer        :: ID
 !
 ! Work Space
 !
 integer :: ik,ik_mem,i_fragment,i_sp_pol,DIP_size(6),DIP_ipos(6),VAR_ID
#if defined _RT || defined _SC
 integer :: P2_size(5),P2_ipos(5),Pspinor_size(7),Pspinor_ipos(7)
#endif
 integer, external    :: variables_DIPOLES
 character(schlen)    :: VAR_name
 !
 if (.not.io_DIP) then
   io_DIPOLES=IO_NOT_ALLOWED
   call io_disconnect(ID,FORCE=.TRUE.)
   return
 endif
 !
 i_sp_pol=-1
 ik=-1
 i_fragment=maxval(io_sec(ID,:))-1
 if(i_fragment<=nXkibz) then
   i_sp_pol=1
   ik=i_fragment
 else if(n_sp_pol==2.and.i_fragment<=2*nXkibz) then
   i_sp_pol=2
   ik=i_fragment-nXkibz
 endif
 !
 call timing('io_DIPOLES',OPR='start')
 !
 io_extension(ID)='dipoles'
 !
 io_DIPOLES=io_connect(desc=trim(io_extension(ID)),type=2,ID=ID,COMM=PAR_K_scheme%COM_ibz_index%COMM)
 if (io_DIPOLES/=0) goto 1
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   io_DIPOLES=io_header(ID,R_LATT=.true.,WF=.true.,IMPOSE_SN=.true.,T_EL=.true.,FRAG=frag_DIP)
   if (io_DIPOLES/=0) goto 1
   !
   io_DIPOLES=variables_DIPOLES(Dip,ID)
   if (io_DIPOLES/=0) goto 1
   !
 endif
 !
 if (i_fragment==0)               goto 1
 if (i_fragment> n_sp_pol*nXkibz) goto 1
 !
 if (i_fragment>=1) ik_mem=PAR_K_scheme%ibz_index(ik)
 !
 ! Reading size and position
 !
 DIP_size=(/2,3,Dip%db_ib(2)-Dip%db_ib_lim(2)+1,Dip%db_ib_lim(1)-Dip%db_ib(1)+1,nXkibz,n_sp_pol/)
 DIP_ipos=(/1,1,Dip%ib_lim(2)-Dip%db_ib_lim(2)+1,Dip%ib(1)-Dip%db_ib(1)+1,ik,i_sp_pol/)
 !
#if defined _RT
 Pspinor_size(1:4)=(/2,3,n_spinor,n_spinor/)
 Pspinor_ipos(1:4)=(/1,1,1,1/)
 !
 Pspinor_size(5:7)=DIP_size(3:5)
 Pspinor_ipos(5:7)=DIP_ipos(3:5)
#endif
 !
#if defined _RT || defined _SC
 P2_size(1)=2
 P2_ipos(1)=1
 !
 P2_size(2:5)=DIP_size(3:6)
 P2_ipos(2:5)=DIP_ipos(3:6)
#endif
 !
 if (read_is_on(ID).and.i_fragment==-1) then
   call DIP_alloc('DIP_iR',(/3,Dip%ib_lim(2),Dip%ib(2),Dip%ib(1),Dip%ib_lim(1),PAR_K_scheme%nibz/))
   call DIP_alloc('DIP_P' ,(/3,Dip%ib_lim(2),Dip%ib(2),Dip%ib(1),Dip%ib_lim(1),PAR_K_scheme%nibz/))
   call DIP_alloc('DIP_v' ,(/3,Dip%ib_lim(2),Dip%ib(2),Dip%ib(1),Dip%ib_lim(1),PAR_K_scheme%nibz/))
   !
   ! GPL_EXCLUDE_START
   !
#if defined _RT || defined _KERR
   if (compute_Orb_dipoles) &
   & call DIP_alloc('DIP_orbital',(/3,Dip%ib_lim(2),Dip%ib(2),Dip%ib(1),Dip%ib_lim(1),PAR_K_scheme%nibz,2/))
   if (compute_Spin_dipoles) &
   & call DIP_alloc('DIP_spin',(/3,Dip%ib_lim(2),Dip%ib(2),Dip%ib(1),Dip%ib_lim(1),PAR_K_scheme%nibz/))
#endif
#if defined _RT
   if (compute_Spin_dipoles.and.n_spinor==2) &
   & call DIP_alloc('DIP_P_spinor',(/3,Dip%ib_lim(2),Dip%ib(2),Dip%ib(1),Dip%ib_lim(1),PAR_K_scheme%nibz/))
#endif
#if defined _SC || defined _RT
   if (compute_P2_dipoles)  call DIP_alloc('P_square',(/Dip%ib_lim(2),Dip%ib(2),Dip%ib(1),Dip%ib_lim(1),PAR_K_scheme%nibz/))
#endif
#if defined _KERR
   if(l_BS_dichroism) then
     call DIP_alloc('DIP_orbital',(/3,Dip%ib_lim(2),Dip%ib(2),Dip%ib(1),Dip%ib_lim(1),PAR_K_scheme%nibz,1/))
   endif
#endif
   !
   ! GPL_EXCLUDE_END
   !
 endif
 !
 ! iR
 !
 VAR_ID=1
 VAR_name='DIP_iR'
 if (i_fragment< 1) call def_variable_bulk(ID,trim(VAR_name),VAR_ID,DIP_size,SP,par_io_kind='independent')
 if (i_fragment>=1) call  io_variable_bulk(ID,VAR_ID,C5=DIP_iR(:,:,:,ik_mem:ik_mem,i_sp_pol:i_sp_pol),IPOS=DIP_ipos)
 !
 ! P
 !
 VAR_ID=2
 VAR_name='DIP_P'
 if (i_fragment< 1) call def_variable_bulk(ID,trim(VAR_name),VAR_ID,DIP_size,SP,par_io_kind='independent')
 if (i_fragment>=1) call  io_variable_bulk(ID,VAR_ID,C5=DIP_P(:,:,:,ik_mem:ik_mem,i_sp_pol:i_sp_pol),IPOS=DIP_ipos)
 !
 ! v
 !
 VAR_ID=3
 VAR_name='DIP_v'
 if (i_fragment< 1) call def_variable_bulk(ID,trim(VAR_name),VAR_ID,DIP_size,SP,par_io_kind='independent')
 if (i_fragment>=1) call  io_variable_bulk(ID,VAR_ID,C5=DIP_v(:,:,:,ik_mem:ik_mem,i_sp_pol:i_sp_pol),IPOS=DIP_ipos)
 !
 ! GPL_EXCLUDE_START
 !
#if defined _RT || defined _KERR
 if (compute_Orb_dipoles) then
   !
   ! Itinerant magnetization
   !
   VAR_ID=4
   VAr_name='DIP_itinerant'
   if (i_fragment< 1) call def_variable_bulk(ID,trim(VAR_name),VAR_ID,DIP_size,SP,par_io_kind='independent')
   if (i_fragment>=1) call  io_variable_bulk(ID,VAR_ID,C5=DIP_orbital(:,:,:,ik_mem:ik_mem,i_sp_pol:i_sp_pol,2),IPOS=DIP_ipos)
   !
   ! Orbital magnetization
   !
   VAR_ID=5
   VAR_name='DIP_orbital'
   if (i_fragment< 1) call def_variable_bulk(ID,trim(VAR_name),VAR_ID,DIP_size,SP,par_io_kind='independent')
   if (i_fragment>=1) call  io_variable_bulk(ID,VAR_ID,C5=DIP_orbital(:,:,:,ik_mem:ik_mem,i_sp_pol:i_sp_pol,1),IPOS=DIP_ipos)
   !
 endif
 !
 if (compute_Spin_dipoles) then
   !
   ! Spin magntization
   !
   VAR_ID=5
   VAR_name='DIP_spin'
   if (i_fragment< 1) call def_variable_bulk(ID,trim(VAR_name),VAR_ID,DIP_size,SP,par_io_kind='independent')
   if (i_fragment>=1) call  io_variable_bulk(ID,VAR_ID,C5=DIP_spin(:,:,:,ik_mem:ik_mem,i_sp_pol:i_sp_pol),IPOS=DIP_ipos)
   !
 endif
#endif
 !
#if defined _RT
 if (compute_Spin_dipoles.and.n_spinor==2) then
   !
   ! P_Spinor
   !
   VAR_ID=6
   VAR_name='DIP_P_spinor'
   if (i_fragment< 1) call def_variable_bulk(ID,trim(VAR_name),VAR_ID,Pspinor_size,SP,par_io_kind='independent')
   if (i_fragment>=1) call  io_variable_bulk(ID,VAR_ID,C6=DIP_P_spinor(:,:,:,:,:,ik_mem:ik_mem),IPOS=Pspinor_ipos)
   !
 endif
#endif
 !
#if defined _SC || defined _RT
 if (compute_P2_dipoles) then
   !
   ! P2
   !
   VAR_ID=7
   VAR_name='P_square'
   if (i_fragment< 1) call def_variable_bulk(ID,trim(VAR_name),VAR_ID,P2_size,SP,par_io_kind='independent')
   if (i_fragment>=1) call  io_variable_bulk(ID,VAR_ID,C4=P_square(:,:,ik_mem:ik_mem,i_sp_pol:i_sp_pol),IPOS=P2_ipos)
   !
 endif
#endif
 !
#if defined _KERR
 if (l_BS_dichroism) then
   !
   ! Orbital magnetization
   !
   VAR_ID=9
   VAR_name='DIP_Orbital'
   if (i_fragment< 1) call def_variable_bulk(ID,trim(VAR_name),VAR_ID,DIP_size,SP,par_io_kind='independent')
   if (i_fragment>=1) call  io_variable_bulk(ID,VAR_ID,C5=DIP_orbital(:,:,:,ik_mem:ik_mem,i_sp_pol:i_sp_pol,1),IPOS=DIP_ipos)
   !
 endif
#endif
 !
 ! GPL_EXCLUDE_END
 !
1 call io_disconnect(ID=ID)
 !
 if (io_DIP) call timing('io_DIPOLES',OPR='stop')
 !
end function
