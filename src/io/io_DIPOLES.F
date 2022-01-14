!
!        Copyright (C) 2000-2021 the YAMBO team
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
 use pars,          ONLY:schlen
 use DIPOLES,       ONLY:DIPOLE_t,DIP_iR,DIP_P,DIP_v,DIP_alloc
 use electrons,     ONLY:n_spin
 use R_lattice,     ONLY:nXkibz
 use IO_m,          ONLY:io_sec,frag_DIP,io_DIP,read_is_on,io_extension,&
&                        IO_NOT_ALLOWED,frag_DIP
 use IO_int,        ONLY:io_connect,io_disconnect,io_bulk,io_header,io_fragment
 use DIPOLES,       ONLY:DIP_spin,DIP_orbital,&
&                        compute_Orb_dipoles,compute_Spin_dipoles
 use electrons,     ONLY:n_spinor
#if defined _RT
 use DIPOLES,       ONLY:DIP_P_spinor
#endif
#if defined _SC || defined _RT
 use DIPOLES,       ONLY:P_square,compute_P2_dipoles
#endif
 use BS,            ONLY:l_BS_dichroism
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
 integer :: ik,ik_mem,i_fragment,i_sp_pol,DIP_size(4),DIP_ipos(4),ID_frag
#if defined _RT || defined _SC
 integer :: P2_size(3),P2_ipos(3),Pspinor_size(6),Pspinor_ipos(6)
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
 call timing('io_DIPOLES',OPR='start')
 !
 io_extension(ID)='dipoles'
 !
 io_DIPOLES=io_connect(desc=trim(io_extension(ID)),type=2,ID=ID,FORCE_READ_MODE=(.not.any((/io_sec(ID,:)==1/))).and.frag_DIP)
 if (io_DIPOLES/=0) goto 1
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   io_DIPOLES=io_header(ID,R_LATT=.true.,WF=.true.,IMPOSE_SN=.true.,TEMP=.true.,FRAG=frag_DIP)
   if (io_DIPOLES/=0) goto 1
   !
   io_DIPOLES=variables_DIPOLES(Dip,ID)
   if (io_DIPOLES/=0) goto 1
   !
 endif
 !
 i_fragment=maxval(io_sec(ID,:))-1
 if(i_fragment<=nXkibz) then
   i_sp_pol=1
   ik=i_fragment
 else
   i_sp_pol=2
   ik=i_fragment-nXkibz
 endif
 !
 if (ik==0) goto 1
 !
 ik_mem=PAR_K_scheme%ibz_index(ik)
 !
 ! Reading size and position
 !
 DIP_size=(/2,3,Dip%db_ib(2)-Dip%db_ib_lim(2)+1,Dip%db_ib_lim(1)-Dip%db_ib(1)+1/)
 DIP_ipos=(/1,1,Dip%ib_lim(2)-Dip%db_ib_lim(2)+1,Dip%ib(1)-Dip%db_ib(1)+1/)
 !
#if defined _RT
 Pspinor_size(1:4)=(/2,3,n_spinor,n_spinor/)
 Pspinor_ipos(1:4)=(/1,1,1,1/)
 !
 Pspinor_size(5:6)=DIP_size(3:4)
 Pspinor_ipos(5:6)=DIP_ipos(3:4)
#endif
 !
#if defined _RT || defined _SC
 P2_size(1)=2
 P2_ipos(1)=1
 !
 P2_size(2:3)=DIP_size(3:4)
 P2_ipos(2:3)=DIP_ipos(3:4)
#endif
 !
 if (read_is_on(ID)) then
   call DIP_alloc('DIP_iR',(/3,Dip%ib_lim(2),Dip%ib(2),Dip%ib(1),Dip%ib_lim(1),PAR_K_scheme%nibz/))
   call DIP_alloc('DIP_P' ,(/3,Dip%ib_lim(2),Dip%ib(2),Dip%ib(1),Dip%ib_lim(1),PAR_K_scheme%nibz/))
   call DIP_alloc('DIP_v' ,(/3,Dip%ib_lim(2),Dip%ib(2),Dip%ib(1),Dip%ib_lim(1),PAR_K_scheme%nibz/))
   !
   if (compute_Orb_dipoles) &
   & call DIP_alloc('DIP_orbital',(/3,Dip%ib_lim(2),Dip%ib(2),Dip%ib(1),Dip%ib_lim(1),PAR_K_scheme%nibz,2/))
   if (compute_Spin_dipoles) &
   & call DIP_alloc('DIP_spin',(/3,Dip%ib_lim(2),Dip%ib(2),Dip%ib(1),Dip%ib_lim(1),PAR_K_scheme%nibz/))
#if defined _RT 
   if (compute_Spin_dipoles.and.n_spinor==2) &
   & call DIP_alloc('DIP_P_spinor',(/3,Dip%ib_lim(2),Dip%ib(2),Dip%ib(1),Dip%ib_lim(1),PAR_K_scheme%nibz/))
#endif
#if defined _SC || defined _RT
   if (compute_P2_dipoles) call DIP_alloc('P_square',(/Dip%ib_lim(2),Dip%ib(2),Dip%ib(1),Dip%ib_lim(1),PAR_K_scheme%nibz/))
#endif
   if(l_BS_dichroism) then
     call DIP_alloc('DIP_orbital',(/3,Dip%ib_lim(2),Dip%ib(2),Dip%ib(1),Dip%ib_lim(1),PAR_K_scheme%nibz,1/))
   endif  
   !
 endif
 !
 ! Fragmentation
 !
 call io_fragment(ID,ID_frag,i_fragment=i_fragment)
 !
 ! iR
 !
 write (VAR_name,'(3(a,i4.4))') 'DIP_iR_k_',ik,'_spin_',i_sp_pol
 call io_bulk(ID_frag,VAR=trim(VAR_name),VAR_SZ=DIP_size)
 call io_bulk(ID_frag,C3=DIP_iR(:,:,:,ik_mem,i_sp_pol),IPOS=DIP_ipos)
 !
 ! P
 !
 write (VAR_name,'(3(a,i4.4))') 'DIP_P_k_',ik,'_spin_',i_sp_pol
 call io_bulk(ID_frag,VAR=trim(VAR_name),VAR_SZ=DIP_size)
 call io_bulk(ID_frag,C3=DIP_P(:,:,:,ik_mem,i_sp_pol),IPOS=DIP_ipos)
 !
 ! v
 !
 write (VAR_name,'(3(a,i4.4))') 'DIP_v_k_',ik,'_spin_',i_sp_pol
 call io_bulk(ID_frag,VAR=trim(VAR_name),VAR_SZ=DIP_size)
 call io_bulk(ID_frag,C3=DIP_v(:,:,:,ik_mem,i_sp_pol),IPOS=DIP_ipos)
 !
 if (compute_Orb_dipoles) then
   !
   ! Itinerant magnetization
   !
   write (VAR_name,'(3(a,i4.4))') 'DIP_itinerant_k_',ik,'_spin_',i_sp_pol
   call io_bulk(ID_frag,VAR=trim(VAR_name),VAR_SZ=DIP_size)
   call io_bulk(ID_frag,C3=DIP_orbital(:,:,:,ik_mem,i_sp_pol,2),IPOS=DIP_ipos)
   !
   ! Orbital magnetization
   !
   write (VAR_name,'(3(a,i4.4))') 'DIP_orbital_k_',ik,'_spin_',i_sp_pol
   call io_bulk(ID_frag,VAR=trim(VAR_name),VAR_SZ=DIP_size)
   call io_bulk(ID_frag,C3=DIP_orbital(:,:,:,ik_mem,i_sp_pol,1),IPOS=DIP_ipos)
   !
 endif
 !
 if (compute_Spin_dipoles) then 
   !
   ! Spin magntization
   !
   write (VAR_name,'(3(a,i4.4))') 'DIP_spin_k_',ik,'_spin_',i_sp_pol
   call io_bulk(ID_frag,VAR=trim(VAR_name),VAR_SZ=DIP_size)
   call io_bulk(ID_frag,C3=DIP_spin(:,:,:,ik_mem,i_sp_pol,1),IPOS=DIP_ipos)
   !
 endif
 !
#if defined _RT
 if (compute_Spin_dipoles) then
   !
   if (n_spinor==2) then
     !
     ! P_Spinor
     !
     write (VAR_name,'(a,i4.4)') 'DIP_P_spinor_k_',ik
     call io_bulk(ID_frag,VAR=trim(VAR_name),VAR_SZ=Pspinor_size)
     call io_bulk(ID_frag,C5=DIP_P_spinor(:,:,:,:,:,ik_mem),IPOS=Pspinor_ipos)
     !
   endif
   !
 endif
#endif
 !
#if defined _SC || defined _RT
 if (compute_P2_dipoles) then
   !
   ! P2
   !
   write (VAR_name,'(3(a,i4.4))') 'P_square_k_',ik,'_spin_',i_sp_pol
   call io_bulk(ID_frag,VAR=trim(VAR_name),VAR_SZ=P2_size)
   call io_bulk(ID_frag,C2=P_square(:,:,ik_mem,i_sp_pol),IPOS=P2_ipos )
   !
 endif
#endif
 !
if (l_BS_dichroism) then
    !   
    ! Orbital magnetization
    !   
    write (VAR_name,'(3(a,i4.4))') 'DIP_orbital_k_',ik,'_spin_',i_sp_pol
    call io_bulk(ID_frag,VAR=trim(VAR_name),VAR_SZ=DIP_size)
    call io_bulk(ID_frag,C3=DIP_orbital(:,:,:,ik_mem,i_sp_pol,1),IPOS=DIP_ipos)  
 endif
 ! 
 call io_fragment_disconnect(ID,ID_frag)
 !
1 call io_disconnect(ID=ID)
 !
 if (io_DIP) call timing('io_DIPOLES',OPR='stop')
 !
end function
