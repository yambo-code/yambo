!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
integer function io_DIPOLES(Dip,ID)
 !
 use pars,          ONLY:schlen,SP
 use DIPOLES,       ONLY:DIPOLE_t,DIP_iR,DIP_P,DIP_v,DIP_alloc
 use electrons,     ONLY:n_spin,n_sp_pol
 use R_lattice,     ONLY:nXkibz
 use IO_m,          ONLY:io_sec,frag_DIP,io_DIP,read_is_on,io_extension,IO_NOT_ALLOWED, &
&                        nf90_sync,io_unit,netcdf_call,io_PAR_cpu
 use IO_int,        ONLY:io_connect,io_disconnect,io_variable_bulk,def_variable_bulk,io_header
 use DIPOLES,       ONLY:DIP_spin,DIP_orbital, compute_Orb_dipoles,compute_Spin_dipoles
 use electrons,     ONLY:n_spinor
#if defined _RT
 use DIPOLES,       ONLY:DIP_P_spinor
#endif
 use DIPOLES,       ONLY:P_square,compute_P2_dipoles
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
 integer :: ik,ik_mem,i_fragment,i_sp_pol,DIP_size(6),DIP_ipos(6),VAR_ID,stat,&
&           P2_size(5),P2_ipos(5)
#if defined _RT
 integer :: Pspinor_size(7),Pspinor_ipos(7)
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
 io_DIPOLES=io_connect(desc=trim(io_extension(ID)),type=2,ID=ID)
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
 if (i_fragment==0)               goto 1
 if (i_fragment> n_sp_pol*nXkibz) then
   if(io_PAR_cpu(ID)==1) then
     stat = nf90_sync(io_unit(ID))
     call netcdf_call(stat,ID)
   endif
   goto 1
 endif
 ! 
                    ik_mem=0
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
 P2_size(1)=2
 P2_ipos(1)=1
 !
 P2_size(2:5)=DIP_size(3:6)
 P2_ipos(2:5)=DIP_ipos(3:6)
 !
 if (read_is_on(ID).and.i_fragment==-1) then
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
   if (compute_P2_dipoles)  call DIP_alloc('P_square',(/Dip%ib_lim(2),Dip%ib(2),Dip%ib(1),Dip%ib_lim(1),PAR_K_scheme%nibz/))
   if(l_BS_dichroism) then
     call DIP_alloc('DIP_orbital',(/3,Dip%ib_lim(2),Dip%ib(2),Dip%ib(1),Dip%ib_lim(1),PAR_K_scheme%nibz,1/))
   endif
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
   VAR_ID=6
   VAR_name='DIP_spin'
   if (i_fragment< 1) call def_variable_bulk(ID,trim(VAR_name),VAR_ID,DIP_size,SP,par_io_kind='independent')
   if (i_fragment>=1) call  io_variable_bulk(ID,VAR_ID,C5=DIP_spin(:,:,:,ik_mem:ik_mem,i_sp_pol:i_sp_pol,1),IPOS=DIP_ipos)
   !
 endif
 !
#if defined _RT
 if (compute_Spin_dipoles.and.n_spinor==2) then
   !
   ! P_Spinor
   !
   VAR_ID=7
   VAR_name='DIP_P_spinor'
   if (i_fragment< 1) call def_variable_bulk(ID,trim(VAR_name),VAR_ID,Pspinor_size,SP,par_io_kind='independent')
   if (i_fragment>=1) call  io_variable_bulk(ID,VAR_ID,C6=DIP_P_spinor(:,:,:,:,:,ik_mem:ik_mem),IPOS=Pspinor_ipos)
   !
 endif
#endif
 !
 if (compute_P2_dipoles) then
   !
   ! P2
   !
   VAR_ID=8
   VAR_name='P_square'
   if (i_fragment< 1) call def_variable_bulk(ID,trim(VAR_name),VAR_ID,P2_size,SP,par_io_kind='independent')
   if (i_fragment>=1) call  io_variable_bulk(ID,VAR_ID,C4=P_square(:,:,ik_mem:ik_mem,i_sp_pol:i_sp_pol),IPOS=P2_ipos)
   !
 endif
 !
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
 !
1 call io_disconnect(ID=ID)
 !
 if (io_DIP) call timing('io_DIPOLES',OPR='stop')
 !
end function
