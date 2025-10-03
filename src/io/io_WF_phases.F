!
! License-Identifier: GPL
!
! Copyright (C) 2011 The Yambo Team
!
! Authors (see AUTHORS file for details): DS
!
integer function io_WF_phases(nb,ik,istark,i_sp_pol,ID,nsz,WF_phases)
 !
 use R_lattice,     ONLY:nXkibz
 use D_lattice,     ONLY:nsym
 use electrons,     ONLY:n_sp_pol,n_max_deg
 use pars,          ONLY:schlen,SP,SP_YIO,IP_YIO
 use IO_m,          ONLY:io_sec,io_extension,read_is_on
 use IO_int,        ONLY:io_connect,io_disconnect,io_fragment,def_variable_bulk,&
&                        io_variable_bulk,io_header,def_variable_elemental,io_variable_elemental
 use wave_func,     ONLY:WF_phases_b_map
 use timing_m,      ONLY:timing
 !
 implicit none
 !
 integer               :: nb(2),ik,istark,i_sp_pol,ID
 integer, intent(in)   :: nsz(5)
 complex(SP), optional :: WF_phases(:,:,:,:,:)
 !
 ! Work Space
 !
 integer :: VAR_ID,VAR_SIZE(6),VAR_POS(6),i_fragment,ID_frag
 character(schlen)    :: VAR_name,DIM_NAMES(6)
 !
 call timing('io_WF_phases',OPR='start')
 !
 io_extension(ID)='wf_phases'
 !
 io_WF_phases=io_connect(desc=trim(io_extension(ID)),type=2,ID=ID)
 if (io_WF_phases/=0) goto 1
 !
 if (io_sec(ID,1)==0) goto 1
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   io_WF_phases=io_header(ID,R_LATT=.true.,WF=.true.,IMPOSE_SN=.true.,TEMP=.true.)
   if (io_WF_phases/=0) goto 1
   !
   call def_variable_elemental(ID,"nbands",2,IP_YIO,0)
   call io_variable_elemental(ID,VAR="Number of bands",I1=nb,CHECK=.true.,OP=(/"==","=="/))
   !call io_variable_elemental(ID,VAR="Number of bands",I1=nb,CHECK=.true.,OP=(/">=","<="/))
   !
 endif
 !
 ! WF_phases_b_map
 !
 if (any((/io_sec(ID,:)==2/))) then
   VAR_ID=2
   VAR_name='WF_phases_b_map'
   call def_variable_bulk(ID,trim(VAR_name),VAR_ID,shape(WF_phases_b_map),IP_YIO)
   call  io_variable_bulk(ID,VAR_ID,I4=WF_phases_b_map)
   !
 endif
 !
 ! WF_phases
 !
 if (any((/io_sec(ID,:)==3/))) then
   !
   ! Fragmentation
   !
   i_fragment=ik+(i_sp_pol-1)*nXkibz
   call io_fragment(ID,ID_frag,i_fragment=i_fragment)
   !
   VAR_ID=1
   VAR_name='WF_phases'
   VAR_SIZE=(/2,nsz(1),nsz(2),nsz(3),nsz(4),nsz(5)/)
   DIM_NAMES = [character(schlen) :: 're_im', 'bands', 'bands', 'bands_grp', 'syms', 'istark']
   !VAR_SIZE=(/2,shape(WF_phases)/)
   VAR_POS=(/1,1,1,1,1,1/)
   if(istark>0) VAR_POS=(/1,1,1,1,1,istark/)
   !
   call def_variable_bulk(ID_frag,trim(VAR_name),VAR_ID,VAR_SIZE,SP_YIO,DIM_NAMES)
   call io_variable_bulk(ID_frag,VAR_ID,C5=WF_phases,IPOS=VAR_POS)
   ! 
   call io_fragment_disconnect(ID,ID_frag)
   !
 endif
 !
1 call io_disconnect(ID=ID)
 !
 call timing('io_WF_phases',OPR='stop')
 !
end function
