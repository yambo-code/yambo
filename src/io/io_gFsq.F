!
! License-Identifier: GPL
!
! Copyright (C) 2022 The Yambo Team
!
! Authors (see AUTHORS file for details): AM AC
!
integer function io_gFsq(ID)
 !
 use pars,           ONLY:schlen
 use IO_m,           ONLY:io_sec,read_is_on,frag_ELPH
 use IO_int,         ONLY:io_connect,io_disconnect,io_fragment,&
&                         io_bulk,io_header
 use ELPH,           ONLY:ph_modes,PH_freqs_sq,elph_nQ,&
&                         gsqF_fan,gsqF_dw,gsqF_ca_corr,gsqF_life_bose ,gsqF_life_f
 use stderr,         ONLY:intc
 use electrons,      ONLY:n_sp_pol
 use QP_m,           ONLY:QP_n_states,QP_table
 use parallel_m,     ONLY:PAR_Q_bz_index
 use timing_m,       ONLY:timing
 !
#include<memory.h>
 !
 integer      ::ID
 !
 !Work Space
 !
 integer                ::iq,ierr,iq_mem,ID_frag
 character(schlen)      ::db_name
 integer, external      ::variables_ELPH
 !
 call timing('io_gFsq',OPR='start')
 !
 ID_frag=0
 !
 ! DB name
 !
 db_name='elph_gFsq'
 !
 io_gFsq=io_connect(desc=trim(db_name),type=2,ID=ID,FORCE_READ_MODE=(.not.any((/io_sec(ID,:)==1/))).and.frag_ELPH)
 if (io_gFsq/=0) goto 1
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   io_gFsq=io_header(ID,R_LATT=.true.,KPTS=.true.,FRAG=frag_ELPH)
   if (io_gFsq/=0) goto 1
   !
   io_gFsq=variables_ELPH(ID,'gFsq')
   if (io_gFsq/=0) goto 1
   !
 endif
 !
 iq=maxval(io_sec(ID,:))-1
 if (iq<=0) goto 1
 !
 ! Fragmentation
 !
 call io_fragment(ID,ID_frag,i_fragment=iq,ierr=io_gFsq)
 if (ierr<0.and.read_is_on(ID)) goto 1
 !
 ! When the DB is fragmented I allow a partial reading checking
 ! if the fragment exists or not.
 ! If the fragment does not exist (ierr<0) I return an error code
 !
 !
 if (iq==1) then
   call io_bulk(ID_frag,VAR="QP_table",VAR_SZ=(/QP_n_states,3+n_sp_pol-1/))
   call io_bulk(ID_frag,I2=QP_table)
 endif
 !
 iq_mem=iq
 if (allocated(PAR_Q_bz_index)) iq_mem=PAR_Q_bz_index(iq)
 !
 call io_bulk(ID_frag,VAR="ELPH_GFSQ_fan_Q"//trim(intc(iq)),VAR_SZ=(/QP_n_states,ph_modes/))
 call io_bulk(ID_frag,R2=gsqF_fan(:,iq_mem,:,1))
 call io_bulk(ID_frag,VAR="ELPH_GFSQ_dw_Q"//trim(intc(iq)),VAR_SZ=(/QP_n_states,ph_modes/))
 call io_bulk(ID_frag,R2=gsqF_dw(:,iq_mem,:))
 call io_bulk(ID_frag,VAR="ELPH_GFSQ_ca_corr_Q"//trim(intc(iq)),VAR_SZ=(/QP_n_states,ph_modes/))
 call io_bulk(ID_frag,R2=gsqF_ca_corr(:,iq_mem,:,1))
 call io_bulk(ID_frag,VAR="ELPH_GFSQ_life_bose_Q"//trim(intc(iq)),VAR_SZ=(/QP_n_states,ph_modes/))
 call io_bulk(ID_frag,R2=gsqF_life_bose(:,iq_mem,:,1))
 call io_bulk(ID_frag,VAR="ELPH_GFSQ_life_f_Q"//trim(intc(iq)),VAR_SZ=(/QP_n_states,ph_modes/))
 call io_bulk(ID_frag,R2=gsqF_life_f(:,iq_mem,:,1))
 !
 ! PH frequencies (again)
 !
 if (iq==1) then
   call io_bulk(ID_frag,VAR="PH_FREQS",VAR_SZ=(/elph_nQ,ph_modes/))
   call io_bulk(ID_frag,R2=PH_freqs_sq)
 endif
 !
1 call io_fragment_disconnect(ID,ID_frag)
 !
 call io_disconnect(ID)
 !
 call timing("io_gFsq",OPR="STOP")
 !
end function
