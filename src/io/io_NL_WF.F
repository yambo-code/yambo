!
! License-Identifier: GPL
!
! Copyright (C) 2024 The Yambo Team
!
! Authors (see AUTHORS file for details): CA
!
!> @brief Write/Read wave-function at time NE_i_time together polarization and current
!!
!! @param[in]       i_f                         run index
!! @param[in]       en                          electronic levels
!! @param[in]       ID                          IO file ID
!
integer function io_NL_WF(en,i_f,ID)
 !
 use pars,          ONLY:schlen,SP,IP,CR_YIO,IP_YIO,cONE,cZERO
 use units,         ONLY:HA2EV, AU2KWCMm2, AUT2FS
 use electrons,     ONLY:n_sp_pol,levels
 use nl_optics,     ONLY:NL_P_t,n_frequencies,NL_J_t,NE_steps_done,NL_bands,NL_nbands,V_bands
 use real_time,     ONLY:NE_steps,NE_i_time
 use IO_m,          ONLY:io_sec,read_is_on,write_is_on,io_extension
 use IO_int,        ONLY:io_connect,io_disconnect,io_bulk,io_header,def_variable_elemental,&
&                        io_elemental,io_variable_elemental
 use stderr,        ONLY:intc
 !
#if defined _TIMING
 use timing_m,      ONLY:timing
#endif
 !
#include<memory.h>
 !
 type(levels), intent(in) :: en
 integer,      intent(in) :: ID
 integer,      intent(in) :: i_f
 !
 integer, external    :: variables_NL
 !
 ! Work Space
 !
 integer              :: ib,max_nbf,NE_steps_save
 character(schlen)    :: VAR_name
 complex(SP), allocatable :: damp_var(:,:,:)
 !
#if defined _TIMING
 call timing('io_NL_WF',OPR='start')
#endif
 !
 io_extension(ID)="NL_restart_Run"//intc(i_f)
 !
 io_NL_WF=io_connect(desc=trim(io_extension(ID)),type=2,ID=ID,FORCE_READ_MODE=(.not.any((/io_sec(ID,:)==1/))) )
 if (io_NL_WF/=0) goto 1
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   io_NL_WF=io_header(ID,R_LATT=.true.,WF=.true.,IMPOSE_SN=.true.,GAUGE=.true.)
   if (io_NL_WF/=0) goto 1
   !
   ! I do not want to overwrite NL_steps
   ! 
!   NE_steps_save=NE_steps
!  io_NL_WF=variables_NL(ID)
!  NE_steps=NE_steps_save
   !
   ! Number of steps done in each fragment
   !
   if(write_is_on(ID)) NE_steps_done=NE_i_time
   call def_variable_elemental(ID,"NE_steps_done",1,IP_YIO,0)
   call io_variable_elemental(ID,VAR="[RT] Time steps done",I0=NE_steps_done)
   if (io_NL_WF/=0) goto 1
   !
 endif
 !
 if (any((/io_sec(ID,:)==2/))) then
   ! 
   ! Polarization
   ! 
   write (VAR_name,'(a,i4.4)') 'NL_P_freq_',i_f
   call io_bulk(ID,VAR=trim(VAR_name),VAR_SZ=(/NE_steps_done,3/))
   call io_bulk(ID,R2=NL_P_t(1:NE_steps_done,:))
   !
   ! Current
   !
   write (VAR_name,'(a,i4.4)') 'NL_J_freq_',i_f
   call io_bulk(ID,VAR=trim(VAR_name),VAR_SZ=(/NE_steps_done,6/))
   call io_bulk(ID,R2=NL_J_t(1:NE_steps_done,:))
   !
   ! WF
   !
   if(read_is_on(ID)) then
     V_bands=cZERO
     do ib=1,NL_bands(1)-1,1
       V_bands(ib,ib,:,:)=cONE
     enddo
   endif
   max_nbf=maxval(en%nbf)
   YAMBO_ALLOC(damp_var,(max_nbf,en%nk,n_sp_pol))
   do ib=NL_bands(1),NL_bands(2)
     write (VAR_name,'(a,i4.4,a,i4.4)') 'WF_b_',ib,'_r_',i_f
     call io_bulk(ID,VAR=trim(VAR_name),VAR_SZ=(/2,max_nbf,en%nk,n_sp_pol/))
     if(write_is_on(ID)) damp_var=V_bands(ib,:,:,:)
     call io_bulk(ID,C3=damp_var)
     if(read_is_on(ID)) V_bands(ib,:,:,:)=damp_var
   enddo
   YAMBO_FREE(damp_var)
   !
 endif
 !
1 call io_disconnect(ID=ID)
 !
#if defined _TIMING
 call timing('io_NL_WF',OPR='stop')
#endif
 !
end function
