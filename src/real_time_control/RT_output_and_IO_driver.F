!
! License-Identifier: GPL
!
! Copyright (C) 2017 The Yambo Team
!
! Authors (see AUTHORS file for details): DS
!
subroutine RT_output_and_IO_driver(what,E,k,OBSERVABLES)
 !
 use pars,           ONLY:SP
 use RT_control,     ONLY:RT_output,RT_DB_ID,N_RT_databases
 use units,          ONLY:AUT2FS
 use electrons,      ONLY:levels
 use R_lattice,      ONLY:bz_samp
 use IO_m,           ONLY:WR_CL
 use IO_int,         ONLY:IO_and_Messaging_switch,io_control,io_disconnect
 use RT_lifetimes,   ONLY:RT_EP_emit_life,RT_EE_life,RT_EH_life,RT_EP_abs_life,&
&                         RT_EP_emit_REF_life,RT_EE_REF_life,RT_EH_REF_life,RT_EP_abs_REF_life
 use real_time,      ONLY:eval_energy,eval_entropy,l_RT_update_Energies,l_NE_with_fields,NE_time, &
&                         l_RT_induced_field,l_RT_impose_N_dN,l_RT_impose_E,RT_is_dynamically_dephased
 use D_lattice,      ONLY:Bose_Temp
 !
 implicit none
 !
 character(*)           :: what
 type(levels)           :: E
 type(bz_samp)          :: k
 real(SP), intent(inout):: OBSERVABLES(25)
 !
 ! Workspace
 !
 integer  :: i_ID
 real(SP) :: NCarr(3),TFit(2),EfFit(2),Ener(5),Entr(3),TEntr(3),EnEntr(3),BGR(4)
 logical, external   :: RT_do_it
 !
 select case(trim(what))
   !
   case('open','close')
     !
     if(trim(what)=="open") OBSERVABLES=0._SP
     !
     call RT_output(what=trim(what)//" curr pol")
     !
     call RT_output(what=trim(what)//" carr spin orbt")
     if (eval_energy)  call RT_output(what=trim(what)//" energy")
     if (eval_entropy) call RT_output(what=trim(what)//" therm")
     if (l_RT_update_Energies) call RT_output(what=trim(what)//" BGR")
     if (RT_EP_emit_life%active) call RT_output(trim(what)//' mean_EPlife')
     if (RT_EE_life%active     ) call RT_output(trim(what)//' mean_EElife')
     if (RT_EH_life%active     ) call RT_output(trim(what)//' mean_RADlife')
     if ((RT_EP_emit_life%active.or.RT_EE_life%active).and.&
&        (l_RT_impose_N_dN.or.l_RT_impose_E)) call RT_output(trim(what)//' N_dN_E')
     !
     ! Field(s)
     !==========
     if (l_NE_with_fields) then
       call RT_output(what=trim(what)//" ext")
       if(l_RT_induced_field)  call RT_output(what=trim(what)//" tot ind")
     endif
     !
     if(trim(what)=="close") then
       do i_ID=1,N_RT_databases
         if (RT_DB_ID(i_ID)==0) cycle
         call io_control(ACTION=WR_CL,ID=RT_DB_ID(i_ID))
         call io_disconnect(ID=RT_DB_ID(i_ID))
       enddo
     endif
     !
   case('write')
     !
     if (RT_is_dynamically_dephased) call RT_Lifetimes_merge_driver("INTER_k")
     !
     NCarr =OBSERVABLES( 1: 3)
     TFit  =OBSERVABLES( 4: 5)
     EfFit =OBSERVABLES( 6: 7)
     Ener  =OBSERVABLES( 8:12)
     Entr  =OBSERVABLES(13:15)
     TEntr =OBSERVABLES(16:18)
     EnEntr=OBSERVABLES(19:21)
     BGR   =OBSERVABLES(22:25)
     !
     call IO_and_Messaging_switch("-fragments_IO_log")
     call RT_databases_IO(E,.FALSE.)
     call IO_and_Messaging_switch("+fragments_IO_log")
     !
     ! OUTPUT(s)
     !###########
     !
     if (.not.RT_do_it('OUT')) return
     !
     call RT_output('curr pol',TIME=NE_time*AUT2FS)
     !
     ! Energetics and BGR
     !====================
     !
     call RT_output('spin orbt',TIME=NE_time*AUT2FS)
     call RT_output('carr',VALUES=(/NCarr(:),TFit(:),EfFit(:)/),TIME=NE_time*AUT2FS)
     if (eval_energy)          call RT_output('energy',VALUES=Ener,                          TIME=NE_time*AUT2FS)
     if (eval_entropy)         call RT_output('therm ',VALUES=(/EnEntr(:),Entr(:),TEntr(:)/),TIME=NE_time*AUT2FS)
     if (l_RT_update_Energies) call RT_output('BGR',   VALUES=BGR,                           TIME=NE_time*AUT2FS)
     !
     ! Mean Carrier Lifetimes
     !========================
     if (RT_is_dynamically_dephased) then
       !
       call RT_mean_lifetimes_and_temperatures( RT_EP_emit_life, RT_EP_emit_REF_life )
       call RT_mean_lifetimes_and_temperatures( RT_EP_abs_life , RT_EP_abs_REF_life )
       if (RT_EP_emit_life%active) call RT_output('mean_EPlife', TIME=NE_time*AUT2FS)
       call RT_mean_lifetimes_and_temperatures( RT_EE_life, RT_EE_REF_life )
       if (RT_EE_life%active) call RT_output('mean_EElife', TIME=NE_time*AUT2FS)
       call RT_mean_lifetimes_and_temperatures( RT_EH_life, RT_EH_REF_life )
       if (RT_EH_life%active) call RT_output('mean_RADlife', TIME=NE_time*AUT2FS)
       !
       if (l_RT_impose_N_dN.or.l_RT_impose_E) call RT_output('N_dN_E',TIME=NE_time*AUT2FS)
       !
     endif
     !
     ! Field(s)
     !==========
     if (l_NE_with_fields) then
       call RT_output('external_field', TIME=NE_time*AUT2FS)
       if(l_RT_induced_field) then
         call RT_output('total_field'  ,TIME=NE_time*AUT2FS)
         call RT_output('induced_field',TIME=NE_time*AUT2FS)
       endif
     endif
     !
 end select
 !
end subroutine RT_output_and_IO_driver
