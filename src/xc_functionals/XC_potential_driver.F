!
! License-Identifier: GPL
!
! Copyright (C) 2010 The Yambo Team
!
! Authors (see AUTHORS file for details): MG AM
!
!> @brief Calculate exchange-correlation energy and its derivatives
!!
!! @param[in]       en                  electronic levels
!! @param[in]       Xk                  k-point sampling
!! @param[in]       KIND                
!! @param[in]       FUNCTIONAL          exchange-correlation functional
!! @param[in]       ORDER               0 = E_xc, 1 = V_xc, 2 = F_xc 
!! @param[in]       EXX_FRACTION        exchange fraction (optional)
!! @param[in]       EXX_SCREENING       screened exchange fraction (optional)
!!
!! @param[out]      E_xc                exchange correlation energy    (if ORDER=0)
!! @param[out]      V_xc                exchange correlation potential (if ORDER=1)
!! @param[out]      F_xc                exchange correlation kernel    (if ORDER=2)
!
subroutine XC_potential_driver(en,Xk,KIND,FUNCTIONAL,ORDER,EXX_FRACTION,EXX_SCREENING)
 !
 ! Wrapper for calculation of the XC potential. If available libxc,
 ! will be used instead of internal routines
 !
 use pars,           ONLY:SP,rZERO
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels
 use com,            ONLY:msg
 use xc_f03_lib_m,   ONLY:XC_LDA_X, XC_LDA_C_PW
 use xc_functionals, ONLY:XC_EXCHANGE_CORRELATION,xc_string,XC_libxc_driver,&
&                         XC_NOT_AVAILABLE,XC_FACTOR,current_xc_string,LIBXC_message_done,XC_HYB_GGA_XC_GAUPBE
 use IO_int,         ONLY:IO_and_Messaging_switch
#if defined _SC
 use drivers,        ONLY:l_sc_run
#endif
#if defined _RT
 use drivers,        ONLY:l_real_time
#endif
#if defined _NL
 use drivers,        ONLY:l_nl_optics
#endif
 use timing_m,       ONLY:timing
 !
 implicit none
 !
 type(levels),  intent(in)       ::en
 type(bz_samp), intent(in)       ::Xk
 integer,       intent(inout)    :: KIND,FUNCTIONAL
 integer,       intent(in)       :: ORDER
 real(SP),      intent(inout),optional :: EXX_FRACTION, EXX_SCREENING
 !
 ! Work space
 !
 real(SP)          :: local_exx_fraction, local_exx_screening
 !
 call timing('XC_potential_driver',OPR='start')
 !
#if defined _RT
 if (l_real_time) call IO_and_Messaging_switch("SAVE MUTE")
#endif
#if defined _SC
 if (l_sc_run)    call IO_and_Messaging_switch("SAVE MUTE")
#endif
#if defined _NL
 if (l_nl_optics) call IO_and_Messaging_switch("SAVE MUTE")
#endif
 !
 local_exx_fraction = rZERO
 local_exx_screening= rZERO
 if (present(EXX_FRACTION))  local_exx_fraction  = EXX_FRACTION
 if (present(EXX_SCREENING)) local_exx_screening = EXX_SCREENING
 !
 ! COM
 !
 if (KIND>0.and.FUNCTIONAL>0) then
   if (current_xc_string/=xc_string(FUNCTIONAL)) then
     call msg('rsn','[xc] Functional ',xc_string(FUNCTIONAL))
     current_xc_string=xc_string(FUNCTIONAL)
   endif
 else if (FUNCTIONAL==XC_NOT_AVAILABLE) then
   KIND=XC_EXCHANGE_CORRELATION
   FUNCTIONAL=XC_LDA_X*XC_FACTOR+XC_LDA_C_PW
   call msg('rsn','[xc] Functional unknown. Used ',xc_string(FUNCTIONAL))
 endif
 !
 ! Maybe also (spin)density should be calculated in the wrapper, since it does 
 ! not use anything of libxc.   
 !
 if (.not.LIBXC_message_done) then
   call msg('rsn','[xc] LIBXC used to calculate xc functional ')
   LIBXC_message_done=.TRUE.
 endif
 !
 call XC_libxc_driver(en,Xk,FUNCTIONAL,ORDER,EXX_FRACTION=local_exx_fraction,&
&                     EXX_SCREENING=local_exx_screening)
 !
 if (local_exx_fraction.gt.rZERO) then
   if (present(EXX_FRACTION)) EXX_FRACTION = local_exx_fraction 
   call msg('rsn','[xc] Hybrid functional. EXX fraction ',EXX_FRACTION)
 endif
 !
 if (local_exx_screening.gt.rZERO) then
   if (present(EXX_SCREENING)) EXX_SCREENING = local_exx_screening 
     if (FUNCTIONAL==XC_HYB_GGA_XC_GAUPBE*XC_FACTOR) then
       call msg('rsn','[xc] Hybrid functional. EXX GAUPBE parameter ',EXX_SCREENING)
      else
       call msg('rsn','[xc] Hybrid functional. EXX screening ',EXX_SCREENING)
     endif
   if (present(EXX_SCREENING)) EXX_SCREENING = local_exx_screening 
   call msg('rsn','[xc] Hybrid functional. EXX screening ',EXX_SCREENING)
 endif
 !
#if defined _RT
 if (l_real_time) call IO_and_Messaging_switch("RESTORE")
#endif
#if defined _SC
 if (l_sc_run)    call IO_and_Messaging_switch("RESTORE")
#endif
#if defined _NL
 if (l_nl_optics) call IO_and_Messaging_switch("RESTORE")
#endif
 !
 call timing('XC_potential_driver',OPR='stop')
 !
end subroutine XC_potential_driver
