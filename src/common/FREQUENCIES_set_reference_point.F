!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): DS
!
subroutine FREQUENCIES_set_reference_point(omega,n_freq,iw_ref)
 !
 ! Look for the W(iw_ref) closest 0
 ! This is needed by the velocity gauge to impose the diamagnetic sum-rule
 !
 use pars,          ONLY:SP
 use X_m,           ONLY:global_gauge,skip_cond_sum_rule
 use parser_m,      ONLY:parser
 !
 implicit none
 !
 integer,     intent(in)        :: n_freq
 complex(SP), intent(inout)     :: omega(n_freq)
 integer,     intent(out)       :: iw_ref
 !
 ! Work space
 !
 integer :: iw
 !
 iw_ref=1
 !
 if(trim(global_gauge)=="velocity") call parser('NoCondSumRule',skip_cond_sum_rule)
 !
 !if(all(abs(real(omega(:)))>0.01) .and. trim(global_gauge)=="velocity".and..not.skip_cond_sum_rule) then
 !  call warning(' No w close to 0 in the frequency grid. Length gauge imposed')
 !  skip_cond_sum_rule=.false.
 !  global_gauge='length'
 !  return
 !endif
 !
 do iw=1,n_freq
   if(abs(real(omega(iw)))>0.01) cycle
   if(abs(omega(iw))<abs(omega(iw_ref))) iw_ref=iw
 enddo
 !
 if(trim(global_gauge)=="velocity" .and. abs(real(omega(iw_ref)))==0._SP) omega(iw_ref)=cmplx(1.E-10,aimag(omega(iw_ref)))
 !
end subroutine FREQUENCIES_set_reference_point
