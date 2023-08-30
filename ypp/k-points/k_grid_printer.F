!
! License-Identifier: GPL
!
! Copyright (C) 2018 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine k_grid_printer(nk,k,coo_in,form,wk,coo_out)
 !
 use pars,                ONLY:SP,schlen,pi
 use stderr,              ONLY:f_format_length,intc
 use LIVE_t,              ONLY:live_timing_is_on
 use parser_m,            ONLY:parser
 use vec_operate,         ONLY:c2a
 use com,                 ONLY:msg
 use YPPm,                ONLY:K_transform,alat_used_for_output
 !
 implicit none
 !
 ! Whatever coo_in this routine prints the list of points using coo_out
 !
 integer               ::nk
 real(SP)              ::k(nk,3)
 character(*)          ::coo_in
 character(*)          ::form
 real(SP),optional     ::wk(nk)
 character(*),optional ::coo_out
 !
 ! Work Space
 !
 integer               :: ik
 real(SP)              :: v(3),Vo(4),weight
 character(schlen)     :: ch
 logical               :: No_Weight
 !
 call parser('NoWeights'   ,No_Weight)
 !
 live_timing_is_on=.false.
 !
 do ik=1,nk
   !
   v=k(ik,:)
   if (trim(coo_in)=="rlu") call c2a(v_in=v,mode="ka2i")
   if (trim(coo_in)=="cc")  call c2a(v_in=v,mode="kc2i")
   !
   weight=1._SP
   if (present(wk)) then
    weight=wk(ik)
   endif
   !
   if (present(coo_out)) then
     if (trim(coo_out)=="rlu")  call c2a(v_in=v,mode="ki2a")
     if (trim(coo_out)=="cc" )  call c2a(v_in=v,mode="ki2c")
     if (trim(coo_out)=="alat") then
       call c2a(v_in=v,mode="ki2c")
       v=v*alat_used_for_output/2./pi
     endif
   else
     call K_transform(v,'iku')
   endif
   !
   if (form=="Y") then
     if(No_Weight) then
       write(ch,'(3(f12.'//trim(intc(f_format_length))//',1x,a,1x))') v(1),'|',v(2),'|',v(3)
     else
       write(ch,'(4(f12.'//trim(intc(f_format_length))//',1x,a,1x))') v(1),'|',v(2),'|',v(3),'|',weight
     endif
     call msg("s",'      ',trim(ch))
   else if (form=="PW") then
     write(ch,'(3(f12.'//trim(intc(f_format_length))//',1x),i1)') v(:),1
     call msg("s",'      ',trim(ch))
   else if (form=="none") then
     Vo(:3)=v
     if(.not.No_Weight) Vo(4)=weight
     call msg("s","   k"//trim(intc(ik)),Vo)
   endif
   !
 enddo
 live_timing_is_on=.true.
 !
end subroutine
