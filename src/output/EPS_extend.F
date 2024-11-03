!
! License-Identifier: GPL
!
! Copyright (C) 2009 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
subroutine EPS_extend(wv,nw,iw_ref,be_verbose,ordering,eps,eps_extended)
 !
 ! extend epsilon to the negative axes if needed
 ! This subroutines always works with (eps - 1.) to avoid numerical noise.
 ! It is based on Kramers-Kronig relations, so it gives the retared response
 ! 
 use pars,          ONLY:SP,pi,cZERO,cI
 use units,         ONLY:HA2EV
 use com,           ONLY:msg
 !
 implicit none
 !
 integer,      intent(in) :: nw,iw_ref
 complex(SP),  intent(in) :: wv(nw)
 logical,      intent(in) :: be_verbose
 character(2), intent(in) :: ordering
 complex(SP),  intent(in) :: eps(nw)
 complex(SP), intent(out) :: eps_extended(nw)
 !
 ! Work Space
 !
 integer            :: iw,nw_kk
 complex(SP)        :: wv_kk(nw),eps_kk(nw-iw_ref+1)
 real(SP)           :: wv_kk_p(nw-iw_ref+1),wv_kk_m(nw-iw_ref+1)
 !
 if (trim(ordering)=='R'.or.trim(ordering)=="T") then
   eps_extended=eps
   return
 endif
 !
 if ( be_verbose ) then
   !
   call msg('s',' EPS via KK, poles at w>0 reflected also in w<0')
   !
   if ( (maxval(aimag(eps))/aimag(eps(1+iw_ref-1))<100._SP .or. &
&        maxval(aimag(eps))/aimag(eps(nw))<100._SP)  ) &
&    call warning(' [EPS_extend] Possible inaccuracy in the EPS prolungation')
   !
 endif
 !
 nw_kk=nw-iw_ref+1
 !
 eps_extended   =cZERO
 wv_kk          = real(wv)+cI*1.E-4_SP/HA2EV
 !
 wv_kk_p        = real(wv(iw_ref:nw))
 eps_kk         =-1._SP/pi*aimag(eps(iw_ref:nw))
 eps_kk(1)      =cZERO
 eps_kk(nw_kk)  =cZERO
 !
 call Kramers_Kronig(eps_kk,wv_kk_p,nw_kk,eps_extended,wv_kk,nw,cZERO)
 !
 ! extend eps to the negative frequencies
 !
 do iw=1,nw_kk
   wv_kk_m(iw)  =-real(wv(nw-iw+1))
   eps_kk(iw)   = 1._SP/pi*aimag(eps(nw-iw+1))
 enddo
 eps_kk(1)      =cZERO
 eps_kk(nw_kk)  =cZERO
 call Kramers_Kronig(eps_kk,wv_kk_m,nw_kk,eps_extended,wv_kk,nw,cZERO)
 !
end subroutine EPS_extend
