!
!        Copyright (C) 2000-2022 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
subroutine PHEL_from_GF_to_quasiphonons(il,iq,ph_freq,QPH)
 !
 use drivers,        ONLY:l_life
 use pars,           ONLY:SP,cI,rZERO,cZERO
 use units,          ONLY:HA2EV
 use PHEL,           ONLY:PHEL_qp,QPHt,PI_ref,PI_oms,PHEL_fit_Energies
 use stderr,         ONLY:STRING_same
 use QP_m,           ONLY:l_QP_eval_GF,QP_solver
 !
 implicit none
 !
 integer          :: il,iq
 real(SP)         :: ph_freq
 type(QPHt)       :: QPH
 !
 ! Work Space
 !
 integer          :: iw
 complex(SP)      :: sqrt_arg,W,alpha(2),PI_fit(2)
 real(SP)         :: A(QPH%N_D_freqs),A_max,E_fit(2)
 !
 ! REF and OMS
 !=============
 PI_ref     = rZERO
 PI_oms     = cZERO
 !
 ! PI_ref
 if (     l_QP_eval_GF) PI_ref=real(return_PI( 0._SP ))
 if (.not.l_QP_eval_GF) PI_ref=real(PHEL_qp%PI(il,iq,1))
 !
 ! OMS
 if (              l_life      ) PI_oms=PHEL_qp%PI(il,iq,1)
 if (              l_QP_eval_GF) PI_oms=return_PI( ph_freq )
 if (STRING_same(QP_solver,"n")) PI_oms=PHEL_qp%PI(il,iq,3)
 !
 ! OMS+bare Pole 
 !===============
 if (trim(QPH%what)=="OMS_bare_Pole") then
   QPH%Gamma =aimag(FIX_the_SIGN(PI_oms))/2.
   QPH%Pole=ph_freq
   QPH%Residual=QPH%Pole
 endif
 !
 ! DDFPT
 !=======
 if (trim(QPH%what)=="DDFPT") then
   QPH%Pole=PHEL_qp%PI(il,iq,4)-PI_ref+ph_freq
   return
 endif
 !
 ! OMS
 !=====
 if (trim(QPH%what)=="OMS") then
   sqrt_arg=ph_freq**2+ph_freq*real(PI_oms-PI_ref)
   QPH%Pole=1._SP/2._SP*sqrt( sqrt_arg+ sqrt( sqrt_arg**2+ph_freq**2*aimag(PI_oms)**2))
   QPH%Gamma=abs(ph_freq/QPH%Pole/2.*aimag(PI_oms))
   QPH%Residual=QPH%Pole
 endif
 !
 ! QPH
 !=====
 if (trim(QPH%what)=="QPH") then
   E_fit=PHEL_fit_Energies*ph_freq
   if (l_QP_eval_GF) then
     PI_fit(1)=return_PI(E_fit(1))-PI_ref
     PI_fit(2)=return_PI(E_fit(2))-PI_ref
   else
     PI_fit(1:2)=PHEL_qp%PI(il,iq,2:3)-PI_ref
   endif
   alpha(1)=(E_fit(2)**2.*PI_fit(1)-E_fit(1)**2.*PI_fit(2))/(E_fit(1)*E_fit(2))/(E_fit(2)-E_fit(1))
   alpha(2)=(E_fit(2)*PI_fit(1)-E_fit(1)*PI_fit(2))/(E_fit(1)*E_fit(2))/(E_fit(1)-E_fit(2))
   QPH%Residual=ph_freq/(1.-ph_freq*alpha(2))
   QPH%Pole=sqrt(ph_freq*QPH%Residual)
   QPH%Gamma=-alpha(1)*QPH%Residual/2.
   do iw=1,QPH%N_D_freqs
     QPH%PI(iw)=alpha(1)*real( QPH%W(iw) )+alpha(2)*real( QPH%W(iw) )**2
   enddo
 endif
 !
 ! PI...
 !=======
 !... FULL
 if (trim(QPH%what)=="FULL") then
   QPH%PI=PHEL_qp%PI(il,iq,:)-PI_ref
 endif
 !
 ! D...
 !======
 do iw=1,QPH%N_D_freqs
   W=real( QPH%W(iw) ) + cI*0.1_SP/1000._SP/HA2EV
   if (trim(QPH%what)=="FULL") then
     QPH%D(iw)=D_from_PI(cmplx(ph_freq),PHEL_qp%PI(il,iq,iw)-PI_ref,W)
   else if (.not.trim(QPH%what)=="DDFPT)") then
     QPH%D(iw)=D_from_POLE_and_GAMMA(QPH%Pole,QPH%Gamma,QPH%Residual,W)
   endif
   A(iw)=-aimag(QPH%D(iw))
 enddo
 !
 ! MaX
 A_max=maxval(A)
 do iw=1,QPH%N_D_freqs
   if ( abs(A(iw)-A_max)<1.E-5_SP ) QPH%Pole=real(QPH%W(iw))
 enddo
 !
 ! +/- Widths
 do iw=1,QPH%N_D_freqs-1
   if ( A(iw)<A_max/2..and.A(iw+1)>A_max/2. ) QPH%Half_Width_left =abs(real(QPH%W(iw))-QPH%Pole)
   if ( A(iw)>A_max/2..and.A(iw+1)<A_max/2. ) QPH%Half_Width_right=abs(real(QPH%W(iw))-QPH%Pole)
 enddo
 !
 contains
   !
   function return_PI(freq)
     real(SP)    :: freq,W(2)
     complex(SP) :: return_PI
     integer     :: iw_p
     return_PI=cZERO
     do iw_p=1,QPH%N_D_freqs-1
       W=real(QPH%W(iw_p:iw_p+1))
       if ( W(1)<freq.and. W(2) >freq)    return_PI=PHEL_qp%PI(il,iq,iw_p)
       if ( abs(W(1)-freq)<0.00005/HA2EV) return_PI=PHEL_qp%PI(il,iq,iw_p)
       if ( abs(W(2)-freq)<0.00005/HA2EV) return_PI=PHEL_qp%PI(il,iq,iw_p+1)
     enddo
   end function
   !
   function D_from_PI(POLE,PI,freq)
     complex(SP) :: freq,D_from_PI,PI,POLE
     D_from_PI=D_o(POLE,freq)/(1._SP-D_o(POLE,freq)*PI)
   end function
   !
   function D_from_POLE_and_GAMMA(POLE,GAMMA,RES,freq)
     complex(SP) :: D_from_POLE_and_GAMMA,freq,POLE,GAMMA,RES
     D_from_POLE_and_GAMMA=RES/((freq+cI*GAMMA)**2-POLE**2)
   end function
   !
   function D_o(omega_o,freq)
     complex(SP) :: freq,D_o,omega_o
     D_o=-1._SP/2._SP*(1._SP/(freq+omega_o)-1._SP/(freq-omega_o))
   end function
   !
   function FIX_the_sign(CVAL)
     complex(SP) :: CVAL,FIX_the_sign
     FIX_the_sign= real(CVAL)+cI*abs(aimag(CVAL))
   end function
  !
end subroutine
