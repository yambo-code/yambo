!
!        Copyright (C) 2000-2021 the YAMBO team
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
subroutine PHEL_from_GF_to_quasiphonons(il,iq,ph_freq,D)
 !
 use drivers,        ONLY:l_life
 use pars,           ONLY:SP,cI,rZERO,cZERO
 use units,          ONLY:HA2EV
 use PHEL,           ONLY:PH_SF_W,PHEL_qp,QPH
 use QP_m,           ONLY:QP_Sc_steps,QP_G_damp,l_QP_eval_GF,On_Mass_Shell_approx
 !
 implicit none
 !
 integer          :: il,iq
 real(SP)         :: ph_freq
 complex(SP)      :: D(QP_Sc_steps)
 !
 ! Work Space
 !
 integer          :: iw,ip,i_err
 complex(SP)      :: sqrt_arg,W,Poles(2)
 real(SP)         :: A(QP_Sc_steps),A_max
 !
 ! Zeroing
 QPH%PI_ref = rZERO
 QPH%PI_oms = cZERO
 QPH%beta   = cZERO
 QPH%Z      = cZERO
 QPH%Pole   = cZERO
 QPH%Half_Width_left  = rZERO
 QPH%Half_Width_right = rZERO
 !
 ! Erroring
 i_err=0
 !
 ! PI_ref
 if (.not.l_life) QPH%PI_ref=real ( return_PI(0._SP) )
 !
 ! OMS
 if (l_life) then
   QPH%PI_oms=FIX_the_sign(PHEL_qp%PI(il,iq,1))
   return
 else
   QPH%PI_oms=return_PI( ph_freq )
 endif
 QPH%Z=ph_freq/QPH%PI_oms
 if (On_Mass_Shell_approx) then
   QPH%PI_oms=FIX_the_sign(QPH%PI_oms)
   return
 endif
 !
 ! Beta
 QPH%beta= (return_PI( ph_freq + QP_G_damp)-QPH%PI_oms)/QP_G_damp
 !
 ! QuasiPhonon (QPH)
 sqrt_arg=((QPH%PI_oms-QPH%PI_ref)**2+4.*ph_freq**2)
 Poles(1)=(QPH%PI_oms-QPH%PI_ref+sqrt( sqrt_arg))/2._SP
 Poles(2)=(QPH%PI_oms-QPH%PI_ref-sqrt( sqrt_arg))/2._SP
 QPH%Pole=Poles(1)
 if (real(sqrt_arg)<0) then
   i_err=-1
   QPH%Pole=0._SP
 endif
 !
 ! QPH Z
 QPH%Z=0._SP
 if (i_err==0) QPH%Z=2._SP*ph_freq/(Poles(1)-Poles(2))
 !
 if (.not.l_QP_eval_GF) then
   QPH%Pole  =FIX_the_sign(QPH%Pole)
   QPH%PI_oms=FIX_the_sign(QPH%PI_oms)
   return
 endif
 !
 ! D
 do iw=1,QP_Sc_steps
   W=real( PH_SF_W(il,iq)%p(iw) ) + cI*0.1_SP/1000._SP/HA2EV
   D(iw)=D_o(ph_freq,W)/(1._SP-D_o(ph_freq,W)*(PHEL_qp%PI(il,iq,iw)-QPH%PI_ref))
   A(iw)=-aimag(D(iw))
 enddo
 !
 ! MaX
 A_max=maxval(A)
 do iw=1,QP_Sc_steps
   if ( abs(A(iw)-A_max)<1.E-5_SP ) QPH%Pole=real(PH_SF_W(il,iq)%p(iw))
 enddo
 !
 ! +/- Widths
 do iw=1,QP_Sc_steps-1
   if ( A(iw)<A_max/2..and.A(iw+1)>A_max/2. ) QPH%Half_Width_left =abs(real(PH_SF_W(il,iq)%p(iw))-real(QPH%Pole))
   if ( A(iw)>A_max/2..and.A(iw+1)<A_max/2. ) QPH%Half_Width_right=abs(real(PH_SF_W(il,iq)%p(iw))-real(QPH%Pole))
 enddo
 !
 QPH%Pole  =FIX_the_sign(QPH%Pole)
 QPH%PI_oms=FIX_the_sign(QPH%PI_oms)
 !
 contains
   !
   function return_PI(freq)
     real(SP)    :: freq,W(2)
     complex(SP) :: return_PI
     do iw=1,QP_Sc_steps-1
       W=real(PH_SF_W(il,iq)%p(iw:iw+1))
       if ( W(1)<freq.and. W(2) >freq)    return_PI=PHEL_qp%PI(il,iq,iw)
       if ( abs(W(1)-freq)<0.00005/HA2EV) return_PI=PHEL_qp%PI(il,iq,iw)
       if ( abs(W(2)-freq)<0.00005/HA2EV) return_PI=PHEL_qp%PI(il,iq,iw+1)
     enddo
   end function
   !
   function D_o(omega_o,freq)
     real(SP)    :: omega_o
     complex(SP) :: freq,D_o
     D_o=-1._SP/2._SP*(1._SP/(freq+omega_o)-1._SP/(freq-omega_o))
   end function
   !
   function FIX_the_sign(CVAL)
     complex(SP) :: CVAL,FIX_the_sign
     FIX_the_sign= real(CVAL)+cI*abs(aimag(CVAL))
   end function
   !
end subroutine
