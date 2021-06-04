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
subroutine PHEL_from_GF_to_quasiphonons(il,iq,ph_freq,PI_ref,D,QPH_Z,OMS_freq,QPH_freq)
 !
 use pars,           ONLY:SP,cI
 use units,          ONLY:HA2EV
 use PHEL,           ONLY:PH_SF_W,PHEL_qp
 use QP_m,           ONLY:QP_Sc_steps,QP_G_damp,l_QP_eval_GF,On_Mass_Shell_approx
 !
 implicit none
 !
 integer          :: il,iq
 real(SP)         :: ph_freq,PI_ref
 complex(SP)      :: D(QP_Sc_steps),QPH_Z,OMS_freq,QPH_freq
 !
 ! Work Space
 !
 integer          :: iw,ip,i_err
 complex(SP)      :: sqrt_arg,W,beta,Poles(2)
 !
 ! Erroring
 i_err=0
 !
 ! PI_ref
 PI_ref=real ( return_PI(0._SP) )
 !
 ! OMS
 OMS_freq=return_PI( ph_freq )
 QPH_Z=ph_freq/OMS_freq
 if (On_Mass_Shell_approx) return
 !
 ! Beta
 beta= (return_PI( ph_freq + QP_G_damp)-OMS_freq)/QP_G_damp
 !
 ! QuasiPhonon (QPH)
 sqrt_arg= (beta*ph_freq)**2-4.*ph_freq*(beta*ph_freq-OMS_freq+PI_ref-ph_freq)
 Poles(1)=(beta*ph_freq+sqrt( sqrt_arg))/2._SP
 Poles(2)=(beta*ph_freq-sqrt( sqrt_arg))/2._SP
 if (real(sqrt_arg)<0) then
   i_err=-1
   QPH_freq=0._SP
 else
   QPH_freq=Poles(1)
 endif
 !
 ! QPH Z
 if (i_err==0) then
   QPH_Z=2._SP*ph_freq/(Poles(1)-Poles(2))
 else
   QPH_Z=0._SP
 endif
 !
 if (.not.l_QP_eval_GF) return
 !
 ! D
 do iw=1,QP_Sc_steps
   W=real( PH_SF_W(il,iq)%p(iw) ) + cI*1._SP/1000._SP/HA2EV
   D(iw)=D_o(ph_freq,W)/(1._SP-D_o(ph_freq,W)*(PHEL_qp%PI(il,iq,iw)-PI_ref))
 enddo
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
end subroutine
