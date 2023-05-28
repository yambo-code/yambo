!
!        Copyright (C) 2000-2023 the YAMBO team
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
subroutine PH_of(q)
 !
 use pars,           ONLY:SP,schlen
 use drivers,        ONLY:l_life
 use OUTPUT,         ONLY:OUTPUT_driver
 use PHEL,           ONLY:PH_Q,PHEL_qp,PH_energy,&
&                         QPH_OMS,QPH_OMS_bare_Pole,QPH_full,QPH,QPH_DDFPT
 use ELPH,           ONLY:elph_branches,PHEL_desc,ELPH_desc
 use R_lattice,      ONLY:bz_samp
 use interfaces,     ONLY:DESC_write
 use QP_m,           ONLY:l_extended_output,l_QP_eval_GF,On_Mass_Shell_approx
 use stderr,         ONLY:intc
 !
#include<memory.h>
 !
 type(bz_samp),     intent(in) :: q
 !
 ! Work Space
 !
 integer          :: iq,il,iw,ip,s(2),i_f
 real(SP)         :: ph_freq
 character(schlen):: files(2)
 !
 ! Ouput init...
 call OUTPUT_driver("ph",action="reset")
 call OUTPUT_driver("ph",action="open")
 call DESC_write('o ph','#',PHEL_desc)
 call DESC_write('o ph','#',ELPH_desc)
 !
 do iq=PH_Q(1),PH_Q(2)
   do il=elph_branches(1),elph_branches(2)
     !
     ! PH freq
     ph_freq=PH_energy(q,il,iq)
     !
     ! PI -> QPH properties ?
     call PHEL_from_GF_to_quasiphonons(il,iq,ph_freq,QPH_OMS_bare_Pole)
     if (.not.l_life) then
       if (On_Mass_Shell_approx) call PHEL_from_GF_to_quasiphonons(il,iq,ph_freq,QPH_OMS)
       call PHEL_from_GF_to_quasiphonons(il,iq,ph_freq,QPH)
       if (     l_QP_eval_GF) call PHEL_from_GF_to_quasiphonons(il,iq,ph_freq,QPH_full)
       if (.not.l_QP_eval_GF) call PHEL_from_GF_to_quasiphonons(il,iq,ph_freq,QPH_DDFPT)
     endif
     !
     ! D's & PI's
     files(1)="D_ph_Q"//trim(intc(iq))//"_Branch_"//trim(intc(il))
     files(2)="PI_ph_Q"//trim(intc(iq))//"_Branch_"//trim(intc(il))
     do iw=1,QPH%N_D_freqs
       do i_f=1,2
         if (iw==1) then
           call OUTPUT_driver(trim(files(i_f)),action="reset")
           call OUTPUT_driver(trim(files(i_f)),action="open")
           call DESC_write('o '//trim(files(i_f)),'#',PHEL_desc)
           call DESC_write('o '//trim(files(i_f)),'#',ELPH_desc)
         endif
         call OUTPUT_driver(trim(files(i_f)),TITLES=(/"Energy"/),R_VALUES=(/real(QPH%W(iw))/),UNIT="meV")
       enddo
       if (l_QP_eval_GF) then 
         call OUTPUT_driver(trim(files(1)),TITLES=(/"-Im[D]","Re[D] "/),&
&                               R_VALUES=(/-aimag(QPH_full%D(iw)),real(QPH_full%D(iw))/))
       endif
       if (On_Mass_Shell_approx) then
         call OUTPUT_driver(trim(files(1)),TITLES=(/"-Im[D](OMS)","Re[D](OMS) "/),&
&                               R_VALUES=(/-aimag(QPH_OMS%D(iw)),real(QPH_OMS%D(iw))/))
       endif
       call OUTPUT_driver(trim(files(1)),TITLES=(/"-Im[D](OMS+BP)","Re[D](OMS+BP) "/),&
&                             R_VALUES=(/-aimag(QPH_OMS_bare_Pole%D(iw)),real(QPH_OMS_bare_Pole%D(iw))/))
       call OUTPUT_driver(trim(files(1)),TITLES=(/"-Im[D](QPH)","Re[D](QPH) "/),&
&                             R_VALUES=(/-aimag(QPH%D(iw)),real(QPH%D(iw))/))
       call OUTPUT_driver(trim(files(1)),action="write")
       if (l_QP_eval_GF) then 
         call OUTPUT_driver(trim(files(2)),TITLES=(/"Im[\Pi]","Re[\Pi]"/),&
&                               R_VALUES=(/aimag(QPH_full%PI(iw)),real(QPH_full%PI(iw))/),UNIT="meV")
       endif
       call OUTPUT_driver(trim(files(2)),TITLES=(/"Im[\Pi](QPH)","Re[\Pi](QPH)"/),&
&                             R_VALUES=(/aimag(QPH%PI(iw)),real(QPH%PI(iw))/),UNIT="meV")
       call OUTPUT_driver(trim(files(2)),action="write")
     enddo
     call OUTPUT_driver(trim(files(1)),action="close")
     call OUTPUT_driver(trim(files(2)),action="close")
     !
     ! o.ph
     !======
     call OUTPUT_driver("ph",TITLES=(/"Q-point","Branch "/),I_VALUES=(/iq,il/))
     call OUTPUT_driver("ph",TITLES=(/"Eo"/),R_VALUES=(/ph_freq/),UNIT="meV")
     !
     ! Lifetimes...
     if (l_life) then
       call OUTPUT_driver("ph",TITLES=(/"FWMH"/),R_VALUES=(/real(QPH_OMS_bare_Pole%GAMMA)*2./),UNIT="meV")
       if (l_extended_output) then
         call OUTPUT_driver("ph",TITLES=(/"FWMH(interb)"/),R_VALUES=(/PHEL_qp%interband(il,iq)/),UNIT="meV")
         call OUTPUT_driver("ph",TITLES=(/"FWMH(intrab)"/),R_VALUES=(/PHEL_qp%intraband(il,iq)/),UNIT="meV")
         call OUTPUT_driver("ph",TITLES=(/"FWMH(Allen)"/),R_VALUES=(/PHEL_qp%allen(il,iq)/),UNIT="meV")
       endif
       call OUTPUT_driver("ph",action="write")
       cycle
     endif
     !
     ! ...Energies 
     if (l_QP_eval_GF) then
       call OUTPUT_driver("ph",TITLES=(/"E(SF)"/),R_VALUES=(/real(QPH_full%Pole)/),UNIT="meV")
     else
       call OUTPUT_driver("ph",TITLES=(/"E(DDFPT)"/),R_VALUES=(/real(QPH_DDFPT%Pole)/),UNIT="meV")
     endif
     call OUTPUT_driver("ph",TITLES=(/"E(QPH)"/),R_VALUES=(/real(QPH%Pole)/),UNIT="meV")
     if (On_Mass_Shell_approx) then
       call OUTPUT_driver("ph",TITLES=(/"E(OMS)"/),R_VALUES=(/real(QPH_OMS%Pole)/),UNIT="meV")
     endif
     !
     ! ...widths
     if (l_QP_eval_GF) then
       call OUTPUT_driver("ph",TITLES=(/"\G(SF,L)"/),R_VALUES=(/QPH_full%Half_Width_left/),UNIT="meV")
       call OUTPUT_driver("ph",TITLES=(/"\G(SF,R)"/),R_VALUES=(/QPH_full%Half_Width_right/),UNIT="meV")
     endif
     if (On_Mass_Shell_approx) then
       call OUTPUT_driver("ph",TITLES=(/"\G(OMS,L)"/),R_VALUES=(/QPH_OMS%Half_Width_left/),UNIT="meV")
       call OUTPUT_driver("ph",TITLES=(/"\G(OMS,R)"/),R_VALUES=(/QPH_OMS%Half_Width_right/),UNIT="meV")
     endif
     call OUTPUT_driver("ph",TITLES=(/"FWMH(OMS+BP)"/),R_VALUES=(/real(QPH_OMS_bare_Pole%GAMMA)*2./),UNIT="meV")
     call OUTPUT_driver("ph",TITLES=(/"\G(QPH,L)"/),R_VALUES=(/QPH%Half_Width_left/),UNIT="meV")
     call OUTPUT_driver("ph",TITLES=(/"\G(QPH,R)"/),R_VALUES=(/QPH%Half_Width_right/),UNIT="meV")
     !
     call OUTPUT_driver("ph",action="write")
     !
   enddo
 enddo
 !
 call OUTPUT_driver("ph",action="close")
 !
 ! Clean
 !
end subroutine
