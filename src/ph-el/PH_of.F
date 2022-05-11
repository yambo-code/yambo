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
subroutine PH_of(q)
 !
 use pars,           ONLY:SP,schlen
 use drivers,        ONLY:l_life
 use OUTPUT_simple,  ONLY:OUTPUT_add_column
 use PHEL,           ONLY:PH_Q,PH_SF_W,PHEL_qp,PH_energy,QPH
 use ELPH,           ONLY:elph_branches,PHEL_desc,ELPH_desc
 use R_lattice,      ONLY:bz_samp
 use interfaces,     ONLY:DESC_write
 use QP_m,           ONLY:l_extended_output,QP_Sc_steps,l_QP_eval_GF,l_QP_eval_Z
 use stderr,         ONLY:intc
 !
 implicit none
 !
 type(bz_samp),     intent(in) :: q
 !
 ! Work Space
 !
 integer          :: iq,il,iw,ip,s(2)
 real(SP)         :: ph_freq
 character(schlen):: Gfile
 ! SF's
 complex(SP)      :: D(QP_Sc_steps)
 !
 call OUTPUT_add_column("ph",action="reset")
 call OUTPUT_add_column("ph",action="open")
 call DESC_write('o ph','#',PHEL_desc)
 call DESC_write('o ph','#',ELPH_desc)
 !
 do iq=PH_Q(1),PH_Q(2)
   do il=elph_branches(1),elph_branches(2)
     !
     ! PH freq
     ph_freq=PH_energy(q,il,iq)
     !
     ! PI -> quasiphonon props
     call PHEL_from_GF_to_quasiphonons(il,iq,ph_freq,D)
     !
     ! GF 
     if (l_QP_eval_GF) then
       Gfile="G_ph_Q"//trim(intc(iq))//"_Branch_"//trim(intc(il))
       call OUTPUT_add_column(trim(Gfile),action="reset")
       call OUTPUT_add_column(trim(Gfile),action="open")
       call DESC_write('o '//trim(Gfile),'#',PHEL_desc)
       call DESC_write('o '//trim(Gfile),'#',ELPH_desc)
       do iw=1,QP_Sc_steps
         call OUTPUT_add_column(trim(Gfile),TITLES=(/"Energy"/),R_VALUES=(/real(PH_SF_W(il,iq)%p(iw))/),UNIT="meV")
         call OUTPUT_add_column(trim(Gfile),TITLES=(/"-Im[D]","Re[D] "/),R_VALUES=(/-aimag(D(iw)),real(D(iw))/))
         call OUTPUT_add_column(trim(Gfile),TITLES=(/"-Im[\Pi]    ","Re[\Pi]-Ref "/),&
&                               R_VALUES=(/-aimag(PHEL_qp%PI(il,iq,iw)),real(PHEL_qp%PI(il,iq,iw)-QPH%PI_ref)/),UNIT="meV")
         call OUTPUT_add_column(trim(Gfile),action="write")
       enddo
       !
       call OUTPUT_add_column(trim(Gfile),action="close")
       !
     endif
     !
     ! o.ph
     call OUTPUT_add_column("ph",TITLES=(/"Q-point","Branch "/),I_VALUES=(/iq,il/))
     call OUTPUT_add_column("ph",TITLES=(/"W^0_ph"/),R_VALUES=(/ph_freq/),UNIT="meV")
     if (l_life) then
       call OUTPUT_add_column("ph",TITLES=(/"FWMH(OMS)"/),R_VALUES=(/aimag(QPH%PI_oms)/),UNIT="meV")
     else
       call OUTPUT_add_column("ph",TITLES=(/"W-Wo(OMS)"/),R_VALUES=(/real(QPH%PI_oms)-QPH%PI_ref/),UNIT="meV")
       call OUTPUT_add_column("ph",TITLES=(/"Im[\Pi](OMS)"/),R_VALUES=(/aimag(QPH%PI_oms)/),UNIT="meV")
     endif
     !
     if (l_extended_output.and.l_life) then
       call OUTPUT_add_column("ph",TITLES=(/"FWMH(interb)"/),R_VALUES=(/PHEL_qp%interband(il,iq)/),UNIT="meV")
       call OUTPUT_add_column("ph",TITLES=(/"FWMH(intrab)"/),R_VALUES=(/PHEL_qp%intraband(il,iq)/),UNIT="meV")
       call OUTPUT_add_column("ph",TITLES=(/"FWMH(Allen)"/),R_VALUES=(/PHEL_qp%allen(il,iq)/),UNIT="meV")
     endif
     !
     if (l_QP_eval_Z) then
       call OUTPUT_add_column("ph",TITLES=(/"Re[beta]"/),R_VALUES=(/real(QPH%beta)/))
       call OUTPUT_add_column("ph",TITLES=(/"Im[beta]"/),R_VALUES=(/aimag(QPH%beta)/))
       call OUTPUT_add_column("ph",TITLES=(/"W-Wo"/),R_VALUES=(/real(QPH%Pole)-ph_freq/),UNIT="meV")
       call OUTPUT_add_column("ph",TITLES=(/"\Im[W]"/),R_VALUES=(/aimag(QPH%Pole)/),UNIT="meV")
     endif
     if (l_QP_eval_GF) then
       call OUTPUT_add_column("ph",TITLES=(/"W-Wo"/),R_VALUES=(/real(QPH%Pole)-ph_freq/),UNIT="meV")
       call OUTPUT_add_column("ph",TITLES=(/"Width(left)"/),R_VALUES=(/QPH%Half_Width_left/),UNIT="meV")
       call OUTPUT_add_column("ph",TITLES=(/"Width(right)"/),R_VALUES=(/QPH%Half_Width_right/),UNIT="meV")
     endif
     !
     call OUTPUT_add_column("ph",action="write")
     !
   enddo
 enddo
 !
 call OUTPUT_add_column("ph",action="close")
 !
end subroutine
