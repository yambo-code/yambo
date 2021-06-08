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
subroutine PH_of(q)
 !
 use pars,           ONLY:SP,schlen
 use OUTPUT_simple,  ONLY:OUTPUT_add_column
 use PHEL,           ONLY:PH_Q,PH_SF_W,PHEL_qp,PH_energy
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
 real(SP)         :: ph_freq,delta_ph_freq
 character(schlen):: Gfile
 ! SF's
 real(SP)         :: PI_ref
 complex(SP)      :: D(QP_Sc_steps),QPH_Z,PI_OMS,QPH_freq
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
     call PHEL_from_GF_to_quasiphonons(il,iq,ph_freq,PI_ref,D,QPH_Z,PI_OMS,QPH_freq)
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
         call OUTPUT_add_column(trim(Gfile),TITLES=(/"-Im[\Pi]","Re[\Pi] "/),&
&                               R_VALUES=(/-aimag(PHEL_qp%PI(il,iq,iw)),real(PHEL_qp%PI(il,iq,iw)-PI_ref)/),UNIT="meV")
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
     call OUTPUT_add_column("ph",TITLES=(/"Width(OMS)"/),R_VALUES=(/aimag(PI_OMS)*(-.5_SP)/),UNIT="meV")
     !
     if (l_extended_output) then
       call OUTPUT_add_column("ph",TITLES=(/"Width(interb)"/),R_VALUES=(/-PHEL_qp%interband(il,iq)/),UNIT="meV")
       call OUTPUT_add_column("ph",TITLES=(/"Width(intrab)"/),R_VALUES=(/-PHEL_qp%intraband(il,iq)/),UNIT="meV")
       call OUTPUT_add_column("ph",TITLES=(/"Width(approx)"/),R_VALUES=(/-PHEL_qp%approx(il,iq)/),UNIT="meV")
     endif
     !
     if (l_QP_eval_Z.or.l_QP_eval_GF) then
       call OUTPUT_add_column("ph",TITLES=(/"Re[Z]"/),R_VALUES=(/real(QPH_Z)/))
       call OUTPUT_add_column("ph",TITLES=(/"Im[Z]"/),R_VALUES=(/aimag(QPH_Z)/))
       delta_ph_freq=0._SP
       if (real(QPH_freq)>0._SP) delta_ph_freq=real(QPH_freq)-ph_freq
       call OUTPUT_add_column("ph",TITLES=(/"W_ph-W^0_ph"/),R_VALUES=(/delta_ph_freq/),UNIT="meV")
       call OUTPUT_add_column("ph",TITLES=(/"Width"/),R_VALUES=(/-aimag(QPH_freq)/),UNIT="meV")
     endif
     !
     call OUTPUT_add_column("ph",action="write")
     !
   enddo
 enddo
 call OUTPUT_add_column("ph",action="close")
 !
end subroutine
