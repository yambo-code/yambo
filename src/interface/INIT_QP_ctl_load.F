!
!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine INIT_QP_ctl_load(defs,ID)
 !
 use it_tools,      ONLY:it
 use it_m,          ONLY:initdefs,V_qp,E_unit
 use QP_CTL_m,      ONLY:QP_ctl_DB_user,QP_ctl_user
 !
 implicit none
 integer, intent(in) :: ID
 type(initdefs)      :: defs
 ! 
 ! Work Space
 ! 
 integer           ::i_f,verbosity_here
 character(1)      ::prefix(3)
 character(3)      ::spin_ch(3)
 character(7)      ::description_ch(3)
 character(4)      ::spin_description_ch(3)
 !
 spin_ch=(/'   ','_up','_dn'/)
 spin_description_ch= ' '
 spin_description_ch=(/'    ','UP  ','DOWN'/)
 prefix=(/'X','K','G'/)
 description_ch(1)='Xd'
 description_ch(2)='BSK BSS'
 description_ch(3)='G'
 !
 verbosity_here=V_qp
 !
 do i_f=1,3
   !
   call it(defs,&
&          prefix(ID)//'fnQP'//trim(spin_ch(i_f))//'_E',&
&          '[EXTQP '//trim(description_ch(ID))//'] E parameters '//&
&          trim(spin_description_ch(i_f))//' (c/v) eV|adim|adim',&
&          QP_ctl_user(ID,i_f)%E,verb_level=V_qp)
   !
   call it(defs,&
&          prefix(ID)//'fnQP'//trim(spin_ch(i_f))//'_Wv_E',&
&          '[EXTQP '//trim(description_ch(ID))//'] W Energy reference '//&
&          trim(spin_description_ch(i_f))//' (valence)',&
&          QP_ctl_user(ID,i_f)%Wv_E_ref,E_unit,verb_level=verbosity_here)
   !
   call it(defs,&
&          prefix(ID)//'fnQP'//trim(spin_ch(i_f))//'_Wv',&
&          '[EXTQP '//trim(description_ch(ID))//'] W parameters '//&
&          trim(spin_description_ch(i_f))//' (valence) eV| 1|eV^-1',&
&          QP_ctl_user(ID,i_f)%Wv,verb_level=verbosity_here)
   !
   call it(defs,&
&          prefix(ID)//'fnQP'//trim(spin_ch(i_f))//'_Wc_E',&
&          '[EXTQP '//trim(description_ch(ID))//'] W Energy reference '//&
&          trim(spin_description_ch(i_f))//' (conduction)',&
&          QP_ctl_user(ID,i_f)%Wc_E_ref,E_unit,verb_level=verbosity_here)
   !
   call it(defs,&
&          prefix(ID)//'fnQP'//trim(spin_ch(i_f))//'_Wc',&
&          '[EXTQP '//trim(description_ch(ID))//'] W parameters '//&
&          trim(spin_description_ch(i_f))//' (conduction) eV| 1 |eV^-1',&
&          QP_ctl_user(ID,i_f)%Wc,verb_level=verbosity_here)
   !
   call it(defs,&
&          prefix(ID)//'fnQP'//trim(spin_ch(i_f))//'_Z',&
&          '[EXTQP '//trim(description_ch(ID))//'] Z factor '//&
&          trim(spin_description_ch(i_f))//' (c/v)',&
&          QP_ctl_user(ID,i_f)%Z,verb_level=V_qp)
   !
   call it(defs,&
&          prefix(ID)//'fnQP'//trim(spin_ch(i_f))//'_Wc_dos',&
&          '[EXTQP '//trim(description_ch(ID))//'] W dos pre-factor '//&
&          trim(spin_description_ch(i_f))//' (conduction)',&
&          QP_ctl_user(ID,i_f)%Wc_dos,E_unit,verb_level=V_qp)
   !
   call it(defs,&
&          prefix(ID)//'fnQP'//trim(spin_ch(i_f))//'_Wv_dos',&
&          '[EXTQP '//trim(description_ch(ID))//'] W dos pre-factor '//&
&          trim(spin_description_ch(i_f))//' (valence)',&
&          QP_ctl_user(ID,i_f)%Wv_dos,E_unit,verb_level=V_qp)
   !
 enddo
 !
 call it(defs,prefix(ID)//'fnQPdb',&
&        '[EXTQP '//trim(description_ch(ID))//'] Database action',&
&        QP_ctl_DB_user(ID)%action,verb_level=V_qp)
 !
 call it(defs,prefix(ID)//'fnQP_DbGd_INTERP_mode',&
&        '[EXTQP '//trim(description_ch(ID))//'] Interpolation DbGd mode',&
&        QP_ctl_DB_user(ID)%INTERP_DbGd_mode,verb_level=V_qp)
 !
 call it(defs,prefix(ID)//'fnQP_INTERP_NN',&
&        '[EXTQP '//trim(description_ch(ID))//'] Interpolation neighbours (NN mode)',&
&        QP_ctl_DB_user(ID)%INTERP_N_neigh,verb_level=V_qp)
 !
 call it(defs,prefix(ID)//'fnQP_INTERP_shells',&
&        '[EXTQP '//trim(description_ch(ID))//'] Interpolation shells (BOLTZ mode)',&
&        QP_ctl_DB_user(ID)%INTERP_shell_factor,verb_level=V_qp)
 !
end subroutine
