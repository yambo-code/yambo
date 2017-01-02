!
!        Copyright (C) 2000-2017 the YAMBO team
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
subroutine INIT_QP_ctl_load(defs,i_type)
 !
 ! Note that this routine is called in 'load' mode BEFORE
 ! the DB1 is read, so before n_sp_pol is set to the correct value.
 ! 
 ! Thus we must call it on the the no spin, spin + and spin - cases 
 ! because the code does not know if there is
 ! spin unless the DB1 is read. 
 !
 use electrons,     ONLY:n_sp_pol
 use it_m,          ONLY:it,initmode,initdefs,V_qp,E_unit
 use QP_m,          ONLY:QP_ctl_E,QP_ctl_db,QP_ctl_interp_neigh,QP_ctl_Wv,&
&                        QP_ctl_Wc,QP_ctl_Z,QP_ctl_Wc_E_ref,QP_ctl_Wv_E_ref
 !
 implicit none
 integer, intent(in) :: i_type
 type(initdefs)      :: defs
 ! 
 ! Work Space
 ! 
 integer           ::i_spin,i1,i1_range(2),verbosity_here
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
 if (initmode==0) i1_range=(/1,3/)
 if (initmode/=0.and.n_sp_pol==1) i1_range=1
 if (initmode/=0.and.n_sp_pol==2) i1_range=(/2,3/)
 !
 do i1=i1_range(1),i1_range(2)
   ! 
   if (i1==1) i_spin=1
   if (i1> 1) i_spin=i1-1
   !
   call it(defs,&
&          prefix(i_type)//'fnQP'//trim(spin_ch(i1))//'_E',&
&          '[EXTQP '//trim(description_ch(i_type))//'] E parameters '//&
&          trim(spin_description_ch(i1))//' (c/v) eV|adim|adim',&
&          QP_ctl_E(i_type,:,i_spin),verb_level=V_qp)
   !
   call it(defs,&
&          prefix(i_type)//'fnQP'//trim(spin_ch(i1))//'_Wv_E',&
&          '[EXTQP '//trim(description_ch(i_type))//'] W Energy reference '//&
&          trim(spin_description_ch(i1))//' (valence)',&
&          QP_ctl_Wv_E_ref(i_type,i_spin),E_unit,verb_level=verbosity_here)
   !
   call it(defs,&
&          prefix(i_type)//'fnQP'//trim(spin_ch(i1))//'_Wv',&
&          '[EXTQP '//trim(description_ch(i_type))//'] W parameters '//&
&          trim(spin_description_ch(i1))//' (valence) eV| eV|eV^-1',&
&          QP_ctl_Wv(i_type,:,i_spin),verb_level=verbosity_here)
   !
   call it(defs,&
&          prefix(i_type)//'fnQP'//trim(spin_ch(i1))//'_Wc_E',&
&          '[EXTQP '//trim(description_ch(i_type))//'] W Energy reference '//&
&          trim(spin_description_ch(i1))//' (conduction)',&
&          QP_ctl_Wc_E_ref(i_type,i_spin),E_unit,verb_level=verbosity_here)
   !
   call it(defs,&
&          prefix(i_type)//'fnQP'//trim(spin_ch(i1))//'_Wc',&
&          '[EXTQP '//trim(description_ch(i_type))//'] W parameters '//&
&          trim(spin_description_ch(i1))//' (conduction) eV| eV|eV^-1',&
&          QP_ctl_Wc(i_type,:,i_spin),verb_level=verbosity_here)
   !
   call it(defs,&
&          prefix(i_type)//'fnQP'//trim(spin_ch(i1))//'_Z',&
&          '[EXTQP '//trim(description_ch(i_type))//'] Z factor '//&
&          trim(spin_description_ch(i1))//' (c/v)',&
&          QP_ctl_Z(i_type,i_spin),verb_level=V_qp)
   !
 enddo
 !
 call it(defs,prefix(i_type)//'fnQPdb',&
&        '[EXTQP '//trim(description_ch(i_type))//'] Database',&
&        QP_ctl_db(i_type),verb_level=V_qp)
 !
 call it(defs,prefix(i_type)//'fnQP_N',&
&        '[EXTQP '//trim(description_ch(i_type))//'] Interpolation neighbours',&
&        QP_ctl_interp_neigh(i_type),verb_level=V_qp)
 !
end subroutine
