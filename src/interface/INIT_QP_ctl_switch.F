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
subroutine INIT_QP_ctl_switch(what)
 !
 use electrons,  ONLY:n_sp_pol
 !
 implicit none
 !
 character(1)::what
 !
 ! Work Space
 !
 call initactivate(1,what//'fnQPdb '//what//'fnQP_N '//what//'fnFdb')
 if (n_sp_pol==1) then
   call initactivate(1,what//'fnQP_E '//what//'fnQP_Z')   
   call initactivate(1,what//'fnQP_Wv_E '//what//'fnQP_Wv '//what//'fnQP_Wv_dos ')
   call initactivate(1,what//'fnQP_Wc_E '//what//'fnQP_Wc '//what//'fnQP_Wc_dos ')
 else
   call initactivate(1,what//'fnQP_up_E '//what//'fnQP_up_Z')   
   call initactivate(1,what//'fnQP_up_Wv_E '//what//'fnQP_up_Wv '//what//'fnQP_up_Wv_dos ')
   call initactivate(1,what//'fnQP_up_Wc_E '//what//'fnQP_up_Wc '//what//'fnQP_up_Wc_dos ')
   call initactivate(1,what//'fnQP_dn_E '//what//'fnQP_dn_Z')   
   call initactivate(1,what//'fnQP_dn_Wv_E '//what//'fnQP_dn_Wv '//what//'fnQP_dn_Wv_dos ')
   call initactivate(1,what//'fnQP_dn_Wc_E '//what//'fnQP_dn_Wc '//what//'fnQP_dn_Wc_dos ')
 endif
 !
end subroutine
