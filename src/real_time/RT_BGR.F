!
!        Copyright (C) 2000-2014 the YAMBO team
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
subroutine RT_BGR(G,E,k)
 !
 ! This routine calculates the BGR
 !
 use pars,           ONLY:SP
 use drivers,        ONLY:l_sc_hf,l_sc_cohsex
 use SC,             ONLY:SC_bands,RT_nk,RT_ibz_coo,V_hartree_sc,V_xc_sc,rho_n
 use R_lattice,      ONLY:nkibz,bz_samp
#if defined _TIMING
 use timing_m,       ONLY:timing
#endif
 use electrons,      ONLY:levels
 use parallel_m,     ONLY:master_cpu
 use QP_m,           ONLY:QP_Vnl_xc
 use real_time,      ONLY:G_lesser_reference
 use xc_functionals, ONLY:V_xc,XC_potential_driver
 use global_XC,      ONLY:WF_kind,WF_xc_functional 
 use IO_m,           ONLY:IO_and_Messaging_switch
 !
 implicit none
 !
 type(levels),  intent(in) :: E
 type(bz_samp), intent(in) :: k
 complex(SP),   intent(in) :: G(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),RT_nk)
 !
 ! Work Space
 !
 complex(SP)  :: G_hf(SC_bands(2),SC_bands(2),nkibz),G_cohsex(SC_bands(2),SC_bands(2),nkibz)
 integer      :: ib,ik,ik_ref
 !
#if defined _TIMING
 call timing('RT BGR',OPR='start')
#endif
 !
 ! From RT_Collision I get rho_n to calculate the new Hartree and XC potential
 !
 ! Hartree
 !
 call V_Hartree(rho_n,V_hartree_sc)
 !
 ! V_xc
 !
 call IO_and_Messaging_switch("-report -log -io_in -io_out")
 call XC_potential_driver(E,k,WF_KIND,WF_xc_functional,1)
 call IO_and_Messaging_switch("+report +log +io_in +io_out",CONDITION=master_cpu)
 V_xc_sc=V_xc
 !
 if (l_sc_hf.or.l_sc_cohsex) then
   !
   do ik=1,nkibz
     !
     ik_ref=RT_ibz_coo(ik,1)
     !
     do ib=1,SC_bands(2)
       if (ib<SC_bands(1)) then
         G_hf(ib,ib,ik)    =(0._SP,1._SP)*E%f(ib,ik,1)
         G_cohsex(ib,ib,ik)=(0._SP,1._SP)*(E%f(ib,ik,1)-1.)
       else
         G_hf(ib,ib,ik)    =G(ib,ib,ik_ref)+G_lesser_reference(ib,ib,ik_ref)
         G_cohsex(ib,ib,ik)=G(ib,ib,ik_ref)+G_lesser_reference(ib,ib,ik_ref)- (0._SP,1._SP)
       endif
     enddo
   enddo
   !
   call COLLISIONS_compose(G_hf,G_cohsex)
   !
 endif
 !
#if defined _TIMING
 call timing('RT BGR',OPR='stop')
#endif
 !
end subroutine RT_BGR
