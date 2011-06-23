!
! Copyright (C) 2000-2010 A. Marini, C. Attaccalite and the YAMBO team 
!              http://www.yambo-code.org
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
subroutine RT_Observables(en,k,on_the_fly_operators)
 !
 use pars,           ONLY:SP,pi,cI
 use units,          ONLY:SPEED_OF_LIGHT,HARTREE
 use drivers,        ONLY:l_sc_hf,l_sc_contains_lda,l_sc_cohsex
 use R_lattice,      ONLY:bz_samp,nkibz
 use D_lattice,      ONLY:DL_vol
 use SC,             ONLY:V_hartree_sc,Ho,SC_bands
 use X_m,            ONLY:P_square
 use real_time,      ONLY:XC0_and_HARTREE0,G_lesser,RT_average_operator,XCfrozen,l_NE_with_fields, &
&                         l_NE_simulate_IP ,l_Dynamics_is_over,RT_output
 use fields,         ONLY:A_vecpot
 use electrons,      ONLY:levels
 use QP_m,           ONLY:QP_Vnl_xc,QP_Sc
 use xc_functionals, ONLY:E_xc
 use global_XC,      ONLY:WF_kind,WF_xc_functional
 use vec_operate,    ONLY:sort
 !
 implicit none 
 !
 type(levels),  intent(in) :: en
 type(bz_samp), intent(in) :: k
 complex(SP),   intent(out):: on_the_fly_operators(8)
 !
 ! WorkSpace
 !
 integer     :: i1,indexes(SC_bands(2)*nkibz)
 real(SP)    :: Energies_and_occupations(SC_bands(2)*nkibz,2)
 !
 ! Shadows
 !
 integer     :: ib,ik
 complex(SP) :: E_kin,E_ion,E_h,Trace_Gf,Exc,Ex,E_mg,delta_G
 !
 !      
 E_kin    =(0._SP,0._SP)
 E_ion    =(0._SP,0._SP)
 E_h      =(0._SP,0._SP)
 Trace_Gf =(0._SP,0._SP)
 Exc      =(0._SP,0._SP)
 Ex       =(0._SP,0._SP)
 E_mg     =(0._SP,0._SP)
 delta_G  =(0._SP,0._SP)
 !
 ! Non-interaction kinetc energy
 !
 ! E_Kin = - i Tr { P^2 G^<}/2  
 !
 call RT_average_operator(VALUE=E_kin,O_MATRIX=P_square,G=G_lesser,k=k,TRANS=.TRUE.)
 !
 E_kin=E_kin/2.
 !  
 ! E_ion = - i Tr {Ho G^<} - Ekin
 !
 call RT_average_operator(VALUE=E_ion,O_MATRIX=Ho,G=G_lesser,k=k)
 !
 E_ion = E_ion - E_kin
 !
 if(.not.l_NE_simulate_IP) then
   !
   ! Hartree potential Energy
   !
   call RT_average_operator(VALUE=E_h,O_REAL=V_hartree_sc,G=G_lesser,k=k)
   !
   E_h=E_h/2._SP
   !
 endif
 !
 if(l_sc_hf.or.l_sc_cohsex.and..not.l_NE_simulate_IP) then
   !
   call RT_average_operator(VALUE=Ex,O_QP=QP_Vnl_xc,G=G_lesser,k=k)
   !
   Ex=Ex/2._SP
   !
 endif
 !
 if(XCfrozen) then
   !
   ! Notice that XC0_and_HARTREE0 contains the Hartree part only in case
   ! of indipendent particles
   !
   call RT_average_operator(VALUE=Exc,O_MATRIX=XC0_and_HARTREE0,G=G_lesser,k=k)
   !
 else
   !
   if(l_sc_contains_lda) then 
     !        
     ! E_xc Energy 
     !
     call IO_and_Messaging_switch("-report -log -io_in -io_out")
     call XC_potential_driver(en,k,WF_KIND,WF_xc_functional,0)
     call IO_and_Messaging_switch("+report +log +io_in +io_out")
     !
     call RT_average_operator(VALUE=Exc,O_REAL=E_xc,G=G_lesser,k=k)
     !
   elseif(l_sc_cohsex) then
     !      
     call RT_average_operator(VALUE=Exc,O_QP=QP_Sc,G=G_lesser,k=k)
     !
   endif
   !
 endif
 !
 ! Energy of the Electro-Magnetic Field
 !
 if(l_NE_with_fields) then
   !                    
   E_mg=DL_vol/(8._SP*pi*SPEED_OF_LIGHT**2)*abs(sum(A_vecpot%vecpot))
   !
 endif
 !
 ! Total number of electrons
 !
 call RT_average_operator(VALUE=Trace_Gf,G=G_lesser,k=k)
 !
 ! delta_G=\sum_c (-i G_lesser(c,c,k) + i G_lesser(v,v,k)) w_k
 ! where conduction and valence are defined with reference to the unperturbed
 ! system.
 !
 ! Note that this definition is faulty in the case of metals where a different
 ! quantity must be defined.
 !
 do ik=1,en%nk
   do ib=1,SC_bands(2)
     if (ib<=en%nbf) delta_G=delta_G-cI*G_lesser(ib,ib,ik)*k%weights(ik)
     if (ib> en%nbf) delta_G=delta_G+cI*G_lesser(ib,ib,ik)*k%weights(ik)
   enddo
 enddo
 !
 if (l_Dynamics_is_over.and.l_NE_with_fields) then
   !
   i1=1
   do ik=1,en%nk
     do ib=1,SC_bands(2)
       Energies_and_occupations(i1,1)=en%E(ib,ik,1)
       Energies_and_occupations(i1,2)=real(-cI*G_lesser(ib,ib,ik))
       i1=i1+1
     enddo
   enddo
   !
   call sort(Energies_and_occupations(:,1),indx=indexes)
   !
   do i1=1,SC_bands(2)*en%nk
     call RT_output('occ',(/Energies_and_occupations(indexes(i1),2)/),E=Energies_and_occupations(i1,1)*HARTREE)
   enddo
   !
 endif
 !
 ! Transfer the shadow variables
 !
 on_the_fly_operators=(/E_kin,E_ion,E_h,Ex,Exc,E_mg,Trace_Gf,delta_G/)
 !
 !
end subroutine RT_Observables

