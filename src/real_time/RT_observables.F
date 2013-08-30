!
! Copyright (C) 2000-2013 A. Marini, C. Attaccalite and the YAMBO team 
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
subroutine RT_Observables(en,k,G_lesser,on_the_fly_operators)
 !
 use pars,           ONLY:SP,cI,zero_dfl
 use units,          ONLY:HA2EV
 use drivers,        ONLY:l_sc_hf,l_sc_contains_lda,l_sc_cohsex,l_carrier_dynamics
 use R_lattice,      ONLY:bz_samp
 use SC,             ONLY:V_hartree_sc,Ho,SC_bands,it_now
 use X_m,            ONLY:P_square
 use real_time,      ONLY:XC0_and_HARTREE0,RT_average_operator,XCfrozen,&
&                         l_NE_simulate_IP,Fluence
 use electrons,      ONLY:levels
 use QP_m,           ONLY:QP_Vnl_xc,QP_Sc,QP_nk
 use xc_functionals, ONLY:E_xc
 use global_XC,      ONLY:WF_kind,WF_xc_functional
 use IO_m,           ONLY:IO_and_Messaging_switch
 use parallel_m,     ONLY:PAR_Xk_nibz
 !
 implicit none 
 !
 type(levels),  intent(in) :: en
 type(bz_samp), intent(in) :: k
 complex(SP),   intent(in) :: G_lesser(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),QP_nk)
 complex(SP),   intent(out):: on_the_fly_operators(8)
 !
 ! WorkSpace
 !
 real(SP)    :: dG_sign
 !
 ! Shadows
 !
 integer     :: ib,ik
 complex(SP) :: E_kin,E_ion,E_h,Trace_Gf,Exc,Ex,E_mg,delta_G
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
 call RT_average_operator(VALUE=E_kin,O_MATRIX=P_square,G=G_lesser(:,:,:),en=en,k=k,TRANS=.TRUE.,nK_mem=PAR_Xk_nibz)
 !
 E_kin=E_kin/2.
 !  
 ! E_ion = - i Tr {Ho G^<} - Ekin
 !
 call RT_average_operator(VALUE=E_ion,O_MATRIX=Ho,G=G_lesser(:,:,:),en=en,k=k,nK_mem=QP_nk)
 !
 E_ion = E_ion - E_kin
 !
 if(.not.l_NE_simulate_IP) then
   !
   ! Hartree potential Energy
   !
   call RT_average_operator(VALUE=E_h,O_REAL=V_hartree_sc,G=G_lesser(:,:,:),en=en,k=k,nK_mem=QP_nk)
   !
   E_h=E_h/2.
   !
 endif
 !
 if(l_sc_hf.or.(l_sc_cohsex.and..not.l_carrier_dynamics).and..not.l_NE_simulate_IP) then
   !
   call RT_average_operator(VALUE=Ex,O_QP=QP_Vnl_xc,G=G_lesser(:,:,:),en=en,k=k,nK_mem=QP_nk)
   !
 endif
 !
 if (XCfrozen) then
   !
   ! Notice that XC0_and_HARTREE0 contains the Hartree part only in case
   ! of indipendent particles
   !
   call RT_average_operator(VALUE=Exc,O_MATRIX=XC0_and_HARTREE0,G=G_lesser(:,:,:),en=en,k=k,nK_mem=QP_nk)
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
     call RT_average_operator(VALUE=Exc,O_REAL=E_xc,G=G_lesser(:,:,:),en=en,k=k,nK_mem=QP_nk)
     !
   else if(l_sc_cohsex.and..not.l_carrier_dynamics) then
     !      
     call RT_average_operator(VALUE=Exc,O_QP=QP_Sc,G=G_lesser(:,:,:),en=en,k=k,nK_mem=QP_nk)
     !
   endif
   !
 endif
 !
 ! Total number of electrons
 !
 call RT_average_operator(VALUE=Trace_Gf,G=G_lesser(:,:,:),en=en,k=k,nK_mem=QP_nk)
 !
 ! Andrea [21/12/2011]
 ! 
 ! In a semiconductor/isolant delta_G is defined as:
 !
 !  delta_G=\sum_{n=(c,v),k} (-i G_lesser(v,v,k) + i G_lesser(c,c,k)) w_k=
 !          \sum_{n=(c,v),k} (f_vk-f_ck) w_k
 !
 ! where conduction and valence are defined with reference to the unperturbed
 ! system.
 !
 ! This definition is easily extended to metals 
 !
 do ik=1,en%nk
   do ib=1,SC_bands(2)
     !
     dG_sign=1.
     if (en%f(ib,ik,1)<zero_dfl) dG_sign=-1.
     !
     if (ib<SC_bands(1)) then
       delta_G=delta_G+dG_sign*en%f(ib,ik,1)*k%weights(ik)
     else
       delta_G=delta_G-dG_sign*cI*G_lesser(ib,ib,ik)*k%weights(ik)
     endif
     !
   enddo
 enddo
 !
 ! Transfer the shadow variables
 !
 on_the_fly_operators(1:5)=(/E_kin,E_ion,E_h,Ex,Exc/)*HA2EV
 on_the_fly_operators(6:8)=(/cmplx(Fluence(it_now),0._SP,SP),Trace_Gf,delta_G/)
 !
end subroutine RT_Observables

