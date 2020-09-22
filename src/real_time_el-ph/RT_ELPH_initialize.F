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
subroutine RT_ELPH_initialize(E,k,q)
 ! 
 use pars,           ONLY:SP,pi,cZERO
 use real_time,      ONLY:q_weight_RT,THETA_matrix,&
&                         THETA_matrix_index,&
&                         l_RT_CCA_Kernel,RT_bands
 use electrons,      ONLY:levels,spin_occ
 use drivers,        ONLY:l_elph_scatt
 use vec_operate,    ONLY:v_norm
 use D_lattice,      ONLY:Bose_Temp
 use R_lattice,      ONLY:RL_vol,bz_samp,nXkibz
 use IO_m,           ONLY:io_control,OP_RD_CL,VERIFY,NONE
 use ELPH,           ONLY:ph_freqs_sq,ph_modes,elph_branches
 use functions,      ONLY:bose_f
 use R_lattice,      ONLY:RIM_n_rand_pts
 !
#include<memory.h>
 !
 type(levels)             :: E
 type(bz_samp),intent(in) :: k,q
 !
 ! Work Space
 !
 integer             ::iq,ID,i_counter,il
 real(SP)            ::RADIUS_sphere,q_weight_tmp(q%nbz,2),ph_mean_speed(3),ph_E
 integer, external   ::io_RT_components
 !
 ! Restart
 !==========
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=VERIFY,ID=ID) 
 io_REF=io_RT_components('REF',ID)
 !
 ! Logicals
 !==========
 if (.not.l_elph_scatt) return
 !
 ! El-Ph databases I/O
 !=====================
 call ELPH_expanded_databases_map(E,k,q,RT_bands(2),i_err)
 !
 if (i_err<0) then
   l_elph_scatt   =.FALSE.
   l_RT_CCA_Kernel=.FALSE.
   return
 endif
 !
 ! Q weights 
 !===========
 ! q_weight_RT are written in section 2, thus here are not known
 ! since only section 1 of the REF DB is written
 !
 if (.not.allocated(q_weight_RT)) then
   !
   ! RIM support
   !=============
   !
   ! Here It would be possible to have one different weight for each elph_mode.
   ! However in practice I assume there are two possibilities:
   ! 1 - acustic phonons w_ph(i) ~ \alpha(i)*|q|
   ! 2 - optical phonons w_ph(i) ~ const(i)
   !
   YAMBO_ALLOC(q_weight_RT,(q%nbz,elph_branches(1):elph_branches(2),2))
   q0_mod=(3._SP*RL_vol/4._SP/4._SP/pi)**(1._SP/3._SP)
   i1=0
   ph_mean_speed=0._SP
   q_mod=0._SP
   q_weight_RT=0._SP
   !
   call k_ibz2bz(q,'c',.TRUE.)  
   !
   if (RIM_n_rand_pts>0) then
     !
     do iq=1,q%nbz
       !
       q_mod(iq)=v_norm(q%ptbz(iq,:))
       !
       if (q_mod(iq)>q0_mod.or.iq==1) cycle
       !
       i1=i1+1
       !
       do il=1,3
         ph_mean_speed(il)=ph_mean_speed(il)+sqrt(ph_freqs_sq(iq,il))/q_mod(iq)
       enddo
       !
     enddo
     !
     ph_mean_speed=ph_mean_speed/float(i1)
     !
   endif
   !
   call k_ibz2bz(q,'i',.TRUE.)  
   !
   RADIUS_sphere=(3._SP*RL_vol/q%nbz/4._SP/pi)**(1._SP/3._SP)
   !
   call rim_spherical(q%nbz,q%ptbz,q_weight_tmp(:,1),RADIUS_sphere,1,.FALSE.)
   call rim_spherical(q%nbz,q%ptbz,q_weight_tmp(:,2),RADIUS_sphere,2,.FALSE.)
   !
   do iq=1,q%nbz
     !
     do il=elph_branches(1),elph_branches(2)
       !
       ph_E=sqrt(abs(ph_freqs_sq(iq,il)))
       !
       if (ph_E>0.) then
         q_weight_RT(iq,il,1)=1._SP/ph_E/float(q%nbz)
         q_weight_RT(iq,il,2)=bose_f(ph_E)/ph_E/float(q%nbz)/spin_occ
       endif
       !
       if (RIM_n_rand_pts>0.and.q_mod(iq)<=q0_mod .and. il<=3) then
         q_weight_RT(iq,il,1)=q_weight_tmp(iq,1)/ph_mean_speed(il)
         q_weight_RT(iq,il,2)=Bose_Temp*q_weight_tmp(iq,2)/ph_mean_speed(il)**2/spin_occ
       endif
       !
     enddo
     !
   enddo
   !
 endif
 !
 ! THETA matrix indexes
 !======================
 if (l_RT_CCA_Kernel) return
 !
 YAMBO_ALLOC(THETA_matrix,(2*q%nbz*ph_modes,RT_bands(2)-RT_bands(1)+1,RT_bands(2)-RT_bands(1)+1,nXkibz))
 THETA_matrix=cZERO
 YAMBO_ALLOC(THETA_matrix_index,(q%nbz,ph_modes,2))
 !
 i_counter=0
 !
 do iq=1,q%nbz
   do il=1,ph_modes
     do i_s=1,2
       !
       i_counter=i_counter+1
       !
       THETA_matrix_index(iq,il,i_s)=i_counter
       !
     enddo
   enddo
 enddo
 !
end subroutine RT_ELPH_initialize
