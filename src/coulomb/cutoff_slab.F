!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM,DV
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
 subroutine cutoff_slab(q,is_cut)
 !
 use com,           ONLY:msg,of_open_close
 use pars,          ONLY:SP,pi,schlen
 use wave_func,     ONLY:wf_ng
 use D_lattice,     ONLY:alat
 use R_lattice,     ONLY:bz_samp,g_vec,bare_qpg,q0_def_norm,k_grid_uc_vol,&
&                        d3q_factor,RIM_qpg,q_norm
 use com,           ONLY:error
 use parallel_m,    ONLY:PP_indexes,PP_indexes_reset
 use interfaces,    ONLY:PARALLEL_index
 !
 implicit none
 !
 ! Work Space
 ! 
 type(bz_samp) :: q
 logical       :: is_cut(3)
 integer       :: ipar1,ipar2,iort
 integer       :: iq,ig,n_idx,n_max
 real(SP)      :: slab_vz,slab_vplane,lcut,pre_factor,R_circle,Sup,factorial
 type(PP_indexes) ::px
 !
 call PP_indexes_reset(px)


 if(is_cut(1).and.is_cut(2)) call error('Check slab normal direction')
 if(is_cut(2).and.is_cut(3)) call error('Check slab normal direction')
 if(is_cut(1).and.is_cut(3)) call error('Check slan normal direction')
 !
 if(is_cut(1)) then
   iort=1
   ipar1=2
   ipar2=3
 elseif(is_cut(2)) then
   iort=2
   ipar1=1
   ipar2=3
 elseif(is_cut(3)) then
   iort=3
   ipar1=2
   ipar2=1
 endif

 lcut=alat(iort)/2.

 call PARALLEL_index(px,(/q%nibz,wf_ng/))
 !
 do ig=1,wf_ng
   do iq=1,q%nibz
! kz
     slab_vz=(q%pt(iq,iort)+g_vec(ig,iort) )*2.*pi/alat(iort)
! kxy
     slab_vplane=sqrt(((q%pt(iq,ipar1)+g_vec(ig,ipar1))*2.*pi/alat(ipar1))**2+&
&                 ((q%pt(iq,ipar2)+g_vec(ig,ipar2))*2.*pi/alat(ipar2))**2)               
     !Regularization of the potential
     if(slab_vplane < q0_def_norm) slab_vplane = q0_def_norm
! k=0 case
     if (iq==1.and.ig==1) cycle
! all other
     pre_factor=sqrt(1.-exp(-slab_vplane*lcut)*cos(slab_vz*lcut))
     !if (abs(pre_factor)<1.E-5_SP) pre_factor=1.E-5_SP
     bare_qpg(iq,ig)=bare_qpg(iq,ig)/pre_factor
   enddo
 enddo
 !
 !AG: The previous regularization was not correct.
 if (.not.allocated(RIM_qpg)) then
   !Here, my proposal, in which the potential is regularized over a circle centered in Gamma
   n_max = 15
   factorial = 1._SP
   pre_factor = 0._SP
   Sup = d3q_factor/((2._SP*pi)/alat(iort))
   R_circle = sqrt(Sup/pi)
   do n_idx=1,n_max
     factorial = factorial*n_idx
     pre_factor = pre_factor+(-1._SP)**(n_idx+1)/n_idx/factorial*(R_circle*lcut)**n_idx
   enddo
   pre_factor = 2._SP*pi/Sup*pre_factor
   bare_qpg(1,1) = 1._SP/sqrt(pre_factor)
   !
 else
   !
   slab_vplane=q_norm(1)
   pre_factor=sqrt(1._SP-exp(-slab_vplane*lcut))
   bare_qpg(1,1)=bare_qpg(1,1)/pre_factor
   !
 end if
end subroutine
