!
!        Copyright (C) 2000-2019 the YAMBO team
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
subroutine cutoff_slab(q,is_cut)
 !
 use com,           ONLY:msg,of_open_close
 use pars,          ONLY:SP,pi,schlen
 use wave_func,     ONLY:wf_ng
 use D_lattice,     ONLY:alat
 use R_lattice,     ONLY:bz_samp,g_vec,bare_qpg,k_grid_uc_vol
 use com,           ONLY:error
 use parallel_m,    ONLY:PP_indexes,PP_indexes_reset
 use interfaces,    ONLY:PARALLEL_index

! use R_lattice,     ONLY:cyl_ph_radius,bare_qpg,nqibz
! use R_lattice,     ONLY:cyl_ph_radius,bare_qpg,nqibz

 implicit none
 !
 ! Work Space
 ! 
! real(SP)::dummy
 type(bz_samp) :: q
 logical       :: is_cut(3)
 integer       :: ipar1,ipar2,iort
 integer       :: iq,ig
 real(SP)      :: slab_vz,slab_vplane,lcut
 complex(SP)   :: dummy
 type(PP_indexes) ::px
 character(schlen)  :: of_name
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

of_name='debug'
call of_open_close(of_name,'ot')
 !
 do ig=1,wf_ng
   do iq=1,q%nibz
!marg: shall I put this px thing? 
     if (.not.px%element_2D(iq,ig)) cycle
! kz
     slab_vz=(q%pt(iq,iort)+g_vec(ig,iort) )*2.*pi/alat(iort)
! kxy
     slab_vplane=sqrt(((q%pt(iq,ipar1)+g_vec(ig,ipar1))*2.*pi/alat(ipar1))**2+&
&                 ((q%pt(iq,ipar2)+g_vec(ig,ipar2))*2.*pi/alat(ipar2))**2)               

! k=0 case
     if (iq==1.and.ig==1) cycle
! all other
     dummy=sqrt(1.-exp(-slab_vplane*lcut)*cos(slab_vz*lcut))
     call msg('o deb','',(/real(bare_qpg(iq,ig)),aimag(bare_qpg(iq,ig)),real(dummy),aimag(dummy)/),INDENT=-2,USE_TABS=.true.) 

!marg: check if needed/makes sense
     if (abs(dummy)<1.E-5) dummy=1.E-5
     bare_qpg(iq,ig)=bare_qpg(iq,ig)/dummy
   enddo
 enddo

 dummy=2.0*pi*sqrt(lcut*k_grid_uc_vol)
  
 bare_qpg(1,1)=1.0/dummy

 call of_open_close(of_name) 
 call PP_indexes_reset(px)
 !
end subroutine
