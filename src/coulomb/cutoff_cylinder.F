!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DV
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
subroutine cutoff_cylinder(q,is_cut)
 !
 use pars,          ONLY:SP,pi
 use wave_func,     ONLY:wf_ng
 use D_lattice,     ONLY:alat
 use R_lattice,     ONLY:cyl_ph_radius,bare_qpg,cyl_length,cyl_cut,g_vec,&
&                        bz_samp,cyl_vr_save,cyl_vz_save
 use parallel_m,    ONLY:PP_redux_wait,PP_indexes,myid,PP_indexes_reset
 use interfaces,    ONLY:PARALLEL_index
 use LIVE_t,        ONLY:live_timing
 use com,           ONLY:msg,error
 !
#include<memory.h>
 type(bz_samp) ::q
 logical       ::is_cut(3)
 !
 ! Work Space
 !
 integer       ::iq,ig,iort1,iort2,ipar,neval,ier,last,key,lenw,&
&                lenw2,limit,leniw,maxp1
 real(SP)      ::cyl_ph_radius2,cyl_cut2,c1,c2,Vd,abserr,epsabs
 complex(SP)   ::V_cut(q%nibz,wf_ng)
 logical       ::infcyl
#if defined _DOUBLE
 real(SP),  external    ::DBESJ1_
#else
 real(SP),  external    ::BESJ1
#endif
 real(SP),  external    ::bessel_F2,bessel_F3,bessel_F4,bessel_F5,bessel_F6
 real(SP),  allocatable ::work(:),work2(:)
 integer,   allocatable ::iwork(:),iwork2(:)
 type(PP_indexes) ::px
 !
 call PP_indexes_reset(px)
 !
 infcyl=cyl_length==0.
 !
 ! Check cylinder axis in along one principal axis
 !
 if(is_cut(1).and.is_cut(2)) call error('Check cylinder axis direction')
 if(is_cut(2).and.is_cut(3)) call error('Check cylinder axis direction')
 if(is_cut(1).and.is_cut(3)) call error('Check cylinder axis direction')
 !
 if(is_cut(1)) then
   ipar=1
   iort1=2
   iort2=3
 elseif(is_cut(2)) then
   ipar=2
   iort1=1
   iort2=3
 elseif(is_cut(3)) then
   ipar=3
   iort1=2
   iort2=1
 endif
 !
 cyl_cut=cyl_length
 if (infcyl) then
  cyl_cut=abs(1/(q%pt(2,ipar))*alat(ipar))-1.
  call msg('r','Infinite cylinder: Length set to 1/dq [au]:',cyl_cut)
 endif
 !
 cyl_cut2 = cyl_cut**2
 cyl_ph_radius2  = cyl_ph_radius**2
 !
 V_cut=cmplx(0.)
 !
 call PARALLEL_index(px,(/q%nibz,wf_ng/))
 !
 call live_timing('Cylinder',px%n_of_elements(myid+1))
 !
 ! finite cylinder
 !
 if (.not.infcyl) then 
   epsabs=8e-1
   key=4
   limit=2
   lenw=limit*4
   leniw=8
   maxp1=2
   lenw2=leniw*2+maxp1*25
   allocate (iwork(limit),work(lenw))
   allocate (iwork2(leniw),work2(lenw2))
   do ig=1,wf_ng
     do iq=1,q%nibz
       if (.not.px%element_2D(iq,ig)) cycle
       cyl_vz_save=(q%pt(iq,ipar)+g_vec(ig,ipar) )*2.*pi/alat(ipar)
       cyl_vr_save=sqrt(((q%pt(iq,iort1)+g_vec(ig,iort1))*2.*pi/alat(iort1))**2+&
&            ((q%pt(iq,iort2)+g_vec(ig,iort2))*2.*pi/alat(iort2))**2)
       !  
       if (cyl_vr_save==0..and.cyl_vz_save/=0) then
         c1=1./cyl_vz_save**2-cos(cyl_vz_save*cyl_cut)/cyl_vz_save**2-&
&        cyl_cut*sin(cyl_vz_save*cyl_cut)/cyl_vz_save
         c2=sin(cyl_vz_save*cyl_cut)*sqrt(cyl_ph_radius2+cyl_cut2)
#if defined _DOUBLE
         call dqawo (bessel_F3,0,cyl_cut,cyl_vz_save,2,epsabs,0,Vd,&
&                   abserr,neval,ier,leniw,maxp1,lenw2,last,iwork2,work2)
#else
         call qawo (bessel_F3,0,cyl_cut,cyl_vz_save,2,epsabs,0,Vd,&
&                   abserr,neval,ier,leniw,maxp1,lenw2,last,iwork2,work2)
#endif
         V_cut(iq,ig)=c1+(c2-Vd)/cyl_vz_save
       elseif (cyl_vz_save==0.) then
#if defined _DOUBLE
         call dqag(bessel_F4,0,cyl_ph_radius,epsabs,0,key,Vd,abserr, &
&                 neval,ier,limit,lenw,last,iwork,work)
#else
         call qag(bessel_F4,0,cyl_ph_radius,epsabs,0,key,Vd,abserr, &
&                 neval,ier,limit,lenw,last,iwork,work)
#endif
        V_cut(iq,ig)=Vd
       else
#if defined _DOUBLE
         call dqawo(bessel_F2,0,cyl_cut,cyl_vz_save,1,epsabs,0,Vd,  &
&                  abserr,neval,ier,leniw,maxp1,lenw2,last,iwork2,work2)
#else
         call qawo(bessel_F2,0,cyl_cut,cyl_vz_save,1,epsabs,0,Vd,  &
&                  abserr,neval,ier,leniw,maxp1,lenw2,last,iwork2,work2)
#endif
     
         V_cut(iq,ig)=Vd
       endif
      call live_timing(steps=1)
     enddo
   enddo
   YAMBO_FREE(iwork)
   YAMBO_FREE(iwork2)
   YAMBO_FREE(work)
   YAMBO_FREE(work2)
   !
   ! infinite
   !
 else   
   !
   limit=2
   lenw=limit*4
   epsabs=8e-1
   key=4
   allocate (iwork(limit),work(lenw))
   do ig=1,wf_ng
     do iq=1,q%nibz
       if (.not.px%element_2D(iq,ig)) cycle
       cyl_vz_save=(q%pt(iq,ipar)+g_vec(ig,ipar) )*2.*pi/alat(ipar)
       cyl_vr_save=sqrt(((q%pt(iq,iort1)+g_vec(ig,iort1))*2.*pi/alat(iort1))**2+&
&                 ((q%pt(iq,iort2)+g_vec(ig,iort2))*2.*pi/alat(iort2))**2)
       ! 
       if (cyl_vz_save==0.and.cyl_vr_save>0.) then
#if defined _DOUBLE
         call dqag(bessel_F5,0,cyl_ph_radius,epsabs,0,key,Vd,abserr, &
&                 neval,ier,limit,lenw,last,iwork,work)
         V_cut(iq,ig)=-Vd+cyl_ph_radius*log(2*cyl_cut)* &
&                     DBESJ1_(cyl_vr_save*cyl_ph_radius)/cyl_vr_save 
#else
         call qag(bessel_F5,0,cyl_ph_radius,epsabs,0,key,Vd,abserr, &
&                 neval,ier,limit,lenw,last,iwork,work)
         V_cut(iq,ig)=-Vd+cyl_ph_radius*log(2*cyl_cut)* &
&                     BESJ1(cyl_vr_save*cyl_ph_radius)/cyl_vr_save 
#endif
       else if (cyl_vz_save/=0.) then
         V_cut(iq,ig)=bessel_F6(cyl_vr_save,abs(cyl_vz_save))/(cyl_vz_save**2+cyl_vr_save**2)
       endif
       call live_timing(steps=1)
     enddo
   enddo
   YAMBO_FREE(work)
   YAMBO_FREE(work)
 endif
 !
 ! MPI 2 all
 !
 call PP_redux_wait(V_cut)
 !
 V_cut(1,1)=0.5*(-cyl_cut2+cyl_cut*sqrt(cyl_cut2+cyl_ph_radius2)+&
&           cyl_ph_radius2*log((cyl_cut+sqrt(cyl_cut2+cyl_ph_radius2))/&
            cyl_ph_radius))
 !
 call live_timing()
 !
 forall (iq=1:q%nibz,ig=1:wf_ng) bare_qpg(iq,ig)=sqrt(1./V_cut(iq,ig))
 !
end subroutine
