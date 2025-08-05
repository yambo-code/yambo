!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): DV
!
! headers
!
#include<y_memory.h>
!
subroutine cutoff_cylinder(q,is_cut)
 !
 use pars,          ONLY:SP,pi,schlen
 use wave_func,     ONLY:wf_ng
 use D_lattice,     ONLY:alat
 use R_lattice,     ONLY:cyl_ph_radius,bare_qpg,cyl_cut,g_vec,&
&                        bz_samp,cyl_vr_save,cyl_vz_save
 use parallel_m,    ONLY:PP_indexes,myid,PP_indexes_reset
 use parallel_int,  ONLY:PP_redux_wait,PARALLEL_index 
 use LIVE_t,        ONLY:live_timing
 use com,           ONLY:msg
 use y_memory_alloc
 !
 implicit none
 type(bz_samp) ::q
 logical       ::is_cut(3)
 !
 ! Work Space
 !
 integer       ::iq,ig,iort1,iort2,ipar,neval,ier,last,key,lenw,&
&                lenw2,limit,leniw,maxp1
 real(SP)      ::cyl_ph_radius2,cyl_cut2,c1,c2,Vd,abserr,epsabs
 complex(SP)   ::V_cut(q%nibz,wf_ng)
 character(schlen) :: cyl_direction
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
 ! Check cylinder axis in along one principal axis
 !
 if(all(is_cut)) call error('Infinite cylinder, but cutoff in all direction')
 cyl_direction="unknonw"
 if(is_cut(2).and.is_cut(3)) cyl_direction="x"
 if(is_cut(1).and.is_cut(3)) cyl_direction="y"
 if(is_cut(1).and.is_cut(2)) cyl_direction="z"
 if(trim(cyl_direction)=="unkonwn") call error('Unknown cylinder direction')
 !
 if(trim(cyl_direction)=="x") then
   ipar=1
   iort1=2
   iort2=3
 elseif(trim(cyl_direction)=="y") then
   ipar=2
   iort1=1
   iort2=3
 elseif(trim(cyl_direction)=="z") then
   ipar=3
   iort1=2
   iort2=1
 endif
 !
 cyl_cut=abs(1/(q%pt(2,ipar))*alat(ipar))-1.
 call msg('r','Infinite cylinder: Length set to 1/dq     ',cyl_cut,"[a.u.]")
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
 ! infinite cylinder
 !
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
         call dqag(bessel_F5,0.,cyl_ph_radius,epsabs,0.,key,Vd,abserr, &
&                 neval,ier,limit,lenw,last,iwork,work)
         V_cut(iq,ig)=-Vd+cyl_ph_radius*log(2*cyl_cut)* &
&                     DBESJ1_(cyl_vr_save*cyl_ph_radius)/cyl_vr_save 
#else
         call qag(bessel_F5,0.,cyl_ph_radius,epsabs,0.,key,Vd,abserr, &
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
 ! cleanup
 call PP_indexes_reset(px)
 !
end subroutine
