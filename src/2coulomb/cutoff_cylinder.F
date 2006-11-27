!
! Copyright (C) 2000-2005 D. Varsano and the SELF team 
!         http://www.fisica.uniroma2.it/~self
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
!====================================
 use pars,          ONLY:SP,pi
 use wave_func,     ONLY:wf_ng
 use D_lattice,     ONLY:alat
 use R_lattice,     ONLY:cyl_ph_radius,bare_qpg,cyl_length,cyl_cut,g_vec,&
&                        bz_samp,ng_closed,cyl_vr_save,cyl_vz_save,cyl_zz_save
 use par_proc_m,    ONLY:pp_redux_wait,pp_indexes,myid,pp_indexes_reset
 use par_indexes_m, ONLY:par_indexes
 use timing,        ONLY:live_timing
 use com,           ONLY:msg,error
 
 implicit none
 type(bz_samp) ::q
!ws
 integer       ::iq,ig,iort1,iort2,ipar
 integer       :: neval,ier,last,key,lenw,lenw2,limit,leniw,maxp1
 real(SP)      ::cyl_ph_radius2,cyl_cut2,c1,c2,Vd
 real(SP)      ::BESJ1 
 real(SP),  external ::F1,F2,F3,F4,F5,F6
 real(SP)      :: abserr,epsabs
 real(SP)      :: z
 complex(SP)   ::V_cut(q%nibz,wf_ng)
 real(SP),  allocatable ::work(:),work2(:)
 integer,   allocatable ::iwork(:),iwork2(:)
 type(pp_indexes) ::px
 logical       ::infcyl
 logical       ::is_cut(3)
!
!-------------------------------
 call pp_indexes_reset(px)

 infcyl=cyl_length==0.

!
!Check cylinder axis in along one principal axis
!

 if(is_cut(1).and.is_cut(2)) call error('Check cylinder axis direction')
 if(is_cut(2).and.is_cut(3)) call error('Check cylinder axis direction')
 if(is_cut(1).and.is_cut(3)) call error('Check cylinder axis direction')
 
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

 cyl_cut=cyl_length
 if (infcyl) then
  cyl_cut=abs(1/(q%pt(2,ipar))*alat(ipar))-1.
  call msg('r','Infinite cylinder: Length set to 1/dq [au]:',cyl_cut)
 endif

 
 cyl_cut2 = cyl_cut**2
 cyl_ph_radius2  = cyl_ph_radius**2

!
 V_cut=cmplx(0.)
!
 call par_indexes(px,(/q%nibz,wf_ng/))
!
 call live_timing('Cylinder',px%stps(myid+1))

 if (.not.infcyl) then !finite cylinder
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
       if (.not.px%i2p(iq,ig)) cycle
       cyl_vz_save=(q%pt(iq,ipar)+g_vec(ig,ipar) )*2.*pi/alat(ipar)
       cyl_vr_save=sqrt(((q%pt(iq,iort1)+g_vec(ig,iort1))*2.*pi/alat(iort1))**2+&
&            ((q%pt(iq,iort2)+g_vec(ig,iort2))*2.*pi/alat(iort2))**2)
!  
       if (cyl_vr_save==0..and.cyl_vz_save/=0) then
         c1=1./cyl_vz_save**2-cos(cyl_vz_save*cyl_cut)/cyl_vz_save**2-&
&        cyl_cut*sin(cyl_vz_save*cyl_cut)/cyl_vz_save
         c2=sin(cyl_vz_save*cyl_cut)*sqrt(cyl_ph_radius2+cyl_cut2)
         call qawo (F3,0,cyl_cut,cyl_vz_save,2,epsabs,0,Vd,&
&                   abserr,neval,ier,leniw,maxp1,lenw2,last,iwork2,work2)
         V_cut(iq,ig)=c1+(c2-Vd)/cyl_vz_save
       elseif (cyl_vz_save==0.) then
         call qag(F4,0,cyl_ph_radius,epsabs,0,key,Vd,abserr, &
&                 neval,ier,limit,lenw,last,iwork,work)
        V_cut(iq,ig)=Vd
       else
         call qawo(F2,0,cyl_cut,cyl_vz_save,1,epsabs,0,Vd,  &
&                  abserr,neval,ier,leniw,maxp1,lenw2,last,iwork2,work2)
     
         V_cut(iq,ig)=Vd
       endif
      call live_timing(steps=1)
     enddo
   enddo
   deallocate(iwork,iwork2,work,work2)
!
 else   ! infinite
!    
    limit=2
    lenw=limit*4
    epsabs=1e-2
    key=3
    allocate (iwork(limit),work(lenw))
    do ig=1,wf_ng
      do iq=1,q%nibz
        if (.not.px%i2p(iq,ig)) cycle
          cyl_vz_save=(q%pt(iq,ipar)+g_vec(ig,ipar) )*2.*pi/alat(ipar)
          cyl_vr_save=sqrt(((q%pt(iq,iort1)+g_vec(ig,iort1))*2.*pi/alat(iort1))**2+&
&                    ((q%pt(iq,iort2)+g_vec(ig,iort2))*2.*pi/alat(iort2))**2)
!  
        if (cyl_vz_save==0.and.cyl_vr_save>0.) then
          call qag(F5,0,cyl_ph_radius,epsabs,0,key,Vd,abserr, &
&                  neval,ier,limit,lenw,last,iwork,work)
          V_cut(iq,ig)=-Vd+cyl_ph_radius*log(2*cyl_cut)* &
&                      BESJ1(cyl_vr_save*cyl_ph_radius)/cyl_vr_save 
        else if (cyl_vz_save/=0.) then
          V_cut(iq,ig)=F6(cyl_vr_save,abs(cyl_vz_save))/(cyl_vz_save**2+cyl_vr_save**2)
        endif
     call live_timing(steps=1)
    enddo
  enddo
  deallocate(iwork,work)
 endif
 
 V_cut(1,1)=0.5*(-cyl_cut2+cyl_cut*sqrt(cyl_cut2+cyl_ph_radius2)+&
&           cyl_ph_radius2*log((cyl_cut+sqrt(cyl_cut2+cyl_ph_radius2))/&
            cyl_ph_radius))
 call live_timing()
 forall (iq=1:q%nibz,ig=1:wf_ng) bare_qpg(iq,ig)=sqrt(1./V_cut(iq,ig))

 end subroutine
!
 

 function F1(s)
!----------------------------
 use pars,          ONLY:SP
 use R_lattice,     ONLY:cyl_vr_save,cyl_vz_save,cyl_zz_save
 real(SP) s,arg,BESJ0
 arg=cyl_vr_save*s
 if(cyl_zz_save==0) then
  F1=BESJ0(arg)
 else
  F1=BESJ0(arg)*s/sqrt(s**2+cyl_zz_save**2)
 endif
 end function F1
!
 function F2(z)
!----------------------------
 use pars,          ONLY:SP
 use R_lattice,     ONLY:cyl_vr_save,cyl_vz_save,cyl_zz_save,cyl_ph_radius
 real(SP) z,G
 real(SP),  external ::F1
 real(SP)      :: abserr
 integer       :: neval,ier,last,iwork(3)
 real(SP)      :: work(12) 
 cyl_zz_save=z

 call qag(F1,0,cyl_ph_radius,8.e-1,0.,4,G,abserr,neval,ier, &
 &             3,12,last,iwork,work)
 F2=G
 end function F2
!
 function F3(z)
!----------------------------
 use pars,          ONLY:SP 
 use R_lattice,     ONLY:cyl_vr_save,cyl_vz_save,cyl_zz_save,cyl_ph_radius
 real(SP) z
 F3=z/sqrt(cyl_ph_radius**2+z**2)
 end function F3
!
 function F4(z)
!----------------------------
 use pars,          ONLY:SP
 use R_lattice,     ONLY:cyl_vr_save,cyl_vz_save,cyl_zz_save,cyl_cut
 real(SP) z,arg,BESJ0
 if (z==0) then
  F4=0.
 else 
  arg=cyl_vr_save*z
  F4=BESJ0(arg)*z*log((cyl_cut+sqrt(cyl_cut**2+z**2))/z)
 endif
 end function F4
!
 function F5(z)
!----------------------------
 use pars,          ONLY:SP
 use R_lattice,     ONLY:cyl_vr_save,cyl_vz_save,cyl_zz_save
 real(SP) z,arg,BESJ0
 if (z==0) then
  F5=0.
 else 
  arg=cyl_vr_save*z
  F5=BESJ0(arg)*z*log(z)
 endif
 end function F5
!
 function F6(x,y)
!----------------------------
 use pars,          ONLY:SP
 use R_lattice,     ONLY:cyl_ph_radius
 real(SP) x,y,arg1,arg2,BESJ0,BESJ1,BESK0,BESK1
 arg1=x*cyl_ph_radius
 arg2=y*cyl_ph_radius
 F6=arg1*BESJ1(arg1)*BESK0(arg2)-arg2*BESJ0(arg1)*BESK1(arg2)
 F6=F6+1
 end function F6


