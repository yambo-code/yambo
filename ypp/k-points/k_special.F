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
subroutine k_special(LIST_only)
 !
 ! Special k-points. Output in IKU
 !
 use pars,                ONLY:SP,schlen
 use units,               ONLY:pi
 use R_lattice,           ONLY:bz_samp_reset,b
 use D_lattice,           ONLY:alat,lattice,a
 use com,                 ONLY:msg
 use vec_operate,         ONLY:c2a
 use YPPm,                ONLY:K_transform,SPECIAL_k,n_SPECIAL_k_MAX,SPECIAL_k_label
 use LIVE_t,              ONLY:live_timing_is_on
 !
#include<memory.h>
 !
 ! Input 
 !
 logical, optional :: LIST_only
 !
 ! Work Space
 !
 logical            :: l_use_yambo_b
 integer            :: ik
 real(SP)           :: trial_b(3,3),ap,bp,cp,fp,ep,k_l(3)
 character(schlen)  :: dumb_ch
 !
 call section('+',"Special Points for the "//trim(lattice)//" lattice")
 !======================================================================  
 !
 call bz_samp_reset(SPECIAL_k)
 !
 YAMBO_ALLOC(SPECIAL_k%pt,(n_SPECIAL_k_MAX,3))
 !
 ! Special K taken from 
 ! 
 !  http://cst-www.nrl.navy.mil/bind/kpts/index.html
 !  http://lamp.tu-graz.ac.at/
 !
 ! 2019/02/01:
 ! If yambo "b" is different
 ! i.e. in case for example the FCC has
 ! b(1,:)=(/-0.5, 0.5, 0.5/)*2.*pi/alat(1)
 ! b(2,:)=(/ 0.5,-0.5.,0.5/)*2.*pi/alat(1)
 ! b(3,:)=(/ 0.5, 0.5,-0.5/)*2.*pi/alat(1)
 ! to properly detect special k-points
 ! set the following logical to .true.
 !
 l_use_yambo_b=.false.
 !
 select case (trim(lattice))
   !
   case('FCC')
     !
     if(l_use_yambo_b) then
       trial_b=b
     else
       trial_b(1,:)=(/-1., 1., 1./)*2.*pi/alat(1)
       trial_b(2,:)=(/ 1.,-1., 1./)*2.*pi/alat(1)
       trial_b(3,:)=(/ 1., 1.,-1./)*2.*pi/alat(1)
     endif
     !
     SPECIAL_k%nbz=6
     !
     SPECIAL_k%pt(1,:)=(/0._SP,0._SP,0._SP/)
     SPECIAL_k_label(1)='G'
     SPECIAL_k%pt(2,:)=(/0.5_SP,0.5_SP,0._SP/)
     SPECIAL_k_label(2)='X'
     SPECIAL_k%pt(3,:)=(/0.5_SP,0.5_SP,0.5_SP/)
     SPECIAL_k_label(3)='L'
     SPECIAL_k%pt(4,:)=(/0.5_SP,0.75_SP,0.25_SP/)
     SPECIAL_k_label(4)='W'
     SPECIAL_k%pt(5,:)=(/0.375_SP,0.375_SP,0.750_SP/)
     SPECIAL_k_label(5)='K'
     SPECIAL_k%pt(6,:)=(/0.375_SP,0.375_SP,0.750_SP/)
     SPECIAL_k_label(6)='U'
     !
     do ik=1,SPECIAL_k%nbz
       call c2a(b_in=trial_b,v_in=SPECIAL_k%pt(ik,:),mode='ka2c')
     enddo
     !
   case('BCC')
     !
     if(l_use_yambo_b) then
       trial_b=b
     else
       trial_b(1,:)=(/ 0._SP, 1._SP, 1._SP/)*2._SP*pi/alat(1)
       trial_b(2,:)=(/ 1._SP, 0._SP, 1._SP/)*2._SP*pi/alat(1)
       trial_b(3,:)=(/ 1._SP, 0._SP,-1._SP/)*2._SP*pi/alat(1)
     endif
     !
     SPECIAL_k%nbz=4
     !
     SPECIAL_k%pt(1,:)=(/0._SP,0._SP,0._SP/)
     SPECIAL_k_label(1)='G'
     SPECIAL_k%pt(2,:)=(/0.5_SP,0.5_SP,0._SP/)
     SPECIAL_k_label(2)='N'
     SPECIAL_k%pt(3,:)=(/0.5_SP,0.5_SP,0.5_SP/)
     SPECIAL_k_label(3)='P'
     SPECIAL_k%pt(4,:)=(/0._SP ,1._SP  ,0._SP  /)
     SPECIAL_k_label(4)='H'
     !
   case('FC-ORTHOROMBIC (b<a)')
     !
     ap=pi/a(1,1)
     bp=pi/a(1,2)
     cp=pi/a(3,3)
     !
     call orthorombic_lengths(bp,ap)
     !
     SPECIAL_k%nbz=10
     !
     SPECIAL_k%pt(1,:)=(/0._SP,0._SP,0._SP/)
     SPECIAL_k_label(1)='G'
     SPECIAL_k%pt(2,:)=SPECIAL_k%pt(1,:)+(/0._SP,0._SP,cp/)
     SPECIAL_k_label(2)='Z'
     !
     SPECIAL_k%pt(3,:)=(/ap,0._SP,0._SP/)
     SPECIAL_k_label(3)='X'
     SPECIAL_k%pt(4,:)=SPECIAL_k%pt(3,:)+(/0._SP,0._SP,cp/)
     SPECIAL_k_label(4)='A'
     !
     SPECIAL_k_label(5)='Y'
     SPECIAL_k%pt(5,:)=(/0._SP,fp,0._SP/)
     SPECIAL_k_label(6)='T'
     SPECIAL_k%pt(6,:)=SPECIAL_k%pt(5,:)+(/0._SP,0._SP,cp/)
     !
     SPECIAL_k_label(7)='S'
     SPECIAL_k%pt(7,:)=(/ap/2._SP,bp/2._SP,0._SP/)
     SPECIAL_k_label(8)='R'
     SPECIAL_k%pt(8,:)=SPECIAL_k%pt(7,:)+(/0._SP,0._SP,cp/)
     !
     SPECIAL_k_label(9)='X1'
     SPECIAL_k%pt(9,:)=(/ap,ep,0._SP/)
     SPECIAL_k_label(10)='A1'
     SPECIAL_k%pt(10,:)=SPECIAL_k%pt(9,:)+(/0._SP,0._SP,cp/)
     !
   case('FC-ORTHOROMBIC (b>a)')
     !
     ap=pi/a(1,1)
     bp=pi/a(1,2)
     cp=pi/a(3,3)
     !
     call orthorombic_lengths(ap,bp)
     !
     SPECIAL_k%nbz=15
     !
     SPECIAL_k%pt(1,:)=(/0._SP,0._SP,0._SP/)
     SPECIAL_k_label(1)='G'
     SPECIAL_k%pt(2,:)=SPECIAL_k%pt(1,:)+(/0._SP,0._SP,cp/)
     SPECIAL_k_label(2)='Z'
     !
     SPECIAL_k%pt(3,:)=(/fp,0._SP,0._SP/)
     SPECIAL_k_label(3)='X'
     SPECIAL_k%pt(4,:)=SPECIAL_k%pt(3,:)+(/0._SP,0._SP,cp/)
     SPECIAL_k_label(4)='A'
     !
     SPECIAL_k_label(5)='Y'
     SPECIAL_k%pt(5,:)=(/0._SP,bp,0._SP/)
     SPECIAL_k_label(6)='T'
     SPECIAL_k%pt(6,:)=SPECIAL_k%pt(5,:)+(/0._SP,0._SP,cp/)
     !
     SPECIAL_k_label(7)='S'
     SPECIAL_k%pt(7,:)=(/ap/2._SP,bp/2._SP,0._SP/)
     SPECIAL_k_label(8)='R'
     SPECIAL_k%pt(8,:)=SPECIAL_k%pt(7,:)+(/0._SP,0._SP,cp/)
     !
     SPECIAL_k_label(9)='X1'
     SPECIAL_k%pt(9,:)=(/ep,bp,0._SP/)
     SPECIAL_k_label(10)='A1'
     SPECIAL_k%pt(10,:)=SPECIAL_k%pt(9,:)+(/0._SP,0._SP,cp/)
     !
     SPECIAL_k_label(10)='X2'
     SPECIAL_k%pt(10,:)=SPECIAL_k%pt(3,:)+SPECIAL_k%pt(5,:)
     SPECIAL_k_label(11)='A2'
     SPECIAL_k%pt(11,:)=SPECIAL_k%pt(4,:)-2.*SPECIAL_k%pt(3,:)
     SPECIAL_k_label(12)='A3'
     SPECIAL_k%pt(12,:)=SPECIAL_k%pt(4,:)-2.*SPECIAL_k%pt(2,:)
     SPECIAL_k_label(13)='A4'
     SPECIAL_k%pt(13,:)=SPECIAL_k%pt(4,:)+SPECIAL_k%pt(5,:)
     SPECIAL_k_label(14)='A5'
     SPECIAL_k%pt(14,:)=SPECIAL_k%pt(12,:)+SPECIAL_k%pt(5,:)
     SPECIAL_k_label(15)='A6'
     SPECIAL_k%pt(15,:)=SPECIAL_k%pt(11,:)+SPECIAL_k%pt(5,:)
     !
     call k_add_special_regions( (/0.88,0.  /),"XYR1" )
     call k_add_special_regions( (/0.12,0.  /),"XYR2" )
     call k_add_special_regions( (/0.12,0.26/),"XYR3" )
     !
     call k_add_special_regions( (/0.62,0.16/),"YpR1" )
     call k_add_special_regions( (/0.38,0.16/),"YpR2" )
     call k_add_special_regions( (/0.38,0.26/),"YpR3" )
     !
     call k_add_special_regions( (/0.72,0.26/),"GR1" )
     call k_add_special_regions( (/0.28,0.26/),"GR2" )
     call k_add_special_regions( (/0.28,0.50/),"GR3" )
     !
     call k_add_special_regions( (/0.72,0.50/),"GL1" )
     call k_add_special_regions( (/0.28,0.50/),"GL2" )
     call k_add_special_regions( (/0.28,0.74/),"GL3" )
     !
     call k_add_special_regions( (/0.88,0.74/),"XYL1" )
     call k_add_special_regions( (/0.12,0.74/),"XYL2" )
     call k_add_special_regions( (/0.12,1.  /),"XYL3" )
     !
     call k_add_special_regions( (/0.62,0.74/),"YpL1" )
     call k_add_special_regions( (/0.38,0.74/),"YpL2" )
     call k_add_special_regions( (/0.38,0.84/),"YpL3" )
     !
   case('CUB')
     !
     if(l_use_yambo_b) then
       trial_b=b
     else
       trial_b(1,:)=(/ 1., 0., 0./)*2.*pi/alat(1)
       trial_b(2,:)=(/ 0., 1., 0./)*2.*pi/alat(1)
       trial_b(3,:)=(/ 0., 0., 1./)*2.*pi/alat(1)
     endif
     !
     SPECIAL_k%nbz=4
     !
     SPECIAL_k%pt(1,:)=(/0.,0.,0./)
     SPECIAL_k_label(1)='G'
     SPECIAL_k%pt(2,:)=(/0.5,0.,0./)
     SPECIAL_k_label(2)='X'
     SPECIAL_k%pt(3,:)=(/0.5,0.5,0./)
     SPECIAL_k_label(3)='M'
     SPECIAL_k%pt(4,:)=(/0.5,0.5,0.5/)
     SPECIAL_k_label(4)='R'
     !
     do ik=1,SPECIAL_k%nbz
       call c2a(b_in=trial_b,v_in=SPECIAL_k%pt(ik,:),mode='ka2c')
     enddo
     !
   case('HCP')
     !
     if(l_use_yambo_b) then
       trial_b=b
     else
       trial_b(1,:)=(/ 1.,-1./sqrt(3.), 0./)*2.*pi/alat(1)
       trial_b(2,:)=(/ 1., 1./sqrt(3.), 0./)*2.*pi/alat(1)
       trial_b(3,:)=(/ 0., 0.,          1./)*2.*pi/alat(3)
     endif
     !
     SPECIAL_k%nbz=7
     !
     SPECIAL_k%pt(1,:)=(/0.,0.,0./)
     SPECIAL_k_label(1)='G'
     SPECIAL_k%pt(2,:)=(/0. ,0.5,0./)
     SPECIAL_k_label(2)='M'
     SPECIAL_k%pt(3,:)=(/2./3.,0.,0./)
     SPECIAL_k%pt(3,:)=(/1./3.,1./3.,0./)
     SPECIAL_k_label(3)='K'
     SPECIAL_k%pt(4,:)=(/-1./3.,-1./3.,0./)
     SPECIAL_k_label(4)='Kp'
     SPECIAL_k%pt(5,:)=(/0. ,0.  ,0.5/)
     SPECIAL_k_label(5)='A'
     SPECIAL_k%pt(6,:)=(/0.,0.5,0.5/)
     SPECIAL_k_label(6)='L'
     SPECIAL_k%pt(7,:)=(/1./3.,1./3.,0.5/)
     SPECIAL_k_label(7)='H'
     !
     do ik=1,SPECIAL_k%nbz
       call c2a(b_in=trial_b,v_in=SPECIAL_k%pt(ik,:),mode='ka2c')
     enddo
     !
 end select
 !
 if (SPECIAL_k%nbz==0) then
   call warning('Unknown lattice unit cell')
   return
 endif
 !
 ! Report 
 !--------
 if (present(LIST_only)) then
   if (LIST_only) then
     live_timing_is_on=.false.
     do ik=1,SPECIAL_k%nbz
       k_l=SPECIAL_k%pt(ik,:)
       call K_transform(k_l,'cc')
       write (dumb_ch,'(3f12.7,2x,a)') k_l,trim(SPECIAL_k_label(ik))
       call msg("s",trim(dumb_ch))
     enddo
     live_timing_is_on=.true.
   endif
   YAMBO_FREE(SPECIAL_k%pt)
   return
 endif
 !
 do ik=1,SPECIAL_k%nbz
   call c2a(v_in=SPECIAL_k%pt(ik,:),mode='kc2i')
 enddo
 !
 contains
   !
   subroutine orthorombic_lengths(xp,yp)
    !
    real(SP) :: dp,xp,yp
    !
    dp=sqrt(xp**2 + yp**2)/2._SP
    fp=2*dp**2/xp
    ep=xp-fp
    !
   end subroutine
   !
end subroutine k_special
!
subroutine k_add_special_regions( coordinates,label )
 !
 use pars,                ONLY:SP
 use YPPm,                ONLY:SPECIAL_k,n_SPECIAL_k_MAX,SPECIAL_k_label
 !
 implicit none
 !
 real(SP)     :: coordinates(2)
 character(*) :: label
 !
 SPECIAL_k%nbz=SPECIAL_k%nbz+1
 SPECIAL_k%pt(SPECIAL_k%nbz,:)=K_el(coordinates(1),coordinates(2))
 SPECIAL_k_label(SPECIAL_k%nbz)=label
 !
 contains 
   !
   function K_el(alpha,beta)
     real(SP) :: K_el(3),alpha,beta
     K_el=(1.-alpha-beta)*SPECIAL_k%pt(13,:)+alpha*SPECIAL_k%pt(14,:)+beta*SPECIAL_k%pt(15,:)
   end function
 !
end subroutine
