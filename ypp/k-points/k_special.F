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
 use YPPm,                ONLY:K_transform,SPECIAL_k,n_SPECIAL_k_MAX,SPECIAL_k_label,coo_out
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
     call k_add((/0._SP,0._SP,0._SP/),'G' )
     call k_add((/0.5_SP,0.5_SP,0._SP/),'X' )
     call k_add((/0.5_SP,0.5_SP,0.5_SP/),'L' )
     call k_add((/0.5_SP,0.75_SP,0.25_SP/),'W' )
     call k_add((/0.375_SP,0.375_SP,0.750_SP/),'K' )
     call k_add((/0.375_SP,0.375_SP,0.750_SP/),'U' )
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
     call k_add((/0._SP,0._SP,0._SP/),'G' )
     call k_add((/0.5_SP,0.5_SP,0._SP/),'N' )
     call k_add((/0.5_SP,0.5_SP,0.5_SP/),'P' )
     call k_add((/0._SP ,1._SP  ,0._SP  /),'H' )
     !
   case('FC-ORTHOROMBIC (b<a)')
     !
     ap=pi/a(1,1)
     bp=pi/a(1,2)
     cp=pi/a(3,3)
     !
     call orthorombic_lengths(bp,ap)
     !
     call k_add((/0._SP,0._SP,0._SP/),'G' )
     call k_add(SPECIAL_k%pt(1,:)+(/0._SP,0._SP,cp/),'Z' )
     !
     call k_add((/ap,0._SP,0._SP/),'X' )
     call k_add(SPECIAL_k%pt(3,:)+(/0._SP,0._SP,cp/),'A' )
     !
     call k_add((/0._SP,fp,0._SP/),'Y' )
     call k_add(SPECIAL_k%pt(5,:)+(/0._SP,0._SP,cp/),'T' )
     !
     call k_add((/ap/2._SP,bp/2._SP,0._SP/),'S' )
     call k_add(SPECIAL_k%pt(7,:)+(/0._SP,0._SP,cp/),'R' )
     !
     call k_add((/ap,ep,0._SP/),'X1')
     call k_add(SPECIAL_k%pt(9,:)+(/0._SP,0._SP,cp/),'A1' )
     !
   case('FC-ORTHOROMBIC (b>a)')
     !
     ap=pi/a(1,1)
     bp=pi/a(1,2)
     cp=pi/a(3,3)
     !
     call orthorombic_lengths(ap,bp)
     !
     call k_add((/0._SP,0._SP,0._SP/),'G' ) 
     call k_add(SPECIAL_k%pt(1,:)+(/0._SP,0._SP,cp/),'Z' )
     !
     call k_add((/fp,0._SP,0._SP/),'X' )
     call k_add(SPECIAL_k%pt(3,:)+(/0._SP,0._SP,cp/),'A' )
     !
     call k_add((/0._SP,bp,0._SP/),'Y' )
     call k_add(SPECIAL_k%pt(5,:)+(/0._SP,0._SP,cp/),'T' )
     !
     call k_add((/ap/2._SP,bp/2._SP,0._SP/),'S' )
     call k_add(SPECIAL_k%pt(7,:)+(/0._SP,0._SP,cp/),'R' )
     !
     call k_add((/ep,bp,0._SP/),'X1' )
     call k_add(SPECIAL_k%pt(9,:)+(/0._SP,0._SP,cp/),'A1' )
     !
     call k_add(SPECIAL_k%pt(3,:)+SPECIAL_k%pt(5,:),'X2' )
     call k_add(SPECIAL_k%pt(4,:)-2.*SPECIAL_k%pt(3,:),'A2' )
     call k_add(SPECIAL_k%pt(4,:)-2.*SPECIAL_k%pt(2,:),'A3' )
     call k_add(SPECIAL_k%pt(4,:)+SPECIAL_k%pt(5,:),'A4' )
     call k_add(SPECIAL_k%pt(13,:)+SPECIAL_k%pt(5,:),'A5' )
     call k_add(SPECIAL_k%pt(12,:)+SPECIAL_k%pt(5,:),'A6' )
     !
     ! X=+, Z=- quadrant
     !
     call k_ORTHO_b_gt_a_add( (/0.88_SP,0._SP  /),"XYpm1 XYp1" )
     call k_ORTHO_b_gt_a_add( (/0.88_SP,0._SP  /),"XYpm1" )
     call k_ORTHO_b_gt_a_add( (/0.50_SP,0._SP  /),"XYpp1 XYpm2" )
     call k_ORTHO_b_gt_a_add( (/0.50_SP,0.26_SP/),"XYpm3 Ypm3 Gpm2 Gpp1" )
     call k_ORTHO_b_gt_a_add( (/0.88_SP,0.26_SP/),"XYpm4" )
     !
     call k_ORTHO_b_gt_a_add( (/0.62_SP,0.26_SP/),"Gp1" )
     call k_ORTHO_b_gt_a_add( (/0.72_SP,0.26_SP/),"Gpm1" )
     call k_ORTHO_b_gt_a_add( (/0.50_SP,0.50_SP/),"Gpm3 Gmp1 Gmm2" )
     !
     call k_ORTHO_b_gt_a_add( (/0.62_SP,0.1_SP/),"Yp1" )
     call k_ORTHO_b_gt_a_add( (/0.62_SP,0.16_SP/),"Ypm1" )
     call k_ORTHO_b_gt_a_add( (/0.50_SP,0.16_SP/),"Ypp1 Ypm2" )
     call k_ORTHO_b_gt_a_add( (/0.62_SP,0.26_SP/),"Ypm4" )
     !
     ! X=+, Z=+ quadrant
     !
     call k_ORTHO_b_gt_a_add( (/0.12_SP,0._SP  /),"XYpp2 XYp2" )
     call k_ORTHO_b_gt_a_add( (/0.12_SP,0.26_SP/),"XYpp3 XYp3" )
     !
     call k_ORTHO_b_gt_a_add( (/0.28_SP,0.26_SP/),"Gpp2" )
     call k_ORTHO_b_gt_a_add( (/0.38_SP,0.50_SP/),"Gp3 Gm2" )
     call k_ORTHO_b_gt_a_add( (/0.28_SP,0.50_SP/),"Gpp3 Gmp2" )
     !
     call k_ORTHO_b_gt_a_add( (/0.38_SP,0.1_SP/),"Yp2" )
     call k_ORTHO_b_gt_a_add( (/0.38_SP,0.16_SP/),"Ypp2" )
     call k_ORTHO_b_gt_a_add( (/0.38_SP,0.26_SP/),"Ypp3 Yp3 Gp2" )
     !
     ! X=-, Z=- quadrant
     !
     call k_ORTHO_b_gt_a_add( (/0.88_SP,0.74_SP/),"XYmm1 XYm1" )
     call k_ORTHO_b_gt_a_add( (/0.50_SP,0.74_SP/),"Gmm3 XYmp1 Ymp1 Ymm2 XYmm2" )
     call k_ORTHO_b_gt_a_add( (/0.50_SP,1._SP  /),"XYmm3" )
     call k_ORTHO_b_gt_a_add( (/0.88_SP,1._SP  /),"XYmm4" )
     !
     call k_ORTHO_b_gt_a_add( (/0.62_SP,0.50_SP/),"Gm1" )
     call k_ORTHO_b_gt_a_add( (/0.72_SP,0.50_SP/),"Gmm1" )
     call k_ORTHO_b_gt_a_add( (/0.50_SP,0.84_SP/),"Ymm3" )
     call k_ORTHO_b_gt_a_add( (/0.72_SP,0.74_SP/),"Gmm4" )
     !
     call k_ORTHO_b_gt_a_add( (/0.62_SP,0.74_SP/),"Ymm1 Ym1" )
     call k_ORTHO_b_gt_a_add( (/0.62_SP,0.84_SP/),"Ymm4" )
     !
     ! X=-, Z=+ quadrant
     !
     call k_ORTHO_b_gt_a_add( (/0.12_SP,1._SP  /),"XYmp3 XYm3" )
     call k_ORTHO_b_gt_a_add( (/0.12_SP,0.74_SP/),"XYmp2 XYm2" )
     call k_ORTHO_b_gt_a_add( (/0.28_SP,0.74_SP/),"Gmp3" )
     call k_ORTHO_b_gt_a_add( (/0.38_SP,0.9_SP/),"Ym3" )
     call k_ORTHO_b_gt_a_add( (/0.38_SP,0.84_SP/),"Ymp3" )
     call k_ORTHO_b_gt_a_add( (/0.38_SP,0.74_SP/),"Ymp2 Ym2 Gm3" )
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
     call k_add((/0._SP,0._SP,0._SP/),'G' )
     call k_add((/0.5_SP,0._SP,0._SP/),'X' )
     call k_add((/0.5_SP,0.5_SP,0._SP/),'M' )
     call k_add((/0.5_SP,0.5_SP,0.5_SP/),'R' )
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
     call k_add((/0._SP,0._SP,0._SP/),'G' )
     call k_add((/0._SP ,0.5_SP,0._SP/),'M' )
     !
     ! In the old version also the point (/1./3.,1./3.,0./) was assigned to K. To be checked.
     !
     call k_add((/1._SP/3._SP,1._SP/3._SP,0._SP/),'K' )
     call k_add((/-1._SP/3._SP,-1._SP/3._SP,0._SP/),'Kp' )
     call k_add((/0._SP ,0._SP  ,0.5_SP/),'A' )
     call k_add((/0._SP,0.5_SP,0.5_SP/),'L' )
     call k_add((/1._SP/3._SP,1._SP/3._SP,0.5_SP/),'H' )
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
     call msg("s","Special points ["//trim(coo_out)//"]")
     do ik=1,SPECIAL_k%nbz
       k_l=SPECIAL_k%pt(ik,:)
       call K_transform(k_l,'cc')
       write (dumb_ch,'(a,i3,3f12.7,2x,a)') "k#",ik,k_l,trim(SPECIAL_k_label(ik))
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
   subroutine k_add( coordinates,labels_string )
    !
    use stderr,              ONLY:STRING_split
    !
    implicit none
    !
    integer           :: i_l
    real(SP)          :: coordinates(3)
    character(*)      :: labels_string
    character(schlen) :: label(10)
    !
    call STRING_split(labels_string,label," ")
    do i_l=1,10
      if (len_trim(label(i_l))==0) cycle
      SPECIAL_k%nbz=SPECIAL_k%nbz+1
      SPECIAL_k%pt(SPECIAL_k%nbz,:)=coordinates
      SPECIAL_k_label(SPECIAL_k%nbz)=trim(label(i_l))
    enddo
    !
   end subroutine
   !
   subroutine k_ORTHO_b_gt_a_add( coordinates,labels_string )
    !
    use stderr,              ONLY:STRING_split
    !
    implicit none
    !
    integer           :: i_l
    real(SP)          :: coordinates(2)
    character(*)      :: labels_string
    character(schlen) :: label(10)
    !
    call STRING_split(labels_string,label," ")
    do i_l=1,10
      if (len_trim(label(i_l))==0) cycle
      SPECIAL_k%nbz=SPECIAL_k%nbz+1
      SPECIAL_k%pt(SPECIAL_k%nbz,:)=K_ORTHO_el(coordinates(1),coordinates(2))
      SPECIAL_k_label(SPECIAL_k%nbz)=trim(label(i_l))
    enddo
    !
   end subroutine
   !
   function K_ORTHO_el(alpha,beta)
    real(SP) :: K_ORTHO_el(3),alpha,beta
    integer           :: i_l,iA4,iA5,iA6
    do i_l=1,SPECIAL_k%nbz
      if (trim(SPECIAL_k_label(i_l))=="A4") iA4=i_l
      if (trim(SPECIAL_k_label(i_l))=="A5") iA5=i_l
      if (trim(SPECIAL_k_label(i_l))=="A6") iA6=i_l
    enddo
    K_ORTHO_el=(1.-alpha-beta)*SPECIAL_k%pt(iA4,:)+alpha*SPECIAL_k%pt(iA5,:)+beta*SPECIAL_k%pt(iA6,:)
   end function
   !
end subroutine k_special
