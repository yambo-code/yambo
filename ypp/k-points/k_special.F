!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine k_special(LIST_only)
 !
 ! Special k-points. Output in IKU.
 ! 
 ! The strategy in this routine is to define the list of special points in IKU 
 ! by specifing a set of reference b1,b2,b3. This in order to prevent 
 ! problems when, internally, yambo adopts different forms of them.
 !
 ! To get the coordinates one possibility is to find the space gro
 !
 ! 1. find the space group: e.g. https://materialsproject.org/materials/
 ! 2. find the special points list: e.g. https://www.cryst.ehu.es/cryst/get_kvec.html
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
 select case (trim(lattice))
   !
   case('FCC')
     !
     trial_b(1,:)=(/-1., 1., 1./)*2.*pi/alat(1)
     trial_b(2,:)=(/ 1.,-1., 1./)*2.*pi/alat(1)
     trial_b(3,:)=(/ 1., 1.,-1./)*2.*pi/alat(1)
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
     trial_b(1,:)=(/ 0._SP, 1._SP, 1._SP/)*2._SP*pi/alat(1)
     trial_b(2,:)=(/ 1._SP, 0._SP, 1._SP/)*2._SP*pi/alat(1)
     trial_b(3,:)=(/ 1._SP, 0._SP,-1._SP/)*2._SP*pi/alat(1)
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
     call k_add((/0._SP,0._SP,0._SP/),'G' ) ! 1 
     call k_add(SPECIAL_k%pt(1,:)+(/0._SP,0._SP,cp/),'Z' )! 2
     !
     call k_add((/fp,0._SP,0._SP/),'X' ) ! 3
     call k_add(SPECIAL_k%pt(3,:)+(/0._SP,0._SP,cp/),'A' ) ! 4
     !
     call k_add((/0._SP,bp,0._SP/),'Y' ) ! 5
     call k_add(SPECIAL_k%pt(5,:)+(/0._SP,0._SP,cp/),'T' ) ! 6
     !
     call k_add((/ap/2._SP,bp/2._SP,0._SP/),'S' ) ! 7
     call k_add(SPECIAL_k%pt(7,:)+(/0._SP,0._SP,cp/),'R' ) ! 8
     !
     call k_add((/ep,bp,0._SP/),'X1' ) ! 9 
     call k_add(SPECIAL_k%pt(9,:)+(/0._SP,0._SP,cp/),'A1' ) ! 10
     !
     call k_add(SPECIAL_k%pt(3,:)+SPECIAL_k%pt(5,:),'X2' ) ! 11
     call k_add(SPECIAL_k%pt(4,:)-2.*SPECIAL_k%pt(3,:),'A2' ) ! 12
     call k_add(SPECIAL_k%pt(4,:)-2.*SPECIAL_k%pt(2,:),'A3' )
     call k_add(SPECIAL_k%pt(4,:)+SPECIAL_k%pt(5,:),'A4' )
     call k_add(SPECIAL_k%pt(13,:)+SPECIAL_k%pt(5,:),'A5' )
     call k_add(SPECIAL_k%pt(12,:)+SPECIAL_k%pt(5,:),'A6' )
     call k_add(SPECIAL_k%pt(4,:)-SPECIAL_k%pt(5,:),'A7' )
     !
     call k_add(SPECIAL_k%pt(5,:)-(/0._SP,0._SP,cp/),'Tp' ) 
     !
     ! Y direction
     !
     call k_add(SPECIAL_k%pt(3,:)-SPECIAL_k%pt(5,:), 'X3' ) ! X-Y
     call k_add(-SPECIAL_k%pt(3,:)-SPECIAL_k%pt(5,:),'X4' ) ! -X-Y
     call k_add(-SPECIAL_k%pt(5,:),'Yp' ) ! -Y
     !
     ! XY plane 
     !==========
     call k_FC_compose_and_add( (/0._SP,0.88_SP   /),"xy","YZp1" )
     call k_FC_compose_and_add( (/0._SP,0.12_SP   /),"xy","YZp2" )
     call k_FC_compose_and_add( (/0.26_SP,0.12_SP /),"xy","YZp3" )
     call k_FC_compose_and_add( (/0.74_SP,0.88_SP /),"xy","YZm1" )
     call k_FC_compose_and_add( (/0.74_SP,0.12_SP /),"xy","YZm2" )
     call k_FC_compose_and_add( (/1._SP,0.12_SP   /),"xy","YZm3" )
     !
     ! YZ plane 
     !==========
     call k_FC_compose_and_add( (/0.88_SP,0._SP   /),"yz","XZp1" )
     call k_FC_compose_and_add( (/0.12_SP,0._SP   /),"yz","XZp2" )
     call k_FC_compose_and_add( (/0.12_SP,0.26_SP /),"yz","XZp3" )
     call k_FC_compose_and_add( (/0.88_SP,0.74_SP /),"yz","XZm1" )
     call k_FC_compose_and_add( (/0.12_SP,0.74_SP /),"yz","XZm2" )
     call k_FC_compose_and_add( (/0.12_SP,1._SP   /),"yz","XZm3" )
     !
     ! XZ plane 
     !==========
     !
     ! X=+, Z=- quadrant
     !
     call k_FC_compose_and_add( (/0.88_SP,0._SP  /),"xz","XYpm1 XYp1" )
     call k_FC_compose_and_add( (/0.88_SP,0._SP  /),"xz","XYpm1" )
     call k_FC_compose_and_add( (/0.50_SP,0._SP  /),"xz","XYpp1 XYpm2" )
     call k_FC_compose_and_add( (/0.50_SP,0.26_SP/),"xz","XYpm3 Ypm3 Gpm2 Gpp1" )
     call k_FC_compose_and_add( (/0.88_SP,0.26_SP/),"xz","XYpm4" )
     !
     call k_FC_compose_and_add( (/0.62_SP,0.26_SP/),"xz","Gp1" )
     call k_FC_compose_and_add( (/0.72_SP,0.26_SP/),"xz","Gpm1" )
     call k_FC_compose_and_add( (/0.50_SP,0.50_SP/),"xz","Gpm3 Gmp1 Gmm2" )
     !
     call k_FC_compose_and_add( (/0.62_SP,0.1_SP/),"xz","Yp1" )
     call k_FC_compose_and_add( (/0.62_SP,0.16_SP/),"xz","Ypm1" )
     call k_FC_compose_and_add( (/0.50_SP,0.16_SP/),"xz","Ypp1 Ypm2" )
     call k_FC_compose_and_add( (/0.62_SP,0.26_SP/),"xz","Ypm4" )
     !
     ! X=+, Z=+ quadrant
     !
     call k_FC_compose_and_add( (/0.12_SP,0._SP  /),"xz","XYpp2 XYp2" )
     call k_FC_compose_and_add( (/0.12_SP,0.26_SP/),"xz","XYpp3 XYp3" )
     !
     call k_FC_compose_and_add( (/0.28_SP,0.26_SP/),"xz","Gpp2" )
     call k_FC_compose_and_add( (/0.38_SP,0.50_SP/),"xz","Gp3 Gm2" )
     call k_FC_compose_and_add( (/0.28_SP,0.50_SP/),"xz","Gpp3 Gmp2" )
     !
     call k_FC_compose_and_add( (/0.38_SP,0.1_SP/),"xz","Yp2" )
     call k_FC_compose_and_add( (/0.38_SP,0.16_SP/),"xz","Ypp2" )
     call k_FC_compose_and_add( (/0.38_SP,0.26_SP/),"xz","Ypp3 Yp3 Gp2" )
     !
     ! X=-, Z=- quadrant
     !
     call k_FC_compose_and_add( (/0.88_SP,0.74_SP/),"xz","XYmm1 XYm1" )
     call k_FC_compose_and_add( (/0.50_SP,0.74_SP/),"xz","Gmm3 XYmp1 Ymp1 Ymm2 XYmm2" )
     call k_FC_compose_and_add( (/0.50_SP,1._SP  /),"xz","XYmm3" )
     call k_FC_compose_and_add( (/0.88_SP,1._SP  /),"xz","XYmm4" )
     !
     call k_FC_compose_and_add( (/0.62_SP,0.50_SP/),"xz","Gm1" )
     call k_FC_compose_and_add( (/0.72_SP,0.50_SP/),"xz","Gmm1" )
     call k_FC_compose_and_add( (/0.50_SP,0.84_SP/),"xz","Ymm3" )
     call k_FC_compose_and_add( (/0.72_SP,0.74_SP/),"xz","Gmm4" )
     !
     call k_FC_compose_and_add( (/0.62_SP,0.74_SP/),"xz","Ymm1 Ym1" )
     call k_FC_compose_and_add( (/0.62_SP,0.84_SP/),"xz","Ymm4" )
     !
     ! X=-, Z=+ quadrant
     !
     call k_FC_compose_and_add( (/0.12_SP,1._SP  /),"xz","XYmp3 XYm3" )
     call k_FC_compose_and_add( (/0.12_SP,0.74_SP/),"xz","XYmp2 XYm2" )
     call k_FC_compose_and_add( (/0.28_SP,0.74_SP/),"xz","Gmp3" )
     call k_FC_compose_and_add( (/0.38_SP,0.9_SP/),"xz","Ym3" )
     call k_FC_compose_and_add( (/0.38_SP,0.84_SP/),"xz","Ymp3" )
     call k_FC_compose_and_add( (/0.38_SP,0.74_SP/),"xz","Ymp2 Ym2 Gm3" )
     !
   case('CUB')
     !
     trial_b(1,:)=(/ 1., 0., 0./)*2.*pi/alat(1)
     trial_b(2,:)=(/ 0., 1., 0./)*2.*pi/alat(1)
     trial_b(3,:)=(/ 0., 0., 1./)*2.*pi/alat(1)
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
     ! https://www.cryst.ehu.es/cgi-bin/cryst/programs/nph-kv-list
     !
     trial_b(1,:)=(/1., 0., 0./)
     trial_b(2,:)=(/0., 1., 0./)
     trial_b(3,:)=(/0., 0., 1./)
     !
     call k_add((/0.0_SP,0.0_SP,0.0_SP/),'G' )
     call k_add((/0.0_SP,0.0_SP,0.5_SP/),'A' )
     !
     call k_expand_plane((/0.5_SP,0.25_SP,0.0_SP/),"xy","M")
     call k_expand_plane((/0.5_SP,0.25_SP,0.5_SP/),"xy","L")
     call k_expand_plane((/1./3._SP,0.5_SP,0.0_SP/),"xy","K")
     call k_expand_plane((/1./3._SP,0.5_SP,0.5_SP/),"xy","H")
     !
     do ik=1,SPECIAL_k%nbz
       call c2a(b_in=trial_b,v_in=SPECIAL_k%pt(ik,:),mode='ki2c')
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
   subroutine k_expand_plane(base,plane,label)
    !
    implicit none
    !
    real(SP)     :: base(3)
    character(*) :: plane
    character(*) :: label
    !
    ! Work Space
    !
    integer      :: i1,i2
    character(2) :: chpm
    real(SP)     :: v(3)
    !
    do i1=-1,1,2
      do i2=-1,1,2
        if (plane=="xy") v=(/i1*base(1),i2*base(2),   base(3)/)
        if (plane=="xz") v=(/i1*base(1),   base(2),i2*base(3)/)
        if (plane=="yz") v=(/   base(1),i1*base(2),i2*base(3)/)
        chpm="p"
        if (i1<0) chpm="m"
        chpm=trim(chpm)//"p"
        if (i2<0) chpm=trim(chpm)//"m"
        if (i1==i2.and.i1==1) chpm=" "
        SPECIAL_k%nbz=SPECIAL_k%nbz+1
        SPECIAL_k%pt(SPECIAL_k%nbz,:)=v
        SPECIAL_k_label(SPECIAL_k%nbz)=label//trim(chpm)
      enddo
    enddo
    !
   end subroutine
   !
   subroutine k_add(coordinates,labels_string)
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
   subroutine k_FC_compose_and_add(coordinates,plane,labels_string)
    !
    use stderr,              ONLY:STRING_split
    !
    implicit none
    !
    integer           :: i_l
    real(SP)          :: coordinates(2)
    character(*)      :: labels_string
    character(2)      :: plane
    character(schlen) :: label(10)
    !
    call STRING_split(labels_string,label," ")
    do i_l=1,10
      if (len_trim(label(i_l))==0) cycle
      SPECIAL_k%nbz=SPECIAL_k%nbz+1
      SPECIAL_k%pt(SPECIAL_k%nbz,:)=k_FC_plane(coordinates(1),coordinates(2),plane)
      SPECIAL_k_label(SPECIAL_k%nbz)=trim(label(i_l))
    enddo
    !
   end subroutine
   !
   function k_FC_plane(alpha,beta,plane)
    real(SP) :: k_FC_plane(3),alpha,beta
    integer           :: i_l,iO,iX,iY
    character(2)      :: plane
    do i_l=1,SPECIAL_k%nbz
      if (plane=="xz") then
        if (trim(SPECIAL_k_label(i_l))=="A4") iO=i_l
        if (trim(SPECIAL_k_label(i_l))=="A5") iX=i_l
        if (trim(SPECIAL_k_label(i_l))=="A6") iY=i_l
      else if (plane=="xy") then
        if (trim(SPECIAL_k_label(i_l))=="A4") iO=i_l
        if (trim(SPECIAL_k_label(i_l))=="A6") iX=i_l
        if (trim(SPECIAL_k_label(i_l))=="A7") iY=i_l
      else if (plane=="yz") then
        if (trim(SPECIAL_k_label(i_l))=="A4") iO=i_l
        if (trim(SPECIAL_k_label(i_l))=="A7") iX=i_l
        if (trim(SPECIAL_k_label(i_l))=="A5") iY=i_l
      endif
    enddo
    k_FC_plane=(1.-alpha-beta)*SPECIAL_k%pt(iO,:)+alpha*SPECIAL_k%pt(iX,:)+beta*SPECIAL_k%pt(iY,:)
   end function
   !
end subroutine k_special
