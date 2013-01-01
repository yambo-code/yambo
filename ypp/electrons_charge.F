! 
! Copyright (C) 2000-2013 E. Cannuccia and the YAMBO team
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
subroutine Zcharge(i_qp,iband,ikpt,sp_wf,charge_region)
!
! I want to associate a charge to different regions of the monomer
!      H2                    
!      |  -- > cylinder    H1=atom_pos(:,1,1)
!  C2  o  -- > sphere      H2=atom_pos(:,2,1)
!    //                    C1=atom_pos(:,1,2)
!C1 o                      C2=atom_pos(:,2,2)
!   |
!  H1
!
 use pars,           ONLY:SP
 use D_lattice,      ONLY:a,atom_pos
 use vec_operate,    ONLY:v_norm
 use FFT_m,          ONLY:fft_size,fft_dim
 !
 implicit none
 !
 integer               :: i_qp,iband,ikpt
 real(SP)              :: sp_wf(fft_size) 
 real(SP)              :: charge_region(7)
 !
 ! Work Space
 !
 integer               :: i1,i2,i3,ir
 real(SP)              :: el_pos(3), Charge2Bond,Charge1Bond,ChargeCHbond,ChargePiOrbital
 real(SP)              ::  chargeCHPI,charge2BPI,charge1BPI
 real(SP), parameter   :: C2B_SIDE_xy=1.,C2B_SIDE_z=1.5,C1B_SIDE_xy=1.,C1B_SIDE_z=1.5, CH_SIDE_xy=1.,CH_SIDE_z=1.5,PI_SIDE=1.
 logical               :: is_2B,is_1B,is_CH,is_PI
 !
 Charge2Bond=0.0
 Charge1Bond=0.0
 ChargeCHbond=0.0
 ChargePiOrbital=0.0
 chargeCHPI=0.
 charge2BPI=0.
 charge1BPI=0.
 !
 !sp_wf=0.
 !
 if (i_qp==1) write (*,'(/a)') '  K  B 2bond  1bond  CH     Pi     SUM    2B+PI  1B+PI   CH+PI'
 !
 write (1000,*) atom_pos(:,1,1)
 write (1000,*) atom_pos(:,1,2)
 write (1000,*) atom_pos(:,2,1)
 write (1000,*) atom_pos(:,2,2)
 write (1000,*) (/a(1,1) ,a(2,2)/2._SP,a(3,3)/2._SP/)
 write (1000,*) (/0._SP  ,a(2,2)/2._SP,a(3,3)/2._SP/) 
 !
 do i1 = 0, fft_dim(1)-1         ! x 
   do i2 = 0, fft_dim(2)-1       ! y
     do i3 = 0, fft_dim(3)-1     ! z
       ir = 1 + i1 + i2*fft_dim(1) + i3*fft_dim(1)*fft_dim(2)
       el_pos(:) = i1*a(1,:)/fft_dim(1) +&
&                  i2*a(2,:)/fft_dim(2) +&
&                  i3*a(3,:)/fft_dim(3)
       !  
       is_2B=.FALSE.
       is_1B=.FALSE.
       is_CH=.FALSE.
       is_PI=.FALSE.
       !
       ! C-C
       !
       is_1B=is_in_tube( atom_pos(:,2,2), (/a(1,1),a(2,2)/2._SP,a(3,3)/2._SP/) ,&
&                        C1B_SIDE_xy, C1B_SIDE_z, el_pos,.TRUE.,.FALSE.).or.&
&            is_in_tube( atom_pos(:,1,2), (/0._SP ,a(2,2)/2._SP,a(3,3)/2._SP/) ,& 
&                        C1B_SIDE_xy, C1B_SIDE_z, el_pos,.TRUE.,.FALSE.)
       if (is_1B) Charge1Bond=Charge1Bond+sp_wf(ir)
       !
       ! C=C
       !
       is_2B=is_in_tube( atom_pos(:,1,2), atom_pos(:,2,2) , C2B_SIDE_xy, C2B_SIDE_z, el_pos,.FALSE.,.FALSE.)
       if (is_2B) Charge2Bond=Charge2Bond+sp_wf(ir)
       !
       ! C-H
       !
       is_CH=is_in_tube( atom_pos(:,2,2), atom_pos(:,2,1) , CH_SIDE_xy, CH_SIDE_z, el_pos,.FALSE.,.FALSE.).or.&
&            is_in_tube( atom_pos(:,1,2), atom_pos(:,1,1) , CH_SIDE_xy, CH_SIDE_z, el_pos,.FALSE.,.FALSE.)
       if (is_CH) ChargeCHbond=ChargeCHbond+sp_wf(ir)
       !
       ! PI
       !
       is_PI=is_in_tube(atom_pos(:,2,2), (/atom_pos(1,2,2),atom_pos(2,2,2),a(3,3)/), PI_SIDE, PI_SIDE, el_pos,.TRUE.,.TRUE.).or.&
&            is_in_tube(atom_pos(:,1,2), (/atom_pos(1,1,2),atom_pos(2,1,2),a(3,3)/), PI_SIDE, PI_SIDE, el_pos,.TRUE.,.TRUE.)
       if (is_PI) ChargePIOrbital=ChargePIOrbital+sp_wf(ir)
       !
       if ( count( (/is_1B,is_2B,is_CH,is_PI/).eqv..TRUE. ) >  1) then
         if (all((/is_CH,is_PI/))) chargeCHPI=chargeCHPI+sp_wf(ir)
         if (all((/is_2B,is_PI/))) charge2BPI=charge2BPI+sp_wf(ir)
         if (all((/is_1B,is_PI/))) charge1BPI=charge1BPI+sp_wf(ir)
       endif
       !
     enddo
   enddo
 enddo
 !
 write (*,'(2I3,11F7.4)') ikpt,iband,Charge2Bond,Charge1Bond,ChargeCHbond,ChargePiOrbital,&
&                        Charge2Bond+Charge1Bond+ChargeCHbond+ChargePiOrbital,charge2BPI,charge1BPI,chargeCHPI 
 !
 contains
   !
   logical function is_in_tube(P1,P2,SIDE_xy,SIDE_z,r,skip_second_atom_cut,z_tube)
     use D_lattice,   ONLY:alat
     use vec_operate, ONLY:v_norm
     real(SP) :: P1(3),P2(3),SIDE_xy,SIDE_z,r(3)
     real(SP) :: depth
     logical  :: skip_second_atom_cut,z_tube
     !
     ! Workspace
     !
     real(SP) :: tube_projection,perp_projection,tube_length,distance,versor(3),r_m_P1(3),z_projection
     !
     is_in_tube =.TRUE.
     !
     versor=(P2-P1)/v_norm(P2-P1)
     r_m_P1=r-P1
     !
     if (.not.z_tube) then
       z_projection=abs(r_m_P1(3))
       tube_projection=r_m_P1(1)*versor(1)+r_m_P1(2)*versor(2)
       perp_projection=sqrt(v_norm(r_m_P1)**2.-z_projection**2.-tube_projection**2.)
     else
       tube_projection=abs(r_m_P1(3))
       z_projection=r_m_P1(1)*versor(1)+r_m_P1(2)*versor(2)
       perp_projection=sqrt(v_norm(r_m_P1)**2.-z_projection**2.-tube_projection**2.)
     endif
     !
     tube_length=v_norm(P2-P1)
     !
     if (tube_projection<0.or.(tube_projection>tube_length.and..not.skip_second_atom_cut)) then
       is_in_tube =.FALSE.
       return
     endif
     !
     if (perp_projection>SIDE_xy) then
       is_in_tube =.FALSE.
       return
     endif
     !
     !if (z_tube) return
     !
     depth=tube_length/5.
     !
     if (tube_projection<depth) then
       if ( perp_projection > tube_projection/depth*SIDE_xy) then 
         is_in_tube =.FALSE.
         return
       endif
       if ( z_projection > tube_projection/depth*SIDE_z) then 
         is_in_tube =.FALSE.
         return
       endif
     endif
     !
     if (tube_projection>tube_length-depth.and..not.skip_second_atom_cut) then
       if ( perp_projection > (tube_length-tube_projection)/depth*SIDE_xy) then 
         is_in_tube =.FALSE.
         return
       endif
       if ( z_projection > (tube_length-tube_projection)/depth*SIDE_z) then 
         is_in_tube =.FALSE.
         return
       endif
     endif
     !
   end function
end subroutine
