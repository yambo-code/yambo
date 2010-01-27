!
! Copyright (C) 2000-2010 A. Marini and the YAMBO team 
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
subroutine ps_convert(rvar,ivar,v_name)
 !
 use pars,  ONLY:SP,lchlen,schlen
 use units
 use it_m,  ONLY: nflines,flines,ni1v,ni2v,ni3v,nr1v,nr2v,nc1v,&
&                 nr3v,i1vars,i2vars,i3vars,r1vars,r2vars,r3vars,c1vars,&
&                 B_unit,Time_unit,I_unit,Angle_unit
 implicit none
 !
 real(SP):: rvar
 integer :: ivar
 character(*)::v_name
 ! 
 ! Work Space
 !
 integer :: i1,ipos           
 character(lchlen) :: v_line  
 character(schlen) :: v_s_unit
 logical :: var_is_time,var_is_B,var_is_intensity,var_is_angle
 !
 ! First I search for the variable name in the input file 
 !
 v_line=' '
 v_s_unit=' '
 do i1=1,nflines
   if (index(trim(flines(i1)),trim(v_name))/=0) v_line=flines(i1)
   if (index(v_line,'%')/=0) v_line=flines(i1+1)
 enddo
 if (len_trim(v_line)==0) return
 !
 ! Then the Unit is read and value scaled.
 ! At the same time I have to redefine the variable Unit
 !
 ipos=index(trim(v_line),trim(v_name))+len_trim(v_name)+1
 v_line=trim(v_line(ipos:))
 !
 ! Get the standard unit (if any)
 !
 call set_itp_unit(v_s_unit,-1)
 !
 ! No unit no party
 !
 if (len_trim(v_s_unit)==0) return
 !
 ! check if the var is a Time variable (incompatible with energy types)
 !
 var_is_time     =trim(v_s_unit)==Time_unit
 var_is_B        =trim(v_s_unit)==B_unit
 var_is_angle    =trim(v_s_unit)==Angle_unit
 var_is_intensity=trim(v_s_unit)==I_unit
 !
 if (ivar>0) then
   !
   ! INTEGERS
   !
   if (index(v_line,'RL')/=0.or.index(v_line,'rl')/=0) then
     call Gclose(ivar,'fRL')
     call set_itp_unit('RL',1)
   else if (index(v_line,'mha')/=0.or.index(v_line,'mHa')/=0) then
     call Gclose(ivar,'fmHa')
     call set_itp_unit('mHa',1)
   else if (index(v_line,'Ha')/=0.or.index(v_line,'ha')/=0) then
     call Gclose(ivar,'fHa')
     call set_itp_unit('Ha',1)
   else if (index(v_line,'mry')/=0.or.index(v_line,'mRy')/=0) then
     call Gclose(ivar,'fmRy')
     call set_itp_unit('mRy',1)
   else if (index(v_line,'Ry')/=0.or.index(v_line,'ry')/=0) then
     call Gclose(ivar,'fRy')
     call set_itp_unit('Ry',1)
   endif
   return
   !
 else if (abs(rvar)>1.E-10.and..not.var_is_time.and..not.var_is_B &
&         .and..not.var_is_intensity.and..not.var_is_angle) then
   !
   ! REALS (ENERGY)
   !
   if (index(v_line,'eV')/=0.or.index(v_line,'ev')/=0) then
    if (index(v_line,'meV')/=0.or.index(v_line,'mev')/=0) then
      rvar=rvar/HARTREE/1000.
      call set_itp_unit('meV',1)
    else
      rvar=rvar/HARTREE
      call set_itp_unit('eV',1)
    endif
    return
   endif
   if (index(v_line,'Ry')/=0.or.index(v_line,'ry')/=0) then
     if (index(v_line,'mRy')/=0.or.index(v_line,'mry')/=0) then
       rvar=rvar/1000./2.
       call set_itp_unit('mRy',1)
     else
       call set_itp_unit('Ry',1)
       rvar=rvar/2.
     endif
     return
   endif
   if (index(v_line,'Ha')/=0.or.index(v_line,'ha')/=0) then
     if (index(v_line,'mHa')/=0.or.index(v_line,'mha')/=0) then
       rvar=rvar/1000.
       call set_itp_unit('mHa',1)
     else
       call set_itp_unit('Ha',1)
       rvar=rvar
     endif
     return
   endif
   if (index(v_line,'K')/=0) then
     rvar=rvar/11604./HARTREE
     call set_itp_unit('Kn',1)
     return
   endif
   if (any((/index(v_line,'thz')/=0,index(v_line,'THz')/=0/))) then
     rvar=rvar/HA2THZ
     call set_itp_unit('THz',1)
     return
   endif
   if (any((/index(v_line,'ghz')/=0,index(v_line,'GHz')/=0/))) then
     rvar=rvar/HA2GHZ
     call set_itp_unit('GHz',1)
     return
   endif
   !
 else if (abs(rvar)>1.E-10.and.var_is_time) then
   !
   ! REALS (TIME)
   !
   if (index(v_line,'ps')/=0) then
     !
     ! 1 AUT= 1 PS * PSAUT
     !
     rvar=rvar*PS2AUT
     call set_itp_unit('ps',1)
     return
   else 
     !
     ! 1 AUT= 1 FS * FSAUT
     !
     rvar=rvar*FS2AUT
     call set_itp_unit('fs',1)
     return
   endif
   !
   ! If nothing has been done and the real has a unit I assume it is in eV!
   !
   rvar=rvar/HARTREE
   !
 else if (var_is_B) then
   !
   ! MAGNETIC Fields
   !
   if (index(v_line,'T')/=0) then
     rvar=rvar*Tesla2AU
     call set_itp_unit('T',1)
     return
   endif 
   !
 else if (var_is_angle) then
   !
   ! Angles (among B and z direction, used by MAGNETIC project)
   ! 
   if (index(v_line,'deg')/=0) then
     rvar=rvar*radian
     call set_itp_unit('deg',1)
     return
   else
     call set_itp_unit('rad',1)
     return
   endif 
   !
 else if (var_is_intensity) then
   !
   ! Laser Intensity 
   !
   if (index(v_line,'kWLm2')/=0) then
     rvar=rvar*W2Au
     call set_itp_unit('kWLm2',1)
     return
   endif 
 endif
 !
 contains 
   !
   subroutine set_itp_unit(unit_,im)
     integer :: im
     character(*)::unit_
     integer :: i1!ws
     do i1=1,ni1v
       if (trim(i1vars(i1,1))==trim(v_name).and.im>0) i1vars(i1,3)=unit_
       if (trim(i1vars(i1,1))==trim(v_name).and.im<0) unit_=i1vars(i1,3)
     enddo
     do i1=1,ni2v
       if (trim(i2vars(i1,1))==trim(v_name).and.im>0) i2vars(i1,3)=unit_
       if (trim(i2vars(i1,1))==trim(v_name).and.im<0) unit_=i2vars(i1,3)
     enddo
     do i1=1,ni3v
       if (trim(i3vars(i1,1))==trim(v_name).and.im>0) i3vars(i1,3)=unit_
       if (trim(i3vars(i1,1))==trim(v_name).and.im<0) unit_=i3vars(i1,3)
     enddo
     do i1=1,nr1v
       if (trim(r1vars(i1,1))==trim(v_name).and.im>0) r1vars(i1,3)=unit_
       if (trim(r1vars(i1,1))==trim(v_name).and.im<0) unit_=r1vars(i1,3)
     enddo
     do i1=1,nr2v
       if (trim(r2vars(i1,1))==trim(v_name).and.im>0) r2vars(i1,3)=unit_
       if (trim(r2vars(i1,1))==trim(v_name).and.im<0) unit_=r2vars(i1,3)
     enddo
     do i1=1,nr3v
       if (trim(r3vars(i1,1))==trim(v_name).and.im>0) r3vars(i1,3)=unit_
       if (trim(r3vars(i1,1))==trim(v_name).and.im<0) unit_=r3vars(i1,3)
     enddo
     do i1=1,nc1v
       if (trim(c1vars(i1,1))==trim(v_name).and.im>0) c1vars(i1,3)=unit_
       if (trim(c1vars(i1,1))==trim(v_name).and.im<0) unit_=c1vars(i1,3)
     enddo
   end subroutine
   !
end subroutine
