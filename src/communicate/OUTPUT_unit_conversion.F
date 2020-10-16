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
subroutine OUTPUT_unit_conversion(Ivar,Rvar,DESC,KIND,PARSER_call)
 !
 use pars,  ONLY:SP,lchlen,schlen
 use units, ONLY:AS2AUT,AU2VMm1,FS2AUT,HA2GHZ,HA2THZ,kWCMm22AU,&
&                Tesla2AU,DEG2RAD,HA2EV,PS2AUT,Megabyte,Gigabyte,Kilobyte
 use it_m,  ONLY:nflines,flines,ni1v,ni2v,ni3v,nr1v,nr2v,nc1v,&
&                nr3v,i1vars,i2vars,i3vars,r1vars,r2vars,r3vars,c1vars,&
&                Bfield_unit,Efield_unit,Time_unit,I_unit,Angle_unit
 use QP_m,  ONLY:Time_of
 implicit none
 !
 real(SP)    ,optional :: Rvar
 integer     ,optional :: Ivar
 character(*),optional :: DESC
 character(*),optional :: KIND
 logical     ,optional :: PARSER_call
 ! 
 ! Work Space
 !
 integer           :: i_line,ipos           
 character(lchlen) :: v_line  
 character(schlen) :: v_s_unit
 real(SP)          :: Rvar_tmp
 logical :: var_is_time,var_is_B,var_is_E,var_is_intensity,var_is_angle,is_multi_line,PARSER_call_
 !
 PARSER_call_=.FALSE.
 if (present(PARSER_call)) then
   PARSER_call_=.TRUE.
 endif
 !
 if (PARSER_call_) then
   !
   ! First I search for the variable name in the input file 
   !
   v_line       =' '
   v_s_unit     =' '
   is_multi_line=.FALSE.
   do i_line=1,nflines
     if (index(trim(flines(i_line)),trim(DESC))/=0) v_line=flines(i_line)
     if (index(v_line,'%')/=0) is_multi_line=.TRUE.
     if (index(v_line,'%')/=0) v_line=flines(i_line+1)
   enddo
   if (len_trim(v_line)==0) return
   !
   ! [12/7/2016] Bug-Fix by AM.
   !
   ! For scalar variables the variable name is removed so to avoid 
   ! wrong matches with unit strings.
   !
   if (.not.is_multi_line) then
     ipos=index(trim(v_line),trim(DESC))+len_trim(DESC)+1
     v_line=trim(v_line(ipos:))
   endif
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
   var_is_time     =any((/trim(v_s_unit)==Time_unit/))
   var_is_B        =trim(v_s_unit)==Bfield_unit
   var_is_E        =trim(v_s_unit)==Efield_unit
   var_is_angle    =any((/trim(v_s_unit)==Angle_unit/))
   var_is_intensity=trim(v_s_unit)==I_unit 
   !
 else
   !
   v_line  =DESC
   var_is_time     =any((/trim(KIND)=="Time"/))
   var_is_B        =trim(KIND)=="Bfield"
   var_is_E        =trim(KIND)=="Efield"
   var_is_angle    =any((/trim(KIND)=="Angle"/))
   var_is_intensity=trim(KIND)=="Int" 
   !
 endif
 !
 ! INTEGERS
 !-----------
 if (present(Ivar)) then
   !
   if (Ivar<=0) return
   !
   if (index(v_line,'RL')/=0.or.index(v_line,'rl')/=0) then
     call PARSER_close_G(Ivar,'fRL')
     call set_itp_unit('RL',1)
   else if (index(v_line,'mha')/=0.or.index(v_line,'mHa')/=0) then
     call PARSER_close_G(Ivar,'fmHa')
     call set_itp_unit('mHa',1)
   else if (index(v_line,'Ha')/=0.or.index(v_line,'ha')/=0) then
     call PARSER_close_G(Ivar,'fHa')
     call set_itp_unit('Ha',1)
   else if (index(v_line,'mry')/=0.or.index(v_line,'mRy')/=0) then
     call PARSER_close_G(Ivar,'fmRy')
     call set_itp_unit('mRy',1)
   else if (index(v_line,'Ry')/=0.or.index(v_line,'ry')/=0) then
     call PARSER_close_G(Ivar,'fRy')
     call set_itp_unit('Ry',1)
   else if (index(v_line,'eV')/=0.or.index(v_line,'ev')/=0) then
     call PARSER_close_G(Ivar,'feV')
     call set_itp_unit('eV',1)
   else if (index(v_line,'Gb')/=0) then
     Ivar=Ivar*nint(Gigabyte/Kilobyte)
     call set_itp_unit('Gb',1)
   else if (index(v_line,'Mb')/=0) then
     Ivar=Ivar*nint(Megabyte/Kilobyte)
     call set_itp_unit('Mb',1)
   endif
   !
   return
   !
 endif
 !
 if (.not.present(Rvar)) return
 !
 if (     PARSER_call_) Rvar_tmp=Rvar
 if (.not.PARSER_call_) Rvar_tmp=1._SP
 !
 ! REALS
 !-----------
 !
 if (all((/.not.var_is_time,.not.var_is_B,.not.var_is_E,.not.var_is_intensity,.not.var_is_angle/))) then
   !
   ! REALS (ENERGY)
   !
   if (index(v_line,'eV')/=0.or.index(v_line,'ev')/=0) then
    if (index(v_line,'eVm1')/=0) then
      Rvar_tmp=Rvar_tmp*HA2EV
    else if (index(v_line,'meV')/=0.or.index(v_line,'mev')/=0) then
      Rvar_tmp=Rvar_tmp/HA2EV/1000.
      call set_itp_unit('meV',1)
    else
      Rvar_tmp=Rvar_tmp/HA2EV
      call set_itp_unit('eV',1)
    endif
   endif
   if (index(v_line,'Ry')/=0.or.index(v_line,'ry')/=0) then
     if (index(v_line,'mRy')/=0.or.index(v_line,'mry')/=0) then
       Rvar_tmp=Rvar_tmp/1000./2.
       call set_itp_unit('mRy',1)
     else
       call set_itp_unit('Ry',1)
       Rvar_tmp=Rvar_tmp/2.
     endif
   endif
   if (index(v_line,'Ha')/=0.or.index(v_line,'ha')/=0) then
     if (index(v_line,'mHa')/=0.or.index(v_line,'mha')/=0) then
       Rvar_tmp=Rvar_tmp/1000.
       call set_itp_unit('mHa',1)
     else
       call set_itp_unit('Ha',1)
       Rvar_tmp=Rvar_tmp
     endif
   endif
   if (index(v_line,'K')/=0) then
     Rvar_tmp=Rvar_tmp/11604./HA2EV
     call set_itp_unit('Kn',1)
   endif
   if (any((/index(v_line,'thz')/=0,index(v_line,'THz')/=0/))) then
     Rvar_tmp=Rvar_tmp/HA2THZ
     call set_itp_unit('THz',1)
   endif
   if (any((/index(v_line,'ghz')/=0,index(v_line,'GHz')/=0/))) then
     Rvar_tmp=Rvar_tmp/HA2GHZ
     call set_itp_unit('GHz',1)
   endif
   if (any((/index(v_line,'mhz')/=0,index(v_line,'MHz')/=0/))) then
     Rvar_tmp=Rvar_tmp/HA2GHZ/1000.
     call set_itp_unit('MHz',1)
   endif
   if (.not.PARSER_call_) then
     if (any((/index(v_line,'as')/=0/))) then
       Rvar=Time_of(Rvar,"as")
     endif
     if (any((/index(v_line,'fs')/=0/))) then
       Rvar=Time_of(Rvar,"fs")
     endif
     if (any((/index(v_line,'ps')/=0/))) then
       Rvar=Time_of(Rvar,"ps")
     endif
     if (any((/index(v_line,'ns')/=0/))) then
       Rvar=Time_of(Rvar,"ns")
     endif
   endif
   !
 else if (var_is_time) then
   !
   ! REALS (TIME)
   !
   if (index(v_line,'ps')/=0) then
     !
     ! 1 AUT= 1 PS * PSAUT
     !
     Rvar_tmp=Rvar_tmp*PS2AUT
     call set_itp_unit('ps',1)
     !
   else if (index(v_line,'fs')/=0) then
     !
     ! 1 AUT= 1 FS * FSAUT
     !
     Rvar_tmp=Rvar_tmp*FS2AUT
     call set_itp_unit('fs',1)
     !
   else if (index(v_line,'as')/=0) then
     !
     ! 1 AUT= 1 AS * ASAUT
     !
     Rvar_tmp=Rvar_tmp*AS2AUT
     call set_itp_unit('as',1)
     !
   endif
   !
   ! If nothing has been done and the real has a unit I assume it is in eV!
   !
   Rvar_tmp=Rvar_tmp/HA2EV
   !
 else if (var_is_B) then
   !
   ! MAGNETIC Fields
   !
   if (index(v_line,'T')/=0) then
     Rvar_tmp=Rvar_tmp*Tesla2AU
     call set_itp_unit('T',1)
   endif
   !
 else if (var_is_E) then
   !
   ! Electric Fields
   !
   if (index(v_line,'V/mm')/=0) then
     Rvar_tmp=Rvar_tmp/AU2VMm1
     call set_itp_unit('V/mm',1)
   endif 
   !
 else if (var_is_angle) then
   !
   ! Angles (among B and z direction, used by MAGNETIC project)
   ! 
   if (index(v_line,'deg')/=0) then
     Rvar_tmp=Rvar_tmp*DEG2RAD
     call set_itp_unit('deg',1)
   else
     call set_itp_unit('rad',1)
   endif 
   !
 else if (var_is_intensity) then
   !
   ! Laser Intensity 
   !
   if (index(v_line,'kWLm2')/=0) then
     Rvar_tmp=Rvar_tmp*kWCMm22AU
     call set_itp_unit('kWLm2',1)
   endif 
 endif
 !
 if (     PARSER_call_) Rvar=Rvar_tmp
 if (.not.PARSER_call_) Rvar=Rvar/Rvar_tmp
 !
 contains 
   !
   subroutine set_itp_unit(unit_,im)
     !
     integer :: im
     character(*)::unit_
     !
     integer :: i_var!ws
     !
     if (.not.PARSER_call_) return
     !
     do i_var=1,ni1v
       if (trim(i1vars(i_var,1))==trim(DESC).and.im>0) i1vars(i_var,3)=unit_
       if (trim(i1vars(i_var,1))==trim(DESC).and.im<0) unit_=i1vars(i_var,3)
     enddo
     do i_var=1,ni2v
       if (trim(i2vars(i_var,1))==trim(DESC).and.im>0) i2vars(i_var,3)=unit_
       if (trim(i2vars(i_var,1))==trim(DESC).and.im<0) unit_=i2vars(i_var,3)
     enddo
     do i_var=1,ni3v
       if (trim(i3vars(i_var,1))==trim(DESC).and.im>0) i3vars(i_var,3)=unit_
       if (trim(i3vars(i_var,1))==trim(DESC).and.im<0) unit_=i3vars(i_var,3)
     enddo
     do i_var=1,nr1v
       if (trim(r1vars(i_var,1))==trim(DESC).and.im>0) r1vars(i_var,3)=unit_
       if (trim(r1vars(i_var,1))==trim(DESC).and.im<0) unit_=r1vars(i_var,3)
     enddo
     do i_var=1,nr2v
       if (trim(r2vars(i_var,1))==trim(DESC).and.im>0) r2vars(i_var,3)=unit_
       if (trim(r2vars(i_var,1))==trim(DESC).and.im<0) unit_=r2vars(i_var,3)
     enddo
     do i_var=1,nr3v
       if (trim(r3vars(i_var,1))==trim(DESC).and.im>0) r3vars(i_var,3)=unit_
       if (trim(r3vars(i_var,1))==trim(DESC).and.im<0) unit_=r3vars(i_var,3)
     enddo
     do i_var=1,nc1v
       if (trim(c1vars(i_var,1))==trim(DESC).and.im>0) c1vars(i_var,3)=unit_
       if (trim(c1vars(i_var,1))==trim(DESC).and.im<0) unit_=c1vars(i_var,3)
     enddo
   end subroutine
   !
end subroutine
