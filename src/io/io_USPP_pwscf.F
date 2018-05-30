!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AF
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
integer function io_USPP_pwscf(ID)
 !
 use pars,             ONLY: DP,SP,IP
 use stderr,           ONLY: intc
 use IO_m,             ONLY: io_connect,io_disconnect,io_sec, &
&                            io_elemental,io_bulk,io_header,&
&                            read_is_on
 use pseudo,           ONLY: pp_is_uspp,PP_uspp_alloc,PP_uspp_free,qe_pseudo_alloc
 use qe_pseudo_m,      ONLY: nspin, noncolin, npol, nat, nsp, ityp, tau, zv, omega, &
&                            npwx, gcutw, ecutwfc, lspinorb, tpiba
! use gvect,            ONLY: eigts1, eigts2, eigts3, g, ngm, mill
 use us,               ONLY: nqxq, dq, nqx, tab, qrad
 use uspp,             ONLY: nhtol, nhtoj, nhtolm, ijtoh, dvan, qq_at, indv,&
&                            qq_so, dvan_so, okvan, indv_ijkb0, nkb, nkbus
 use uspp_param,       ONLY: upf, lmaxq, nbetam, nh, nhm, lmaxkb 
 !
 implicit none
 !
 integer,intent(in) :: ID
 ! 
 ! Work Space
 !
 integer :: ik
 !
 ik=maxval(io_sec(ID,:))-1
 !
 io_USPP_pwscf=io_connect(desc="uspp_pp_pwscf",type=0,ID=ID)
 if (io_USPP_pwscf/=0) goto 1
 !
 ! Dimensions
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   io_USPP_pwscf=io_header(ID,IMPOSE_SN=.true.)
   if (io_USPP_pwscf/=0) goto 1
   !
   call io_elemental(ID,VAR="PARS",VAR_SZ=11,MENU=0)
   call io_elemental(ID,L0=lspinorb,VAR="lspinorb")
   call io_elemental(ID,L0=noncolin,VAR="noncolin")
   call io_elemental(ID,I0=npol,VAR="npol")
   call io_elemental(ID,I0=nspin,VAR="nspin")
   call io_elemental(ID,I0=nat,VAR="nat")
   call io_elemental(ID,I0=nsp,VAR="nsp")
   call io_elemental(ID,I0=npwx,VAR="npwx")
   call io_elemental(ID,D0=omega,VAR="omega")
   call io_elemental(ID,D0=tpiba,VAR="tpiba")
   call io_elemental(ID,D0=gcutw,VAR="gcutw")
   call io_elemental(ID,D0=ecutwfc,VAR="ecutwfc")
   !
   call io_elemental(ID,VAR="USPP_PARAM",VAR_SZ=4,MENU=0)
   call io_elemental(ID,I0=lmaxq,VAR="lmaxq")
   call io_elemental(ID,I0=lmaxkb,VAR="lmaxkb")
   call io_elemental(ID,I0=nbetam,VAR="nbetam")
   call io_elemental(ID,I0=nhm,VAR="nhm")
   !
   call io_elemental(ID,VAR="USPP",VAR_SZ=3,MENU=0)
   call io_elemental(ID,I0=nkb,VAR="nkb")
   call io_elemental(ID,I0=nkbus,VAR="nkbus")
   call io_elemental(ID,L0=okvan,VAR="okvan")
   !
   call io_elemental(ID,VAR="US",VAR_SZ=3,MENU=0)
   call io_elemental(ID,I0=nqxq,VAR="nqxq")
   call io_elemental(ID,I0=nqx,VAR="nqx")
   call io_elemental(ID,D0=dq,VAR="dq")
   !
   call io_elemental(ID,VAR="",VAR_SZ=0)
   !
 endif
 !
 if (read_is_on(ID)) then
   if (qe_pseudo_alloc) call PP_uspp_free()
   call PP_uspp_alloc()
   pp_is_uspp=.true.
 endif
 !
 ! qe_pseudo_m
 call io_bulk(ID,VAR="ityp",VAR_SZ=shape(ityp))
 call io_bulk(ID,I1=ityp)
 call io_bulk(ID,VAR="tau",VAR_SZ=shape(tau))
 call io_bulk(ID,D2=tau)
 call io_bulk(ID,VAR="zv",VAR_SZ=shape(zv))
 call io_bulk(ID,D1=zv)
 !
 ! uspp_param
 call io_bulk(ID,VAR="nh",VAR_SZ=shape(nh))
 call io_bulk(ID,I1=nh)
 ! XXX add UPF data
 !
 ! uspp
 call io_bulk(ID,VAR="indv",VAR_SZ=shape(indv))
 call io_bulk(ID,I2=indv)
 call io_bulk(ID,VAR="nhtol",VAR_SZ=shape(nhtol))
 call io_bulk(ID,I2=nhtol)
 call io_bulk(ID,VAR="nhtolm",VAR_SZ=shape(nhtolm))
 call io_bulk(ID,I2=nhtolm)
 call io_bulk(ID,VAR="ijtoh",VAR_SZ=shape(ijtoh))
 call io_bulk(ID,I3=ijtoh)
 call io_bulk(ID,VAR="indv_ijkb0",VAR_SZ=shape(indv_ijkb0))
 call io_bulk(ID,I1=indv_ijkb0)
 !
 call io_bulk(ID,VAR="qq_at",VAR_SZ=shape(qq_at))
 call io_bulk(ID,D3=qq_at)
 call io_bulk(ID,VAR="nhtoj",VAR_SZ=shape(nhtoj))
 call io_bulk(ID,D2=nhtoj)
 !
 if (lspinorb) then
   call io_bulk(ID,VAR="dvan_so",VAR_SZ=shape(dvan_so))
   call io_bulk(ID,Z4=dvan_so)
   call io_bulk(ID,VAR="qq_so",VAR_SZ=shape(qq_so))
   call io_bulk(ID,Z4=qq_so)
 else
   call io_bulk(ID,VAR="dvan",VAR_SZ=shape(dvan))
   call io_bulk(ID,D3=dvan)
 endif
 !
 ! us
 if (lmaxq > 0 ) then
   call io_bulk(ID,VAR="qrad",VAR_SZ=shape(qrad))
   call io_bulk(ID,D4=qrad)
 endif
 call io_bulk(ID,VAR="tab",VAR_SZ=shape(tab))
 call io_bulk(ID,D3=tab)
 !
1 call io_disconnect(ID=ID)
 !
end function io_USPP_pwscf
