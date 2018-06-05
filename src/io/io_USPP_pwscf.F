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
 use pars,             ONLY: DP,SP,LP
 use stderr,           ONLY: intc
 use IO_m,             ONLY: io_connect,io_disconnect,io_sec, &
&                            io_elemental,io_bulk,io_header,&
&                            read_is_on,io_mode,DUMP
 use D_lattice,        ONLY: n_atoms,n_atomic_species
 use electrons,        ONLY: l_spin_orbit,n_spinor
 !
 use pseudo,           ONLY: pp_is_uspp,PP_uspp_alloc,PP_uspp_free,qe_pseudo_alloc
 use qe_pseudo_m,      ONLY: nspin, noncolin, npol, nat, nsp, ityp, tau, zv, omega, &
&                            npwx, gcutw, gcutm, ecutwfc, ecutrho, lspinorb, tpiba
 use us,               ONLY: nqxq, nqx, tab, qrad
 use uspp,             ONLY: nhtol, nhtoj, nhtolm, ijtoh, dvan, qq_at, indv,&
&                            qq_so, dvan_so, indv_ijkb0, nkb, nkbus, okvan,&
                             lpl, lpx, ap
 use uspp_param,       ONLY: upf, lmaxq, nbetam, nh, nhm, lmaxkb 
 !
 implicit none
 !
 integer,intent(in) :: ID
 ! 
 ! Work Space
 !
 integer :: i, nlx
 integer(LP), allocatable :: lvar(:)
 integer,     allocatable :: ivar(:)
 logical :: ltmp


 io_USPP_pwscf=io_connect(desc="uspp_pp_pwscf",type=0,ID=ID)
 if (io_USPP_pwscf/=0) goto 1
 !
 io_USPP_pwscf=io_header(ID,IMPOSE_SN=.true.)
 if (io_USPP_pwscf/=0) goto 1
 !
 ! Dimensions
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   call io_elemental(ID,VAR="PARS",VAR_SZ=5,MENU=0)
   call io_elemental(ID,L0=l_spin_orbit,DB_L0=lspinorb,CHECK=.true.,OP=(/"=="/))
   !
   ltmp=(n_spinor==2)
   call io_elemental(ID,L0=ltmp,DB_L0=noncolin,CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,I0=nspin,DB_I0=nspin)
   call io_elemental(ID,I0=n_atoms,DB_I0=nat,CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,I0=n_atomic_species,DB_I0=nsp,CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,VAR="",VAR_SZ=0)
   !
 endif
 !
 ! system and lattice
 call io_bulk(ID,VAR="npol",VAR_SZ=(/1/))
 call io_bulk(ID,I0=npol)
 call io_bulk(ID,VAR="npwx",VAR_SZ=(/1/))
 call io_bulk(ID,I0=npwx)
 call io_bulk(ID,VAR="omega",VAR_SZ=(/1/))
 call io_bulk(ID,D0=omega)
 call io_bulk(ID,VAR="tpiba",VAR_SZ=(/1/))
 call io_bulk(ID,D0=tpiba)
 call io_bulk(ID,VAR="gcutw",VAR_SZ=(/1/))
 call io_bulk(ID,D0=gcutw)
 call io_bulk(ID,VAR="ecutwfc",VAR_SZ=(/1/))
 call io_bulk(ID,D0=ecutwfc)
 call io_bulk(ID,VAR="gcutm",VAR_SZ=(/1/))
 call io_bulk(ID,D0=gcutm)
 call io_bulk(ID,VAR="ecutrho",VAR_SZ=(/1/))
 call io_bulk(ID,D0=ecutrho)

 !
 ! USPP param
 call io_bulk(ID,VAR="lmaxq",VAR_SZ=(/1/))
 call io_bulk(ID,I0=lmaxq) 
 call io_bulk(ID,VAR="lmaxkb",VAR_SZ=(/1/))
 call io_bulk(ID,I0=lmaxkb) 
 call io_bulk(ID,VAR="nbetam",VAR_SZ=(/1/))
 call io_bulk(ID,I0=nbetam) 
 call io_bulk(ID,VAR="nhm",VAR_SZ=(/1/))
 call io_bulk(ID,I0=nhm) 

 !
 ! USPP module
 call io_bulk(ID,VAR="nkb",VAR_SZ=(/1/))
 call io_bulk(ID,I0=nkb)
 call io_bulk(ID,VAR="nkbus",VAR_SZ=(/1/))
 call io_bulk(ID,I0=nkbus)
 !
 if (read_is_on(ID)) okvan=.true.
 !
 ! US module
 call io_bulk(ID,VAR="nqxq",VAR_SZ=(/1/))
 call io_bulk(ID,I0=nqxq)
 call io_bulk(ID,VAR="nqx",VAR_SZ=(/1/))
 call io_bulk(ID,I0=nqx)
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
 !
 allocate(lvar(nsp))
 allocate(ivar(nsp))
 !
 if (read_is_on(ID)) then
   call io_bulk(ID,VAR="tvanp",VAR_SZ=shape(lvar))
   call io_bulk(ID,L1=lvar)
   do i = 1, nsp
     upf(i)%tvanp= (lvar(i)==1)
   enddo
   !
   call io_bulk(ID,VAR="has_so",VAR_SZ=shape(lvar))
   call io_bulk(ID,L1=lvar)
   do i = 1, nsp
     upf(i)%has_so= (lvar(i)==1)
   enddo
   !
   call io_bulk(ID,VAR="nbeta",VAR_SZ=shape(ivar))
   call io_bulk(ID,I1=ivar)
   do i = 1, nsp
     upf(i)%nbeta=ivar(i)
   enddo
 else
   lvar=0
   do i = 1, nsp
     if (upf(i)%tvanp) lvar(i)=1
   enddo
   call io_bulk(ID,VAR="tvanp",VAR_SZ=shape(lvar))
   call io_bulk(ID,L1=lvar)
   !
   lvar=0
   do i = 1, nsp
     if (upf(i)%has_so) lvar(i)=1
   enddo
   call io_bulk(ID,VAR="has_so",VAR_SZ=shape(lvar))
   call io_bulk(ID,L1=lvar)
   !
   do i = 1, nsp
     ivar(i)=upf(i)%nbeta
   enddo
   call io_bulk(ID,VAR="nbeta",VAR_SZ=shape(ivar))
   call io_bulk(ID,I1=ivar)
   !
 endif
 !
 deallocate(lvar)
 deallocate(ivar)
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
 call io_bulk(ID,VAR="lpx",VAR_SZ=shape(lpx))
 call io_bulk(ID,I2=lpx)
 call io_bulk(ID,VAR="lpl",VAR_SZ=shape(lpl))
 call io_bulk(ID,I3=lpl)
 call io_bulk(ID,VAR="ap",VAR_SZ=shape(ap))
 call io_bulk(ID,D3=ap)
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
