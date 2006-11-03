!
! Copyright (C) 2000-2005 A. Marini and the SELF team 
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
 subroutine fermi_level(E,K,test_Tel,mode)
!=========================================
! Mode = 0 
!  Update the Fermi energy
!  Output : E%Efermi(1)
! Mode = 1 -> Mode 0 +
!  Define E%f and reports variations in Ef/nbf/nbm
!  Output : E%Efermi(2:) E%nbf,E%nbm
! Mode > 1 -> Mode 1 +
!  Full report of system electronic character
!
 use pars,          ONLY:SP,DP,HARTREE,Kelvin
 use drivers,       ONLY:Finite_Tel
 use D_lattice,     ONLY:Tel
 use electrons,     ONLY:levels,nel,n_spin,n_sp_pol,spin_occ,&
&                        BZ_RIM_nbands,BZ_RIM_tot_nkpts,n_spinor
 use R_lattice,     ONLY:bz_samp
 use com,           ONLY:msg
 use vec_operate,   ONLY:sort
 implicit none
 type(levels) ::E
 type(bz_samp)::K
 integer :: mode
 real(SP):: test_Tel
!ws
 integer :: i1,i2,is,ib,ik,trials(2),ires
 integer :: index(E%nk*E%nb*n_spin,3),index_s(E%nk*E%nb*n_spin)
 real(SP):: E_s(E%nk*E%nb*n_spin)
 real(SP):: lnel(2),Ef(2),Ffermi,Efdist,neltresh
 real(SP),parameter :: Telstep=0.0001/HARTREE
!
!     n_spin n_sp_pol n_spinor  spin_occ ffac
!       1        1        1         2      2
!       2        1        2        0.5     1
!       2        2        1         1      1
!
 spin_occ=2.0_SP/real(n_spin,SP)/real(n_spinor,SP)
!
 neltresh=1.E-5
 i1=0
 do ik=1,E%nk
   do ib=1,E%nb
     do is=1,n_spin
       i1=i1+1
       index(i1,:)=(/ib,ik,is/)
       E_s(i1)=E%E(ib,ik,is)
     enddo
   enddo
 enddo
 call sort(arrin=E_s,indx=index_s)
 trials=1
1 trials(1)=trials(1)+1 
!
! First guess
!--------------
!
 Ef(1)=minval(E%E(nint( real(nel)/2.0_SP*real(n_spinor)+.1),:,:))
 Ef(2)=maxval(E%E(nint( real(nel)/2.0_SP*real(n_spinor)+.1),:,:))
 call ef2nel(test_Tel,Ef(1),lnel(1))
 call ef2nel(test_Tel,Ef(2),lnel(2))
!
!Bracketing section: to find a lower and upper limit
!------------------
!to the Fermi level.
!
!Bracketing done when ires=0
!
 call bracket(Ef(1),lnel(1),Ef(2),lnel(2),ires)
 do while (ires/=0)
   call ef2nel(test_Tel,Ef(ires),lnel(ires))
   call bracket(Ef(1),lnel(1),Ef(2),lnel(2),ires)
 enddo
 E%Efermi(1)=(Ef(1)+Ef(2))/2.
 ires=1
 trials(2)=1
 do while (ires>0.and.trials(2)<=100)
   call ef2nel(test_Tel,E%Efermi(1),lnel(1))
   if (lnel(1)>neltresh) Ef(2)=E%Efermi(1)
   if (lnel(1)<-neltresh) Ef(1)=E%Efermi(1)
   if (abs(lnel(1))<=neltresh) then
     ires=-1
     exit
   endif
   E%Efermi(1)=(Ef(1)+Ef(2))/2.
   trials(2)=trials(2)+1
 enddo
 if (ires>0) then
   test_Tel=test_Tel+Telstep
   if (trials(1)<=100) goto 1
 endif
!
! Mode = 0. Check only for the Fermi energy
 Finite_Tel=(Tel>=1.E-5)
 if (mode==0) return
!
 if (.not.associated(E%f)) allocate(E%f(E%nb,E%nk,n_spin))
 Efdist=1.E5
 do i1=1,E%nk*E%nb*n_spin
   ib=index( index_s(i1) , 1)
   ik=index( index_s(i1) , 2)
   is=index( index_s(i1) , 3)

   E%f(ib,ik,is)=spin_occ*Ffermi(E_s(i1)-E%Efermi(1),test_Tel)

   if (abs(E%E(ib,ik,is)-E%Efermi(1))<Efdist) then
     E%bf=ib
     E%kf=ik
     E%sf=is
     Efdist=abs(E%E(ib,ik,is)-E%Efermi(1))
   endif

 enddo
 E%E(:,:,:)=E%E(:,:,:)-E%Efermi(1)
!
! If %E_RIM is associated I simply extend the %E occupations to %E_RIM
!======================================================================
 if (BZ_RIM_nbands>0) then
   do ik=1,BZ_RIM_tot_nkpts
     do ib=1,BZ_RIM_nbands
       do is=1,n_spin
         E%E_RIM(ib,ik,is)=E%E_RIM(ib,ik,is)-E%Efermi(1)
         E%f_RIM(ib,ik,is)=spin_occ*Ffermi(E%E_RIM(ib,ik,is),test_Tel)
       enddo
     enddo
   enddo
 endif
!
! Mode = 1. Define E%f and exits
 if (mode==1) then
   call fermi_msg(E,1)
   return
 endif
!
 call msg('r','Fermi Energy[ev] - T[ev/K] :',&
&               (/E%Efermi(1)*HARTREE,test_Tel*HARTREE,test_Tel*Kelvin/))
 call fermi_msg(E,mode)
 if (E%nbf/=E%nbm) then
   call msg('r','Electrons - <f_met>        :',(/lnel(1)+nel,&
&               sum(E%f(E%nbf+1:E%nbm,:,:))/real(K%nibz*n_spin*(E%nbm-E%nbf))/))
 endif
!
 contains
!
   subroutine ef2nel(tTel,Ef,lnel)
!=================================
   implicit none
   real(SP)::tTel,Ef,lnel
   !ws
   integer  :: i1,ik,ib 
   real(SP) :: Ffermi 
   real(DP):: nel_acc
   nel_acc=0.d0
   do i1=1,E%nk*E%nb*n_spin
     ib=index( index_s(i1) , 1)
     ik=index( index_s(i1) , 2)
     is=index( index_s(i1) , 3)
     nel_acc=nel_acc+spin_occ*Ffermi(E_s(i1)-Ef,tTel)*K%weights(ik)
   enddo
   lnel=nel_acc-nel
   end subroutine

 end subroutine

