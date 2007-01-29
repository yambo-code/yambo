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
 !
 ! Mode = 0 
 !  Update the Fermi energy
 !  Output : E%Efermi(1)
 !
 ! Mode = 1 -> Mode 0 +
 !  Define E%f and reports variations in Ef/nbf/nbm
 !  Output : E%Efermi(2:) E%nbf,E%nbm
 !
 ! Mode > 1 -> Mode 1 +
 !  Full report of system electronic character
 !
 use pars,          ONLY:SP,DP,HARTREE,Kelvin
 use drivers,       ONLY:Finite_Tel
 use D_lattice,     ONLY:Tel
 use electrons,     ONLY:levels,nel,n_sp_pol,spin_occ,filled_tresh,&
&                        BZ_RIM_nbands,BZ_RIM_tot_nkpts,n_spinor,n_spin
 use R_lattice,     ONLY:bz_samp
 use com,           ONLY:msg,error,warning
 use vec_operate,   ONLY:sort
 use functions,     ONLY:Fermi_fnc
 implicit none
 type(levels) ::E
 type(bz_samp)::K
 integer :: mode
 real(SP):: test_Tel
 !
 !Work Space
 !
 integer :: i1,i2,is,ib,ik,i_trials(3),ires,n_b_full_guess
 integer :: index(E%nk*E%nb*n_sp_pol,3),index_s(E%nk*E%nb*n_sp_pol)
 real(SP):: E_s(E%nk*E%nb*n_sp_pol)
 real(SP):: lnel(2),Ef(2),Efdist,neltresh
 real(SP),parameter :: Telstep=0.0001/HARTREE
 !
 !     n_spin n_sp_pol n_spinor  spin_occ 
 !       1        1        1         2      
 !       2        1        2         1      non collinear
 !       2        2        1         1          collinear
 !
 spin_occ=2.0_SP/real(n_spin)
 !
 ! All energies are stored in E_s...
 !
 neltresh=1.E-5
 i1=0
 do ik=1,E%nk
   do ib=1,E%nb
     do is=1,n_sp_pol
       i1=i1+1
       index(i1,:)=(/ib,ik,is/)
       E_s(i1)=E%E(ib,ik,is)
     enddo
   enddo
 enddo
 !
 ! ... and sorted
 !
 call sort(arrin=E_s,indx=index_s)
 !
 ! Start Loop
 ! 
 i_trials=1
 !
1 continue
 !
 i_trials(1)=i_trials(1)+1 
 !
 ! First guess
 !--------------
 !
 ! real(nel)/2.0_SP*real(n_spinor) is last occupied state
 !
 n_b_full_guess= nint( real(nel)/2.0_SP*real(n_spinor)+.1)
 !
 Ef(1)=maxval(E%E(n_b_full_guess,:,:))
 Ef(2)=minval(E%E(n_b_full_guess+1,:,:))
 !
 ! n_b_full_guess is metallic
 ! 
 if (Ef(2)<Ef(1).or.Ef(1)==Ef(2)) then
   Ef(1)=minval(E%E(n_b_full_guess,:,:))
   Ef(2)=maxval(E%E(n_b_full_guess,:,:))
   n_b_full_guess=0
 endif
 !
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
   i_trials(2)=i_trials(2)+1
   if (i_trials(2)>100) call error('Impossible to bracket the Fermi level')
 enddo 
 !
 E%Efermi(1)=(Ef(1)+Ef(2))/2.
 ires=1
 !
 i_trials(3)=1
 !
 do while (ires>0.and.i_trials(3)<=100)
   call ef2nel(test_Tel,E%Efermi(1),lnel(1))
   if (lnel(1)>neltresh) Ef(2)=E%Efermi(1)
   if (lnel(1)<-neltresh) Ef(1)=E%Efermi(1)
   if (abs(lnel(1))<=neltresh) then
     ires=-1
     exit
   endif
   E%Efermi(1)=(Ef(1)+Ef(2))/2.
   i_trials(3)=i_trials(3)+1
 enddo
 !
 if (ires>0) then
   test_Tel=test_Tel+Telstep
   if (i_trials(1)<=100) goto 1
 endif
 !
 ! Mode = 0. Check only for the Fermi energy
 !
 Finite_Tel=(Tel>=1.E-5)
 if (mode==0) return
 !
 ! build the occupations ...
 !
 !... and find the nearest occupied state to the Fermi level
 !
 if (.not.associated(E%f)) allocate(E%f(E%nb,E%nk,n_spin))
 Efdist=1.E5
 do i1=1,E%nk*E%nb*n_sp_pol
   ib=index( index_s(i1) , 1)
   ik=index( index_s(i1) , 2)
   is=index( index_s(i1) , 3)

   E%f(ib,ik,is)=spin_occ*Fermi_fnc(E_s(i1)-E%Efermi(1),test_Tel)
   if (n_spinor==2) E%f(ib,ik,2)=E%f(ib,ik,1)

   if (abs(E%E(ib,ik,is)-E%Efermi(1))<Efdist) then
     if ( E%f(ib,ik,is) < filled_tresh ) cycle
     if ( E%f(ib,ik,is)  ==   spin_occ ) cycle
     E%bf=ib
     E%kf=ik
     E%sf=is
     Efdist=abs(E%E(ib,ik,is)-E%Efermi(1))
   endif

 enddo
 E%E(:,:,:)=E%E(:,:,:)-E%Efermi(1)

 !
 ! If %E_RIM is associated I simply extend the %E occupations to %E_RIM
 !
 if (BZ_RIM_nbands>0) then
   do ik=1,BZ_RIM_tot_nkpts
     do ib=1,BZ_RIM_nbands
       do is=1,n_sp_pol
         E%f_RIM(ib,ik,is)=spin_occ*Fermi_fnc(E%E_RIM(ib,ik,is),test_Tel)
         if (n_spinor==2) E%f_RIM(ib,ik,2)=E%f_RIM(ib,ik,1)
       enddo
     enddo
   enddo
   E%E_RIM(:,:,:)=E%E_RIM(:,:,:)-E%Efermi(1)
 endif
 !
 ! Mode = 1. Define E%f and exits
 !
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
&               sum(E%f(E%nbf+1:E%nbm,:,:))/real(K%nibz*(E%nbm-E%nbf))/))
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
   real(DP) :: nel_acc
   !
   ! For some compilers (like PGI) the summed nel_acc
   ! is different from nel even for insulators.
   ! This problem is solved comparing nel_acc with nel_theo
   ! simlarly summed. Note that nel_theo=nel for metals
   !
   real(DP) :: nel_theo
   nel_acc=0.d0
   nel_theo=0.d0
   do i1=1,E%nk*E%nb*n_sp_pol
     ib=index( index_s(i1) , 1)
     ik=index( index_s(i1) , 2)
     is=index( index_s(i1) , 3)
     if (ib<=n_b_full_guess) nel_theo=nel_theo+spin_occ*K%weights(ik)
     nel_acc=nel_acc+spin_occ*Fermi_fnc(E_s(i1)-Ef,tTel)*K%weights(ik)
   enddo
   if (n_b_full_guess==0) nel_theo=nel
   lnel=nel_acc-nel_theo
   end subroutine

 end subroutine

