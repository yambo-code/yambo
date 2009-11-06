!
! Copyright (C) 2000-2009 A. Marini and the YAMBO team 
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
subroutine fermi_level(E,K,test_Tel,mode)
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
 use pars,          ONLY:SP,DP
 use units,         ONLY:HARTREE,Kelvin
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
 ! Work Space
 !
 integer :: i1,i2,is,ib,ik,i_trials(2),n_b_full_guess,i_Ef(2),&
&           n_total_states,i_group,group_size
 integer :: index(E%nk*E%nb*n_sp_pol,3),index_E_sorted(E%nk*E%nb*n_sp_pol)
 real(SP):: E_sorted(E%nk*E%nb*n_sp_pol)
 real(SP):: n_of_el_from_zero(2),Ef(2),Efdist
 logical :: Fermi_is_converged
 real(SP),parameter :: Telstep=0.0001/HARTREE
 real(SP),parameter :: nel_diff_zero=1.E-5
 !
 !     n_spin n_sp_pol n_spinor  spin_occ 
 !       1        1        1         2      
 !       2        1        2         1      non collinear
 !       2        2        1         1          collinear
 !
 spin_occ=2.0_SP/real(n_spin)
 !
 ! All energies are stored in E_sorted...
 !
 i1=0
 do ik=1,E%nk
   do ib=1,E%nb
     do is=1,n_sp_pol
       i1=i1+1
       index(i1,:)=(/ib,ik,is/)
       E_sorted(i1)=E%E(ib,ik,is)
     enddo
   enddo
 enddo
 n_total_states=E%nk*E%nb*n_sp_pol
 !
 ! ... and sorted
 !
 call sort(arrin=E_sorted,indx=index_E_sorted)
 !
 ! First guess
 !
 ! real(nel)/2.0_SP*real(n_spinor) is last occupied state
 !
 ! n_b_full_guess is used to avoid spurious oscillations
 ! in the Fermi Level search for system with a gap.
 ! In general those oscillations appear when the gap is small.
 !
 n_b_full_guess= nint( real(nel)/2.0_SP*real(n_spinor)+.1)
 !
 Ef(1)=maxval(E%E(n_b_full_guess,:,:))
 Ef(2)=minval(E%E(n_b_full_guess+1,:,:))
 !
 ! n_b_full_guess not set when the system is metallic ...
 ! 
 if (Ef(2)<Ef(1).or.Ef(1)==Ef(2)) then
   Ef(1)=minval(E%E(n_b_full_guess,:,:))
   Ef(2)=maxval(E%E(n_b_full_guess,:,:))
   n_b_full_guess=0
 endif
 !
 ! ... or when there is a odd number of electrons (when n_sp_pol>1
 ! the system can be semiconductive in the two channels).
 ! 
 if (mod(nel,2._SP)/=0._SP) n_b_full_guess=0
 !
 ! Start Loop
 ! 
 i_trials=1
 Fermi_is_converged=.false.
 !
 do while (.not.Fermi_is_converged)
   !
   i_Ef=0
   i_trials(1)=i_trials(1)+1 
   !
   ! Here we define a first approx for the range
   ! of values where E_fermi should fall in two steps
   !
   ! 1. [Rough search] by moving of group_size elements
   !
   group_size=max(1,int(n_total_states/500))
   do i1=1,n_total_states,group_size
     call ef2nel(test_Tel,E_sorted(i1),n_of_el_from_zero(1))
     i_Ef(1)= (int(i1/group_size)-1)*group_size 
     if (group_size>1) i_Ef(1)=i_Ef(1)+1
     if (n_of_el_from_zero(1)>0) i_Ef(2)=i1
     if (i_Ef(2)/=0) exit
   enddo
   !
   ! 2. [Finer search] by moving of 1 element
   !
   do i1=i_Ef(1),i_Ef(2),1
     call ef2nel(test_Tel,E_sorted(i1),n_of_el_from_zero(1))
     if ( abs(n_of_el_from_zero(1)) < nel_diff_zero) then
       E%Efermi(1)=E_sorted(i1)
       Fermi_is_converged=.true.
       exit
     endif
     if (n_of_el_from_zero(1)>0) then
       Ef(1)=E_sorted(i1-1)
       Ef(2)=E_sorted(i1)
       exit
     endif
   enddo
   !
   if (Fermi_is_converged) exit
   !
   call ef2nel(test_Tel,Ef(1),n_of_el_from_zero(1))
   call ef2nel(test_Tel,Ef(2),n_of_el_from_zero(2))
   !
   E%Efermi(1)=Ef(1)
   if (n_of_el_from_zero(2)<-n_of_el_from_zero(1)) E%Efermi(1)=Ef(2)
   !
   i_trials(2)=1
   do while (i_trials(2)<=100)
     call ef2nel(test_Tel,E%Efermi(1),n_of_el_from_zero(1))
     if (n_of_el_from_zero(1)<-nel_diff_zero) Ef(1)=E%Efermi(1)
     if (n_of_el_from_zero(1)> nel_diff_zero) Ef(2)=E%Efermi(1)
     if (abs(n_of_el_from_zero(1))<=nel_diff_zero) then
       Fermi_is_converged=.true.
       exit
     endif
     if (abs(Ef(1)-Ef(2))<1.E-8) exit
     E%Efermi(1)=(Ef(1)+Ef(2))/2.
     i_trials(2)=i_trials(2)+1
   enddo
   !
   if (Fermi_is_converged) exit
   !
   test_Tel=test_Tel+Telstep
   if (i_trials(1)==100) call error('Impossible to converge the Fermi Level')
   !
 enddo
 !
 ! Mode = 0. Check only for the Fermi energy
 !
 Finite_Tel=(Tel>=1.E-5)
 !
 ! GPL_INCLUDE_START
 !
 ! if (Finite_Tel) call warning('Non zero Temp. is still unsupported in this version')
 !
 ! GPL_INCLUDE_END
 !
 if (mode==0) return
 !
 ! build the occupations ...
 !
 !... and find the nearest occupied state to the Fermi level
 !
 if (.not.associated(E%f)) allocate(E%f(E%nb,E%nk,n_spin))
 Efdist=1.E5
 do i1=1,n_total_states
   ib=index( index_E_sorted(i1) , 1)
   ik=index( index_E_sorted(i1) , 2)
   is=index( index_E_sorted(i1) , 3)
   !
   E%f(ib,ik,is)=spin_occ*Fermi_fnc(E_sorted(i1)-E%Efermi(1),test_Tel)
   !
   if (abs(E%E(ib,ik,is)-E%Efermi(1))<Efdist) then
     if ( E%f(ib,ik,is) < filled_tresh ) cycle
     if ( E%f(ib,ik,is)  ==   spin_occ ) cycle
     E%bf=ib
     E%kf=ik
     E%sf=is
     Efdist=abs(E%E(ib,ik,is)-E%Efermi(1))
   endif
   !
 enddo
 E%E(:,:,:)=E%E(:,:,:)-E%Efermi(1)
 !
 ! If %E_RIM is associated I simply extend the %E occupations to %E_RIM
 !
 if (BZ_RIM_nbands>0) then
   E%E_RIM(:,:,:)=E%E_RIM(:,:,:)-E%Efermi(1)
   do ik=1,BZ_RIM_tot_nkpts
     do ib=1,BZ_RIM_nbands
       do is=1,n_sp_pol
         E%f_RIM(ib,ik,is)=spin_occ*Fermi_fnc(E%E_RIM(ib,ik,is),test_Tel)
       enddo
     enddo
   enddo
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
   call msg('r','Electrons - <f_met>        :',(/n_of_el_from_zero(1)+nel,&
&               sum(E%f(E%nbf+1:E%nbm,:,:))/real(K%nibz*(E%nbm-E%nbf))/))
 endif
 !
 contains
   !
   subroutine ef2nel(tTel,Ef,N_of_el_diff)
     !
     implicit none
     real(SP)::tTel,Ef,N_of_el_diff
     !
     !Work Space
     !
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
     do i1=1,n_total_states
       ib=index( index_E_sorted(i1) , 1)
       ik=index( index_E_sorted(i1) , 2)
       is=index( index_E_sorted(i1) , 3)
       if (ib<=n_b_full_guess) nel_theo=nel_theo+spin_occ*K%weights(ik)
       nel_acc=nel_acc+spin_occ*Fermi_fnc(E_sorted(i1)-Ef,tTel)*K%weights(ik)
     enddo
     if (n_b_full_guess==0) nel_theo=nel
     N_of_el_diff=nel_acc-nel_theo
   end subroutine
   !
end subroutine
