!
!        Copyright (C) 2000-2019 the YAMBO team
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
subroutine OCCUPATIONS_Fermi(E,K,OBJ,mode,n_bands)
 !
 ! Mode = "FERMI" 
 !  Update the Fermi energy
 !  Output : E%E_fermi
 !
 ! Mode =  "OCCUPATIONS" (implies "FERMI")
 !  Define E%f and reports variations in Ef/nbf/nbm
 !  Output : E%nbf,E%nbm
 !  E%E are shifted 
 !
 ! no Mode  (implies "FERMI" and "OCCUPATIONS")
 ! Full report of system electronic character
 !
 use pars,          ONLY:SP,DP
 use units,         ONLY:HA2KEL
 use drivers,       ONLY:Finite_Tel
 use D_lattice,     ONLY:Tel,Bose_Temp
 use electrons,     ONLY:levels,nel,n_sp_pol,spin_occ,filled_tresh,&
&                        BZ_DbGd_nbands,BZ_DbGd_nkpt,n_spinor,n_spin,&
&                        BZ_DbGd_table
 use R_lattice,     ONLY:bz_samp
 use com,           ONLY:error,warning
 use stderr,        ONLY:real2ch
 use vec_operate,   ONLY:sort
 use functions,     ONLY:Fermi_fnc
 use interfaces,    ONLY:OCCUPATIONS_Gaps
#include<memory.h>
 !
 type(levels)  ::E
 type(bz_samp) ::K
 character(*), OPTIONAL ::mode,OBJ
 integer,      OPTIONAL ::n_bands
 !
 ! Work Space
 !
 integer :: i1,is,ib,ik,ikbz,ik_rand,i_trials(2),n_b_full_guess,i_Ef(2),i_Ef_fine(2),n_total_states,group_size,nb_here
 integer :: index(E%nk*E%nb*n_sp_pol,3),index_E_sorted(E%nk*E%nb*n_sp_pol)
 real(SP):: E_sorted(E%nk*E%nb*n_sp_pol),n_of_el_from_zero(2),Ef(2),Efdist,nel_DbGd,nel_diff_zero
 logical :: Fermi_is_converged
 real(SP),parameter :: Tel_step=0.1_SP/HA2KEL
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
 nb_here=E%nb
 if (present(n_bands)) nb_here=n_bands
 !
 i1=0
 do ik=1,E%nk
   do ib=1,nb_here
     do is=1,n_sp_pol
       i1=i1+1
       index(i1,:)=(/ib,ik,is/)
       E_sorted(i1)=E%E(ib,ik,is)
     enddo
   enddo
 enddo
 n_total_states=E%nk*nb_here*n_sp_pol
 !
 ! ... and sorted
 !
 call sort(arrin=E_sorted(:n_total_states),indx=index_E_sorted(:n_total_states))
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
 if (n_b_full_guess+1 > nb_here ) call error(' Too few states. Include more states in the DFT run.')
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
 nel_diff_zero=1.E-5
 Fermi_is_converged=.false.
 !
 ! BUG-fix [Andrea 1/6/2012]: when the system is semiconductive it may happen the
 ! Efermi falls below or above the CBM/VBM of the Double Grid energy levels. To avoid this
 ! a very small temperature is imposed forcing the Fermi level to fall within the
 ! gap.
 !
 if (BZ_DbGd_nbands>0) Tel=Tel_step
 !
 FERMI_LOOP: do while (.not.Fermi_is_converged)
   !
   i_Ef=(/1,n_total_states/)
   i_trials(1)=i_trials(1)+1 
   !
   ! 1. [Rough search] by looping of group_size elements
   !
   group_size=max(1,int(n_total_states/500))
   do i1=1,n_total_states,group_size
     call ef2nel(Tel,E_sorted(i1),n_of_el_from_zero(1))
     if (n_of_el_from_zero(1)<-nel_diff_zero) i_Ef(1)=i1
     if (n_of_el_from_zero(1)> nel_diff_zero) then
       i_Ef(2)=i1
       exit
     endif
   enddo
   !
   i_Ef_fine=i_Ef
   !
   if (group_size>1) then
     !
     ! 2. [Finer search] by looping of 1 element
     !
     do i1=i_Ef(1),i_Ef(2)
       call ef2nel(Tel,E_sorted(i1),n_of_el_from_zero(1))
       if (n_of_el_from_zero(1)<-nel_diff_zero) i_Ef_fine(1)=i1
       if (n_of_el_from_zero(1)> nel_diff_zero) then
         i_Ef_fine(2)=i1
         exit
       endif
     enddo
     !
   endif
   !
   if (i_Ef_fine(2)==n_total_states) call error('Not enough states to converge the Fermi Level')
   !
   ! 3. Is any level in the Ef_fine range ok?
   !
   do i1=i_Ef_fine(1),i_Ef_fine(2)
     call ef2nel(Tel,E_sorted(i1),n_of_el_from_zero(1))
     if (abs(n_of_el_from_zero(1))<nel_diff_zero.and.E_sorted(i1+1)-E_sorted(i1)>Tel_step) then
       E%E_Fermi=E_sorted(i1)
       ! To Code!
       !E%E_Fermi=E_sorted(i1)+E_sorted(i1+1))/2.
       exit FERMI_LOOP
     endif
   enddo
   !
   ! 4. extend i_Ef_fine to include degenerate states
   !
   i_Ef=i_Ef_fine
   do i1=i_Ef_fine(1)-1,1,-1
     if (E_sorted(i_Ef_fine(1))-E_sorted(i1)<Tel_step) i_Ef(1)=i1
     if (E_sorted(i_Ef_fine(1))-E_sorted(i1)>Tel_step) exit
   enddo
   do i1=i_Ef_fine(2)+1,n_total_states
     if (E_sorted(i1)-E_sorted(i_Ef_fine(2))<Tel_step) i_Ef(2)=i1
     if (E_sorted(i1)-E_sorted(i_Ef_fine(2))>Tel_step) exit
   enddo
   if (i_Ef(1)/=i_Ef(2).or.i_Ef(1)/=i_Ef_fine(2)) then
     Tel=Tel+Tel_step
     i_Ef(1)=i_Ef(1)-2
     i_Ef(2)=i_Ef(2)+2
   endif
   !
   ! 5. Use a secant-like procedure to converge to the final Level
   !
   Ef=(/E_sorted(i_Ef(1)),E_sorted(i_Ef(2))/)
   !
   call ef2nel(Tel,Ef(1),n_of_el_from_zero(1))
   call ef2nel(Tel,Ef(2),n_of_el_from_zero(2))
   !
   E%E_Fermi=Ef(1)
   if (n_of_el_from_zero(2)<-n_of_el_from_zero(1)) E%E_Fermi=Ef(2)
   !
   i_trials(2)=1
   do while (i_trials(2)<=1000)
     call ef2nel(Tel,E%E_Fermi,n_of_el_from_zero(1))
     if (n_of_el_from_zero(1)<-nel_diff_zero) Ef(1)=E%E_Fermi
     if (n_of_el_from_zero(1)> nel_diff_zero) Ef(2)=E%E_Fermi
     if (abs(n_of_el_from_zero(1))<=nel_diff_zero) then
       Fermi_is_converged=.true.
       exit FERMI_LOOP
     endif
     if (abs(Ef(1)-Ef(2))<1.E-8) exit FERMI_LOOP
     E%E_Fermi=(Ef(1)+Ef(2))/2.
     i_trials(2)=i_trials(2)+1
   enddo
   !
   if (Fermi_is_converged) exit FERMI_LOOP
   !
   Tel=Tel+Tel_step
   if (i_trials(1)>100) nel_diff_zero=nel_diff_zero*10.
   if (i_trials(1)==1000) call error('Impossible to converge the Fermi Level')
   !
 enddo FERMI_LOOP
 !
 ! Mode = 0. Check only for the Fermi energy
 !
 if (Bose_Temp<0.) Bose_Temp=Tel
 Finite_Tel=any((/Tel,Bose_Temp/)>=Tel_step*3._SP)
 !
 if (present(mode)) then
   if (mode=="FERMI") return
 endif
 !
 ! Build the occupations and find the nearest occupied state to the Fermi level
 !
  if (.not.allocated(E%f)) then  
   YAMBO_ALLOC(E%f,(nb_here,E%nk,n_sp_pol))
 endif
 Efdist=1.E5_SP
 !
 do i1=1,n_total_states
   ib=index( index_E_sorted(i1) , 1)
   ik=index( index_E_sorted(i1) , 2)
   is=index( index_E_sorted(i1) , 3)
   !
   E%f(ib,ik,is)=spin_occ*Fermi_fnc(E_sorted(i1)-E%E_Fermi,Tel)
   !
   if (abs(E%E(ib,ik,is)-E%E_Fermi)<Efdist) then
     if ( E%f(ib,ik,is) < filled_tresh ) cycle
     if ( E%f(ib,ik,is)  ==   spin_occ ) cycle
     E%bf=ib
     E%kf=ik
     E%sf=is
     Efdist=abs(E%E(ib,ik,is)-E%E_Fermi)
   endif
   !
 enddo
 !
 E%E(:,:,:)=E%E(:,:,:)-E%E_Fermi
 !
 ! If %E_DbGd is associated I simply extend the %E occupations to %E_DbGd
 !
 if (BZ_DbGd_nbands>0.and.allocated(E%E_DbGd)) then
   ! 
   nel_DbGd=0._SP 
   !
   E%E_DbGd(:,:,:)=E%E_DbGd(:,:,:)-E%E_Fermi
   do is=1,n_sp_pol
     do ikbz=1,K%nbz
       do i1=1,BZ_DbGd_nkpt(ikbz)
         ik_rand = BZ_DbGd_table(ikbz,i1,1)
         do ib=1,BZ_DbGd_nbands
           E%f_DbGd(ib,ik_rand,is)=spin_occ*Fermi_fnc(E%E_DbGd(ib,ik_rand,is),Tel)
           nel_DbGd=nel_DbGd+E%f_DbGd(ib,ik_rand,is)
         enddo
       enddo
     enddo
   enddo
   nel_DbGd=nel_DbGd/real(sum(BZ_DbGd_nkpt),SP)
   !
   if (abs(nel-nel_DbGd)>        1.E-2) call error  (' Double grid produces  '//trim(real2ch(nel_DbGd))//' e-.')
   if (abs(nel-nel_DbGd)>nel_diff_zero) call warning(' Double grid produces  '//trim(real2ch(nel_DbGd))//' e-.')
   !
 endif
 !
 ! Gaps, VBM/CBM
 !
 if (present(mode)) then
   if (mode=="OCCUPATIONS") then
    call OCCUPATIONS_Gaps(E)
     return
   endif
 endif
 !
 if (present(OBJ)) then
   call REPORT_Occupations(OBJ,E)
 else
   call REPORT_Occupations(" ",E)
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
     nel_acc=0._DP
     nel_theo=0._DP
     do i1=1,n_total_states
       ib=index( index_E_sorted(i1) , 1)
       ik=index( index_E_sorted(i1) , 2)
       is=index( index_E_sorted(i1) , 3)
       if (ib<=n_b_full_guess) nel_theo=nel_theo+spin_occ*K%weights(ik)
       nel_acc=nel_acc+real(spin_occ*Fermi_fnc(E_sorted(i1)-Ef,tTel)*K%weights(ik),DP)
     enddo
     if (n_b_full_guess==0) nel_theo=nel
     N_of_el_diff=real(nel_acc-nel_theo,SP)
   end subroutine
   !
end subroutine
