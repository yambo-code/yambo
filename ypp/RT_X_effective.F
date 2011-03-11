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
subroutine RT_X_effective(en,k)
 !
 use pars,           ONLY:SP,lchlen,cI
 use YPP,            ONLY:RT_J_t,RT_P_t
 use units,          ONLY:HARTREE
 use electrons,      ONLY:levels
 use R_lattice,      ONLY:bz_samp
 use com,            ONLY:core_io_path,jobstr,msg,file_exists
 use fields,         ONLY:Efield,i_Probe
 use real_time,      ONLY:NE_steps,RT_step
 implicit none
 !
 type(levels)  :: en
 type(bz_samp) :: k
 !
 integer,     parameter   :: N_max_freqs=100
 integer                  :: i1,N_freqs,i_t
 real(SP),    parameter   :: W_step=0.05
 character(lchlen)        :: jobstr_save
 real(SP)                 :: Frequency(N_max_freqs),Wo,Time
 real(SP), allocatable    :: RT_J_freq(:,:),RT_P_freq(:,:)
 complex(SP)              :: X_effective(N_max_freqs),prefactor
 !
 call section("=",'Effective Response function ')
 !===============================================
 !
 jobstr_save=jobstr
 !
 N_freqs=0
 do i1=1,N_max_freqs
   write (jobstr,'(a,f3.2)') 't1_',W_step*i1
   if (file_exists(trim(jobstr)//'/ndb.RT_GreenF')) then
     N_freqs=N_freqs+1
     Frequency(N_freqs)=W_step*i1
   endif
 enddo
 !
 if (N_freqs==0) return
 !N_freqs=1
 !
 call msg('s',':: Frequency range   [eV]:',(/minval(Frequency(:N_freqs)),maxval(Frequency(:N_freqs))/))
 call msg('s','')
 !
 allocate(RT_J_freq(NE_steps,N_freqs),RT_P_freq(NE_steps,N_freqs))
 !
 do i1=1,N_freqs
   write (jobstr,'(a,f3.2)') 't1_',Frequency(i1)
   call IO_and_Messaging_switch("-log")
   call RT_databases(en)
   call IO_and_Messaging_switch("+log")
   call msg('s','')
   !
   Frequency(i1)=Efield(i_Probe)%frequency
   !
   RT_J_freq(:,i1)=real(RT_J_t(:,1))
   RT_P_freq(:,i1)=real(RT_P_t(:,1))
   !
 enddo
 !
 Wo    =en%E(2,1,1)-en%E(1,1,1)
 !
 do i_t=1,NE_steps
   !
   Time=i_t*RT_step
   !
   do i1=1,N_freqs
     !
     prefactor=exp(cI*Frequency(i1)*Time)/2./Efield(i_Probe)%amplitude
     X_effective(i1)=prefactor*( RT_P_freq(i_t,i1)+cI*RT_J_t(i_t,i1)/Wo)
     !
     if (i1==1) write (100,'(i5,2f20.10)') i_t,X_effective(i1)
     !if (mod(i_t,1000)==0.or.i_t==1) write (100+i_t,'(2f20.10)') real(Frequency(i1))*HARTREE,real(X_effective(i1))
     !
   enddo
 enddo
 !
end subroutine
