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
subroutine RT_X_effective(en)
 !
 use pars,           ONLY:SP,lchlen,cI,schlen,pi
 use YPP,            ONLY:RT_J_t,RT_P_t,X_effective_keyword,i_t_start,i_t_end,i_t_jump
 use units,          ONLY:HARTREE,FS2AUT 
 use electrons,      ONLY:levels
 use com,            ONLY:core_io_path,jobstr,msg,file_exists
 use fields,         ONLY:Efield,i_Probe
 use real_time,      ONLY:NE_steps,RT_step,RT_output
 use stderr,         ONLY:string_split,cstr
 use vec_operate,    ONLY:sort
 use timing,         ONLY:live_timing
 implicit none
 !
 type(levels)  :: en
 !
 integer, parameter :: N_max_folders=10000
 !
 character(lchlen) :: jobstr_save
 character(schlen) :: folder(N_max_folders)
 integer           :: nf,N_freqs,i_t,i_f,i_fp,f_indx(N_max_folders)
 real(SP)          :: Wo,TIME
 complex(SP)       :: EPS
 logical           :: IO_error
 character(10*N_max_folders) :: folder_list
 real(SP),    allocatable    :: RT_J_freq(:,:),RT_P_freq(:,:),Frequency(:)
 complex(SP), allocatable    :: X_effective(:)
 !
 call section("=",'Effective Response function ')
 !===============================================
 !
 call ifolder_list(cstr(trim(core_io_path)),folder_list,nf)
 !
 call string_split(folder_list(:nf),folder)
 !
 nf=count( len_trim(folder(:)) > 0 ) 
 !
 N_freqs=0
 do i_f=1,nf
1  if (file_exists(trim(core_io_path)//"/"//trim(folder(i_f))//'/ndb.RT_GreenF').and.&
&      ( trim(X_effective_keyword)=="none".or.index(trim(folder(i_f)),trim(X_effective_keyword))>0 ) ) then
     N_freqs=N_freqs+1
   else
     forall (i_fp=i_f+1:nf) folder(i_fp-1)=folder(i_fp)
     nf=nf-1
     if (nf<=0) exit
     goto 1
   endif
 enddo
 !
 if (N_freqs==0) return
 !
 jobstr_save=jobstr
 !
 do i_f=1,N_freqs
   !
   jobstr=trim(folder(i_f))
   call IO_and_Messaging_switch("-log")
   call RT_databases(en,IO_error)
   call IO_and_Messaging_switch("+log")
   !
   if (i_f==1) then
     !
     !  Initialization 
     !
     call RT_init_X_one(.TRUE.)
     !
     allocate(RT_J_freq(NE_steps,N_freqs),RT_P_freq(NE_steps,N_freqs),&
&             X_effective(N_freqs),Frequency(N_freqs))
   endif
   if (i_f/=N_freqs) call msg('s','')
   !
   Frequency(i_f)=Efield(i_Probe)%frequency
   !
   RT_J_freq(:,i_f)=real(RT_J_t(:,1))
   RT_P_freq(:,i_f)=real(RT_P_t(:,1))
   !
 enddo
 !
 call msg('s',':: Frequency range   [eV]:',(/minval(Frequency),maxval(Frequency)/)*HARTREE)
 !
 jobstr=jobstr_save
 !
 call sort(Frequency,indx=f_indx(:N_freqs))
 !
 Wo    =en%E(2,1,1)-en%E(1,1,1)
 !
 call RT_output("open X_effective eps")
 !
 call live_timing('Output:',(i_t_end-i_t_start+1)/(i_t_jump+1)+1)
 !
 do i_t=i_t_start,i_t_end,i_t_jump+1
   !
   TIME=(i_t-1)*RT_step
   !
   do i_fp=1,N_freqs
     !
     i_f=f_indx(i_fp)
     !
     X_effective(i_fp)=cI*exp(-cI*Frequency(i_fp)*TIME)/2./Efield(i_Probe)%amplitude*&
&                      ( cI*RT_P_freq(i_t,i_f)+RT_J_freq(i_t,i_f)/Wo )
     !
     EPS=1.-4.*pi*X_effective(i_fp)
     !
     call RT_output('X_effective',VALUEs=(/aimag(EPS),real(EPS)/),E=real(Frequency(i_fp))*HARTREE,TIME=TIME/FS2AUT)
     !
     if (i_t+i_t_jump>i_t_end) call RT_output('eps',VALUEs=(/aimag(EPS),real(EPS),0._SP,0._SP/),E=real(Frequency(i_fp))*HARTREE)
     !
   enddo
   !
   call msg('o X_effective',' ',' ',INDENT=0)
   !
   call live_timing(steps=1)
   ! eps
 enddo
 !
 call live_timing()
 !
 call RT_output("close X_effective eps")
 !
 deallocate(RT_J_freq,RT_P_freq,X_effective,Frequency)
 !
end subroutine
