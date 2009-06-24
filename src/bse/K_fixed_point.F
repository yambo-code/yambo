!
! Copyright (C) 2000-2008 D. Sangalli and the YAMBO team 
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
subroutine K_fixed_point(E_in,E_out,n_E_theory,n_check_in,n_E,freq)
 use pars,          ONLY:SP
 use BS,            ONLY:BS_K_dim
 use BS_doubles,    ONLY:BS_K_doubles_dim,BS_table_fixed_E,&
&                        change,tab_poles2iw,tab_check2icv_zero,degeneracy_zero
 use frequency,     ONLY:w_samp
 use memory_m,      ONLY:mem_est
 !use par_proc_m,    ONLY:pp_redux_wait,pp_indexes,myid,pp_indexes_reset
 !use par_indexes_m, ONLY:par_indexes
 use timing,        ONLY:live_timing
 !
 implicit none
 ! Paral : to parallelize this we need to change how n_E
 !         is used in the tables...for future developments
 !type(pp_indexes)   ::px
 !
 ! Input variables
 type(w_samp)       :: freq
 integer            :: n_E_theory,n_check_in
 real               :: E_in(BS_K_dim,freq%n(1))
 !
 ! Output variables
 ! Even the table BS_table_fixed is allocated and 
 ! constructed from this subroutine
 complex            :: E_out(n_E_theory)
 integer            :: n_E
 !
 ! Work space
 integer            :: iw,i_check,i1,icv,icv1,icv1_old,i_E,i_molt,i_sign
 real(SP)           :: tmp_function,m,q
 !
 !
 ! We expect a number od excitations for the singlet case equal to
 ! n_singles+n_doubles. The number of solution that we will found
 ! with this approach will be n_E <= n_E_theory as doubles at high 
 ! energy will not give any contribution due to the finitness of the
 ! i\eta factor which cut the functions 1/\omega-E_d
 n_E=0
 !Allocation
 allocate(BS_table_fixed_E(n_E_theory,2))
 allocate(tab_poles2iw(n_E_theory))
 tab_poles2iw(:)=0
 BS_table_fixed_E(:,:)=0
 !
 ! DEBUG <
 open(unit=699,file='Fixed_solutions.txt', status='unknown')
 write(699,*) ' '
 ! DEBUG >
 !
 ! Paral <
 !call par_indexes(px,(/n_check_in/),(/1/))
 !call pp_redux_wait
 !call live_timing('E_out',px%stps(myid+1))
 call live_timing('E_out',n_check_in,.true.)
 ! Paral >
 !
 do i_check=1,n_check_in
   !
   ! Paral <
   !if (.not.px%i1p(i_check)) cycle
   call live_timing(steps=1)
   ! Paral >
   !
   i_molt=0
   i_sign=0
   icv=tab_check2icv_zero(i_check)
   if(E_in(icv,1)<0) then
     write(*,*) 'Warning: the static solution is negative!', &
&               'Skip the index icv: ',icv
     n_E=n_E+1
     E_out(n_E)=E_in(icv,1)
     BS_table_fixed_E(n_E,:)=(/icv,1/)
     tab_poles2iw(n_E)=1    
     cycle
   endif
   do iw=1,freq%n(1)
     icv1=change(icv,iw)
     tmp_function=real(freq%p(iw))-E_in(icv1,iw)
     ! The following "if" works in the loop after we found a pole untill the
     ! tmp_function change sign again. This second change should happen near 
     ! a doubles excitation energy. Anyway in practice the structure of the kernel
     ! and so of E_I(\omega) can be more complicated...
     if(mod(i_sign,2)==1) then
       if(tmp_function>0) cycle
       if(tmp_function<0) i_sign=i_sign+1
     endif
     if(tmp_function<0.or.iw==1) cycle
     if(tmp_function>0) then
       icv1_old=change(icv,iw-1)
       n_E=n_E+1
       i_molt=i_molt+1   
       i_sign=i_sign+1
       ! Here we do an interpolation among the two point where the
       ! function $\omega-E_I(\omega)$ change sign with y=mx+q
       ! The point y=0 ==> y=-q/m will be an approximation for
       ! the fixed point
       m=(real(freq%p(iw)-freq%p(iw-1))-E_in(icv1,iw)+E_in(icv1_old,iw-1))/real(freq%p(iw)-freq%p(iw-1))
       q=real(freq%p(iw))-E_in(icv1,iw)-m*real(freq%p(iw))
       E_out(n_E)=-q/m
       BS_table_fixed_E(n_E,:)=(/icv,i_molt/)
       tab_poles2iw(n_E)=iw    
       ! DEBUG <
!       write(699,*) "E_out:",E_out(n_E)*27.2116,"n_E:",n_E,"icv:",icv,&
!                    "i_molt",i_molt,"w",real(freq%p(iw))*27.2116    
!                    "i_molt",i_molt,"deg",degeneracy_zero(icv),"w",real(freq%p(iw))*27.2116    
       write(699,*) E_out(n_E)*27.2116,n_E,icv,i_molt,m,q*27.2116,iw,E_in(icv,1)*27.2116,real(freq%p(iw))*27.2116       
       ! DEBUG >
     endif
   enddo
 enddo
 !
 ! Paral <
 call live_timing()
 !call pp_redux_wait
 !call pp_indexes_reset(px)
 ! Paral >
 !
 ! DEBUG <
 ! close(unit=699)
 ! DEBUG >
 !
 return 
 !
end subroutine
