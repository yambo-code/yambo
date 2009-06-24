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
subroutine K_doubles_diagonalization(W,max_n_poles,BS_E_out,n_poles_out)
 !
 ! STEP [0-1]  
 ! The subroutine diagonalize the BSE eigenvalue equation with the second order
 ! frequency dependent kernel over a grid and stores the frequency dependent solutions
 ! E_I(\omega) in a matrix [BS_E_real_freq(icv,iw)]
 ! STEP [2]  !Not yet implemented!
 ! Manage degeneracies among the eigenvalue as well as try to follow
 ! the path of each eigenvalue in the frequency domain describing correctly
 ! what happen befor and after accidental degeneracies
 ! STEP [3]
 ! Then call K_fixed_point.F to solve the fixed point problem: $\omega-E_I(\omega)=0$,
 ! with $I=1,BS_K_dim$ for each frequency.
 ! In general it will found N solutions with $BS_K_dim<= N <= (BS_K_dim+BS_K_doubles_dim)$
 ! STEP [4] 
 ! The next step is to diagonalize again the problem at the energy of all the solutions
 ! and for each diagonalization to store the eigenvalue corrispondent to the self-consistent
 ! solution $M(\E_I)=A^I(E_I) E_I(E_I)$.
 ! STEP [5] 
 ! Finally it will compute the doubles part of the eigenvalues for each solution
 ! according to the equation:
 ! $A^I_d(E_I)=\sum_s  \frac{C*_{ds}}{E_I-E_d} A^I_s(E_I)$
 ! Here s={cv},d={c1c2,v1v2}
 ! 
 !
 use pars,           ONLY:SP
 use BS,             ONLY:BS_mat,BS_K_dim,BSS_rhoq0
 use BS_doubles,     ONLY:BS_doubles_E,BS_K_doubles_dim,BS_mat_doubles,&
&                         BS_doubles_table,BS_table_fixed_E,Eigen_singles,&
&                         Eigen_doubles,BS_mat_tmp,&
! Degeneracy
&                         change,inv_change,tab_poles2iw,tab_check2icv_zero,degeneracy_zero,&   
! DOUBLES CORR PROJECT
&                         doubles_corr
 use frequency,      ONLY:W_duplicate,W_reset,w_samp
 use units,          ONLY:HARTREE
 use matrix_operate, ONLY:mat_dia_inv,DIAGO,USE_LK
 use IO_m,           ONLY:io_control,OP_RD_CL,REP,VERIFY
 use memory_m,       ONLY:mem_est
 use par_proc_m,     ONLY:pp_redux_wait,pp_indexes,myid,pp_indexes_reset
 use par_indexes_m,  ONLY:par_indexes
 use timing,         ONLY:live_timing
 !
 implicit none
 ! Paral
 type(pp_indexes)         ::px
 !
 ! Input variables
 integer                  :: max_n_poles
 type(w_samp)             :: W
 !
 ! Output variables
 complex                  :: BS_E_out(max_n_poles)
 integer                  :: n_poles_out
 !
 ! Dummies
 integer                  :: iw,icin,i1,i2,i_molt,icv,icv1,icv2,i_solution,&
&                            i_doubles,ic1,ic2,iv1,iv2,i_pole,n_check
 !
 ! Work space
 integer                  :: max_doubles_contrib,lib_driver,ncin
 integer    , allocatable :: tab_poles2solutions(:)
 real(SP),    allocatable :: BS_E_real_freq(:,:),BS_E_tmp(:)
 integer    , allocatable :: Eigen_doubles_index(:,:,:)
 real(SP)                 :: doubles_norm(2),norm,check
 complex(SP), allocatable :: Eigen_doubles_tmp(:,:)
 logical                  :: print_eigen_doubles
 ! From blas library
 real(SP)                 :: scnrm2
 !
 ! New frequency grid
 type(w_samp)             :: W_new
 real(SP)                 :: E_min,E_max,delta
 complex(SP)              :: E_start,E_end 
 !
 ! Degeneracies
 integer                  :: pos_max(BS_K_dim,W%n(1))
 !
 ! I/O
 integer                  :: i_err,ID
 integer, external        :: ioBS_doubles
 !
 !
 ! Sectioning
 !
 call section('=','Doubles diagonalization solver')
 !
 ! Inizialisation
 max_doubles_contrib=BS_K_dim  ! 2*(BS_K_dim**2)
 print_eigen_doubles=.false.
 lib_driver=USE_LK
 ncin=2
 if(doubles_corr) ncin=1
 !
 ! ******************
 ! **  Allocation  **
 ! ******************
 allocate(BS_mat_tmp(BS_K_dim,BS_K_dim,2))
 call mem_est("BS_mat_tmp",(/(BS_K_dim**2)*4/))
 allocate(BS_E_tmp(BS_K_dim))
 call mem_est("BS_E_tmp",(/BS_K_dim/),(/SP/))
 allocate(BS_E_real_freq(BS_K_dim,W%n(1)))
 call mem_est("BS_E_real_freq",(/BS_K_dim,W%n(1)/),(/SP/))
 allocate(Eigen_singles(BS_K_dim,max_n_poles))
 allocate(Eigen_doubles(max_doubles_contrib,max_n_poles),Eigen_doubles_index(max_doubles_contrib,max_n_poles,5))
 call mem_est("Eigen_singles Eigen_doubles",&
&              (/BS_K_dim*max_n_poles,max_doubles_contrib*max_n_poles/),(/SP,SP/))
 allocate(Eigen_doubles_tmp(BS_K_doubles_dim,ncin))
 call mem_est("Eigen_doubles_tmp",(/BS_K_doubles_dim*ncin/),(/SP/))
 Eigen_singles(:,:)=0.
 Eigen_doubles(:,:)=0.
 Eigen_doubles_index(:,:,:)=-1
 allocate(BS_mat_doubles(BS_K_dim,BS_K_doubles_dim,ncin+1))
 call mem_est("BS_mat_doubles",(/size(BS_mat_doubles)*2/))
 allocate(BS_doubles_E(BS_K_doubles_dim))
 call mem_est("BS_doubles_E",(/size(BS_doubles_E)/))
 allocate(BS_doubles_table(BS_K_doubles_dim,4))
 !
 !
 ! Read the doubles database
 call io_control(action=OP_RD_CL,COM=REP,MODE=VERIFY,SEC=(/1,2/),ID=ID)
 i_err=ioBS_doubles(ID)
 !
 ! ***********************
 ! *****  Main loop  *****
 ! ***********************
 !DEBUG <
 !open(unit=677,file='diag_solution.txt', status='unknown')
 !open(unit=688,file='eigen_vectors.txt', status='unknown')
 !DEBUG >
 !
 !        *****  STEP [0]  *****
 ! Define a new frequency grid
 ! with smaller intervals in the region where doubles energies
 ! lays, so where the kernel has a strong frequency dependence
 i1=1
 i2=W%n(1)
 iw=1
 E_min=minval(BS_doubles_E)
 E_max=maxval(BS_doubles_E)
 E_start=W%p(1)
 E_end=W%p(W%n(1))
 delta=real(E_end-E_start)/(W%n(1)-1)
 call W_duplicate(W,W_new)
 ! If the eigenvalues lay out of the frequency interval W_new=W else
 ! the new grid is computed
 if((.not.(E_min<real(E_start).and.E_max<real(E_start)))&
&    .and.(.not.(E_min>real(E_end).and.E_max<real(E_end)))) then
   ! Grid with large intervals.
   if (E_min-2/HARTREE>real(W%p(1))) then
     do iw=1,W%n(1),4
       W_new%p(i1)=W%p(iw)
       i1=i1+1
       if (real(W%p(iw))>(E_min-2/HARTREE)) exit 
     enddo
   endif
   if (E_max+2/HARTREE<real(W%p(W%n(1)))) then
     do iw=W%n(1),1,-4
       W_new%p(i2)=W%p(iw)
       i2=i2-1
       if (real(W%p(iw))<(E_max+2/HARTREE)) exit
     enddo
   endif
   ! Grid with standard intervals.
   if (E_min-2/HARTREE>real(W%p(1))) then
     do iw=i1,i2
       W_new%p(iw)=W_new%p(i1-1)+(1.,0.)*delta*(iw-i1+1)
       i1=i1+1
       if (real(W_new%p(iw))>(E_min-0.2/HARTREE)) exit 
     enddo
     E_start=W_new%p(iw)+delta
   endif
   if (E_max+2/HARTREE<real(W%p(W%n(1)))) then
     do iw=i2,i1,-1
       W_new%p(iw)=W_new%p(i2+1)+(1.,0.)*delta*(iw-i2-1)
       i2=i2-1
       if (real(W_new%p(iw))<(E_max+0.2/HARTREE)) exit 
     enddo
     E_end=W_new%p(iw)-delta
   endif
   ! Grid with smaller intervals
   delta=real(E_end-E_start)/(i2-i1)
   do iw=i1,i2
     W_new%p(iw)=E_start+(1.,0.)*delta*(iw-i1)
   enddo
   !
 endif
 !
 !        *****  STEP [1] *****
 !  diagonalization over the new grid
 !
 ! Paral+timing<
 call pp_indexes_reset(px)
 call par_indexes(px,(/W_new%n(1)/),(/1/))
 call pp_redux_wait
 call live_timing('Diag over a grid',px%stps(myid+1))
 ! Paral+timing >
 !
 !
 BS_E_real_freq(:,:)=0.
 pos_max(:,:)=0.
 BS_mat_tmp(:,:,1)=BS_mat(:,:)
 do iw=1,W_new%n(1)
   ! Paral+timing <
   if (.not.px%i1p(iw)) cycle
   call live_timing(steps=1)
   ! Paral+timing >
   call K_doubles_mount(W_new%p(iw))
   call mat_dia_inv(DIAGO,lib_driver,BS_mat,E_real=BS_E_tmp)
   BS_E_real_freq(:,iw)=BS_E_tmp(:)
   do icv1=1,BS_K_dim
     pos_max(icv1,iw)=maxloc(abs(BS_mat(:,icv1)),1)
   enddo
   BS_mat(:,:)=BS_mat_tmp(:,:,1)
 enddo
 !
 ! Paral+timing <
 call live_timing()
 call pp_redux_wait(BS_E_real_freq)
 call pp_redux_wait(pos_max)
 call pp_indexes_reset(px)
 ! Paral+timing >
 !
 !        *****  STEP [2]  *****
 ! Here degeneracies are computed (not yet implemented)
 ! Presently K_doubles_degeneracy is a fake subroutine
 call K_doubles_degeneracies(BS_E_real_freq,pos_max,W_new,n_check)
 !
! ! DEBUG <
! do iw=1,W%n(1)
!   write(677,*)  real(W_new%p(iw))*HARTREE,&
!&                (BS_E_real_freq(change(icv1,iw),iw)*HARTREE,icv1=1,BS_K_dim)
! enddo
! ! DEBUG >
 !
 !        *****  STEP [3]  *****
 ! Here we solve the fixed problem point
 call K_fixed_point(BS_E_real_freq,BS_E_out,max_n_poles,n_check,n_poles_out,W_new)
 call W_reset(W_new)
 ! Compute the total number of poles and the table poles2solutions
 ! This can differ as each degenerate poles count for more than one 
 ! solution
 i1=0
 allocate(tab_poles2solutions(n_poles_out))
 do i_pole=1,n_poles_out
   icv=BS_table_fixed_E(i_pole,1)
   i1=i1+1+degeneracy_zero(icv)
   tab_poles2solutions(i_pole)=i1
 enddo
 !  
 !        *****  STEP [4]  *****
 ! Here we compute the eigenvectors for each solution
 !
 ! Paral <
 ! The parallelizatoin is not 100% efficient as some CPU has to compute
 ! poles with degeneracies, some other without. Anyway each CPU diagonalize
 ! the same number of time the BS_mat matrix.
 ! Presently thi is not a problem as K_doubles_degeneracy is a fake subroutine
 ! and all poles have degeneracy 1
 call par_indexes(px,(/n_poles_out/))
 call pp_redux_wait
 call live_timing('Eigenvalues',px%stps(myid+1))
 ! Paral >
 do i_pole=1,n_poles_out
   ! Paral <
   if (.not.px%i1p(i_pole)) cycle
   call live_timing(steps=1)
   ! Paral >   
   iw=tab_poles2iw(i_pole)
   icv=BS_table_fixed_E(i_pole,1)
   i_molt=BS_table_fixed_E(i_pole,2)
   icv1=change(icv,iw)
   call K_doubles_mount(real(BS_E_out(i_pole)))
   call mat_dia_inv(DIAGO,lib_driver,BS_mat,E_real=BS_E_tmp) 
   do i2=1,degeneracy_zero(icv)+1
     i_solution=tab_poles2solutions(i_pole)+degeneracy_zero(icv)
     Eigen_singles(:,i_pole)=BS_mat(:,icv1+i2-1)
 !        *****  STEP [5]  *****
 ! Here we compute the doubles for each eigenvector
     norm=0
     do icin=1,ncin
       call cgemv('c',BS_K_dim,BS_K_doubles_dim,(1._SP,0._SP),&
&        BS_mat_doubles(:,:,icin),BS_K_dim,Eigen_singles(:,i_solution),1,&
&        (0._SP,0._SP),Eigen_doubles_tmp(:,icin),1)
       forall (i_doubles=1:BS_K_doubles_dim) Eigen_doubles_tmp(i_doubles,icin)=&
&              Eigen_doubles_tmp(i_doubles,icin)/(BS_E_out(i_pole)-BS_doubles_E(i_doubles))
       doubles_norm(icin)=scnrm2(BS_K_doubles_dim,Eigen_doubles_tmp(:,icin),1)
       doubles_norm(icin)=doubles_norm(icin)**2
       norm=norm+doubles_norm(icin)
     enddo
     norm=norm+1
     Eigen_doubles_tmp(:,:)=Eigen_doubles_tmp(:,:)/sqrt(norm)
     Eigen_singles(:,i_solution)=Eigen_singles(:,i_solution)/sqrt(norm)
     if(print_eigen_doubles) then
       check=0.01
       if ((norm-1)/norm > 0.5) check=0.1
       i1=0
       do icin=1,ncin
         do i_doubles=1,BS_K_doubles_dim
           if ( abs(Eigen_doubles_tmp(i_doubles,icin)) < check) cycle
           i1=i1+1
           Eigen_doubles(i1,i_solution)=Eigen_doubles_tmp(i_doubles,icin)
           iv1=BS_doubles_table(i_doubles,1)
           iv2=BS_doubles_table(i_doubles,2)
           ic1=BS_doubles_table(i_doubles,3)
           ic2=BS_doubles_table(i_doubles,4)
           Eigen_doubles_index(i1,i_solution,:)=(/iv1,iv2,ic1,ic2,icin/)
           if(i1==max_doubles_contrib.and.i_doubles/=BS_K_doubles_dim) then
!             write(*,*) 'Warning: reached max_doubles_contrib.',&
!&                       'Some relavant doubles component missing in the report.'
             exit
           endif
         enddo
       enddo
       ! DEBUG <
       !write(688,*) '  '
       !do i1=1,max_doubles_contrib
       !  write(688,*) Eigen_doubles(i1,i_solution), Eigen_doubles_index(i1,i_solution,:),&
!&                     i_solution,100*(norm-1)/norm,'%'
       !enddo
       ! DEBUG >
     endif
     !
   enddo
   BS_mat(:,:)=BS_mat_tmp(:,:,1)
 enddo
 !
 ! Paral <
 call live_timing()
 call pp_redux_wait(Eigen_singles)
 if(print_eigen_doubles) call pp_redux_wait(Eigen_doubles)
 ! Paral >
 !
 !
 !DEBUG <
 !close(unit=677)
 !close(unit=688)
 !DEBUG >
 ! Set the correct number of poles considering the degeneracies
 icv=BS_table_fixed_E(n_poles_out,1)
 n_poles_out=tab_poles2solutions(n_poles_out)+degeneracy_zero(icv)
 !
 !
 ! Deallocate
 deallocate(BS_mat_tmp,BS_mat_doubles)
 call mem_est("BS_mat_tmp BS_mat_doubles")
 deallocate(BS_E_tmp,BS_E_real_freq,BS_doubles_E)
 call mem_est("BS_E_tmp BS_E_real_freq BS_doubles_E")
 deallocate(Eigen_doubles_tmp)
 call mem_est("Eigen_doubles_tmp")
 deallocate(BS_doubles_table,Eigen_doubles_index)
 deallocate(tab_poles2solutions)
 !
 ! ***** DEGENERACY DEALLOCATION *****
 deallocate(change)
 deallocate(tab_check2icv_zero,degeneracy_zero)
 deallocate(tab_poles2iw)
 !
end subroutine
