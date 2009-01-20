!
! Copyright (C) 2000-2008 A. Marini and the YAMBO team 
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
subroutine K_diagonalization(iq,W) 
 !
 ! Generic diagonalization method to solve resonant and non resonant
 ! Hamiltonians.
 !
 use pars,           ONLY:SP,DP,pi
 use X_m,            ONLY:X_epsilon
 use BS,             ONLY:BS_mat,BS_K_dim,BSS_rhoq0,Kernel_by_V,&
&                         BS_K_coupling,BSS_write_eig_2_db,BS_eh_f,BS_not_const_eh_f,&
&                         BS_eh_W
#if defined _ELPH
 use BS,             ONLY:BS_eh_Eo,BS_eh_E
#endif
 use memory_m,       ONLY:mem_est
 use electrons,      ONLY:spin_occ
 use matrix_operate, ONLY:mat_dia_inv,DIAGO,USE_LK,USE_SLK,INV
 use frequency,      ONLY:w_samp
 use R_lattice,      ONLY:d3k_factor,q_norm
 use com,            ONLY:isec
 use timing,         ONLY:live_timing
 use par_proc_m,     ONLY:pp_redux_wait,pp_indexes,ncpu,myid,pp_indexes_reset
 use par_indexes_m,  ONLY:par_indexes
 use IO_m,           ONLY:io_control,OP_RD_CL,REP,VERIFY,OP_WR_CL
 use parser_m,       ONLY:parser
 implicit none
 type(w_samp)  :: W
 integer       :: iq
 !
 ! Work Space
 !
 type(pp_indexes)     ::px
 integer              ::i1,lib_driver,BS_H_dim
 logical              ::K_is_not_hermitian
#if defined _DOUBLE
 complex(DP):: zdotu,zdotc
#else
 complex(SP):: cdotu,cdotc
#endif
 !
 ! Residuals & Energies
 !
 complex(SP), allocatable ::BS_R(:) 
 complex(SP), allocatable ::BS_E(:)
 !
 ! Resonant K
 !
 real(SP),    allocatable ::BS_E_real(:)
 !
 ! Coupling
 !
 complex(SP), allocatable ::BS_V_left(:,:)
 complex(SP), allocatable ::BS_V_right(:,:)
 complex(SP), allocatable ::BS_R_left(:)
 complex(SP), allocatable ::BS_R_right(:)
 complex(SP), allocatable ::BS_overlap(:,:)
 !
 ! I/O
 !
 integer           ::io_err,ID
 integer, external ::ioBSS_diago
 !  
 ! Sectioning
 !
 if (isec(2)/=0) then
   call section('=','Diagonalization solver')
 else if (isec(2)==0) then
   call section('+','Diagonalization solver')
 endif
 !
 ! Eigenstates 2 DB ?
 !
 if (.not.BSS_write_eig_2_db) call parser('WRbsWF',BSS_write_eig_2_db)
 !
 ! Allocation & Par Procs
 !
 K_is_not_hermitian=BS_K_coupling.or.allocated(BS_eh_W)
 !
 BS_H_dim=BS_K_dim
 if (BS_K_coupling) BS_H_dim=2*BS_K_dim
 !
 allocate(BS_R(BS_H_dim),BS_E(BS_H_dim))
 call mem_est("BS_R BS_E",(/BS_H_dim,BS_H_dim/))
 !
 if (K_is_not_hermitian) then
   allocate(BS_V_left(BS_H_dim,BS_H_dim),&
&           BS_V_right(BS_H_dim,BS_H_dim))
   call mem_est("BS_V_left BS_V_right",&
&              (/BS_H_dim,size(BS_V_left),size(BS_V_right)/))
   allocate(BS_R_left(BS_H_dim),BS_R_right(BS_H_dim),&
&           BS_overlap(BS_H_dim,BS_H_dim))
   call mem_est("BS_R_left BS_R_right BS_overlap",&
&              (/BS_H_dim,BS_H_dim,size(BS_overlap)/))
 else
   allocate(BS_E_real(BS_H_dim))
   call mem_est("BS_E_real",(/BS_H_dim/),(/SP/))
 endif
 !
 call pp_indexes_reset(px)
 call par_indexes(px,(/BS_H_dim/))
 !
 ! Diagonalization DB (IN)
 !
#if defined _ELPH
 !
 allocate(BS_mat(BS_H_dim,BS_H_dim))
 call mem_est('BS_mat',(/2*size(BS_mat)/))
 !
 call io_control(ACTION=OP_RD_CL,COM=REP,MODE=VERIFY,SEC=(/1,2,3/),ID=ID)
 io_err=ioBSS_diago(iq,BS_H_dim,BS_E,BS_R,ID)
 !
#else
 call io_control(ACTION=OP_RD_CL,COM=REP,MODE=VERIFY,SEC=(/1,2/),ID=ID)
 io_err=ioBSS_diago(iq,BS_H_dim,BS_E,BS_R,ID)
#endif
 !
 if(io_err<0) then
   !
   ! Kernel loading
   !
   call Kernel_by_V(iq)
   !
   ! eh damping 
   !
   if (allocated(BS_eh_W)) &
&     forall(i1=1:BS_K_dim) BS_mat(i1,i1)=BS_mat(i1,i1)+(0.,1.)*BS_eh_W(i1)
   !
   if (BS_not_const_eh_f) then
     !
     ! In case the f_i(1-f_j) occupation factors are state-dependent
     ! here I multiply the kernel by BS_eh_f
     !
     forall(i1=1:BS_K_dim) BS_mat(i1,:)=BS_mat(i1,:)*BS_eh_f(i1)
     if (K_is_not_hermitian) then
       forall(i1=BS_K_dim+1:BS_H_dim) BS_mat(i1,:)=BS_mat(i1,:)*BS_eh_f(i1-BS_K_dim)
     endif
   endif
   !
 endif
 !
 ! Initialize the output file 
 !
 call K_dump_to_o_file(iq,-2)
 !
 if(io_err<0) then
   !
   ! Diagonalization of the excitonic hamiltonian
   !
   lib_driver=USE_LK
   if (ncpu>1) lib_driver=USE_SLK
   call live_timing('BSK diagonalize',1)
   if (K_is_not_hermitian) then
     call mat_dia_inv(DIAGO,lib_driver,BS_mat,E_cmpl=BS_E,&
&                     V_left=BS_V_left,V_right=BS_V_right)
     !
     if (BSS_write_eig_2_db) BS_mat=BS_V_right
     !
   else
     call mat_dia_inv(DIAGO,lib_driver,BS_mat,E_real=BS_E_real)
     BS_E=BS_E_real
   endif
   call live_timing(steps=1)
   call live_timing
   !
   if (K_is_not_hermitian) then
     !
     !   The right eigenvector v(j) of A satisfies
     !                   A * v(j) = lambda(j) * v(j)
     !  where lambda(j) is its eigenvalue.
     !  The left eigenvector u(j) of A satisfies
     !                u(j)**H * A = lambda(j) * u(j)**H
     !  where u(j)**H denotes the conjugate transpose of u(j).
     !
     ! Remember that
     !
     ! 1/(w-H)= \sum_ij |i right><i left|j right>^{-1) <j left| /(w-E_i)
     !
     ! [1] BS_R_right(i)=<q0|j><j|i_R>
     !                  =conjg(BSS_rhoq0(j))*BS_V_right(j,i)
     !
     call live_timing('BSK R residuals',px%stps(myid+1))
     !
     do i1=1,BS_H_dim
       BS_R_right(i1)=cmplx(0.,0.,SP)
       if (.not.px%i1p(i1)) cycle
#if defined _DOUBLE
       BS_R_right(i1)=zdotc(BS_H_dim,BSS_rhoq0,1,BS_V_right(:,i1),1)
#else
       BS_R_right(i1)=cdotc(BS_H_dim,BSS_rhoq0,1,BS_V_right(:,i1),1)
#endif
       call live_timing(steps=1)
     enddo
     call pp_redux_wait(BS_R_right)
     call live_timing
     !
     ! [2] BS_R(i)= <i_K|j><j|q0> 
     !            = conjg( BS_V_left(k,i))*BSS_rhoq0(k)*R_k
     !
     !     R(j) = f_v - f_c
     !
     ! In the right residuals I include the occupation factors
     ! contribution
     !
     BSS_rhoq0(:BS_K_dim)  = BSS_rhoq0(:BS_K_dim)  *BS_eh_f(:BS_K_dim)
     BSS_rhoq0(BS_K_dim+1:)=-BSS_rhoq0(BS_K_dim+1:)*BS_eh_f(:BS_K_dim)
     !
     call live_timing('BSK L residuals',px%stps(myid+1))
     !
     do i1=1,BS_H_dim
       BS_R(i1)=cmplx(0.,0.,SP)
       if (.not.px%i1p(i1)) cycle
#if defined _DOUBLE
       BS_R(i1)=zdotc(BS_H_dim,BS_V_left(:,i1),1,BSS_rhoq0,1)
#else
       BS_R(i1)=cdotc(BS_H_dim,BS_V_left(:,i1),1,BSS_rhoq0,1)
#endif
       call live_timing(steps=1)
     enddo
     call pp_redux_wait(BS_R)
     !
     ! Remove the occupation factors from the oscillators (in case
     ! other solvers are used)
     !
     BSS_rhoq0(:BS_K_dim)  = BSS_rhoq0(:BS_K_dim)/  BS_eh_f(:BS_K_dim)
     BSS_rhoq0(BS_K_dim+1:)=-BSS_rhoq0(BS_K_dim+1:)/BS_eh_f(:BS_K_dim)
     !
     call live_timing
     !
     ! [3] BS_overlap(i,j)=conjg(BS_V_left(k,i))*BS_V_right(k,j)
     !
     call live_timing('BSK overlap mat',1)
#if defined _DOUBLE
     call zgemm('c','n',BS_H_dim,BS_H_dim,BS_H_dim,(1.,0.),&
&               BS_V_left,BS_H_dim,BS_V_right,BS_H_dim,(0.,0.),&
&               BS_overlap,BS_H_dim)
#else
     call cgemm('c','n',BS_H_dim,BS_H_dim,BS_H_dim,(1.,0.),&
&               BS_V_left,BS_H_dim,BS_V_right,BS_H_dim,(0.,0.),&
&               BS_overlap,BS_H_dim)
#endif
     call mat_dia_inv(INV,USE_SLK,BS_overlap)
     call live_timing(steps=1)
     call live_timing()
     !
     ! [4] BS_R_left(i)=BS_overlap(i,j)BS_R(j)
     !
     call live_timing('BSK L x overlap',px%stps(myid+1))
     do i1=1,BS_H_dim
       BS_R_left(i1)=cmplx(0.,0.,SP)
       if (.not.px%i1p(i1)) cycle
#if defined _DOUBLE
       BS_R_left(i1)=zdotu(BS_H_dim,BS_overlap(i1,:),1,BS_R,1)
#else
       BS_R_left(i1)=cdotu(BS_H_dim,BS_overlap(i1,:),1,BS_R,1)
#endif
       call live_timing(steps=1)
     enddo
     call pp_redux_wait(BS_R_left)
     call live_timing
     !
     ! [5] BS_R(i)=BS_R_left(i)BS_R_right(i)
     !
     do i1=1,BS_H_dim
       BS_R(i1)=BS_R_left(i1)*BS_R_right(i1)
     enddo
     !
   else
     !
     !Resonant Residuals
     !------------------
     !
     ! BS_R(i)=\sum_k <q0|k><k|i> =
     !         \sum_k BS_mat(k,i) x conjg( BSS_rhoq0(k) )
     !
     call live_timing('BSK   residuals',px%stps(myid+1))
     BS_R=(0.,0.)
     !
     do i1=1,BS_K_dim
       if (.not.px%i1p(i1)) cycle
#if defined _DOUBLE
       BS_R(i1)=zdotc(BS_K_dim,BSS_rhoq0,1,BS_mat(:,i1),1)
#else
       BS_R(i1)=cdotc(BS_K_dim,BSS_rhoq0,1,BS_mat(:,i1),1)
#endif
       call live_timing(steps=1)
     enddo
     call live_timing
     !
     call pp_redux_wait(BS_R)
     !
   endif
 endif
 !
 !Now I calculate epsilon
 !
 ! eps2(iw)= 1 - Sum  BS_R(K)/(w +I eta - E)     (coupling or non-hermitian)
 !           1 - Sum |BS_R(K)|^2/(w +I eta - E)  (resonant)
 !
 X_epsilon(1,:)=(0.,0.)
 X_epsilon(2,:)=(0.,0.)
 X_epsilon(3,:)=X_epsilon(3,:)/real(ncpu,SP)
 call live_timing('BSK res epsilon',px%stps(myid+1))
 do i1=1,BS_H_dim
   if (.not.px%i1p(i1)) cycle
   !
   ! Note the use of a causal expression here needed to avoid any
   ! distinction between the resonant and antiresonant e/h Green's
   ! functions.
   !
   if (K_is_not_hermitian)      X_epsilon(2,:)=&
&                               X_epsilon(2,:)-BS_R(i1)/(W%p(:)-BS_E(i1))
   if (.not.K_is_not_hermitian) X_epsilon(2,:)=&
&                               X_epsilon(2,:)-BS_R(i1)*conjg(BS_R(i1))/(W%p(:)-BS_E(i1))
   !
   call live_timing(steps=1)
 enddo
 call live_timing
 call pp_redux_wait(X_epsilon)
 !
 X_epsilon(2,:)=1.+X_epsilon(2,:)*&
&                  real(spin_occ)/(2.*pi)**3.*d3k_factor*4.*pi/(q_norm(1))**2
 !
 X_epsilon(1,:)=W%p(:)
 !
 ! I write the output file 
 !
 call K_dump_to_o_file(iq,2)
 !
 ! Diagonalization DB (OUT)
 !
 if (io_err/=0) then
   call io_control(ACTION=OP_WR_CL,COM=REP,MODE=VERIFY,SEC=(/1,2,3/),ID=ID)
   io_err=ioBSS_diago(iq,BS_H_dim,BS_E,BS_R,ID)
 endif
 !
#if defined _ELPH
 call eval_IP_contrib_to_E_lambda(BS_mat)
#endif
 !
 ! CLEAN
 !
 if (allocated(BS_mat)) deallocate(BS_mat)
 call mem_est("BS_mat")
 deallocate(BS_R,BS_E)
 call mem_est("BS_R BS_E")
 if (K_is_not_hermitian) then
   deallocate(BS_V_left,BS_V_right,BS_R_left,BS_R_right)
   call mem_est("BS_V_left BS_V_right BS_R_left BS_R_right")
 else
   deallocate(BS_E_real)
   call mem_est("BS_E_real")
 endif
 call pp_indexes_reset(px)
 !
#if defined _ELPH
 contains 
   !
   subroutine eval_IP_contrib_to_E_lambda(Eigen_V)
     use D_lattice, ONLY:Tel
     use units,     ONLY:KELVIN,HARTREE
     complex(SP) :: Eigen_V(BS_K_dim,BS_K_dim)
     complex(SP) :: Eigen_V_sq(BS_K_dim),dE_lambda(BS_K_dim),dE_o(BS_K_dim)
     integer     :: il,i2
     integer     :: Lambda_T_table(7,1000)
     real(SP)    :: BS_Eo(7)
     ! 
     Lambda_T_table( : ,:)=0
     BS_Eo=0.
     !
     ! Silicon
     !
     !BS_Eo=(/2.65271 , 3.41360 , 2.45073 , 3.61531 , 4.23670/)/HARTREE
     !Lambda_T_table(:,   15)=(/   44,  159,   38,  171,  222/)
     !Lambda_T_table(:,   30)=(/   46,  158,   29,  172,  222/)
     !Lambda_T_table(:,   50)=(/   32,   88,   28,  122,  223/)
     !Lambda_T_table(:,   75)=(/   32,   88,   24,  147,  225/)
     !Lambda_T_table(:,  100)=(/   33,   88,   22,  158,  223/)
     !Lambda_T_table(:,  150)=(/   44,  150,   31,  166,  221/)
     !Lambda_T_table(:,  200)=(/   32,   87,   28,  158,  232/)
     !Lambda_T_table(:,  243)=(/   46,  160,   39,  172,  236/)
     !Lambda_T_table(:,  297)=(/   34,   87,   25,  158,  222/)
     !Lambda_T_table(:,  350)=(/   32,  135,   23,  149,  229/)
     !Lambda_T_table(:,  465)=(/   33,  118,   26,  149,  236/)
     !Lambda_T_table(:,  500)=(/   34,  120,   22,  160,  243/)
     !Lambda_T_table(:,  600)=(/   46,  159,   39,  169,  245/)
     !Lambda_T_table(:,  676)=(/   45,   86,   28,  160,  243/)
     !Lambda_T_table(:,  793)=(/   32,  160,   26,  171,  243/)
     !Lambda_T_table(:,  874)=(/   44,  159,   36,  170,  243/)
     !Lambda_T_table(:,  972)=(/   46,  160,   39,  170,  244/)

     Lambda_T_table(:,   5)=(/    75,    81,    88,   371,   397,   406,   444/)
     Lambda_T_table(:,  20)=(/    75,    81,    88,   372,   397,   405,   445/)
     Lambda_T_table(:,  50)=(/    75,    81,    87,   372,   398,   417,   451/)
     Lambda_T_table(:, 100)=(/    75,    81,    88,   371,   397,   417,   444/)
     Lambda_T_table(:, 150)=(/    75,    81,    88,   160,   397,   422,   440/)
     Lambda_T_table(:, 200)=(/    63,    69,    88,   220,   389,   402,   434/)
     Lambda_T_table(:, 250)=(/    75,    81,   184,   384,   389,   404,   440/)
     Lambda_T_table(:, 300)=(/    63,    69,    83,   165,   177,   184,   222/)
     Lambda_T_table(:, 350)=(/    69,    75,    83,   163,   171,   178,   431/)
     Lambda_T_table(:, 400)=(/    69,    75,    83,   164,   395,   416,   440/)
     Lambda_T_table(:, 425)=(/    69,    75,    83,   352,   389,   396,   440/)
     Lambda_T_table(:, 450)=(/    69,    75,    83,   351,   396,   420,   439/)
     Lambda_T_table(:, 475)=(/    69,    75,    84,   163,   391,   410,   440/)
     Lambda_T_table(:, 500)=(/    69,    75,    84,   381,   398,   416,   441/)
     Lambda_T_table(:, 525)=(/    69,    75,    82,   388,   403,   420,   469/)
     Lambda_T_table(:, 550)=(/    69,    75,    84,   352,   412,   416,   432/)
     Lambda_T_table(:, 575)=(/    69,    75,    80,   224,   259,   279,   292/)
     Lambda_T_table(:, 600)=(/    69,    75,    79,   248,   296,   305,   342/)
     Lambda_T_table(:, 620)=(/    63,    75,    79,   223,   249,   303,   315/)
     Lambda_T_table(:, 640)=(/    70,    76,    78,   343,   366,   375,   387/)
     Lambda_T_table(:, 660)=(/    70,   139,   136,   352,   376,   385,   461/)
     Lambda_T_table(:, 680)=(/   130,   139,   134,   418,   443,   449,   461/)
     Lambda_T_table(:, 700)=(/    64,    73,    68,   345,   371,   379,   389/)
     Lambda_T_table(:, 720)=(/    70,    80,    74,   352,   379,   387,   399/)
     Lambda_T_table(:, 740)=(/    70,    81,    74,   352,   379,   384,   398/)
     Lambda_T_table(:, 760)=(/    70,   141,   134,   352,   386,   378,   461/)
     Lambda_T_table(:, 780)=(/    70,    81,    74,   345,   380,   372,   396/)
     Lambda_T_table(:, 800)=(/    64,    81,    68,   343,   382,   369,   388/)
     !
     forall(i1=1:BS_K_dim) dE_o(i1)=BS_eh_E(i1)-BS_eh_Eo(i1)+(0.,1.)*BS_eh_W(i1)
     !
     do i2=1,7
       il=Lambda_T_table(i2,nint(Tel*KELVIN))
       forall(i1=1:BS_K_dim) Eigen_V_sq(i1)=Eigen_V(i1,il)*conjg(Eigen_V(i1,il))
       dE_lambda(il)=cdotu(BS_K_dim,Eigen_V_sq,1,dE_o,1)
       if (i2==1) write (100,'(a,i10)') "T =",nint(Tel*KELVIN)
!       write (100,'(a,i3,1x,3(a,f7.3,1x))') 'INDX:',il,'Eo:',real(BS_Eo(i2))*HARTREE,&
!&                      'DeltaE/IPcontrib:',real(BS_E(il)-BS_Eo(i2))/real(dE_lambda(il))*100.,&
!&                      'DeltaG/IPcontrib:',aimag(BS_E(il))/aimag(dE_lambda(il))*100.
       write (100,'(a,i3,1x,4(a,f10.3,1x))') 'INDX:',il,'E:',real(BS_E(il))*HARTREE,&
&                      'dE:',real(dE_lambda(il))*HARTREE,&
&                      'Eo:',real(BS_E(il)-dE_lambda(il))*HARTREE,&
&                      'Go:',aimag(BS_E(il))*HARTREE*1000.
     enddo
   end subroutine
   !
#endif
end subroutine
