!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM,AF,DS
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
integer function eval_G_minus_G(iG,iGo,COMM)
 !
 ! Evaluates the  G-G' table :
 !
 !  g_vec( G_m_G(i,j) ) = g_vec(i)-g_vec(j)
 !
 ! and returns the orginal iG that is redefined in output
 ! in such a way that G_m_G(iG,j) exists for all j
 !
 use pars,         ONLY:SP
 use vec_operate,  ONLY:iku_v_norm
 use parallel_m,   ONLY:MPI_comm,PP_redux_wait,PP_indexes,PP_indexes_reset,myid
 use R_lattice,    ONLY:G_m_G,g_vec,ng_in_shell,n_g_shells,minus_G,E_of_shell
 use interfaces,   ONLY:PARALLEL_index,PARALLEL_live_message
 use timing_m,     ONLY:timing
#include<memory.h>
 !
 integer :: iG,iGo
 type(MPI_comm), optional :: COMM
 !
 ! Work Space
 !
 integer :: i1,i2,is
 integer :: iG_shell,iGo_shell,iG_shell_0,iG_shell_max,iG_,iGo_
 real(SP):: E_iG, E_iGo, E_max
 real(SP):: v1(3)
 real(SP), allocatable :: E_G_m_G(:,:)
 type(PP_indexes) :: PAR_IND_G
 !
 integer, external :: G_index
 !
 ! Init
 !
 if (allocated(G_m_G)) then
   eval_G_minus_G=size(G_m_G,1)
   return
 endif
 !
 call timing("eval_G_minus_G",opr="start")
 !
 iG_=iG
 iGo_=iGo
 if (iGo==0) iGo_=iG
 !
 ! aux data
 YAMBO_ALLOC(E_G_m_G,(iG_,iGo_))
 !
 ! setup MPI parallelism if required
 !
 call PP_indexes_reset(PAR_IND_G)
 if (present(COMM)) then
   !
   call PARALLEL_index(PAR_IND_G,(/iGo_/),COMM=COMM)
   !
   call PARALLEL_live_message("[G_m_G] G-vectors",LOADED=PAR_IND_G%n_of_elements(myid+1),TOTAL=iGo_)
   !
 endif
 !
#if defined _OPENMP
!$omp parallel do default(shared), private(i2,i1,v1), schedule(dynamic)
#endif
 do i2=1,iGo_
   !
   if(present(COMM)) then
     E_G_m_G(:,i2)=0.0_SP
     if (.not.PAR_IND_G%element_1D(i2)) cycle
   endif
   !
   do i1=1,iG_
     v1(:)=g_vec(i1,:)-g_vec(i2,:)
     E_G_m_G(i1,i2)=0.5_SP*iku_v_norm(v1)**2
   enddo
 enddo
#if defined _OPENMP
!$omp end parallel do
#endif
 !
 if (present(COMM)) call PP_redux_wait(E_G_m_G,COMM=COMM%COMM)

 !
 ! Find the shell corresponding to iGo
 !
 do i1=1,n_g_shells
   if (ng_in_shell(i1)<=iG_)  iG_shell=i1
   if (ng_in_shell(i1)<=iGo_) iGo_shell=i1
 enddo
 if (ng_in_shell(iG_shell)  < iG_ )  iG_shell=iG_shell+1
 if (ng_in_shell(iGo_shell) < iGo_ ) iGo_shell=iGo_shell+1
 !
 iG_shell_max=iG_shell
 E_max=(1+1.E-5_SP)*E_of_shell(n_g_shells)
 !
 ! define the a lower bound of the G shell
 !
 iG_shell_0=iG_shell_max
 !
 shell1_loop:&
 do is=2,iG_shell_max
   !
   iG_shell=is
   if (iGo==0) iGo_shell=iG_shell
   !
   E_iG=E_of_shell(iG_shell)
   E_iGo=E_of_shell(iGo_shell)
   !
   if ( sqrt(E_iG)+sqrt(E_iGo) > sqrt(E_of_shell(n_g_shells)) ) then
     iG_shell_0=is-1
     exit shell1_loop
   endif
   !
 enddo shell1_loop
 !
 ! search for a larger shell (if any)
 !
 iG_shell=iG_shell_0
 !
 shell2_loop:&
 do is=iG_shell_0,iG_shell_max
   !
   iG_= ng_in_shell(is)
   if (iGo==0) iGo_=iG_
   !
   if( maxval(E_G_m_G(1:iG_,1:iGo_)) > E_max ) then
      iG_shell=is-1
      exit shell2_loop
   endif
   !
 enddo shell2_loop
 !
 iG_= ng_in_shell(iG_shell)
 eval_G_minus_G=iG_
 if (iGo==0) iGo_=iG_
 !
 ! Fill the actual G_m_G matrix
 !
 YAMBO_ALLOC(G_m_G,(iG_,iGo_))
 G_m_G=0
 !
#if defined _OPENMP
!$omp parallel do default(shared), private(i2,i1,v1), schedule(dynamic)
#endif
 do i2=1,iGo_
   !
   if(present(COMM)) then
     G_m_G(:,i2)=0
     if (.not.PAR_IND_G%element_1D(i2)) cycle
   endif
   !
   do i1=1,iG_
     !
     v1(:)=g_vec(i1,:)-g_vec(i2,:)
     G_m_G(i1,i2)=G_index(v1)
     !
   enddo
 enddo
#if defined _OPENMP
!$omp end parallel do
#endif
 ! 
 if (present(COMM)) call PP_redux_wait(G_m_G,COMM=COMM%COMM)
 !
 call PP_indexes_reset(PAR_IND_G)
 YAMBO_FREE(E_G_m_G)
 !
 call timing("eval_G_minus_G",opr="stop")
 !
end function
