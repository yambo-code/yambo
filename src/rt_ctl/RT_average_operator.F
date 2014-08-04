!
!        Copyright (C) 2000-2014 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM CA DS
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
subroutine RT_average_operator(VALUE,O_REAL,O_QP,O_MATRIX,G,k,en,TRANS,RIM,nK_mem)
 !
 use pars,           ONLY:SP,cI
 use SC,             ONLY:SC_bands,SC_nbands
 use real_time,      ONLY:RIM_ibz_coo,RT_Nk_ibz,RT_nk,RT_ibz_coo
 use R_lattice,      ONLY:bz_samp 
 use QP_m,           ONLY:QP_n_states
 use electrons,      ONLY:n_sp_pol,levels
 use FFT_m,          ONLY:fft_size
 use wave_func,      ONLY:WF
#if defined _OPENMP
 use wrapper,        ONLY:Vstar_dot_V_omp
#else
 use wrapper,        ONLY:Vstar_dot_V
#endif
 use parallel_m,     ONLY:PP_redux_wait,PAR_IND_Xk_ibz,PAR_Xk_ibz_index,&
&                         PAR_COM_Xk_ibz_INDEX,PAR_COM_Plasma_INDEX
#if defined _TIMING
 use timing_m,       ONLY:timing
#endif
 !
 implicit none
 !
 type(bz_samp), intent(in) :: k
 complex(SP),   intent(in) :: G(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),RT_nk)
 integer,       intent(in) :: nK_mem
 complex(SP),  intent(out) :: VALUE
 complex(SP),     optional, intent(in) :: O_MATRIX(SC_bands(2),SC_bands(2),nK_mem,n_sp_pol)
 complex(SP),     optional, intent(in) :: O_QP(QP_n_states)
 real(SP),        optional, intent(in) :: O_REAL(fft_size,n_sp_pol)
 logical,         optional, intent(in) :: TRANS,RIM
 type(levels),    optional, intent(in) :: en
 !
 ! Work Space
 !
 integer     :: i1,ik,SC_bands2,ik_ibz,iE_rim
 complex(SP) :: Hk(SC_bands(2),SC_bands(2),n_sp_pol),VALUE_tmp
 logical     :: l_rim_local
 !
#if defined _TIMING
 call timing('RT average Op',OPR='start')
#endif
 !
 l_rim_local=.false.
 if(present(RIM)) l_rim_local=RIM
 !
 SC_bands2=SC_nbands**2
 VALUE=(0._SP,0._SP)
 !
 ! If not operator are present the subroutine returns the trace of the Green Function
 ! 
 if(.not.present(O_MATRIX).and..not.present(O_REAL).and..not.present(O_QP)) then
   !
   ! Total number of electrons
   !
   !do i1=1,SC_bands(1)-1
   !  VALUE=VALUE+sum(en%f(i1,:,1)*k%weights(:))
   !enddo
   do i1=SC_bands(1),SC_bands(2)
     do ik_ibz=1,k%nibz
       VALUE_tmp=(0._SP,0._SP)
       do ik=RT_ibz_coo(ik_ibz,1),RT_ibz_coo(ik_ibz,2)
         VALUE_tmp=VALUE_tmp-cI*G(i1,i1,ik)
       enddo
       VALUE=VALUE+VALUE_tmp*k%weights(ik_ibz)/real(RT_Nk_ibz(ik_ibz))
     enddo
   enddo
   !
   goto 1
   !
 endif 
 !
 do ik_ibz=1,k%nibz
   ! 
   if (k%nibz/=nK_mem) then
     if (.not.PAR_IND_Xk_ibz%element_1D(ik_ibz)) cycle   
   endif
   !
   if(.not.l_rim_local) call make_Hk_matrix(ik_ibz,1)
   !
   VALUE_tmp=(0._SP,0._SP)
   !
   do ik=RT_ibz_coo(ik_ibz,1),RT_ibz_coo(ik_ibz,2)
     !
     iE_rim=ik-RT_ibz_coo(ik_ibz,1)+RIM_ibz_coo(ik_ibz,1)
     if(l_rim_local) call make_Hk_matrix(ik_ibz,iE_rim)
     !
#if defined _OPENMP
     VALUE_tmp=VALUE_tmp-cI*Vstar_dot_V_omp(SC_bands2,Hk(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),1), & 
&                                             G(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),ik))
#else
     VALUE_tmp=VALUE_tmp-cI*Vstar_dot_V(SC_bands2,Hk(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),1), & 
&                                             G(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),ik))
#endif
   enddo
   !
   VALUE=VALUE+VALUE_tmp*k%weights(ik_ibz)/real(RT_Nk_ibz(ik_ibz))
   !
 enddo
 !
 if (present(O_QP).or.present(O_REAL).or.k%nibz/=nK_mem) then
   !
   call PP_redux_wait(VALUE,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
   !
   ! When the PAR_COM_Xk_ibz_INDEX%COMM is not defined then the job can be
   ! overdone. This occurs for a 1.M.P.N CPU structure
   !
   if (PAR_COM_Xk_ibz_INDEX%n_CPU==1) VALUE=VALUE/PAR_COM_Plasma_INDEX%n_CPU
   !
 endif
 !
1 continue
 !
#if defined _TIMING
 call timing('RT average Op',OPR='stop')
#endif
 !
 contains 
   !
   subroutine make_Hk_matrix(ik,iE_rim)
     implicit none
     integer, intent(in) :: ik,iE_rim
     integer :: i1,ik_mem,ib,ibp
     !
     ik_mem=ik
     if (k%nibz/=nK_mem) ik_mem=PAR_Xk_ibz_index(ik)
     !
     Hk=(0._SP,0._SP)
     !
     if (present(O_MATRIX)) then
       do ib=SC_bands(1),SC_bands(2)
         do ibp=SC_bands(1),SC_bands(2)
           do i1=1,n_sp_pol
             if (     present(TRANS)) Hk(ibp,ib,i1)=O_MATRIX(ib,ibp,ik_mem,i1) 
             if (.not.present(TRANS)) Hk(ib,ibp,i1)=O_MATRIX(ib,ibp,ik_mem,i1) 
             !
             if (l_rim_local.and.ib==ibp) Hk(ib,ib,i1)=Hk(ib,ib,i1) &
&                                           -en%E(ib,ik,i1)+en%E_RIM(ib,iE_rim,i1)
           enddo
         enddo
       enddo
     endif
     !
     if(present(O_QP))     call V_qp_basis_to_H(ik,O_QP,Hk)
     if(present(O_REAL))   call V_real_space_to_H(ik,O_REAL,Hk,WF)
     !
   end subroutine make_Hk_matrix
   !
end subroutine RT_average_operator
