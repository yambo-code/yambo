!
! Copyright (C) 2000-2013 F. De Fausti and the YAMBO team
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
subroutine el_density(en,Xk,rho,force_si)
 !
 !Electronic density.
 !
 !Note that here the identity
 !
 ! \sum_{S_k} F(S_k r) = 1/R_k \sum_{S} F(S r) (1)
 ! 
 !where S_k is a symm. op. in the star of k, S is a generic symm. op.
 !and R_k is the rank of the small group at k.
 !
 !Using (1) the density is calculated in two steps
 !
 ! rho(r) = 1/nkibz \sum_{ n k S_k } f_{n k} |wf_{n k}(S_k^-1 r)|^2=
 !        = 1/nkibz \sum_{S} \sum_{n k} f_{n k}/R_k |wf_{n k}(S^-1 r)|^2 =
 !        = 1/nsym \sum_{S} ( \sum_{n k} f_{n k} w_k |wf_{n k}(S^-1 r)|^2 )
 !
 !where we have used the relation
 !
 ! w_k = nsym/(nkibz * rank(k))
 !
 !
 use pars,          ONLY:SP
 use com,           ONLY:warning
 use electrons,     ONLY:levels,n_spin,n_sp_pol
 use R_lattice,     ONLY:bz_samp
 use D_lattice,     ONLY:nsym,i_time_rev,mag_syms
 use FFT_m,         ONLY:fft_size,fft_rot_r,fft_rot_r_inv
 use wave_func,     ONLY:wf_state,wf
 use parallel_m,    ONLY:PP_redux_wait,PAR_IND_Xk_ibz,PAR_IND_G_b,&
&                        PAR_COM_QP_A2A,l_par_HF,l_par_GW,&
&                        l_par_DENSITY,ncpu,myid
 !
 implicit none
 type(bz_samp)::Xk
 type(levels) ::en       
 real(SP)     ::rho(fft_size)
 logical,     intent(in) :: force_si
 !
 ! Work Space 
 !
 integer :: ib,ik,i_spin,ifft,rho_syms,ir
 real(SP):: rho_no_sym(fft_size),f_occ
 logical :: warn_done
 integer :: ID
 integer, allocatable ::states_filling(:,:,:)
 !
 rho=0._SP
 rho_no_sym=0._SP
 warn_done=.false.
 !
 ! March 2013 (Andrea)
 !
 ! The calculation of the density is tricky. Indeed the bands and k-points
 ! are distributed in the PAR_COM_QP_A2A groups in a non predictable manner.
 ! This means the bands and k-points can be duplicated and, for this reason,
 ! only the first CPU of the block that has that (n,k) must use it.
 ! This condiction in ensured by the line:
 ! 
 ! if (.not.all(states_filling(ib,ik,1:ID-1)==0))
 !
 ! Sept 2013 (Andrea)
 !
 ! Andrea F reported that <DFT> is wrong when only QP states are distributed.
 ! In this case the COMM PAR_COM_QP_A2A is not defined and the check on 
 ! states_filling was not done. This check has been imposed to be done all times.
 !
 if (PAR_COM_QP_A2A%n_CPU>1) then
   allocate(states_filling(en%nbm,Xk%nibz,PAR_COM_QP_A2A%n_CPU))
   ID=PAR_COM_QP_A2A%CPU_id+1
 else
   allocate(states_filling(en%nbm,Xk%nibz,ncpu))
   ID=myid+1
 endif
 !
 states_filling=0
 !
 do ib=1,en%nbm
   do ik=1,Xk%nibz
     if (PAR_IND_G_b%element_1D(ib).and.PAR_IND_Xk_ibz%element_1D(ik)) states_filling(ib,ik,ID)=1
   enddo
 enddo
 !
 call PP_redux_wait(states_filling)
 !
 bands_loop: do ib=1,en%nbm
   !
   if (.not.PAR_IND_G_b%element_1D(ib)) cycle
   !
   do ik=1,Xk%nibz
     !
     if (.not.PAR_IND_Xk_ibz%element_1D(ik)) cycle
     !
     if (.not.lowest_empty_ID(states_filling(ib,ik,:))==ID) cycle
     !
     do i_spin=1,n_spin
       !
       if (size(wf_state,3)<i_spin) cycle
       if (size(wf_state,1)<ib) cycle
       if (size(wf_state,2)<ik) cycle
       !
       ifft=wf_state(ib,ik,i_spin)
       !
       if (ifft==0) then
         if (.not.warn_done) call warning('Not enough states to calculate rho')
         warn_done=.true.
         cycle
       endif
       !
       f_occ=en%f(ib,ik,1)
       if (n_sp_pol==2) f_occ=en%f(ib,ik,i_spin)
       rho_no_sym(:)=rho_no_sym(:)+f_occ*Xk%weights(ik)*abs(wf(:,ifft))**2.
       !
     enddo
   enddo
 enddo bands_loop
 !
 if (PAR_COM_QP_A2A%n_CPU>1) then
   call PP_redux_wait(rho_no_sym,COMM=PAR_COM_QP_A2A%COMM)
 else
   if (l_par_GW.or.l_par_HF.or.l_par_DENSITY) call PP_redux_wait(rho_no_sym)
 endif
 deallocate(states_filling)
 !
 ! Simmetrization
 !
 rho_syms=nsym/(i_time_rev+1)
 !
 if(mag_syms) rho_syms=nsym 
 !
 do ir=1,rho_syms
   rho(:)=rho(:)+real(rho_no_sym(fft_rot_r(ir,:)),SP)/real(nsym,SP)
 enddo
 !
 if (mag_syms)      return
 if (.not.force_si) rho(:)=(1+i_time_rev)*rho(:)
 if (     force_si) rho(:)=   i_time_rev *real(rho(fft_rot_r_inv(:)),SP)+rho(:)
 ! 
 contains
   !
   integer function lowest_empty_ID(vector)
     !
     integer vector(:),i1
     !
     lowest_empty_ID=0
     do i1=1,size(vector)
       if (vector(i1)>0) then
         lowest_empty_ID=i1
         exit
       endif
     enddo
     !
   end function
   !
end subroutine
