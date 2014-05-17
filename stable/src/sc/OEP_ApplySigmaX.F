!
! Copyright (C) 2000-2014 M. Gruening and the YAMBO team
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
subroutine OEP_ApplySigmaX(E,k,q,U_x)
 !
 !  Apply the NL local X operator to the valence wfs:
 !  
 !  OUTPUT: U_x^{nk}(r) = \int dr' \Sigma_x(r',r) \phi_{nk} 
 !
 !  Found as 
 !
 !  U_x^{nk}(r) = \sum_q \sum_m \phi_{m,k-q}(r)*u_rs_{n,m}(k,q,r)
 !
 !  where u_rs_{n,m}(k,q,r) = FT[rho_{m,n}(k,q,G)/|q+G|**2] (EXX case)
 !
 !
 !      NO SPIN/PARALLEL EXECUTION NOT TESTED  
 ! 
 use pars,            ONLY:SP,DP,pi,schlen
 use D_lattice,       ONLY:i_time_rev,sop_inv,nsym
 use electrons,       ONLY:levels,n_met_bands,spin_occ
 use R_lattice,       ONLY:bz_samp,g_rot,qindx_S,nkibz
 use FFT_m,           ONLY:fft_size,fft_g_table,fft_dim,fftw_plan,fft_rot_r
 use QP_m,            ONLY:QP_ng_Sx
 use collision,       ONLY:ggwinfo,collision_reset 
 use memory_m,        ONLY:mem_est
 use parallel_m,      ONLY:PP_redux_wait,PP_indexes,myid,PP_indexes_reset
 use interfaces,      ONLY:PARALLEL_index
 use wave_func,       ONLY:WF_load,wf,wf_state
 use timing,          ONLY:live_timing
 use stderr,          ONLY:intc
 use SC,              ONLY:it_now
 !
 ! I/O
 !
 implicit none
 complex(SP),intent(out)  :: U_x(nkibz,n_met_bands,fft_size)
 type(levels), intent(in) :: E
 type(bz_samp), intent(in):: k,q
 !
 ! Work space
 !
 type(ggwinfo)    ::isc
 type(PP_indexes) ::px
 integer          ::iv1,iv2,ir,ig,i1,ikbz,ik,iq,ifft,offt,tr_syms,os,is,iGo,qs
 character(schlen)::ch
 logical          ::lostr, listr     
 complex(DP),allocatable ::u(:),rhotw_rs(:),u_rs(:),local_u(:)
 !
 ! Resets
 !
 call collision_reset(isc)
 call PP_indexes_reset(px)
 !
 allocate(isc%gamp(QP_ng_Sx,1),isc%rhotw(QP_ng_Sx),local_u(fft_size),u(fft_size),&
      &u_rs(fft_size),rhotw_rs(fft_size))
 !
 isc%qs=1
 isc%ngrho=QP_ng_Sx
 call scatterGamp(isc,'x')
 U_x=(0._SP,0._SP)
 call PARALLEL_index(px,(/q%nbz,E%nbm/))
 call PP_redux_wait
 ch="Sigma_X|WF>@it"//trim(intc(it_now))
 call live_timing(trim(ch),px%n_of_elements(myid+1)*E%nbm*k%nibz)
 do iq=1,q%nbz
   isc%qs(2:)=(/q%sstar(iq,1),q%sstar(iq,2)/)
   if (isc%qs(2)/=isc%iqref)    call scatterGamp(isc,'x')
   do iv1 = 1 ,E%nbm
     if (.not.px%element_2D(iq,iv1)) cycle
     do ik = 1,k%nibz
       is = k%sstar(ik,2)
       do iv2 = 1 ,E%nbm
         u =(0._DP,0._DP)
         u_rs =(0._DP,0._DP) 
         local_u =(0._DP,0._DP)
         rhotw_rs =(0._DP,0._DP)  
         isc%is=(/iv1,ik,1,1/)
         isc%os=(/iv2,k%sstar(qindx_S(ik,iq,1),:),1/)
         isc%qs(1)=qindx_S(ik,iq,2)
         os=isc%os(3)
         qs=isc%qs(3)
         lostr = os>nsym/(i_time_rev+1)
         call scatterBamp(isc)
         u(1:QP_ng_Sx) = 4._SP*pi*conjg(isc%rhotw(1:QP_ng_Sx))*isc%gamp(1:QP_ng_Sx,1)
         iGo=g_rot(sop_inv(qs),isc%qs(1))
         u_rs(fft_g_table(1:QP_ng_Sx,iGo))=u(1:QP_ng_Sx)
#if defined _FFTW
         fftw_plan=0
         call fft_3d(u_rs,fft_dim,1,fftw_plan)
#else
         call fft_3d(u_rs,fft_dim,1)
#endif      
         ifft=wf_state(iv1,ik,1)
         offt=wf_state(iv2,isc%os(2),1)
         rhotw_rs(:)=E%f(iv2,isc%os(2),1)/spin_occ
         if (lostr)      rhotw_rs(1:fft_size)=rhotw_rs(:)*conjg(wf(fft_rot_r(os,1:fft_size),offt))
         if (.not.lostr) rhotw_rs(1:fft_size)=rhotw_rs(:)*wf(fft_rot_r(os,1:fft_size),offt)
         local_u(:) =  u_rs(fft_rot_r(qs,:))*rhotw_rs(:)
         U_x(ik,iv1,:) = U_x(ik,iv1,:)-local_u(:)
         call live_timing(steps=1)
         !
       end do
     end do
   end do
 end do
 !
 call live_timing()
 deallocate(isc%gamp,isc%rhotw,local_u,u,u_rs,rhotw_rs)
 call collision_reset(isc)
 call PP_indexes_reset(px)
 !
end subroutine OEP_ApplySigmaX
