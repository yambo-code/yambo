!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, DS
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
subroutine QP_apply_DB_interpolation(band_range,qp,en,k,do_EWZ_using_Ik_IE,&
&                                    DB_corrected,n_neigh)
 !
 ! Extend the QP correction from the kpt in qp to the kpt in k
 ! Requires qp%nk, qp%k, qp%table, qp%n_states, qp%E, qp%E_bare, qp%Z
 !
 ! do_EWZ_using_Ik_IE=( E W Z Interp/k Interp/E )
 !
 ! Note that as output of this routine W is positive(negative) above(below) the Fermi level. 
 ! This is true both at zero and finite temperature. Indeed also at finite temperature
 ! when the theory is causal the GF must be rotated to the real-energy axis.
 !
 use pars,          ONLY:SP
 use units,         ONLY:HA2EV
 use electrons,     ONLY:n_sp_pol,spin
 use com,           ONLY:msg,warning
 use electrons,     ONLY:levels
 use R_lattice,     ONLY:bz_samp
 use stderr,        ONLY:real2ch
 use QP_m,          ONLY:QP_t,QP_ctl_applied
 !
#include<memory.h>
 !
 type(levels)::en
 type(QP_t)  ::qp
 type(bz_samp)::k
 integer :: n_neigh,band_range(2),DB_corrected(en%nb,en%nk,n_sp_pol),DB_band_range(2)
 logical :: do_EWZ_using_Ik_IE(5),wrong_width_sign_warn
 !
 ! Work Space
 !
 real(SP), allocatable :: Vali(:,:),Valo(:,:,:,:)
 real(SP) :: E_diff
 integer  :: i1,ik,iband,i_spin,nk_interpolated,&
&            nk_exact,i_qp_ref,N_val_to_int,N_exact,N_close,N_far
 !
 logical, external :: QP_check_if_corrected
 !
 ! IBZ -> BZ (iku)
 !
 call k_ibz2bz(k,'i',.true.)
 !
 ! Transfer & interpolation
 !
 wrong_width_sign_warn=.false.
 nk_interpolated=0
 nk_exact=0
 !
 DB_band_range=(/minval(qp%table(:,1)),maxval(qp%table(:,1))/)
 !
 if (do_EWZ_using_Ik_IE(4)) then
   !
   ! Nearest K-point interpolation
   !===============================
   !
   call QP_transfer('init')
   !
   call Nearest_kpt_interpolation(qp%nk,k%nibz,band_range,qp%n_states,N_val_to_int,n_neigh,&
&                                 qp%table,qp%k,k%pt,qp%wk,k%weights,Vali,Valo,'QP ',.false.)
   !
   call QP_transfer('appl')
   !
 else if (do_EWZ_using_Ik_IE(5)) then
   !
   ! Nearest Level interpolation
   !===============================
   !
   E_diff=minval(En%Eo(DB_band_range(1),:,:))-minval(qp%E_bare)
   !
   N_exact=0
   N_close=0
   N_far  =0
   !
   if (E_diff>1.E-5_SP) &
&    call warning(' Energy not aligned. Difference is '//trim(real2ch(E_diff))//' Ha. QP interpolation maybe wrong')
   !
   do ik=1,k%nibz
     do iband=band_range(1),band_range(2)
       do i_spin=1,n_sp_pol
         !
         call Nearest_level_interpolation(En%Eo(iband,ik,i_spin),qp%E_bare(:),qp%n_states,E_diff,i_qp_ref)
         !
         if (                        E_diff<1.E-5_SP    ) N_exact=N_exact+1
         if (E_diff>1.E-5_SP    .and.E_diff<0.1_SP/HA2EV) N_close=N_close+1
         if (E_diff>0.1_SP/HA2EV                        ) N_far  =N_far  +1
         !
         if (do_EWZ_using_Ik_IE(1)) then
           if (QP_check_if_corrected((/iband,iband/),(/ik,ik/),(/i_spin,i_spin/),en,'E')) cycle
           en%E(iband,ik,i_spin)=en%Eo(iband,ik,i_spin)+real(qp%E(i_qp_ref),SP)-qp%E_bare(i_qp_ref)
           DB_corrected(iband,ik,i_spin)=1
           en%QP_corrected(iband,ik,i_spin)=en%QP_corrected(iband,ik,i_spin)+1
         endif
         !
         if (do_EWZ_using_Ik_IE(2)) then
           if (QP_check_if_corrected((/iband,iband/),(/ik,ik/),(/i_spin,i_spin/),en,'W')) cycle
           if (real(qp%E_bare(i_qp_ref))>0..and.aimag(qp%E(i_qp_ref))>0) then
             if (.not.wrong_width_sign_warn) call warning(' Wrong QP width sign fixed')
             en%W(iband,ik,i_spin)=-aimag(qp%E(i_qp_ref))
             wrong_width_sign_warn=.true.
           else
             en%W(iband,ik,i_spin)=aimag(qp%E(i_qp_ref))
           endif
           DB_corrected(iband,ik,i_spin)=1
           en%QP_corrected(iband,ik,i_spin)=en%QP_corrected(iband,ik,i_spin)+2
         endif
         !
         if (do_EWZ_using_Ik_IE(3)) then
           if (QP_check_if_corrected((/iband,iband/),(/ik,ik/),(/i_spin,i_spin/),en,'Z')) cycle
           en%Z(iband,ik,i_spin)=qp%Z(i_qp_ref)
           DB_corrected(iband,i1,i_spin)=1
           en%QP_corrected(iband,ik,i_spin)=en%QP_corrected(iband,ik,i_spin)+4
         endif
         !
       enddo
     enddo
   enddo
   !
   call msg('sr','Nearest energy level interpolation.')
   call msg('sr','Exactly mapped, closly mapped and far:',(/N_exact,N_close,N_far/))
   !
 endif
 !
 QP_ctl_applied=.true.
 !
 ! Clean
 !
 call k_ibz2bz(k,'d',.false.)
 !
 contains
   !
   subroutine QP_transfer(action_kind)
     !
     character(4) :: action_kind
     !
     select case(action_kind)
       !
       case('init')
         !
         N_val_to_int=0
         if(do_EWZ_using_Ik_IE(1)) N_val_to_int=N_val_to_int+1
         if(do_EWZ_using_Ik_IE(2)) N_val_to_int=N_val_to_int+1
         if(do_EWZ_using_Ik_IE(3)) N_val_to_int=N_val_to_int+2
         !
         YAMBO_ALLOC(Vali,(qp%n_states,N_val_to_int))
         YAMBO_ALLOC(Valo,(band_range(1):band_range(2),k%nibz,n_sp_pol,N_val_to_int))
         Valo=0._SP
         !
         i1=0
         if(do_EWZ_using_Ik_IE(1)) then
           i1=i1+1
           Vali(:,i1)=real(qp%E(:)-qp%E_bare(:))
         endif
         if(do_EWZ_using_Ik_IE(2)) then
           i1=i1+1
           Vali(:,i1)=aimag(qp%E(:))
         endif
         if(do_EWZ_using_Ik_IE(3)) then
           i1=i1+2
           Vali(:,i1-1)= real(qp%Z(:))
           Vali(:,i1  )=aimag(qp%Z(:))
         endif
         !
       case('appl')
         !
         do ik=1,k%nibz
           do iband=band_range(1),band_range(2)
             !
             if (iband<DB_band_range(1)) cycle
             if (iband>DB_band_range(2)) cycle
             !
             i1=0
             if(do_EWZ_using_Ik_IE(1)) then
               i1=i1+1
               do i_spin=1,n_sp_pol
                 if (QP_check_if_corrected((/iband,iband/),(/ik,ik/),(/i_spin,i_spin/),en,'E')) cycle
                 en%E(iband,ik,i_spin)=en%Eo(iband,ik,i_spin)+Valo(iband,ik,i_spin,i1)
                 DB_corrected(iband,ik,i_spin)=1
                 en%QP_corrected(iband,ik,i_spin)=en%QP_corrected(iband,ik,i_spin)+1
               enddo
             endif
             if(do_EWZ_using_Ik_IE(2)) then
               i1=i1+1
               do i_spin=1,n_sp_pol
                 if (QP_check_if_corrected((/iband,iband/),(/ik,ik/),(/i_spin,i_spin/),en,'W')) cycle
                 if (en%Eo(iband,ik,i_spin)>0. .and. Valo(iband,ik,i_spin,i1)>0) then
                   if (.not.wrong_width_sign_warn) call warning(' Wrong QP width sign fixed')
                   en%W(iband,ik,i_spin)=-Valo(iband,ik,i_spin,i1)
                   wrong_width_sign_warn=.true.
                 else
                   en%W(iband,ik,i_spin)=Valo(iband,ik,i_spin,i1)
                 endif
                 DB_corrected(iband,ik,i_spin)=1
                 en%QP_corrected(iband,ik,i_spin)=en%QP_corrected(iband,ik,i_spin)+2
               enddo
             endif
             if(do_EWZ_using_Ik_IE(3)) then
               i1=i1+2
               do i_spin=1,n_sp_pol
                 if (QP_check_if_corrected((/iband,iband/),(/ik,ik/),(/i_spin,i_spin/),en,'Z')) cycle
                 en%Z(iband,ik,i_spin)=cmplx(Valo(iband,ik,i_spin,i1-1),Valo(iband,ik,i_spin,i1))
                 DB_corrected(iband,ik,i_spin)=1
                 en%QP_corrected(iband,ik,i_spin)=en%QP_corrected(iband,ik,i_spin)+4
               enddo
             endif
             !
           enddo
         enddo
         !
         YAMBO_FREE(Vali)
         YAMBO_FREE(Valo)
         !
     end select
     !
   end subroutine
   !
end subroutine
