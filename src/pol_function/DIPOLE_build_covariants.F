!
!        Copyright (C) 2000-2015 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): MG, CA, DS
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
subroutine DIPOLE_build_covariants(Xen,Xk,X)
 !
 ! Build the covariant dipole operator at zero field
 ! in PRB 69, 085106 (2004)
 !
 use pars,           ONLY:SP,cI,cZERO,pi
 use LIVE_t,         ONLY:live_timing
 use interfaces,     ONLY:WF_load
 use wave_func,      ONLY:WF
 use electrons,      ONLY:levels,n_sp_pol
 use X_m,            ONLY:X_t,DIP_S,DIP_iR,DIP_P,X_alloc,force_v_g_space_approach
 use R_lattice,      ONLY:bz_samp,k_map
 use vec_operate,    ONLY:k_periodic_idx
 use matrix_operate, ONLY:hermitian
 use parallel_m,     ONLY:PAR_IND_DIPk_ibz,PAR_DIPk_ibz_index,PAR_IND_Xk_ibz_ID,&
&                         PAR_Xk_nbz,PAR_COM_q_for_Xo,PP_redux_wait
 use com,            ONLY:error,warning
 use D_lattice,      ONLY:a,nsym
 use drivers,        ONLY:l_sc_run
 use parallel_m,     ONLY:master_cpu
 use IO_m,           ONLY:io_control,VERIFY,REP,OP_WR_CL,OP_APP_WR_CL,OP_RD_CL
 !
 implicit none
 !
 type(levels),           intent(in)     :: Xen
 type(bz_samp),          intent(in)     :: Xk
 type(X_t),              intent(in)     :: X      
 !
 ! Work Space
 !
 logical     :: idir_not_done(3)
 integer     :: ik,iv,ic,i_sp_pol,ikm1,id_red,idir,idx(3),i1,i2,ikbz,ik_mem
 real(SP)    :: Ev_m_Ec
 complex(SP) :: Sm1_plus (Xen%nbm,Xen%nbm,3)
 complex(SP) :: Sm1_minus(Xen%nbm,Xen%nbm,3)
 complex(SP) :: S_det(3)
 complex(SP) :: Wop(1:X%ib(2),1:X%ib(2)),DIP_tmp(3,1:X%ib(2),1:X%ib(2))
 complex(SP) :: S_tmp(X%ib(2),X%ib(2),2)
 !
 integer           :: io_err,ID
 integer, external :: io_Overlaps
 !
 if (nsym>1) call error(' Dipole covariant implementation to be extended to the case with symmetries')
 !
 ! WF load 
 ! Here I avoid the deallocation of states_to_load as I ASSUME that AFTER this routine 
 ! there will be other WF_load calls using the present WF distribution. 
 !
 call WF_load(WF,0,1,(/1,X%ib(2)/),(/1,Xk%nibz/),space='G',title='-Oscillators/G space',&
&             force_WFo=l_sc_run,keep_states_to_load=.TRUE.)
 !
 call k_map_nearest_by_dir(Xk,k_map)
 ! In the covariat approach I do need the overlap. 
 ! So as a first thing I check if they exist
 call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1,2/),MODE=VERIFY,ID=ID)
 io_err=io_Overlaps(X,Xen,ID)
 !
 ! If they do not I have to compute them
 if(io_err/=0) then
   ! Notice that DIP_S is defined in the BZ       
   call X_alloc('DIP_S',(/X%ib(2),X%ib(2),3,PAR_Xk_nbz/))
   DIP_S=cZERO
   !
   call DIPOLE_overlaps(Xen,Xk,X)
   !
   if (     master_cpu) then
     call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1/),ID=ID)
     io_err=io_Overlaps(X,Xen,ID)
   endif
   call PP_redux_wait(COMM=PAR_COM_q_for_Xo%COMM)
   if (.not.master_cpu) then
     call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1/),ID=ID)
     io_err=io_Overlaps(X,Xen,ID)
   endif
   !
   call io_control(ACTION=OP_APP_WR_CL,COM=REP,SEC=(/2/),ID=ID)
   io_err=io_Overlaps(X,Xen,ID)
   !
 endif
 !
 idir_not_done(:)= (k_map%max_kdir(:)==1)
 !
 if(any(idir_not_done)) call warning(' Covariant Dipoles not in 3d. Assuming non periodic system.')
 !
 call live_timing('Covariant Dipoles:',PAR_IND_DIPk_ibz%n_of_elements(PAR_IND_Xk_ibz_ID+1)*n_sp_pol)
 !
 ! Notice that DIP_iR/DIP_P are defined only in the IBZ
 ! while the overlaps are in the BZ 
 !
 !
 do i_sp_pol=1,n_sp_pol
   !
   do ik=1,Xk%nibz 
     !
     if (.not.PAR_IND_DIPk_ibz%element_1D(ik)) cycle
     !
     ik_mem=PAR_DIPk_ibz_index(ik)
     !
     ikbz=Xk%sstar(ik,1)
     !
     call Build_Overlaps_Det(Xen,i_sp_pol,ikbz,X,Sm1_plus,Sm1_minus,S_det)
     !
     DIP_tmp=cZERO
     !
     do id_red=1,3  ! loop on reciprocal lattice
       !
       if(k_map%max_kdir(id_red)==1) cycle
       !
       idx=k_map%k_map_inv(ikbz,:)
       idx(id_red)=idx(id_red)-1
       idx=k_periodic_idx(idx,k_map)
       ikm1=k_map%k_map_dir(idx(1),idx(2),idx(3))
       !
       S_tmp(:,:,1)=          DIP_S(:,:,id_red,ikbz,i_sp_pol)
       S_tmp(:,:,2)=hermitian(DIP_S(:,:,id_red,ikm1,i_sp_pol))
       !
       Wop=cZERO
       !
       forall(i1=Xen%nbm+1:X%ib(2),i2=1:Xen%nbf) 
         Wop(i1,i2)=sum(S_tmp(i1,1:Xen%nbf,1)* Sm1_plus(1:Xen%nbf,i2,id_red))- &
&                   sum(S_tmp(i1,1:Xen%nbf,2)*Sm1_minus(1:Xen%nbf,i2,id_red))
       end forall
       !
       Wop=Wop*cI*k_map%max_kdir(id_red)/(4._SP*pi)
       !
       ! 
       ! Transform in cartesian coordinates 
       !
       do idir=1,3
         DIP_tmp(idir,:,:)=DIP_tmp(idir,:,:)+transpose(Wop(:,:))*a(id_red,idir)
       enddo
       !
     enddo 
     !
     !======
     ! <iR>
     !======
     do iv=X%ib(1),X%ib_lim(1)
       do ic=X%ib_lim(2),X%ib(2)
         do idir=1,3
           DIP_iR(idir,ic,iv,ik_mem,i_sp_pol)=(DIP_tmp(idir,ic,iv)+conjg(DIP_tmp(idir,iv,ic)))/cI
         enddo
       enddo
     enddo
     !
     !=====
     ! <P>
     !=====
     if(.not.force_v_g_space_approach) then
       do iv=X%ib(1),X%ib_lim(1) 
         do ic=max(iv,X%ib_lim(2)),X%ib(2)
           ! This is directly p_eff, see Dipole transverse and PRB 48, 11789 (1993)
           Ev_m_Ec=Xen%E(iv,ik,i_sp_pol)-Xen%E(ic,ik,i_sp_pol)
           do idir=1,3
             DIP_P(idir,ic,iv,ik_mem,i_sp_pol)=DIP_iR(idir,ic,iv,ik_mem,i_sp_pol)*Ev_m_Ec
           enddo
         enddo
       enddo
     endif
     !
     call live_timing(steps=1)
     !
   enddo ! ik
 enddo ! i_sp_pol
 !
 call live_timing()
 !
 call X_alloc('DIP_S')
 !
 call WF_free(WF)
 !
 if(any(idir_not_done)) call DIPOLE_x_real_space(Xen,Xk,X,idir_not_done)
 !
 return
 !
end subroutine DIPOLE_build_covariants
