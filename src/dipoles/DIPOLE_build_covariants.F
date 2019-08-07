!
!        Copyright (C) 2000-2019 the YAMBO team
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
subroutine DIPOLE_build_covariants(Xen,Xk,Dip)
 !
 ! Build the covariant dipole operator at zero field
 ! in PRB 69, 085106 (2004)
 !
 use pars,           ONLY:SP,cI,cZERO,pi
 use LIVE_t,         ONLY:live_timing
 use interfaces,     ONLY:WF_load,PARALLEL_WF_distribute,WF_free,PARALLEL_WF_index
 use wave_func,      ONLY:WF
 use electrons,      ONLY:levels,n_sp_pol
 use DIPOLES,        ONLY:DIPOLE_t,DIP_alloc,DIP_S,DIP_iR,DIP_v
 use R_lattice,      ONLY:bz_samp,k_map
 use vec_operate,    ONLY:k_periodic_idx
 use matrix_operate, ONLY:hermitian
 use parallel_m,     ONLY:PAR_IND_DIPk_ibz,PAR_DIPk_ibz_index,PAR_IND_DIPk_ibz_ID,&
&                         PAR_DIPk_nbz,PAR_IND_OVLPk_ibz,&
&                         PAR_IND_CON_BANDS_DIP, PAR_IND_VAL_BANDS_DIP
 use com,            ONLY:error,warning,msg
 use D_lattice,      ONLY:a
 use parser_m,       ONLY:parser

 !
 implicit none
 !
 type(levels),           intent(in)     :: Xen
 type(bz_samp),          intent(inout)  :: Xk
 type(DIPOLE_t),         intent(inout)  :: Dip
 !
 ! Work Space
 !
 logical     :: idir_not_done(3)
 logical     :: l_eval_polarization,l_force_SndOrd
 integer     :: ik,iv,ic,ic_min,iv_max,i_sp_pol,ikm1,id_red,idir,idx(3),ikbz,ik_mem
 integer     :: istep,max_step
 real(SP)    :: Ev_m_Ec
 complex(SP) :: Sm1_plus (Xen%nbf,Xen%nbm,6)
 complex(SP) :: Sm1_minus(Xen%nbf,Xen%nbm,6)
 complex(SP) :: S_det(6)
 complex(SP) :: Wop(Dip%ib(2),Dip%ib(2),2),DIP_tmp(6,Dip%ib(2),Dip%ib(2))
 complex(SP) :: S_tmp(Dip%ib(2),Dip%ib(2),2)
 !
 integer           :: ID
 !
 if (Xen%nbf/=Xen%nbm) call error(' Covariant dipoles not implemented for metals')
 !
 call parser('EvPolarization'  ,l_eval_polarization)
 call parser('FrSndOrd'        ,l_force_SndOrd)
 !
 if(.not.allocated(k_map%k_map_dir)) call k_map_nearest_by_dir(Xk,k_map)
 if(.not.allocated(Xk%k_table))      call k_build_up_BZ_tables(Xk)
 !
 call PARALLEL_WF_distribute(K_index=PAR_IND_OVLPk_ibz,CLEAN_UP=.TRUE.)
 !
 call PARALLEL_WF_index( )
 !
 call WF_load(WF,0,1,(/1,Dip%ib(2)/),(/1,Xk%nibz/),space='G',title='-Oscillators/G space')
 !
 ! Notice that DIP_S is defined in the BZ       
 !
 call DIP_alloc('DIP_S',(/Dip%ib(2),Dip%ib(2),6,PAR_DIPk_nbz/))
 DIP_S=cZERO
 !
 call DIPOLE_overlaps(Xk,Dip)
 !
 idir_not_done(:)= (k_map%max_kdir(:)==1)
 !
 if(any(idir_not_done)) call warning(' Covariant Dipoles not in 3d. Assuming non periodic system.')
 !
 if(any(k_map%max_kdir(:)>=6.and..not.l_force_SndOrd)) &
 &   call msg('rs','Using forth order approximation for covariant dipoles')
 !
 call live_timing('Covariant Dipoles:',PAR_IND_DIPk_ibz%n_of_elements(PAR_IND_DIPk_ibz_ID+1)*n_sp_pol)
 !
 ! Notice that DIP_iR/DIP_v are defined only in the IBZ
 ! while the overlaps are in the BZ 
 !
 do i_sp_pol=1,n_sp_pol
   !
   do ik=1,Xk%nibz 
     !
     if (.not.PAR_IND_DIPk_ibz%element_1D(ik)) cycle
     !
     ik_mem=PAR_DIPk_ibz_index(ik)
     !
     ikbz=Xk%k_table(ik,1)
     !
     call Build_Overlaps_Det_EQ(Xen,i_sp_pol,ikbz,Sm1_plus,Sm1_minus,S_det)
     !
     DIP_tmp=cZERO
     !
     do id_red=1,3  ! loop on reciprocal lattice
       !
       if(k_map%max_kdir(id_red)==1) cycle
       !
       max_step=1
       !
       ! I need at leat 5 k-points in the line to
       ! use the second order formula
       !
       if(k_map%max_kdir(id_red)>=6.and..not.l_force_SndOrd) max_step=2
       !
       do istep=1,max_step 
         !
         idx=k_map%k_map_inv(ikbz,:)
         idx(id_red)=idx(id_red)-istep
         idx=k_periodic_idx(idx,k_map)
         ikm1=k_map%k_map_dir(idx(1),idx(2),idx(3))
         !
         S_tmp(:,:,1)=          DIP_S(:,:,id_red+(istep-1)*3,ikbz,i_sp_pol)
         S_tmp(:,:,2)=hermitian(DIP_S(:,:,id_red+(istep-1)*3,ikm1,i_sp_pol))
         !
         Wop(:,:,istep)=cZERO
         !
         iv_max=min(Dip%ib_lim(1),Xen%nbm)
         do iv=Dip%ib(1),iv_max
           ic_min=max(iv,Dip%ib_lim(2))
           do ic=ic_min,Dip%ib(2)
             !
             Wop(ic,iv,istep)=sum(S_tmp(ic,1:Xen%nbf,1)* Sm1_plus(1:Xen%nbf,iv,id_red + (istep-1)*3))- &
&                             sum(S_tmp(ic,1:Xen%nbf,2)*Sm1_minus(1:Xen%nbf,iv,id_red + (istep-1)*3))
             !
           enddo
         enddo
         !
         Wop(:,:,istep)=Wop(:,:,istep)*cI*k_map%max_kdir(id_red)/(4._SP*pi*real(istep,SP))
         !
       enddo ! istep
       !
       ! Transform in cartesian coordinates 
       !
       if(max_step==1) then 
         !      
         ! First order formula D(dk)      
         ! Eq. 96 of PRB 63, 155107(2001)
         !
         do idir=1,3
           DIP_tmp(idir,:,:)=DIP_tmp(idir,:,:)+transpose(Wop(:,:,1))*a(id_red,idir)
         enddo
         !
       else
         !      
         ! Second order formula   ( 4*D(dk) - D(2*dk) ) / 3
         ! Eq. 97 of PRB 63, 155107(2001)
         !
         do idir=1,3
           DIP_tmp(idir,:,:)=DIP_tmp(idir,:,:)+transpose(4._SP*Wop(:,:,1)-Wop(:,:,2))/3._SP*a(id_red,idir)
         enddo
       endif
       !
     enddo ! id_red
     !
     !======
     ! <iR>
     !======
     do iv=Dip%ib(1),Dip%ib_lim(1)
       if(.not.PAR_IND_VAL_BANDS_DIP%element_1D(iv)) cycle
       do ic=max(iv,Dip%ib_lim(2)),Dip%ib(2)
         if(.not.PAR_IND_CON_BANDS_DIP%element_1D(ic)) cycle
         do idir=1,3
           DIP_iR(idir,ic,iv,ik_mem,i_sp_pol)=(DIP_tmp(idir,ic,iv)+conjg(DIP_tmp(idir,iv,ic)))/cI
         enddo
       enddo
     enddo
     !
     !=====
     ! <v>
     !=====
     if(.not.Dip%force_v_g_space) then
       do iv=Dip%ib(1),Dip%ib_lim(1) 
         if(.not.PAR_IND_VAL_BANDS_DIP%element_1D(iv)) cycle
         do ic=max(iv,Dip%ib_lim(2)),Dip%ib(2)
           if(.not.PAR_IND_CON_BANDS_DIP%element_1D(ic)) cycle
           ! This is directly p_eff, see Dipole transverse and PRB 48, 11789 (1993)
           Dip%v_eff=.true.
           Ev_m_Ec=Xen%E(iv,ik,i_sp_pol)-Xen%E(ic,ik,i_sp_pol)
           do idir=1,3
             DIP_v(idir,ic,iv,ik_mem,i_sp_pol)=DIP_iR(idir,ic,iv,ik_mem,i_sp_pol)*Ev_m_Ec
           enddo
           !
         enddo
       enddo
       !
     endif
     !
     call live_timing(steps=1)
     !
   enddo ! ik
 enddo ! i_sp_pol
 !
 call live_timing()
 !
#if defined _NL
 if(l_eval_polarization) call POLARIZATION_output(Xen,Xk,Dip)
#endif
 !
 call WF_free(WF)
 !
 if(any(idir_not_done)) call DIPOLE_x_real_space(Xen,Xk,Dip,idir_not_done)
 !
 return
 !
end subroutine DIPOLE_build_covariants
