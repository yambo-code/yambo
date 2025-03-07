!
! License-Identifier: GPL
!
! Copyright (C) 2015 The Yambo Team
!
! Authors (see AUTHORS file for details): MG CA DS
!
subroutine DIPOLE_covariant(Xen,Xk,Dip,direction_is_todo)
 !
 ! Build the covariant dipole operator at zero field
 ! in PRB 69, 085106 (2004)
 !
 use pars,           ONLY:SP,cI,cZERO,pi
 use LIVE_t,         ONLY:live_timing
 use electrons,      ONLY:levels,n_sp_pol
 use DIPOLES,        ONLY:DIPOLE_t,DIP_S,DIP_iR,DIP_v,covariant_obs,x_space_obs
 use ALLOC,          ONLY:DIPOLE_ALLOC_elemental
 use R_lattice,      ONLY:bz_samp,k_map
 use vec_operate,    ONLY:k_periodic_idx
 use matrix_operate, ONLY:hermitian
 use parallel_m,     ONLY:PAR_IND_DIPk_ibz,PAR_DIPk_ibz_index,PAR_IND_DIPk_ibz_ID,&
&                         PAR_IND_CON_BANDS_DIP, PAR_IND_VAL_BANDS_DIP
 use com,            ONLY:msg
 use D_lattice,      ONLY:a
 use parser_m,       ONLY:parser
 use timing_m,       ONLY:timing
 use stderr,         ONLY:STRING_match
 !
#include<y_memory.h>
 !
 type(levels),           intent(in)     ::Xen
 type(bz_samp),          intent(inout)  ::Xk
 type(DIPOLE_t),         intent(inout)  ::Dip
 logical,                intent(out)    ::direction_is_todo(3)
 !
 ! Work Space
 !
 logical     ::l_eval_polarization,l_force_SndOrd
 integer     ::ik,iv,ic,ic_min,iv_max,i_sp_pol,ikm1,id_red,idir,idx(3),ikbz,ik_mem,istep,max_step
 real(SP)    ::Ev_m_Ec_KS
 complex(SP), allocatable :: Sm1_plus (:,:,:),Sm1_minus(:,:,:)
 complex(SP) ::S_det(6),Wop(Dip%ib(2),Dip%ib(2),2),DIP_tmp(6,Dip%ib(2),Dip%ib(2)),S_tmp(Dip%ib(2),Dip%ib(2),2)
 character(1)::str(3)=(/'x','y','z'/)
 !
 call timing("DIPOLE_covariant",OPR="start")
 !
 if(.not.Dip%force_v_g_space) Dip%Vnl_included=.true.
 !
 if (any(Xen%nbf/=Xen%nbm)) call error(' Covariant dipoles not implemented for metals')
 !
 call parser('EvPolarization'  ,l_eval_polarization)
 call parser('FrSndOrd'        ,l_force_SndOrd)
 !
 if(.not.allocated(k_map%k_map_dir)) call k_map_nearest_by_dir(Xk,k_map)
 if(.not.allocated(Xk%k_table))      call k_build_up_BZ_tables(Xk)
 !
 ! Notice that DIP_S is defined in the BZ       
 !
 call DIPOLE_ALLOC_elemental('DIP_S',(/Dip%ib(2),Dip%ib(2),6,Xk%nbz/))
 DIP_S=cZERO
 !
 call DIPOLE_overlaps(Xk,Dip)
 !
 direction_is_todo(:)= (k_map%max_kdir(:)==1)
 !
 if(any(direction_is_todo)) call warning(' Covariant Dipoles not in 3d. Assuming non periodic system.')
 !
 if(any(k_map%max_kdir(:)>=6.and..not.l_force_SndOrd)) &
 &   call msg('rs','Using forth order approximation for covariant dipoles')
 !
 call live_timing(trim(covariant_obs)//" [covariant]",PAR_IND_DIPk_ibz%n_of_elements(PAR_IND_DIPk_ibz_ID+1)*n_sp_pol)
 !
 ! Notice that DIP_iR/DIP_v are defined only in the IBZ
 ! while the overlaps are in the BZ 
 !
 do i_sp_pol=1,n_sp_pol
   !
   YAMBO_ALLOC(Sm1_plus, (Xen%nbf(i_sp_pol),Xen%nbm(i_sp_pol),6))
   YAMBO_ALLOC(Sm1_minus,(Xen%nbf(i_sp_pol),Xen%nbm(i_sp_pol),6))
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
         iv_max=min(Dip%ib_lim(1),Xen%nbm(i_sp_pol))
         do iv=Dip%ib(1),iv_max
           ic_min=max(iv,Dip%ib_lim(2))
           do ic=ic_min,Dip%ib(2)
             !
             Wop(ic,iv,istep)=sum(S_tmp(ic,1:Xen%nbf(i_sp_pol),1)* Sm1_plus(1:Xen%nbf(i_sp_pol),iv,id_red + (istep-1)*3))- &
&                             sum(S_tmp(ic,1:Xen%nbf(i_sp_pol),2)*Sm1_minus(1:Xen%nbf(i_sp_pol),iv,id_red + (istep-1)*3))
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
           DIP_iR(idir,ic,iv,ik_mem,i_sp_pol)=(DIP_tmp(idir,ic,iv)+conjg(DIP_tmp(idir,iv,ic)))*cI
         enddo
       enddo
     enddo
     !
     !=====
     ! <v>
     !=====
     if(STRING_match(covariant_obs,"V")) then
       do iv=Dip%ib(1),Dip%ib_lim(1) 
         if(.not.PAR_IND_VAL_BANDS_DIP%element_1D(iv)) cycle
         do ic=max(iv,Dip%ib_lim(2)),Dip%ib(2)
           if(.not.PAR_IND_CON_BANDS_DIP%element_1D(ic)) cycle
           Ev_m_Ec_KS   =Xen%E (iv,ik,i_sp_pol)-Xen%E (ic,ik,i_sp_pol)
           if (allocated(Xen%Eo)) Ev_m_Ec_KS   =Xen%Eo(iv,ik,i_sp_pol)-Xen%Eo(ic,ik,i_sp_pol)
           do idir=1,3
             DIP_v(idir,ic,iv,ik_mem,i_sp_pol)=DIP_iR(idir,ic,iv,ik_mem,i_sp_pol)*Ev_m_Ec_KS
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
   !
   YAMBO_FREE(Sm1_plus)
   YAMBO_FREE(Sm1_minus)
   !
 enddo ! i_sp_pol
 !
 call live_timing()
 !
#if defined _NL
 if (l_eval_polarization) call POLARIZATION_output(Xen,Xk,Dip)
#endif
 !
 if (any(direction_is_todo)) then
   do ic=1,3
     if (direction_is_todo(ic)) then
       x_space_obs=trim(x_space_obs)//" R_"//str(ic)
       if (STRING_match(covariant_obs,"V")) x_space_obs=trim(x_space_obs)//" V_"//str(ic)
     endif
   enddo 
   Dip%computed=trim(Dip%computed)//trim(x_space_obs)//" [X-space]"
 endif
 !
 call timing("DIPOLE_covariant",OPR="stop")
 !
end subroutine DIPOLE_covariant
