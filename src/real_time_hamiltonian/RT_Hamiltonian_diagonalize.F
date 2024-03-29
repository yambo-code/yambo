!
! License-Identifier: GPL
!
! Copyright (C) 2018 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
subroutine RT_Hamiltonian_diagonalize(E,k,BGR,A_input)
 !
 ! This routine defines and diagonalize the Coherent part of the
 ! evolution operator.
 !
 ! This is by Ho_plus_Sigma which include a H/xc part plus RT_Vnl_xc
 !
 ! In this routine I do a double job. I diagonalize
 ! 
 !  1. Ho_plus_Sigma
 !  2. Ho_plus_Sigma-RT_Vnl_xc+Z*RT_Vnl_xc
 !
 ! The point 2 is done only if l_RT_update_Energies=.TRUE. and if Z_bgr/=1.
 !
 use pars,           ONLY:SP,rZERO,cONE,cZERO
 use R_lattice,      ONLY:nkibz
 use timing_m,       ONLY:timing
 use electrons,      ONLY:levels
 use hamiltonian,    ONLY:H_rotation,E_reference
 use parallel_m,     ONLY:PAR_IND_Xk_ibz,PAR_COM_Xk_ibz_INDEX
 use parallel_int,   ONLY:PP_redux_wait
 use real_time,      ONLY:RT_levels,Ho_plus_Sigma,RTibz,RT_bands,&
&                         l_RT_uses_E_FineGd,l_RT_update_Energies,l_RT_rotate_DM,&
&                         RT_nbands,l_NE_with_fields,H_EQ,l_RT_RWA
 use RT_lifetimes,   ONLY:l_RT_eval_lifetimes
 use R_lattice,      ONLY:bz_samp
 use fields,         ONLY:gauge_field
 use wrapper_omp,    ONLY:M_plus_alpha_M_omp
 use interfaces,     ONLY:OCCUPATIONS_Fermi
 !
 implicit none
 !
 type(levels)                 :: E
 type(bz_samp), intent(in)    :: k
 real(SP),      intent(out)   :: BGR(4)
 type(gauge_field), intent(in):: A_input
 !
 ! Work Space
 !
 complex(SP)  :: H(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2)),&
&                H_field(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 real(SP)     :: Ed(RT_bands(1):RT_bands(2)),dE
 integer      :: ib,ik,ik_RT,ik_E_fg
 logical      :: l_do_E,l_do_R
 logical, external :: RT_do_it
 !
 BGR=rZERO
 !
 l_do_E=l_RT_update_Energies.and.(l_RT_eval_lifetimes.or.(RT_do_it('OUT').or.RT_do_it('CARR')))
 l_do_R=l_RT_rotate_DM.and.(RT_do_it('OUT').or.RT_do_it('CARR'))
 !
 if (.not.l_do_E .and. .not. l_do_R) return
 !
 call timing('RT H diago',OPR='start')
 !
 if (l_do_E) then
   E%E(RT_bands(1):RT_bands(2),:,:)=rZERO
   if (l_RT_uses_E_FineGd) E%FG%E(RT_bands(1):RT_bands(2),:,:)=rZERO
   RT_levels%E(RT_bands(1):RT_bands(2),:,:)=rZERO
 endif
 !
 do ik=1,nkibz
   !
   if (.not.PAR_IND_Xk_ibz%element_1D(ik) ) cycle
   !
   ik_RT=RTibz%k_range(ik,1)
   !
   H(:,:)=Ho_plus_Sigma(:,:,ik_RT,1)
   !
   if (l_RT_RWA) H=H+H_EQ(:,:,ik_RT,1)
   !
   if (l_NE_with_fields) then
     !
     H_field=cZERO
     call RT_apply_field(ik,1,H_field,A_input)
     call M_plus_alpha_M_omp(RT_nbands,cONE,-H_field,H)
     !
   endif
   !
   call SERIAL_HERMITIAN_diagonalization(RT_nbands,H,Ed)
   !
   ! Rotation
   !
   if (l_do_R) H_rotation(:,:,ik,1)=H(:,:)
   !
   if (.not.l_do_E) cycle
   !
   do ib=RT_bands(2),1,-1
     !
     if (ib>=RT_bands(1)) dE=Ed(ib)-E_reference%E(ib,ik,1)
     if (ib< RT_bands(1)) dE=Ed(RT_bands(1))-E_reference%E(RT_bands(1),ik,1)
     !
     do ik_RT=RTibz%k_range(ik,1),RTibz%k_range(ik,2)
       !
       if(      l_RT_uses_E_FineGd .and.ik_RT/=RTibz%k_range(ik,1)) then
         ik_E_fg=RTibz%E_map(ik_RT)
         E%FG%E(ib,ik_E_fg,1) = E_reference%FG%E(ib,ik_E_fg,1)+dE
         if (ib>=RT_bands(1)) RT_levels%E(ib,ik_RT,1) =E%FG%E(ib,ik_E_fg,1)
       else
         E%E(ib,ik,1)=E_reference%E(ib,ik,1)+dE
         if (ib>=RT_bands(1)) RT_levels%E(ib,ik_RT,1) =E%E(ib,ik,1)
       endif
       !
     enddo
   enddo
   !    
 enddo
 !
 if (l_do_E) then
                           call PP_redux_wait(E%E,   COMM=PAR_COM_Xk_ibz_INDEX%COMM)
   if (l_RT_uses_E_FineGd) call PP_redux_wait(E%FG%E,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
   call PP_redux_wait(RT_levels%E(RT_bands(1):RT_bands(2),:,:),COMM=PAR_COM_Xk_ibz_INDEX%COMM)
 endif
 !
 ! Evaluation of the contribution to the minimal gap
 !
 call OCCUPATIONS_Fermi(E,k,"E_RT",mode="OCCUPATIONS",n_bands=RT_bands(2))
 !
 BGR(1)=E%E_dir_gap(1)
 BGR(2)=E%E_ind_gap(1)
 BGR(3)=BGR(1)-E_reference%E_dir_gap(1)
 BGR(4)=BGR(2)-E_reference%E_ind_gap(1)
 !
 call timing('RT H diago',OPR='stop')
 !
end subroutine RT_Hamiltonian_diagonalize
