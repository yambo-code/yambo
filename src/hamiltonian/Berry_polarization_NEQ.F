!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): MG CA
!                  
subroutine Berry_polarization_NEQ(Xen,Xk,i_sp_pol,V_bands,Polarization)
 !
 use pars,           ONLY:SP,cZERO
 use electrons,      ONLY:levels
 use D_lattice,      ONLY:a,DL_vol
 use R_lattice,      ONLY:bz_samp,k_map,nXkbz
 use hamiltonian,    ONLY:H_ref_bands
 use QP_m,           ONLY:QP_nk
 use electric,       ONLY:All_S_det,Sm1_plus,Sm1_minus
 use parallel_m,     ONLY:PAR_IND_Xk_bz,PAR_COM_Xk_ibz_INDEX
 use parallel_int,   ONLY:PP_redux_wait
#if defined _TIMING
 use timing_m,       ONLY:timing
#endif
 !
 implicit none           
 type(bz_samp), intent(in)   :: Xk
 type(levels),  intent(in)   :: Xen
 integer,       intent(in)   :: i_sp_pol
 complex(SP),   intent(in)   :: V_bands(H_ref_bands(2),Xen%nbf(i_sp_pol),QP_nk)
 complex(SP),   intent(inout):: Polarization(3)
 !
 ! Work Space
 !
 integer       :: id,ik_bz,nb
 complex(SP)   :: Polarization_red(3)
 !
#if defined _TIMING
 call timing('NL Berry Pol NEQ',OPR='start')
#endif
 !
 Sm1_minus=cZERO
 Sm1_plus =cZERO
 All_S_det=cZERO
 !
 if (i_sp_pol==1) Polarization=cZERO
 !
 nb=Xen%nbf(i_sp_pol)
 !
 do ik_bz=1,nXkbz
   !
   if(.not.PAR_IND_Xk_bz%element_1D(ik_bz)) cycle
   !
   call Build_Overlaps_det_NEQ(Xen,Xk,i_sp_pol,ik_bz,V_bands,&
       &   Sm1_plus(:nb,:nb,:,ik_bz,i_sp_pol),Sm1_minus(:nb,:nb,:,ik_bz,i_sp_pol),&
       &   All_S_det(:,ik_bz,i_sp_pol)) 
   !
 enddo
 !
 call PP_redux_wait(Sm1_minus(:,:,:,:,i_sp_pol),COMM=PAR_COM_Xk_ibz_INDEX%COMM) 
 call PP_redux_wait( Sm1_plus(:,:,:,:,i_sp_pol),COMM=PAR_COM_Xk_ibz_INDEX%COMM)
 !
 call PP_redux_wait(All_S_det(:,:,i_sp_pol),COMM=PAR_COM_Xk_ibz_INDEX%COMM ) 
 !
 call Berry_polarization_EQ(Polarization_Red,All_S_det(:,:,i_sp_pol),Xen,Xk,k_map)
 !
 ! Transform in cartesian coordinates
 !
 !$omp parallel do default(shared), private(id)
 do id=1,3
   Polarization(id)=Polarization(id)+sum(Polarization_red(:)*a(:,id))/DL_vol
 enddo
 !$omp end parallel do
 !
#if defined _TIMING
 call timing('NL Berry Pol NEQ',OPR='stop')
#endif
 !
end subroutine Berry_polarization_NEQ
