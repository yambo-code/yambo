!
! License-Identifier: GPL
!
! Copyright (C) 2009 The Yambo Team
!
! Authors (see AUTHORS file for details): DV DS FP
!
subroutine Bare_Hamiltonian_as_E_ks_minus_Vxc(E,Xk,k)
 !
 use pars,           ONLY:cZERO,schlen,lchlen
 use electrons,      ONLY:levels,n_sp_pol,spin,n_spin
 use QP_m,           ONLY:QP_nk,QP_Vxc,QP_n_states
 use hamiltonian,    ONLY:Hzero,E_reference,V_hartree_sc,rho_n,magn_n,V_xc_sc,&
&                         H_ref_bands,WF_G_max,WF_Go_indx
 use parallel_m,     ONLY:PAR_IND_WF_k,PAR_IND_WF_linear
 use parallel_int,   ONLY:PP_redux_wait
 use wave_func,      ONLY:WF
 use com,            ONLY:msg,of_open_close
 use R_lattice,      ONLY:bz_samp,nkibz
 use xc_functionals, ONLY:V_xc
 use interfaces,     ONLY:el_density_and_current,el_magnetization,WF_load
 use H_interfaces,   ONLY:V_real_space_to_H
 use timing_m,       ONLY:timing
 use parser_m,       ONLY:parser
#if defined _SC
 use drivers,        ONLY:l_sc_run,l_sc_magnetic
#endif
#if defined _RT
 use drivers,        ONLY:l_real_time
 use real_time,      ONLY:REF_V_xc_sc,REF_V_hartree_sc,rho_reference,magn_reference
#endif
#if defined _SC
 use drivers,        ONLY:l_sc_magnetic
#endif
#include<y_memory.h>
 !
 type(levels)             :: E
 type(bz_samp)            :: Xk,k
 !
 ! Work space
 !
 integer     :: ik,ib,i_sp_pol,WFbands(2),ib1,ib2
 logical     :: l_write_Hsc
 character(schlen) :: Hsc_file,headings(6),msg_format
 character(lchlen) :: msg_to_dump
 !
 Hzero=cZERO
 !
 call timing('Bare_Hamiltonian_as_E_ks_minus_Vxc',OPR='start')
 !
 l_write_Hsc=.false.  
 call parser('WriteHsc',l_write_Hsc) ! write Hamiltonian on file only for testing purpose 
 !
 if(l_write_Hsc) then
   Hsc_file='H_sc'
   call of_open_close(Hsc_file,'ot')
   call msg('o '//Hsc_file,'#')
   call msg('o '//Hsc_file,'# H-Bare Hamiltonian from Eks - Vxc ') 
   call msg('o '//Hsc_file,'#')
   headings=(/'is_pol','kpt   ','bnd1  ','bnd2  ','Re[H] ','Img[H]'/)
   call msg('o '//Hsc_file,'#',headings,INDENT=0,USE_TABS=.TRUE.)
   call msg('o '//Hsc_file,'#')
   msg_format='(t006,i7,t025,i7,t044,i7,t063,i7,t082,f16.10,t101,f16.10)'
 endif
 !
#if defined _SC
 !
 if (l_sc_magnetic) then
   !
   ! MAG_Hamiltonian
   !=================
   ! - call WF_derivative which computes the derivative in space G
   ! - load the wf in real space
   ! - compute the magnetic part of the hamiltonian, 
   !   allocate and initialise Hzero, write H_magnetic in Hzero 
   !
   call MAG_Hamiltonian()
   !
   ! If exist the non local part of the pseudo then construct kbv
   !
   call Pseudo_KB_G_to_R_space(k,E)
   call Pseudo_Hamiltonian(Xk,E,.true.)
   !call Pseudo_Hamiltonian(Xk,E,.false.)
   !
 endif
 !
#endif
 !
 ! WF Loading, density and magnetization
 !=======================================
 WFbands=(/H_ref_bands(1),max(H_ref_bands(2),maxval(E%nbm))/)
 call WF_load(WF,WF_G_max,WF_Go_indx,WFbands,(/1,nkibz/),title='-SC')
 !
#if defined _RT
 if(l_real_time) then
   rho_n=rho_reference
   if(n_spin>1) magn_n=magn_reference
 endif
#endif
 !
#if defined _SC
 if(l_sc_run) then
   call el_density_and_current(E,Xk,rho=rho_n,bands=H_ref_bands)
   if(n_spin>1) call el_magnetization(E,Xk,magn_n,bands=H_ref_bands)
 endif
#endif
 !
 ! The Hzero = -nabla**2. + V_ext is deduced from the input energies en0:
 !
 ! H = Hzero + v_hxc[rho_in] and
 ! H psi_0(n) = en0(n) psi_0(n)
 !
 ! then Hzero_n,m = en0(n) delta_n,m - v_hxc[rho_0]_n,m
 !
 ! where the matrix elements are calculated from the  psi_0(n)
 !
 ! ... so first store -Vhartree in Hzero ...
 !========================================
 !
 ! V_hartree
 call V_Hartree(rho_n,V_hartree_sc)
 !
 ! QP_Vxc
 YAMBO_ALLOC(QP_Vxc,(QP_n_states))
 QP_Vxc=cZERO
 !
 call XCo_local(E,k)
 !
 V_xc_sc=V_xc
 !
 do i_sp_pol=1,n_sp_pol
   !
   do ik=1,QP_nk
     !
     ! Add bare energies ... 
     !=======================
     !
     do ib=H_ref_bands(1),H_ref_bands(2)
       if (.not.PAR_IND_WF_linear%element_2D(ib,ik)) cycle
       Hzero(ib,ib,ik,i_sp_pol) = Hzero(ib,ib,ik,i_sp_pol) + E_reference%E(ib,ik,i_sp_pol)  
     enddo
     !
#if defined _RT
     if(l_real_time) then
       if (.not.PAR_IND_WF_k%element_1D(ik)) cycle
     endif
#endif
     !
     ! ... then remove Vh[rho_0] 
     !===========================
     !
     call V_real_space_to_H(ik,i_sp_pol,Hzero(:,:,ik,i_sp_pol),WF,'def',V=-V_hartree_sc)
     !
     ! ... then remove Vxc[rho_0] also
     !=================================
     !
     call V_qp_basis_to_H(ik,i_sp_pol,-QP_Vxc,Hzero(:,:,ik,i_sp_pol))
     !
     if(l_write_Hsc) then
       do ib1=H_ref_bands(1),H_ref_bands(2)
         do ib2=H_ref_bands(1),H_ref_bands(2)
           write(msg_to_dump,msg_format) i_sp_pol,ik,ib1,ib2,&
&             real(Hzero(ib1,ib2,ik,i_sp_pol)),aimag(Hzero(ib1,ib2,ik,i_sp_pol))
           call msg('o '//Hsc_file,msg_to_dump)
         enddo
       enddo
     endif
     !
   enddo
   !
 enddo
 !
#if defined _RT
 !
 ! Save the reference xc and Hartree potentials 
 !==============================================
 !
 if (l_real_time) then
   REF_V_xc_sc=V_xc
   REF_V_hartree_sc=V_hartree_sc
 endif
 !
#endif
 !
 if(l_write_Hsc) call of_open_close(Hsc_file)
 !
 YAMBO_FREE(QP_Vxc)
 !
 call PP_redux_wait(Hzero)
 !
 call timing('Bare_Hamiltonian_as_E_ks_minus_Vxc',OPR='stop')
 !
end subroutine Bare_Hamiltonian_as_E_ks_minus_Vxc
