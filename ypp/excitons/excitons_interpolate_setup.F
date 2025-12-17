!
! License-Identifier: GPL
!
! Copyright (C) 2020 The Yambo Team
!
! Authors (see AUTHORS file for details): CA
!
! headers
!
#include<y_memory.h> 
!
subroutine excitons_interpolate_setup(k,Xk,en,Xen,q,BS_all_E,ID_INTERP_EXC,ID_INTERP_OCC,report)
 !
 ! This subroutine read and interpolate exciton 
 !
 use pars,          ONLY:SP,schlen,rZERO
 use units,         ONLY:HA2EV
 use R_lattice,     ONLY:bz_samp,coll_scattering,qindx_C
 use electrons,     ONLY:levels,nel_cond,E_reset
 use interpolate,   ONLY:INTERPOLATION_BZ,INTERP_obj,INTERPOLATION_coefficients
 use YPPm,          ONLY:BS_E,EXCITONS_user_indexes,EXCITONS_n_user_states,coo_out,K_transform,&
 &                       l_exc_occ_interp,EXCITONS_n_user_states,&
 &                       l_magnons,BS_R_left,BS_R_right,BS_R_left_magn,BS_R_right_magn,exc_kind
 use BS_solvers,    ONLY:BSS_n_eig
 use com,           ONLY:msg
 use IO_int,              ONLY:io_control
 use IO_m,                ONLY:OP_APP_CL,REP,OP_RD_CL
 use stderr,        ONLY:intc
 use y_memory_alloc
 !
 implicit none
 !
 type(bz_samp) ::Xk,k,q
 type(levels)  ::Xen,en
 real(SP),intent(out)  :: BS_all_E(EXCITONS_n_user_states,q%nibz)
 logical, intent(in) :: report
 integer, intent(inout) :: ID_INTERP_EXC,ID_INTERP_OCC
 !
 ! Work space
 !
 type(levels)  ::E_QF
 integer :: iq,i_c,i_l,io_err,ID,EXC_n_states_ref,BSS_n_eig_ref,n_eig
 real(SP), allocatable    :: EXC_occ(:,:)
 real(SP), allocatable    :: values(:)
 real(SP) :: Res(BSS_n_eig),BS_E_degs(BSS_n_eig)
 real(SP) :: tmp_q(3),E_fermi_h,E_fermi_e
 character(schlen), allocatable :: headings(:)
 integer, external  ::io_QINDX
 !
 EXC_n_states_ref=EXCITONS_n_user_states
 BSS_n_eig_ref=BSS_n_eig
 ! 
 do i_c=1,EXC_n_states_ref
   i_l=EXCITONS_user_indexes(i_c)
   BS_all_E(i_c,1)=BS_E(i_l)
 enddo
 !
 call section('*',exc_kind//'s Interpolation')
 !
 if(nel_cond/=rZERO) then
   call msg('rs','Excitonic quasi-fermi distribution interpolation')
   l_exc_occ_interp=.TRUE.
   call E_reset(E_QF)
   call OCCUPATIONS_Quasi_Fermi(Xen,Xk,E_QF,E_fermi_h,E_fermi_e)
   if(.not.allocated(q%k_table))  call k_build_up_BZ_tables(q)
   if(.not.allocated(qindx_C)) then
     if(coll_scattering.eqv..FALSE.) call error('Please run again the setup with: K_grids= "X B C S" ')
     call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/6/),ID=ID)
     io_err=io_QINDX(k,q,ID,'minus_q')
     if (io_err/=0) then
       call msg('s',"Missing k/q scattering database")
       call error('Please (re)run the setup K_grids="C"')
     endif
   endif
#if defined _YPP_ELPH
   YAMBO_ALLOC(EXC_occ,(EXCITONS_n_user_states,q%nibz))
#else
   call error("Excitonic occupations only available in ypp_ph")
#endif
 endif
 !
 call excitons_read(k,Xk,en,Xen,1,"clean",io_err)
 if(io_err/=0) call error("Error reading exciton at q = "//intc(iq))
 !
 ! Read all eigenvalues and eigenvectors
 !
 n_eig=BSS_n_eig
 do iq=2,q%nibz
   call msg('s','Reading '//exc_kind//'s at @ Q-index #',iq)
   call excitons_read(k,Xk,en,Xen,iq,"check",io_err)
   !
   if(l_exc_occ_interp) then
     call excitons_read(k,Xk,en,Xen,iq,"eigenvectors eigenvalues",io_err)
#if defined _YPP_ELPH
     call EXC_occupations(E_QF,Xk,q,iq,EXC_n_states_ref,EXCITONS_user_indexes,EXC_occ(:,iq))
#endif
   else
     call excitons_read(k,Xk,en,Xen,iq,"eigenvalues",io_err)
   endif
   !
   n_eig=min(n_eig,BSS_n_eig)
   if (     l_magnons)  Res(:n_eig)=real(BS_R_left_magn(:n_eig,1)*BS_R_right_magn(:n_eig,1),SP)
   if (.not.l_magnons)  Res(:n_eig)=real(BS_R_left(:n_eig)*BS_R_right(:n_eig),SP)
   call excitons_get_user_states(BS_E_degs(:n_eig),Res(:n_eig),n_eig,.true.)
   if(EXC_n_states_ref>size(EXCITONS_user_indexes)) then
     call error("Degenerate excitons please increase the State range to : 1-"//intc(EXC_n_states_ref))
   endif
   do i_c=1,EXC_n_states_ref
     i_l=EXCITONS_user_indexes(i_c)
     BS_all_E(i_c,iq)=BS_E(i_l)
   enddo
   !
   call excitons_read(k,Xk,en,Xen,iq,"clean",io_err)
   !
 enddo
 !
 EXCITONS_n_user_states=EXC_n_states_ref
 BSS_n_eig=BSS_n_eig_ref
 !
 if(report.eqv..true.) then
   !
   ! Write in the report the exciton energies
   !
   YAMBO_ALLOC(values,(EXCITONS_n_user_states+4))
   allocate(headings(EXCITONS_n_user_states+4))
   !
   call msg('nr',exc_kind//'s Energies:')
   headings(1:3)=(/"q_x ("//trim(coo_out)//")","q_y ("//trim(coo_out)//")","q_z ("//trim(coo_out)//")"/)
   !
   do i_c=1,EXCITONS_n_user_states
      i_l=EXCITONS_user_indexes(i_c)
      headings(i_c+3)=' e'//trim(intc(i_l))//' [eV]'
   enddo
   call msg('r',"#",headings(1:EXCITONS_n_user_states+3))
   do iq=1,q%nibz
     tmp_q=q%pt(iq,:3)
     call K_transform(tmp_q,'iku')
     values(1:3)=tmp_q
     do i_c=1,EXCITONS_n_user_states
        i_l=EXCITONS_user_indexes(i_c)
        values(i_c+3)=BS_all_E(i_c,iq)*HA2EV
     enddo
     call msg('r',' ',values(1:EXCITONS_n_user_states+3))
   enddo
   !
   YAMBO_FREE(values)
   deallocate(headings)
   !
 endif
 !
 ! Fourier interpolation of exciton dispersion
 !
 call INTERPOLATION_BZ_setup(q)
 INTERP_obj(ID_INTERP_EXC)%what=exc_kind//"s"
 call INTERPOLATION_coefficients(R1D=BS_all_E,k=q,NK=q%nibz,ID=ID_INTERP_EXC,ID_obj=ID_INTERP_EXC)
 !
 if(l_exc_occ_interp) then
   INTERP_obj(ID_INTERP_OCC)%what="occupations"
   call INTERPOLATION_coefficients(R1D=EXC_occ,k=q,NK=q%nibz,ID=ID_INTERP_OCC,ID_obj=ID_INTERP_OCC)
 endif
 !
end subroutine
