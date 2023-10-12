!
! License-Identifier: GPL
!
! Copyright (C) 2020 The Yambo Team
!
! Authors (see AUTHORS file for details): CA
!
subroutine excitons_interpolate_setup(k,Xk,en,Xen,q,ID_INTERP_EXC,ID_INTERP_OCC,report)
 !
 ! This subroutine read and interpolate exciton 
 !
 use pars,          ONLY:SP,schlen,rZERO
 use units,         ONLY:HA2EV
 use R_lattice,     ONLY:bz_samp,coll_scattering,qindx_C
 use electrons,     ONLY:levels,nel_cond,E_reset
 use interpolate,   ONLY:INTERPOLATION_BZ,INTERP_obj,INTERPOLATION_coefficients
 use YPPm,          ONLY:BS_E,EXCITONS_user_indexes,EXCITONS_n_user_states,coo_out,K_transform,&
&                        l_exc_occ_interp,EXCITONS_user_indexes,EXCITONS_n_user_states
 use com,           ONLY:msg
 use IO_int,              ONLY:io_control
 use IO_m,                ONLY:OP_APP_CL,REP,OP_RD_CL
 use stderr,        ONLY:intc
 !
#include<memory.h> 
 !
 type(bz_samp) ::Xk,k,q
 type(levels)  ::Xen,en,E_QF
 logical, intent(in) :: report
 integer, intent(inout) :: ID_INTERP_EXC,ID_INTERP_OCC
 !
 ! Work space
 !
 integer :: iq,i_c,i_l,io_err,ID
 real(SP), allocatable    :: BS_all_E(:,:)
 real(SP), allocatable    :: EXC_occ(:,:)
 real(SP), allocatable    :: values(:)
 real(SP) :: tmp_q(3),E_fermi_h,E_fermi_e
 character(schlen), allocatable :: headings(:)
 integer, external  ::io_QINDX
 !
 ! Allocate arrays for all eigenvalues/eigenvectors
 ! 
 YAMBO_ALLOC(BS_all_E,(EXCITONS_n_user_states,q%nibz))
 !
 call section('*','Excitons Interpolation')
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
 !
 ! Read all eigenvalues and eigenvectors
 !
 do iq=1,q%nibz
   call msg('s','Reading excitons at @ Q-index #',iq)
   call excitons_read(k,Xk,en,Xen,iq,"check",io_err)
   !
   if(l_exc_occ_interp) then
     call excitons_read(k,Xk,en,Xen,iq,"eigenvectors eigenvalues",io_err)
#if defined _YPP_ELPH
     call EXC_occupations(E_QF,Xk,q,iq,EXCITONS_n_user_states,EXCITONS_user_indexes,EXC_occ(:,iq))
#endif
   else
     call excitons_read(k,Xk,en,Xen,iq,"eigenvalues",io_err)
   endif
   !
   do i_c=1,EXCITONS_n_user_states
     i_l=EXCITONS_user_indexes(i_c)
     BS_all_E(i_c,iq)=BS_E(i_l)
   enddo
   !
   call excitons_read(k,Xk,en,Xen,iq,"clean",io_err)
   !
 enddo
 !
 if(report.eqv..true.) then
   !
   ! Write in the report the exciton energies
   !
   YAMBO_ALLOC(values,(EXCITONS_n_user_states+4))
   allocate(headings(EXCITONS_n_user_states+4))
   !
   call msg('nr','Excitons Energies:')
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
 INTERP_obj(ID_INTERP_EXC)%what="excitons"
 call INTERPOLATION_coefficients(R1D=BS_all_E,k=q,NK=q%nibz,ID=ID_INTERP_EXC,ID_obj=ID_INTERP_EXC)
 !
 if(l_exc_occ_interp) then
   INTERP_obj(ID_INTERP_OCC)%what="occupations"
   call INTERPOLATION_coefficients(R1D=EXC_occ,k=q,NK=q%nibz,ID=ID_INTERP_OCC,ID_obj=ID_INTERP_OCC)
 endif
 !
end subroutine
