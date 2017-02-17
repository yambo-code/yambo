!
!        Copyright (C) 2000-2016 the YAMBO team
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
subroutine Dipole_driver(Xen,Xk,X,field_dir)
 !
 ! Presently there exist two gauges: (i ) the length   which uses <r>            
 !                                   (ii) the velocity which uses <v>=<p-[x,Vnl]> 
 !
 ! DIP_iR=i<r>
 ! DIP_P = <v>   (should be called DIP_V ... )
 !
 ! For each of the two gauges there exist three approaches of computing the dipoles
 ! 1) G space approach      --> directly computes <v> and then <r>=<v>/DeltaE (called transverse approach as well)
 ! 2) real space approach   --> directly computes <r> and then <v>=<r>*DeltaE (only for non periodic directions)
 ! 2) shifted kpts approach --> computes q*<nk|r|mk> as <nk|r|mk+q> with small q (phases not defined)
 !                              and then <v> as <r>*DeltaE
 ! 3) covariant approach    --> computes <nk|r|mk> in reciprocal space as <nk|partial_k|mk>
 !                              takes into account the phases. symmetries not yet implemented 
 !                              and then <v> as <r>*DeltaE
 !
 use drivers,        ONLY:l_optics
 use pars,           ONLY:SP,cZERO
 use com,            ONLY:warning
 use electrons,      ONLY:levels,n_sp_pol
 use R_lattice,      ONLY:bz_samp
 use X_m,            ONLY:X_alloc,X_t,DIP_iR,use_covariant_approach,use_real_space_approach,&
&                         Vnl_commutator_warning,use_g_space_approach,Dipole_approach,&
&                         use_shifted_grids_approach,DIP_P,force_v_g_space_approach,Dipole_Energy_treshold
 use IO_m,           ONLY:io_control,OP_RD_CL,VERIFY,REP,OP_WR_CL,OP_APP_WR_CL,OP_RD,&
&                         IO_and_Messaging_switch
 use wave_func,      ONLY:wf_ng
 use parallel_m,     ONLY:PAR_IND_DIPk_ibz,PAR_DIPk_nibz,master_cpu,PAR_COM_k_for_P,&
&                         PAR_COM_q_for_Xo,PP_redux_wait,PAR_COM_Plasma_INDEX,PAR_COM_Q_INDEX,l_par_RT
 use parser_m,       ONLY:parser
 !
 implicit none
 !
 type(bz_samp), intent(inout) :: Xk
 type(levels),  intent(inout) :: Xen
 type(X_t),     intent(inout) :: X
 real(SP),      intent(inout) :: field_dir(3)
 !
 ! Work Space
 !
 integer           :: ik,i_sp_pol
 logical           :: use_dipole_transverse,Dipole_bands_ordered,idir_not_done(3)
 !
 ! I/O
 !
 integer           :: ID,io_err
 integer, external :: io_DIPOLES
 !
 ! Setup logicals 
 !================
 !
 use_g_space_approach        = trim(Dipole_approach)=='G-space v'
 use_real_space_approach     = trim(Dipole_approach)=='R-space x'
 use_covariant_approach      = trim(Dipole_approach)=='Covariant'
 use_shifted_grids_approach  = trim(Dipole_approach)=='Shifted grids'
 !
 call parser('PDirect'  ,force_v_g_space_approach)
 !
 if(.not.any((/use_g_space_approach,use_real_space_approach,use_covariant_approach,use_shifted_grids_approach/))) then
   call warning(' Dipoles approach defined in input not recognised. Reverting to G-space v')
   use_g_space_approach = .true.
   Dipole_approach='G-space v'
 endif
 !
 force_v_g_space_approach=force_v_g_space_approach.and.(.not.use_g_space_approach)
 use_dipole_transverse   =force_v_g_space_approach.or.       use_g_space_approach
 idir_not_done           =.true.
 !
 X%ngostnts=wf_ng
 !
 !
 ! NOTE: 1 - that in case we will enter here during a SC run to update the
 !           screened interaction all transitions must be considered in order to rotate <P>.
 !       2 - when using internal SC potentials oscilators must be rotated
 !           in the new basis. So all transitions are needed.
 !       3 - In real-time simulations P and P^2 are ALWAYS calculated => no band ordering
 !
 Dipole_bands_ordered=.true.
 !
 !
 ! Set up band limits
 !
 if (Dipole_bands_ordered.or.Dipole_Energy_treshold<0._SP) then
   X%ib_lim(1)=Xen%nbm
   X%ib_lim(2)=Xen%nbf+1
 else
   X%ib_lim(1)=X%ib(2)
   X%ib_lim(2)=X%ib(1)
 endif
 !
 ! Check if Dipoles DBs exist and are ok
 !=======================================
 call Dipoles_IO('read ')
 !
 ! In case dipoles were not found/ok then I need to compute them
 !==============================================================
 if (io_err/=0) then
   !
   ! I/O privilegies: temporarly switch it on
   !
   call IO_and_Messaging_switch("+io_out")
   !
   ! Allocation
   !
   call DIPOLES_alloc()
   !
   !
   if (use_dipole_transverse)      call DIPOLE_transverse(Xen,Xk,X)
   !
   call DIPOLES_ppredux_and_symmetrize()
   !
   ! If the calculation of the dipoles is very short the I/O is switched off
   !
   call DIPOLES_IO('write')
   !
   ! I/O privilegies: RESTORE to previous values
   !
   call IO_and_Messaging_switch("RESTORE")
   !
 endif
 !
 !
 ! Warn about missing [Vnl,r] commutator
 !
 if (.not.X%Vnl_included.and..not.Vnl_commutator_warning.and.l_optics.and.use_dipole_transverse) then
   call warning(' Missing non-local pseudopotential contribution')
   Vnl_commutator_warning=.TRUE.
 endif
 !
 ! Finally project the dipoles along q
 !=====================================
 call DIPOLE_project_along_q(Xen,Xk,X,field_dir)
 !
 ! Clean up
 !
 call X_alloc('DIP_iR') 
 call X_alloc('DIP_P')
 !
 contains
   !
   subroutine DIPOLES_IO(read_or_write)
     !
     use com,        ONLY:msg
     !
     character(5), intent(in) :: read_or_write
     !
     integer                  :: IO_ACTION
     logical                  :: write_header,read_header,reading,writing
     !
     reading=trim(read_or_write)=="read"
     writing=trim(read_or_write)=="write"
     !
     ID=0
     write_header  =(     master_cpu.and.writing)
     read_header   =(.not.master_cpu.and.writing).or.reading
     !
     if(write_header) then
       call msg('s','[DIP] Writing dipoles header ')
       call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1/),MODE=VERIFY,ID=ID)
       io_err=io_DIPOLES(X,Xen,ID)
     endif
     !
     call PP_redux_wait(COMM=PAR_COM_q_for_Xo%COMM)
     !
     if(read_header) then
       call msg('s','[DIP] Checking dipoles header ')
       call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1/),MODE=VERIFY,ID=ID)
       io_err=io_DIPOLES(X,Xen,ID)
     endif
     !
     if (io_err==0) then
       !
       if(reading) IO_ACTION=OP_RD_CL
       if(writing) IO_ACTION=OP_APP_WR_CL
       !
       do ik=1,Xk%nibz
         if (.not.PAR_IND_DIPk_ibz%element_1D(ik)        ) cycle
         if (.not.PAR_COM_k_for_P%CPU_id==0 .and. writing) cycle
         do i_sp_pol=1,n_sp_pol
           call io_control(ACTION=IO_ACTION,COM=REP,SEC=(/1+ik+(i_sp_pol-1)*Xk%nibz/),ID=ID)
           io_err=io_DIPOLES(X,Xen,ID)
        enddo
       enddo
       !
     endif
     !
   end subroutine DIPOLES_IO
   !
   !
   subroutine DIPOLES_alloc()
     !
     call X_alloc('DIP_iR',(/3,X%ib_lim(2),X%ib(2),X%ib(1),X%ib_lim(1),PAR_DIPk_nibz/))
     call X_alloc('DIP_P' ,(/3,X%ib_lim(2),X%ib(2),X%ib(1),X%ib_lim(1),PAR_DIPk_nibz/))
     DIP_iR=cZERO
     DIP_P=cZERO
     !
     !
   end subroutine DIPOLES_alloc
   !
   !
   subroutine DIPOLES_ppredux_and_symmetrize()
    !
    use parallel_m,  ONLY:PAR_DIPk_ibz_index
    !
    integer  :: ic,iv,i_sp_pol,ik,ik_mem
    !
    do ik=1,Xk%nibz
      !
      if (.not.PAR_IND_DIPk_ibz%element_1D(ik)) cycle
      !
      ik_mem=PAR_DIPk_ibz_index(ik)
      ! 
      do i_sp_pol=1,n_sp_pol
        !
        call PP_redux_wait(DIP_iR(:,:,:,ik_mem,i_sp_pol),COMM=PAR_COM_k_for_P%COMM)
        call PP_redux_wait( DIP_P(:,:,:,ik_mem,i_sp_pol),COMM=PAR_COM_k_for_P%COMM)
        !
      enddo
      !
      ! Symmetrization 
      !================
      ! Impose P(/iR) to be Hermitian (/anti-Hermitian)
      !
      do i_sp_pol=1,n_sp_pol
        do iv=X%ib(1),X%ib_lim(1)
          if(iv>=X%ib_lim(2)) then
            DIP_iR(:,iv,iv,ik_mem,i_sp_pol)=cmplx(0.,aimag(DIP_iR(:,iv,iv,ik_mem,i_sp_pol)),SP)
            DIP_P (:,iv,iv,ik_mem,i_sp_pol)=cmplx( real(DIP_P(:,iv,iv,ik_mem,i_sp_pol)),0. ,SP)
          endif
          do ic=iv+1,X%ib(2)
            if( iv>=X%ib_lim(2) .and. ic<=X%ib_lim(1) ) then
              DIP_iR(:,iv,ic,ik_mem,i_sp_pol) = -conjg(DIP_iR(:,ic,iv,ik_mem,i_sp_pol))
              DIP_P (:,iv,ic,ik_mem,i_sp_pol) =  conjg(DIP_P (:,ic,iv,ik_mem,i_sp_pol))
            endif
          enddo
        enddo
      enddo
      !
    enddo
    !
   end subroutine DIPOLES_ppredux_and_symmetrize
   !
end subroutine
