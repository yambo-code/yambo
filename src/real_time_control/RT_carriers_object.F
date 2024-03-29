! 
! License-Identifier: GPL
!
! Copyright (C) 2015 The Yambo Team
!
! Authors (see AUTHORS file for details): DS AM
!
subroutine RT_carriers_object(RT,E,K,DB_id,WHAT)
 !
 ! This subroutine contains the RT objects, included the RT table
 ! In the future the RT code can be based on those, at least partially
 !
 use pars,           ONLY:SP,schlen
 use iso_c_binding,  ONLY:C_LOC,C_F_POINTER
 use drivers,        ONLY:l_elel_scatt,l_elph_scatt,l_elphoton_scatt
 use electrons,      ONLY:n_sp_pol,spin_occ,levels
 use R_lattice,      ONLY:bz_samp
 use RT_control,     ONLY:CARR_RT_IO_t
 use RT_occupations, ONLY:RT_el_occ
 use RT_lifetimes,   ONLY:RT_EP_emit_life,RT_EE_life,RT_EH_life,RT_EP_abs_life,&
&                         RT_EP_emit_ref_life,RT_EE_ref_life,RT_EH_ref_life,RT_EP_abs_ref_life,&
&                         RT_obj_lifetime
 use RT_operations,  ONLY:EMIT_RT_life_REF,ABS_RT_life_REF,ABS_RT_life_dGAMMA,&
&                         EMIT_RT_life_dGAMMA,RT_lifetime_to_RT_obj
 use real_time,      ONLY:RT_t,RTibz,RT_nk,RT_bands,RT_obj_alloc,RT_obj_free,&
&                         RT_levels,RT_is_dynamically_dephased
 use hamiltonian,    ONLY:E_reference
 use IO_m,           ONLY:NF90_unlimited
 use IO_int,         ONLY:def_variable_bulk,io_variable_bulk
 !
 implicit none
 !
 ! WHAT=1 allocate and define, 2 deallocate
 !
 type(RT_t),      target, intent(inout):: RT
 type(levels),  intent(inout),optional :: E
 type(bz_samp), intent(in)   ,optional :: K
 integer,       intent(in)   ,optional :: DB_id
 character(*),  intent(in)   ,optional :: WHAT
 !
 integer  :: i_RT,i_b,i_k,iE_fg,i_k_RT,i_spin
 real(SP) :: Hole_lifetime,Elec_lifetime,df_occ,f_occ,f_hol
 !
 integer           :: VAR_SZ(2),IPOS(2),TMP_SIZE(2)
 real(SP), pointer :: TMP_VAR(:,:)
 character(schlen) :: dim_names(2)
 !
 ! LIFETIMES
 !===========
 !
 if ((WHAT=='fill'.or.WHAT=='dump'.or.WHAT=="fill_ref").and.RT_is_dynamically_dephased) then
   do i_RT=1,RT%nstates
     i_b    =RT%table(i_RT,1)
     i_k    =RT%table(i_RT,2)
     i_k_RT =RT%table(i_RT,3)
     call RT_lifetime_to_RT_obj(WHAT,i_b,i_k_RT,i_RT,RT_EH_life,RT_EH_ref_life,RT%GAMMA_EH)
     call RT_lifetime_to_RT_obj(WHAT,i_b,i_k_RT,i_RT,RT_EE_life,RT_EE_ref_life,RT%GAMMA_EE)
     call RT_lifetime_to_RT_obj(WHAT,i_b,i_k_RT,i_RT,RT_EP_emit_life,RT_EP_emit_ref_life,RT%GAMMA_EP_emit)
     call RT_lifetime_to_RT_obj(WHAT,i_b,i_k_RT,i_RT,RT_EP_abs_life,RT_EP_abs_ref_life,RT%GAMMA_EP_abs) 
   enddo
 endif
 !
 select case(WHAT)
   !
   case('allocate')
     !
     ! Allocate and define
     !
     RT%nk=RT_nk
     RT%nb=RT_bands
     RT%nstates=RT_nk*(RT_bands(2)-RT_bands(1)+1)
     !
     call RT_obj_alloc(RT)
     !
     RT%E_bare=0._SP
     RT%OCC_el%N_ref=0._SP
     !
     do i_k=1,K%nibz
       do i_k_RT=RTibz%k_range(i_k,1),RTibz%k_range(i_k,2)
         RT%kpt(i_k_RT,:)    = RTibz%pt(i_k_RT,:)
         RT%k_weight(i_k_RT) = RTibz%weights( i_k_RT)
       enddo
     enddo
     !
     call build_RT_table()
     !
     do i_RT=1,RT%nstates
       !
       i_b    =RT%table(i_RT,1)
       i_k    =RT%table(i_RT,2)
       i_k_RT =RT%table(i_RT,3)
       i_spin =RT%table(i_RT,4)
       !
       if(i_k_RT/=RTibz%k_range(i_k,1)) then
         iE_fg=RTibz%E_map(i_k_RT)
         RT%E_bare(i_RT)=E_reference%FG%E(i_b,iE_fg,i_spin)
         if(.not.allocated(E%fo)) RT%OCC_el%N_ref(i_RT)=E%FG%f(i_b,iE_fg,i_spin)
         if(     allocated(E%fo)) RT%OCC_el%N_ref(i_RT)=E%FG%f(i_b,iE_fg,i_spin)-E%FG%df(i_b,iE_fg,i_spin)
       else
         RT%E_bare(i_RT)=E_reference%E(i_b,i_k,i_spin)
         if(.not.allocated(E%fo)) RT%OCC_el%N_ref(i_RT)=E%f(i_b,i_k,i_spin)
         if(     allocated(E%fo)) RT%OCC_el%N_ref(i_RT)=E%fo(i_b,i_k,i_spin)
       endif
       !
     enddo
     !
   case('dump')
     !
     do i_RT=1,RT%nstates
       !
       i_b    =RT%table(i_RT,1)
       i_k    =RT%table(i_RT,2)
       i_k_RT =RT%table(i_RT,3)
       i_spin =RT%table(i_RT,4)
       !
       ! Occupations & Energies
       !
       if (allocated(RT_el_occ%dN)) RT_el_occ%dN(i_b,i_k_RT)      =RT%OCC_el%dN(i_RT)
       if (allocated(RT_levels%E) ) RT_levels%E(i_b,i_k_RT,i_spin)=RT%delta_E(i_RT)+RT%E_bare(i_RT)
       !
     enddo
     !
   case('fill')
     !
     do i_RT=1,RT%nstates
       !
       i_b    =RT%table(i_RT,1)
       i_k    =RT%table(i_RT,2)
       i_k_RT =RT%table(i_RT,3)
       i_spin =RT%table(i_RT,4)
       !
       ! Occupations & Energies
       !
       RT%OCC_el%dN(i_RT)=RT_el_occ%dN(i_b,i_k_RT)
       RT%delta_E(i_RT)  =RT_levels%E(i_b,i_k_RT,i_spin)-RT%E_bare(i_RT)
       !
       f_occ =RT%OCC_el%N_ref(i_RT)/spin_occ
       f_hol =1._SP-f_occ
       df_occ=RT%OCC_el%dN(i_RT)/spin_occ
       if(any((/RT_EP_emit_life%active,RT_EE_life%active,RT_EH_life%active/))) then
         Elec_lifetime=EMIT_RT_life_REF(i_b,i_k_RT)+EMIT_RT_life_dGAMMA(i_b,i_k_RT)
         Hole_lifetime=ABS_RT_life_REF(i_b,i_k_RT) +ABS_RT_life_dGAMMA(i_b,i_k_RT)
         if(RT%E_bare(i_RT)> 0._SP) then
           RT%NEQ_Lifetime(i_RT)=  Elec_lifetime
           if ( abs(f_occ+df_occ)>0._SP ) RT%NEQ_Lifetime(i_RT)=RT%NEQ_Lifetime(i_RT)-Hole_lifetime/(f_occ+df_occ)+Hole_lifetime
         else if(RT%E_bare(i_RT)<=0._SP) then
           RT%NEQ_Lifetime(i_RT)=  Hole_lifetime
           if ( abs(f_hol-df_occ)>0._SP ) RT%NEQ_Lifetime(i_RT)=RT%NEQ_Lifetime(i_RT)-Elec_lifetime/(f_hol-df_occ)+Elec_lifetime
         endif
       endif
       !
     enddo
     !
   case('ref_io')
     !
     call def_variable_bulk(DB_id,"RT_carriers_E_bare",7,(/RT%nstates/),SP)
     call  io_variable_bulk(DB_id,7,R1=RT%E_bare)
     call def_variable_bulk(DB_id,"RT_carriers_f_bare",8,(/RT%nstates/),SP)
     call  io_variable_bulk(DB_id,8,R1=RT%OCC_el%N_ref)
     !
     call LOCAL_GAMMA_IO("bare",RT%GAMMA_EH,9)
     call LOCAL_GAMMA_IO("bare",RT%GAMMA_EE,11)
     call LOCAL_GAMMA_IO("bare",RT%GAMMA_EP_abs,13)
     call LOCAL_GAMMA_IO("bare",RT%GAMMA_EP_emit,15)
     !
   case('def_io_variables')
     !
     VAR_SZ=(/RT%nstates,NF90_unlimited/)
     dim_names  =[character(schlen) :: 'RT_nstates', 'time']
     !
     ! Energies
     !
     call def_variable_bulk(DB_id,"RT_carriers_delta_E",17,VAR_SZ,SP,dim_names=dim_names,silent=.true.)
     !
     ! Occupations
     !
     call def_variable_bulk(DB_id,"RT_carriers_delta_f",18,VAR_SZ,SP,dim_names=dim_names,silent=.true.)
     !
     ! Lifetimes
     !
     if (l_elel_scatt.or.l_elph_scatt.or.l_elphoton_scatt) then
       call def_variable_bulk(DB_id,"RT_NEQ_Lifetime",19,VAR_SZ,SP,dim_names=dim_names,silent=.true.)
     endif
     !
     call LOCAL_GAMMA_IO("def",RT%GAMMA_EE,20)
     call LOCAL_GAMMA_IO("def",RT%GAMMA_EH,22)
     call LOCAL_GAMMA_IO("def",RT%GAMMA_EP_emit,24)
     call LOCAL_GAMMA_IO("def",RT%GAMMA_EP_abs,26)
     !
   case('do_io_variables')
     !
     IPOS=(/1,CARR_RT_IO_t%N/)
     TMP_SIZE=(/RT%nstates,1/)
     !
     ! Energies
     !
     call C_F_POINTER(C_LOC(RT%delta_E), TMP_VAR, TMP_SIZE)
     call io_variable_bulk(DB_id,17,R2=TMP_VAR,IPOS=IPOS)
     !
     ! Occupations
     !
     call C_F_POINTER(C_LOC(RT%OCC_el%dN), TMP_VAR, TMP_SIZE)
     call io_variable_bulk(DB_id,18,R2=TMP_VAR,IPOS=IPOS)
     !
     ! Lifetimes
     !
     if (l_elel_scatt.or.l_elph_scatt.or.l_elphoton_scatt) then
       !
       call C_F_POINTER(C_LOC(RT%NEQ_Lifetime), TMP_VAR, TMP_SIZE)
       call io_variable_bulk(DB_id,19,R2=TMP_VAR,IPOS=IPOS)
       !
     endif
     !
     call LOCAL_GAMMA_IO("IO",RT%GAMMA_EE,20)
     call LOCAL_GAMMA_IO("IO",RT%GAMMA_EH,22)
     call LOCAL_GAMMA_IO("IO",RT%GAMMA_EP_emit,24)
     call LOCAL_GAMMA_IO("IO",RT%GAMMA_EP_abs,26)
     !
   case('free')
     !
     ! Free
     !
     call RT_obj_free(RT)
     !
 end select
 !
 contains
   !
   subroutine build_RT_table()
     !
     i_RT=0
     !
     do i_k_RT=1,RT%nk
       do i_b=RT%nb(1),RT%nb(2)
         do i_spin=1,n_sp_pol
           !
           i_RT=i_RT+1
           RT%table(i_RT,1)=i_b
           RT%table(i_RT,2)=RTibz%k_map(i_k_RT)
           RT%table(i_RT,3)=i_k_RT
           RT%table(i_RT,4)=i_spin
           !
         enddo
       enddo
     enddo
     !
   end subroutine
   !
   subroutine LOCAL_GAMMA_IO(what,LIFE,N)
     type(RT_obj_lifetime), target:: LIFE
     character(*)                 :: what
     integer                      :: N
     if (.not.LIFE%active) return
     if (what=="bare") then
       call def_variable_bulk(DB_id,trim(LIFE%kind)//"_GAMMA_emit_bare",N,(/RT%nstates/),SP)
       call  io_variable_bulk(DB_id,N,R1=LIFE%emit_bare)
       call def_variable_bulk(DB_id,trim(LIFE%kind)//"_GAMMA_abs_bare",N+1,(/RT%nstates/),SP)
       call  io_variable_bulk(DB_id,N+1,R1=LIFE%abs_bare)
     endif
     if (what=="def") then
       call def_variable_bulk(DB_id,trim(LIFE%kind)//"_GAMMA_emit",N,VAR_SZ,SP,dim_names=dim_names,silent=.true.)
       call def_variable_bulk(DB_id,trim(LIFE%kind)//"_GAMMA_abs",N+1,VAR_SZ,SP,dim_names=dim_names,silent=.true.)
     endif
     if (what=="IO") then
       call C_F_POINTER(C_LOC(LIFE%emit), TMP_VAR, TMP_SIZE)
       call io_variable_bulk(DB_id,N,R2=TMP_VAR,IPOS=IPOS)
       call C_F_POINTER(C_LOC(LIFE%abs), TMP_VAR, TMP_SIZE)
       call io_variable_bulk(DB_id,N+1,R2=TMP_VAR,IPOS=IPOS)
     endif
   end subroutine
   !
end subroutine RT_carriers_object
