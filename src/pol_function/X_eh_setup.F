!
! License-Identifier: GPL
!
! Copyright (C) 2007 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
integer function X_eh_setup(iq,X,Xen,Xk,minmax_ehe)
 !
 use pars,          ONLY:SP
 use X_m,           ONLY:X_t,X_poles,X_Ein_poles,X_poles_tab,l_X_terminator,X_term_E
 use R_lattice,     ONLY:qindx_X,bz_samp
 use electrons,     ONLY:levels,n_sp_pol,spin_occ
 use frequency,     ONLY:ordered_grid_index
 use parallel_m,    ONLY:PAR_IND_CON_BANDS_X,PAR_IND_Xk_bz,PAR_IND_VAL_BANDS_X
 !
#include<y_memory.h>
 !
 type(levels)  ::Xen 
 type(bz_samp) ::Xk
 type(X_t)     ::X
 integer       ::iq
 real(SP)      ::minmax_ehe(2)
 !
 ! Work Space
 !
 integer :: ik_bz,i_sym,ik,ik_m_q,iv,ic,Nv(n_sp_pol),Nc(n_sp_pol),i_sp,i_pole,ic_min,Nc_XTerm(n_sp_pol),N_tot
 logical :: MPI_logicals_on,l_XTerm_vv
 real(SP):: E_eh,E_in,f_eh,local_tresh
 real(SP), allocatable :: poles(:),Ein_poles(:)
 !
 do i_sp=1,n_sp_pol
   Nv(i_sp)=Xen%nbm(i_sp)-X%ib(1)+1
   Nc(i_sp)=X%ib(2)-Xen%nbf(i_sp)
 enddo
 !
 Nc_XTerm=0
 if (l_X_terminator) Nc_XTerm=Nv
 !
 N_tot=0
 do i_sp=1,n_sp_pol
   N_tot=N_tot+Nv(i_sp)*(Nc(i_sp)+Nc_XTerm(i_sp))
 enddo
 !
 YAMBO_ALLOC(poles,(Xk%nbz*N_tot))
 YAMBO_ALLOC(Ein_poles,(Xk%nbz*N_tot))
 !
 MPI_logicals_on=allocated(PAR_IND_Xk_bz%element_1D)
 !
 ! Note that in metals not all the elements are defined
 !
 poles=0._SP 
 Ein_poles=0._SP
 !
 !
 X_eh_setup=0
 local_tresh=epsilon(1._SP)
 !
 do i_sp=1,n_sp_pol
 do ik_bz=1,Xk%nbz
   !
   if (MPI_logicals_on) then
     if (.not.PAR_IND_Xk_bz%element_1D(ik_bz)) cycle 
   endif
   !
   do iv=X%ib(1),X%ib(1)+Nv(i_sp)-1
     !
     if (MPI_logicals_on) then
       if (.not.PAR_IND_VAL_BANDS_X(X%whoami)%element_1D(iv)) cycle 
     endif
     !
     ! setting terminator bnd range
     ic_min = X%ib(2)-Nc(i_sp)+1
     if (l_X_terminator) ic_min=X%ib(1) 
     !
     do  ic=ic_min,X%ib(2)
         !
         if (MPI_logicals_on) then
           if (.not.PAR_IND_CON_BANDS_X(X%whoami)%element_1D(ic)) cycle 
         endif
         !
         i_sym  =qindx_X(iabs(iq),ik_bz,1)
         ik     =Xk%sstar(ik_bz,1) 
         ik_m_q =Xk%sstar(i_sym,1)
         !
         ! terminator handling
         if (ic>=X%ib(1).and.ic<=Xen%nbm(i_sp).and.l_X_terminator) then
           l_XTerm_vv=.TRUE.
           E_eh=X_term_E-Xen%E(iv,ik_m_q,i_sp)       
         else
           l_XTerm_vv=.FALSE.
           E_eh=Xen%E(ic,ik,i_sp)-Xen%E(iv,ik_m_q,i_sp)
         endif 
         !
         E_in=Xen%E(iv,ik_m_q,i_sp)
         !
         ! Note that all possible E_eh signs are accepted. Negative
         ! transitions energies appear at finite temperature.
         !
         ! The way to distinguish between resonant and anti-reonant transitions
         ! is to check fv(1-fc) factor that comes from the t>0 ordering
         ! of the G's function. In this way, however, E_eh can be negative as
         ! shown below
         !
         ! n(E) ----     
         !          *    
         !          4*  3
         !            -.   
         !              *
         !          1   2-._______
         !          -Eeh-
         !
         !   1,2 = v , 3,4 = c
         !
         !   both transitions 1->3 , 2->4 are resonant but 2->4 has negative energy
         !
         ! When iv--> iv transitions are counted, f_eh=0. 
         ! Therefore  if (abs(f_eh)<local_tresh) cycle hold only 
         ! when the terminator  is off 
         !
         if (.not.l_XTerm_vv) f_eh=Xen%f(iv,ik_m_q,i_sp)*(spin_occ-Xen%f(ic,ik,i_sp))/spin_occ
         if (     l_XTerm_vv) f_eh=Xen%f(iv,ik_m_q,i_sp)*Xen%f(ic,ik,i_sp)/spin_occ
         !
         if (abs(f_eh)<local_tresh)  cycle  
         !                                                              
         if (any((/abs(E_eh)<X%ehe(1),abs(E_eh)>X%ehe(2).and.X%ehe(2)>0._SP/))) cycle
         !
         X_eh_setup=X_eh_setup+1
         poles(X_eh_setup)=E_eh
         Ein_poles(X_eh_setup)=E_in
         !
         i_pole=X_eh_setup
         if (allocated(ordered_grid_index)) i_pole=ordered_grid_index(X_eh_setup)
         if (iq>0) X_poles_tab(i_pole,:)=(/ik_bz,iv,ic,i_sp/)
         !
       enddo
     enddo
   enddo
 enddo
 !
 minmax_ehe=(/max(minval(poles(:X_eh_setup))-0.1_SP,0._SP),maxval(poles(:X_eh_setup))+0.1_SP/)
 !
 if (iq<0) then
   if (.not.allocated(X_poles)) then
     YAMBO_ALLOC(X_poles,(X_eh_setup))
     X_poles=0._SP
   endif
   X_poles=X_poles+poles(:X_eh_setup)
   !
   if (.not.allocated(X_Ein_poles)) then
     YAMBO_ALLOC(X_Ein_poles,(X_eh_setup))
     X_Ein_poles=0._SP
   endif
   X_Ein_poles=X_Ein_poles+Ein_poles(:X_eh_setup)
 endif
 !
 YAMBO_FREE(poles)
 YAMBO_FREE(Ein_poles)
 !
end function
!
subroutine X_weighted_bands_setup(Xen,Xk,X,iq_range,io_X_done)
 !
 ! Estimate, by conduction band, the number of X-CG groups generated over all
 ! pending q points owned by this q group.  The provisional c mask is ignored;
 ! q, k and v ownership is retained.  Bands are sampled cyclically on c and
 ! combined with a 64-bit reduction before the contiguous minimax partition.
 !
 use pars,         ONLY:SP,schlen
 use X_m,          ONLY:X_t,l_X_terminator,X_term_E,X_terminator_E,n_OPTICAL_dir_to_eval
 use R_lattice,    ONLY:bz_samp,qindx_X
 use electrons,    ONLY:levels,n_sp_pol,spin_occ
 use parallel_m,   ONLY:PAR_IND_Q_ibz,PAR_IND_Xk_bz,PAR_IND_VAL_BANDS_X,&
&                       PAR_IND_CON_BANDS_X,PAR_COM_CON_INDEX_X
 use parallel_int, ONLY:PP_redux_wait,PARALLEL_index_weighted_contiguous
 use com,          ONLY:msg
 !
#include<y_memory.h>
 !
 type(levels) :: Xen
 type(bz_samp):: Xk
 type(X_t)    :: X
 integer      :: iq_range(2),io_X_done(*)
 !
 integer :: iq,ik_bz,i_sym,ik,ik_m_q,iv,ic,i_sp,ic_min,ic_low,nmax,npoles
 integer :: Nv(n_sp_pol),Nc(n_sp_pol),Nc_XTerm(n_sp_pol),ngroups,ngroups_initial
 integer :: ndir,ir
 integer(8),allocatable :: weights(:),accepted(:)
 integer,allocatable :: sorted_x(:),group_sorted(:)
 real(SP),allocatable :: poles(:),ein(:),sorted(:)
 real(SP) :: E_eh,E_in,f_eh,local_tresh
 logical :: l_XTerm_vv
 character(schlen) :: ch
 !
 do i_sp=1,n_sp_pol
   Nv(i_sp)=Xen%nbm(i_sp)-X%ib(1)+1
   Nc(i_sp)=X%ib(2)-Xen%nbf(i_sp)
 enddo
 Nc_XTerm=0
 if (l_X_terminator) Nc_XTerm=Nv
 ic_low=minval(Xen%nbf(:n_sp_pol))+1
 if (l_X_terminator) ic_low=X%ib(1)
 nmax=Xk%nbz*sum(Nv)
 allocate(weights(X%ib(2)),accepted(X%ib(2)))
 weights=0_8
 accepted=0_8
 allocate(poles(nmax),ein(nmax),sorted(nmax),sorted_x(nmax),group_sorted(nmax))
 local_tresh=epsilon(1._SP)
 if (l_X_terminator) X_term_E=minval(Xen%E(X%ib(2),:,:))+X_terminator_E
 !
 do iq=iq_range(1),iq_range(2)
   if (io_X_done(iq)>0) cycle
   if (.not.PAR_IND_Q_ibz%element_1D(iq)) cycle
   ndir=1
   if (iq==1) then
     call X_AVERAGE_setup('Q-LOOP',iq,X%q0)
     ndir=n_OPTICAL_dir_to_eval
   endif
   do ic=ic_low,X%ib(2)
     if (mod(ic-ic_low,PAR_COM_CON_INDEX_X(X%whoami)%n_CPU)/=&
&        PAR_COM_CON_INDEX_X(X%whoami)%CPU_id) cycle
     npoles=0
     do i_sp=1,n_sp_pol
       ic_min=X%ib(2)-Nc(i_sp)+1
       if (l_X_terminator) ic_min=X%ib(1)
       if (ic<ic_min) cycle
       do ik_bz=1,Xk%nbz
         if (allocated(PAR_IND_Xk_bz%element_1D)) then
           if (.not.PAR_IND_Xk_bz%element_1D(ik_bz)) cycle
         endif
         do iv=X%ib(1),X%ib(1)+Nv(i_sp)-1
           if (allocated(PAR_IND_VAL_BANDS_X(X%whoami)%element_1D)) then
             if (.not.PAR_IND_VAL_BANDS_X(X%whoami)%element_1D(iv)) cycle
           endif
           i_sym=qindx_X(iq,ik_bz,1)
           ik=Xk%sstar(ik_bz,1)
           ik_m_q=Xk%sstar(i_sym,1)
           if (ic>=X%ib(1).and.ic<=Xen%nbm(i_sp).and.l_X_terminator) then
             l_XTerm_vv=.true.
             E_eh=X_term_E-Xen%E(iv,ik_m_q,i_sp)
           else
             l_XTerm_vv=.false.
             E_eh=Xen%E(ic,ik,i_sp)-Xen%E(iv,ik_m_q,i_sp)
           endif
           E_in=Xen%E(iv,ik_m_q,i_sp)
           if (.not.l_XTerm_vv) f_eh=Xen%f(iv,ik_m_q,i_sp)*(spin_occ-Xen%f(ic,ik,i_sp))/spin_occ
           if (     l_XTerm_vv) f_eh=Xen%f(iv,ik_m_q,i_sp)*Xen%f(ic,ik,i_sp)/spin_occ
           if (abs(f_eh)<local_tresh) cycle
           if (any((/abs(E_eh)<X%ehe(1),abs(E_eh)>X%ehe(2).and.X%ehe(2)>0._SP/))) cycle
           npoles=npoles+1
           poles(npoles)=E_eh
           ein(npoles)=E_in
         enddo
       enddo
     enddo
     accepted(ic)=accepted(ic)+int(npoles,8)*int(ndir,8)
     if (npoles>0) then
       call FREQUENCIES_group_engine(poles,npoles,X%cg_percentual,ein,l_X_terminator,&
&                                   ngroups,ngroups_initial,sorted,sorted_x,group_sorted)
       weights(ic)=weights(ic)+int(ngroups,8)*int(ndir,8)
     endif
   enddo
 enddo
 call PP_redux_wait(weights,COMM=PAR_COM_CON_INDEX_X(X%whoami)%COMM)
 call PP_redux_wait(accepted,COMM=PAR_COM_CON_INDEX_X(X%whoami)%COMM)
 call PARALLEL_index_weighted_contiguous(PAR_IND_CON_BANDS_X(X%whoami),X%ib(2),ic_low,weights,&
&                                        PAR_COM_CON_INDEX_X(X%whoami))
 !
 if (PAR_COM_CON_INDEX_X(X%whoami)%CPU_id==0) then
 do ir=1,PAR_COM_CON_INDEX_X(X%whoami)%n_CPU
   write(ch,'(a,i0,a,i0,a,i0,a,i0)') '[X-WB] c-rank ',ir-1,' bands ',&
&    PAR_IND_CON_BANDS_X(X%whoami)%first_of_1D(ir),':',&
&    PAR_IND_CON_BANDS_X(X%whoami)%last_of_1D(ir),' count ',&
&    PAR_IND_CON_BANDS_X(X%whoami)%n_of_elements(ir)
   call msg('r',trim(ch))
   if (PAR_IND_CON_BANDS_X(X%whoami)%first_of_1D(ir)>0) then
     write(ch,'(a,i0,a,i0)') '[X-WB] predicted groups ',sum(weights(&
&      PAR_IND_CON_BANDS_X(X%whoami)%first_of_1D(ir):PAR_IND_CON_BANDS_X(X%whoami)%last_of_1D(ir))),&
&      ' accepted transitions ',sum(accepted(PAR_IND_CON_BANDS_X(X%whoami)%first_of_1D(ir):&
&      PAR_IND_CON_BANDS_X(X%whoami)%last_of_1D(ir)))
     call msg('r',trim(ch))
   endif
 enddo
 endif
 deallocate(weights,accepted,poles,ein,sorted,sorted_x,group_sorted)
 !
end subroutine
