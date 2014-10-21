!
! Here there are pieces of code which were diagonalizing
! the SC_R during the termalization
!

!*********************
!**** RT_driver.F ****
!*********************

 !
 ! if l_RT_diagonalize=.true.  "en"  will contains new energies and occupations
 ! otherwise it will not be used


   ! Steps & times ...
   !===================
   ! 
   !   Thermal_steps   NE_steps
   ! --------------- 0 ---------------------------- t
   ! ^               ^                            ^
   ! |               |                            NE_i_time=NE_steps
   ! it=1            |    
   !                 it=Thermal_steps+1          
   ! NE_i_time<0     NE_i_time=1
   !
   ! 1 .... Thermal_steps (Thermal period) => NE_time=-Thermal_steps*RT_step ... -RT_step
   ! it = Thermal_steps+1                  => NE_time =0 (NE_i_time = 1)
   !
   ! at  it = Thermal_steps+1    NE_dynamics=.TRUE.
   !
   NE_i_time  =it-Thermal_steps


   !
   if (it==Thermal_steps) then
     !
     ! I freeze the last self-energy + hartree in XC0_and_HARTREE and I work
     ! only with the differences rho-rho_reference and G_lesser-G_lesser_reference
     ! 
     call RT_Collision(dG_lesser(:,:,:,it_mem),A_vecpot_previous,E,k,q,NE_time)
     XC0_and_HARTREE0=SC_R-Ho
     !
     ! Davide: Thermal steps broken.
     !         Here I should also update REF_V_xc ...
     !
   endif


   !
   ! Diagonalization
   ! ===============
   if (it==Thermal_steps.or. (mod(it,RT_diagonalization_steps)==0.and.RT_diagonalization_steps>0) ) then
     !
     do ik=1,E%nk
       call mat_dia_inv(DIAGO,SC_R(1:SC_bands(2),1:SC_bands(2),ik,1),E_real=E%E(:SC_bands(2),ik,1))
     enddo
     !
     ! Updating
     !===========
     !
     ! At the end of the thermalization update the reference G_lesser and dipoles
     if (it==Thermal_steps) call RT_update(G_lesser_reference,SC_R,it,E,k,X(1),up_Gref=.true.,up_DIP=.true.)
     !
     ! Analize quasi-particles
     if (l_RT_diagonalize) call RT_update(G_lesser(:,:,:,it_mem),SC_R,it,E,k,X(1),an_QP=.true.)
     !
   endif

 !
 if (l_RT_diagonalize) call SC_History(-1,E)

!**************************
!**** mod_real_time.F *****
!**************************
 !
 ! Time steps and step length...
 !------------------------------
 ! 
 !   Thermal_steps   NE_steps
 ! --------------- 0 ---------------------------- (end)
 !                 ^   ^
 !                 |   |
 !                 Thermal_steps+1
 !                     |__ ef1/2_i_time
 ! 
 ! NE_time is negative during the Thermal steps. When NE_time is positive
 ! l_NE_dynamics_started=.TRUE.
 !
 logical           :: l_NE_dynamics_started
 ! 
 ! Diagonalization (QP analysis)
 !
 logical           :: l_RT_diagonalize
 integer           :: RT_diagonalization_steps
 ! 
 ! ... thermalization ...
 !
 integer  :: Thermal_steps


 !
 ! Occupation natural orbitals
 !
 real(SP),    allocatable :: Occ_natural_orb(:,:) 


     if(l_RT_diagonalize) then
       allocate(Occ_natural_orb(1:SC_bands(2),QP_nk))
       call mem_est("Occ_natural",(/size(Occ_natural_orb)/))
     endif
     !
     !
     if(l_RT_diagonalize) then
       deallocate(Occ_natural_orb)
       call mem_est("Occ_natural")
     endif


!********************
!**** RT_update *****
!********************
!
subroutine RT_update(G_input,SC_R,it,Xen,k,Xk,X,up_Gref,up_DIP,an_QP)
 !
 ! This subroutine takes as input the rotation matrix SC_R,
 ! its eigenvalues Xen%E and occupations Xen%f
 ! to update G_lesser_reference,Dipoles and anlize quasi-paricles
 !
 use pars,       ONLY:SP
 use units,      ONLY:HA2EV 
 use com,        ONLY:msg
 use stderr,     ONLY:set_real_printed_length
 use SC,         ONLY:SC_bands,E_bare,H_rotate,SC_nbands
 use R_lattice,  ONLY:bz_samp
 use QP_m,       ONLY:QP_nk
 use electrons,  ONLY:levels,n_sp_pol
 use real_time,  ONLY:rho_reference
 use wrapper,    ONLY:Vstar_dot_V,M_by_V
 use X_m,        ONLY:X_t
 !
 implicit none
 !
 type(X_t),      intent(inout)        :: X
 complex(SP),    intent(inout)        :: G_input(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),QP_nk)
 complex(SP) ,   intent(in)           :: SC_R(SC_bands(2),SC_bands(2),QP_nk,n_sp_pol)
 type(bz_samp),  intent(in)           :: k,Xk
 integer,        intent(in)           :: it
 type(levels),   intent(inout)        :: Xen
 logical,        optional, intent(in) :: up_Gref,up_DIP,an_QP
 !
 ! Work Space
 !
 integer :: ik,ib
 complex(SP) :: tmp_v(SC_bands(2))
 !
 ! Update Dipoles
 !
 if(present(up_DIP)) call DIPOLE_rotate(SC_R,Xen,Xk,X)
 !
 ! Anlize quasi-particles
 ! 
 if(present(an_QP).and.(mod(it,RT_diagonalization_steps)==0.or.it==Thermal_steps)) then
   !
   call set_real_printed_length()
   !
   do ik=1,QP_nk
     !
     ! natural orbitals
     !
     do ib=1,SC_bands(1)-1
       Occ_natural_orb(ib,ik)=cmplx(2._SP,0._SP)
     enddo
     do ib=SC_bands(1),SC_bands(2)
       call M_by_V('N',SC_nbands,SC_nbands,(0._SP,-1._SP),G_input(:,:,ik),SC_nbands,SC_R(SC_bands(1):SC_bands(2),ib,ik,1), &
&                  1,(0._SP,0._SP),tmp_v,1)
       Occ_natural_orb(ib,ik)=real(Vstar_dot_V(SC_nbands,SC_R(SC_bands(1):SC_bands(2),ib,ik,1),tmp_v))
     enddo
     !
   enddo
   !
   call OCCUPATIONS_Fermi(Xen,k,0)
   Xen%E(:SC_bands(2),:,:)=Xen%E(:SC_bands(2),:,:)-Xen%E_Fermi(1)
   !
   if(it==Thermal_steps) then
     call msg('r',' ')
     call msg('r','Natural (O)ccupations, (E)nergies and (d_E) E-Eo.')
     do ik=1,QP_nk
       call REPORT_Energies(Xen%E(:SC_bands(2),:,:),k,Xk,SC_bands(2),(/ik,ik/),'E  ',.TRUE.)
       call REPORT_Energies(E_bare,k,Xk,SC_bands(2),(/ik,ik/),'E_o  ',.FALSE.)
       call REPORT_Energies(Xen%E(:SC_bands(2),:,:)-E_bare,k,Xk,SC_bands(2),(/ik,ik/),'d_E',.FALSE.)
       call REPORT_Energies(Occ_natural_orb(:SC_bands(2),:)/HA2EV,k,Xk,SC_bands(2),(/ik,ik/),'O  ',.FALSE.)
     enddo
   endif
   !
   call set_real_printed_length(f_length=15,g_length=15)
   !
   ! History
   !
   call  SC_History(it,Xen)    
   !
 endif
 !
 ! Update G_lesser and rho_reference
 !
 if(present(up_Gref)) then
   !
   G_input=(0._SP,0._SP)
   !
   do ik=1,Xen%nk
     !
     forall(ib=SC_bands(1):SC_bands(2))
       G_input(ib,ib,ik)=(0._SP,1._SP)*Xen%f(ib,ik,1)
     end forall   
     !
     call H_rotate(SC_R(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),ik,1),G_input(:,:,ik),SC_nbands,-1)
    !
  enddo
  !
  ! Update the full density and the one between SC_bands(1):SC_bands(2)
  !
  if(allocated(rho_reference)) call el_density_matrix(G_input,Xen,Xk,rho_reference,1)
  !
 endif 
 !
end subroutine RT_update


!************************
!**** RT_Collisions *****
!************************

 !
 ! Note that G_lesser_reference and rho_reference are zero during
 ! the thermalization
 !
 ! This subroutine also fills SC_R with the full Hamiltonian if this is allocated
 !

 ! l_NE_dynamics_started=.FALSE.
 ! corresponds to Thermalization where the density must be updated
 ! This part of the code is presently not used so I comment it

 !
 ! Rotation Matrix
 !=================
 if(allocated(SC_R)) then
   SC_R(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),:,:) = &
&      Ho_plus_Sigma(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),RT_ibz_coo(:,1),:)
   do i1=1,SC_bands(1)-1
     SC_R(i1,i1,:,:)=cONE
   enddo
 endif
 
