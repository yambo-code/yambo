!
! License-Identifier: GPL
!
! Copyright (C) 2025 The Yambo Team
!
! Authors (see AUTHORS file for details): DS
!
subroutine RT_gradk_rho_times_E_wrong(ik,i_sp_pol,H_nl_sc,dG_in,A_input,Xk,Xen)
 !
 ! This subroutine computes the k-gradient of the density matrix,
 ! and multiplies it by the electric field.
 ! See Phys. Rev. B 76, 035213 (2007)
 !
 ! To guage the density matrix it uses the phases matrixi of the dipoles
 ! defined in Phys. Rev. Lett. 131, 236902 (2023)
 ! In doing so we move into a gauge where the dipoles projected along E are real.
 ! Not sure if such gauge really makes sense ...
 !
 use pars,           ONLY:SP,cI,cZERO,cONE,pi
 use units,          ONLY:SPEED_OF_LIGHT,HA2EV
 use real_time,      ONLY:l_length_grad_k,RT_bands
 use fields,         ONLY:gauge_field
 use real_time,      ONLY:RT_bands,NE_i_time,NE_i_last_field
 use vec_operate,    ONLY:k_periodic_idx,degeneration_finder
 use matrix_operate, ONLY:hermitian
 use DIPOLES,        ONLY:DIP_S,DIP_iR,l_force_SndOrd
 use electrons,      ONLY:levels,n_sp_pol
 use R_lattice,      ONLY:bz_samp,k_map
 use D_lattice,      ONLY:a
 use parallel_m,     ONLY:PAR_G_k_range
 use parser_m,       ONLY:parser
 !
 implicit none
 !
 integer,           intent(in) :: ik,i_sp_pol
 complex(SP),    intent(inout) :: H_nl_sc(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 complex(SP),       intent(in) :: dG_in(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),PAR_G_k_range(1):PAR_G_k_range(2),n_sp_pol)
 type(gauge_field), intent(in) :: A_input
 type(bz_samp),     intent(in) :: Xk
 type(levels),      intent(in) :: Xen
 !
 ! Workspace
 !
 logical     :: l_dips_term
 integer     :: id_red,max_step,istep,idx(3),idir,ib1,jb1,nb(2),&
                ikm1,ikp1,ikbz,ikm1_ibz,ikm1_is,ikp1_ibz,ikp1_is
 real(SP)    :: q_fac,E_field(3)
 complex(SP) :: DIP_kp_c(3),DIP_km_c(3),DIP_k_c(3)
 real(SP)    :: DIP_kp_mod,DIP_km_mod,DIP_k_mod
 !
 complex(SP), allocatable :: DIP_kp(:,:),DIP_km(:,:)
 !
 complex(SP) :: gradk_rho(RT_Bands(1):RT_bands(2),RT_bands(1):RT_bands(2),3),&
 &          gradk_rho_red(RT_Bands(1):RT_bands(2),RT_Bands(1):RT_bands(2),2)
 !
 ! Warning: kpts parallelization to be fixed, since rho is distributed
 !
 if (.not.l_length_grad_k) return
 !
 ! Length gauge
 !===============
 E_field=-A_input%vecpot_vel/SPEED_OF_LIGHT ! Gaussian units
 !
 if (NE_i_time>NE_i_last_field) return
 !
 nb=RT_bands
 call parser("RTgradkDips",l_dips_term)
 !
 allocate(DIP_kp(nb(1):nb(2),nb(1):nb(2)))
 allocate(DIP_km(nb(1):nb(2),nb(1):nb(2)))
 !
 ikbz=Xk%k_table(ik,1)
 !
 gradk_rho=cZERO
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
   gradk_rho_red=cZERO
   !
   do istep=1,max_step 
     !
     idx=k_map%k_map_inv(ikbz,:)
     idx(id_red)=idx(id_red)-istep
     idx=k_periodic_idx(idx,k_map)
     ikm1=k_map%k_map_dir(idx(1),idx(2),idx(3))
     !
     ikm1_ibz=Xk%sstar(ikm1,1)
     ikm1_is =Xk%sstar(ikm1,2)
     !
     idx=k_map%k_map_inv(ikbz,:)
     idx(id_red)=idx(id_red)+istep
     idx=k_periodic_idx(idx,k_map)
     ikp1=k_map%k_map_dir(idx(1),idx(2),idx(3))
     !
     ikp1_ibz=Xk%sstar(ikp1,1)
     ikp1_is =Xk%sstar(ikp1,2)
     !
     if (l_dips_term) then
     DIP_km=cZERO
     DIP_kp=cZERO
     do ib1=RT_bands(1),RT_bands(2)
       do jb1=RT_bands(1),RT_bands(2)
         call DIPOLE_rotate(jb1,ib1,ikm1,i_sp_pol,"DIP_iR",Xk,DIP_km_c)
         call DIPOLE_rotate(jb1,ib1,ikp1,i_sp_pol,"DIP_iR",Xk,DIP_kp_c)
         ! DEBUG lines to verify the time gain when non-expanding on the fly
         !DIP_km_c=DIP_iR(:,ib1,jb1,ikm1_ibz,i_sp_pol)
         !DIP_kp_c=DIP_iR(:,ib1,jb1,ikp1_ibz,i_sp_pol)
         ! As always in yambo, band indexes are inverted in the dipoles
         DIP_km_mod=sqrt(abs(DIP_km_c(1))**2+abs(DIP_km_c(2))**2+abs(DIP_km_c(3))**2)
         DIP_kp_mod=sqrt(abs(DIP_kp_c(1))**2+abs(DIP_kp_c(2))**2+abs(DIP_kp_c(3))**2)
         do idir=1,3
           DIP_km(ib1,jb1)=DIP_km(ib1,jb1)-cI*DIP_km_c(idir)*a(id_red,idir)/DIP_km_mod
           DIP_kp(ib1,jb1)=DIP_kp(ib1,jb1)-cI*DIP_kp_c(idir)*a(id_red,idir)/DIP_kp_mod
         enddo
       enddo
     enddo
     else
       DIP_km=cONE
       DIP_kp=cONE
     endif
     !
     ! 1/q factor = k_map%max_kdir(id_red)/(4._SP*pi*real(istep,SP))
     q_fac=k_map%max_kdir(id_red)/(4._SP*pi*real(istep,SP))
     !
     do ib1=RT_bands(1),RT_bands(2)
       do jb1=RT_bands(1),RT_bands(2)
         gradk_rho_red(ib1,jb1,istep)=gradk_rho_red(ib1,jb1,istep)+  &
             &  -cI*(dG_in(ib1,jb1,ikp1_ibz,i_sp_pol)*conjg(DIP_km(ib1,jb1))+ &
             &      -dG_in(ib1,jb1,ikm1_ibz,i_sp_pol)*conjg(DIP_kp(ib1,jb1))  )
       enddo
     enddo
     !
     gradk_rho_red(:,:,istep)=gradk_rho_red(:,:,istep)*q_fac/2._SP
     !
   enddo ! istep
   !
   ! Rotate in cartesian coordinates
   !
   if(max_step==1) then
     !      
     ! First order formula D(dk) 
     !
     do idir=1,3
       gradk_rho(:,:,idir)=gradk_rho(:,:,idir)+transpose(gradk_rho_red(:,:,1))*a(id_red,idir)
     enddo
     !
   else
     !      
     ! Second order formula   ( 4*D(dk) - D(2*dk) ) / 3
     !
     do idir=1,3
       gradk_rho(:,:,idir)=gradk_rho(:,:,idir)+transpose(4._SP*gradk_rho_red(:,:,1)-gradk_rho_red(:,:,2))/3._SP*a(id_red,idir)
     enddo
     !
   endif
   !
 enddo   ! id_red
 !
 l_dips_term=.false.
 !
 do ib1=RT_bands(1),RT_bands(2)
   do jb1=RT_bands(1),RT_bands(2)
     if (l_dips_term) then
     call DIPOLE_rotate(jb1,ib1,ikbz,i_sp_pol,"DIP_iR",Xk,DIP_k_c)
     ! DEBUG lines to verify the time gain when non-expanding on the fly
     !DIP_km_c=DIP_iR(:,ib1,jb1,ikm1_ibz,i_sp_pol)
     ! As always in yambo, band indexes are inverted in the dipoles
     DIP_k_mod =sqrt(abs(DIP_k_c(1))**2+abs(DIP_k_c(2))**2+abs(DIP_k_c(3))**2)
     DIP_k_c=-cI*DIP_kp_c/DIP_k_mod
     else
       DIP_k_c=cONE
     endif
     do idir=1,3
       H_nl_sc(ib1,jb1)=H_nl_sc(ib1,jb1)-gradk_rho(ib1,jb1,idir)*E_field(idir)*DIP_k_c(idir)
     enddo
   enddo
 enddo
 !
 !
end subroutine RT_gradk_rho_times_E_wrong
!
!
subroutine RT_gradk_rho_times_E(ik,i_sp_pol,H_nl_sc,dG_in,A_input,Xk,Xen)
 !
 ! This subroutine computes the k-gradient of the density matrix,
 ! and multiplies it by the electric field.
 ! See Phys. Rev. B 76, 035213 (2007)
 !
 use pars,           ONLY:SP,cI,cZERO,pi
 use units,          ONLY:SPEED_OF_LIGHT,HA2EV
 use real_time,      ONLY:l_length_grad_k,RT_bands
 use fields,         ONLY:gauge_field
 use real_time,      ONLY:RT_bands,NE_i_time,NE_i_last_field
 use vec_operate,    ONLY:k_periodic_idx,degeneration_finder
 use matrix_operate, ONLY:hermitian
 use DIPOLES,        ONLY:DIP_S,DIP_iR,l_force_SndOrd
 use electrons,      ONLY:levels,n_sp_pol
 use R_lattice,      ONLY:bz_samp,k_map
 use D_lattice,      ONLY:a
 use parallel_m,     ONLY:PAR_G_k_range
 use parser_m,       ONLY:parser
 !
 implicit none
 !
 integer,           intent(in) :: ik,i_sp_pol
 complex(SP),    intent(inout) :: H_nl_sc(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 complex(SP),       intent(in) :: dG_in(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),PAR_G_k_range(1):PAR_G_k_range(2),n_sp_pol)
 type(gauge_field), intent(in) :: A_input
 type(bz_samp),     intent(in) :: Xk
 type(levels),      intent(in) :: Xen
 !
 ! Workspace
 !
 logical     :: l_dips_term,l_svd
 integer     :: id_red,max_step,istep,idx(3),idir,ib1,ib2,jb1,jb2,nb(2),&
                ikm1,ikp1,ikbz,ikm1_ibz,ikm1_is,ikp1_ibz,ikp1_is
 real(SP)    :: q_fac,E_field(3)
 complex(SP) :: DIP_kp_c(3),DIP_km_c(3)
 integer     :: ib,ibp,i_grp,first_el_k(RT_bands(2)-RT_bands(1)+1),n_of_el_k(RT_bands(2)-RT_bands(1)+1),n_deg_grp_k
 logical     :: deg_table_k(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 real(SP)    :: deg_thresh
 !
 !complex(SP) :: Mp_kmq(RT_bands(2),RT_bands(2),RT_bands(2),RT_bands(2)),&
 !&              Mm_kpq(RT_bands(2),RT_bands(2),RT_bands(2),RT_bands(2)),&
 !&              Sp_kmq(RT_bands(2),RT_bands(2)),Sm_kpq(RT_bands(2),RT_bands(2)),&
 !&              A1p_kmq(RT_bands(2),RT_bands(2)),A1m_kpq(RT_bands(2),RT_bands(2)),&
 !&              A2p_kmq(RT_bands(2),RT_bands(2)),A2m_kpq(RT_bands(2),RT_bands(2)),&
 !&              DIP_kp(RT_bands(2),RT_bands(2)),DIP_km(RT_bands(2),RT_bands(2))
 !
 complex(SP), allocatable :: Mp_kmq(:,:,:,:),Mm_kpq(:,:,:,:),&
 &              Sp_kmq(:,:),Sm_kpq(:,:),gp_kmq(:,:),gm_kpq(:,:),&
 &              A1p_kmq(:,:),A1m_kpq(:,:),A2p_kmq(:,:),A2m_kpq(:,:),&
 &              DIP_kp(:,:),DIP_km(:,:)
 !
 complex(SP) :: gradk_rho(RT_Bands(1):RT_bands(2),RT_bands(1):RT_bands(2),3),&
 &          gradk_rho_red(RT_Bands(1):RT_bands(2),RT_Bands(1):RT_bands(2),2)
 !
 ! Warning: kpts parallelization to be fixed, since rho is distributed
 !
 if (.not.l_length_grad_k) return
 !
 ! Length gauge
 !===============
 E_field=-A_input%vecpot_vel/SPEED_OF_LIGHT ! Gaussian units
 !
 if (NE_i_time>NE_i_last_field) return
 !
 call parser("RTgradkDips",l_dips_term)
 l_svd=.true.
 deg_thresh=1.E-5/HA2EV
 !
 nb=RT_bands
 !
 allocate(Mp_kmq(nb(1):nb(2),nb(1):nb(2),nb(1):nb(2),nb(1):nb(2)))
 allocate(Mm_kpq(nb(1):nb(2),nb(1):nb(2),nb(1):nb(2),nb(1):nb(2)))
 !
 allocate(Sp_kmq(nb(1):nb(2),nb(1):nb(2)))
 allocate(Sm_kpq(nb(1):nb(2),nb(1):nb(2)))
 !
 allocate(gp_kmq(nb(1):nb(2),nb(1):nb(2)))
 allocate(gm_kpq(nb(1):nb(2),nb(1):nb(2)))
 !
 allocate(A1p_kmq(nb(1):nb(2),nb(1):nb(2)))
 allocate(A1m_kpq(nb(1):nb(2),nb(1):nb(2)))
 !
 allocate(A2p_kmq(nb(1):nb(2),nb(1):nb(2)))
 allocate(A2m_kpq(nb(1):nb(2),nb(1):nb(2)))

 allocate(DIP_kp(nb(1):nb(2),nb(1):nb(2)))
 allocate(DIP_km(nb(1):nb(2),nb(1):nb(2)))
 !
 ikbz=Xk%k_table(ik,1)
 !
 ! define degeneration table at ik
 !
 call degeneration_finder(Xen%E(nb(1):nb(2),ik,i_sp_pol),nb(2)-nb(1)+1,&
 &                           first_el_k,n_of_el_k,n_deg_grp_k,deg_thresh,Include_single_values=.true.)
 !
 deg_table_k=.false.
 do i_grp=1,n_deg_grp_k
   do ib=nb(1)+first_el_k(i_grp)-1,nb(1)+first_el_k(i_grp)+n_of_el_k(i_grp)-2
     do ibp=nb(1)+first_el_k(i_grp)-1,nb(1)+first_el_k(i_grp)+n_of_el_k(i_grp)-2
       deg_table_k(ib,ibp)=.true.
       deg_table_k(ibp,ib)=.true.
     enddo
   enddo
 enddo
 !
 gradk_rho=cZERO
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
   gradk_rho_red=cZERO
   !
   do istep=1,max_step 
     !
     idx=k_map%k_map_inv(ikbz,:)
     idx(id_red)=idx(id_red)-istep
     idx=k_periodic_idx(idx,k_map)
     ikm1=k_map%k_map_dir(idx(1),idx(2),idx(3))
     !
     ikm1_ibz=Xk%sstar(ikm1,1)
     ikm1_is =Xk%sstar(ikm1,2)
     !
     idx=k_map%k_map_inv(ikbz,:)
     idx(id_red)=idx(id_red)+istep
     idx=k_periodic_idx(idx,k_map)
     ikp1=k_map%k_map_dir(idx(1),idx(2),idx(3))
     !
     ikp1_ibz=Xk%sstar(ikp1,1)
     ikp1_is =Xk%sstar(ikp1,2)
     !
     Sm_kpq(:,:)=hermitian(DIP_S(nb(1):,nb(1):,id_red+(istep-1)*3,ikbz,i_sp_pol))   ! (g-(k+q))= g+(k)\dag
     Sp_kmq(:,:)=          DIP_S(nb(1):,nb(1):,id_red+(istep-1)*3,ikm1,i_sp_pol)    !  g+(k-q) =(g-(k)\dag)
     !
     ! k+q part
     ! Disconnect non connected blocks
     call zeroing_disconnected_blocks(Xen,Xk,RT_bands,ikp1,i_sp_pol,&
     &                         deg_table_k,deg_thresh,Sm_kpq,gm_kpq)
     ! Perform SVD
     call SERIAL_SVD(nb(2)-nb(1)+1,Sm_kpq,'uni',0)
     !
     ! k-q part
     ! Disconnect non connected blocks
     call zeroing_disconnected_blocks(Xen,Xk,RT_Bands,ikm1,i_sp_pol,&
     &                         deg_table_k,deg_thresh,Sp_kmq,gp_kmq)
     ! Perform SVD
     call SERIAL_SVD(nb(2)-nb(1)+1,Sp_kmq,'uni',0)
     !
     ! define M matrix
     do ib1=RT_bands(1),RT_bands(2)
       do ib2=RT_bands(1),RT_bands(2)
         Mp_kmq(ib1,ib2,:,:)=conjg(gp_kmq(ib1,ib2))*gp_kmq(:,:)
         Mm_kpq(ib1,ib2,:,:)=conjg(gm_kpq(ib1,ib2))*gm_kpq(:,:)
       enddo
     enddo
     !
     if (l_dips_term) then
       !
       ! I need to rotate the dipoles in k-space,
       ! and then project them along the id_red direction
       ! Note: this term is quitre slow.
       ! Pre-expanding the dipoles with the old
       ! DIPOLE expand wuold speed-up this part
       !
       DIP_km=cZERO
       DIP_kp=cZERO
       do ib1=RT_bands(1),RT_bands(2)
         do jb1=RT_bands(1),RT_bands(2)
           call DIPOLE_rotate(jb1,ib1,ikm1,i_sp_pol,"DIP_iR",Xk,DIP_km_c)
           call DIPOLE_rotate(jb1,ib1,ikp1,i_sp_pol,"DIP_iR",Xk,DIP_kp_c)
           ! DEBUG lines to verify the time gain when non-expanding on the fly
           !DIP_km_c=DIP_iR(:,ib1,jb1,ikm1_ibz,i_sp_pol)
           !DIP_kp_c=DIP_iR(:,ib1,jb1,ikp1_ibz,i_sp_pol)
           ! As always in yambo, band indexes are inverted in the dipoles
           do idir=1,3
             DIP_km(ib1,jb1)=DIP_km(ib1,jb1)-cI*DIP_km_c(idir)*a(id_red,idir)
             DIP_kp(ib1,jb1)=DIP_kp(ib1,jb1)-cI*DIP_kp_c(idir)*a(id_red,idir)
           enddo
         enddo
       enddo
       !
       !
       do ib1=RT_bands(1),RT_bands(2)
         do jb1=RT_bands(1),RT_bands(2)
           do ib2=RT_bands(1),RT_bands(2)
             !A1_ll' ib1=l jb1=l' ib2=s
             A1p_kmq(ib1,jb1)=-cI*dG_in(ib2,jb1,ikm1_ibz,i_sp_pol)*conjg(DIP_km(ib2,ib1))
             A1m_kpq(ib1,jb1)=-cI*dG_in(ib2,jb1,ikp1_ibz,i_sp_pol)*conjg(DIP_kp(ib2,ib1))
             !A2_ll' ib1=l jb1=l' ib2=s
             A2p_kmq(ib1,jb1)=-cI*dG_in(ib1,ib2,ikm1_ibz,i_sp_pol)*      DIP_km(ib2,jb1)
             A2m_kpq(ib1,jb1)=-cI*dG_in(ib1,ib2,ikp1_ibz,i_sp_pol)*      DIP_kp(ib2,jb1)
           enddo
         enddo
       enddo
       !
     else
       !
       A1p_kmq=cZERO
       A1m_kpq=cZERO
       A2p_kmq=cZERO
       A2m_kpq=cZERO
       !
     endif
     !
     ! 1/q factor = k_map%max_kdir(id_red)/(4._SP*pi*real(istep,SP))
     q_fac=k_map%max_kdir(id_red)/(4._SP*pi*real(istep,SP))
     !
     do ib1=RT_bands(1),RT_bands(2)
       do jb1=RT_bands(1),RT_bands(2)
         do ib2=RT_bands(1),RT_bands(2)
           do jb2=RT_bands(1),RT_bands(2)
             ! ib1=n, jb1=m, ib2=l, jb2=l'
             gradk_rho_red(ib1,jb1,istep)=gradk_rho_red(ib1,jb1,istep)+ &
             &  -cI*Mm_kpq(ib2,ib1,jb2,jb1)*dG_in(ib2,jb2,ikp1_ibz,i_sp_pol)+    &
             &  +cI*Mp_kmq(ib2,ib1,jb2,jb1)*dG_in(ib2,jb2,ikm1_ibz,i_sp_pol)+    &
             &  -cI/q_fac*Mm_kpq(ib2,ib1,jb2,jb1)*(A1m_kpq(ib2,jb2)-A2m_kpq(ib2,jb2))+ &
             &  -cI/q_fac*Mp_kmq(ib2,ib1,jb2,jb1)*(A1p_kmq(ib2,jb2)-A2p_kmq(ib2,jb2))
           enddo
         enddo
       enddo
     enddo
     !
     gradk_rho_red(:,:,istep)=gradk_rho_red(:,:,istep)*q_fac/2._SP
     !
   enddo ! istep
   !
   ! Rotate in cartesian coordinates
   !
   if(max_step==1) then
     !      
     ! First order formula D(dk)      
     !
     do idir=1,3
       gradk_rho(:,:,idir)=gradk_rho(:,:,idir)+transpose(gradk_rho_red(:,:,1))*a(id_red,idir)
     enddo
     !
   else
     !      
     ! Second order formula   ( 4*D(dk) - D(2*dk) ) / 3
     !
     do idir=1,3
       gradk_rho(:,:,idir)=gradk_rho(:,:,idir)+transpose(4._SP*gradk_rho_red(:,:,1)-gradk_rho_red(:,:,2))/3._SP*a(id_red,idir)
     enddo
     !
   endif
   !
 enddo   ! id_red
 !
 do idir=1,3
   H_nl_sc(:,:)=H_nl_sc(:,:)-gradk_rho(:,:,idir)*E_field(idir)
 enddo
 !
end subroutine RT_gradk_rho_times_E
