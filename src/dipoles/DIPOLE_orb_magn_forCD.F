!
! License-Identifier: GPL
!
! Copyright (C) 2019 The Yambo Team
!
! Authors (see AUTHORS file for details): DS
!
subroutine Dipole_orb_magn_forCD(Xen,Xk,Dip)
 !
 ! This routine returns <L>
 !
 use pars,                ONLY:SP,cI
 use LIVE_t,              ONLY:live_timing
 use electrons,           ONLY:levels,n_sp_pol
 use parallel_m,          ONLY:PAR_IND_DIPk_ibz,PAR_DIPk_ibz_index
 use vec_operate,         ONLY:cross_product
 use R_lattice,           ONLY:bz_samp
 use DIPOLES,             ONLY:DIP_iR,DIP_orbital,DIPOLE_t,DIP_v
 use parallel_m,          ONLY:PAR_IND_DIPk_ibz,PAR_DIPk_ibz_index,PAR_IND_DIPk_ibz_ID,&
&                              PAR_IND_VAL_BANDS_DIP,PAR_IND_VAL_BANDS_DIP_ID,&
&                              PAR_IND_CON_BANDS_DIP,PAR_IND_CON_BANDS_DIP_ID
 !
 implicit none
 !
 type(levels),  intent(in)    :: Xen
 type(bz_samp), intent(in)    :: Xk
 type(DIPOLE_t),intent(inout) :: Dip
 !
 ! Work Space
 !
 integer                      :: ik,ic,iv,ib,i_sp_pol,n_LT_steps,ik_mem
 complex(SP)                  :: DIP_x_loc(3),DIP_v_loc(3)
 !
 n_LT_steps=n_sp_pol*PAR_IND_DIPk_ibz%n_of_elements(PAR_IND_DIPk_ibz_ID+1)*&
&                    PAR_IND_VAL_BANDS_DIP%n_of_elements(PAR_IND_VAL_BANDS_DIP_ID+1)*&
&                    PAR_IND_CON_BANDS_DIP%n_of_elements(PAR_IND_CON_BANDS_DIP_ID+1)
 !
 if (n_LT_steps>0) call live_timing('Dipoles: orbital magnetization',n_LT_steps)
 !
 ! Main loop over k in IBZ
 !
 do i_sp_pol=1,n_sp_pol
   !
   if (n_LT_steps==0) cycle
   ! 
   do ik=1,Xk%nibz
     !
     if (.not.PAR_IND_DIPk_ibz%element_1D(ik)) cycle 
     !
     ik_mem=PAR_DIPk_ibz_index(ik)
     ! 
     do iv=Dip%ib(1),Dip%ib_lim(1) 
       !
       if(.not.PAR_IND_VAL_BANDS_DIP%element_1D(iv)) cycle
       !
       do ic=max(iv,Dip%ib_lim(2)),Dip%ib(2)
         !
         if(.not.PAR_IND_CON_BANDS_DIP%element_1D(ic)) cycle
         !
         do ib=Dip%ib(1),Dip%ib(2)
           !
           DIP_x_loc(:)=-cI*DIP_iR(:,ib,iv,ik_mem,i_sp_pol)
           
           DIP_v_loc(:)=DIP_v(:,ic,ib,ik_mem,i_sp_pol)
           !
           DIP_orbital(:,ic,iv,ik_mem,i_sp_pol,1)=DIP_orbital(:,ic,iv,ik_mem,i_sp_pol,1)+ &
           & cross_product(DIP_x_loc,DIP_v_loc)
           !
           ! DEBUG <
           !write(*,*) "[DIP] writing DIP_orbital(..,ic,iv,ik,,) values"
           ! DEBUG >
           !
         enddo
         !
         call live_timing(steps=1)
         !
       enddo    ! conduction band loop
     enddo      ! valence    band loop
     ! 
   enddo        ! sp_pol loop
   !
 enddo          ! k-points
 !
 call live_timing()
 !
end subroutine Dipole_orb_magn_forCD
