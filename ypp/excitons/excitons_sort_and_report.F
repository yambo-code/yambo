!
! License-Identifier: GPL
!
! Copyright (C) 2015 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DV
!
subroutine excitons_sort_and_report(iq,BS_R,BS_E,BS_E_SOC_corr,EXC_spin)
 !
 use pars,          ONLY:SP,schlen,pi,zero_dfl
 use units,         ONLY:HA2EV
 use electrons,     ONLY:spin_occ,spin,n_sp_pol,n_spinor
 use stderr,        ONLY:intc
 use BS,            ONLY:BSqpts
 use BS_solvers,    ONLY:BSS_desc,BSS_n_eig
 use com,           ONLY:msg,of_open_close
 use R_lattice,     ONLY:d3k_factor,q0_def_norm
 use D_lattice,     ONLY:n_atomic_species,n_atoms_species,atom_pos,Z_species
 use vec_operate,   ONLY:iku_v_norm,sort
 use YPPm,          ONLY:EXCITONS_user_indexes,EXCITON_spin
 use interfaces,    ONLY:DESC_write
 !
 implicit none 
 !
 integer                      :: iq
 complex(SP)                  :: BS_R(BSS_n_eig),BS_E(BSS_n_eig)
 real(SP), optional, pointer  :: BS_E_SOC_corr(:,:)
 type(EXCITON_spin),optional  :: EXC_spin
 !
 ! Work Space
 !
 integer            :: j1,j2,ia,is,i_mod,n_elements,i_S,i_M,S_indx(BSS_n_eig)
 real(SP)           :: Residuals(BSS_n_eig),v2sort(BSS_n_eig),Rmax,ERmax,rv(20),q_norm
 character(schlen)  :: titles(20),ch_dummy,dump_ch
 logical            :: write_widths,write_soc_corr,write_spin,E_mode,I_mode
 !
 ! Excitonc amplitude
 !
 character(3), parameter:: R_normalize="yes"
 !
 write_spin    = present(EXC_spin)
 write_widths  = any(abs(aimag(BS_E))>1.E-5)
 write_soc_corr= .false.
 if(present(BS_E_SOC_corr)) write_soc_corr=associated(BS_E_SOC_corr)
 !
 dump_ch=" "
 if (write_spin)     dump_ch=trim(dump_ch)//" Spin"
 if (write_widths)   dump_ch=trim(dump_ch)//" Im[E_lambda]"
 if (write_soc_corr) dump_ch=trim(dump_ch)//" SOC-corr"
 if (len_trim(dump_ch)>0) dump_ch="vs "//trim(dump_ch)
 !
 call section('=','Reporting sorted Energies and Strengths '//trim(dump_ch))
 !
 do i_mod=1,2
   !
   E_mode=i_mod==1
   I_mode=i_mod==2
   !
   if (E_mode) then
     ch_dummy='exc_qpt'//trim(intc(iq))//'_E_sorted'
     if (write_spin) ch_dummy='exc_qpt'//trim(intc(iq))//'_E+spin_sorted'
   else if (I_mode) then
     ch_dummy='exc_qpt'//trim(intc(iq))//'_I_sorted'
     if (write_spin) ch_dummy='exc_qpt'//trim(intc(iq))//'_I+spin_sorted'
   endif
   !
   call of_open_close(ch_dummy,'ot')
   !
   call DESC_write('o sort','#',BSS_desc)
   !
   call msg('o sort',"#")
   do is=1,n_atomic_species
     do ia=1,n_atoms_species(is)
       call msg('o sort',"# Atom "//trim(intc(ia))//&
&                        " with Z "//trim(intc(Z_species(is)))//" [cc]:",&
&               atom_pos(:,ia,is),INDENT=0)
     enddo
   enddo
   call msg('o sort','#','',INDENT=0) 
   !
   n_elements=3
   titles(1)='E [ev]'
   titles(2)='Strength'
   titles(3)='Index'
   if (write_widths) then
     n_elements=n_elements+1
     titles(4)='W [meV]'
   endif
   if (write_spin) then
     n_elements=n_elements+1
     titles(n_elements)='<S>'
     if (n_spinor==2) then
       n_elements=n_elements+1
       titles(n_elements)='<S_z>'
     endif
     do i_S=0,1
       do i_M=-i_S,i_S
         if (n_sp_pol==2.and.i_M/=0) cycle
         n_elements=n_elements+1
         titles(n_elements)='|S='//trim(intc(i_S))//' M='//trim(intc(i_M))//'>'
       enddo
     enddo
   endif
   if (write_soc_corr) then
     n_elements=n_elements+2
     titles(n_elements-1)='dE(1)[meV]'
     titles(n_elements  )='dE(2)[meV]'
   endif
   !
   Residuals(:) = real(BS_R(:)*conjg(BS_R(:)),SP)
   if (write_widths)  Residuals(:) = abs(BS_R(:))
   if(iq==1) q_norm=q0_def_norm**2
   if(iq> 1) q_norm=iku_v_norm(BSqpts(:,iq))**2
   Residuals(:) = Residuals(:)*real(spin_occ,SP)/(2._SP*pi)**3*d3k_factor*4._SP*pi/q_norm*HA2EV
   !
   if (trim(R_normalize)=='yes') then
     Rmax = maxval(Residuals)
     Residuals=Residuals/Rmax
     write(dump_ch,'(a,1e12.5)') " Maximum Residual Value = ",Rmax
   endif
   !
   if (trim(R_normalize)=='yes_deg') then
      v2sort=Residuals
      call sort(arrin=v2sort,indx=S_indx,r_zero=zero_dfl)
      Rmax=0._SP
      do j1=BSS_n_eig,1,-1
        j2=S_indx(j1)
        if(j1==BSS_n_eig) ERmax=real(BS_E(j2),SP)
        if(abs(ERmax-BS_E(j2))>1.E-5_SP) exit
        Rmax=Rmax+v2sort(j1)**2
      enddo
      Rmax=sqrt(Rmax/real(BSS_n_eig-j1,SP))
      write(dump_ch,'(a,1e12.5)') " Maximum Residual Value (summed over degenerate states) = ",Rmax
      Residuals=Residuals/Rmax
   endif
   !
   if (E_mode) v2sort=real(BS_E,SP)
   if (I_mode) v2sort=Residuals
   !
   call sort(arrin=v2sort,indx=S_indx)
   !
   call msg('o sort','#',dump_ch,USE_TABS=.TRUE.)
   call msg('o sort','#','',USE_TABS=.TRUE.)
   !
   call msg('o sort','#',titles(:n_elements),INDENT=0,USE_TABS=.true.)
   call msg('o sort','#','',INDENT=0)
   !
   do j1=BSS_n_eig,1,-1
     !
     if (E_mode) j2=S_indx(BSS_n_eig-j1+1)
     if (I_mode) j2=S_indx(j1)
     !
     rv(:3)=(/real(BS_E(j2),SP)*HA2EV,Residuals(j2),real(j2,SP)/)
     !
     n_elements=3
     if (write_widths) then
       n_elements=n_elements+1
       rv(n_elements)=abs(aimag(BS_E(j2)))*HA2EV*1000._SP
     endif
     if (write_spin) then
       if (.not.any((/EXCITONS_user_indexes==j2/))) cycle
       n_elements=n_elements+1
       rv(n_elements)=(-1.+sqrt(1.+4*EXC_spin%S_sq(j2)))/2.
       if (n_spinor==2) then
         n_elements=n_elements+1
         rv(n_elements)=EXC_spin%S_z(j2)
       endif
       do i_S=0,1
         do i_M=-i_S,i_S
           if (n_sp_pol==2.and.i_M/=0) cycle
           n_elements=n_elements+1
           rv(n_elements)=EXC_spin%N_SM(j2,i_S,i_M)
         enddo
       enddo
     endif
     if (write_soc_corr) then
       if (.not.any((/EXCITONS_user_indexes==j2/))) cycle
       n_elements=n_elements+2
       rv(n_elements-1:n_elements)=(/BS_E_SOC_corr(1,j2),BS_E_SOC_corr(2,j2)/)*HA2EV*1000._SP
     endif
     !
     call msg('o sort','',rv(:n_elements),INDENT=-2,USE_TABS=.TRUE.)
     !
   enddo
   !
   call of_open_close(ch_dummy)
   !
 enddo
 !
end subroutine
