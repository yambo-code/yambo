!
! License-Identifier: GPL
!
! Copyright (C) 2015 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
integer function io_BSS_diago(iq,i_BS_mat,ID,X_static,bsE,bsRl,bsRr,BsE_corr,&
&                             bsL_magn,bsR_magn,bsR_kerr,bsR_pl)
 !
 use pars,          ONLY:SP,schlen,IP
 use stderr,        ONLY:intc
 use BS,            ONLY:BS_K_dim,BS_H_dim,BSqpts,BS_perturbative_SOC,BS_res_ares_n_mat
 use BS_solvers,    ONLY:BSS_q0,BSS_write_eig_2_db,BSS_eh_table,BS_mat,variables_BS,BSS_n_eig,BSS_desc
 use X_m,           ONLY:X_t
 use descriptors,   ONLY:IO_desc,IO_desc_reset,IO_desc_duplicate
 use IO_m,          ONLY:io_sec,io_status,read_is_on
 use IO_int,        ONLY:io_connect,io_disconnect,io_elemental,io_bulk,io_header
 use electrons,     ONLY:n_sp_pol
 use timing_m,      ONLY:timing
 !
 implicit none
 !
 type(X_t),intent(inout)        :: X_static
 integer                        :: iq,ID,i_BS_mat
 complex(SP)        , optional  :: bsE(:)
 real(SP)   ,pointer, optional  :: BsE_corr(:,:),bsR_pl(:,:)
 complex(SP),pointer, optional  :: bsR_kerr(:),bsL_magn(:,:),bsR_magn(:,:),bsRl(:),bsRr(:)
 !
 ! Work Space
 !
 character(schlen):: db_name
 type(IO_desc)    :: local_desc
 !
 call timing('io_BSS_diago',OPR='start')
 !
 if (BS_res_ares_n_mat>1) then
   if(i_BS_mat==1) db_name='BS_diago_Q'//trim(intc(iq))
   if(i_BS_mat==2) db_name='BS_diago_Q'//trim(intc(iq))//'_ares'
 else
   db_name='BS_diago_Q'//trim(intc(iq))
 endif
 !
 io_BSS_diago=io_connect(desc=trim(db_name),type=2,ID=ID)
 if (io_BSS_diago/=0) goto 1
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   ! As this DB must respect the BS ideentifier I only need to
   ! check the K QP corrections
   !
   io_BSS_diago=io_header(ID,XC_KIND="K_E force")
   !
   io_BSS_diago=variables_BS(ID,iq,local_desc,.FALSE.,X=X_static)
   !
   call io_elemental(ID,VAR="PARS",VAR_SZ=6,MENU=0)
   !
   call io_elemental(ID,VAR="[BSS] Eigenstates included",L0=BSS_write_eig_2_db,CHECK=.true.,OP=(/"=="/))
   if(iq==1) then
     call io_elemental(ID,VAR="[BSS] Field direction",R1=BSS_q0,CHECK=.true.,OP=(/"==","==","=="/))
     if (allocated(BSqpts)) BSqpts(:,iq)=BSS_q0
   endif
   call io_elemental(ID,VAR="[BSS] Eigenstates computed     ",I0=BSS_n_eig,CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,VAR="[BSS] Number of BSE matrices   ",I0=BS_res_ares_n_mat,CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=1)
   !
   ! header return
   !
   if (io_BSS_diago/=0) goto 1
   !
   io_BSS_diago=io_status(ID)
   if (io_BSS_diago/=0) goto 1
   !
   ! Descriptions transfer
   !
   if (read_is_on(ID)) then
     call IO_desc_reset(BSS_desc)
     call IO_desc_duplicate(local_desc,BSS_desc)
   endif
   !
 endif
 !
 if (any((/io_sec(ID,:)==2/))) then
   !
   if (present(bsE)) then
     !
     call io_bulk(ID,'BS_Energies',VAR_SZ=(/2,BSS_n_eig/))
     call io_bulk(ID,C1=bsE)
     !
   endif
   !
   if (present(bsE_corr).and.BS_perturbative_SOC) then
     if (associated(bsE_corr)) then
       call io_bulk(ID,'BS_Energies_SOC_corr',VAR_SZ=(/2/n_sp_pol,BSS_n_eig/))
       call io_bulk(ID,R2=BsE_corr)
     endif
   endif
   !
   if (present(bsRl)) then
     call io_bulk(ID,'BS_left_Residuals',VAR_SZ=(/2,BSS_n_eig/))
     call io_bulk(ID,C1=bsRl)
   endif
   !
   if (present(bsRr)) then
     call io_bulk(ID,'BS_right_Residuals',VAR_SZ=(/2,BSS_n_eig/))
     call io_bulk(ID,C1=bsRr)
   endif
   !
   if(present(bsL_magn)) then
     if(associated(bsL_magn)) then
       call io_bulk(ID,'BS_L_magn_Residuals',VAR_SZ=(/2,2,BSS_n_eig/))
       call io_bulk(ID,C2=bsL_magn)
     endif
   endif
   !
   if(present(bsR_magn)) then
     if(associated(bsR_magn)) then
       call io_bulk(ID,'BS_R_magn_Residuals',VAR_SZ=(/2,2,BSS_n_eig/))
       call io_bulk(ID,C2=bsR_magn)
     endif
   endif
   !
   if(present(bsR_kerr)) then
     if(associated(bsR_kerr)) then
       call io_bulk(ID,'BS_KERR_Residuals',VAR_SZ=(/2,BSS_n_eig/))
       call io_bulk(ID,C1=bsR_kerr)
     endif
   endif
   !
   if(present(bsR_pl)) then
     if(associated(bsR_pl)) then
       call io_bulk(ID,'BS_PL_Residuals',VAR_SZ=(/2,BSS_n_eig/))
       call io_bulk(ID,R2=bsR_pl)
     endif
   endif
   !
 endif
 !
 if (any((/io_sec(ID,:)==3/))) then
   ! 
   ! If not switched on skip this section
   !
   if (.not.BSS_write_eig_2_db) then
     io_BSS_diago=-1
     goto 1
   endif
   !
   ! BS_eh_table
   !
   call io_bulk(ID,'BS_TABLE',VAR_SZ=(/BS_K_dim(1),5/))
   call io_bulk(ID,I2=BSS_eh_table(:BS_K_dim(1),:))
   !
   ! Eigenstates (resonant only matrix)
   !
   call io_bulk(ID,'BS_EIGENSTATES',VAR_SZ=(/2,BS_H_dim,BSS_n_eig/))
   call io_bulk(ID,C2=BS_mat,IPOS=(/1,1,1/))
   !
 endif
 !
1 call io_disconnect(ID)
 !
 call timing('io_BSS_diago',OPR='stop')
 !
end function
