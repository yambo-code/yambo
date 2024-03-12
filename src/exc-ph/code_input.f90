 ! code taken from branch phys-excitons
 ! file ypp/excitons/exciton_symmetries.f90

 use wave_func,      ONLY:io_WF_phases,WF_phases_b_map,WF_phases
 !
 ! Electrons part (pre-requisite)
 !
 call k_build_up_BZ_tables(Xk)
 call k_ibz2bz(Xk,'iku',.true.)
 call k_small_group(Xk)
 !
 call WF_phase_matrices(Xen,Xk,BS_bands,(/1,Xk%nibz/))
 !
 YAMBO_ALLOC(WF_phases_b_map,(BS_bands(1):BS_bands(2),Xk%nibz,n_sp_pol,2))
 !
 call io_control(ACTION=OP_RD,COM=REP,SEC=(/1,2/),MODE=DUMP,ID=ID)
 io_err=io_WF_phases(BS_bands,0,0,0,ID,(/0,0,0,0,0/))
 !
 call parser("MinimizeMem",l_min_mem)
 !
 if (.not.l_min_mem) then
   nsz(1:2)=maxval(WF_phases_b_map(:,:,:,1))
   nsz(3)  =maxval(WF_phases_b_map(:,:,:,2))
   nsz(4)  =nsym
   nsz(5)  =Xk%nbz
   YAMBO_ALLOC(WF_phases,(nsz(1),nsz(2),nsz(3),nsz(4),nsz(5)))
   !
   do i_sp_pol=1,n_sp_pol
     do ik=1,Xk%nibz
       nsz(1:2)=maxval(WF_phases_b_map(:,ik,i_sp_pol,1))
       nsz(3)  =maxval(WF_phases_b_map(:,ik,i_sp_pol,2))
       nsz(4)  =nsym
       nsz(5)  =Xk%nstar(ik)
       il1=1
       if(ik>0) il1= sum(Xk%nstar(:ik-1))+1
       call io_control(ACTION=RD,COM=REP,SEC=(/3/),MODE=DUMP,ID=ID)
       io_err=io_WF_phases( BS_bands,ik,0,i_sp_pol,ID,nsz, &
       & WF_phases(:nsz(1),:nsz(2),:nsz(3),:,il1:il1+nsz(5)-1) )
     enddo
   enddo
 endif
