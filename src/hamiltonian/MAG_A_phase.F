!
! License-Identifier: GPL
!
! Copyright (C) 2010 The Yambo Team
!
! Authors (see AUTHORS file for details): DS
!
subroutine MAG_A_phase()
 !
 use pars,           ONLY:SP
 use units,          ONLY:SPEED_OF_LIGHT
 use D_lattice,      ONLY:x_cc
 use com,            ONLY:msg
 use stderr,         ONLY:intc
 use magnetic,       ONLY:Bz,MAG_radius,n_loops
 use wave_func,      ONLY:WF
 use FFT_m,          ONLY:fft_size
 use interfaces,     ONLY:WF_load,WF_free
 use wrapper,        ONLY:Vstar_dot_V
 !
 implicit none
 !
 real(SP)    :: r2_ref,r2_cyl(fft_size)
 complex(SP) :: wf_r2(fft_size)
 integer     :: ib_ref,ik_ref,is_ref,iwf
 !
 n_loops=0
 r2_cyl(:)=x_cc(:,1,1)**2+x_cc(:,2,1)**2
 !
 if(MAG_radius>1.E-5) then
   n_loops=int(MAG_radius**2*Bz/(2*SPEED_OF_LIGHT)+0.5)
   if(n_loops==0) call msg('rs','[MAG] Phase trick: check done, not needed')
   if(n_loops> 0) call msg('rs','[MAG] Applying phase trick. Nloops='//trim(intc(n_loops))//' ')
   return
 endif 
 !
 ik_ref=1
 ib_ref=1
 is_ref=1
 !
 call WF_load(WF,0,1,(/ib_ref,ib_ref/),(/ik_ref,ik_ref/),(/is_ref,is_ref/),title='-MAG_GAUGE',impose_free_and_alloc=.TRUE.)
 !
 iwf=WF%index(ib_ref,ik_ref,is_ref)
 wf_r2(:)=r2_cyl(:)*WF%c(:,is_ref,iwf)
 r2_ref=real(Vstar_dot_V(fft_size,WF%c(:,is_ref,iwf),wf_r2))
 !
 n_loops=int(r2_ref*Bz/(2*SPEED_OF_LIGHT)+0.5)
 !
 call WF_free(WF)
 !
 if(n_loops==0) call msg('rs','[MAG] Phase trick: check done, not needed')
 if(n_loops> 0) call msg('rs','[MAG] Applying phase trick. Nloops='//trim(intc(n_loops))//' ')
 !
end subroutine
