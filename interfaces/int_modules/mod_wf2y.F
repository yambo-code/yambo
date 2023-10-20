!
! License-Identifier: GPL
!
! Copyright (C) 2008 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
module mod_wf2y
 ! 
 implicit none
 !
 contains
   !
   subroutine WF_splitter()
     !
     use pars,           ONLY:SP
     use mod_com2y,      ONLY:wf_nb_io_user
     use units,          ONLY:Megabyte
     use com,            ONLY:msg
     use wave_func,      ONLY:wf_nb_io,wf_nb_io_groups,wf_ncx
     use electrons,      ONLY:n_bands,n_spin
     !
     ! Work Space
     !
     real(SP),parameter :: max_wf_block_size=1000._SP ! MB
     real(SP) :: array_sz
     !
     wf_nb_io_groups=1
     wf_nb_io=n_bands
     if (wf_nb_io_user>0) then
       !
       wf_nb_io=wf_nb_io_user
       wf_nb_io_groups=ceiling(real(n_bands,SP)/real(wf_nb_io,SP))
       !
     else
       !
       array_sz = real(2*n_bands*wf_ncx*n_spin*SP,SP)/Megabyte
       !
       ! No memory problems: no blocks
       ! 
       if (array_sz<=max_wf_block_size) return
       !
       wf_nb_io_groups = ceiling(array_sz/max_wf_block_size)
       wf_nb_io        = ceiling(real(n_bands,SP)/wf_nb_io_groups)
       !
       if(wf_nb_io_groups .gt. n_bands) then
         wf_nb_io_groups = n_bands 
         wf_nb_io = 1
       endif
       !
     endif
     !
     array_sz = real(2*wf_nb_io*wf_ncx*n_spin*SP,SP)/Megabyte
     !
     call msg('s',' WF splitter Blocks/bands/block size(Mb)',(/real(wf_nb_io_groups,SP),real(wf_nb_io,SP),array_sz/))
     !
   end subroutine
   !
   integer function make_real_wf(ib,ik,wf_,nc)
     !
     use pars,      ONLY:SP
     use stderr,    ONLY:intc
     integer     :: ib,ik,nc
     real (SP)   :: wf_(2,nc)
     !
     ! Work Space
     !
     integer     :: ic,ic_start,ic_check
     real(SP)    :: c1,c2,c_norm_sq,ph_sin(nc),ph_cos(nc),ph_cos_sq(nc)
     complex(SP) :: phase,wf_c
     ic_start=-1
     ic_check=0
     make_real_wf=1
     do ic=1,nc
       c1=wf_(1,ic)
       c2=wf_(2,ic)
       c_norm_sq=c1**2.+c2**2.
       if (c_norm_sq>1.E-5) then
         ic_check=ic_check+1
         ph_cos_sq(ic)=1./(1.+(c2/c1)**2.)
         ph_cos(ic)=c1/sqrt(c_norm_sq)
         ph_sin(ic)=c2/sqrt(c_norm_sq)
         if (ic_start<0) ic_start=ic
       else
         cycle
       endif
       if (abs(ph_cos_sq(ic)-ph_cos_sq(ic_start))>0.1) then
         call warning('No phase @ band '//trim(intc(ib))//' k-point '//trim(intc(ik)))
         make_real_wf=-1
       endif
       if (ic_check>10) exit
     enddo
     phase=cmplx(ph_cos(ic_start),-ph_sin(ic_start))
     do ic=1,nc
       wf_c=cmplx(wf_(1,ic),wf_(2,ic))
       wf_(1,ic)=real(phase*wf_c)
       wf_(2,ic)=0.
     enddo
     !
   end function
   !
end module
