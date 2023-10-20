!
! License-Identifier: GPL
!
! Copyright (C) 2021 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine X_mat_filling(iq_mem,X,Xw,X_MEM_n_freqs,X_MEM_n_tot_freqs,X_mat)
 !
 ! PAR => SER for I/O or for cases where I/O is not done
 !
 use pars,          ONLY:SP,cZERO
 use IO_m,          ONLY:io_RESPONSE
 use frequency,     ONLY:w_samp
 use X_m,           ONLY:X_par,X_t
 use parallel_int,  ONLY:PP_redux_wait
 use parallel_m,    ONLY:PAR_COM_X_WORLD_RL_resolved,PAR_COM_X_WORLD
 !
 implicit none
 !
 integer       ::iq_mem,X_MEM_n_freqs,X_MEM_n_tot_freqs
 type(X_t)     ::X
 type(w_samp)  ::Xw
 complex(SP)   ::X_mat(X%ng,X%ng,X_MEM_n_tot_freqs)
 !
 ! Work Space
 !
 integer :: iw,iw_X_mat
 !
#if defined _PAR_IO
 if (io_RESPONSE) return
#endif
 !
 do iw=1,Xw%n_freqs
   !
   X_par(iq_mem)%I=iw
   !
   if (     io_RESPONSE) iw_X_mat=iw
   if (.not.io_RESPONSE) iw_X_mat=(iq_mem-1)*X_MEM_n_freqs+iw
   !
   X_mat(:,:,iw_X_mat)=cZERO
   !
   if (PAR_COM_X_WORLD_RL_resolved%CPU_id==0) then
     X_mat(X_par(iq_mem)%rows(1):X_par(iq_mem)%rows(2),&
&          X_par(iq_mem)%cols(1):X_par(iq_mem)%cols(2),iw_X_mat)=X_par(iq_mem)%blc(:,:,iw)
   endif
   !
   call PP_redux_wait(X_mat(:,:,iw_X_mat),COMM=PAR_COM_X_WORLD%COMM)
   !
 enddo
 !
end subroutine
