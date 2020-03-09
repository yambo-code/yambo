!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
! 
! This file is distributed under the terms of the GNU 
! General Public License. You can redistribute it and/or 
! modify it under the terms of the GNU General Public 
! License as published by the Free Software Foundation; 
! either version 2, or (at your option) any later version.
!
! This program is distributed in the hope that it will 
! be useful, but WITHOUT ANY WARRANTY; without even the 
! implied warranty of MERCHANTABILITY or FITNESS FOR A 
! PARTICULAR PURPOSE.  See the GNU General Public License 
! for more details.
!
! You should have received a copy of the GNU General Public 
! License along with this program; if not, write to the Free 
! Software Foundation, Inc., 59 Temple Place - Suite 330,Boston, 
! MA 02111-1307, USA or visit http://www.gnu.org/copyleft/gpl.txt.
!
subroutine ELPH_Hamiltonian(en,k,q)
 !
 use pars,          ONLY:SP,lchlen,rZERO
 use units,         ONLY:HA2EV
 use ELPH,          ONLY:elph_global_alloc,elph_global_free,elph_Ham_bands,elph_Ham_ik,&
&                        ph_freqs_sq,elph_gkkp,elph_branches,pol_vector,RES_tresh
 use IO_m,          ONLY:io_control,manage_action,OP_RD,REP,RD_CL_IF_END,mk_dir
 use com,           ONLY:msg,of_open_close,jobstr,jobdir,core_io_path
 use R_lattice,     ONLY:nqbz,nqibz,bz_samp,qindx_S
 use D_lattice,     ONLY:sop_inv,nsym,sop_tab,i_time_rev,n_atoms
 use electrons,     ONLY:levels
 use LIVE_t,        ONLY:live_timing
 use interfaces,    ONLY:LINEAR_ALGEBRA_driver
 use linear_algebra,ONLY:DIAGO,USE_LK
 use stderr,        ONLY:intc
 use QP_m,          ONLY:QP_G_damp
 use parser_m,      ONLY:parser
 !
#include<memory.h>
 !
 type(levels)  ::en
 type(bz_samp) ::k,q
 !
 ! Work Space
 !
 integer              :: H_size,ib,iq_bz,iq_ibz,iq_s,ID,io_err,IO_ACT,iH,il,ok_bz,ok_ibz,ok_s,No_PH_basis_size,&
&                        iHp,ibp,iq_ibz_db,ik_bz_gkkp,ib_gkkp,ibp_gkkp,iw
 real(SP)             :: ph_E,residual,q_weight
 logical              :: l_eval_Displacements
 integer, external    :: io_ELPH
 integer,     allocatable :: elph_Ham_Table(:,:)
 real   (SP), allocatable :: elph_E(:)
 complex(SP), allocatable :: elph_Mat(:,:)
 !
 ! GF's
 !
 integer, parameter :: N_w=1000
 complex(SP)        :: W(N_w),G(N_w)
 character(lchlen)  :: file_name,tmp_string,db_name
 !
 ! Residuals
 !
 logical, allocatable :: top_residual(:)
 real(SP)             :: max_residual
 !
 ! Mean value of displacements
 !
 integer            :: ic,ia,Ham_Index
 real(SP)           :: ph_E_gamma,pol_vector_gamma(n_atoms*3,n_atoms,3)
 complex(SP), allocatable :: mean_u(:,:,:)
 !
 !Mean value of Phonons Number
 !
 integer            :: Ph_branches
 real               :: ph_residual
 real(SP),allocatable     :: top_ph_residual(:,:,:)
 complex(SP), allocatable :: num_of_ph(:,:,:)
 !
 !Mean value of Displacements^2
 !
 integer            :: iV
 real               :: A_nk_sq,B_nkq_sq
 complex(SP), allocatable :: mean_u_sq(:,:,:)
 !
 logical, external  :: file_exists
 !
 call section('*','Correlation: Electron-Phonon Hamiltonian')
 !
 call k_build_up_BZ_tables(k)
 call k_expand(k)
 !
 call parser('Eval_Displacements',l_eval_displacements)
 !
 Ph_branches=elph_branches(2)-elph_branches(1)+1
 No_PH_basis_size=elph_Ham_bands(2)-elph_Ham_bands(1)+1
 H_size=No_PH_basis_size*(nqbz*(elph_branches(2)-elph_branches(1)+1)+1)
 call msg('rsn','[ElPh Ham] Hamiltonian size is',H_size)
 !
 YAMBO_ALLOC(elph_Ham_Table,(H_size,3))
 YAMBO_ALLOC(elph_Mat,(H_size,H_size))
 YAMBO_ALLOC(elph_E,(H_size))
 YAMBO_ALLOC(mean_u,(n_atoms,3,H_size))
 YAMBO_ALLOC(num_of_ph,(nqbz,Ph_branches,H_size))
 YAMBO_ALLOC(top_residual,(H_size))
 YAMBO_ALLOC(top_ph_residual,(nqbz,Ph_branches,H_size))
 YAMBO_ALLOC(mean_u_sq,(n_atoms,3,H_size))
 top_residual=.FALSE.
 elph_Mat=(0._SP,0._SP)
 elph_Ham_Table=0
 !
 H_size=0
 q_weight=1./sqrt(real(nqbz))
 !
 ! Basis ...
 !===========
 !
 ! ... no phonons part ...
 !
 do ib=elph_Ham_bands(1),elph_Ham_bands(2)
   H_size=H_size+1
   elph_Ham_Table(H_size,:)=(/ib,0,0/)
 enddo
 !
 ! ... 1 Phonon part
 !
 do iq_bz=1,nqbz
   do il=elph_branches(1),elph_branches(2)
     do ib=elph_Ham_bands(1),elph_Ham_bands(2)
       H_size=H_size+1
       elph_Ham_Table(H_size,:)=(/ib,iq_bz,il/)
     enddo
   enddo
 enddo
 !
 ! ELPH DB
 !=========
 !
 call io_control(ACTION=OP_RD,COM=REP,SEC=(/1/),ID=ID)
 io_err=io_ELPH(ID,'gkkp')
 if (io_err/=0) call error('El-Ph database not found')
 !
 call elph_global_alloc('gkkp')
 !
 ! Hamiltonian Loop
 !==================
 !
 call live_timing('El-Ph Hamiltonian',No_PH_basis_size*(H_size-No_PH_basis_size))
 !
 iq_ibz_db=0
 !
 do iH=1,No_PH_basis_size
   ib             =elph_Ham_Table(iH,1)
   elph_Mat(iH,iH)=en%E(ib,elph_Ham_ik,1)
   do iHp=No_PH_basis_size+1,H_size
     ibp    =elph_Ham_Table(iHp,1)
     iq_bz  =elph_Ham_Table(iHp,2)
     il     =elph_Ham_Table(iHp,3)
     iq_ibz =q%sstar(iq_bz,1)
     iq_s   =q%sstar(iq_bz,2)
     ok_bz  =qindx_S(elph_Ham_ik,iq_bz,1)
     ok_ibz =k%sstar(ok_bz,1)
     ok_s   =k%sstar(ok_bz,2)
     !
     if (iq_ibz/=iq_ibz_db) then
       IO_ACT=manage_action(RD_CL_IF_END,iq_ibz,1,nqibz)
       call io_control(ACTION=IO_ACT,SEC=(/iq_ibz+1/),ID=ID)
       io_err=io_ELPH(ID,'gkkp')
       iq_ibz_db=iq_ibz
       if (iq_ibz==1) pol_vector_gamma=pol_vector
     endif
     !
     ph_E=sqrt(abs(ph_freqs_sq(iq_ibz,il)))
     !
     if (iH==1) elph_Mat(iHp,iHp)=en%E(ibp,ok_ibz,1)+ph_E
     !
     if (abs(ph_E)<epsilon(1._SP)) then
       call live_timing(steps=1)
       cycle
     endif
     !
     ib_gkkp =ib
     ibp_gkkp=ibp
     ik_bz_gkkp=k%k_table(elph_Ham_ik,sop_inv(iq_s))
     if (iq_s>nsym/(i_time_rev+1)) then
       ib_gkkp   =ib
       ibp_gkkp  =ibp
       ik_bz_gkkp=k%k_table(ok_ibz,sop_tab(sop_inv(iq_s-nsym/2),ok_s))
     endif
     !
     elph_Mat(iH,iHp)=q_weight*conjg(elph_gkkp(il,ibp_gkkp,ib_gkkp,ik_bz_gkkp))/sqrt(2._SP*ph_E)
     !
     elph_Mat(iHp,iH)=conjg( elph_Mat(iH,iHp) )
     !
     call live_timing(steps=1)
     !
   enddo
 enddo
 !
 call live_timing()
 !
 ! Diagonalization 
 !=================
 !
 if (trim(jobstr)=="") call error("Please provide a non empty string for the job_name")
 !
 call mk_dir(trim(core_io_path)//"/"//trim(jobdir))
 !
 db_name=trim(jobdir)//'/DIAGO_DB_k_'//trim(intc(elph_Ham_ik))
 if (file_exists(db_name)) then
   call msg('s','Eigenvectors/Eigenvalues I/O')
   open(unit=1,file=trim(db_name),status='old',form='unformatted')
   read(1) elph_E
   read(1) elph_Mat
   close(1)
 else
   call live_timing('Diagonalization',1)
   call LINEAR_ALGEBRA_driver(DIAGO,M=elph_Mat,lib_in=USE_LK,E_real=elph_E)
   call live_timing(steps=1)
   call live_timing()
   open(unit=100,file=trim(db_name),status='new',form='unformatted')
   write(100) elph_E(:)
   write(100) elph_Mat(:,:)
   close(100)
 endif
 !
 ! Green's Functions 
 !===================
 !
 do iw=1,N_w
   W(iw)= (minval(elph_E)-.1/HA2EV)+ ( maxval(elph_E) -  minval(elph_E) + .2/HA2EV )/real(N_w)*iw+cmplx(0.,QP_G_damp)
 enddo
 !
 call live_timing('Green`s Functions',elph_Ham_bands(2)-elph_Ham_bands(1)+1)
 !
 do ib=elph_Ham_bands(1),elph_Ham_bands(2)
   G(:)=(0.,0.)
   do iH=1,H_size
     do iw=1,N_w
       G(iw)=G(iw)+abs(elph_Mat(ib-elph_Ham_bands(1)+1,iH))**2./( conjg(W(iw)) - elph_E(iH) )
     enddo
   enddo
   !
   db_name=trim(jobstr)//'/TOP_RESIDUAL_b'//trim(intc(ib))//'_k'//trim(intc(elph_Ham_ik))
   open(unit=100,file=trim(db_name),form='unformatted')
   write (100) elph_Ham_bands
   write (file_name,'(2(a,i3.3))') 'G_Ham_band_',ib,'_k_',elph_Ham_ik
   call of_open_close(file_name,'ot')
   call msg('o Ham','# El-Ph Hamiltonian derived Green`s function')
   !
   call msg('o Ham','#')
   call msg('o Ham','#',(/' Index  ','Pole[eV]','Residual'/),INDENT=0,USE_TABS=.true.)
   call msg('o Ham','#')
   !
   max_residual=0.
   !
   do iH=1,H_size
     residual= abs(elph_Mat(ib-elph_Ham_bands(1)+1,iH))**2.
     if (residual>max_residual) max_residual=residual
   enddo
   do iH=1,H_size
     residual= abs(elph_Mat(ib-elph_Ham_bands(1)+1,iH))**2.
     if (residual>RES_tresh*max_residual) then
       call msg('o Ham','#',(/real(iH,SP),elph_E(iH)*HA2EV,residual/),INDENT=0,USE_TABS=.true.)
       write (100) iH,residual
       top_residual(iH)=.TRUE.
     endif
   enddo
   !
   call msg('o Ham','#')
   call msg('o Ham','# Maximum residual is ', max_residual,INDENT=0)
   call msg('o Ham','#')
   !
   do iH=1,H_size
     if (.not.top_residual(iH)) cycle
     max_residual=0.
     do iHp=1,H_size
       residual= abs(elph_Mat(iHp,iH))**2.
       if (residual>max_residual) max_residual=residual
     enddo
     call msg('o Ham','# State '//trim(intc(iH))//' structure')
     call msg('o Ham','#')
     call msg('o Ham','#',(/' Band   ','Q-point ','K-point ',' Branch ','Residual'/),INDENT=0,USE_TABS=.true.)
     do iHp=1,H_size
       residual= abs(elph_Mat(iHp,iH))**2.
       if (residual>RES_tresh*max_residual) then
         ibp    =elph_Ham_Table(iHp,1)
         iq_bz  =elph_Ham_Table(iHp,2)
         il     =elph_Ham_Table(iHp,3)
         iq_ibz =q%sstar(iq_bz,1)
         iq_s   =q%sstar(iq_bz,2)
         ok_bz  =qindx_S(elph_Ham_ik,iq_bz,1)
         ok_ibz =k%sstar(ok_bz,1)
         ok_s   =k%sstar(ok_bz,2)
         if (iq_bz==0) call msg('o Ham','#',(/real(ibp,SP),rZERO,rZERO,rZERO,residual/),INDENT=0,USE_TABS=.true.)
         if (iq_bz> 0) call msg('o Ham','#',(/real(ibp,SP),real(iq_ibz,SP),real(ok_ibz,SP), &
&                                             real(il,SP),residual/),INDENT=0,USE_TABS=.true.)
       endif
     enddo
     call msg('o Ham','#')
   enddo
   !
   call msg('o Ham','#')
   call msg('o Ham','#',(/'Energy [eV]','R(G)[eV^-1]','I(G)[eV^-1]'/),INDENT=0,USE_TABS=.TRUE.)
   call msg('o Ham','#')
   do iw=1,N_w
     call msg('o Ham','',(/real(W(iw))*HA2EV,real(G(iw))/HA2EV,aimag(G(iw))/HA2EV/),INDENT=-2,USE_TABS=.TRUE.)
   enddo
   call of_open_close(file_name)
   close(100)
   !
   call live_timing(steps=1)
   !
 enddo
 !
 call live_timing()
 !
 ! Eigenvectors written to DB for post-analysis
 !=============================================
 !
 ! |I_k>= A_nk * |nk> + SUM_n_q_il ( B_n_q_il * |n,k-q>|q,il> )
 !
 do iH=1,H_size
   if (.not.top_residual(iH)) cycle
   db_name=trim(jobstr)//'/EIGENV_I'//trim(intc(iH))//'_k'//trim(intc(elph_Ham_ik))
   open(unit=100,file=trim(db_name),form='unformatted')
   write(100) H_size
   do iHp=1,H_size
     ibp    =elph_Ham_Table(iHp,1)
     iq_bz  =elph_Ham_Table(iHp,2)
     if (iq_bz>0) then
       il     =elph_Ham_Table(iHp,3)
       iq_ibz =q%sstar(iq_bz,1)
       iq_s   =q%sstar(iq_bz,2)
       ok_bz  =qindx_S(elph_Ham_ik,iq_bz,1)
       ok_ibz =k%sstar(ok_bz,1)
       ok_s   =k%sstar(ok_bz,2)
     else
       il     =0
       iq_ibz =0
       iq_s   =0
       ok_bz  =0
       ok_ibz =0
       ok_s   =0
     endif
     write(100) ibp,ok_ibz,ok_s,il,iq_ibz,iq_s,elph_Mat(iHp,iH) 
   enddo
   close(100)
 enddo
 !
 if (.not.l_eval_Displacements) goto 1
 !
 call section('+','Mean Displacements (MD)')
 !                                
 ! Mean values of displacement <iH|u_alfa_i_s|iH>
 ! ==============================================
 ! iH is the eigenvector of elph_Mat
 !
 mean_u=0.0
 !
 call live_timing('MD evaluation',H_size)
 !
 call msg('r','Displacements')
 !
 do iH=1,H_size
   call live_timing(steps=1)
   if (.not.top_residual(iH)) cycle
   do ia=1,n_atoms
     do ic=1,3
       Ham_Index=0
       do il=elph_branches(1),elph_branches(2)
         !
         ph_E_gamma=sqrt( MAX ( ph_freqs_sq(1,il), 0.) )
         if ( ph_E_gamma < epsilon(1._SP)) cycle
         !
         Ham_Index=Ham_Index+No_PH_basis_size
         !
         do ib=elph_Ham_bands(1),elph_Ham_bands(2)
           mean_u(ia,ic,iH)=mean_u(ia,ic,iH)+&
&            1./sqrt(real(2*nqbz*ph_E_gamma))*pol_vector_gamma(il,ia,ic)*(conjg(elph_Mat(ib,iH))*elph_Mat(Ham_Index+ib,iH)+&
&                                                                         conjg(elph_Mat(Ham_Index+ib,iH))*elph_Mat(ib,iH))
           mean_u(ia,ic,iH)=real( mean_u(ia,ic,iH) )
         enddo
       enddo
     enddo
     write (tmp_string,'(a,i4.4,2a,i2.2,a,3(1x,f5.3))') 'State(',iH,')-','Atom(',ia,')',real(mean_u(ia,:,iH))
     call msg('r',trim(tmp_string))
   enddo
 enddo
 call live_timing()
 !                                
 !Mean values of displacement <iH|(u_alfa_i_s)^2|iH>
 !===================================================
 ! iH is the eigenvector of elph_Mat
 ! |iH> = Sum_n A_nk*|n,k> + Sum_n_q_lambda B_nkq*|n,k-q> |q,lambda>
 ! A_nk_sq = conjg(A_nk)*A_nk
 ! B_nkq_sq = conjg(B_nkq)*B_nkq
 !
 mean_u_sq=0.0
 !
 call live_timing('MD square evaluation',H_size)
 !
 call msg('nr','sqrt(MD square)')
 !
 do iH=1,H_size
   call live_timing(steps=1)
   if (.not.top_residual(iH)) cycle
   !
   A_nk_sq=0.0
   B_nkq_sq=0.0
   do iV=1,No_PH_basis_size
     A_nk_sq=A_nk_sq+conjg(elph_Mat(iV,iH))*elph_Mat(iV,iH) 
   enddo
   A_nk_sq=real( A_nk_sq )
   !
   do iV=No_PH_basis_size+1,H_size
     B_nkq_sq=B_nkq_sq+conjg(elph_Mat(iV,iH))*elph_Mat(iV,iH)
   enddo
   B_nkq_sq=real( B_nkq_sq )
   !
   do ia=1,n_atoms
     do ic=1,3
       !
       do iq_bz=1,nqbz
         !
         iq_ibz =q%sstar(iq_bz,1)
         IO_ACT=manage_action(RD_CL_IF_END,iq_bz,1,nqbz)
         call io_control(ACTION=IO_ACT,SEC=(/iq_ibz+1/),ID=ID)
         io_err=io_ELPH(ID,'gkkp')
         !
         do il=elph_branches(1),elph_branches(2)
           !
           ph_E=sqrt( MAX ( ph_freqs_sq(iq_ibz,il), 0.) )
           if ( ph_E < epsilon(1._SP) ) cycle
           !
           mean_u_sq(ia,ic,iH) = mean_u_sq(ia,ic,iH)+1/(real(2*nqbz*ph_E))*conjg(pol_vector(il,ia,ic))*pol_vector(il,ia,ic)
           mean_u_sq(ia,ic,iH) = real( mean_u_sq(ia,ic,iH) )
         enddo  
       enddo
       mean_u_sq(ia,ic,iH)=mean_u_sq(ia,ic,iH)*( A_nk_sq + 3*B_nkq_sq )
     enddo
     write (tmp_string,'(a,i4.4,2a,i2.2,a,3(1x,f5.3))') 'State(',iH,')-','Atom(',ia,')',real(mean_u_sq(ia,:,iH))
     call msg('r',trim(tmp_string))
   enddo 
   !
 enddo
 !
 call live_timing()
 !
 ! CLEAN
 !
1 continue
 !
 call elph_global_free( )
 YAMBO_FREE(elph_Ham_Table)
 YAMBO_FREE(elph_Mat)
 YAMBO_FREE(elph_E)
 YAMBO_FREE(mean_u)
 YAMBO_FREE(top_residual)
 YAMBO_FREE(num_of_ph)
 YAMBO_FREE(top_ph_residual)
 YAMBO_FREE(mean_u_sq)
 !
end subroutine
