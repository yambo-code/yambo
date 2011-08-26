!
! Copyright (C) 2000-2010 A. Marini and the YAMBO team 
!              http://www.yambo-code.org
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
 use pars,          ONLY:SP,lchlen
 use units,         ONLY:HA2EV
 use ELPH,          ONLY:elph_global_alloc,elph_global_free,elph_Ham_bands,elph_Ham_ik,&
&                        ph_freqs_sq,elph_gkkp,elph_branches,pol_vector
 use IO_m,          ONLY:io_control,OP_RD,REP,RD_CL_IF_END,mk_dir
 use com,           ONLY:error,msg,of_open_close,file_exists
 use R_lattice,     ONLY:nqibz,nqbz,bz_samp,qindx_S,ik_is_table
 use D_lattice,     ONLY:sop_inv,nsym,sop_tab,i_time_rev,n_atoms
 use electrons,     ONLY:levels
 use timing,        ONLY:live_timing
 use matrix_operate,ONLY:mat_dia_inv,DIAGO,USE_LK
 use stderr,        ONLY:intc,set_real_printed_length
 use QP_m,          ONLY:QP_G_damp
 !
 implicit none
 !
 type(levels)  ::en
 type(bz_samp) ::k,q
 !
 ! Work Space
 !
 integer              :: H_size,ib,iq_bz,iq_ibz,iq_s,ID,io_err,iH,il,ok_bz,ok_ibz,ok_s,No_PH_basis_size,&
&                        iHp,ibp,iq_ibz_db,ik_bz_gkkp,ib_gkkp,ibp_gkkp,iw
 real(SP)             :: ph_E,residual,q_weight
 integer, external    :: ioELPH
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
 call section('*','Correlation: Electron-Phonon Hamiltonian')
 !
 call k_sym2sym(k,'k')
 call k_expand(k)
 !
 Ph_branches=elph_branches(2)-elph_branches(1)+1
 No_PH_basis_size=elph_Ham_bands(2)-elph_Ham_bands(1)+1
 H_size=No_PH_basis_size*(nqbz*(elph_branches(2)-elph_branches(1)+1)+1)
 call msg('rsn','[ElPh Ham] Hamiltonian size is:',H_size)
 !
 allocate(elph_Ham_Table(H_size,3),elph_Mat(H_size,H_size),elph_E(H_size),&
&         mean_u(n_atoms,3,H_size),num_of_ph(nqbz,Ph_branches,H_size)) 
 allocate(top_residual(H_size),top_ph_residual(nqbz,Ph_branches,H_size))
 allocate(mean_u_sq(n_atoms,3,H_size))
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
 io_err=ioELPH(ID,'gkkp')
 if (io_err/=0) call error('El-Ph database not found')
 !
 call elph_global_alloc('gkkp')
 !
 ! Hamiltonian Loop
 !==================
 !
 call live_timing('El-Ph Hamiltonian',No_PH_basis_size*(H_size-No_PH_basis_size),SERIAL=.TRUE.)
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
       call io_control(ACTION=RD_CL_IF_END,SEC=(/iq_ibz+1/),ID=ID)
       io_err=ioELPH(ID,'gkkp')
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
     ik_bz_gkkp=ik_is_table(elph_Ham_ik,sop_inv(iq_s))
     if (iq_s>nsym/(i_time_rev+1)) then
       ib_gkkp   =ib
       ibp_gkkp  =ibp
       ik_bz_gkkp=ik_is_table(ok_ibz,sop_tab(sop_inv(iq_s-nsym/2),ok_s))
     endif
     !
     elph_Mat(iH,iHp)=q_weight*conjg(elph_gkkp(ik_bz_gkkp,il,ibp_gkkp,ib_gkkp))/sqrt(2.*ph_E)
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
 call mk_dir('ELPH_Ham')
 !
 db_name='ELPH_Ham/DIAGO_DB_k_'//trim(intc(elph_Ham_ik))
 if (file_exists(db_name)) then
   call msg('s','Eigenvectors/Eigenvalues I/O')
   open(unit=1,file=trim(db_name),status='old',form='unformatted')
   read(1) elph_E
   read(1) elph_Mat
   close(1)
 else
   call live_timing('Diagonalization',1,SERIAL=.TRUE.)
   call mat_dia_inv(DIAGO,USE_LK,elph_Mat,E_real=elph_E)
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
 call set_real_printed_length(f_length=12,g_length=12)
 !
 call live_timing('Green`s Functions',elph_Ham_bands(2)-elph_Ham_bands(1)+1,SERIAL=.TRUE.)
 !
 do ib=elph_Ham_bands(1),elph_Ham_bands(2)
   G(:)=(0.,0.)
   do iH=1,H_size
     do iw=1,N_w
       G(iw)=G(iw)+abs(elph_Mat(ib-elph_Ham_bands(1)+1,iH))**2./( conjg(W(iw)) - elph_E(iH) )
     enddo
   enddo
   !
   db_name='ELPH_Ham/TOP_RESIDUAL_b'//trim(intc(ib))//'_k'//trim(intc(elph_Ham_ik))
   open(unit=100,file=trim(db_name),form='unformatted')
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
     if (residual>1.E-2) then
       if (residual>max_residual) max_residual=residual
       call msg('o Ham','#',(/real(iH),elph_E(iH)*HA2EV,residual/),INDENT=0,USE_TABS=.true.)
       write (100) iH,residual
       top_residual(iH)=.TRUE.
     endif
   enddo
   !
   call msg('o Ham','#')
   call msg('o Ham','# Maximum residual is ', max_residual,INDENT=0)
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
 !============================
 !
 ! |I_k>= A_nk * |nk> + SUM_n_q_il ( B_n_q_il * |n,k-q>|q,il> )
 !
 do iH=1,H_size
   if (.not.top_residual(iH)) cycle
   db_name='ELPH_Ham/EIGENV_I'//trim(intc(iH))//'_k'//trim(intc(elph_Ham_ik))
   open(unit=100,file=trim(db_name),form='unformatted')
   write(100) H_size
   do iHp=1,H_size
     ibp    =elph_Ham_Table(iHp,1)
     iq_bz  =elph_Ham_Table(iHp,2)
     il     =elph_Ham_Table(iHp,3)
     iq_ibz =q%sstar(iq_bz,1)
     iq_s   =q%sstar(iq_bz,2)
     ok_bz  =qindx_S(elph_Ham_ik,iq_bz,1)
     ok_ibz =k%sstar(ok_bz,1)
     ok_s   =k%sstar(ok_bz,2)
     write(100) ibp,ok_ibz,ok_s,il,iq_ibz,iq_s,elph_Mat(iHp,iH) 
   enddo
   close(100)
 enddo
 !
 call section('+','Mean Displacements (MD)')
 !                                
 ! Mean values of displacement <iH|u_alfa_i_s|iH>
 ! ==============================================
 !iH is the eigenvector of elph_Mat
 !
 mean_u=0.0
 !
 call live_timing('MD evaluation',H_size,SERIAL=.TRUE.)
 !
 call msg('r','Displacements:')
 call msg('r','Index ',(/'C1(x)','C2(x)','H1(x)','H2(x)','C1(y)','C2(y)','H1(y)','H2(y)','C1(z)','C2(z)','H1(z)','H2(z)'/))
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
   enddo
   write (tmp_string,'(i5,12(1x,f5.3))') iH,real(mean_u(:,:,iH))
   call msg('r',trim(tmp_string))
 enddo
 call live_timing()
 !                                
 !Mean values of displacement <iH|(u_alfa_i_s)^2|iH>
 !===========
 ! iH is the eigenvector of elph_Mat
 ! |iH> = Sum_n A_nk*|n,k> + Sum_n_q_lambda B_nkq*|n,k-q> |q,lambda>
 ! A_nk_sq = conjg(A_nk)*A_nk
 ! B_nkq_sq = conjg(B_nkq)*B_nkq
 !
 mean_u_sq=0.0
 !
 call live_timing('MD square evaluation',H_size,SERIAL=.TRUE.)
 !
 call msg('r','sqrt(MD square):')
 call msg('r','Index ',(/'C1(x)','C2(x)','H1(x)','H2(x)','C1(y)','C2(y)','H1(y)','H2(y)','C1(z)','C2(z)','H1(z)','H2(z)'/))
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
         call io_control(ACTION=RD_CL_IF_END,SEC=(/iq_ibz+1/),ID=ID)
         io_err=ioELPH(ID,'gkkp')
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
   enddo 
   !
   write (tmp_string,'(i5,12(1x,f5.3))') iH,sqrt(real(mean_u_sq(:,:,iH)))
   call msg('r',trim(tmp_string))
 enddo
 call live_timing()
 !
 !Mean value of Phonon Number <iH|N_q_lambda|iH>
 !===========
 !iH is the eigenvector of elph_Mat
!DEBUG>
 !
! num_of_ph=0.0
! !
! !Open and close a file where info about more contributing phonons are given
! !
! write (file_name,'(1(a,i3.3))') 'Ph_k_',elph_Ham_ik
! call of_open_close(file_name,'ot')
! call msg('o Ph','# El-Ph Hamiltonian derived Green`s function')
! !
! call msg('o Ph','#')
! call msg('o Ph','#',(/'Index', 'Q point','Branch', 'Residual (Phonon Part)'/),INDENT=0,USE_TABS=.true.)
! call msg('o Ph','#')
! !
! do iH=1,H_size
!   if (.not.top_residual(iH)) cycle
!   !
!   Ham_Index=No_PH_basis_size
!   !
!   do iq_bz=1,nqbz
!     do il=1,Ph_branches
!       !
!       do ib=elph_Ham_bands(1),elph_Ham_bands(2)
!         Ham_Index=Ham_Index+1
!         num_of_ph(iq_bz,il,iH)=num_of_ph(iq_bz,il,iH)+conjg(elph_Mat(Ham_Index,iH))*elph_Mat(Ham_Index,iH)
!         num_of_ph(iq_bz,il,iH)=  real( num_of_ph(iq_bz,il,iH) )
!       enddo
!       ph_residual=real( num_of_ph(iq_bz,il,iH) )
!       if (ph_residual>5.E-2) then
!         top_ph_residual(iq_bz,il,iH)=num_of_ph(iq_bz,il,iH)
!         call msg('o Ph','#',(/real(iH),real(iq_bz),real(il+elph_branches(1)-1),top_ph_residual(iq_bz,il,iH)/),INDENT=0,USE_TABS=.true.)
!       endif
!       !
!     enddo
!   enddo
!   !
! enddo   
! call msg('o Ph','#')
! call of_open_close(file_name)
!DEBUG<
 !
 call set_real_printed_length()
 !
 ! CLEAN
 !
 call elph_global_free( )
 deallocate(elph_Ham_Table,elph_Mat,elph_E)
 deallocate(mean_u,top_residual,num_of_ph,top_ph_residual)
 deallocate(mean_u_sq)
 !
end subroutine
