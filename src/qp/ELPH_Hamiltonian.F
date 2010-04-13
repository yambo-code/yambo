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
 use units,         ONLY:HARTREE
 use ELPH,          ONLY:elph_global_alloc,elph_global_free,elph_Ham_bands,elph_Ham_ik,&
&                        ph_freqs_sq,elph_gkkp,elph_branches
 use IO_m,          ONLY:io_control,OP_RD,REP,RD_CL_IF_END
 use com,           ONLY:error,msg,of_open_close
 use R_lattice,     ONLY:nqibz,nqbz,bz_samp,qindx_S,ik_is_table
 use D_lattice,     ONLY:sop_inv,nsym,sop_tab,i_time_rev
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
&                        iHp,ibp,iq_ibz_db,ik_bz_gkkp,ib_gkkp,ob_gkkp,iw
 real(SP)             :: ph_E,residual
 integer, external    :: ioELPH
 integer,     allocatable :: elph_Ham_Table(:,:)
 real   (SP), allocatable :: elph_E(:)
 complex(SP), allocatable :: elph_Mat(:,:)
 !
 ! GF's
 !
 integer, parameter :: N_w=1000
 complex(SP)        :: W(N_w),G(N_w)
 character(lchlen)  :: file_name
 !
 call section('*','Correlation: Electron-Phonon Hamiltonian')
 !
 call k_sym2sym(k,'k')
 call k_expand(k)
 !
 No_PH_basis_size=elph_Ham_bands(2)-elph_Ham_bands(1)+1
 H_size=No_PH_basis_size*(nqbz*(elph_branches(2)-elph_branches(1)+1)+1)
 call msg('rsn','[ElPh Ham] Hamiltonian size is:',H_size)
 !
 allocate(elph_Ham_Table(H_size,3),elph_Mat(H_size,H_size),elph_E(H_size)) 
 elph_Mat=(0._SP,0._SP)
 elph_Ham_Table=0
 !
 H_size=0
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
     ib_gkkp=ib
     ob_gkkp=ibp
     ik_bz_gkkp=ik_is_table(elph_Ham_ik,sop_inv(iq_s))
     if (iq_s>nsym/(i_time_rev+1)) then
       ib_gkkp   =ib
       ob_gkkp   =ibp
       ik_bz_gkkp=ik_is_table(ok_ibz,sop_tab(sop_inv(iq_s-nsym/2),ok_s))
     endif
     !
     elph_Mat(iH,iHp)=conjg(elph_gkkp(ik_bz_gkkp,il,ob_gkkp,ib_gkkp))/sqrt(2.*ph_E)
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
 call live_timing('Diagonalization',1,SERIAL=.TRUE.)
 call mat_dia_inv(DIAGO,USE_LK,elph_Mat,E_real=elph_E)
 call live_timing(steps=1)
 call live_timing()
 !
 do iw=1,N_w
   W(iw)= (minval(elph_E)-2./HARTREE)+ ( maxval(elph_E) -  minval(elph_E) + 4./HARTREE )/real(N_w)*iw+cmplx(0.,QP_G_damp)
 enddo
 !
 call set_real_printed_length(f_length=12,g_length=12)
 !
 do ib=elph_Ham_bands(1),elph_Ham_bands(2)
   G(:)=(0.,0.)
   do iH=1,H_size
     do iw=1,N_w
       G(iw)=G(iw)+abs(elph_Mat(ib-elph_Ham_bands(1)+1,iH))**2./( conjg(W(iw)) - elph_E(iH) )
     enddo
   enddo
   !
   write (file_name,'(2(a,i3.3))') 'G_Ham_band_',elph_Ham_ik,'_k_',ib
   call of_open_close(file_name,'ot')
   call msg('o Ham','# El-Ph Hamiltonian derived Green`s function')
   !
   call msg('o Ham','#')
   call msg('o Ham','#',(/'Pole[eV]','Residual'/),INDENT=0,USE_TABS=.true.)
   call msg('o Ham','#')
   do iH=1,H_size
     residual= abs(elph_Mat(ib-elph_Ham_bands(1)+1,iH))**2.
     if (residual>1.E-2) call msg('o Ham','#',(/elph_E(iH)*HARTREE,residual/),INDENT=0,USE_TABS=.true.)
   enddo
   call msg('o Ham','#')
   call msg('o Ham','#',(/'Energy [eV]','R(G)[eV^-1]','I(G)[eV^-1]'/),INDENT=0,USE_TABS=.TRUE.)
   call msg('o Ham','#')
   do iw=1,N_w
     call msg('o Ham','',(/real(W(iw))*HARTREE,real(G(iw))/HARTREE,aimag(G(iw))/HARTREE/),INDENT=-2,USE_TABS=.TRUE.)
   enddo
   call of_open_close(file_name)
   !
 enddo
 !
 call set_real_printed_length()
 !
 ! CLEAN
 !
 call elph_global_free( )
 deallocate(elph_Ham_Table,elph_Mat,elph_E)
 !
end subroutine
