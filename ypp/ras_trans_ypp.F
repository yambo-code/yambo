!
! Copyright (C) 2000-2013 C. Hogan and the YAMBO team
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
 subroutine ras_trans_ypp(en,k)
   use pars                    
   use YPP
   use units,                   ONLY : HA2EV
   use com,                     ONLY : error, msg
   use stderr,       ONLY: intc
   use electrons,               ONLY : levels, n_sp_pol
   use matrix_operate,          ONLY : mat_r2c
 use vec_operate,         ONLY:c2a, sort
!  use IO_m,                    ONLY : io_control, OP_RD_CL, OP_WR_CL, VERIFY, REP
   use IO_m
   use optcut,                  ONLY : Xcut_q0grad, Xcut_rhoq0
   use R_lattice,               ONLY : nkibz, bz_samp, nXkibz
   use X_m,           ONLY:X_t,DIP_iR,X_alloc
 use timing,              ONLY:live_timing_is_on

   implicit none
   real(SP),allocatable :: DIP_iR_disk(:,:,:),Xcut_q0grad_disk(:,:,:)
   real(SP),allocatable :: window(:,:)
   real(SP) :: k_int(nkibz), kv(3)
!functions
   type(X_t)   :: X
   type(bz_samp)   :: k
   type(levels),   intent(in)       :: en
   ! 
   ! Work Space
   !
integer :: db_nbm,db_nbf,db_nb(2),nb(2), nbm, ixyz, i_spin, ib, i1, nbf
   logical                          :: lypp, lusecut
   integer                          :: ik,iv,ic,io_err,io_db,sec_size, io_cut
   real(SP)                         :: ehe(2), Ec_m_Ev, PP, tot
   complex(SP)                       :: P1,P2
 character(schlen)    :: VAR_name, sch
 character(len=13)    :: Ewindow
! Saved
   integer, parameter               :: maxt = 9999 
   integer                          :: trans_index(3,maxt), ntrans, indx(maxt), it
   real(SP)                         :: trans_energy(maxt),trans_value(maxt)
 
   call section('*','== Direct transition analyser ==')

   lusecut = .false.
   call get_q0grad
   call get_q0grad_cut
   !
   ! Reporting
   !
   live_timing_is_on=.false.

   write(Ewindow,'(a,f4.2,a,f4.2,a)') "[",Ecv_min*HA2EV,":",Ecv_max*HA2EV,"]eV"
   write(sch,'(a)') " Analysing transitions in range "//Ewindow
   call msg('ns', trim(sch))
   write(sch,'(a,3f6.3)') " Polarization direction [cc]: ",qdir
   call msg('ns', trim(sch))
   write(sch,'(a,i3,a,i3,a)') " From db.ostnts: iv range [",db_nb(1),":",db_nbm,"]"
   call msg('ns', trim(sch))
   write(sch,'(a,i3,a,i3,a)') "                 ic range [",db_nbf+1,":",db_nb(2),"]"
   call msg('ns', trim(sch))
   if(lusecut) call msg('ns',' Mixing transitions from db.ostnts_cut')
   !
   ! Filter transitions inside the requested range
   !
   k_int(:) = 0 
   ntrans = 0 
   do ik=1,nXkibz
     do iv=db_nb(1),db_nbm
       do ic=db_nbf+1,db_nb(2)
         do i_spin = 1,1

           Ec_m_Ev=en%E(ic,ik,i_spin)-en%E(iv,ik,i_spin)

         if( Ec_m_Ev <= Ecv_max .and. Ec_m_Ev >= Ecv_min) then

           P2 = dot_product(qdir,DIP_iR(:,ic,iv,ik,i_spin))
           P1 = conjg(P2)
           if(lusecut) P1 = conjg(dot_product(qdir,Xcut_q0grad(:,ic,iv,ik,i_spin)))
           PP = abs(P1*P2)
           k_int(ik) = k_int(ik)+PP
           ntrans = ntrans + 1
           if(ntrans.gt.maxt) call error('Too many transitions in this range')
           trans_index(:,ntrans) = (/ik,iv,ic/)
           trans_energy(ntrans)  = Ec_m_Ev*HA2EV
           trans_value(ntrans)   = PP

!          write(42,100) ik,iv,ic,Ec_m_Ev*HARTREE,PP
         endif
         enddo

       enddo
     enddo
   enddo
   ! 
   ! Map the transitions in k-space
   !
   tot = sum(k_int)
   call msg('ns',' ')
   call msg('ns','    Transitions summed in k-space, window: '//Ewindow)
   call msg('ns','    Polarization direction: ', qdir)
   call msg('ns','   ----------------------------------')
   do ik=1,nkibz
     call c2a(v_in=k%pt(ik,:),v_out=kv,mode="ki2a")
     write(sch,101) ik,kv(:),k_int(ik)/tot
     call msg('s',trim(sch))
   enddo
   ! 
   ! Sort the transitions with respect to their oscillator weight
   !
   call msg('ns',' ')
   call msg('ns','    Transitions window: '//Ewindow)
   call msg('ns','    Polarization direction: ', qdir)
   write(sch,'(a5,2a4,2a12)') "ik","iv","ic","Evc","|P|^2"
   call msg('ns', trim(sch))
   call msg('ns','   ----------------------------------')
   call sort(arrin=trans_value(1:ntrans),indx=indx(1:ntrans))
   do it=ntrans,max(ntrans-20,1),-1
     write(sch,100) trans_index(:,indx(it)), trans_energy(indx(it)), trans_value(it)
     call msg("s",trim(sch))
   enddo 
   live_timing_is_on=.true.

   
   return
100 format(i5,2i4,f12.5,f12.5)
101 format(i5,3f8.3,f12.5)
 
 contains

   subroutine get_q0grad_cut
     implicit none

     call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/2/),MODE=VERIFY,ID=io_cut)
     io_err=io_connect(desc='ostnts_cut',type=1,ID=io_cut)
     if(io_err.ne.0) return

     allocate(Xcut_q0grad_disk(db_nb(2),db_nbm,2))
     allocate(Xcut_q0grad(3,db_nb(2),db_nbm,nXkibz,n_sp_pol))
     do i1=1,nXkibz
       do ixyz=1,3
         do i_spin=1,n_sp_pol
           write (VAR_name,'(3(a,i4.4))') 'DIP_iR_k_',i1,'_xyz_',ixyz,'_spin_',i_spin
           call io_bulk(io_cut,VAR=trim(VAR_name),VAR_SZ=shape(Xcut_q0grad_disk))
           call io_bulk(io_cut,R3=Xcut_q0grad_disk)
           call mat_r2c(Xcut_q0grad_disk,Xcut_q0grad(ixyz,:,:,i1,i_spin))
         enddo
       enddo
     enddo

     lusecut = .true.
     return
   end subroutine get_q0grad_cut

   subroutine get_q0grad
     implicit none

     call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/2/),MODE=VERIFY,ID=io_db)
     io_err=io_connect(desc='ostnts',type=1,ID=io_db)
     sec_size=8
     io_err=io_header(io_db,R_LATT=.true.,WF=.true.,IMPOSE_SN=.true.,T_EL=.true.)
     call io_elemental(io_db,VAR="PARS",VAR_SZ=sec_size,MENU=1)
     call io_elemental(io_db,DB_I1=db_nb,I1=nb)
     call io_elemental(io_db,R1=ehe)
     call io_elemental(io_db,DB_I0=db_nbm,I0=nbm)
     call io_elemental(io_db,DB_I0=db_nbf,I0=nbf)
     call io_elemental(io_db,VAR="",VAR_SZ=0)

     allocate(DIP_iR_disk(db_nb(2),db_nbm,2))
     call X_alloc('DIP_iR',(/3,db_nb(2),db_nbm,nXkibz/))

     do i1=1,nXkibz
       do ixyz=1,3
         do i_spin=1,n_sp_pol
           write (VAR_name,'(3(a,i4.4))') 'DIP_iR_k_',i1,'_xyz_',ixyz,'_spin_',i_spin
           call io_bulk(io_db,VAR=trim(VAR_name),VAR_SZ=shape(DIP_iR_disk))
           call io_bulk(io_db,R3=DIP_iR_disk)
           call mat_r2c(DIP_iR_disk,DIP_iR(ixyz,:,:,i1,i_spin))
         enddo
       enddo
     enddo
     return
   end subroutine get_q0grad

 end subroutine ras_trans_ypp
