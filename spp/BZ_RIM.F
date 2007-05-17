!
! Copyright (C) 2000-2005 A. Marini and the SELF team 
!         http://www.fisica.uniroma2.it/~self
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
 subroutine BZ_RIM(Xk,Xen)
 !
 use pars,                ONLY:SP,DP,lchlen
 use R_lattice,           ONLY:bz_samp,nkibz
 use D_lattice,           ONLY:alat,nsym,default_Tel,a
 use com,                 ONLY:msg,error
 use vec_operate,         ONLY:c2a,ak2bz
 use SPP,                 ONLY:BZ_RIM_nkpts,coo_out,BZ_RIM_path
 use electrons,           ONLY:levels,default_nel,n_bands,&
&                              BZ_RIM_nbands,BZ_RIM_nkpt,BZ_RIM_table,&
&                              BZ_RIM_tot_nkpts,n_spin,BZ_RIM_max_filling
 use timing,              ONLY:live_timing_is_on
 use IO_m,                ONLY:io_control,OP_RD_CL,NONE,DUMP,OP_WR_CL
 !
 implicit none
 type(bz_samp) :: Xk
 type(levels)  :: Xen
 !
 ! Work Space
 !
 real(SP)     ::dummy,v(3),rand_kpt(BZ_RIM_nkpts+1,3),a_save(3,3),alat_save(3)
 integer      ::i1,i2,ikbz,i3
 integer,parameter :: iseed(4) = (/11,123,44,1546/)
 integer, external :: k_the_nearest
 real(DP)  :: rr
 real(DP),external  :: dlaran
 logical            :: add_X_grid
 !
 ! BZ DB1 I/O
 !
 type(levels)      :: BZ_E
 type(bz_samp)     :: BZ_k
 integer           :: ID,i_err
 integer, external :: ioDB1_reload,ioE_RIM
 !
 ! Saves 2 check
 !
 a_save=a
 alat_save=alat
 !
 ! DB1 reloading
 !
 i_err=ioDB1_reload(BZ_E,BZ_k, trim(BZ_RIM_path) )
 !
 ! DB1 checks
 !
 if (i_err==0.and.any(a/=a_save))  then
   call msg("s",':: RIM DB1 DL structure does not fit')
   i_err=-1
 endif
 if (i_err==0.and.any(alat/=alat_save))  then
   call msg("s",':: RIM DB1 alat does not fit.')
   i_err=-1
 endif
 !
 if (i_err/=0.or.trim(BZ_RIM_path)==".") then
   !
   call section('*',"== BZ RIM grid generator ==")
   live_timing_is_on=.false.
   call msg("s"," ========== RIM K-grid ==========")
   do i1=1,BZ_RIM_nkpts
     do i2=1,3
       call random_number(rr)
       rand_kpt(i1,i2)=(2.*rr-1.)
     enddo
     dummy=ak2bz(rand_kpt(i1,:))
     call c2a(v_in=rand_kpt(i1,:),mode="ka2i")
     if (trim(coo_out)=="cc") call c2a(v_in=rand_kpt(i1,:),mode="ki2c")
     if (trim(coo_out)=="rlu") call c2a(v_in=rand_kpt(i1,:),mode="ki2a")
     call msg("s"," ",rand_kpt(i1,:))
   enddo
   live_timing_is_on=.true.
   return
 endif 
 !
 call section('*','BZ RIM DB')
 !
 call msg('s',':: Electrons             :',default_nel)
 call msg('s',':: Temperature       [ev]:',default_Tel)
 call msg('s',':: Lattice factors [a.u.]:',alat)
 call msg('s',':: K points              :',nkibz)
 call msg('s',':: Bands                 :',n_bands)
 call msg('s',':: Symmmetries           :',nsym)
 !
 call section('+','BZ RIM K-points Tables setup...')
 !
 add_X_grid=.true.
 !
 BZ_RIM_nbands=BZ_E%nb
 BZ_RIM_tot_nkpts=BZ_k%nibz
 if (add_X_grid) BZ_RIM_tot_nkpts=BZ_RIM_tot_nkpts+Xk%nbz
 !
 ! Xk (IBZ->BZ)
 !
 call k_ibz2bz(Xk,'i',.true.)
 !
 allocate(BZ_RIM_nkpt(Xk%nbz))
 BZ_RIM_nkpt=0
 !
 do while(.not.allocated(BZ_RIM_table)) 
   if (maxval(BZ_RIM_nkpt)>0) then
     if (add_X_grid)      allocate(BZ_RIM_table(Xk%nbz,maxval(BZ_RIM_nkpt)+1))
     if (.not.add_X_grid) allocate(BZ_RIM_table(Xk%nbz,maxval(BZ_RIM_nkpt)))
     BZ_RIM_table=0
   endif
   BZ_RIM_nkpt=0
   do i1=1,BZ_k%nibz
     !
     ikbz=k_the_nearest(BZ_k%pt(i1,:),Xk%ptbz(:,:),Xk%nbz)
     BZ_RIM_nkpt(ikbz)=BZ_RIM_nkpt(ikbz)+1
     !
     if (allocated(BZ_RIM_table)) BZ_RIM_table(ikbz,BZ_RIM_nkpt(ikbz))=i1
   enddo
 enddo
 call msg('l','done')
 BZ_RIM_max_filling=maxval(BZ_RIM_nkpt)
 if (add_X_grid) BZ_RIM_max_filling=BZ_RIM_max_filling+1
 call msg('s',':: BZ blocks filling     :',(/minval(BZ_RIM_nkpt),BZ_RIM_max_filling/))
 !
 !  At the end I add to the Xen%E_RIM array the Xen%E energies
 !
 allocate(Xen%E_RIM(BZ_RIM_nbands,BZ_RIM_tot_nkpts,n_spin))
 Xen%E_RIM(:,:BZ_k%nibz,:)=BZ_E%E(:,:,:)
 !
 if (add_X_grid) then
   forall(i1=1:Xk%nbz) BZ_RIM_nkpt(i1)=BZ_RIM_nkpt(i1)+1
   forall(i1=1:Xk%nbz) BZ_RIM_table(i1,BZ_RIM_nkpt(i1))=BZ_k%nibz+i1
   do i1=1,Xk%nbz
     Xen%E_RIM(:,BZ_k%nibz+i1,:)=Xen%E(:BZ_RIM_nbands,Xk%sstar(i1,1),:)+Xen%Efermi(1)
   enddo
 endif
 !
 ! Final I/O
 !
 call io_control(ACTION=OP_WR_CL,SEC=(/1/),COM=NONE,ID=ID)
 i_err=ioE_RIM(Xen,ID)
 !
 ! CLEAN
 !
 deallocate(BZ_RIM_nkpt,BZ_RIM_table,Xen%E_RIM)
 call k_ibz2bz(Xk,'d',.false.) 
 !
 end subroutine
