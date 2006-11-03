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
 subroutine setup(en,Xen,k,Xk)
!=============================
 use pars,           ONLY:SP,HARTREE,IP,pi,schlen
 use electrons,      ONLY:levels,nel,n_met_bands,n_full_bands,E_duplicate,n_sp_pol
 use memory_m,       ONLY:mem_est
 use D_lattice,      ONLY:a,alat,nsym,i_time_rev,DL_vol,Tel,dl_sop,sop_inv,sop_tab
 use R_lattice,      ONLY:RL_vol,b,n_g_shells,ng_in_shell,nkibz,nkbz,nXkibz,nXkbz,&
&                         k_grid_b,rl_sop,bz_samp
 use wave_func,      ONLY:ng_wf
 use com,            ONLY:msg,gen_fmt,error
 use vec_operate,    ONLY:iku_v_module,internal_product,c2a
 implicit none
 type(levels)    ::en,Xen
 type(bz_samp)::k,Xk
!ws
 real(SP)          :: v1(3),m(3,3)
 integer           :: i1,i2,i3,is
 character(schlen) :: ch(5)

 call section('*','Input variables setup')
!-----------------------------------------
 call msg('r','Unit cell scaling factors [a.u.]:',alat)
 call msg('nr','Direct Lattice(DL) unit cell [iru]')
 call msg('r','A1 =',a(1,:)/alat(:))
 call msg('r','A2 =',a(2,:)/alat(:))
 call msg('rn','A3 =',a(3,:)/alat(:))
 DL_vol=0.
 do i1=1,3
   DL_vol=DL_vol+a(1,i1)*internal_product(a(2,:),a(3,:),i1)
 enddo
 call msg('r','DL volume [au]:',DL_vol)
 RL_vol=(2.*pi)**3./DL_vol

 do i1=1,3
   b(1,i1)=internal_product(a(2,:),a(3,:),i1)*2.*pi/DL_vol
   b(2,i1)=internal_product(a(3,:),a(1,:),i1)*2.*pi/DL_vol
   b(3,i1)=internal_product(a(1,:),a(2,:),i1)*2.*pi/DL_vol
 enddo
 call msg('nr','Reciprocal Lattice(RL) unit cell [iku]')
 call c2a(b,b(1,:),v1,'kc2i')
 call msg('r','B1 =',v1)
 call c2a(b,b(2,:),v1,'kc2i')
 call msg('r','B2 =',v1)
 call c2a(b,b(3,:),v1,'kc2i')
 call msg('rn','B3 =',v1)
! 
! Symmetries and moltiplication table:
!
!  R_i*R_j=R_stab(i,j)
!
 call msg('r','DL (S)ymmetries [cc]')
!------------------------------------
 allocate(rl_sop(3,3,nsym))
 do i1=1,nsym
   forall (i2=1:3,i3=1:3) rl_sop(i2,i3,i1)=dl_sop(i2,i3,i1)*alat(i2)/alat(i3)
   if (i_time_rev==1.and.i1>nsym/2) cycle
   write (ch(1),'(a,i2.2,a)') '[S',i1,']:'
   call msg('r',trim(ch(1)),reshape(dl_sop(:,:,i1),(/9/)))  
 enddo
 select case(i_time_rev)
   case(1)
    call msg('r','Time-reversal derived K-space symmetries:',(/nsym/2+1,nsym/))
   case(0)
    call msg('r','K-space Time-reversal not included')
 end select

 allocate(sop_tab(nsym,nsym),sop_inv(nsym))
 call mem_est("SOP-adds SOPs",(/3*3*nsym,nsym**2+nsym/),(/SP,IP/))
 sop_tab=0
 sop_inv=0
 do i1=1,nsym
   do i2=1,nsym
     m=matmul(dl_sop(:,:,i1),dl_sop(:,:,i2))
     do i3=1,nsym
       if (all(abs(m-dl_sop(:,:,i3))<=1.E-5)) then
         if (sop_tab(i1,i2)/=0) call error('Error (stab 1): check the input symmetries!')        
         sop_tab(i1,i2)=i3
         if (sop_tab(i1,i2)==1) sop_inv(i1)=i2                
       endif
     enddo
     if (sop_tab(i1,i2)==0) call error('Error (stab 2): check the input symmetries!')    
   enddo
 enddo
 if (any(sop_inv==0)) call error('Error (sinv): check the input symmetries!')
 call msg('r','Group table built correctly')

 call gops()
 call msg('nr','G-vector (S)hells. Format: [Snn] G`s')
!------------------------------------------------------
 do i1=n_g_shells,max(n_g_shells-27,1),-5
   ch(1)=' '
   do i2=i1,max(i1-4,1),-1
     write (ch(2),'(a,1x,a,i4.4,a,i6)') trim(ch(1)),'[S',i2,']:',ng_in_shell(i2)
     ch(1)=ch(2)
   enddo
   call msg('r',trim(ch(2)))
 enddo
 call msg('r',' ...')
 do i1=min(12,n_g_shells),1,-6
   ch(1)=' '
   do i2=i1,max(i1-5,1),-1
     write (ch(2),'(a,1x,a,i3.3,a,i4)') trim(ch(1)),'[S',i2,']:',ng_in_shell(i2)
     ch(1)=ch(2)
   enddo
   call msg('r',trim(ch(2)))
 enddo
!
 call section('+','K-grid lattice')
!-------------------------------------------------
 call k_lattice(k,Xk) 

 call section('=','Input (E)nergies[ev] & Occupations')
!------------------------------------------------------
 Xen%nk=Xk%nibz
 call E_duplicate(en,Xen)

 call fermi_level(Xen,Xk,Tel,2)
 call extend_occupations(Xen%Efermi,Xen%nbf,Xen%nbm,en)
!
 n_met_bands=en%nbm
 n_full_bands=en%nbf

 call msg('rn','X BZ K-points :',Xk%nbz)
 do i1=1,k%nibz
   ch(3)=gen_fmt(r_v=k%pt(i1,:))
   ch(4)=gen_fmt((/ng_wf(i1)/))
   ch(5)=gen_fmt(r_v=(/0.01/))

   write (ch(1),'(7a)') '(a,i5.5,a,3(',trim(ch(3)),&
&                       '),1x,a,',trim(ch(4)),',a,',trim(ch(5)),',1x,a)'
   write (ch(2),trim(ch(1))) 'K [',i1,'] :',k%pt(i1,:),'(iku) * G`s',ng_wf(i1)

   if (i1<=Xk%nibz) then
     write (ch(2),trim(ch(1))) '*X* K [',i1,'] :',&
&          k%pt(i1,:),'(iku) * G`s',ng_wf(i1),'* weight ',Xk%weights(i1)
   endif
   call msg('r',trim(ch(2)))

   do is=1,n_sp_pol
     do i2=1,min(en%nb,en%nbf+4*int(nel)),8
       if (n_sp_pol==1) then
         call msg('r','E ',(/(en%E(i3,i1,1)*HARTREE,i3=i2,min(i2+7,en%nb))/))
       else
         if (i2==1) then
           if (is==1) call msg('r','[dn] E ',(/(en%E(i3,i1,1)*HARTREE,i3=i2,min(i2+7,en%nb))/))
           if (is==2) call msg('r','[up] E ',(/(en%E(i3,i1,2)*HARTREE,i3=i2,min(i2+7,en%nb))/))
         else
           if (is==1) call msg('r','     E ',(/(en%E(i3,i1,1)*HARTREE,i3=i2,min(i2+7,en%nb))/))
           if (is==2) call msg('r','     E ',(/(en%E(i3,i1,2)*HARTREE,i3=i2,min(i2+7,en%nb))/))
         endif
       endif
     enddo
   enddo

 enddo
!
 end subroutine

