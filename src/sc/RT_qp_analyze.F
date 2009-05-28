!
! Copyright (C) 2000-2008  C. Attaccalite and the YAMBO team 
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
!
subroutine RT_qp_analyze(it,en,k,xK,G_input)
 !
 ! Evaluate quasi-particle energies and occupations
 ! and write them in en%E, en%f
 !
 use pars,           ONLY:SP,DP,lchlen
 use units,          ONLY:HARTREE
 use electrons,      ONLY:levels
 use real_time,      ONLY:H_t,DiagH_iter,Occ_natural_orb,Thermal_steps,RT_step
 use QP_m,           ONLY:QP_nk  
 use stderr,         ONLY:set_real_printed_length,intc,real2ch
 use SC,             ONLY:H_nl_sc,SC_bands,E_bare
 use R_lattice,      ONLY:bz_samp
 use D_lattice,      ONLY:Tel
 use units,          ONLY:FS2AUT
 use com,            ONLY:msg
 use matrix_operate, ONLY:mat_dia_inv,DIAGO,USE_LK,INV
 !
 implicit none
 !
 type(levels),  intent(out)  :: en
 integer,       intent(in)   :: it
 complex(SP) ,  intent(in)   :: G_input(SC_bands,SC_bands,QP_nk)
 type(bz_samp), intent(in)   :: k,Xk
 !
 ! Work Space
 !
 integer :: ik,ib,i1
 complex(SP) :: tmp_v(SC_bands)
 character(lchlen)       :: dump_ch
 !
#if defined _DOUBLE
 complex(DP) :: zdotc
#else
 complex(SP) :: cdotc
#endif
 !
 if(.not.mod(it,DiagH_iter)==0.and..not.it==Thermal_steps) return
 !
 call set_real_printed_length()
 !
 do ik=1,QP_nk
   !
   ! Find the natural orbitals
   !
   H_nl_sc(:,:,1)=H_t(:,:,ik,1) 
   call mat_dia_inv(DIAGO,USE_LK,H_nl_sc(:,:,1),E_real=en%E(1:SC_bands,ik,1))
   !
   do ib=1,SC_bands
#if defined _DOUBLE
     call zgemv('N',SC_bands,SC_bands,(0.,-1.),G_input(:,:,ik),SC_bands,H_nl_sc(:,ib,1),1,(0.,0.),tmp_v,1)
     Occ_natural_orb(ib,ik)=real(zdotc(SC_bands,TmpMatrix(:,ib),1,tmp_v,1))
#else 
     call cgemv('N',SC_bands,SC_bands,(0.,-1.),G_input(:,:,ik),SC_bands,H_nl_sc(:,ib,1),1,(0.,0.),tmp_v,1)
     Occ_natural_orb(ib,ik)=real(cdotc(SC_bands,H_nl_sc(:,ib,1),1,tmp_v,1))
#endif   
   enddo
   !
 enddo
 !
 call fermi_level(en,k,Tel,0)
 !
 en%E(:SC_bands,:,:)=en%E(:SC_bands,:,:)-en%Efermi(1)
 !
 ! Now write the occupation
 !
 dump_ch=' Iteration : '//trim(intc(it))//'  Time :'//trim(real2ch(RT_step*real(it)/FS2AUT))
 call msg('o qp','',dump_ch)
 !
 do ik=1,QP_nk
   !      
   dump_ch='K ['//trim(intc(ik))//'] :'//trim(real2ch(k%pt(ik,1)))//' '//&
&          trim(real2ch(k%pt(ik,2)))//' '//trim(real2ch(k%pt(ik,3)))//&
&          ' (iku) '
   call msg('o qp','',dump_ch)
   !
   do i1=1,SC_bands,8
     write(dump_ch,'(a,8f12.6)') 'E  ',(en%E(ib,ik,1)*HARTREE,ib=i1,min(i1+7,SC_bands))
     call msg('o qp','',dump_ch)
   enddo
   do i1=1,SC_bands,8
     write(dump_ch,'(a,8f12.6)') 'O  ',(Occ_natural_orb(ib,ik),ib=i1,min(i1+7,SC_bands))
     call msg('o qp','',dump_ch)
   enddo
 enddo
 !
 if(it==Thermal_steps) then
   do ik=1,QP_nk
     call report_energies(en%E(:SC_bands,:,:),k,Xk,SC_bands,(/ik,ik/),'E  ',.TRUE.)
     call report_energies(E_bare,k,Xk,SC_bands,(/ik,ik/),'E_o  ',.TRUE.)
     call report_energies(en%E(:SC_bands,:,:)-E_bare,k,Xk,SC_bands,(/ik,ik/),'d_E',.FALSE.)
     call report_energies(Occ_natural_orb(:SC_bands,:),k,Xk,SC_bands,(/ik,ik/),'O  ',.FALSE.)
   enddo
 endif
 !
 call set_real_printed_length(f_length=15,g_length=15)
 !
end subroutine RT_qp_analyze
