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
subroutine RT_qp_analyze()
 !
!  do ik=1,en%nk
!        !
!        ! Find the natural orbitals
!        !
!        TmpMatrix=H_t(:,:,ik) 
!        call mat_dia_inv(DIAGO,USE_LK,TmpMatrix,E_real=Xen%E(1:SC_bands,ik,1))
!        !
!        do ib=1,SC_bands
!          call cgemv('n',SC_bands,SC_bands,-(0._SP,1._SP),G_lesser(:,:,ik),SC_bands,TmpMatrix(:,ib),1,(0._SP,0._SP),V_tmp1,1)
!#if defined _DOUBLE
!          Occ_Natural(ib,ik)=real(zdotc(SC_bands,TmpMatrix(:,ib),1,V_tmp1,1))
!#else 
!          Occ_Natural(ib,ik)=real(cdotc(SC_bands,TmpMatrix(:,ib),1,V_tmp1,1))
!#endif   
!        enddo
!        !
!      enddo
!      !
!      call fermi_level(Xen,k,Tel,0)
!      !
!      call report_energies(Xen%E(:SC_bands,:,:)-Xen%Efermi(1),k,Xk,SC_bands,(/1,Xen%nk/),'E  ',.FALSE.)
!      !
!      ! Now write the occupation
!      !
!      do ik=1,Xen%nk
!        do i1=1,SC_bands,8 
!          !      
!          call msg('r','n   ',(/(Occ_Natural(ib,ik),ib=i1,min(i1+7,SC_bands))/))
!          !
!        enddo
!      enddo
!      !
!      endif
!    endif
!    !
! enddo           
 !
end subroutine RT_qp_analyze

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
subroutine RT_qp_analyze()
 !
!  do ik=1,en%nk
!        !
!        ! Find the natural orbitals
!        !
!        TmpMatrix=H_t(:,:,ik) 
!        call mat_dia_inv(DIAGO,USE_LK,TmpMatrix,E_real=Xen%E(1:SC_bands,ik,1))
!        !
!        do ib=1,SC_bands
!          call cgemv('n',SC_bands,SC_bands,-(0._SP,1._SP),G_lesser(:,:,ik),SC_bands,TmpMatrix(:,ib),1,(0._SP,0._SP),V_tmp1,1)
!#if defined _DOUBLE
!          Occ_Natural(ib,ik)=real(zdotc(SC_bands,TmpMatrix(:,ib),1,V_tmp1,1))
!#else 
!          Occ_Natural(ib,ik)=real(cdotc(SC_bands,TmpMatrix(:,ib),1,V_tmp1,1))
!#endif   
!        enddo
!        !
!      enddo
!      !
!      call fermi_level(Xen,k,Tel,0)
!      !
!      call report_energies(Xen%E(:SC_bands,:,:)-Xen%Efermi(1),k,Xk,SC_bands,(/1,Xen%nk/),'E  ',.FALSE.)
!      !
!      ! Now write the occupation
!      !
!      do ik=1,Xen%nk
!        do i1=1,SC_bands,8 
!          !      
!          call msg('r','n   ',(/(Occ_Natural(ib,ik),ib=i1,min(i1+7,SC_bands))/))
!          !
!        enddo
!      enddo
!      !
!      endif
!    endif
!    !
! enddo           
 !
end subroutine RT_qp_analyze

