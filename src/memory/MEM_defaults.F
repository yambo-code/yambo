!
!        Copyright (C) 2000-2016 the YAMBO team
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
subroutine MEM_defaults( )
 !
 use memory, ONLY:MEM_library
 !
 implicit none
 !
 call MEM_library("add","g_vec","RL")
 call MEM_library("add","minus_G","RL")
 call MEM_library("add","g_rot","RL")
 call MEM_library("add","bare_qpg","RL")
 !
 call MEM_library("add","k%star","KPTS")
 call MEM_library("add","k%sstar","KPTS")
 !
 call MEM_library("add","%f","El.Energies")
 call MEM_library("add","%E","El.Energies")
 call MEM_library("add","%QP_corrected","El.Energies")
 !
 call MEM_library("add","%n_of_elements","PAR.indexes")
 call MEM_library("add","%element_1D","PAR.indexes")
 call MEM_library("add","%element_2D","PAR.indexes")
 call MEM_library("add","%weight_1D","PAR.indexes")
 call MEM_library("add","%first_of_1D","PAR.indexes")
 !
 call MEM_library("add","%Time","TIME_components")
 !
 call MEM_library("add","W%p","FREQS")
 call MEM_library("add","Xw%p","FREQS")
 !
 call MEM_library("add","WF%c","WFs")
 call MEM_library("add","WF%index","WFs")
 call MEM_library("add","WF_out%c","WFs")
 call MEM_library("add","WF_out%index","WFs")
 !
end subroutine
