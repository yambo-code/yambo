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
subroutine spp_init_load(defs)
 !
 use it_m,          ONLY:it,initdefs,E_unit
 use SPP
#if defined _SPP_RAS
 use bulkeps,              ONLY : init_bulk
 use convolute,            ONLY : init_convolute
#endif
 implicit none
 type(initdefs)::defs
 !
 !RunLevels
 !
 call it('r',defs,'kpt',  '[R] K-grid analysis')  ! [1]
 call it('r',defs,'excwf','[R] Excitonic Wavefunction plot')  ! [2]
 call it('r',defs,'bzrim','[R] BZ energy RIM analyzer')  ! [3]
 call it('r',defs,'elph', '[R] Electron-Phonon DBs preprocessing')  ! [4]
 call it('r',defs,'surf', '[R] Surface spectroscopy postprocessor')  ! [5]
 call it('r',defs,'ras',  '[R] RAS spectra generator')  ! [6]
 call it('r',defs,'loc',  '[R] State localizer')  ! [6]
 call it('r',defs,'trans','[R] Transition analyzer')  ! [7]
 !
 !Kpt analysis
 !
 call it(defs,'cooIn','Points coordinates (in) cc/rlu/iku',coo_in)
 call it(defs,'cooOut','Points coordinates (out) cc/rlu/iku',coo_out)
 call it(defs,'GWKpts','GW K-point grid',Kgw_1)
 call it(defs,'Qpts',  'Q-point grid',   Kgw_1)
 call it(defs,'KShift1','K-grid shift vector[1] ',qlong(1,:))
 call it(defs,'KShift2','K-grid shift vector[2] ',qlong(2,:))
 call it(defs,'KShift3','K-grid shift vector[3] ',qlong(3,:))
 !
 !EXC WF
 !
 call it(defs,'Action','Sort energies or plot WF [(s)ort/(p)lot]', action_mode)
 call it(defs,'Max_Energy','Max value energy to be shown', max_energy,E_unit)
 call it(defs,'Degen_Step', 'Maximum energy separation of two degenerate states',&
&                           deg_energy,E_unit)
 call it(defs,'Order','Energies order `mode` [(e)nergy/(i)ntensity]', sort_mode)
 call it(defs,'RNorm','Normalize residuals [yes/no]', R_normalize)
 call it(defs,'State','Ordered Index of the BS state',lambda)
 call it(defs,'Cells', 'Number of cell repetitions (even or 1)',ncell)
 call it(defs,'Hole', '[cc] Hole position in unit cell',r_hole)
 !
 ! p_dir: plot cut in the a1,a2,a3 basis
 ! 
 ! p_format: (g)nuplot/(x)crysden
 !
 call it(defs,'Format', 'Output format [(g)nuplot/(x)crysden]',p_format)
 call it(defs,'Direction', '[rlu] [1/2/3] for 1d or [12/13/23] for 2d [123] for 3D',p_dir)
 !
#if defined _SPP_ELPH
 !
 !EL-PH
 !
 call it(defs,'DBsPATH',  'Path to the PW el-ph databases',elph_dbs_path)
 call it(defs,'PHfreqF',  'PWscf format file containing the phonon frequencies',ph_freqs_file)
 call it(defs,'EfGbroad', 'Fermi surface broadening (Gamma)',elph_gamma_broad,E_unit)
 call it(defs,'PhBroad','Phonon broadening (Eliashberg & DOS)',ph_broad,E_unit)
 call it(defs,'EEfermi','External Fermi Level (Eliashberg)',elph_Ef,E_unit)
 call it(defs,'PhStps', 'Energy steps',elph_steps)
 !
#endif
#if defined _SPP_RAS
 !
 ! RAS
 !
 call it(defs,'XData',     'Filename of q[x] data',xdata)
 call it(defs,'YData',     'Filename of q[y] data',ydata)
 call it(defs,'SrfShift',  'Shift of surface spectra',sshift,E_unit)
 call it(defs,'DataType',  'Form of epsilon/alpha data on input [eps/a_t/a_r+23/45]',datatype)
 call it(defs,'d_cell',    'Perpendicular component of cell',d_cellin)
 call init_bulk(defs)
 call init_convolute(defs)
 !
 call it(defs,'NormDir',   'Index of surface normal (1/2/3)',normdir)
 call it(defs,'LowerLim',  'Lower limit of localization box (frac)',lowerlim)
 call it(defs,'UpperLim',  'Upper limit of localization box (frac)',upperlim)
 call it(defs,'NGLoc',     'G-vectors for localization sum',ngloc)
 !
 call it(defs,'EcvMin',  'Lower limit of transition window',Ecv_min,E_unit)
 call it(defs,'EcvMax',  'Upper limit of transition window',Ecv_max,E_unit)
 call it(defs,'PolDir',  'Index of polarization direction',idir)
 !
#endif
 !
 !BZ E RIM
 !
 call it(defs,'BZ_RIM_Nk','BZ Random Integration points',BZ_RIM_nkpts)
 call it(defs,'BZ_RIM_path','Path to the SAVE folder with the RIM DB1',BZ_RIM_path)
 !
end subroutine
