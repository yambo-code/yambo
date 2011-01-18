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
subroutine ypp_init_load(defs)
 !
 use it_m,          ONLY:it,initdefs,E_unit,G_unit,V_general,&
&                        Time_unit,Bfield_unit,Efield_unit,V_real_time
 use wave_func,     ONLY:wf_ng
 use YPP
#if defined _YPP_RAS
 use bulkeps,              ONLY: init_bulk
 use convolute,            ONLY: init_convolute
 use eels_kinematics,      ONLY: init_kinematics
 use model_loss_function,  ONLY: init_loss_function
 use surface_geometry,     ONLY: init_surface
 use eels_detector,        ONLY: init_detector
#endif
#if defined _YPP_BOLTZMANN
 use electrons,            ONLY: nel,Boltz_Efield,Boltz_tau
 use D_lattice,            ONLY: Tel
 use it_m,                 ONLY: T_unit
#endif
 implicit none
 type(initdefs)::defs
 !
 ! RunLevels
 !
 call it('r',defs,'bzgrids',   '[R] BZ Grid') 
 call it('r',defs,'K_grid',    '[R] K-grid analysis') 
 call it('r',defs,'Q_grid',    '[R] Q-grid analysis') 
 call it('r',defs,'dos',       '[R] DOS')
 !
 ! GPL_EXCLUDE_START
 !
 call it('r',defs,'Long_Gauge','[R] Longitudinal gauge grids') 
 !
 ! GPL_EXCLUDE_END
 !
 call it('r',defs,'High_Symm', '[R] High_Symmetry points and paths') 
 call it('r',defs,'excitons',  '[R] Excitons')
 call it('r',defs,'electrons', '[R] Electrons (and holes)')
 call it('r',defs,'freehole',  '[R] Free hole position')
 call it('r',defs,'amplitude', '[R] Amplitude')
 call it('r',defs,'plot',         '[R] Plot')
 call it('r',defs,'density',      '[R] Density')
 call it('r',defs,'wavefunction', '[R] Wavefunction')
#if defined _YPP_ELPH
 call it('r',defs,'eliashberg',   '[R] Eliashberg')
 call it('r',defs,'phonons',      '[R] Phonons')
 call it('r',defs,'gkkp',         '[R] Electron-Phonon databases')
#endif
#if defined _YPP_BOLTZMANN
 call it('r',defs,'current',      '[R] Current')
#endif
#if defined _YPP_MAGNETIC
 call it('r',defs,'angular_m',    '[R] Canonical angular momentum')
 call it('r',defs,'position',     '[R] Average <r^n>, n given in the input')
#endif
 call QP_ctl_load(defs,3)
 !
 ! GPL_EXCLUDE_START SPIN_IGNORE
 !
 call it('r',defs,'spin','[R] Spin')
 call it('r',defs,'magnetization','[R] Magnetization')
 !
 ! GPL_EXCLUDE_END SPIN_IGNORE
 ! GPL_EXCLUDE_START 
 !
 call it('r',defs,'bzrim',   '[R] BZ energy RIM analyzer')
#if defined _YPP_RT
 call it('r',defs,'fixsyms', '[R] Reduce Symmetries')
 call it('r',defs,'RToptics','[R] Real-Time Optics')
 call it('r',defs,'RTnlo'   ,'[R] Real-Time Non-Linear Optics')
 call it('r',defs,'rtpp',    '[R] Real-Time Post-Processing')
#endif
 call it('r',defs,'surf',    '[R] Surface spectroscopy postprocessor')
 call it('r',defs,'ras',     '[R] RAS spectra generator')
 call it('r',defs,'reels',   '[R] REELS spectra generator')
 call it('r',defs,'loc',     '[R] State localizer')
 call it('r',defs,'trans',   '[R] Transition analyzer')
 !
 ! GPL_EXCLUDE_END
 !
 ! DOS
 !
 call it(defs,'DOS_broad', 'Broadening of the DOS',dos_broadening,E_unit) 
 call it(defs,'DOS_bands', 'DOS bands',dos_bands) 
 !
 ! BZ grids
 !
 call it(defs,'cooIn','Points coordinates (in) cc/rlu/iku/alat',coo_in)
 call it(defs,'cooOut','Points coordinates (out) cc/rlu/iku/alat',coo_out)
 call it(defs,'GWKpts','GW K-point grid',Kgw_1)
 call it(defs,'Qpts',  'Q-point grid',   Kgw_1)
 call it(defs,'KShift1','K-grid shift vector[1] ',qlong(1,:))
 call it(defs,'KShift2','K-grid shift vector[2] ',qlong(2,:))
 call it(defs,'KShift3','K-grid shift vector[3] ',qlong(3,:))
 !
 ! GPL_EXCLUDE_START 
 !
 call it(defs,'PtsPath','Series of Points for path generation',PtsPath)
 call it(defs,'NPtsPath','Number of Points for each segment of the path',N_path_pts)
 !
 ! GPL_EXCLUDE_END
 !
 call it('f',defs,'NoWeights',    ' Do not print K-points weight') 
 call it('f',defs,'ForceUserPts', ' Do not check the correcteness of the user points') 
 !
 ! EXC WF
 !
 call it(defs,'FFTGvecs','[FFT] Plane-waves',wf_ng,G_unit)
 call it(defs,'Degen_Step', 'Maximum energy separation of two degenerate states',&
&                           deg_energy,E_unit)
 call it(defs,'States','Index of the BS state(s)',state_ctl)
 call it(defs,'Cells', 'Number of cell repetitions (even or 1)',ncell)
 call it(defs,'Hole', '[cc] Hole position in unit cell',r_hole)
 call it(defs,'WFMult', 'Multiplication factor to the excitonic wavefunction',WF_multiplier,verb_level=V_general)
 !
#if defined _YPP_BOLTZMANN
 ! CURRENT & BOLTZMANN
 !
 call it(defs,'ElecTemp','Electronic Temperature',Tel,T_unit)
 call it(defs,'Nelectro','Electrons number',nel)
 call it(defs,'Tau','Electronic scattering-free time',Boltz_tau,unit=Time_unit)
 call it(defs,'El_Field','Electric field',Boltz_Efield,unit=Efield_unit)
#endif
#if defined _YPP_MAGNETIC
 call it('f',defs,'PhaseTrick','[MAG] Phase trick for a better diagonalization')
#endif
 !
 ! PLOT
 !
 ! p_dir: plot cut in the a1,a2,a3 basis
 ! 
 ! p_format: (c)ube/(g)nuplot/(x)crysden
 !
 call it(defs,'Format', 'Output format [(c)ube/(g)nuplot/(x)crysden]',p_format)
 call it(defs,'Direction', '[rlu] [1/2/3] for 1d or [12/13/23] for 2d [123] for 3D',p_dir)
 !
 ! GPL_EXCLUDE_START SPIN_IGNORE
 !
 call it(defs,'MagDir', 'Magnetization component',mag_dir)
 !
 ! GPL_EXCLUDE_END SPIN_IGNORE
 !
#if defined _YPP_ELPH
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
#if defined _YPP_RAS
 !
 ! RAS
 !
 call it(defs,'XData',     'Filename of q[x] data',xdata)
 call it(defs,'YData',     'Filename of q[y] data',ydata)
 call it(defs,'ZData',     'Filename of q[z] data',zdata)
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
 call it(defs,'LbndRnge',  'Loc bands range',loc_bands)
 call it(defs,'KptRange',  'Loc kpts range',loc_kpts)
 call it('f',defs,'WrtLocDB','Create localization database')
 !
 call it(defs,'EcvMin',  'Lower limit of transition window',Ecv_min,E_unit)
 call it(defs,'EcvMax',  'Upper limit of transition window',Ecv_max,E_unit)
 call it(defs,'PolDir',  'Index of polarization direction',idir)
 call it(defs,'qdir'  ,  '[cc] X-polarization direction', qdir )
 call init_kinematics(defs) 
 call init_loss_function(defs)
 call init_surface(defs)
 call init_detector(defs)
 !
#endif
 !
 ! GPL_EXCLUDE_START
 !
 !BZ E RIM
 !
 call it(defs,'BZ_RIM_Nk','BZ Random Integration points',BZ_RIM_nkpts)
 call it(defs,'BZ_RIM_path','Path to the SAVE folder with the RIM DB1',BZ_RIM_path)
 call it(defs,'GammaRadius','[a.u.] Maximum distance from gamma',gamma_radius,verb_level=V_general)
 call it(defs,'OutputAlat','[a.u.] Lattice constant used for "alat" ouput format',alat_used_for_output,verb_level=V_general)
 !
#if defined _YPP_RT
 !
 call it(defs,'Efield1','First external Electric Field',E_field(1,:))
 call it(defs,'Efield2','Additional external Electric Field',E_field(2,:))
 call it(defs,'BField',    '[MAG] Magnetic field modulus',B_field,unit=Bfield_unit)
 call it(defs,'Bpsi',      '[MAG] Magnetic field psi angle [degree]',B_psi,unit='deg')
 call it(defs,'Btheta',    '[MAG] Magnetic field theta angle [degree]',B_theta,unit='deg')
 call it('f',defs,'RmAllSymm','Remove all symmetries') 
 call it('f',defs,'RmTimeRev','Remove Time Reversal') 
 !
 call it(defs,'EnRngeRt','Energy range',X_rt%energy_range,unit=E_unit)
 call it(defs,'ETStpsRt','Total Energy steps',X_rt%energy_steps)
 call it(defs,'TmRngeRt','Time-window where processing is done',X_rt%time_range,unit=Time_unit,verb_level=V_real_time)
 call it(defs,'DampMode','Damping type ( NONE | LORENTZIAN | GAUSSIAN )',X_rt%damp,verb_level=V_real_time)
 call it(defs,'DampFactor','Damping parameter',X_rt%damp_factor,unit=E_unit,verb_level=V_real_time) 
 !
 call it('f',defs,'UseFFT','Use Fast Fourier transform with EnRange taken from dynamics',verb_level=V_real_time)
 !
#endif
 !
 ! GPL_EXCLUDE_END
 !
end subroutine
