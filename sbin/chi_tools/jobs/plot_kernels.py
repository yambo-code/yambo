#!/usr/bin/env python3
"""Figures for the kernel comparison (kernel_compare.sh). Run in the folder where it ran:

    python3 plot_kernels.py [--dirs . sh_dump] [--emin 1.6 --emax 3.2]

kernel_spectra.png  absorption at q1 per kernel family, each panel against the BSE reference
kernel_binding.png  A-exciton binding energy and peak height (relative to the BSE) per kernel
kernel_time.png     wall time of each run, from Yambo's [Time-Profile] line in its log

Reads kernel_compare.dat, the o-kc_*.eps_q1_* spectra, and l-kc_* (or LOG/l-kc_*_CPU_1) logs.
The exact kernel fxc(w) spectrum and timings are taken from the xq_dispersion runs (o-xw_q1.eps*,
l-xq_bse*, l-xw_q1*) when found in any of --dirs.
"""
import argparse, glob, os, re
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

# reference palette (validated: categorical slots 1-3, ordinal blue ramp from step 250)
INK, INK2, MUTED, GRID, BASE, SURF = '#0b0b0b', '#52514e', '#898781', '#e1e0d9', '#c3c2b7', '#fcfcfb'
CAT = ['#2a78d6', '#eb6834', '#1baf7a']
RAMP = ['#86b6ef', '#3987e5', '#1c5cab', '#0d366b']
REF = INK2                     # the BSE reference: neutral, thick, under everything

p = argparse.ArgumentParser()
p.add_argument('--dirs', nargs='+', default=['.', 'sh_dump'], help='where to look for files')
p.add_argument('--emin', type=float); p.add_argument('--emax', type=float)
a = p.parse_args()

def find(pattern):
    out = []
    for d in a.dirs:
        out += sorted(glob.glob(os.path.join(d, pattern)), key=os.path.getmtime)
    return out

# --- kernel_compare.dat ---
rows, gap, afit = [], np.nan, np.nan
for l in open(find('kernel_compare.dat')[-1]):
    if l.startswith('#'):
        m = re.search(r'window gap ([0-9.]+)', l); gap = float(m.group(1)) if m else gap
        m = re.search(r'binding \(.*\): +([-0-9.na]+)', l); afit = float(m.group(1)) if m else afit
        continue
    x = l.split()
    if len(x) >= 4:
        rows.append((' '.join(x[:-3]), *map(float, x[-3:])))
row = {r[0]: r for r in rows}
bse = rows[0]                                     # 'BSE (SEX), reference'
EA, EB = bse[1], bse[2]

def spectrum(job):
    fs = [f for f in find(f'o-{job}.eps_q1_*') + find(f'o-{job}.alpha_q1_*')]
    if not fs:
        return None
    d = np.loadtxt(fs[-1])
    return d[:, 0], d[:, 1]                       # E, Im eps (with the kernel)

def peak_height(job):
    s = spectrum(job)
    if s is None:
        return np.nan
    w, y = s
    for i in range(1, len(y) - 1):
        if y[i] > 0.05 * y.max() and y[i] >= y[i - 1] and y[i] >= y[i + 1]:
            return y[i]
    return np.nan

def walltime(job):
    fs = find(f'l-{job}_*') + find(f'LOG/l-{job}_*_CPU_1')
    fs = [f for f in fs if re.search(rf'l-{re.escape(job)}_[a-zA-Z]', os.path.basename(f))]
    t = np.nan
    for f in fs:                                  # the last complete run
        for l in open(f, errors='ignore'):
            m = re.search(r'\[Time-Profile\]: *(.*)', l)
            if m:
                s = 0.0
                for v, u in re.findall(r'([0-9.]+)([hms])', m.group(1)):
                    s += float(v) * {'h': 3600, 'm': 60, 's': 1}[u]
                t = s
    return t

lrc = sorted((float(re.match(r'LRC alpha = ([-0-9.]+)', n).group(1)), n) for n in row if n.startswith('LRC alpha'))
lrc_job = lambda al: f'kc_lrc{next(n for n in [f"{al:g}", f"{al:.4f}", f"{al}"] if spectrum("kc_lrc" + n) is not None)}'

# --- figure 1: spectra by family ---
ip = spectrum('kc_ip')
xw = spectrum('xw_q1')                            # exact kernel fxc(w), Casida at q1 (xq_dispersion)
if xw is not None:                                # it must reproduce the BSE: same system and settings
    w, y = xw; sel = (w > EA - 0.3) & (w < EA + 0.3)
    if not sel.any() or abs(w[sel][np.argmax(y[sel])] - EA) > 0.02:
        print(f'o-xw_q1.eps* does not peak at the BSE exciton ({EA:.3f} eV): not used (other run or settings?)')
        xw = None
emin = a.emin or (EA - 0.4); emax = a.emax or (gap + 0.6)

def style(ax, title):
    ax.set_facecolor(SURF)
    for s in ('top', 'right'):
        ax.spines[s].set_visible(False)
    for s in ('left', 'bottom'):
        ax.spines[s].set_color(BASE)
    ax.tick_params(colors=INK2, labelsize=9)
    ax.grid(axis='y', color=GRID, lw=0.6); ax.set_axisbelow(True)
    ax.set_title(title, loc='left', fontsize=10.5, color=INK, pad=6)

def base(ax, top):
    if ip is not None:
        ax.plot(*ip, color=MUTED, lw=1.2, ls=(0, (4, 3)), label='IP (no e-h interaction)', zorder=1)
    w, y = spectrum('kc_sex')
    ax.plot(w, y, color=REF, lw=4, alpha=0.35, solid_capstyle='round', label='BSE (reference)', zorder=2)
    for e, t in ((EA, f'BSE exciton {EA:.3f} eV'), (gap, f'gap {gap:.3f} eV')):
        ax.axvline(e, color=MUTED, lw=0.8, ls=':', zorder=0)
        ax.text(e, top, ' ' + t, fontsize=8, color=INK2, va='top', ha='left')

def ylim_of(curves):
    m = 0
    for w, y in curves:
        sel = (w >= emin) & (w <= emax); m = max(m, y[sel].max() if sel.any() else 0)
    return 1.12 * m

fams = []
f1 = [('exact fxc(w), Casida', xw, CAT[0], (0, (5, 2))) if xw is not None else None,
      ('MDR fxc(w)', spectrum('kc_mdr'), CAT[1], '-'),
      ('MDR fxc(0), static', spectrum('kc_mdrst'), CAT[2], '-'),
      ('NQ fxc(w)', spectrum('kc_nq'), CAT[1], ':'),
      ('NQ fxc(0), static', spectrum('kc_nqst'), CAT[2], ':')]
fams.append(('a  Frequency-dependent kernels from the BSE (exact, MDR, NQ) and their static limit', [c for c in f1 if c and c[1] is not None]))
pick = []
for al, n in lrc:
    tag = n.split('(')[-1] if '(' in n else ''
    if 'alpha(0)' in n or 'alpha(E_X)' in n:
        pick.append((al, n))
if np.isfinite(afit) and lrc:
    near = min((x for x in lrc if x not in pick and x[0] > 0), key=lambda x: abs(x[0] - afit), default=None)
    if near: pick.append(near)
pick = sorted(set(pick))
f2 = []
for i, (al, n) in enumerate(pick):
    lab = f'LRC  α = {al:g}' + (' (exact α(0))' if 'alpha(0)' in n else ' (exact α(E_X))' if 'alpha(E_X)' in n else
                                f' (≈ fit {afit:.2f})')
    f2.append((lab, spectrum(lrc_job(al)), RAMP[min(len(RAMP) - 1, i + 1)], '-'))
fams.append(('b  Long-range kernel  fxc = −α/q²', [c for c in f2 if c[1] is not None]))
f3 = [('RPA (Hartree)', spectrum('kc_rpa'), CAT[0], '-'), ('PF RBO', spectrum('kc_pfRBO'), CAT[1], '-'),
      ('PF JGM', spectrum('kc_pfJGM'), CAT[2], '-')]
fams.append(('c  Other TDDFT kernels in Yambo (ALDA ≈ RPA; PF EMP, CUR ≈ RBO)', [c for c in f3 if c[1] is not None]))

fig, axs = plt.subplots(len(fams), 1, figsize=(8.2, 3.0 * len(fams)), sharex=True, facecolor=SURF)
for ax, (title, curves) in zip(axs, fams):
    style(ax, title)
    top = ylim_of([spectrum('kc_sex')] + [c[1] for c in curves])
    base(ax, top * 0.98)
    for lab, (w, y), col, ls in curves:
        ax.plot(w, y, color=col, lw=2, ls=ls, label=lab, zorder=3)
    ax.set_xlim(emin, emax); ax.set_ylim(0, top)
    ax.set_ylabel('ε₂ (Im ε)', color=INK2, fontsize=9)
    ax.legend(frameon=False, fontsize=8, loc='upper left', bbox_to_anchor=(0.0, 0.88), labelcolor=INK)
axs[-1].set_xlabel('Energy (eV)', color=INK2)
fig.suptitle('WS₂ in-plane absorption at q₁: TDDFT kernels against the BSE', x=0.06, ha='left', color=INK, fontsize=12)
fig.tight_layout(); fig.savefig('kernel_spectra.png', dpi=180, facecolor=SURF); print('wrote kernel_spectra.png')

# --- figure 2: binding energy and peak height relative to the BSE ---
jobs = {'BSE (SEX), reference': 'kc_sex', 'RPA (Hartree), transition space': 'kc_hart', 'ALDA, transition space': 'kc_alda',
        'MDR kernel fxc(0), static': 'kc_mdrst', 'MDR kernel fxc(w)': 'kc_mdr', 'IP': 'kc_ip', 'RPA, G space': 'kc_rpa',
        'NQ kernel fxc(0), static': 'kc_nqst', 'NQ kernel fxc(w)': 'kc_nq',
        'exact kernel fxc(w), self-consistent': 'xw_q1' if xw is not None else '-'}
order = ['BSE (SEX), reference', 'exact kernel fxc(w), self-consistent', 'MDR kernel fxc(w)', 'NQ kernel fxc(w)',
         'exact kernel fxc(0), static', 'MDR kernel fxc(0), static', 'NQ kernel fxc(0), static'] + [n for _, n in lrc if _ > 0] + \
        ['PF EMP', 'PF RBO', 'PF CUR', 'PF JGM', 'ALDA, transition space', 'RPA (Hartree), transition space', 'RPA, G space', 'IP']
order = [n for n in order if n in row]
H0 = peak_height('kc_sex')
def height(n):
    if n in jobs: return peak_height(jobs[n])
    if n.startswith('LRC'): return peak_height(lrc_job(float(re.match(r'LRC alpha = ([-0-9.]+)', n).group(1))))
    if n.startswith('PF '): return peak_height('kc_pf' + n.split()[1])
    return np.nan
short = lambda n: (n.replace(' kernel', '').replace(', self-consistent', '').replace(', transition space', ' (T space)')
                   .replace(', G space', ' (G space)').replace(', reference', '').replace('LRC alpha = ', 'LRC α = ')
                   .replace('(exact alpha(0))', '(α(0))').replace('(exact alpha(E_X))', '(α(E_X))'))
eb = [row[n][2] for n in order]; hr = [height(n) / H0 for n in order]
yy = np.arange(len(order))[::-1]
fig, (a1, a2) = plt.subplots(1, 2, figsize=(10, 0.36 * len(order) + 1.6), sharey=True, facecolor=SURF,
                             gridspec_kw={'width_ratios': [1.25, 1]})
for ax in (a1, a2):
    style(ax, '')
    ax.grid(axis='y', visible=False); ax.grid(axis='x', color=GRID, lw=0.6)
a1.barh(yy, eb, height=0.62, color=CAT[0], edgecolor=SURF, linewidth=2)
a1.axvline(EB, color=REF, lw=1.2, ls='--'); a1.axvline(0, color=BASE, lw=1)
a1.text(EB, len(order) - 0.35, f'  BSE {EB:.3f} eV', color=INK2, fontsize=8, va='bottom')
for y, v in zip(yy, eb):
    a1.text(v + (0.006 if v >= 0 else -0.006), y, f'{v:.3f}', va='center', ha='left' if v >= 0 else 'right', fontsize=7.5, color=INK2)
lo, hi = min(eb + [0]), max(eb + [EB])
a1.set_xlim(lo - 0.12 * (hi - lo) if lo < 0 else -0.03 * hi, hi + 0.12 * (hi - lo))
a1.set_yticks(yy); a1.set_yticklabels([short(n) for n in order], fontsize=8.5, color=INK)
a1.set_xlabel('A-exciton binding energy (eV)', color=INK2, fontsize=9)
a1.set_title('a  Binding energy', loc='left', fontsize=10.5, color=INK)
ok = [i for i, v in enumerate(hr) if np.isfinite(v) and v > 0]
a2.barh(yy[ok], [hr[i] for i in ok], height=0.62, color=CAT[0], edgecolor=SURF, linewidth=2)
a2.set_xscale('log'); a2.axvline(1, color=REF, lw=1.2, ls='--')
fin_hr = [v for v in hr if np.isfinite(v) and v > 0]
a2.set_xlim(min(fin_hr + [1]) / 2.5, max(fin_hr + [1]) * 3.5)
for i, v in enumerate(hr):
    a2.text(v * 1.08 if np.isfinite(v) and v > 0 else 1.05, yy[i], f'{v:.2f}×' if np.isfinite(v) and v > 0 else 'n/a',
            va='center', fontsize=7.5, color=INK2 if np.isfinite(v) else MUTED)
a2.set_xlabel('peak height ε₂ relative to the BSE (log scale)', color=INK2, fontsize=9)
a2.set_title('b  Oscillator strength of the A peak', loc='left', fontsize=10.5, color=INK)
fig.tight_layout(); fig.savefig('kernel_binding.png', dpi=180, facecolor=SURF); print('wrote kernel_binding.png')

# --- figure 3: wall time per run ---
runs = [('BSE (SEX), reference', 'kc_sex'), ('exact fxc: BSE export, all q (xq_bse)', 'xq_bse'),
        ('exact fxc(w): Casida at q1 (xw_q1)', 'xw_q1'), ('MDR: BSE export (kc_mdrx)', 'kc_mdrx'),
        ('MDR fxc(w): Casida (kc_mdr)', 'kc_mdr'), ('MDR fxc(0): Casida (kc_mdrst)', 'kc_mdrst'),
        ('NQ: BSE export (kc_nqx)', 'kc_nqx'), ('NQ fxc(w): Casida (kc_nq)', 'kc_nq'), ('NQ fxc(0): Casida (kc_nqst)', 'kc_nqst'),
        ('RPA (T space)', 'kc_hart'), ('ALDA (T space)', 'kc_alda'), ('IP (G space)', 'kc_ip'), ('RPA (G space)', 'kc_rpa')] + \
       [(f'LRC α = {al:g}', lrc_job(al)) for al, _ in lrc if al > 0][:1] + [(f'PF {x}', f'kc_pf{x}') for x in ('EMP', 'JGM')]
tt = [(lab, walltime(j)) for lab, j in runs]
tt = [(l, t) for l, t in tt if np.isfinite(t)]
if tt:
    yy = np.arange(len(tt))[::-1]
    fig, ax = plt.subplots(figsize=(8.2, 0.36 * len(tt) + 1.4), facecolor=SURF)
    style(ax, 'Wall time per run (from the Yambo logs)')
    ax.grid(axis='y', visible=False); ax.grid(axis='x', color=GRID, lw=0.6)
    ax.barh(yy, [t for _, t in tt], height=0.62, color=CAT[0], edgecolor=SURF, linewidth=2)
    ax.set_xscale('log')
    fmt = lambda s: f'{s/3600:.1f} h' if s >= 3600 else f'{s/60:.1f} min' if s >= 60 else f'{s:.0f} s'
    for y, (_, t) in zip(yy, tt):
        ax.text(t * 1.08, y, fmt(t), va='center', fontsize=7.5, color=INK2)
    ax.set_yticks(yy); ax.set_yticklabels([l for l, _ in tt], fontsize=8.5, color=INK)
    ax.set_xlabel('seconds (log scale)', color=INK2, fontsize=9)
    fig.tight_layout(); fig.savefig('kernel_time.png', dpi=180, facecolor=SURF); print('wrote kernel_time.png')
else:
    print('no [Time-Profile] lines found in l-kc_* logs: kernel_time.png skipped')
