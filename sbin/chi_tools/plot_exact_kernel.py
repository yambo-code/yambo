#!/usr/bin/env python3
"""
plot_exact_kernel.py -- the figures of an exact_kernel.sh run, from its data files only (no Yambo).
Edit the STYLE block or the plot functions and rerun; nothing is recomputed.

  python plot_exact_kernel.py [prod]                 all figures that have data
  python plot_exact_kernel.py prod --only wscan      one of: dispersion, wscan, static0, basis
  python plot_exact_kernel.py prod --fmt pdf         vector output (default png)
  python plot_exact_kernel.py prod --label WS2       title prefix

Reads (all in prod/):
  xq_dispersion.dat             iq |q| gap BSE_low BSE_bright sc_low sc_bright static_bright Eb_BSE Eb_sc Eb_static
  wscan_q<iq>.dat + r-kxw_q<iq>_*   w, lambda_bright (re, im), lambda_lowest; the BSE exciton and gap of that q
  o-bcp.*_q1_*, o-cas0.*_q1_*   E, Im alpha, Re alpha (BSE and static Casida)
  basis_scan.dat                 NG, in-plane G, all G, rcond, E_A, gap, Eb(BSE), Eb(static), ratio
Writes <name>.<fmt> next to them: dispersion, wscan_q<iq>, static0, basis.
"""
import argparse
import glob
import os

import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

# ---------------- STYLE: change freely ----------------
STYLE = dict(
    bse='#2a78d6',        # BSE
    exact='#eb6834',      # Casida with the exact fxc(q, w)
    static='#1baf7a',     # Casida with fxc(q, 0)
    gap='#555555',        # lowest transition
    grid='#dddddd',
    dpi=200,
    font=10,
    q_unit='A',           # 'A' (1/Angstrom) or 'bohr' (1/bohr) on the dispersion axis
)
BOHR = 0.529177210903
plt.rcParams.update({'font.size': STYLE['font'], 'axes.grid': True, 'grid.color': STYLE['grid'],
                     'axes.axisbelow': True})


def save(fig, p, name, fmt):
    out = os.path.join(p, f'{name}.{fmt}')
    fig.tight_layout()
    fig.savefig(out, dpi=STYLE['dpi'])
    plt.close(fig)
    print('wrote', out)


def report(p, job, iq, key):
    """last value of a [BSE] report line of momentum iq in prod/r-<job>_*"""
    i, v = None, None
    for f in glob.glob(os.path.join(p, f'r-{job}_*')):
        for l in open(f, errors='ignore'):
            if 'BSE momentum index' in l:
                i = int(l.split()[-1])
            if (i is None or i == iq) and key in l:
                v = float(l.split()[-1])
    return v


def dispersion(p, fmt, label):
    f = os.path.join(p, 'xq_dispersion.dat')
    if not os.path.exists(f):
        return
    r = np.loadtxt(f, ndmin=2)
    q = r[:, 1] / (BOHR if STYLE['q_unit'] == 'A' else 1.0)
    xl = '|q| (Å$^{-1}$)' if STYLE['q_unit'] == 'A' else '|q| (bohr$^{-1}$)'
    o = np.argsort(q)
    fig, ax = plt.subplots(1, 2, figsize=(10, 4))
    ax[0].plot(q[o], r[o, 2], '^', color=STYLE['gap'], ms=4, label='lowest transition (gap of q)')
    ax[0].plot(q[o], r[o, 4], 'o', mfc='none', color=STYLE['bse'], ms=8, label='BSE')
    ax[0].plot(q[o], r[o, 6], 'x', color=STYLE['exact'], ms=6, label='TDDFT, exact $f_{xc}(q,\\omega)$')
    ax[0].plot(q[o], r[o, 7], 's', mfc='none', color=STYLE['static'], ms=4, label='TDDFT, $f_{xc}(q,0)$')
    ax[0].set_ylabel('energy (eV)')
    ax[1].plot(q[o], r[o, 8], 'o', mfc='none', color=STYLE['bse'], ms=8, label='BSE')
    ax[1].plot(q[o], r[o, 9], 'x', color=STYLE['exact'], ms=6, label='TDDFT, exact $f_{xc}(q,\\omega)$')
    ax[1].plot(q[o], r[o, 10], 's', mfc='none', color=STYLE['static'], ms=4, label='TDDFT, $f_{xc}(q,0)$')
    ax[1].axhline(0, color='k', lw=0.6)
    ax[1].set_ylabel('binding energy, gap(q) − E(q) (eV)')
    for a in ax:
        a.set_xlabel(xl)
        a.legend(fontsize=8)
    if label:
        fig.suptitle(f'{label}: exciton dispersion')
    save(fig, p, 'dispersion', fmt)


def wscan(p, fmt, label):
    for f in sorted(glob.glob(os.path.join(p, 'wscan_q*.dat'))):
        iq = int(os.path.basename(f)[7:-4])
        x = np.loadtxt(f, ndmin=2)
        w, lb = x[:, 0], x[:, 1]
        ex = report(p, f'kxw_q{iq}', iq, 'lowest bright excitation (strength')
        gap = report(p, f'kxw_q{iq}', iq, 'lowest transition energy of the window')
        fig, ax = plt.subplots(figsize=(6.4, 4.4))
        lo = min(lb.min(), ex or lb.min()) - 0.15
        hi = (gap or lb.max()) + 0.15
        ok = (lb > lo) & (lb < hi)
        ax.plot(w[ok], lb[ok], 'o', ms=3, color=STYLE['bse'], label='lowest bright eigenvalue, $f_{xc}$ frozen at ω')
        ax.plot(w, w, '-', color='#888888', lw=1, label='λ = ω')
        if ex:
            ax.axhline(ex, color=STYLE['exact'], lw=1.2, ls='--', label=f'BSE exciton {ex:.3f} eV')
        if gap:
            ax.axhline(gap, color=STYLE['static'], lw=1.2, ls=':', label=f'gap {gap:.3f} eV')
        ax.set_xlim(0, w.max())
        ax.set_ylim(lo, hi)
        ax.set_xlabel('ω at which $f_{xc}(q,\\omega)$ is frozen (eV)')
        ax.set_ylabel('static TDDFT eigenvalue λ (eV)')
        ax.set_title(f'{label + ": " if label else ""}q{iq}, exact kernel frozen at each ω')
        ax.legend(fontsize=8, loc='lower left')
        save(fig, p, f'wscan_q{iq}', fmt)


def static0(p, fmt, label):
    fb = sorted(glob.glob(os.path.join(p, 'o-bcp.*_q1_*')))
    fc = sorted(glob.glob(os.path.join(p, 'o-cas0.*_q1_*')))
    if not fb or not fc:
        return
    b, c = np.loadtxt(fb[0])[:, :3], np.loadtxt(fc[0])[:, :3]
    fig, ax = plt.subplots(1, 2, figsize=(10, 3.8))
    ax[0].plot(b[:, 0], b[:, 1], color=STYLE['bse'], label='BSE')
    ax[0].plot(c[:, 0], c[:, 1], color=STYLE['static'], label='TDDFT, $f_{xc}(\\omega=0)$')
    ax[0].set_ylabel('Im α (q → 0)')
    ax[1].plot(b[:, 0], b[:, 2], color=STYLE['bse'], label='BSE')
    ax[1].plot(c[:, 0], c[:, 2], '--', color=STYLE['static'], label='TDDFT, $f_{xc}(\\omega=0)$')
    ax[1].set_ylabel('Re α (q → 0)')
    ax[1].annotate(f'ω = 0: {b[0, 2]:.4f} vs {c[0, 2]:.4f}', (0.03, 0.9), xycoords='axes fraction', fontsize=8)
    for a in ax:
        a.set_xlabel('ω (eV)')
        a.legend(fontsize=8)
    if label:
        fig.suptitle(f'{label}: static response, BSE vs $f_{{xc}}(0)$')
    save(fig, p, 'static0', fmt)


def basis(p, fmt, label):
    f = os.path.join(p, 'basis_scan.dat')
    if not os.path.exists(f):
        return
    rows = []
    for l in open(f):
        x = l.split()
        if l.startswith('#') or len(x) < 9:
            continue
        rows.append([float(v) for v in x[:8]])
    if not rows:
        return
    r = np.array(rows)
    fig, ax = plt.subplots(figsize=(5.6, 4))
    ax.plot(r[:, 1], 100 * r[:, 7] / r[:, 6], 'o-', color=STYLE['static'])
    lab = {}                     # bases with the same in-plane G share one label
    for ng, ns, s in zip(r[:, 0], r[:, 1], 100 * r[:, 7] / r[:, 6]):
        lab.setdefault((ns, round(s, 3)), []).append(f'{ng:g}')
    for (ns, s), ngs in lab.items():
        ax.annotate(', '.join(ngs) + ' Ry', (ns, s), textcoords='offset points', xytext=(4, 4), fontsize=8)
    ax.set_xlabel('in-plane plane waves in the kernel basis')
    ax.set_ylabel('binding from $f_{xc}(\\omega=0)$ (% of BSE)')
    ax.set_ylim(0, None)
    ax.set_title(f'{label + ": " if label else ""}static kernel vs basis size')
    save(fig, p, 'basis', fmt)


if __name__ == '__main__':
    ap = argparse.ArgumentParser(description=__doc__.split('\n')[1])
    ap.add_argument('prod', nargs='?', default='prod')
    ap.add_argument('--only', choices=['dispersion', 'wscan', 'static0', 'basis'])
    ap.add_argument('--fmt', default='png')
    ap.add_argument('--label', default='')
    a = ap.parse_args()
    for name, fn in (('dispersion', dispersion), ('wscan', wscan), ('static0', static0), ('basis', basis)):
        if a.only in (None, name):
            fn(a.prod, a.fmt, a.label)
