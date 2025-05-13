# Recommended Extensions
`Modern Fortran`, `CodeLLDB`, `GDB`, `Better Comments`, `fprettify`.

You have to compile fortls via `pipx` or `python pip`. Subsequently, you might have to specify in  `Modern Fortran`'s settings the `Fortran : fortls PATH` (run `which fortls`).

## GDB-based Fortran debugging

Make sure you have `gdb`.

You must compile yambo with debug flags `CFLAGS="-g -O0" FFLAGS="-g -O0"`

You must compile yambo with debug flags `CFLAGS="-g -O0" FFLAGS="-g -O0"`

Enable debugging from VS code with:
- Breakpoints
- Step-by-step execution
- Variable insepction
- GDB support

### How to Use Debugger in VS code
1) Open the yambo folder in VS code
2) Create a folder `debug_run` and copy inside a SAVE folder and the input files you want to test
3) Edit the `.vsocde/launch.json` file accordingly
4) press F5 for `Run & Debug`
5) `Debug Yambo` from the dropdown
6) Open a given file in VS Code, for example `DIPOLE_driver.F` and hover your mouse on a line, then click left of line number to set a red dot (breakpoint). In this way you  set breakpoints by clicking the glutter (left of line numbers).
7) On the debug menu choose your breakpoints.
8) Click run