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
2) press F5 for `Run & Debug`
3) `Debug Yambo` from the dropdown
4) Set breakpoints by clicking the glutter (left of line numbers)
5) Click run
