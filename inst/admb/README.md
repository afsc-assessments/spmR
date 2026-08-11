# SPM source code

The ADMB templates and helper scripts for the Standard Projection Model live
in this installed-data directory because an R package's `src/` directory is
reserved for source code compiled during package installation.

To compile the main model with ADMB 13.0 or newer, copy this directory to a
writable location and run:

```sh
admb -f spm.tpl
```

`Makefile.admb` contains an optional cross-platform wrapper for this command.
