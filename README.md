README
================
Jim Ianelli
2023-09-15

## Standard Projection Model for Alaska Groundfish

This is the standard projection code for federally managed Alaskan
groundfish species in Tier 3.

spm.tpl is the main code for running these projections Required input
files are spm.dat, tacpar.dat, and a species-specific data input file
with key outputs from an assessment, such as examples/data/ai_spm.dat.

Note: spm.dat combines information from two files used in previous
versions of the projection code (setup.dat and spp_catch.dat)

ADMB version 13.0 or higher is required to compile spm.tpl.

These are the steps to run the projection code: 1. Compile src/spm.tpl
using admb and copy spm.exe to a a folder with spm.dat, tacpar.dat, and
a file containing assessment outputs, such as ai_spm.dat 2. Within
spm.dat specify the location of the assessment input file relative to
the folder with spm.exe 3. From the command line pointed to the folder
with these inputs, type “spm”
