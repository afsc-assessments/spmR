# Historical GT2026 regression fixture

These files preserve the input values from commit `f23b34f`, with line endings
normalized to LF. `ForProjections.dat` contains 47 recruitment values per sex,
wrapped across two lines per sex, but only 45 SSB values. This deliberate mismatch
tests input auditing and rejection before an attempt directory is created.

Successful-staging tests copy this fixture into temporary directories and append
two explicitly synthetic SSB values (`25000 24000`). Those values exist only for
software tests; they are not assessment inputs or projection results. Tests source
the current example runner while keeping this historical data independent of edits
to the working example.
