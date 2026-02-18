# Scripts

Non-package helper scripts.

## rtmb-smoke.R

Runs the RTMB path on the `examples/atka` dataset and compares output to the
existing ADMB `spm_detail.csv`.

```{r}
Rscript scripts/rtmb-smoke.R
```

If you want a different example directory, edit `example_dir` in the script.
