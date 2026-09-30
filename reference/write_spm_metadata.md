# Write explicit recruitment and population-weight metadata

Version 2 metadata accompanies the unchanged positional SPM input files.
Recruitment must be identified as total across sexes or per sex.
Population weights are distinct from fishery and spawning weights.
Inputs are validated before replacing an existing metadata file.

## Usage

``` r
write_spm_metadata(
  dirname,
  species,
  metadata = "spm_metadata.json",
  strict = TRUE
)

validate_spm_inputs(dirname, metadata = "spm_metadata.json", strict = TRUE)
```

## Arguments

- dirname:

  Directory containing \`spm.dat\`, \`tacpar.dat\`, and species files.

- species:

  A list of species records. Each record contains \`file\`,
  \`recruitment_basis\` (\`"total"\` or \`"per_sex"\`), consecutive
  integer \`ages\`, \`units\`, and \`population_weights\`. Units contain
  \`abundance\` (\`"fish"\`, \`"thousand_fish"\`, or
  \`"million_fish"\`), \`weight\` (\`"kg"\`), and matching \`biomass\`
  (\`"kg"\`, \`"t"\`, or \`"thousand_t"\`). Each population-weight
  record contains \`ages\` and \`values\`. Female population weights are
  required. Split-sex male weights are required unless
  \`male_population_substitute\` explicitly specifies
  \`"female_population"\` or \`"mean_male_fishery"\`. Pooled-sex models
  can use the female population vector for both internal sex groups;
  that assumption is recorded and reported.

- metadata:

  Relative metadata filename. Defaults to \`spm_metadata.json\`.

- strict:

  Logical. If \`TRUE\`, reject unknown metadata fields; otherwise report
  them as warnings. Required fields and scientific checks always apply.

## Value

\`write_spm_metadata()\` invisibly returns the validated metadata path.
\`validate_spm_inputs()\` returns an \`spm_inputs\` list containing
parsed inputs, resolved population weights, unit declarations, and
substitutions.
