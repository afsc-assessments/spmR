# Write explicit recruitment and population-weight metadata

Version 2 metadata is embedded in comments in each species input file by
default. An explicitly named JSON file supports older workflows.
Split-sex species files contain \`wt_M\` immediately after \`wt_F\`;
pooled-sex files use \`wt_F\` for both internal sex groups. Recruitment
must be identified as total across sexes or per sex. Population weights
are distinct from fishery and spawning weights. Version-2 projections
use \`TAC_ABC = 1\` and do not read \`tacpar.dat\`. Inputs are validated
before replacing metadata. Work in a copy of the assessment inputs to
retain the original export.

## Usage

``` r
write_spm_metadata(dirname, species, metadata = NULL, strict = TRUE)

validate_spm_inputs(dirname, metadata = NULL, strict = TRUE)
```

## Arguments

- dirname:

  Directory containing \`spm.dat\` and species files.

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

  \`NULL\` embeds metadata when writing and discovers embedded metadata
  when validating. Validation falls back to \`spm_metadata.json\` when
  no species file has embedded metadata. Supply a relative \`.json\`
  filename to explicitly write or read JSON instead. Embedded
  declarations take precedence over an old JSON file during automatic
  discovery.

- strict:

  Logical. If \`TRUE\`, reject unknown metadata fields; otherwise report
  them as warnings. Required fields and scientific checks always apply.

## Value

\`write_spm_metadata()\` invisibly returns the updated species paths or
the explicitly requested JSON path. \`validate_spm_inputs()\` returns an
\`spm_inputs\` list containing parsed inputs, resolved population
weights, unit declarations, and substitutions.
