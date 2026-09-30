# missing recruitment basis preserves existing metadata

    Code
      write_spm_metadata(folder, metadata)
    Condition
      Error:
      ! metadata species 1 (pm.prj): missing recruitment_basis.

# split-sex male population weights must be supplied or declared

    Code
      write_spm_metadata(folder, metadata, strict = FALSE)
    Condition
      Error:
      ! metadata species 1 (pm.prj): missing male population weight-at-age vector; supply it or explicitly declare male_population_substitute.

# female population weights are required

    Code
      write_spm_metadata(folder, metadata)
    Condition
      Error:
      ! metadata species 1 (pm.prj) population_weights: missing female.

# declared male substitute is reported and recorded

    Code
      write_spm_metadata(folder, metadata)
    Condition
      Warning:
      metadata species 1 (pm.prj): male population weights use declared mean_male_fishery.

# units are explicit and coherent

    Code
      write_spm_metadata(folder, metadata)
    Condition
      Error:
      ! metadata species 1 (pm.prj): coherent units required: fish/kg/kg, thousand_fish/kg/t, or million_fish/kg/thousand_t.

---

    Code
      write_spm_metadata(folder, metadata)
    Condition
      Error:
      ! metadata species 1 (pm.prj): coherent units required: fish/kg/kg, thousand_fish/kg/t, or million_fish/kg/thousand_t.

# population age vectors are ordered and dimensioned

    Code
      write_spm_metadata(folder, metadata)
    Condition
      Error:
      ! metadata species 1 (pm.prj): ages must be consecutive and in increasing order.

---

    Code
      write_spm_metadata(folder, metadata)
    Condition
      Error:
      ! metadata species 1 (pm.prj) male population weights: ages must exactly match the declared age order.

---

    Code
      write_spm_metadata(folder, metadata)
    Condition
      Error:
      ! metadata species 1 (pm.prj) male population weights: require exactly 15 finite positive numeric values.

# population weights are finite and positive

    Code
      write_spm_metadata(folder, metadata)
    Condition
      Error:
      ! metadata species 1 (pm.prj) male population weights: require exactly 15 finite positive numeric values.

---

    Code
      write_spm_metadata(folder, metadata)
    Condition
      Error:
      ! metadata species 1 (pm.prj) male population weights: require exactly 15 finite positive numeric values.

---

    Code
      write_spm_metadata(folder, metadata)
    Condition
      Error:
      ! metadata species 1 (pm.prj) male population weights: require exactly 15 finite positive numeric values.

---

    Code
      write_spm_metadata(folder, metadata)
    Condition
      Error:
      ! metadata species 1 (pm.prj) male population weights: require exactly 15 finite positive numeric values.

# unknown formats and fields are visible

    Code
      validate_spm_inputs(folder)
    Condition
      Error:
      ! Unsupported metadata format_version; expected 2.

---

    Code
      write_spm_metadata(folder, metadata)
    Condition
      Error:
      ! metadata species 1 (pm.prj): unknown fields: mistyped_field.

# positional parsing rejects trailing fields and wrong recruitment history

    Code
      write_spm_metadata(folder, spm_fixture_metadata())
    Condition
      Error:
      ! pm.prj: unexpected trailing fields (3 tokens). Check the positional input format.

---

    Code
      write_spm_metadata(folder, spm_fixture_metadata())
    Condition
      Error:
      ! pm.prj: invalid R; expected finite numeric values in [2.22044604925031e-16, Inf].

# catch years and population scalars are checked

    Code
      write_spm_metadata(folder, spm_fixture_metadata())
    Condition
      Error:
      ! spm.dat: fixed catch years must start at styr and be consecutive.

---

    Code
      write_spm_metadata(folder, spm_fixture_metadata())
    Condition
      Error:
      ! spm.dat: invalid N_scalar; expected finite numeric values in [1, 1].

# legacy executable probe cannot clobber previous projection output

    Code
      runSPM(folder, run = TRUE)
    Condition
      Error:
      ! SPM executable lacks input-format-2 support. Compile the package's current inst/admb/spm.tpl; legacy binaries cannot consume explicit recruitment and population-weight metadata.

# fresh execution rejects stale CSV despite success-like stdout

    Code
      runSPM(folder, run = TRUE)
    Condition
      Error:
      ! SPM produced no fresh spm_detail.csv; existing results have been preserved.

# experimental RTMB adapter refuses scientific execution

    Code
      runSPM(folder, run = TRUE, engine = "rtmb")
    Condition
      Error:
      ! The experimental RTMB adapter does not implement validated version-2 scientific projections. Use engine='admb' with a version-2 executable.

# metadata and species filenames protect reserved files

    Code
      write_spm_metadata(folder, spm_fixture_metadata(), metadata = "spm.dat")
    Condition
      Error:
      ! metadata must use a distinct .json filename reserved for input metadata.

---

    Code
      write_spm_metadata(folder, spm_fixture_metadata())
    Condition
      Error:
      ! spm.dat: species files must have distinct, unreserved filenames.

# pooled sexes share declared population weights with an explicit record

    Code
      write_spm_metadata(folder, metadata)
    Condition
      Warning:
      metadata species 1 (pm.prj): male population weights use declared female_population (common population weights for pooled sexes).

# failed retry preserves the previous successful provenance

    Code
      runSPM(folder, run = TRUE)
    Condition
      Error:
      ! Missing spm_metadata.json: declare recruitment basis, units, and female/male population weights with write_spm_metadata() before running projections.

# stock recruitment fits require a small gradient and positive Hessian

    Code
      spm_verify_convergence(folder, 2)
    Condition
      Error:
      ! Stock-recruitment fit requires a finite maximum gradient below 1e-4.

---

    Code
      spm_verify_convergence(folder, 2)
    Condition
      Error:
      ! Stock-recruitment Hessian must be symmetric and positive definite.

# receipt uses the native zero-variation cutoff

    Code
      spm_verify_receipt(path, inputs)
    Condition
      Error:
      ! SPM receipt disagrees with declared inputs for pm.prj.

# duplicate projection keys are rejected before publication

    Code
      spm_verify_fresh_output(folder, inputs, 0L)
    Condition
      Error:
      ! Result rows must be unique by Stock, Scenario, Sim, and Year.

# a publication copy failure restores every previous output

    Code
      spm_publish_outputs(stage, destination, c("first.csv", "second.csv"), list())
    Condition
      Error:
      ! Could not publish validated output: second.csv; previous outputs and their successful provenance were restored.

# a provenance failure rolls back output publication

    Code
      spm_publish_outputs(stage, destination, "spm_detail.csv", list())
    Condition
      Error:
      ! Provenance save failed.; previous outputs and their successful provenance were restored.

# constant buffer flag is an integer

    Code
      write_spm_metadata(folder, spm_fixture_metadata())
    Condition
      Error:
      ! pm.prj: invalid Const_Buffer; expected finite integer values in [0, 1].

# species share abundance and biomass units for aggregate limits

    Code
      write_spm_metadata(folder, metadata)
    Condition
      Error:
      ! All species in a projection run must use the same abundance and biomass units because catches and biomass are aggregated against shared OY limits.
