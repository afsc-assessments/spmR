# incomplete embedded metadata cannot fall back to JSON

    Code
      validate_spm_inputs(folder)
    Condition
      Error:
      ! pm.prj: expected one complete embedded metadata block.

# inline metadata uses native token semantics

    Code
      validate_spm_inputs(folder)
    Condition
      Error:
      ! pm.prj: invalid metadata ages; expected finite integer values in [0, Inf].

# inline header codes require native integer spelling

    Code
      validate_spm_inputs(folder)
    Condition
      Error:
      ! pm.prj: embedded metadata header codes must use integer tokens.

# a failed inline replacement preserves the source bytes

    Code
      write_spm_metadata(folder, inline_species_record())
    Condition
      Error:
      ! Could not replace species input: pm.prj

# a later inline replacement failure restores earlier stocks

    Code
      spm_write_inline_input(inputs, folder)
    Condition
      Error:
      ! Could not replace species input: second.prj
