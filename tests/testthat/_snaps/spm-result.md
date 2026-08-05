# as_spm_result validates the common schema

    Code
      spmR::as_spm_result(list())
    Condition
      Error:
      ! `x` must be a data frame.

---

    Code
      spmR::as_spm_result(missing_metric)
    Condition
      Error:
      ! Missing required result columns: OFL.

---

    Code
      spmR::as_spm_result(duplicate_key)
    Condition
      Error:
      ! Result rows must be unique by Stock, Scenario, Sim, and Year.

---

    Code
      spmR::as_spm_result(character_year)
    Condition
      Error:
      ! Result columns must be numeric: Year.

