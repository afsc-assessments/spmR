#' Parse SPM input files
#'
#' Reads spm.dat and associated species files into a structured list.
#' @param dirname Directory containing spm.dat and species files.
#' @return A list with spm input components.
#' @export
read_spm_inputs <- function(dirname) {
  spm_path <- file.path(dirname, "spm.dat")
  if (!file.exists(spm_path)) {
    stop("spm.dat not found in ", dirname)
  }
  spm <- dat2list(spm_path)

  if (is.null(spm$spp_file_name)) {
    if (!is.null(spm$datafile)) {
      spm$spp_file_name <- spm$datafile
    } else {
      stop("spm.dat is missing spp_file_name")
    }
  }

  spp_files <- as.character(spm$spp_file_name)
  spp_list <- lapply(spp_files, function(f) {
    fpath <- file.path(dirname, f)
    if (!file.exists(fpath)) {
      stop("Species file not found: ", fpath)
    }
    dat2list(fpath)
  })

  list(spm = spm, spp = spp_list, spp_files = spp_files)
}

#' Run SPM with RTMB (experimental)
#'
#' This is an experimental, non-ADMB path that produces spm_detail.csv-like
#' output for alternatives 1-5 using RTMB-compatible R code. It is not yet a
#' full translation of spm.tpl.
#'
#' @param dirname Directory containing spm.dat and species files.
#' @param run Logical. If TRUE, run the RTMB path and write spm_detail_rtmb.csv.
#' @param seed Random seed for stochastic components.
#' @return A data frame similar to spm_detail.csv.
#' @export
runSPM_rtmb <- function(dirname, run = TRUE, seed = 123) {
  if (!requireNamespace("RTMB", quietly = TRUE)) {
    stop("RTMB is required for the RTMB path. Install RTMB and retry.")
  }

  inputs <- read_spm_inputs(dirname)
  spm <- inputs$spm
  spp <- inputs$spp

  nspp <- as.integer(spm$nspp)
  npro <- as.integer(spm$npro)
  if (length(npro) == 0 || is.na(npro)) npro <- as.integer(spm$nprj_yrs)
  nsims <- as.integer(spm$nsims)
  styr <- as.integer(spm$styr)
  if (length(styr) == 0 || is.na(styr)) styr <- as.integer(spm$beg_yr)
  nyrs_catch <- as.integer(spm$nyrs_catch_in)
  if (length(nyrs_catch) == 0 || is.na(nyrs_catch)) nyrs_catch <- as.integer(spm$nyrs_fixed_catch)

  if (is.na(nspp) || is.na(npro) || is.na(nsims)) {
    stop("spm.dat is missing nspp, npro, or nsims")
  }

  alt_list <- as.integer(spm$alt_list)
  if (length(alt_list) == 0 || all(is.na(alt_list))) {
    alt_list <- as.integer(spm$alts)
  }
  alt_list <- alt_list[alt_list %in% 1:5]
  if (length(alt_list) == 0) {
    alt_list <- 1:5
  }

  set.seed(seed)

  detail_rows <- list()
  row_id <- 1

  invgauss_draws <- function(n, mean_val, cv) {
    if (is.na(mean_val) || mean_val <= 0) {
      return(rep(NA_real_, n))
    }
    if (is.na(cv) || cv <= 0) {
      cv <- 0.2
    }
    gamma <- 1 + 1 / (cv^2)
    delta <- 1 / (gamma - 1)
    beta <- mean_val
    draws <- numeric(n)
    for (i in seq_len(n)) {
      psi <- rnorm(1)^2
      omega <- beta * (1 + (psi - sqrt(4 * delta * psi + psi^2)) / (2 * delta))
      zeta <- beta * (1 + (psi + sqrt(4 * delta * psi + psi^2)) / (2 * delta))
      gtheta <- beta / (beta + omega)
      draws[i] <- if (runif(1) <= gtheta) omega else zeta
    }
    draws
  }

  for (ispp in seq_len(nspp)) {
    spname <- as.character(spp[[ispp]]$spname)
    if (length(spname) == 0) spname <- paste0("spp", ispp)

    Rtmp <- as.numeric(spp[[ispp]]$R)
    SSBtmp <- as.numeric(spp[[ispp]]$SSB)

    mean_rec <- mean(Rtmp, na.rm = TRUE)
    mean_ssb <- mean(SSBtmp, na.rm = TRUE)

    hmean_rec <- 1 / mean(1 / Rtmp, na.rm = TRUE)
    gamma <- mean_rec / hmean_rec
    delta <- 1 / (gamma - 1)
    cvrec <- sqrt(1 / delta)

    b100 <- mean_ssb
    b40 <- 0.4 * b100
    b35 <- 0.35 * b100

    obs_catch <- rep(NA_real_, npro)
    obs <- NULL
    if (!is.null(spm$Obs_Catch)) {
      obs <- spm$Obs_Catch
    } else if (!is.null(spm$fixed_catch)) {
      obs <- spm$fixed_catch
    }
    if (!is.null(obs) && is.matrix(obs) && nrow(obs) >= nyrs_catch) {
      if (ncol(obs) >= (ispp + 1)) {
        obs_catch[seq_len(nyrs_catch)] <- obs[seq_len(nyrs_catch), ispp + 1]
      } else if (ncol(obs) == 2) {
        obs_catch[seq_len(nyrs_catch)] <- obs[seq_len(nyrs_catch), 2]
      }
    }

    rec_sim <- matrix(NA_real_, nrow = nsims, ncol = npro)
    for (isim in seq_len(nsims)) {
      rec_sim[isim, ] <- invgauss_draws(npro, mean_rec, cvrec)
    }

    for (alt in alt_list) {
      for (isim in seq_len(nsims)) {
        for (ipro in seq_len(npro)) {
          year <- styr + ipro - 1

          rec <- rec_sim[isim, ipro]
          ssb <- mean_ssb
          tot_biom <- mean_ssb * 2
          fval <- 0
          n_tot <- NA_real_
          catch <- obs_catch[ipro]
          abc <- catch
          ofl <- catch
          maxabc <- catch

          detail_rows[[row_id]] <- data.frame(
            Stock = spname,
            Alt = alt,
            Sim = isim,
            Year = year,
            SSB = ssb,
            Rec = rec,
            Tot_biom = tot_biom,
            SPR_Implied = NA_real_,
            F = fval,
            Ntot = n_tot,
            Catch = catch,
            ABC = abc,
            OFL = ofl,
            AvgAge = NA_real_,
            AvgAgeTot = NA_real_,
            SexRatio = NA_real_,
            B100 = b100,
            B40 = b40,
            B35 = b35,
            MaxABC = maxabc
          )
          row_id <- row_id + 1
        }
      }
    }
  }

  detail <- do.call(rbind, detail_rows)

  if (run) {
    out_path <- file.path(dirname, "spm_detail_rtmb.csv")
    readr::write_csv(detail, out_path)
  }

  detail
}
