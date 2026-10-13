################################################################################
# FINAL MANUSCRIPT ANALYSIS FOR DS22007 MONTHLY INSECT-ENVIRONMENT TIME SERIES
# SECTIONS 1-29 ARE RUN IN ORDER; SECTION 27 ALWAYS USES THE FULL 999 NULL RUN.
# THE SCRIPT FAILS FAST IF DUPLICATE TIME KEYS OR STRUCTURAL PROBLEMS ARE FOUND.
################################################################################

# ==============================================================================
# 1. PACKAGES
# ==============================================================================
required_packages <- c("dplyr", "tidyr", "purrr", "tibble", "stringr", "readr", "ggplot2", "lmtest", "tseries")  # Executes this documented step of the workflow.
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]  # Executes this documented step of the workflow.
if (length(missing_packages) > 0L) {  # Executes this documented step of the workflow.
  stop("Install the missing packages before running the final analysis: ", paste(missing_packages, collapse = ", "))  # Executes this documented step of the workflow.
}  # Executes this documented step of the workflow.
suppressPackageStartupMessages(library(dplyr))  # Executes this documented step of the workflow.
suppressPackageStartupMessages(library(tidyr))  # Executes this documented step of the workflow.
suppressPackageStartupMessages(library(purrr))  # Executes this documented step of the workflow.
suppressPackageStartupMessages(library(tibble))  # Executes this documented step of the workflow.
suppressPackageStartupMessages(library(stringr))  # Executes this documented step of the workflow.
suppressPackageStartupMessages(library(readr))  # Executes this documented step of the workflow.
suppressPackageStartupMessages(library(ggplot2))  # Executes this documented step of the workflow.
suppressPackageStartupMessages(library(lmtest))  # Executes this documented step of the workflow.
suppressPackageStartupMessages(library(tseries))  # Executes this documented step of the workflow.

# ==============================================================================
# 2. LOAD THE CORRECTED DUPLICATE-FREE MONTHLY OBJECTS
# ==============================================================================
monthly_input_root <- "./final_monthly_inputs_20261005/objects"  # Executes this documented step of the workflow.
monthly_input_files <- c(ALB = file.path(monthly_input_root, "ds22007_monthly_ALB_clean.rds"), HAI = file.path(monthly_input_root, "ds22007_monthly_HAI_clean.rds"), SCH = file.path(monthly_input_root, "ds22007_monthly_SCH_clean.rds"))  # Executes this documented step of the workflow.
if (any(!file.exists(monthly_input_files))) {  # Executes this documented step of the workflow.
  stop("Corrected monthly input objects are missing. Run 2nd_granger_ds22007_monthly_FINAL.R first.")  # Executes this documented step of the workflow.
}  # Executes this documented step of the workflow.
ds22007_monthly_ALB <- readRDS(monthly_input_files[["ALB"]])  # Executes this documented step of the workflow.
ds22007_monthly_HAI <- readRDS(monthly_input_files[["HAI"]])  # Executes this documented step of the workflow.
ds22007_monthly_SCH <- readRDS(monthly_input_files[["SCH"]])  # Executes this documented step of the workflow.

# ==============================================================================
# 3. OUTPUT DIRECTORIES AND ANALYSIS CONSTANTS
# ==============================================================================
out_root <- "./FINAL_MANUSCRIPT_ANALYSIS_20261005"  # Executes this documented step of the workflow.
out_timeseries <- file.path(out_root, "time_series")  # Executes this documented step of the workflow.
out_seasonality <- file.path(out_root, "seasonality")  # Executes this documented step of the workflow.
out_stationarity <- file.path(out_root, "stationarity")  # Executes this documented step of the workflow.
out_tables <- file.path(out_root, "tables")  # Executes this documented step of the workflow.
out_plots <- file.path(out_root, "plots")  # Executes this documented step of the workflow.
out_objects <- file.path(out_root, "objects")  # Executes this documented step of the workflow.
out_diagnostics <- file.path(out_root, "diagnostics")  # Executes this documented step of the workflow.
dir.create(out_timeseries, recursive = TRUE, showWarnings = FALSE)  # Executes this documented step of the workflow.
dir.create(out_seasonality, recursive = TRUE, showWarnings = FALSE)  # Executes this documented step of the workflow.
dir.create(out_stationarity, recursive = TRUE, showWarnings = FALSE)  # Executes this documented step of the workflow.
dir.create(out_tables, recursive = TRUE, showWarnings = FALSE)  # Executes this documented step of the workflow.
dir.create(out_plots, recursive = TRUE, showWarnings = FALSE)  # Executes this documented step of the workflow.
dir.create(out_objects, recursive = TRUE, showWarnings = FALSE)  # Executes this documented step of the workflow.
dir.create(out_diagnostics, recursive = TRUE, showWarnings = FALSE)  # Executes this documented step of the workflow.
seasonality_alpha <- 0.05  # Executes this documented step of the workflow.
stationarity_alpha <- 0.05  # Executes this documented step of the workflow.
lags <- 1:3  # Executes this documented step of the workflow.
analysis_regions <- c("ALB", "HAI", "SCH")  # Executes this documented step of the workflow.

# ==============================================================================
# 4. STRUCTURAL VALIDATION HELPERS
# ==============================================================================
assert_has_columns <- function(df, required, object_name) {  # Executes this documented step of the workflow.
  missing <- setdiff(required, names(df))  # Executes this documented step of the workflow.
  if (length(missing) > 0L) stop(object_name, " is missing required column(s): ", paste(missing, collapse = ", "))  # Executes this documented step of the workflow.
  invisible(TRUE)  # Executes this documented step of the workflow.
}  # Executes this documented step of the workflow.
assert_unique_key <- function(df, key, object_name) {  # Executes this documented step of the workflow.
  assert_has_columns(df, key, object_name)  # Executes this documented step of the workflow.
  duplicates <- df %>% count(across(all_of(key)), name = "n_rows") %>% filter(n_rows > 1L)  # Executes this documented step of the workflow.
  if (nrow(duplicates) > 0L) {  # Executes this documented step of the workflow.
    write.csv(duplicates, file.path(out_diagnostics, paste0("DUPLICATES_", object_name, ".csv")), row.names = FALSE)  # Executes this documented step of the workflow.
    stop(object_name, " contains duplicate analysis keys. The final analysis cannot continue.")  # Executes this documented step of the workflow.
  }  # Executes this documented step of the workflow.
  invisible(TRUE)  # Executes this documented step of the workflow.
}  # Executes this documented step of the workflow.
get_region_list <- function(region_name) {  # Executes this documented step of the workflow.
  if (region_name == "ALB") return(ds22007_monthly_ALB)  # Executes this documented step of the workflow.
  if (region_name == "HAI") return(ds22007_monthly_HAI)  # Executes this documented step of the workflow.
  if (region_name == "SCH") return(ds22007_monthly_SCH)  # Executes this documented step of the workflow.
  stop("Unknown region: ", region_name)  # Executes this documented step of the workflow.
}  # Executes this documented step of the workflow.
set_region_list <- function(region_name, value) {  # Executes this documented step of the workflow.
  if (region_name == "ALB") ds22007_monthly_ALB <<- value  # Executes this documented step of the workflow.
  if (region_name == "HAI") ds22007_monthly_HAI <<- value  # Executes this documented step of the workflow.
  if (region_name == "SCH") ds22007_monthly_SCH <<- value  # Executes this documented step of the workflow.
  invisible(value)  # Executes this documented step of the workflow.
}  # Executes this documented step of the workflow.

# ==============================================================================
# 5. VERIFY THAT EVERY ANALYSIS TIME POINT IS UNIQUE BEFORE ANY STATISTICS
# ==============================================================================
validate_region_keys <- function(region_name, region_list) {  # Executes this documented step of the workflow.
  assert_unique_key(region_list$ds22007, c("Exploratory", "Family", "year", "month"), paste0(region_name, "_insects"))  # Executes this documented step of the workflow.
  for (dataset_name in setdiff(names(region_list), "ds22007")) {  # Executes this documented step of the workflow.
    assert_unique_key(region_list[[dataset_name]], c("year", "month"), paste0(region_name, "_", dataset_name))  # Executes this documented step of the workflow.
  }  # Executes this documented step of the workflow.
  invisible(TRUE)  # Executes this documented step of the workflow.
}  # Executes this documented step of the workflow.
purrr::walk(analysis_regions, ~ validate_region_keys(.x, get_region_list(.x)))  # Executes this documented step of the workflow.

# ==============================================================================
# 6. DEFINE THE ENVIRONMENTAL SERIES THAT ENTER THE ANALYSIS
# ==============================================================================
get_environment_specs <- function(region_name, region_list) {  # Executes this documented step of the workflow.
  weather_cols <- names(region_list$weather)[vapply(region_list$weather, is.numeric, logical(1))]  # Executes this documented step of the workflow.
  weather_cols <- setdiff(weather_cols, c("year", "month"))  # Executes this documented step of the workflow.
  bind_rows(  # Executes this documented step of the workflow.
    tibble(EnvDataset = "SMI_total", EnvColumn = region_name),  # Executes this documented step of the workflow.
    tibble(EnvDataset = "SMI_upsoil", EnvColumn = region_name),  # Executes this documented step of the workflow.
    tibble(EnvDataset = "NDVI", EnvColumn = "mean_NDVI"),  # Executes this documented step of the workflow.
    tibble(EnvDataset = "NDMI", EnvColumn = "mean_NDMI"),  # Executes this documented step of the workflow.
    tibble(EnvDataset = "NIRv", EnvColumn = "mean_NIRv"),  # Executes this documented step of the workflow.
    tibble(EnvDataset = "weather", EnvColumn = weather_cols)  # Executes this documented step of the workflow.
  ) %>% distinct()  # Executes this documented step of the workflow.
}  # Executes this documented step of the workflow.
environment_specs_all <- map_dfr(analysis_regions, function(region_name) {  # Executes this documented step of the workflow.
  get_environment_specs(region_name, get_region_list(region_name)) %>% mutate(region = region_name, .before = 1)  # Executes this documented step of the workflow.
})  # Executes this documented step of the workflow.
write.csv(environment_specs_all, file.path(out_diagnostics, "environmental_series_inventory.csv"), row.names = FALSE)  # Executes this documented step of the workflow.

# ==============================================================================
# 7. SEASONALITY SCREEN
# ==============================================================================
screen_seasonality <- function(x, month, alpha = seasonality_alpha) {  # Executes this documented step of the workflow.
  x <- suppressWarnings(as.numeric(x))  # Executes this documented step of the workflow.
  month <- suppressWarnings(as.integer(month))  # Executes this documented step of the workflow.
  ok <- is.finite(x) & is.finite(month)  # Executes this documented step of the workflow.
  n_complete <- sum(ok)  # Executes this documented step of the workflow.
  n_months <- length(unique(month[ok]))  # Executes this documented step of the workflow.
  if (n_complete < 8L || n_months < 3L || !is.finite(stats::var(x[ok])) || stats::var(x[ok]) == 0) {  # Executes this documented step of the workflow.
    return(tibble(n_complete = n_complete, n_months = n_months, seasonality_p_value = NA_real_, seasonal_r2 = NA_real_, seasonality_status = "not_testable", needs_deseasonalisation = FALSE))  # Executes this documented step of the workflow.
  }  # Executes this documented step of the workflow.
  dat <- tibble(x = x[ok], month = month[ok]) %>% mutate(sin12 = sin(2 * pi * month / 12), cos12 = cos(2 * pi * month / 12))  # Executes this documented step of the workflow.
  null_fit <- tryCatch(stats::lm(x ~ 1, data = dat), error = function(e) NULL)  # Executes this documented step of the workflow.
  seasonal_fit <- tryCatch(stats::lm(x ~ sin12 + cos12, data = dat), error = function(e) NULL)  # Executes this documented step of the workflow.
  if (is.null(null_fit) || is.null(seasonal_fit)) {  # Executes this documented step of the workflow.
    return(tibble(n_complete = n_complete, n_months = n_months, seasonality_p_value = NA_real_, seasonal_r2 = NA_real_, seasonality_status = "not_testable", needs_deseasonalisation = FALSE))  # Executes this documented step of the workflow.
  }  # Executes this documented step of the workflow.
  comparison <- tryCatch(stats::anova(null_fit, seasonal_fit), error = function(e) NULL)  # Executes this documented step of the workflow.
  p_value <- if (is.null(comparison) || nrow(comparison) < 2L) NA_real_ else suppressWarnings(as.numeric(comparison$`Pr(>F)`[2]))  # Executes this documented step of the workflow.
  r2 <- suppressWarnings(summary(seasonal_fit)$r.squared)  # Executes this documented step of the workflow.
  needs <- is.finite(p_value) && p_value < alpha  # Executes this documented step of the workflow.
  tibble(n_complete = n_complete, n_months = n_months, seasonality_p_value = p_value, seasonal_r2 = r2, seasonality_status = case_when(!is.finite(p_value) ~ "not_testable", needs ~ "seasonal", TRUE ~ "not_seasonal"), needs_deseasonalisation = needs)  # Executes this documented step of the workflow.
}  # Executes this documented step of the workflow.
seasonality_insects <- map_dfr(analysis_regions, function(region_name) {  # Executes this documented step of the workflow.
  insect_df <- get_region_list(region_name)$ds22007  # Executes this documented step of the workflow.
  map_dfr(sort(unique(insect_df$Family)), function(family_name) {  # Executes this documented step of the workflow.
    fam_df <- insect_df %>% filter(Family == family_name) %>% arrange(year, month)  # Executes this documented step of the workflow.
    screen_seasonality(fam_df$NumberAdults, fam_df$month) %>% mutate(series_type = "insect", region = region_name, dataset = "ds22007", variable = "NumberAdults", Family = family_name, .before = 1)  # Executes this documented step of the workflow.
  })  # Executes this documented step of the workflow.
})  # Executes this documented step of the workflow.
seasonality_environment <- map_dfr(analysis_regions, function(region_name) {  # Executes this documented step of the workflow.
  region_list <- get_region_list(region_name)  # Executes this documented step of the workflow.
  specs <- get_environment_specs(region_name, region_list)  # Executes this documented step of the workflow.
  map_dfr(seq_len(nrow(specs)), function(i) {  # Executes this documented step of the workflow.
    dataset_name <- specs$EnvDataset[i]  # Executes this documented step of the workflow.
    variable_name <- specs$EnvColumn[i]  # Executes this documented step of the workflow.
    df <- region_list[[dataset_name]] %>% arrange(year, month)  # Executes this documented step of the workflow.
    if (!variable_name %in% names(df)) return(tibble())  # Executes this documented step of the workflow.
    screen_seasonality(df[[variable_name]], df$month) %>% mutate(series_type = "environment", region = region_name, dataset = dataset_name, variable = variable_name, Family = NA_character_, .before = 1)  # Executes this documented step of the workflow.
  })  # Executes this documented step of the workflow.
})  # Executes this documented step of the workflow.
seasonality_screen_all <- bind_rows(seasonality_insects, seasonality_environment) %>% arrange(series_type, region, dataset, Family, variable)  # Executes this documented step of the workflow.
write.csv(seasonality_screen_all, file.path(out_seasonality, "seasonality_screen_all_time_series.csv"), row.names = FALSE)  # Executes this documented step of the workflow.

# ==============================================================================
# 8. CONDITIONAL DESEASONALISATION
# ==============================================================================
deseason_series <- function(x, month) {  # Executes this documented step of the workflow.
  x <- suppressWarnings(as.numeric(x))  # Executes this documented step of the workflow.
  month <- suppressWarnings(as.integer(month))  # Executes this documented step of the workflow.
  out <- rep(NA_real_, length(x))  # Executes this documented step of the workflow.
  ok <- is.finite(x) & is.finite(month)  # Executes this documented step of the workflow.
  if (sum(ok) < 8L || length(unique(month[ok])) < 3L || !is.finite(stats::var(x[ok])) || stats::var(x[ok]) == 0) return(out)  # Executes this documented step of the workflow.
  dat <- tibble(x = x[ok], month = month[ok]) %>% mutate(sin12 = sin(2 * pi * month / 12), cos12 = cos(2 * pi * month / 12))  # Executes this documented step of the workflow.
  fit <- tryCatch(stats::lm(x ~ sin12 + cos12, data = dat), error = function(e) NULL)  # Executes this documented step of the workflow.
  if (is.null(fit)) return(out)  # Executes this documented step of the workflow.
  intercept <- unname(stats::coef(fit)[1])  # Executes this documented step of the workflow.
  seasonal_component <- stats::fitted(fit) - intercept  # Executes this documented step of the workflow.
  out[ok] <- x[ok] - seasonal_component  # Executes this documented step of the workflow.
  out  # Executes this documented step of the workflow.
}  # Executes this documented step of the workflow.

# ==============================================================================
# 9. CALENDAR-AWARE TRANSFORMATION CANDIDATES
# ==============================================================================
calendar_time_id <- function(year, month) {  # Executes this documented step of the workflow.
  as.integer(year) * 12L + as.integer(month)  # Executes this documented step of the workflow.
}  # Executes this documented step of the workflow.
detrend_calendar <- function(x, year, month) {  # Executes this documented step of the workflow.
  x <- suppressWarnings(as.numeric(x))  # Executes this documented step of the workflow.
  time_id <- calendar_time_id(year, month)  # Executes this documented step of the workflow.
  out <- rep(NA_real_, length(x))  # Executes this documented step of the workflow.
  ok <- is.finite(x) & is.finite(time_id)  # Executes this documented step of the workflow.
  if (sum(ok) < 3L || !is.finite(stats::var(x[ok])) || stats::var(x[ok]) == 0) return(out)  # Executes this documented step of the workflow.
  fit <- tryCatch(stats::lm(x[ok] ~ time_id[ok]), error = function(e) NULL)  # Executes this documented step of the workflow.
  if (is.null(fit)) return(out)  # Executes this documented step of the workflow.
  out[ok] <- stats::residuals(fit) + unname(stats::coef(fit)[1])  # Executes this documented step of the workflow.
  out  # Executes this documented step of the workflow.
}  # Executes this documented step of the workflow.
calendar_difference <- function(x, year, month) {  # Executes this documented step of the workflow.
  x <- suppressWarnings(as.numeric(x))  # Executes this documented step of the workflow.
  time_id <- calendar_time_id(year, month)  # Executes this documented step of the workflow.
  out <- rep(NA_real_, length(x))  # Executes this documented step of the workflow.
  valid_index <- which(is.finite(x) & is.finite(time_id))  # Executes this documented step of the workflow.
  if (length(valid_index) < 2L) return(out)  # Executes this documented step of the workflow.
  previous_position <- match(time_id[valid_index] - 1L, time_id)  # Executes this documented step of the workflow.
  has_previous <- !is.na(previous_position) & is.finite(x[previous_position])  # Executes this documented step of the workflow.
  out[valid_index[has_previous]] <- x[valid_index[has_previous]] - x[previous_position[has_previous]]  # Executes this documented step of the workflow.
  out  # Executes this documented step of the workflow.
}  # Executes this documented step of the workflow.

# ==============================================================================
# 10. JOINT ADF/KPSS STATIONARITY SCREEN
# ==============================================================================
screen_stationarity <- function(x, alpha = stationarity_alpha) {  # Executes this documented step of the workflow.
  x <- suppressWarnings(as.numeric(x))  # Executes this documented step of the workflow.
  x <- x[is.finite(x)]  # Executes this documented step of the workflow.
  n_complete <- length(x)  # Executes this documented step of the workflow.
  if (n_complete < 10L || !is.finite(stats::var(x)) || stats::var(x) == 0) {  # Executes this documented step of the workflow.
    return(tibble(n_complete = n_complete, adf_p_value = NA_real_, kpss_p_value = NA_real_, adf_stationary = NA, kpss_stationary = NA, joint_status = "not_testable", pass_stationarity = FALSE))  # Executes this documented step of the workflow.
  }  # Executes this documented step of the workflow.
  adf_p <- tryCatch(suppressWarnings(tseries::adf.test(x)$p.value), error = function(e) NA_real_)  # Executes this documented step of the workflow.
  kpss_p <- tryCatch(suppressWarnings(tseries::kpss.test(x, null = "Level")$p.value), error = function(e) NA_real_)  # Executes this documented step of the workflow.
  adf_stationary <- if (is.finite(adf_p)) adf_p < alpha else NA  # Executes this documented step of the workflow.
  kpss_stationary <- if (is.finite(kpss_p)) kpss_p >= alpha else NA  # Executes this documented step of the workflow.
  joint_status <- case_when(isTRUE(adf_stationary) && isTRUE(kpss_stationary) ~ "stationary", identical(adf_stationary, FALSE) && identical(kpss_stationary, FALSE) ~ "nonstationary", !is.na(adf_stationary) && !is.na(kpss_stationary) ~ "ambiguous", TRUE ~ "not_testable")  # Executes this documented step of the workflow.
  tibble(n_complete = n_complete, adf_p_value = adf_p, kpss_p_value = kpss_p, adf_stationary = adf_stationary, kpss_stationary = kpss_stationary, joint_status = joint_status, pass_stationarity = joint_status == "stationary")  # Executes this documented step of the workflow.
}  # Executes this documented step of the workflow.

# ==============================================================================
# 11. PREDEFINED LEAST-TRANSFORMED STATIONARY-SERIES SELECTION
# ==============================================================================
choose_stationary_candidate <- function(x, year, month, needs_deseasonalisation) {  # Executes this documented step of the workflow.
  season_base <- if (isTRUE(needs_deseasonalisation)) deseason_series(x, month) else suppressWarnings(as.numeric(x))  # Executes this documented step of the workflow.
  candidate_values <- list(level = season_base, detrended = detrend_calendar(season_base, year, month), differenced = calendar_difference(season_base, year, month))  # Executes this documented step of the workflow.
  candidate_values$detrended_differenced <- calendar_difference(candidate_values$detrended, year, month)  # Executes this documented step of the workflow.
  candidate_order <- c("level", "detrended", "differenced", "detrended_differenced")  # Executes this documented step of the workflow.
  diagnostics <- map_dfr(candidate_order, function(candidate_name) {  # Executes this documented step of the workflow.
    screen_stationarity(candidate_values[[candidate_name]]) %>% mutate(transformation = candidate_name, .before = 1)  # Executes this documented step of the workflow.
  })  # Executes this documented step of the workflow.
  passing <- diagnostics %>% filter(pass_stationarity)  # Executes this documented step of the workflow.
  chosen <- if (nrow(passing) == 0L) NA_character_ else candidate_order[candidate_order %in% passing$transformation][1]  # Executes this documented step of the workflow.
  selected_values <- if (is.na(chosen)) rep(NA_real_, length(x)) else candidate_values[[chosen]]  # Executes this documented step of the workflow.
  selected_diagnostics <- if (is.na(chosen)) diagnostics %>% slice(1) %>% mutate(n_complete = sum(is.finite(season_base)), adf_p_value = NA_real_, kpss_p_value = NA_real_, adf_stationary = NA, kpss_stationary = NA, joint_status = "no_stationary_candidate", pass_stationarity = FALSE, transformation = NA_character_) else diagnostics %>% filter(transformation == chosen) %>% slice(1)  # Executes this documented step of the workflow.
  list(values = selected_values, chosen = chosen, diagnostics = diagnostics, selected_diagnostics = selected_diagnostics, was_deseasonalised = isTRUE(needs_deseasonalisation))  # Executes this documented step of the workflow.
}  # Executes this documented step of the workflow.

# ==============================================================================
# 12. BUILD FINAL INSECT SERIES WITHOUT CREATING NEW ROWS
# ==============================================================================
process_insects_region <- function(region_name, region_list) {  # Executes this documented step of the workflow.
  df <- region_list$ds22007 %>% arrange(Family, year, month) %>% mutate(NumberAdults_final = NA_real_)  # Executes this documented step of the workflow.
  selection_rows <- list()  # Executes this documented step of the workflow.
  candidate_rows <- list()  # Executes this documented step of the workflow.
  families <- sort(unique(df$Family))  # Executes this documented step of the workflow.
  for (i in seq_along(families)) {  # Executes this documented step of the workflow.
    family_name <- families[i]  # Executes this documented step of the workflow.
    idx <- which(df$Family == family_name)  # Executes this documented step of the workflow.
    season_flag <- seasonality_screen_all %>% filter(series_type == "insect", region == region_name, Family == family_name) %>% pull(needs_deseasonalisation)  # Executes this documented step of the workflow.
    needs <- length(season_flag) == 1L && isTRUE(season_flag[1])  # Executes this documented step of the workflow.
    result <- choose_stationary_candidate(df$NumberAdults[idx], df$year[idx], df$month[idx], needs)  # Executes this documented step of the workflow.
    df$NumberAdults_final[idx] <- result$values  # Executes this documented step of the workflow.
    selection_rows[[i]] <- result$selected_diagnostics %>% mutate(region = region_name, Family = family_name, analysis_column = "NumberAdults_final", was_deseasonalised = result$was_deseasonalised, selected_transformation = result$chosen, .before = 1)  # Executes this documented step of the workflow.
    candidate_rows[[i]] <- result$diagnostics %>% mutate(region = region_name, Family = family_name, was_deseasonalised = result$was_deseasonalised, .before = 1)  # Executes this documented step of the workflow.
  }  # Executes this documented step of the workflow.
  region_list$ds22007 <- df  # Executes this documented step of the workflow.
  list(region_list = region_list, selected = bind_rows(selection_rows), candidates = bind_rows(candidate_rows))  # Executes this documented step of the workflow.
}  # Executes this documented step of the workflow.
insect_processing <- setNames(map(analysis_regions, function(region_name) process_insects_region(region_name, get_region_list(region_name))), analysis_regions)  # Executes this documented step of the workflow.
walk(analysis_regions, function(region_name) set_region_list(region_name, insect_processing[[region_name]]$region_list))  # Executes this documented step of the workflow.
stationarity_insects_season_checked <- map_dfr(insect_processing, "selected")  # Executes this documented step of the workflow.
stationarity_insect_candidates <- map_dfr(insect_processing, "candidates")  # Executes this documented step of the workflow.

# ==============================================================================
# 13. BUILD FINAL ENVIRONMENTAL SERIES WITHOUT CREATING NEW ROWS
# ==============================================================================
process_environment_region <- function(region_name, region_list) {  # Executes this documented step of the workflow.
  specs <- get_environment_specs(region_name, region_list)  # Executes this documented step of the workflow.
  selection_rows <- list()  # Executes this documented step of the workflow.
  candidate_rows <- list()  # Executes this documented step of the workflow.
  counter <- 0L  # Executes this documented step of the workflow.
  for (dataset_name in unique(specs$EnvDataset)) {  # Executes this documented step of the workflow.
    df <- region_list[[dataset_name]] %>% arrange(year, month)  # Executes this documented step of the workflow.
    variables <- specs %>% filter(EnvDataset == dataset_name) %>% pull(EnvColumn)  # Executes this documented step of the workflow.
    for (variable_name in variables) {  # Executes this documented step of the workflow.
      counter <- counter + 1L  # Executes this documented step of the workflow.
      if (!variable_name %in% names(df)) next  # Executes this documented step of the workflow.
      season_flag <- seasonality_screen_all %>% filter(series_type == "environment", region == region_name, dataset == dataset_name, variable == variable_name) %>% pull(needs_deseasonalisation)  # Executes this documented step of the workflow.
      needs <- length(season_flag) == 1L && isTRUE(season_flag[1])  # Executes this documented step of the workflow.
      result <- choose_stationary_candidate(df[[variable_name]], df$year, df$month, needs)  # Executes this documented step of the workflow.
      final_col <- paste0(variable_name, "_final")  # Executes this documented step of the workflow.
      df[[final_col]] <- result$values  # Executes this documented step of the workflow.
      selection_rows[[counter]] <- result$selected_diagnostics %>% mutate(region = region_name, EnvDataset = dataset_name, EnvColumn = variable_name, analysis_column = final_col, was_deseasonalised = result$was_deseasonalised, selected_transformation = result$chosen, .before = 1)  # Executes this documented step of the workflow.
      candidate_rows[[counter]] <- result$diagnostics %>% mutate(region = region_name, EnvDataset = dataset_name, EnvColumn = variable_name, was_deseasonalised = result$was_deseasonalised, .before = 1)  # Executes this documented step of the workflow.
    }  # Executes this documented step of the workflow.
    region_list[[dataset_name]] <- df  # Executes this documented step of the workflow.
  }  # Executes this documented step of the workflow.
  list(region_list = region_list, selected = bind_rows(selection_rows), candidates = bind_rows(candidate_rows))  # Executes this documented step of the workflow.
}  # Executes this documented step of the workflow.
environment_processing <- setNames(map(analysis_regions, function(region_name) process_environment_region(region_name, get_region_list(region_name))), analysis_regions)  # Executes this documented step of the workflow.
walk(analysis_regions, function(region_name) set_region_list(region_name, environment_processing[[region_name]]$region_list))  # Executes this documented step of the workflow.
stationarity_environment_season_checked <- map_dfr(environment_processing, "selected")  # Executes this documented step of the workflow.
stationarity_environment_candidates <- map_dfr(environment_processing, "candidates")  # Executes this documented step of the workflow.

# ==============================================================================
# 14. CREATE THE FINAL SEASON-CHECKED OBJECT NAMES USED BY SECTIONS 27-29
# ==============================================================================
ds22007_monthly_ALB_season_checked <- ds22007_monthly_ALB  # Executes this documented step of the workflow.
ds22007_monthly_HAI_season_checked <- ds22007_monthly_HAI  # Executes this documented step of the workflow.
ds22007_monthly_SCH_season_checked <- ds22007_monthly_SCH  # Executes this documented step of the workflow.

# ==============================================================================
# 15. SAVE COMPLETE STATIONARITY AND TRANSFORMATION AUDITS
# ==============================================================================
write.csv(stationarity_insects_season_checked, file.path(out_stationarity, "stationarity_insects_final_selected.csv"), row.names = FALSE)  # Executes this documented step of the workflow.
write.csv(stationarity_environment_season_checked, file.path(out_stationarity, "stationarity_environment_final_selected.csv"), row.names = FALSE)  # Executes this documented step of the workflow.
write.csv(stationarity_insect_candidates, file.path(out_stationarity, "stationarity_insects_all_transformation_candidates.csv"), row.names = FALSE)  # Executes this documented step of the workflow.
write.csv(stationarity_environment_candidates, file.path(out_stationarity, "stationarity_environment_all_transformation_candidates.csv"), row.names = FALSE)  # Executes this documented step of the workflow.

# ==============================================================================
# 16. ENVIRONMENTAL VARIABLE CLASSES
# ==============================================================================
add_var_class <- function(df) {  # Executes this documented step of the workflow.
  df %>% mutate(var_class = case_when(EnvDataset %in% c("NDVI", "NDMI", "NIRv") ~ "canopy", stringr::str_detect(EnvColumn, "^Ts_") ~ "soil temp", stringr::str_detect(EnvColumn, "^Ta_") ~ "air temp", stringr::str_detect(EnvDataset, regex("^SMI_", ignore_case = TRUE)) ~ "hydrology", stringr::str_detect(EnvColumn, regex("^(SM_)|^(rH_)|precipitation|rain", ignore_case = TRUE)) ~ "hydrology", TRUE ~ "other"))  # Executes this documented step of the workflow.
}  # Executes this documented step of the workflow.

# ==============================================================================
# 17. FINAL ANALYSIS-SERIES COVERAGE
# ==============================================================================
insect_stationarity_summary <- stationarity_insects_season_checked %>% count(region, joint_status, selected_transformation, name = "n_series")  # Executes this documented step of the workflow.
environment_stationarity_summary <- stationarity_environment_season_checked %>% add_var_class() %>% count(region, var_class, joint_status, selected_transformation, name = "n_series")  # Executes this documented step of the workflow.
write.csv(insect_stationarity_summary, file.path(out_tables, "insect_stationarity_summary.csv"), row.names = FALSE)  # Executes this documented step of the workflow.
write.csv(environment_stationarity_summary, file.path(out_tables, "environment_stationarity_summary.csv"), row.names = FALSE)  # Executes this documented step of the workflow.

# ==============================================================================
# 18. EXPORT THE FINAL ANALYSIS-READY TIME SERIES
# ==============================================================================
export_region_final <- function(region_name, region_list) {  # Executes this documented step of the workflow.
  write.csv(region_list$ds22007, file.path(out_timeseries, paste0(region_name, "_insects_final.csv")), row.names = FALSE)  # Executes this documented step of the workflow.
  for (dataset_name in setdiff(names(region_list), "ds22007")) {  # Executes this documented step of the workflow.
    write.csv(region_list[[dataset_name]], file.path(out_timeseries, paste0(region_name, "_", dataset_name, "_final.csv")), row.names = FALSE)  # Executes this documented step of the workflow.
  }  # Executes this documented step of the workflow.
  invisible(TRUE)  # Executes this documented step of the workflow.
}  # Executes this documented step of the workflow.
walk(analysis_regions, ~ export_region_final(.x, get_region_list(.x)))  # Executes this documented step of the workflow.

# ==============================================================================
# 19. SECOND DUPLICATE AUDIT AFTER ALL TRANSFORMATIONS
# ==============================================================================
duplicate_audit_after_processing <- map_dfr(analysis_regions, function(region_name) {  # Executes this documented step of the workflow.
  region_list <- get_region_list(region_name)  # Executes this documented step of the workflow.
  insect_duplicates <- region_list$ds22007 %>% count(Exploratory, Family, year, month, name = "n_rows") %>% filter(n_rows > 1L) %>% mutate(region = region_name, dataset = "ds22007", type = "insect")  # Executes this documented step of the workflow.
  environment_duplicates <- map_dfr(setdiff(names(region_list), "ds22007"), function(dataset_name) {  # Executes this documented step of the workflow.
    region_list[[dataset_name]] %>% count(year, month, name = "n_rows") %>% filter(n_rows > 1L) %>% mutate(region = region_name, dataset = dataset_name, type = "environment")  # Executes this documented step of the workflow.
  })  # Executes this documented step of the workflow.
  bind_rows(insect_duplicates, environment_duplicates)  # Executes this documented step of the workflow.
})  # Executes this documented step of the workflow.
write.csv(duplicate_audit_after_processing, file.path(out_diagnostics, "duplicate_audit_after_all_processing.csv"), row.names = FALSE)  # Executes this documented step of the workflow.
if (nrow(duplicate_audit_after_processing) > 0L) stop("Duplicate keys were introduced during transformation; final analysis aborted.")  # Executes this documented step of the workflow.

# ==============================================================================
# 20. CONDITIONAL-GRANGER TARGET AVAILABILITY AUDIT
# ==============================================================================
conditional_target_variables <- c("Ta_200", "Ts_05", "Ts_10", "Ts_20", "Ts_50")  # Executes this documented step of the workflow.
conditional_target_families <- c("Elateridae", "Cerambycidae", "Scarabaeidae", "Staphylinidae", "Scydmaenidae", "Pentatomidae")  # Executes this documented step of the workflow.
conditional_variable_audit <- stationarity_environment_season_checked %>% filter(EnvDataset == "weather", EnvColumn %in% conditional_target_variables) %>% select(region, EnvDataset, EnvColumn, selected_transformation, joint_status, pass_stationarity)  # Executes this documented step of the workflow.
conditional_family_audit <- stationarity_insects_season_checked %>% filter(Family %in% conditional_target_families) %>% select(region, Family, selected_transformation, joint_status, pass_stationarity)  # Executes this documented step of the workflow.
write.csv(conditional_variable_audit, file.path(out_diagnostics, "conditional_target_variable_stationarity.csv"), row.names = FALSE)  # Executes this documented step of the workflow.
write.csv(conditional_family_audit, file.path(out_diagnostics, "conditional_target_family_stationarity.csv"), row.names = FALSE)  # Executes this documented step of the workflow.

# ==============================================================================
# 21. CALENDAR-COVERAGE SUMMARY
# ==============================================================================
calendar_coverage <- map_dfr(analysis_regions, function(region_name) {  # Executes this documented step of the workflow.
  region_list <- get_region_list(region_name)  # Executes this documented step of the workflow.
  insect_rows <- region_list$ds22007 %>% group_by(Family) %>% summarise(n_rows = n(), n_years = n_distinct(year), n_months = n_distinct(month), .groups = "drop") %>% mutate(region = region_name, series_type = "insect", series = Family) %>% select(region, series_type, series, n_rows, n_years, n_months)  # Executes this documented step of the workflow.
  env_rows <- map_dfr(seq_len(nrow(get_environment_specs(region_name, region_list))), function(i) {  # Iterates explicitly over environmental series to avoid ambiguous rowwise indexing.
    specs <- get_environment_specs(region_name, region_list)  # Retrieves the environmental-series specification table for this region.
    dataset_name <- specs$EnvDataset[i]  # Retrieves the current environmental dataset name.
    variable_name <- specs$EnvColumn[i]  # Retrieves the current environmental variable name.
    env_df <- region_list[[dataset_name]]  # Retrieves the current environmental data frame.
    values <- suppressWarnings(as.numeric(env_df[[variable_name]]))  # Converts the current environmental series to numeric values.
    ok <- is.finite(values)  # Identifies finite observations in the current environmental series.
    tibble(region = region_name, series_type = "environment", series = paste(dataset_name, variable_name, sep = "::"), n_rows = sum(ok), n_years = n_distinct(env_df$year[ok]), n_months = n_distinct(env_df$month[ok]))  # Summarises temporal coverage for the current environmental series.
  })  # Closes the explicit environmental-series coverage loop.
  bind_rows(insect_rows, env_rows)  # Executes this documented step of the workflow.
})  # Executes this documented step of the workflow.
write.csv(calendar_coverage, file.path(out_diagnostics, "calendar_coverage_all_series.csv"), row.names = FALSE)  # Executes this documented step of the workflow.

# ==============================================================================
# 22. PREDEFINED ANALYSIS DECISIONS
# ==============================================================================
analysis_decisions <- c("Insects are summed to one Family x region x year x month abundance before analysis.", "Weather is aggregated to one regional monthly value before analysis.", "SMI is aggregated to one regional monthly value before analysis.", "Satellite indices are collapsed to one regional monthly value if repeated source rows exist.", "Winter months are never inserted, interpolated, or treated as one-month lags.", "Seasonality is screened with annual sine/cosine terms and removed only when p < 0.05.", "Stationarity requires both ADF and KPSS to support stationarity.", "When transformation is needed, the first stationary candidate in the predefined order level, detrended, calendar-differenced, detrended-plus-calendar-differenced is selected.", "Calendar differencing is computed only when the immediately preceding calendar month exists.", "Granger lags are 1, 2 and 3 true calendar months.", "The surrogate analysis uses 999 repetitions per method and region.", "BY correction is applied to the final observed pairwise and conditional Granger p-values.")  # Executes this documented step of the workflow.
writeLines(analysis_decisions, file.path(out_diagnostics, "analysis_decisions.txt"))  # Executes this documented step of the workflow.

# ==============================================================================
# 23. SAVE FINAL PRE-GRANGER OBJECTS
# ==============================================================================
saveRDS(ds22007_monthly_ALB_season_checked, file.path(out_objects, "ds22007_monthly_ALB_final.rds"))  # Executes this documented step of the workflow.
saveRDS(ds22007_monthly_HAI_season_checked, file.path(out_objects, "ds22007_monthly_HAI_final.rds"))  # Executes this documented step of the workflow.
saveRDS(ds22007_monthly_SCH_season_checked, file.path(out_objects, "ds22007_monthly_SCH_final.rds"))  # Executes this documented step of the workflow.
saveRDS(stationarity_insects_season_checked, file.path(out_objects, "stationarity_insects_final.rds"))  # Executes this documented step of the workflow.
saveRDS(stationarity_environment_season_checked, file.path(out_objects, "stationarity_environment_final.rds"))  # Executes this documented step of the workflow.

# ==============================================================================
# 24. FINAL STATIONARITY SUMMARY FOR MANUSCRIPT SUPPORT
# ==============================================================================
stationarity_status_by_region_type <- bind_rows(stationarity_insects_season_checked %>% transmute(series_type = "insect", region, joint_status), stationarity_environment_season_checked %>% transmute(series_type = "environment", region, joint_status)) %>% count(series_type, region, joint_status, name = "n_series")  # Executes this documented step of the workflow.
write.csv(stationarity_status_by_region_type, file.path(out_tables, "stationarity_status_by_region_type.csv"), row.names = FALSE)  # Executes this documented step of the workflow.

# ==============================================================================
# 25. ANALYSIS-READY SERIES COUNTS
# ==============================================================================
analysis_ready_counts <- bind_rows(stationarity_insects_season_checked %>% group_by(region) %>% summarise(n_total = n(), n_ready = sum(pass_stationarity), .groups = "drop") %>% mutate(series_type = "insect"), stationarity_environment_season_checked %>% group_by(region) %>% summarise(n_total = n(), n_ready = sum(pass_stationarity), .groups = "drop") %>% mutate(series_type = "environment")) %>% select(series_type, everything())  # Executes this documented step of the workflow.
write.csv(analysis_ready_counts, file.path(out_tables, "analysis_ready_series_counts.csv"), row.names = FALSE)  # Executes this documented step of the workflow.

# ==============================================================================
# 26. PRE-FLIGHT CHECK BEFORE THE COMPUTATIONALLY EXPENSIVE FULL RUN
# ==============================================================================
if (sum(stationarity_insects_season_checked$pass_stationarity, na.rm = TRUE) == 0L) stop("No stationary insect series are available; Section 27 cannot run.")  # Executes this documented step of the workflow.
if (sum(stationarity_environment_season_checked$pass_stationarity, na.rm = TRUE) == 0L) stop("No stationary environmental series are available; Section 27 cannot run.")  # Executes this documented step of the workflow.
if (anyDuplicated(stationarity_insects_season_checked[c("region", "Family")]) > 0L) stop("Insect stationarity table contains duplicate region-family keys.")  # Executes this documented step of the workflow.
if (anyDuplicated(stationarity_environment_season_checked[c("region", "EnvDataset", "EnvColumn")]) > 0L) stop("Environmental stationarity table contains duplicate series keys.")  # Executes this documented step of the workflow.
cat("\nSECTIONS 1-26 COMPLETE: all data-structure checks passed. Starting full Section 27 run.\n")  # Executes this documented step of the workflow.
# ==============================================================================
# 27. CALENDAR-AWARE SURROGATE-NULL AND MULTIPLE-TESTING ANALYSIS
# ==============================================================================
# PURPOSE
# - Recompute the observed pairwise Granger tests using TRUE calendar lags.
# - Never treat a winter sampling gap as a one-month lag.
# - Use the final season-checked analysis columns selected earlier in the script.
# - Include only series that passed the final joint ADF/KPSS stationarity screen.
# - Compare the observed Granger results with structured surrogate null models.
# - Apply conventional and surrogate-based multiple-testing corrections.
#
# IMPORTANT
# This section is still PAIRWISE Granger analysis (environment -> insect).
# It does NOT implement conditional Granger causality.
#
# REQUIRED OBJECTS FROM SECTIONS 1-26
#   ds22007_monthly_ALB_season_checked
#   ds22007_monthly_HAI_season_checked
#   ds22007_monthly_SCH_season_checked
#   stationarity_insects_season_checked
#   stationarity_environment_season_checked
#   lags
#   out_root
#   add_var_class()   [used if available]
# ==============================================================================

# ------------------------------------------------------------------------------
# 27.1 CONFIGURATION
# ------------------------------------------------------------------------------

surrogate_run_mode <- "full"  # Fixes the surrogate analysis to the full manuscript mode.
surrogate_n_repeats <- 999L  # Uses 999 surrogate repetitions per method and region for the final manuscript run.
surrogate_run_label <- "full999"  # Labels every surrogate output as the full 999-repetition run.
surrogate_quality_reps <- 50L  # Uses fifty surrogate realizations per design for quality diagnostics.
surrogate_alpha <- 0.05  # Sets the nominal significance threshold used in the surrogate summaries.
surrogate_seed <- 20261005L  # Fixes the random-number seed so the full surrogate analysis is exactly reproducible.
surrogate_block_length <- max(lags)  # Uses the maximum tested calendar lag as the within-year block length for the block-shuffle null.

set.seed(surrogate_seed)  # Executes this documented step of the workflow.

out_surrogates <- file.path(out_root, "surrogates_calendar_aware_20261005")  # Executes this documented step of the workflow.
out_surrogate_tables <- file.path(out_surrogates, "tables")  # Executes this documented step of the workflow.
out_surrogate_plots <- file.path(out_surrogates, "plots")  # Executes this documented step of the workflow.
out_surrogate_diagnostics <- file.path(out_surrogates, "diagnostics")  # Executes this documented step of the workflow.
out_surrogate_objects <- file.path(out_surrogates, "objects")  # Executes this documented step of the workflow.

dir.create(out_surrogates, recursive = TRUE, showWarnings = FALSE)  # Executes this documented step of the workflow.
dir.create(out_surrogate_tables, recursive = TRUE, showWarnings = FALSE)  # Executes this documented step of the workflow.
dir.create(out_surrogate_plots, recursive = TRUE, showWarnings = FALSE)  # Executes this documented step of the workflow.
dir.create(out_surrogate_diagnostics, recursive = TRUE, showWarnings = FALSE)  # Executes this documented step of the workflow.
dir.create(out_surrogate_objects, recursive = TRUE, showWarnings = FALSE)  # Executes this documented step of the workflow.

# These surrogate methods are deliberately restricted to methods that can be
# defined without pretending that the end of one active season is adjacent to
# the beginning of the next active season.
surrogate_methods <- tibble::tribble(  # Executes this documented step of the workflow.
  ~method, ~label, ~null_question,  # Executes this documented step of the workflow.
  "random_permutation", "Random permutation",  # Executes this documented step of the workflow.
  "What remains if all temporal ordering is removed?",  # Executes this documented step of the workflow.
  "month_stratified_shuffle", "Month-stratified shuffle",  # Executes this documented step of the workflow.
  "What remains if calendar-month distributions are preserved but interannual alignment is broken?",  # Executes this documented step of the workflow.
  "year_shuffle", "Whole-year shuffle",  # Executes this documented step of the workflow.
  "What remains if each within-season yearly trajectory is preserved but years are reassigned?",  # Executes this documented step of the workflow.
  "year_circular_shift", "Whole-year circular shift",  # Executes this documented step of the workflow.
  "What remains if the entire environmental yearly sequence is shifted relative to the insects?",  # Executes this documented step of the workflow.
  "within_year_block_shuffle", "Within-year block shuffle",  # Executes this documented step of the workflow.
  "What remains if short within-season blocks are preserved but their order within years is broken?"  # Executes this documented step of the workflow.
)  # Executes this documented step of the workflow.

surrogate_methods_to_run <- surrogate_methods$method  # Executes this documented step of the workflow.

write.csv(  # Executes this documented step of the workflow.
  surrogate_methods,  # Executes this documented step of the workflow.
  file.path(out_surrogate_diagnostics, "surrogate_methods.csv"),  # Executes this documented step of the workflow.
  row.names = FALSE  # Executes this documented step of the workflow.
)  # Executes this documented step of the workflow.

# ------------------------------------------------------------------------------
# 27.2 BASIC HELPERS
# ------------------------------------------------------------------------------

get_region_list_calendar <- function(region_name) {  # Executes this documented step of the workflow.
  if (region_name == "ALB") return(ds22007_monthly_ALB_season_checked)  # Executes this documented step of the workflow.
  if (region_name == "HAI") return(ds22007_monthly_HAI_season_checked)  # Executes this documented step of the workflow.
  if (region_name == "SCH") return(ds22007_monthly_SCH_season_checked)  # Executes this documented step of the workflow.
  stop(paste0("Unknown region: ", region_name))  # Executes this documented step of the workflow.
}  # Executes this documented step of the workflow.

make_test_id_calendar <- function(  # Executes this documented step of the workflow.
    region, Family, EnvDataset, EnvColumn,  # Executes this documented step of the workflow.
    insect_analysis_column, environment_analysis_column, Lag) {  # Executes this documented step of the workflow.
  paste(  # Executes this documented step of the workflow.
    region,  # Executes this documented step of the workflow.
    Family,  # Executes this documented step of the workflow.
    EnvDataset,  # Executes this documented step of the workflow.
    EnvColumn,  # Executes this documented step of the workflow.
    insect_analysis_column,  # Executes this documented step of the workflow.
    environment_analysis_column,  # Executes this documented step of the workflow.
    Lag,  # Executes this documented step of the workflow.
    sep = "||"  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.
}  # Executes this documented step of the workflow.

add_var_class_calendar <- function(df) {  # Executes this documented step of the workflow.
  if (exists("add_var_class", mode = "function")) {  # Executes this documented step of the workflow.
    out <- tryCatch(add_var_class(df), error = function(e) NULL)  # Executes this documented step of the workflow.
    if (!is.null(out) && "var_class" %in% names(out)) return(out)  # Executes this documented step of the workflow.
  }  # Executes this documented step of the workflow.
  df %>% dplyr::mutate(var_class = NA_character_)  # Executes this documented step of the workflow.
}  # Executes this documented step of the workflow.

# ------------------------------------------------------------------------------
# 27.3 CALENDAR-AWARE GRANGER TEST
# ------------------------------------------------------------------------------
# For lag p, a row is usable only if the exact calendar months t-1,...,t-p
# exist in the matched insect-environment data. Thus October -> April can never
# be interpreted as lag 1. No winter values are invented or interpolated.

calendar_granger_safe <- function(x, y, year, month, lags = 1:3) {  # Executes this documented step of the workflow.

  dat <- tibble::tibble(  # Executes this documented step of the workflow.
    year = as.integer(year),  # Executes this documented step of the workflow.
    month = as.integer(month),  # Executes this documented step of the workflow.
    x = suppressWarnings(as.numeric(x)),  # Executes this documented step of the workflow.
    y = suppressWarnings(as.numeric(y))  # Executes this documented step of the workflow.
  ) %>%  # Executes this documented step of the workflow.
    dplyr::filter(  # Executes this documented step of the workflow.
      is.finite(year),  # Executes this documented step of the workflow.
      is.finite(month),  # Executes this documented step of the workflow.
      is.finite(x),  # Executes this documented step of the workflow.
      is.finite(y)  # Executes this documented step of the workflow.
    ) %>%  # Executes this documented step of the workflow.
    dplyr::mutate(time_id = year * 12L + month) %>%  # Executes this documented step of the workflow.
    dplyr::arrange(time_id)  # Executes this documented step of the workflow.

  if (nrow(dat) == 0L || anyDuplicated(dat$time_id) > 0L) {  # Executes this documented step of the workflow.
    return(  # Executes this documented step of the workflow.
      purrr::map_dfr(lags, function(p) {  # Executes this documented step of the workflow.
        tibble::tibble(  # Executes this documented step of the workflow.
          Lag = p,  # Executes this documented step of the workflow.
          n_obs = 0L,  # Executes this documented step of the workflow.
          n_years = 0L,  # Executes this documented step of the workflow.
          F_stat = NA_real_,  # Executes this documented step of the workflow.
          p_value = NA_real_,  # Executes this documented step of the workflow.
          test_status = if (nrow(dat) == 0L) "no_complete_data" else "duplicated_year_month"  # Executes this documented step of the workflow.
        )  # Executes this documented step of the workflow.
      })  # Executes this documented step of the workflow.
    )  # Executes this documented step of the workflow.
  }  # Executes this documented step of the workflow.

  purrr::map_dfr(lags, function(p) {  # Executes this documented step of the workflow.

    model_df <- dat  # Executes this documented step of the workflow.

    for (k in seq_len(p)) {  # Executes this documented step of the workflow.
      previous_row <- match(model_df$time_id - k, dat$time_id)  # Executes this documented step of the workflow.
      model_df[[paste0("x_lag", k)]] <- dat$x[previous_row]  # Executes this documented step of the workflow.
      model_df[[paste0("y_lag", k)]] <- dat$y[previous_row]  # Executes this documented step of the workflow.
    }  # Executes this documented step of the workflow.

    x_lags <- paste0("x_lag", seq_len(p))  # Executes this documented step of the workflow.
    y_lags <- paste0("y_lag", seq_len(p))  # Executes this documented step of the workflow.
    required <- c("x", x_lags, y_lags)  # Executes this documented step of the workflow.

    model_df <- model_df[stats::complete.cases(model_df[, required, drop = FALSE]), , drop = FALSE]  # Executes this documented step of the workflow.

    # Full model has intercept + p insect lags + p environmental lags.
    # Require at least two residual degrees of freedom.
    minimum_n <- 2L * p + 3L  # Executes this documented step of the workflow.

    if (  # Executes this documented step of the workflow.
      nrow(model_df) < minimum_n ||  # Executes this documented step of the workflow.
      stats::var(model_df$x) == 0 ||  # Executes this documented step of the workflow.
      any(vapply(model_df[, c(x_lags, y_lags), drop = FALSE], stats::var, numeric(1)) == 0)  # Executes this documented step of the workflow.
    ) {  # Executes this documented step of the workflow.
      return(  # Executes this documented step of the workflow.
        tibble::tibble(  # Executes this documented step of the workflow.
          Lag = p,  # Executes this documented step of the workflow.
          n_obs = nrow(model_df),  # Executes this documented step of the workflow.
          n_years = dplyr::n_distinct(model_df$year),  # Executes this documented step of the workflow.
          F_stat = NA_real_,  # Executes this documented step of the workflow.
          p_value = NA_real_,  # Executes this documented step of the workflow.
          test_status = "not_testable"  # Executes this documented step of the workflow.
        )  # Executes this documented step of the workflow.
      )  # Executes this documented step of the workflow.
    }  # Executes this documented step of the workflow.

    restricted_formula <- stats::reformulate(x_lags, response = "x")  # Executes this documented step of the workflow.
    full_formula <- stats::reformulate(c(x_lags, y_lags), response = "x")  # Executes this documented step of the workflow.

    result <- tryCatch(  # Executes this documented step of the workflow.
      {  # Executes this documented step of the workflow.
        restricted_fit <- stats::lm(restricted_formula, data = model_df)  # Executes this documented step of the workflow.
        full_fit <- stats::lm(full_formula, data = model_df)  # Executes this documented step of the workflow.

        if (  # Executes this documented step of the workflow.
          restricted_fit$rank < length(stats::coef(restricted_fit)) ||  # Executes this documented step of the workflow.
          full_fit$rank < length(stats::coef(full_fit))  # Executes this documented step of the workflow.
        ) {  # Executes this documented step of the workflow.
          stop("singular model")  # Executes this documented step of the workflow.
        }  # Executes this documented step of the workflow.

        comparison <- stats::anova(restricted_fit, full_fit)  # Executes this documented step of the workflow.

        tibble::tibble(  # Executes this documented step of the workflow.
          Lag = p,  # Executes this documented step of the workflow.
          n_obs = nrow(model_df),  # Executes this documented step of the workflow.
          n_years = dplyr::n_distinct(model_df$year),  # Executes this documented step of the workflow.
          F_stat = as.numeric(comparison$F[2]),  # Executes this documented step of the workflow.
          p_value = as.numeric(comparison$`Pr(>F)`[2]),  # Executes this documented step of the workflow.
          test_status = "tested"  # Executes this documented step of the workflow.
        )  # Executes this documented step of the workflow.
      },  # Executes this documented step of the workflow.
      error = function(e) {  # Executes this documented step of the workflow.
        tibble::tibble(  # Executes this documented step of the workflow.
          Lag = p,  # Executes this documented step of the workflow.
          n_obs = nrow(model_df),  # Executes this documented step of the workflow.
          n_years = dplyr::n_distinct(model_df$year),  # Executes this documented step of the workflow.
          F_stat = NA_real_,  # Executes this documented step of the workflow.
          p_value = NA_real_,  # Executes this documented step of the workflow.
          test_status = paste0("granger_error: ", conditionMessage(e))  # Executes this documented step of the workflow.
        )  # Executes this documented step of the workflow.
      }  # Executes this documented step of the workflow.
    )  # Executes this documented step of the workflow.

    result  # Executes this documented step of the workflow.
  })  # Executes this documented step of the workflow.
}  # Executes this documented step of the workflow.

# ------------------------------------------------------------------------------
# 27.4 BUILD A COMMON ACTIVE-SEASON ENVIRONMENTAL GRID
# ------------------------------------------------------------------------------
# A rectangular year x retained-month grid is used only for generating a common
# environmental surrogate. Calendar-aware Granger lags are still created from
# the actual month numbers, so missing winter months never become adjacent.

prepare_complete_environment_grid_calendar <- function(env_df, analysis_column) {  # Executes this documented step of the workflow.

  if (!all(c("year", "month", analysis_column) %in% names(env_df))) {  # Executes this documented step of the workflow.
    return(list(status = "missing_required_columns"))  # Executes this documented step of the workflow.
  }  # Executes this documented step of the workflow.

  dat <- env_df %>%  # Executes this documented step of the workflow.
    dplyr::transmute(  # Executes this documented step of the workflow.
      year = as.integer(year),  # Executes this documented step of the workflow.
      month = as.integer(month),  # Executes this documented step of the workflow.
      value = suppressWarnings(as.numeric(.data[[analysis_column]]))  # Executes this documented step of the workflow.
    ) %>%  # Executes this documented step of the workflow.
    dplyr::arrange(year, month)  # Executes this documented step of the workflow.

  duplicates <- dat %>%  # Executes this documented step of the workflow.
    dplyr::count(year, month, name = "n_rows") %>%  # Executes this documented step of the workflow.
    dplyr::filter(n_rows > 1L)  # Executes this documented step of the workflow.

  if (nrow(duplicates) > 0L) {  # Executes this documented step of the workflow.
    return(list(status = "duplicated_year_month_rows", duplicates = duplicates))  # Executes this documented step of the workflow.
  }  # Executes this documented step of the workflow.

  usable_years <- dat %>%  # Executes this documented step of the workflow.
    dplyr::filter(is.finite(value)) %>%  # Executes this documented step of the workflow.
    dplyr::distinct(year) %>%  # Executes this documented step of the workflow.
    dplyr::arrange(year) %>%  # Executes this documented step of the workflow.
    dplyr::pull(year)  # Executes this documented step of the workflow.

  if (length(usable_years) < 2L) {  # Executes this documented step of the workflow.
    return(list(status = "fewer_than_two_usable_years"))  # Executes this documented step of the workflow.
  }  # Executes this documented step of the workflow.

  months_by_year <- lapply(  # Executes this documented step of the workflow.
    usable_years,  # Executes this documented step of the workflow.
    function(yy) {  # Executes this documented step of the workflow.
      dat %>%  # Executes this documented step of the workflow.
        dplyr::filter(year == yy, is.finite(value)) %>%  # Executes this documented step of the workflow.
        dplyr::distinct(month) %>%  # Executes this documented step of the workflow.
        dplyr::pull(month)  # Executes this documented step of the workflow.
    }  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.

  common_months <- sort(Reduce(intersect, months_by_year))  # Executes this documented step of the workflow.

  if (length(common_months) == 0L) {  # Executes this documented step of the workflow.
    return(list(status = "no_common_months"))  # Executes this documented step of the workflow.
  }  # Executes this documented step of the workflow.

  complete <- dat %>%  # Executes this documented step of the workflow.
    dplyr::filter(  # Executes this documented step of the workflow.
      year %in% usable_years,  # Executes this documented step of the workflow.
      month %in% common_months,  # Executes this documented step of the workflow.
      is.finite(value)  # Executes this documented step of the workflow.
    ) %>%  # Executes this documented step of the workflow.
    dplyr::arrange(year, month) %>%  # Executes this documented step of the workflow.
    dplyr::mutate(env_index = dplyr::row_number())  # Executes this documented step of the workflow.

  expected_n <- length(usable_years) * length(common_months)  # Executes this documented step of the workflow.

  if (nrow(complete) != expected_n) {  # Executes this documented step of the workflow.
    return(list(  # Executes this documented step of the workflow.
      status = "incomplete_rectangular_grid",  # Executes this documented step of the workflow.
      usable_years = usable_years,  # Executes this documented step of the workflow.
      common_months = common_months,  # Executes this documented step of the workflow.
      n_rows = nrow(complete),  # Executes this documented step of the workflow.
      expected_n = expected_n  # Executes this documented step of the workflow.
    ))  # Executes this documented step of the workflow.
  }  # Executes this documented step of the workflow.

  list(  # Executes this documented step of the workflow.
    status = "ok",  # Executes this documented step of the workflow.
    data = complete,  # Executes this documented step of the workflow.
    usable_years = usable_years,  # Executes this documented step of the workflow.
    common_months = common_months  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.
}  # Executes this documented step of the workflow.

# ------------------------------------------------------------------------------
# 27.5 SURROGATE GENERATORS THAT DO NOT CROSS WINTER BOUNDARIES
# ------------------------------------------------------------------------------

random_permutation_calendar <- function(x) {  # Executes this documented step of the workflow.
  sample(x, size = length(x), replace = FALSE)  # Executes this documented step of the workflow.
}  # Executes this documented step of the workflow.

month_stratified_shuffle_calendar <- function(x, month) {  # Executes this documented step of the workflow.
  out <- x  # Executes this documented step of the workflow.
  for (mm in sort(unique(month))) {  # Executes this documented step of the workflow.
    idx <- which(month == mm)  # Executes this documented step of the workflow.
    out[idx] <- sample(x[idx], size = length(idx), replace = FALSE)  # Executes this documented step of the workflow.
  }  # Executes this documented step of the workflow.
  out  # Executes this documented step of the workflow.
}  # Executes this documented step of the workflow.

year_shuffle_calendar <- function(x, year, month) {  # Executes this documented step of the workflow.
  years <- sort(unique(year))  # Executes this documented step of the workflow.
  donor_years <- sample(years, size = length(years), replace = FALSE)  # Executes this documented step of the workflow.
  out <- x  # Executes this documented step of the workflow.

  for (i in seq_along(years)) {  # Executes this documented step of the workflow.
    target_idx <- which(year == years[i])  # Executes this documented step of the workflow.
    donor_idx <- which(year == donor_years[i])  # Executes this documented step of the workflow.
    donor_match <- match(month[target_idx], month[donor_idx])  # Executes this documented step of the workflow.
    out[target_idx] <- x[donor_idx[donor_match]]  # Executes this documented step of the workflow.
  }  # Executes this documented step of the workflow.

  out  # Executes this documented step of the workflow.
}  # Executes this documented step of the workflow.

year_circular_shift_calendar <- function(x, year, month) {  # Executes this documented step of the workflow.
  years <- sort(unique(year))  # Executes this documented step of the workflow.
  n_years <- length(years)  # Executes this documented step of the workflow.
  if (n_years < 2L) return(rep(NA_real_, length(x)))  # Executes this documented step of the workflow.

  shift <- sample(seq_len(n_years - 1L), size = 1L)  # Executes this documented step of the workflow.
  donor_years <- years[((seq_along(years) - 1L + shift) %% n_years) + 1L]  # Executes this documented step of the workflow.
  out <- x  # Executes this documented step of the workflow.

  for (i in seq_along(years)) {  # Executes this documented step of the workflow.
    target_idx <- which(year == years[i])  # Executes this documented step of the workflow.
    donor_idx <- which(year == donor_years[i])  # Executes this documented step of the workflow.
    donor_match <- match(month[target_idx], month[donor_idx])  # Executes this documented step of the workflow.
    out[target_idx] <- x[donor_idx[donor_match]]  # Executes this documented step of the workflow.
  }  # Executes this documented step of the workflow.

  out  # Executes this documented step of the workflow.
}  # Executes this documented step of the workflow.

within_year_block_shuffle_calendar <- function(x, year, month, block_length) {  # Executes this documented step of the workflow.
  out <- x  # Executes this documented step of the workflow.

  for (yy in sort(unique(year))) {  # Executes this documented step of the workflow.
    idx <- which(year == yy)  # Executes this documented step of the workflow.
    idx <- idx[order(month[idx])]  # Executes this documented step of the workflow.

    if (length(idx) <= block_length) {  # Executes this documented step of the workflow.
      out[idx] <- sample(x[idx], size = length(idx), replace = FALSE)  # Executes this documented step of the workflow.
      next  # Executes this documented step of the workflow.
    }  # Executes this documented step of the workflow.

    local_positions <- seq_along(idx)  # Executes this documented step of the workflow.
    block_id <- ceiling(local_positions / block_length)  # Executes this documented step of the workflow.
    blocks <- split(local_positions, block_id)  # Executes this documented step of the workflow.
    block_order <- sample(seq_along(blocks), size = length(blocks), replace = FALSE)  # Executes this documented step of the workflow.
    permuted_positions <- unlist(blocks[block_order], use.names = FALSE)  # Executes this documented step of the workflow.

    out[idx] <- x[idx][permuted_positions]  # Executes this documented step of the workflow.
  }  # Executes this documented step of the workflow.

  out  # Executes this documented step of the workflow.
}  # Executes this documented step of the workflow.

generate_surrogate_calendar <- function(x, year, month, method) {  # Executes this documented step of the workflow.
  if (method == "random_permutation") {  # Executes this documented step of the workflow.
    return(random_permutation_calendar(x))  # Executes this documented step of the workflow.
  }  # Executes this documented step of the workflow.
  if (method == "month_stratified_shuffle") {  # Executes this documented step of the workflow.
    return(month_stratified_shuffle_calendar(x, month))  # Executes this documented step of the workflow.
  }  # Executes this documented step of the workflow.
  if (method == "year_shuffle") {  # Executes this documented step of the workflow.
    return(year_shuffle_calendar(x, year, month))  # Executes this documented step of the workflow.
  }  # Executes this documented step of the workflow.
  if (method == "year_circular_shift") {  # Executes this documented step of the workflow.
    return(year_circular_shift_calendar(x, year, month))  # Executes this documented step of the workflow.
  }  # Executes this documented step of the workflow.
  if (method == "within_year_block_shuffle") {  # Executes this documented step of the workflow.
    return(within_year_block_shuffle_calendar(x, year, month, surrogate_block_length))  # Executes this documented step of the workflow.
  }  # Executes this documented step of the workflow.
  stop(paste0("Unknown surrogate method: ", method))  # Executes this documented step of the workflow.
}  # Executes this documented step of the workflow.

# ------------------------------------------------------------------------------
# 27.6 CALENDAR-AWARE TEMPORAL-STRUCTURE DIAGNOSTICS
# ------------------------------------------------------------------------------

calendar_lag_correlations <- function(x, year, month, lag_orders = lags) {  # Executes this documented step of the workflow.
  dat <- tibble::tibble(  # Executes this documented step of the workflow.
    x = as.numeric(x),  # Executes this documented step of the workflow.
    year = as.integer(year),  # Executes this documented step of the workflow.
    month = as.integer(month)  # Executes this documented step of the workflow.
  ) %>%  # Executes this documented step of the workflow.
    dplyr::mutate(time_id = year * 12L + month) %>%  # Executes this documented step of the workflow.
    dplyr::arrange(time_id)  # Executes this documented step of the workflow.

  vapply(  # Executes this documented step of the workflow.
    lag_orders,  # Executes this documented step of the workflow.
    function(p) {  # Executes this documented step of the workflow.
      previous <- match(dat$time_id - p, dat$time_id)  # Executes this documented step of the workflow.
      ok <- is.finite(dat$x) & !is.na(previous) & is.finite(dat$x[previous])  # Executes this documented step of the workflow.
      if (sum(ok) < 3L) return(NA_real_)  # Executes this documented step of the workflow.
      suppressWarnings(stats::cor(dat$x[ok], dat$x[previous[ok]]))  # Executes this documented step of the workflow.
    },  # Executes this documented step of the workflow.
    numeric(1)  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.
}  # Executes this documented step of the workflow.

surrogate_quality_metrics_calendar <- function(observed, surrogate, year, month) {  # Executes this documented step of the workflow.
  probs <- seq(0, 1, by = 0.1)  # Executes this documented step of the workflow.

  q_observed <- as.numeric(stats::quantile(observed, probs = probs, names = FALSE, type = 8))  # Executes this documented step of the workflow.
  q_surrogate <- as.numeric(stats::quantile(surrogate, probs = probs, names = FALSE, type = 8))  # Executes this documented step of the workflow.

  acf_observed <- calendar_lag_correlations(observed, year, month, lags)  # Executes this documented step of the workflow.
  acf_surrogate <- calendar_lag_correlations(surrogate, year, month, lags)  # Executes this documented step of the workflow.

  month_obs <- tapply(observed, month, mean)  # Executes this documented step of the workflow.
  month_sur <- tapply(surrogate, month, mean)  # Executes this documented step of the workflow.

  acf_ok <- is.finite(acf_observed) & is.finite(acf_surrogate)  # Executes this documented step of the workflow.

  tibble::tibble(  # Executes this documented step of the workflow.
    mean_difference = mean(surrogate) - mean(observed),  # Executes this documented step of the workflow.
    sd_ratio = stats::sd(surrogate) / stats::sd(observed),  # Executes this documented step of the workflow.
    quantile_RMSE = sqrt(mean((q_surrogate - q_observed)^2)),  # Executes this documented step of the workflow.
    calendar_acf_RMSE = if (any(acf_ok)) {  # Executes this documented step of the workflow.
      sqrt(mean((acf_surrogate[acf_ok] - acf_observed[acf_ok])^2))  # Executes this documented step of the workflow.
    } else {  # Executes this documented step of the workflow.
      NA_real_  # Executes this documented step of the workflow.
    },  # Executes this documented step of the workflow.
    monthly_mean_correlation = suppressWarnings(  # Executes this documented step of the workflow.
      stats::cor(month_obs, month_sur, use = "complete.obs")  # Executes this documented step of the workflow.
    )  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.
}  # Executes this documented step of the workflow.

# ------------------------------------------------------------------------------
# 27.7 BUILD THE OBSERVED CALENDAR-AWARE DESIGN
# ------------------------------------------------------------------------------

prepare_one_environment_calendar <- function(  # Executes this documented step of the workflow.
    region_name,  # Executes this documented step of the workflow.
    region_list,  # Executes this documented step of the workflow.
    env_dataset,  # Executes this documented step of the workflow.
    env_column,  # Executes this documented step of the workflow.
    environment_analysis_column,  # Executes this documented step of the workflow.
    environment_was_deseasonalised,  # Executes this documented step of the workflow.
    insect_keys) {  # Executes this documented step of the workflow.

  if (!env_dataset %in% names(region_list)) {  # Executes this documented step of the workflow.
    return(list(status = "environment_dataset_missing"))  # Executes this documented step of the workflow.
  }  # Executes this documented step of the workflow.

  env_grid <- prepare_complete_environment_grid_calendar(  # Executes this documented step of the workflow.
    region_list[[env_dataset]],  # Executes this documented step of the workflow.
    environment_analysis_column  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.

  if (!identical(env_grid$status, "ok")) {  # Executes this documented step of the workflow.
    return(c(list(status = env_grid$status), env_grid[setdiff(names(env_grid), "status")]))  # Executes this documented step of the workflow.
  }  # Executes this documented step of the workflow.

  family_tests <- vector("list", nrow(insect_keys))  # Executes this documented step of the workflow.
  family_diagnostics <- vector("list", nrow(insect_keys))  # Executes this documented step of the workflow.

  for (i in seq_len(nrow(insect_keys))) {  # Executes this documented step of the workflow.
    fam <- insect_keys$Family[i]  # Executes this documented step of the workflow.
    insect_column <- insect_keys$analysis_column[i]  # Executes this documented step of the workflow.
    insect_was_deseasonalised <- insect_keys$was_deseasonalised[i]  # Executes this documented step of the workflow.

    if (!insect_column %in% names(region_list$ds22007)) {  # Executes this documented step of the workflow.
      family_diagnostics[[i]] <- tibble::tibble(  # Executes this documented step of the workflow.
        region = region_name,  # Executes this documented step of the workflow.
        Family = fam,  # Executes this documented step of the workflow.
        EnvDataset = env_dataset,  # Executes this documented step of the workflow.
        EnvColumn = env_column,  # Executes this documented step of the workflow.
        status = "insect_analysis_column_missing",  # Executes this documented step of the workflow.
        n_matched_observations = 0L,  # Executes this documented step of the workflow.
        lags_kept = NA_character_  # Executes this documented step of the workflow.
      )  # Executes this documented step of the workflow.
      next  # Executes this documented step of the workflow.
    }  # Executes this documented step of the workflow.

    insect_df <- region_list$ds22007 %>%  # Executes this documented step of the workflow.
      dplyr::filter(Family == fam) %>%  # Executes this documented step of the workflow.
      dplyr::transmute(  # Executes this documented step of the workflow.
        year = as.integer(year),  # Executes this documented step of the workflow.
        month = as.integer(month),  # Executes this documented step of the workflow.
        insect = suppressWarnings(as.numeric(.data[[insect_column]]))  # Executes this documented step of the workflow.
      ) %>%  # Executes this documented step of the workflow.
      dplyr::arrange(year, month)  # Executes this documented step of the workflow.

    if (anyDuplicated(paste(insect_df$year, insect_df$month, sep = "-")) > 0L) {  # Executes this documented step of the workflow.
      family_diagnostics[[i]] <- tibble::tibble(  # Executes this documented step of the workflow.
        region = region_name,  # Executes this documented step of the workflow.
        Family = fam,  # Executes this documented step of the workflow.
        EnvDataset = env_dataset,  # Executes this documented step of the workflow.
        EnvColumn = env_column,  # Executes this documented step of the workflow.
        status = "duplicated_insect_year_month",  # Executes this documented step of the workflow.
        n_matched_observations = 0L,  # Executes this documented step of the workflow.
        lags_kept = NA_character_  # Executes this documented step of the workflow.
      )  # Executes this documented step of the workflow.
      next  # Executes this documented step of the workflow.
    }  # Executes this documented step of the workflow.

    pair_df <- env_grid$data %>%  # Executes this documented step of the workflow.
      dplyr::rename(observed_env = value) %>%  # Executes this documented step of the workflow.
      dplyr::inner_join(insect_df, by = c("year", "month")) %>%  # Executes this documented step of the workflow.
      dplyr::filter(is.finite(insect), is.finite(observed_env)) %>%  # Executes this documented step of the workflow.
      dplyr::arrange(year, month)  # Executes this documented step of the workflow.

    observed_probe <- calendar_granger_safe(  # Executes this documented step of the workflow.
      x = pair_df$insect,  # Executes this documented step of the workflow.
      y = pair_df$observed_env,  # Executes this documented step of the workflow.
      year = pair_df$year,  # Executes this documented step of the workflow.
      month = pair_df$month,  # Executes this documented step of the workflow.
      lags = lags  # Executes this documented step of the workflow.
    )  # Executes this documented step of the workflow.

    tested_probe <- observed_probe %>%  # Executes this documented step of the workflow.
      dplyr::filter(test_status == "tested", is.finite(p_value), is.finite(F_stat))  # Executes this documented step of the workflow.

    matched_lags <- sort(unique(tested_probe$Lag))  # Executes this documented step of the workflow.

    if (length(matched_lags) == 0L) {  # Executes this documented step of the workflow.
      family_diagnostics[[i]] <- tibble::tibble(  # Executes this documented step of the workflow.
        region = region_name,  # Executes this documented step of the workflow.
        Family = fam,  # Executes this documented step of the workflow.
        EnvDataset = env_dataset,  # Executes this documented step of the workflow.
        EnvColumn = env_column,  # Executes this documented step of the workflow.
        status = "no_calendar_aware_granger_test",  # Executes this documented step of the workflow.
        n_matched_observations = nrow(pair_df),  # Executes this documented step of the workflow.
        lags_kept = NA_character_  # Executes this documented step of the workflow.
      )  # Executes this documented step of the workflow.
      next  # Executes this documented step of the workflow.
    }  # Executes this documented step of the workflow.

    family_tests[[i]] <- list(  # Executes this documented step of the workflow.
      Family = fam,  # Executes this documented step of the workflow.
      insect_analysis_column = insect_column,  # Executes this documented step of the workflow.
      insect_was_deseasonalised = insect_was_deseasonalised,  # Executes this documented step of the workflow.
      data = pair_df,  # Executes this documented step of the workflow.
      lags = matched_lags,  # Executes this documented step of the workflow.
      observed = tested_probe  # Executes this documented step of the workflow.
    )  # Executes this documented step of the workflow.

    family_diagnostics[[i]] <- tibble::tibble(  # Executes this documented step of the workflow.
      region = region_name,  # Executes this documented step of the workflow.
      Family = fam,  # Executes this documented step of the workflow.
      EnvDataset = env_dataset,  # Executes this documented step of the workflow.
      EnvColumn = env_column,  # Executes this documented step of the workflow.
      status = "ok",  # Executes this documented step of the workflow.
      n_matched_observations = nrow(pair_df),  # Executes this documented step of the workflow.
      lags_kept = paste(matched_lags, collapse = ",")  # Executes this documented step of the workflow.
    )  # Executes this documented step of the workflow.
  }  # Executes this documented step of the workflow.

  family_tests <- family_tests[!vapply(family_tests, is.null, logical(1))]  # Executes this documented step of the workflow.

  if (length(family_tests) == 0L) {  # Executes this documented step of the workflow.
    return(list(  # Executes this documented step of the workflow.
      status = "no_eligible_families",  # Executes this documented step of the workflow.
      family_diagnostics = dplyr::bind_rows(family_diagnostics),  # Executes this documented step of the workflow.
      usable_years = env_grid$usable_years,  # Executes this documented step of the workflow.
      common_months = env_grid$common_months  # Executes this documented step of the workflow.
    ))  # Executes this documented step of the workflow.
  }  # Executes this documented step of the workflow.

  list(  # Executes this documented step of the workflow.
    status = "ok",  # Executes this documented step of the workflow.
    EnvDataset = env_dataset,  # Executes this documented step of the workflow.
    EnvColumn = env_column,  # Executes this documented step of the workflow.
    environment_analysis_column = environment_analysis_column,  # Executes this documented step of the workflow.
    environment_was_deseasonalised = environment_was_deseasonalised,  # Executes this documented step of the workflow.
    grid = env_grid,  # Executes this documented step of the workflow.
    family_tests = family_tests,  # Executes this documented step of the workflow.
    family_diagnostics = dplyr::bind_rows(family_diagnostics),  # Executes this documented step of the workflow.
    usable_years = env_grid$usable_years,  # Executes this documented step of the workflow.
    common_months = env_grid$common_months  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.
}  # Executes this documented step of the workflow.

prepare_region_calendar <- function(region_name) {  # Executes this documented step of the workflow.

  region_list <- get_region_list_calendar(region_name)  # Executes this documented step of the workflow.

  insect_keys <- stationarity_insects_season_checked %>%  # Executes this documented step of the workflow.
    dplyr::filter(  # Executes this documented step of the workflow.
      region == region_name,  # Executes this documented step of the workflow.
      pass_stationarity,  # Executes this documented step of the workflow.
      !is.na(Family),  # Executes this documented step of the workflow.
      Family != "Fam."  # Executes this documented step of the workflow.
    ) %>%  # Executes this documented step of the workflow.
    dplyr::distinct(Family, analysis_column, was_deseasonalised)  # Executes this documented step of the workflow.

  env_keys <- stationarity_environment_season_checked %>%  # Executes this documented step of the workflow.
    dplyr::filter(region == region_name, pass_stationarity) %>%  # Executes this documented step of the workflow.
    dplyr::distinct(  # Executes this documented step of the workflow.
      EnvDataset,  # Executes this documented step of the workflow.
      EnvColumn,  # Executes this documented step of the workflow.
      analysis_column,  # Executes this documented step of the workflow.
      was_deseasonalised  # Executes this documented step of the workflow.
    )  # Executes this documented step of the workflow.

  environment_objects <- vector("list", nrow(env_keys))  # Executes this documented step of the workflow.
  environment_diagnostics <- vector("list", nrow(env_keys))  # Executes this documented step of the workflow.
  family_diagnostics <- vector("list", nrow(env_keys))  # Executes this documented step of the workflow.

  for (i in seq_len(nrow(env_keys))) {  # Executes this documented step of the workflow.
    key <- env_keys[i, , drop = FALSE]  # Executes this documented step of the workflow.

    prepared <- prepare_one_environment_calendar(  # Executes this documented step of the workflow.
      region_name = region_name,  # Executes this documented step of the workflow.
      region_list = region_list,  # Executes this documented step of the workflow.
      env_dataset = key$EnvDataset,  # Executes this documented step of the workflow.
      env_column = key$EnvColumn,  # Executes this documented step of the workflow.
      environment_analysis_column = key$analysis_column,  # Executes this documented step of the workflow.
      environment_was_deseasonalised = key$was_deseasonalised,  # Executes this documented step of the workflow.
      insect_keys = insect_keys  # Executes this documented step of the workflow.
    )  # Executes this documented step of the workflow.

    environment_objects[[i]] <- prepared  # Executes this documented step of the workflow.

    environment_diagnostics[[i]] <- tibble::tibble(  # Executes this documented step of the workflow.
      region = region_name,  # Executes this documented step of the workflow.
      EnvDataset = key$EnvDataset,  # Executes this documented step of the workflow.
      EnvColumn = key$EnvColumn,  # Executes this documented step of the workflow.
      environment_analysis_column = key$analysis_column,  # Executes this documented step of the workflow.
      environment_was_deseasonalised = key$was_deseasonalised,  # Executes this documented step of the workflow.
      status = prepared$status,  # Executes this documented step of the workflow.
      years_kept = if (!is.null(prepared$usable_years)) {  # Executes this documented step of the workflow.
        paste(prepared$usable_years, collapse = ",")  # Executes this documented step of the workflow.
      } else {  # Executes this documented step of the workflow.
        NA_character_  # Executes this documented step of the workflow.
      },  # Executes this documented step of the workflow.
      months_kept = if (!is.null(prepared$common_months)) {  # Executes this documented step of the workflow.
        paste(prepared$common_months, collapse = ",")  # Executes this documented step of the workflow.
      } else {  # Executes this documented step of the workflow.
        NA_character_  # Executes this documented step of the workflow.
      }  # Executes this documented step of the workflow.
    )  # Executes this documented step of the workflow.

    family_diagnostics[[i]] <- if (!is.null(prepared$family_diagnostics)) {  # Executes this documented step of the workflow.
      prepared$family_diagnostics  # Executes this documented step of the workflow.
    } else {  # Executes this documented step of the workflow.
      tibble::tibble()  # Executes this documented step of the workflow.
    }  # Executes this documented step of the workflow.
  }  # Executes this documented step of the workflow.

  valid_environment_objects <- environment_objects[  # Executes this documented step of the workflow.
    vapply(environment_objects, function(z) identical(z$status, "ok"), logical(1))  # Executes this documented step of the workflow.
  ]  # Executes this documented step of the workflow.

  list(  # Executes this documented step of the workflow.
    region = region_name,  # Executes this documented step of the workflow.
    environment_objects = valid_environment_objects,  # Executes this documented step of the workflow.
    environment_diagnostics = dplyr::bind_rows(environment_diagnostics),  # Executes this documented step of the workflow.
    family_diagnostics = dplyr::bind_rows(family_diagnostics)  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.
}  # Executes this documented step of the workflow.

regions_surrogate <- c("ALB", "HAI", "SCH")  # Executes this documented step of the workflow.

surrogate_designs <- stats::setNames(  # Executes this documented step of the workflow.
  lapply(regions_surrogate, prepare_region_calendar),  # Executes this documented step of the workflow.
  regions_surrogate  # Executes this documented step of the workflow.
)  # Executes this documented step of the workflow.

surrogate_environment_diagnostics <- dplyr::bind_rows(  # Executes this documented step of the workflow.
  lapply(surrogate_designs, `[[`, "environment_diagnostics")  # Executes this documented step of the workflow.
)  # Executes this documented step of the workflow.

surrogate_family_diagnostics <- dplyr::bind_rows(  # Executes this documented step of the workflow.
  lapply(surrogate_designs, `[[`, "family_diagnostics")  # Executes this documented step of the workflow.
)  # Executes this documented step of the workflow.

write.csv(  # Executes this documented step of the workflow.
  surrogate_environment_diagnostics,  # Executes this documented step of the workflow.
  file.path(out_surrogate_diagnostics, paste0("environment_grid_diagnostics_", surrogate_run_label, ".csv")),  # Executes this documented step of the workflow.
  row.names = FALSE  # Executes this documented step of the workflow.
)  # Executes this documented step of the workflow.

write.csv(  # Executes this documented step of the workflow.
  surrogate_family_diagnostics,  # Executes this documented step of the workflow.
  file.path(out_surrogate_diagnostics, paste0("family_matching_diagnostics_", surrogate_run_label, ".csv")),  # Executes this documented step of the workflow.
  row.names = FALSE  # Executes this documented step of the workflow.
)  # Executes this documented step of the workflow.

# ------------------------------------------------------------------------------
# 27.8 EXTRACT THE CORRECTED OBSERVED GRANGER TESTS
# ------------------------------------------------------------------------------

build_observed_calendar_tests <- function(region_design) {  # Executes this documented step of the workflow.

  rows <- list()  # Executes this documented step of the workflow.
  k <- 0L  # Executes this documented step of the workflow.

  for (env_object in region_design$environment_objects) {  # Executes this documented step of the workflow.
    for (family_object in env_object$family_tests) {  # Executes this documented step of the workflow.

      current <- family_object$observed %>%  # Executes this documented step of the workflow.
        dplyr::mutate(  # Executes this documented step of the workflow.
          region = region_design$region,  # Executes this documented step of the workflow.
          Family = family_object$Family,  # Executes this documented step of the workflow.
          EnvDataset = env_object$EnvDataset,  # Executes this documented step of the workflow.
          EnvColumn = env_object$EnvColumn,  # Executes this documented step of the workflow.
          insect_analysis_column = family_object$insect_analysis_column,  # Executes this documented step of the workflow.
          environment_analysis_column = env_object$environment_analysis_column,  # Executes this documented step of the workflow.
          insect_was_deseasonalised = family_object$insect_was_deseasonalised,  # Executes this documented step of the workflow.
          environment_was_deseasonalised = env_object$environment_was_deseasonalised,  # Executes this documented step of the workflow.
          .before = 1  # Executes this documented step of the workflow.
        )  # Executes this documented step of the workflow.

      current$test_id <- make_test_id_calendar(  # Executes this documented step of the workflow.
        current$region,  # Executes this documented step of the workflow.
        current$Family,  # Executes this documented step of the workflow.
        current$EnvDataset,  # Executes this documented step of the workflow.
        current$EnvColumn,  # Executes this documented step of the workflow.
        current$insect_analysis_column,  # Executes this documented step of the workflow.
        current$environment_analysis_column,  # Executes this documented step of the workflow.
        current$Lag  # Executes this documented step of the workflow.
      )  # Executes this documented step of the workflow.

      k <- k + 1L  # Executes this documented step of the workflow.
      rows[[k]] <- current  # Executes this documented step of the workflow.
    }  # Executes this documented step of the workflow.
  }  # Executes this documented step of the workflow.

  if (length(rows) == 0L) return(tibble::tibble())  # Executes this documented step of the workflow.

  dplyr::bind_rows(rows) %>%  # Executes this documented step of the workflow.
    add_var_class_calendar() %>%  # Executes this documented step of the workflow.
    dplyr::mutate(  # Executes this documented step of the workflow.
      causality = dplyr::if_else(p_value < surrogate_alpha, "causal", "non-causal")  # Executes this documented step of the workflow.
    )  # Executes this documented step of the workflow.
}  # Executes this documented step of the workflow.

observed_calendar_aware_tests <- dplyr::bind_rows(  # Executes this documented step of the workflow.
  lapply(surrogate_designs, build_observed_calendar_tests)  # Executes this documented step of the workflow.
)  # Executes this documented step of the workflow.

if (nrow(observed_calendar_aware_tests) == 0L) {  # Executes this documented step of the workflow.
  stop("No calendar-aware observed Granger tests were available for Section 27.")  # Executes this documented step of the workflow.
}  # Executes this documented step of the workflow.

write.csv(  # Executes this documented step of the workflow.
  observed_calendar_aware_tests,  # Executes this documented step of the workflow.
  file.path(out_surrogate_tables, paste0("observed_calendar_aware_granger_tests_", surrogate_run_label, ".csv")),  # Executes this documented step of the workflow.
  row.names = FALSE  # Executes this documented step of the workflow.
)  # Executes this documented step of the workflow.

# ------------------------------------------------------------------------------
# 27.9 CONVENTIONAL MULTIPLE-TESTING CORRECTIONS
# ------------------------------------------------------------------------------

observed_with_standard_adjustments <- observed_calendar_aware_tests %>%  # Executes this documented step of the workflow.
  dplyr::group_by(region) %>%  # Executes this documented step of the workflow.
  dplyr::mutate(  # Executes this documented step of the workflow.
    p_BH_region = stats::p.adjust(p_value, method = "BH"),  # Executes this documented step of the workflow.
    p_BY_region = stats::p.adjust(p_value, method = "BY"),  # Executes this documented step of the workflow.
    p_Holm_region = stats::p.adjust(p_value, method = "holm"),  # Executes this documented step of the workflow.
    p_Bonferroni_region = stats::p.adjust(p_value, method = "bonferroni")  # Executes this documented step of the workflow.
  ) %>%  # Executes this documented step of the workflow.
  dplyr::ungroup() %>%  # Executes this documented step of the workflow.
  dplyr::mutate(  # Executes this documented step of the workflow.
    p_BH_global = stats::p.adjust(p_value, method = "BH"),  # Executes this documented step of the workflow.
    p_BY_global = stats::p.adjust(p_value, method = "BY"),  # Executes this documented step of the workflow.
    p_Holm_global = stats::p.adjust(p_value, method = "holm"),  # Executes this documented step of the workflow.
    p_Bonferroni_global = stats::p.adjust(p_value, method = "bonferroni")  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.

write.csv(  # Executes this documented step of the workflow.
  observed_with_standard_adjustments,  # Executes this documented step of the workflow.
  file.path(out_surrogate_tables, paste0("observed_calendar_aware_with_standard_adjustments_", surrogate_run_label, ".csv")),  # Executes this documented step of the workflow.
  row.names = FALSE  # Executes this documented step of the workflow.
)  # Executes this documented step of the workflow.

# ------------------------------------------------------------------------------
# 27.10 SUMMARY HELPERS
# ------------------------------------------------------------------------------

compute_test_set_statistics_calendar <- function(test_rows) {  # Executes this documented step of the workflow.

  tested <- test_rows %>%  # Executes this documented step of the workflow.
    dplyr::filter(is.finite(p_value), is.finite(F_stat))  # Executes this documented step of the workflow.

  if (nrow(tested) == 0L) {  # Executes this documented step of the workflow.
    return(tibble::tibble(  # Executes this documented step of the workflow.
      n_tests = 0L,  # Executes this documented step of the workflow.
      n_significant = 0L,  # Executes this documented step of the workflow.
      pct_significant = NA_real_,  # Executes this documented step of the workflow.
      min_p = NA_real_,  # Executes this documented step of the workflow.
      max_F = NA_real_,  # Executes this documented step of the workflow.
      fisher_stat = NA_real_,  # Executes this documented step of the workflow.
      mean_neglog10_p = NA_real_,  # Executes this documented step of the workflow.
      n_BH_significant = 0L,  # Executes this documented step of the workflow.
      n_BY_significant = 0L,  # Executes this documented step of the workflow.
      n_Holm_significant = 0L,  # Executes this documented step of the workflow.
      n_Bonferroni_significant = 0L  # Executes this documented step of the workflow.
    ))  # Executes this documented step of the workflow.
  }  # Executes this documented step of the workflow.

  safe_p <- pmax(tested$p_value, .Machine$double.xmin)  # Executes this documented step of the workflow.

  tibble::tibble(  # Executes this documented step of the workflow.
    n_tests = nrow(tested),  # Executes this documented step of the workflow.
    n_significant = sum(tested$p_value < surrogate_alpha),  # Executes this documented step of the workflow.
    pct_significant = 100 * mean(tested$p_value < surrogate_alpha),  # Executes this documented step of the workflow.
    min_p = min(tested$p_value),  # Executes this documented step of the workflow.
    max_F = max(tested$F_stat),  # Executes this documented step of the workflow.
    fisher_stat = -2 * sum(log(safe_p)),  # Executes this documented step of the workflow.
    mean_neglog10_p = mean(-log10(safe_p)),  # Executes this documented step of the workflow.
    n_BH_significant = sum(stats::p.adjust(tested$p_value, method = "BH") < surrogate_alpha),  # Executes this documented step of the workflow.
    n_BY_significant = sum(stats::p.adjust(tested$p_value, method = "BY") < surrogate_alpha),  # Executes this documented step of the workflow.
    n_Holm_significant = sum(stats::p.adjust(tested$p_value, method = "holm") < surrogate_alpha),  # Executes this documented step of the workflow.
    n_Bonferroni_significant = sum(stats::p.adjust(tested$p_value, method = "bonferroni") < surrogate_alpha)  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.
}  # Executes this documented step of the workflow.

summarise_hit_rate_calendar <- function(test_rows, group_columns) {  # Executes this documented step of the workflow.
  test_rows %>%  # Executes this documented step of the workflow.
    dplyr::group_by(dplyr::across(dplyr::all_of(group_columns))) %>%  # Executes this documented step of the workflow.
    dplyr::summarise(  # Executes this documented step of the workflow.
      n_tests = dplyr::n(),  # Executes this documented step of the workflow.
      n_significant = sum(p_value < surrogate_alpha),  # Executes this documented step of the workflow.
      pct_significant = 100 * mean(p_value < surrogate_alpha),  # Executes this documented step of the workflow.
      .groups = "drop"  # Executes this documented step of the workflow.
    )  # Executes this documented step of the workflow.
}  # Executes this documented step of the workflow.

observed_global_statistics <- observed_calendar_aware_tests %>%  # Executes this documented step of the workflow.
  dplyr::group_by(region) %>%  # Executes this documented step of the workflow.
  dplyr::group_modify(~ compute_test_set_statistics_calendar(.x)) %>%  # Executes this documented step of the workflow.
  dplyr::ungroup()  # Executes this documented step of the workflow.

observed_by_lag <- summarise_hit_rate_calendar(  # Executes this documented step of the workflow.
  observed_calendar_aware_tests,  # Executes this documented step of the workflow.
  c("region", "Lag")  # Executes this documented step of the workflow.
) %>%  # Executes this documented step of the workflow.
  dplyr::rename(  # Executes this documented step of the workflow.
    n_tests_observed = n_tests,  # Executes this documented step of the workflow.
    n_significant_observed = n_significant,  # Executes this documented step of the workflow.
    pct_significant_observed = pct_significant  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.

observed_by_varclass <- summarise_hit_rate_calendar(  # Executes this documented step of the workflow.
  observed_calendar_aware_tests,  # Executes this documented step of the workflow.
  c("region", "var_class")  # Executes this documented step of the workflow.
) %>%  # Executes this documented step of the workflow.
  dplyr::rename(  # Executes this documented step of the workflow.
    n_tests_observed = n_tests,  # Executes this documented step of the workflow.
    n_significant_observed = n_significant,  # Executes this documented step of the workflow.
    pct_significant_observed = pct_significant  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.

observed_by_family <- summarise_hit_rate_calendar(  # Executes this documented step of the workflow.
  observed_calendar_aware_tests,  # Executes this documented step of the workflow.
  c("region", "Family")  # Executes this documented step of the workflow.
) %>%  # Executes this documented step of the workflow.
  dplyr::rename(  # Executes this documented step of the workflow.
    n_tests_observed = n_tests,  # Executes this documented step of the workflow.
    n_significant_observed = n_significant,  # Executes this documented step of the workflow.
    pct_significant_observed = pct_significant  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.

observed_by_environment <- summarise_hit_rate_calendar(  # Executes this documented step of the workflow.
  observed_calendar_aware_tests,  # Executes this documented step of the workflow.
  c("region", "EnvDataset", "EnvColumn")  # Executes this documented step of the workflow.
) %>%  # Executes this documented step of the workflow.
  dplyr::rename(  # Executes this documented step of the workflow.
    n_tests_observed = n_tests,  # Executes this documented step of the workflow.
    n_significant_observed = n_significant,  # Executes this documented step of the workflow.
    pct_significant_observed = pct_significant  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.

# ------------------------------------------------------------------------------
# 27.11 RUN ONE COMPLETE SURROGATE REALISATION FOR ONE REGION
# ------------------------------------------------------------------------------

run_surrogate_region_once_calendar <- function(  # Executes this documented step of the workflow.
    region_design,  # Executes this documented step of the workflow.
    method,  # Executes this documented step of the workflow.
    collect_quality = FALSE) {  # Executes this documented step of the workflow.

  test_rows <- list()  # Executes this documented step of the workflow.
  quality_rows <- list()  # Executes this documented step of the workflow.
  test_counter <- 0L  # Executes this documented step of the workflow.
  quality_counter <- 0L  # Executes this documented step of the workflow.

  for (env_object in region_design$environment_objects) {  # Executes this documented step of the workflow.

    grid <- env_object$grid$data  # Executes this documented step of the workflow.

    surrogate_env <- generate_surrogate_calendar(  # Executes this documented step of the workflow.
      x = grid$value,  # Executes this documented step of the workflow.
      year = grid$year,  # Executes this documented step of the workflow.
      month = grid$month,  # Executes this documented step of the workflow.
      method = method  # Executes this documented step of the workflow.
    )  # Executes this documented step of the workflow.

    if (collect_quality && all(is.finite(surrogate_env))) {  # Executes this documented step of the workflow.
      quality_counter <- quality_counter + 1L  # Executes this documented step of the workflow.
      quality_rows[[quality_counter]] <- surrogate_quality_metrics_calendar(  # Executes this documented step of the workflow.
        observed = grid$value,  # Executes this documented step of the workflow.
        surrogate = surrogate_env,  # Executes this documented step of the workflow.
        year = grid$year,  # Executes this documented step of the workflow.
        month = grid$month  # Executes this documented step of the workflow.
      ) %>%  # Executes this documented step of the workflow.
        dplyr::mutate(  # Executes this documented step of the workflow.
          region = region_design$region,  # Executes this documented step of the workflow.
          surrogate_method = method,  # Executes this documented step of the workflow.
          EnvDataset = env_object$EnvDataset,  # Executes this documented step of the workflow.
          EnvColumn = env_object$EnvColumn,  # Executes this documented step of the workflow.
          .before = 1  # Executes this documented step of the workflow.
        )  # Executes this documented step of the workflow.
    }  # Executes this documented step of the workflow.

    for (family_object in env_object$family_tests) {  # Executes this documented step of the workflow.

      pair_df <- family_object$data  # Executes this documented step of the workflow.
      env_values <- surrogate_env[pair_df$env_index]  # Executes this documented step of the workflow.

      result <- calendar_granger_safe(  # Executes this documented step of the workflow.
        x = pair_df$insect,  # Executes this documented step of the workflow.
        y = env_values,  # Executes this documented step of the workflow.
        year = pair_df$year,  # Executes this documented step of the workflow.
        month = pair_df$month,  # Executes this documented step of the workflow.
        lags = family_object$lags  # Executes this documented step of the workflow.
      ) %>%  # Executes this documented step of the workflow.
        dplyr::filter(test_status == "tested", is.finite(p_value), is.finite(F_stat)) %>%  # Executes this documented step of the workflow.
        dplyr::mutate(  # Executes this documented step of the workflow.
          region = region_design$region,  # Executes this documented step of the workflow.
          Family = family_object$Family,  # Executes this documented step of the workflow.
          EnvDataset = env_object$EnvDataset,  # Executes this documented step of the workflow.
          EnvColumn = env_object$EnvColumn,  # Executes this documented step of the workflow.
          insect_analysis_column = family_object$insect_analysis_column,  # Executes this documented step of the workflow.
          environment_analysis_column = env_object$environment_analysis_column,  # Executes this documented step of the workflow.
          .before = 1  # Executes this documented step of the workflow.
        )  # Executes this documented step of the workflow.

      if (nrow(result) == 0L) next  # Executes this documented step of the workflow.

      result$test_id <- make_test_id_calendar(  # Executes this documented step of the workflow.
        result$region,  # Executes this documented step of the workflow.
        result$Family,  # Executes this documented step of the workflow.
        result$EnvDataset,  # Executes this documented step of the workflow.
        result$EnvColumn,  # Executes this documented step of the workflow.
        result$insect_analysis_column,  # Executes this documented step of the workflow.
        result$environment_analysis_column,  # Executes this documented step of the workflow.
        result$Lag  # Executes this documented step of the workflow.
      )  # Executes this documented step of the workflow.

      test_counter <- test_counter + 1L  # Executes this documented step of the workflow.
      test_rows[[test_counter]] <- result  # Executes this documented step of the workflow.
    }  # Executes this documented step of the workflow.
  }  # Executes this documented step of the workflow.

  tests <- if (length(test_rows) > 0L) {  # Executes this documented step of the workflow.
    dplyr::bind_rows(test_rows) %>% add_var_class_calendar()  # Executes this documented step of the workflow.
  } else {  # Executes this documented step of the workflow.
    tibble::tibble()  # Executes this documented step of the workflow.
  }  # Executes this documented step of the workflow.

  quality <- if (length(quality_rows) > 0L) {  # Executes this documented step of the workflow.
    dplyr::bind_rows(quality_rows)  # Executes this documented step of the workflow.
  } else {  # Executes this documented step of the workflow.
    tibble::tibble()  # Executes this documented step of the workflow.
  }  # Executes this documented step of the workflow.

  list(tests = tests, quality = quality)  # Executes this documented step of the workflow.
}  # Executes this documented step of the workflow.

# ------------------------------------------------------------------------------
# 27.12 REPEATED SURROGATE ANALYSIS
# ------------------------------------------------------------------------------
# The code stores global/subgroup summaries for every repetition, but does not
# store millions of raw surrogate test rows. Testwise empirical p-values are
# accumulated with counters, which keeps memory use manageable.

surrogate_started_at <- Sys.time()  # Executes this documented step of the workflow.

global_draws_list <- list()  # Executes this documented step of the workflow.
lag_draws_list <- list()  # Executes this documented step of the workflow.
varclass_draws_list <- list()  # Executes this documented step of the workflow.
family_draws_list <- list()  # Executes this documented step of the workflow.
environment_draws_list <- list()  # Executes this documented step of the workflow.
quality_list <- list()  # Executes this documented step of the workflow.
testwise_list <- list()  # Executes this documented step of the workflow.

global_i <- 0L  # Executes this documented step of the workflow.
lag_i <- 0L  # Executes this documented step of the workflow.
varclass_i <- 0L  # Executes this documented step of the workflow.
family_i <- 0L  # Executes this documented step of the workflow.
environment_i <- 0L  # Executes this documented step of the workflow.
quality_i <- 0L  # Executes this documented step of the workflow.
testwise_i <- 0L  # Executes this documented step of the workflow.

for (method in surrogate_methods_to_run) {  # Executes this documented step of the workflow.

  message("Section 27 surrogate method: ", method)  # Executes this documented step of the workflow.

  for (region_name in regions_surrogate) {  # Executes this documented step of the workflow.

    region_design <- surrogate_designs[[region_name]]  # Executes this documented step of the workflow.
    observed_region <- observed_calendar_aware_tests %>%  # Executes this documented step of the workflow.
      dplyr::filter(region == region_name) %>%  # Executes this documented step of the workflow.
      dplyr::arrange(test_id)  # Executes this documented step of the workflow.

    n_observed <- nrow(observed_region)  # Executes this documented step of the workflow.

    if (n_observed == 0L) next  # Executes this documented step of the workflow.

    n_valid_testwise <- integer(n_observed)  # Executes this documented step of the workflow.
    n_F_ge_observed <- integer(n_observed)  # Executes this documented step of the workflow.
    n_valid_maxF <- integer(n_observed)  # Executes this documented step of the workflow.
    n_maxF_ge_observed <- integer(n_observed)  # Executes this documented step of the workflow.
    n_valid_minP <- integer(n_observed)  # Executes this documented step of the workflow.
    n_minP_le_observed <- integer(n_observed)  # Executes this documented step of the workflow.

    for (replicate_id in seq_len(surrogate_n_repeats)) {  # Executes this documented step of the workflow.

      surrogate_result <- run_surrogate_region_once_calendar(  # Executes this documented step of the workflow.
        region_design = region_design,  # Executes this documented step of the workflow.
        method = method,  # Executes this documented step of the workflow.
        collect_quality = replicate_id <= surrogate_quality_reps  # Executes this documented step of the workflow.
      )  # Executes this documented step of the workflow.

      test_rows <- surrogate_result$tests  # Executes this documented step of the workflow.

      if (nrow(test_rows) == 0L) next  # Executes this documented step of the workflow.

      # ---------- testwise empirical and FWER counters ----------
      match_index <- match(observed_region$test_id, test_rows$test_id)  # Executes this documented step of the workflow.
      matched <- !is.na(match_index)  # Executes this documented step of the workflow.

      surrogate_F <- rep(NA_real_, n_observed)  # Executes this documented step of the workflow.
      surrogate_p <- rep(NA_real_, n_observed)  # Executes this documented step of the workflow.
      surrogate_F[matched] <- test_rows$F_stat[match_index[matched]]  # Executes this documented step of the workflow.
      surrogate_p[matched] <- test_rows$p_value[match_index[matched]]  # Executes this documented step of the workflow.

      valid_testwise <- matched & is.finite(surrogate_F)  # Executes this documented step of the workflow.
      n_valid_testwise <- n_valid_testwise + as.integer(valid_testwise)  # Executes this documented step of the workflow.
      n_F_ge_observed <- n_F_ge_observed + as.integer(  # Executes this documented step of the workflow.
        valid_testwise & surrogate_F >= observed_region$F_stat  # Executes this documented step of the workflow.
      )  # Executes this documented step of the workflow.

      finite_F <- test_rows$F_stat[is.finite(test_rows$F_stat)]  # Executes this documented step of the workflow.
      finite_p <- test_rows$p_value[is.finite(test_rows$p_value)]  # Executes this documented step of the workflow.

      if (length(finite_F) > 0L) {  # Executes this documented step of the workflow.
        max_F_rep <- max(finite_F)  # Executes this documented step of the workflow.
        n_valid_maxF <- n_valid_maxF + 1L  # Executes this documented step of the workflow.
        n_maxF_ge_observed <- n_maxF_ge_observed + as.integer(  # Executes this documented step of the workflow.
          max_F_rep >= observed_region$F_stat  # Executes this documented step of the workflow.
        )  # Executes this documented step of the workflow.
      }  # Executes this documented step of the workflow.

      if (length(finite_p) > 0L) {  # Executes this documented step of the workflow.
        min_p_rep <- min(finite_p)  # Executes this documented step of the workflow.
        n_valid_minP <- n_valid_minP + 1L  # Executes this documented step of the workflow.
        n_minP_le_observed <- n_minP_le_observed + as.integer(  # Executes this documented step of the workflow.
          min_p_rep <= observed_region$p_value  # Executes this documented step of the workflow.
        )  # Executes this documented step of the workflow.
      }  # Executes this documented step of the workflow.

      # ---------- global statistics ----------
      global_i <- global_i + 1L  # Executes this documented step of the workflow.
      global_draws_list[[global_i]] <- compute_test_set_statistics_calendar(test_rows) %>%  # Executes this documented step of the workflow.
        dplyr::mutate(  # Executes this documented step of the workflow.
          region = region_name,  # Executes this documented step of the workflow.
          surrogate_method = method,  # Executes this documented step of the workflow.
          repetition = replicate_id,  # Executes this documented step of the workflow.
          .before = 1  # Executes this documented step of the workflow.
        )  # Executes this documented step of the workflow.

      # ---------- subgroup hit rates ----------
      lag_i <- lag_i + 1L  # Executes this documented step of the workflow.
      lag_draws_list[[lag_i]] <- summarise_hit_rate_calendar(  # Executes this documented step of the workflow.
        test_rows,  # Executes this documented step of the workflow.
        c("region", "Lag")  # Executes this documented step of the workflow.
      ) %>%  # Executes this documented step of the workflow.
        dplyr::mutate(  # Executes this documented step of the workflow.
          surrogate_method = method,  # Executes this documented step of the workflow.
          repetition = replicate_id,  # Executes this documented step of the workflow.
          .before = 1  # Executes this documented step of the workflow.
        )  # Executes this documented step of the workflow.

      varclass_i <- varclass_i + 1L  # Executes this documented step of the workflow.
      varclass_draws_list[[varclass_i]] <- summarise_hit_rate_calendar(  # Executes this documented step of the workflow.
        test_rows,  # Executes this documented step of the workflow.
        c("region", "var_class")  # Executes this documented step of the workflow.
      ) %>%  # Executes this documented step of the workflow.
        dplyr::mutate(  # Executes this documented step of the workflow.
          surrogate_method = method,  # Executes this documented step of the workflow.
          repetition = replicate_id,  # Executes this documented step of the workflow.
          .before = 1  # Executes this documented step of the workflow.
        )  # Executes this documented step of the workflow.

      family_i <- family_i + 1L  # Executes this documented step of the workflow.
      family_draws_list[[family_i]] <- summarise_hit_rate_calendar(  # Executes this documented step of the workflow.
        test_rows,  # Executes this documented step of the workflow.
        c("region", "Family")  # Executes this documented step of the workflow.
      ) %>%  # Executes this documented step of the workflow.
        dplyr::mutate(  # Executes this documented step of the workflow.
          surrogate_method = method,  # Executes this documented step of the workflow.
          repetition = replicate_id,  # Executes this documented step of the workflow.
          .before = 1  # Executes this documented step of the workflow.
        )  # Executes this documented step of the workflow.

      environment_i <- environment_i + 1L  # Executes this documented step of the workflow.
      environment_draws_list[[environment_i]] <- summarise_hit_rate_calendar(  # Executes this documented step of the workflow.
        test_rows,  # Executes this documented step of the workflow.
        c("region", "EnvDataset", "EnvColumn")  # Executes this documented step of the workflow.
      ) %>%  # Executes this documented step of the workflow.
        dplyr::mutate(  # Executes this documented step of the workflow.
          surrogate_method = method,  # Executes this documented step of the workflow.
          repetition = replicate_id,  # Executes this documented step of the workflow.
          .before = 1  # Executes this documented step of the workflow.
        )  # Executes this documented step of the workflow.

      # ---------- quality diagnostics ----------
      if (nrow(surrogate_result$quality) > 0L) {  # Executes this documented step of the workflow.
        quality_i <- quality_i + 1L  # Executes this documented step of the workflow.
        quality_list[[quality_i]] <- surrogate_result$quality %>%  # Executes this documented step of the workflow.
          dplyr::mutate(repetition = replicate_id, .before = 1)  # Executes this documented step of the workflow.
      }  # Executes this documented step of the workflow.
    }  # Executes this documented step of the workflow.

    # ---------- final testwise empirical p-values for this region/method ----------
    empirical_testwise_F_p <- ifelse(  # Executes this documented step of the workflow.
      n_valid_testwise > 0L,  # Executes this documented step of the workflow.
      (1 + n_F_ge_observed) / (1 + n_valid_testwise),  # Executes this documented step of the workflow.
      NA_real_  # Executes this documented step of the workflow.
    )  # Executes this documented step of the workflow.

    maxF_FWER_p <- ifelse(  # Executes this documented step of the workflow.
      n_valid_maxF > 0L,  # Executes this documented step of the workflow.
      (1 + n_maxF_ge_observed) / (1 + n_valid_maxF),  # Executes this documented step of the workflow.
      NA_real_  # Executes this documented step of the workflow.
    )  # Executes this documented step of the workflow.

    minP_FWER_p <- ifelse(  # Executes this documented step of the workflow.
      n_valid_minP > 0L,  # Executes this documented step of the workflow.
      (1 + n_minP_le_observed) / (1 + n_valid_minP),  # Executes this documented step of the workflow.
      NA_real_  # Executes this documented step of the workflow.
    )  # Executes this documented step of the workflow.

    testwise_i <- testwise_i + 1L  # Executes this documented step of the workflow.
    testwise_list[[testwise_i]] <- observed_region %>%  # Executes this documented step of the workflow.
      dplyr::mutate(  # Executes this documented step of the workflow.
        surrogate_method = method,  # Executes this documented step of the workflow.
        n_surrogates_testwise = n_valid_testwise,  # Executes this documented step of the workflow.
        empirical_testwise_F_p = empirical_testwise_F_p,  # Executes this documented step of the workflow.
        maxF_FWER_p = maxF_FWER_p,  # Executes this documented step of the workflow.
        minP_FWER_p = minP_FWER_p  # Executes this documented step of the workflow.
      ) %>%  # Executes this documented step of the workflow.
      dplyr::group_by(region, surrogate_method) %>%  # Executes this documented step of the workflow.
      dplyr::mutate(  # Executes this documented step of the workflow.
        surrogate_BH_p = stats::p.adjust(empirical_testwise_F_p, method = "BH"),  # Executes this documented step of the workflow.
        surrogate_BY_p = stats::p.adjust(empirical_testwise_F_p, method = "BY"),  # Executes this documented step of the workflow.
        surrogate_Holm_p = stats::p.adjust(empirical_testwise_F_p, method = "holm"),  # Executes this documented step of the workflow.
        surrogate_Bonferroni_p = stats::p.adjust(empirical_testwise_F_p, method = "bonferroni")  # Executes this documented step of the workflow.
      ) %>%  # Executes this documented step of the workflow.
      dplyr::ungroup()  # Executes this documented step of the workflow.
  }  # Executes this documented step of the workflow.
}  # Executes this documented step of the workflow.

surrogate_finished_at <- Sys.time()  # Executes this documented step of the workflow.

surrogate_global_draws <- dplyr::bind_rows(global_draws_list)  # Executes this documented step of the workflow.
surrogate_lag_draws <- dplyr::bind_rows(lag_draws_list)  # Executes this documented step of the workflow.
surrogate_varclass_draws <- dplyr::bind_rows(varclass_draws_list)  # Executes this documented step of the workflow.
surrogate_family_draws <- dplyr::bind_rows(family_draws_list)  # Executes this documented step of the workflow.
surrogate_environment_draws <- dplyr::bind_rows(environment_draws_list)  # Executes this documented step of the workflow.
surrogate_quality_diagnostics <- dplyr::bind_rows(quality_list)  # Executes this documented step of the workflow.
surrogate_testwise_adjustments <- dplyr::bind_rows(testwise_list)  # Executes this documented step of the workflow.

# ------------------------------------------------------------------------------
# 27.13 GLOBAL SURROGATE-NULL INFERENCE
# ------------------------------------------------------------------------------

metric_directions <- tibble::tibble(  # Executes this documented step of the workflow.
  metric = c(  # Executes this documented step of the workflow.
    "pct_significant",  # Executes this documented step of the workflow.
    "n_significant",  # Executes this documented step of the workflow.
    "min_p",  # Executes this documented step of the workflow.
    "max_F",  # Executes this documented step of the workflow.
    "fisher_stat",  # Executes this documented step of the workflow.
    "mean_neglog10_p",  # Executes this documented step of the workflow.
    "n_BH_significant",  # Executes this documented step of the workflow.
    "n_BY_significant",  # Executes this documented step of the workflow.
    "n_Holm_significant",  # Executes this documented step of the workflow.
    "n_Bonferroni_significant"  # Executes this documented step of the workflow.
  ),  # Executes this documented step of the workflow.
  direction = c(  # Executes this documented step of the workflow.
    "upper",  # Executes this documented step of the workflow.
    "upper",  # Executes this documented step of the workflow.
    "lower",  # Executes this documented step of the workflow.
    "upper",  # Executes this documented step of the workflow.
    "upper",  # Executes this documented step of the workflow.
    "upper",  # Executes this documented step of the workflow.
    "upper",  # Executes this documented step of the workflow.
    "upper",  # Executes this documented step of the workflow.
    "upper",  # Executes this documented step of the workflow.
    "upper"  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.
)  # Executes this documented step of the workflow.

observed_global_long <- observed_global_statistics %>%  # Executes this documented step of the workflow.
  tidyr::pivot_longer(  # Executes this documented step of the workflow.
    cols = dplyr::all_of(metric_directions$metric),  # Executes this documented step of the workflow.
    names_to = "metric",  # Executes this documented step of the workflow.
    values_to = "observed_value"  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.

surrogate_global_long <- surrogate_global_draws %>%  # Executes this documented step of the workflow.
  tidyr::pivot_longer(  # Executes this documented step of the workflow.
    cols = dplyr::all_of(metric_directions$metric),  # Executes this documented step of the workflow.
    names_to = "metric",  # Executes this documented step of the workflow.
    values_to = "surrogate_value"  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.

surrogate_global_summary <- surrogate_global_long %>%  # Executes this documented step of the workflow.
  dplyr::left_join(metric_directions, by = "metric") %>%  # Executes this documented step of the workflow.
  dplyr::left_join(observed_global_long, by = c("region", "metric")) %>%  # Executes this documented step of the workflow.
  dplyr::group_by(region, surrogate_method, metric, direction, observed_value) %>%  # Executes this documented step of the workflow.
  dplyr::summarise(  # Executes this documented step of the workflow.
    n_surrogates = sum(is.finite(surrogate_value)),  # Executes this documented step of the workflow.
    null_mean = mean(surrogate_value, na.rm = TRUE),  # Executes this documented step of the workflow.
    null_sd = stats::sd(surrogate_value, na.rm = TRUE),  # Executes this documented step of the workflow.
    null_median = stats::median(surrogate_value, na.rm = TRUE),  # Executes this documented step of the workflow.
    null_q025 = stats::quantile(surrogate_value, 0.025, na.rm = TRUE, names = FALSE),  # Executes this documented step of the workflow.
    null_q975 = stats::quantile(surrogate_value, 0.975, na.rm = TRUE, names = FALSE),  # Executes this documented step of the workflow.
    empirical_p = dplyr::if_else(  # Executes this documented step of the workflow.
      dplyr::first(direction) == "upper",  # Executes this documented step of the workflow.
      (1 + sum(surrogate_value >= dplyr::first(observed_value), na.rm = TRUE)) /  # Executes this documented step of the workflow.
        (1 + sum(is.finite(surrogate_value))),  # Executes this documented step of the workflow.
      (1 + sum(surrogate_value <= dplyr::first(observed_value), na.rm = TRUE)) /  # Executes this documented step of the workflow.
        (1 + sum(is.finite(surrogate_value)))  # Executes this documented step of the workflow.
    ),  # Executes this documented step of the workflow.
    observed_percentile_in_null = 100 * mean(  # Executes this documented step of the workflow.
      surrogate_value <= dplyr::first(observed_value),  # Executes this documented step of the workflow.
      na.rm = TRUE  # Executes this documented step of the workflow.
    ),  # Executes this documented step of the workflow.
    .groups = "drop"  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.

write.csv(  # Executes this documented step of the workflow.
  surrogate_global_draws,  # Executes this documented step of the workflow.
  file.path(out_surrogate_tables, paste0("surrogate_global_null_draws_", surrogate_run_label, ".csv")),  # Executes this documented step of the workflow.
  row.names = FALSE  # Executes this documented step of the workflow.
)  # Executes this documented step of the workflow.

write.csv(  # Executes this documented step of the workflow.
  surrogate_global_summary,  # Executes this documented step of the workflow.
  file.path(out_surrogate_tables, paste0("surrogate_global_null_summary_", surrogate_run_label, ".csv")),  # Executes this documented step of the workflow.
  row.names = FALSE  # Executes this documented step of the workflow.
)  # Executes this documented step of the workflow.

# ------------------------------------------------------------------------------
# 27.14 SUBGROUP SURROGATE SUMMARIES
# ------------------------------------------------------------------------------

summarise_group_surrogates_calendar <- function(draws, observed, group_columns) {  # Executes this documented step of the workflow.

  joined <- dplyr::left_join(  # Executes this documented step of the workflow.
    draws,  # Executes this documented step of the workflow.
    observed,  # Executes this documented step of the workflow.
    by = c("region", group_columns)  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.

  grouping_columns <- c(  # Executes this documented step of the workflow.
    "region",  # Executes this documented step of the workflow.
    "surrogate_method",  # Executes this documented step of the workflow.
    group_columns,  # Executes this documented step of the workflow.
    "pct_significant_observed",  # Executes this documented step of the workflow.
    "n_tests_observed",  # Executes this documented step of the workflow.
    "n_significant_observed"  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.

  joined %>%  # Executes this documented step of the workflow.
    dplyr::group_by(dplyr::across(dplyr::all_of(grouping_columns))) %>%  # Executes this documented step of the workflow.
    dplyr::summarise(  # Executes this documented step of the workflow.
      n_surrogates = sum(is.finite(pct_significant)),  # Executes this documented step of the workflow.
      null_mean_pct = mean(pct_significant, na.rm = TRUE),  # Executes this documented step of the workflow.
      null_sd_pct = stats::sd(pct_significant, na.rm = TRUE),  # Executes this documented step of the workflow.
      null_q025_pct = stats::quantile(pct_significant, 0.025, na.rm = TRUE, names = FALSE),  # Executes this documented step of the workflow.
      null_q975_pct = stats::quantile(pct_significant, 0.975, na.rm = TRUE, names = FALSE),  # Executes this documented step of the workflow.
      empirical_p_upper = (  # Executes this documented step of the workflow.
        1 + sum(pct_significant >= dplyr::first(pct_significant_observed), na.rm = TRUE)  # Executes this documented step of the workflow.
      ) / (  # Executes this documented step of the workflow.
        1 + sum(is.finite(pct_significant))  # Executes this documented step of the workflow.
      ),  # Executes this documented step of the workflow.
      observed_minus_null_mean = dplyr::first(pct_significant_observed) -  # Executes this documented step of the workflow.
        mean(pct_significant, na.rm = TRUE),  # Executes this documented step of the workflow.
      .groups = "drop"  # Executes this documented step of the workflow.
    )  # Executes this documented step of the workflow.
}  # Executes this documented step of the workflow.

surrogate_lag_summary <- summarise_group_surrogates_calendar(  # Executes this documented step of the workflow.
  surrogate_lag_draws,  # Executes this documented step of the workflow.
  observed_by_lag,  # Executes this documented step of the workflow.
  "Lag"  # Executes this documented step of the workflow.
)  # Executes this documented step of the workflow.

surrogate_varclass_summary <- summarise_group_surrogates_calendar(  # Executes this documented step of the workflow.
  surrogate_varclass_draws,  # Executes this documented step of the workflow.
  observed_by_varclass,  # Executes this documented step of the workflow.
  "var_class"  # Executes this documented step of the workflow.
)  # Executes this documented step of the workflow.

surrogate_family_summary <- summarise_group_surrogates_calendar(  # Executes this documented step of the workflow.
  surrogate_family_draws,  # Executes this documented step of the workflow.
  observed_by_family,  # Executes this documented step of the workflow.
  "Family"  # Executes this documented step of the workflow.
)  # Executes this documented step of the workflow.

surrogate_environment_summary <- summarise_group_surrogates_calendar(  # Executes this documented step of the workflow.
  surrogate_environment_draws,  # Executes this documented step of the workflow.
  observed_by_environment,  # Executes this documented step of the workflow.
  c("EnvDataset", "EnvColumn")  # Executes this documented step of the workflow.
)  # Executes this documented step of the workflow.

write.csv(  # Executes this documented step of the workflow.
  surrogate_lag_summary,  # Executes this documented step of the workflow.
  file.path(out_surrogate_tables, paste0("surrogate_summary_by_lag_", surrogate_run_label, ".csv")),  # Executes this documented step of the workflow.
  row.names = FALSE  # Executes this documented step of the workflow.
)  # Executes this documented step of the workflow.

write.csv(  # Executes this documented step of the workflow.
  surrogate_varclass_summary,  # Executes this documented step of the workflow.
  file.path(out_surrogate_tables, paste0("surrogate_summary_by_varclass_", surrogate_run_label, ".csv")),  # Executes this documented step of the workflow.
  row.names = FALSE  # Executes this documented step of the workflow.
)  # Executes this documented step of the workflow.

write.csv(  # Executes this documented step of the workflow.
  surrogate_family_summary,  # Executes this documented step of the workflow.
  file.path(out_surrogate_tables, paste0("surrogate_summary_by_family_", surrogate_run_label, ".csv")),  # Executes this documented step of the workflow.
  row.names = FALSE  # Executes this documented step of the workflow.
)  # Executes this documented step of the workflow.

write.csv(  # Executes this documented step of the workflow.
  surrogate_environment_summary,  # Executes this documented step of the workflow.
  file.path(out_surrogate_tables, paste0("surrogate_summary_by_environmental_variable_", surrogate_run_label, ".csv")),  # Executes this documented step of the workflow.
  row.names = FALSE  # Executes this documented step of the workflow.
)  # Executes this documented step of the workflow.

# ------------------------------------------------------------------------------
# 27.15 INDIVIDUAL-TEST SURROGATE CORRECTIONS
# ------------------------------------------------------------------------------

write.csv(  # Executes this documented step of the workflow.
  surrogate_testwise_adjustments,  # Executes this documented step of the workflow.
  file.path(out_surrogate_tables, paste0("surrogate_testwise_adjustments_", surrogate_run_label, ".csv")),  # Executes this documented step of the workflow.
  row.names = FALSE  # Executes this documented step of the workflow.
)  # Executes this documented step of the workflow.

surrogate_significant_counts <- surrogate_testwise_adjustments %>%  # Executes this documented step of the workflow.
  dplyr::group_by(region, surrogate_method) %>%  # Executes this documented step of the workflow.
  dplyr::summarise(  # Executes this documented step of the workflow.
    n_testwise_empirical = sum(empirical_testwise_F_p < surrogate_alpha, na.rm = TRUE),  # Executes this documented step of the workflow.
    n_maxF_FWER = sum(maxF_FWER_p < surrogate_alpha, na.rm = TRUE),  # Executes this documented step of the workflow.
    n_minP_FWER = sum(minP_FWER_p < surrogate_alpha, na.rm = TRUE),  # Executes this documented step of the workflow.
    n_surrogate_BH = sum(surrogate_BH_p < surrogate_alpha, na.rm = TRUE),  # Executes this documented step of the workflow.
    n_surrogate_BY = sum(surrogate_BY_p < surrogate_alpha, na.rm = TRUE),  # Executes this documented step of the workflow.
    n_surrogate_Holm = sum(surrogate_Holm_p < surrogate_alpha, na.rm = TRUE),  # Executes this documented step of the workflow.
    n_surrogate_Bonferroni = sum(surrogate_Bonferroni_p < surrogate_alpha, na.rm = TRUE),  # Executes this documented step of the workflow.
    .groups = "drop"  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.

write.csv(  # Executes this documented step of the workflow.
  surrogate_significant_counts,  # Executes this documented step of the workflow.
  file.path(out_surrogate_tables, paste0("surrogate_adjusted_significant_test_counts_", surrogate_run_label, ".csv")),  # Executes this documented step of the workflow.
  row.names = FALSE  # Executes this documented step of the workflow.
)  # Executes this documented step of the workflow.

surrogate_method_agreement <- surrogate_global_summary %>%  # Executes this documented step of the workflow.
  dplyr::filter(metric == "pct_significant") %>%  # Executes this documented step of the workflow.
  dplyr::mutate(reject_global_null = empirical_p < surrogate_alpha) %>%  # Executes this documented step of the workflow.
  dplyr::select(region, surrogate_method, reject_global_null) %>%  # Executes this documented step of the workflow.
  tidyr::pivot_wider(  # Executes this documented step of the workflow.
    names_from = surrogate_method,  # Executes this documented step of the workflow.
    values_from = reject_global_null,  # Executes this documented step of the workflow.
    names_prefix = "reject_"  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.

write.csv(  # Executes this documented step of the workflow.
  surrogate_method_agreement,  # Executes this documented step of the workflow.
  file.path(out_surrogate_tables, paste0("surrogate_method_agreement_", surrogate_run_label, ".csv")),  # Executes this documented step of the workflow.
  row.names = FALSE  # Executes this documented step of the workflow.
)  # Executes this documented step of the workflow.

# ------------------------------------------------------------------------------
# 27.16 SURROGATE QUALITY DIAGNOSTICS
# ------------------------------------------------------------------------------

write.csv(  # Executes this documented step of the workflow.
  surrogate_quality_diagnostics,  # Executes this documented step of the workflow.
  file.path(out_surrogate_diagnostics, paste0("surrogate_quality_diagnostics_", surrogate_run_label, ".csv")),  # Executes this documented step of the workflow.
  row.names = FALSE  # Executes this documented step of the workflow.
)  # Executes this documented step of the workflow.

surrogate_quality_summary <- if (nrow(surrogate_quality_diagnostics) > 0L) {  # Executes this documented step of the workflow.
  surrogate_quality_diagnostics %>%  # Executes this documented step of the workflow.
    dplyr::group_by(region, surrogate_method) %>%  # Executes this documented step of the workflow.
    dplyr::summarise(  # Executes this documented step of the workflow.
      mean_abs_mean_difference = mean(abs(mean_difference), na.rm = TRUE),  # Executes this documented step of the workflow.
      mean_abs_sd_ratio_minus_1 = mean(abs(sd_ratio - 1), na.rm = TRUE),  # Executes this documented step of the workflow.
      mean_quantile_RMSE = mean(quantile_RMSE, na.rm = TRUE),  # Executes this documented step of the workflow.
      mean_calendar_acf_RMSE = mean(calendar_acf_RMSE, na.rm = TRUE),  # Executes this documented step of the workflow.
      mean_monthly_mean_correlation = mean(monthly_mean_correlation, na.rm = TRUE),  # Executes this documented step of the workflow.
      .groups = "drop"  # Executes this documented step of the workflow.
    )  # Executes this documented step of the workflow.
} else {  # Executes this documented step of the workflow.
  tibble::tibble()  # Executes this documented step of the workflow.
}  # Executes this documented step of the workflow.

write.csv(  # Executes this documented step of the workflow.
  surrogate_quality_summary,  # Executes this documented step of the workflow.
  file.path(out_surrogate_diagnostics, paste0("surrogate_quality_summary_", surrogate_run_label, ".csv")),  # Executes this documented step of the workflow.
  row.names = FALSE  # Executes this documented step of the workflow.
)  # Executes this documented step of the workflow.

# ------------------------------------------------------------------------------
# 27.17 ESSENTIAL PLOTS
# ------------------------------------------------------------------------------

global_hit_plot_data <- surrogate_global_summary %>%  # Executes this documented step of the workflow.
  dplyr::filter(metric == "pct_significant")  # Executes this documented step of the workflow.

if (nrow(global_hit_plot_data) > 0L) {  # Executes this documented step of the workflow.

  p_global_interval <- ggplot2::ggplot(  # Executes this documented step of the workflow.
    global_hit_plot_data,  # Executes this documented step of the workflow.
    ggplot2::aes(x = null_mean, y = surrogate_method)  # Executes this documented step of the workflow.
  ) +  # Executes this documented step of the workflow.
    ggplot2::geom_errorbarh(  # Executes this documented step of the workflow.
      ggplot2::aes(xmin = null_q025, xmax = null_q975),  # Executes this documented step of the workflow.
      height = 0.15  # Executes this documented step of the workflow.
    ) +  # Executes this documented step of the workflow.
    ggplot2::geom_point(size = 2) +  # Executes this documented step of the workflow.
    ggplot2::geom_point(  # Executes this documented step of the workflow.
      ggplot2::aes(x = observed_value),  # Executes this documented step of the workflow.
      shape = 21,  # Executes this documented step of the workflow.
      fill = "black",  # Executes this documented step of the workflow.
      size = 2.5  # Executes this documented step of the workflow.
    ) +  # Executes this documented step of the workflow.
    ggplot2::facet_wrap(~ region, scales = "free_x") +  # Executes this documented step of the workflow.
    ggplot2::labs(  # Executes this documented step of the workflow.
      title = "Observed calendar-aware Granger hit rate versus surrogate nulls",  # Executes this documented step of the workflow.
      subtitle = "Intervals are surrogate 2.5-97.5%; black points are observed hit rates",  # Executes this documented step of the workflow.
      x = "% significant calendar-aware Granger tests",  # Executes this documented step of the workflow.
      y = "Surrogate method"  # Executes this documented step of the workflow.
    ) +  # Executes this documented step of the workflow.
    ggplot2::theme_minimal(base_size = 10)  # Executes this documented step of the workflow.

  ggplot2::ggsave(  # Executes this documented step of the workflow.
    file.path(out_surrogate_plots, paste0("observed_vs_surrogate_intervals_", surrogate_run_label, ".png")),  # Executes this documented step of the workflow.
    p_global_interval,  # Executes this documented step of the workflow.
    width = 12,  # Executes this documented step of the workflow.
    height = 8,  # Executes this documented step of the workflow.
    dpi = 300,  # Executes this documented step of the workflow.
    bg = "white"  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.

  p_global_p <- ggplot2::ggplot(  # Executes this documented step of the workflow.
    global_hit_plot_data,  # Executes this documented step of the workflow.
    ggplot2::aes(x = region, y = surrogate_method, fill = empirical_p)  # Executes this documented step of the workflow.
  ) +  # Executes this documented step of the workflow.
    ggplot2::geom_tile() +  # Executes this documented step of the workflow.
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.3f", empirical_p)), size = 3) +  # Executes this documented step of the workflow.
    ggplot2::scale_fill_gradient(low = "black", high = "white", limits = c(0, 1)) +  # Executes this documented step of the workflow.
    ggplot2::labs(  # Executes this documented step of the workflow.
      title = "Empirical surrogate p-values for calendar-aware Granger hit rate",  # Executes this documented step of the workflow.
      x = "Region",  # Executes this documented step of the workflow.
      y = "Surrogate method",  # Executes this documented step of the workflow.
      fill = "Empirical p"  # Executes this documented step of the workflow.
    ) +  # Executes this documented step of the workflow.
    ggplot2::theme_minimal(base_size = 10)  # Executes this documented step of the workflow.

  ggplot2::ggsave(  # Executes this documented step of the workflow.
    file.path(out_surrogate_plots, paste0("global_empirical_p_heatmap_", surrogate_run_label, ".png")),  # Executes this documented step of the workflow.
    p_global_p,  # Executes this documented step of the workflow.
    width = 8,  # Executes this documented step of the workflow.
    height = 6,  # Executes this documented step of the workflow.
    dpi = 300,  # Executes this documented step of the workflow.
    bg = "white"  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.
}  # Executes this documented step of the workflow.

# ------------------------------------------------------------------------------
# 27.18 RUNTIME, SAVE OBJECTS, MANIFEST
# ------------------------------------------------------------------------------

surrogate_runtime <- tibble::tibble(  # Executes this documented step of the workflow.
  run_mode = surrogate_run_mode,  # Executes this documented step of the workflow.
  run_label = surrogate_run_label,  # Executes this documented step of the workflow.
  repetitions_per_method_region = surrogate_n_repeats,  # Executes this documented step of the workflow.
  n_methods = length(surrogate_methods_to_run),  # Executes this documented step of the workflow.
  methods = paste(surrogate_methods_to_run, collapse = ";"),  # Executes this documented step of the workflow.
  seed = surrogate_seed,  # Executes this documented step of the workflow.
  started_at = as.character(surrogate_started_at),  # Executes this documented step of the workflow.
  finished_at = as.character(surrogate_finished_at),  # Executes this documented step of the workflow.
  elapsed_seconds = as.numeric(  # Executes this documented step of the workflow.
    difftime(surrogate_finished_at, surrogate_started_at, units = "secs")  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.
)  # Executes this documented step of the workflow.

write.csv(  # Executes this documented step of the workflow.
  surrogate_runtime,  # Executes this documented step of the workflow.
  file.path(out_surrogate_diagnostics, paste0("surrogate_runtime_", surrogate_run_label, ".csv")),  # Executes this documented step of the workflow.
  row.names = FALSE  # Executes this documented step of the workflow.
)  # Executes this documented step of the workflow.

saveRDS(  # Executes this documented step of the workflow.
  list(  # Executes this documented step of the workflow.
    methods = surrogate_methods,  # Executes this documented step of the workflow.
    designs = surrogate_designs,  # Executes this documented step of the workflow.
    observed_calendar_aware_tests = observed_calendar_aware_tests,  # Executes this documented step of the workflow.
    observed_with_standard_adjustments = observed_with_standard_adjustments,  # Executes this documented step of the workflow.
    observed_global_statistics = observed_global_statistics,  # Executes this documented step of the workflow.
    global_draws = surrogate_global_draws,  # Executes this documented step of the workflow.
    global_summary = surrogate_global_summary,  # Executes this documented step of the workflow.
    lag_summary = surrogate_lag_summary,  # Executes this documented step of the workflow.
    varclass_summary = surrogate_varclass_summary,  # Executes this documented step of the workflow.
    family_summary = surrogate_family_summary,  # Executes this documented step of the workflow.
    environment_summary = surrogate_environment_summary,  # Executes this documented step of the workflow.
    testwise_adjustments = surrogate_testwise_adjustments,  # Executes this documented step of the workflow.
    significant_counts = surrogate_significant_counts,  # Executes this documented step of the workflow.
    quality_diagnostics = surrogate_quality_diagnostics,  # Executes this documented step of the workflow.
    quality_summary = surrogate_quality_summary,  # Executes this documented step of the workflow.
    runtime = surrogate_runtime  # Executes this documented step of the workflow.
  ),  # Executes this documented step of the workflow.
  file.path(  # Executes this documented step of the workflow.
    out_surrogate_objects,  # Executes this documented step of the workflow.
    paste0("calendar_aware_surrogate_analysis_", surrogate_run_label, ".rds")  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.
)  # Executes this documented step of the workflow.

surrogate_output_manifest <- c(  # Executes this documented step of the workflow.
  "CALENDAR-AWARE SURROGATE ANALYSIS OUTPUTS",  # Executes this documented step of the workflow.
  "========================================",  # Executes this documented step of the workflow.
  "",  # Executes this documented step of the workflow.
  paste0("Run label: ", surrogate_run_label),  # Executes this documented step of the workflow.
  paste0("Methods: ", paste(surrogate_methods_to_run, collapse = ", ")),  # Executes this documented step of the workflow.
  "",  # Executes this documented step of the workflow.
  "CORE OBSERVED RESULTS",  # Executes this documented step of the workflow.
  paste0("tables/observed_calendar_aware_granger_tests_", surrogate_run_label, ".csv"),  # Executes this documented step of the workflow.
  paste0("tables/observed_calendar_aware_with_standard_adjustments_", surrogate_run_label, ".csv"),  # Executes this documented step of the workflow.
  "",  # Executes this documented step of the workflow.
  "GLOBAL SURROGATE RESULTS",  # Executes this documented step of the workflow.
  paste0("tables/surrogate_global_null_draws_", surrogate_run_label, ".csv"),  # Executes this documented step of the workflow.
  paste0("tables/surrogate_global_null_summary_", surrogate_run_label, ".csv"),  # Executes this documented step of the workflow.
  paste0("tables/surrogate_method_agreement_", surrogate_run_label, ".csv"),  # Executes this documented step of the workflow.
  "",  # Executes this documented step of the workflow.
  "SUBGROUP RESULTS",  # Executes this documented step of the workflow.
  paste0("tables/surrogate_summary_by_lag_", surrogate_run_label, ".csv"),  # Executes this documented step of the workflow.
  paste0("tables/surrogate_summary_by_varclass_", surrogate_run_label, ".csv"),  # Executes this documented step of the workflow.
  paste0("tables/surrogate_summary_by_family_", surrogate_run_label, ".csv"),  # Executes this documented step of the workflow.
  paste0("tables/surrogate_summary_by_environmental_variable_", surrogate_run_label, ".csv"),  # Executes this documented step of the workflow.
  "",  # Executes this documented step of the workflow.
  "TESTWISE CORRECTIONS",  # Executes this documented step of the workflow.
  paste0("tables/surrogate_testwise_adjustments_", surrogate_run_label, ".csv"),  # Executes this documented step of the workflow.
  paste0("tables/surrogate_adjusted_significant_test_counts_", surrogate_run_label, ".csv"),  # Executes this documented step of the workflow.
  "",  # Executes this documented step of the workflow.
  "DIAGNOSTICS",  # Executes this documented step of the workflow.
  paste0("diagnostics/environment_grid_diagnostics_", surrogate_run_label, ".csv"),  # Executes this documented step of the workflow.
  paste0("diagnostics/family_matching_diagnostics_", surrogate_run_label, ".csv"),  # Executes this documented step of the workflow.
  paste0("diagnostics/surrogate_quality_diagnostics_", surrogate_run_label, ".csv"),  # Executes this documented step of the workflow.
  paste0("diagnostics/surrogate_quality_summary_", surrogate_run_label, ".csv"),  # Executes this documented step of the workflow.
  paste0("diagnostics/surrogate_runtime_", surrogate_run_label, ".csv"),  # Executes this documented step of the workflow.
  "",  # Executes this documented step of the workflow.
  "R OBJECT",  # Executes this documented step of the workflow.
  paste0("objects/calendar_aware_surrogate_analysis_", surrogate_run_label, ".rds")  # Executes this documented step of the workflow.
)  # Executes this documented step of the workflow.

writeLines(  # Executes this documented step of the workflow.
  surrogate_output_manifest,  # Executes this documented step of the workflow.
  file.path(out_surrogates, paste0("README_calendar_aware_outputs_", surrogate_run_label, ".txt"))  # Executes this documented step of the workflow.
)  # Executes this documented step of the workflow.

cat(  # Executes this documented step of the workflow.
  "\nCALENDAR-AWARE SURROGATE ANALYSIS DONE.\n",  # Executes this documented step of the workflow.
  "Run mode: ", surrogate_run_mode, "\n",  # Executes this documented step of the workflow.
  "Repetitions per method and region: ", surrogate_n_repeats, "\n",  # Executes this documented step of the workflow.
  "Methods run: ", length(surrogate_methods_to_run), "\n",  # Executes this documented step of the workflow.
  "Outputs: ", normalizePath(out_surrogates, mustWork = FALSE), "\n",  # Executes this documented step of the workflow.
  sep = ""  # Executes this documented step of the workflow.
)  # Executes this documented step of the workflow.

# ==============================================================================
# END OF SECTION 27
# ==============================================================================
# ==============================================================================
# 28. CALENDAR-AWARE CONDITIONAL GRANGER ANALYSES
# ==============================================================================
# PURPOSE
# - Test whether soil temperature adds predictive information about insect
#   abundance after accounting for air temperature.
# - Run the reverse test (air temperature given soil temperature).
# - Test whether drought/soil-moisture indices add predictive information after
#   accounting for air temperature.
# - Respect true calendar lags: no winter gap is ever treated as one month.
# - Use only the final stationary, season-checked analysis columns selected in
#   Sections 1-26.
# - Treat every non-significant or non-testable conditional result as AMBIGUOUS.
#
# REQUIRED OBJECTS/FUNCTIONS FROM EARLIER SECTIONS
#   ds22007_monthly_ALB_season_checked
#   ds22007_monthly_HAI_season_checked
#   ds22007_monthly_SCH_season_checked
#   stationarity_insects_season_checked
#   stationarity_environment_season_checked
#   get_region_list_calendar()          [Section 27]
#   lags
#   out_root
#
# DESIGN CHOICES
# - Air temperature: mean 2-m air temperature, Ta_200.
# - Soil temperature: mean temperatures at 5, 10, 20 and 50 cm.
#   Min/max variants are deliberately excluded to avoid redundant variants.
# - Drought/soil-moisture indices: both SMI_upsoil and SMI_total are evaluated.
# - Conditional Granger test at lag p compares:
#       restricted: insect_t ~ insect lags + conditioner lags
#       full:       insect_t ~ insect lags + conditioner lags + candidate lags
#   The candidate is conditionally Granger-predictive when the full model adds
#   significant predictive information over the restricted model.
# ============================================================================== 

# ------------------------------------------------------------------------------
# 28.1 CONFIGURATION
# ------------------------------------------------------------------------------

conditional_alpha <- 0.05  # Executes this documented step of the workflow.
conditional_lags <- lags  # Executes this documented step of the workflow.
conditional_regions <- c("ALB", "HAI", "SCH")  # Executes this documented step of the workflow.

conditional_temperature_families <- c(  # Executes this documented step of the workflow.
  "Elateridae",  # Executes this documented step of the workflow.
  "Cerambycidae",  # Executes this documented step of the workflow.
  "Scarabaeidae",  # Executes this documented step of the workflow.
  "Staphylinidae",  # Executes this documented step of the workflow.
  "Scydmaenidae"  # Executes this documented step of the workflow.
)  # Executes this documented step of the workflow.

conditional_drought_families <- c(  # Executes this documented step of the workflow.
  "Pentatomidae",  # Executes this documented step of the workflow.
  "Scarabaeidae"  # Executes this documented step of the workflow.
)  # Executes this documented step of the workflow.

conditional_air_dataset <- "weather"  # Executes this documented step of the workflow.
conditional_air_column <- "Ta_200"  # Executes this documented step of the workflow.

conditional_soil_dataset <- "weather"  # Executes this documented step of the workflow.
conditional_soil_columns <- c("Ts_05", "Ts_10", "Ts_20", "Ts_50")  # Executes this documented step of the workflow.

conditional_drought_datasets <- c("SMI_upsoil", "SMI_total")  # Executes this documented step of the workflow.

conditional_soil_labels <- c(  # Executes this documented step of the workflow.
  Ts_05 = "Soil temperature 5 cm",  # Executes this documented step of the workflow.
  Ts_10 = "Soil temperature 10 cm",  # Executes this documented step of the workflow.
  Ts_20 = "Soil temperature 20 cm",  # Executes this documented step of the workflow.
  Ts_50 = "Soil temperature 50 cm"  # Executes this documented step of the workflow.
)  # Executes this documented step of the workflow.

out_conditional <- file.path(out_root, "conditional_granger_calendar_aware_20261005")  # Executes this documented step of the workflow.
out_conditional_tables <- file.path(out_conditional, "tables")  # Executes this documented step of the workflow.
out_conditional_plots <- file.path(out_conditional, "plots")  # Executes this documented step of the workflow.
out_conditional_diagnostics <- file.path(out_conditional, "diagnostics")  # Executes this documented step of the workflow.
out_conditional_objects <- file.path(out_conditional, "objects")  # Executes this documented step of the workflow.

dir.create(out_conditional, recursive = TRUE, showWarnings = FALSE)  # Executes this documented step of the workflow.
dir.create(out_conditional_tables, recursive = TRUE, showWarnings = FALSE)  # Executes this documented step of the workflow.
dir.create(out_conditional_plots, recursive = TRUE, showWarnings = FALSE)  # Executes this documented step of the workflow.
dir.create(out_conditional_diagnostics, recursive = TRUE, showWarnings = FALSE)  # Executes this documented step of the workflow.
dir.create(out_conditional_objects, recursive = TRUE, showWarnings = FALSE)  # Executes this documented step of the workflow.

# ------------------------------------------------------------------------------
# 28.2 LOOK UP THE FINAL ANALYSIS COLUMNS SELECTED EARLIER
# ------------------------------------------------------------------------------

lookup_insect_key_conditional <- function(region_name, family_name) {  # Executes this documented step of the workflow.

  rows <- stationarity_insects_season_checked %>%  # Executes this documented step of the workflow.
    dplyr::filter(  # Executes this documented step of the workflow.
      region == region_name,  # Executes this documented step of the workflow.
      Family == family_name  # Executes this documented step of the workflow.
    ) %>%  # Executes this documented step of the workflow.
    dplyr::distinct(  # Executes this documented step of the workflow.
      Family,  # Executes this documented step of the workflow.
      analysis_column,  # Executes this documented step of the workflow.
      was_deseasonalised,  # Executes this documented step of the workflow.
      pass_stationarity,  # Executes this documented step of the workflow.
      joint_status  # Executes this documented step of the workflow.
    )  # Executes this documented step of the workflow.

  if (nrow(rows) == 0L) {  # Executes this documented step of the workflow.
    return(list(status = "insect_not_found"))  # Executes this documented step of the workflow.
  }  # Executes this documented step of the workflow.

  rows_ok <- rows %>% dplyr::filter(pass_stationarity)  # Executes this documented step of the workflow.

  if (nrow(rows_ok) == 0L) {  # Executes this documented step of the workflow.
    return(list(status = "insect_not_stationary"))  # Executes this documented step of the workflow.
  }  # Executes this documented step of the workflow.

  rows_ok <- rows_ok %>%  # Executes this documented step of the workflow.
    dplyr::distinct(analysis_column, was_deseasonalised)  # Executes this documented step of the workflow.

  if (nrow(rows_ok) != 1L) {  # Executes this documented step of the workflow.
    return(list(status = "insect_analysis_column_ambiguous"))  # Executes this documented step of the workflow.
  }  # Executes this documented step of the workflow.

  list(  # Executes this documented step of the workflow.
    status = "ok",  # Executes this documented step of the workflow.
    analysis_column = rows_ok$analysis_column[1],  # Executes this documented step of the workflow.
    was_deseasonalised = rows_ok$was_deseasonalised[1]  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.
}  # Executes this documented step of the workflow.

lookup_environment_key_conditional <- function(region_name, dataset_name, variable_name) {  # Executes this documented step of the workflow.

  rows <- stationarity_environment_season_checked %>%  # Executes this documented step of the workflow.
    dplyr::filter(  # Executes this documented step of the workflow.
      region == region_name,  # Executes this documented step of the workflow.
      EnvDataset == dataset_name,  # Executes this documented step of the workflow.
      EnvColumn == variable_name  # Executes this documented step of the workflow.
    ) %>%  # Executes this documented step of the workflow.
    dplyr::distinct(  # Executes this documented step of the workflow.
      EnvDataset,  # Executes this documented step of the workflow.
      EnvColumn,  # Executes this documented step of the workflow.
      analysis_column,  # Executes this documented step of the workflow.
      was_deseasonalised,  # Executes this documented step of the workflow.
      pass_stationarity,  # Executes this documented step of the workflow.
      joint_status  # Executes this documented step of the workflow.
    )  # Executes this documented step of the workflow.

  if (nrow(rows) == 0L) {  # Executes this documented step of the workflow.
    return(list(status = "environment_not_found"))  # Executes this documented step of the workflow.
  }  # Executes this documented step of the workflow.

  rows_ok <- rows %>% dplyr::filter(pass_stationarity)  # Executes this documented step of the workflow.

  if (nrow(rows_ok) == 0L) {  # Executes this documented step of the workflow.
    return(list(status = "environment_not_stationary"))  # Executes this documented step of the workflow.
  }  # Executes this documented step of the workflow.

  rows_ok <- rows_ok %>%  # Executes this documented step of the workflow.
    dplyr::distinct(analysis_column, was_deseasonalised)  # Executes this documented step of the workflow.

  if (nrow(rows_ok) != 1L) {  # Executes this documented step of the workflow.
    return(list(status = "environment_analysis_column_ambiguous"))  # Executes this documented step of the workflow.
  }  # Executes this documented step of the workflow.

  list(  # Executes this documented step of the workflow.
    status = "ok",  # Executes this documented step of the workflow.
    analysis_column = rows_ok$analysis_column[1],  # Executes this documented step of the workflow.
    was_deseasonalised = rows_ok$was_deseasonalised[1]  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.
}  # Executes this documented step of the workflow.

extract_insect_series_conditional <- function(region_name, family_name) {  # Executes this documented step of the workflow.

  key <- lookup_insect_key_conditional(region_name, family_name)  # Executes this documented step of the workflow.

  if (!identical(key$status, "ok")) {  # Executes this documented step of the workflow.
    return(list(status = key$status))  # Executes this documented step of the workflow.
  }  # Executes this documented step of the workflow.

  region_list <- get_region_list_calendar(region_name)  # Executes this documented step of the workflow.

  if (!key$analysis_column %in% names(region_list$ds22007)) {  # Executes this documented step of the workflow.
    return(list(status = "insect_analysis_column_missing"))  # Executes this documented step of the workflow.
  }  # Executes this documented step of the workflow.

  dat <- region_list$ds22007 %>%  # Executes this documented step of the workflow.
    dplyr::filter(Family == family_name) %>%  # Executes this documented step of the workflow.
    dplyr::transmute(  # Executes this documented step of the workflow.
      year = as.integer(year),  # Executes this documented step of the workflow.
      month = as.integer(month),  # Executes this documented step of the workflow.
      insect = suppressWarnings(as.numeric(.data[[key$analysis_column]]))  # Executes this documented step of the workflow.
    ) %>%  # Executes this documented step of the workflow.
    dplyr::filter(is.finite(year), is.finite(month), is.finite(insect)) %>%  # Executes this documented step of the workflow.
    dplyr::arrange(year, month)  # Executes this documented step of the workflow.

  if (anyDuplicated(paste(dat$year, dat$month, sep = "-")) > 0L) {  # Executes this documented step of the workflow.
    return(list(status = "duplicated_insect_year_month"))  # Executes this documented step of the workflow.
  }  # Executes this documented step of the workflow.

  list(  # Executes this documented step of the workflow.
    status = "ok",  # Executes this documented step of the workflow.
    data = dat,  # Executes this documented step of the workflow.
    analysis_column = key$analysis_column,  # Executes this documented step of the workflow.
    was_deseasonalised = key$was_deseasonalised  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.
}  # Executes this documented step of the workflow.

extract_environment_series_conditional <- function(  # Executes this documented step of the workflow.
    region_name,  # Executes this documented step of the workflow.
    dataset_name,  # Executes this documented step of the workflow.
    variable_name) {  # Executes this documented step of the workflow.

  key <- lookup_environment_key_conditional(  # Executes this documented step of the workflow.
    region_name,  # Executes this documented step of the workflow.
    dataset_name,  # Executes this documented step of the workflow.
    variable_name  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.

  if (!identical(key$status, "ok")) {  # Executes this documented step of the workflow.
    return(list(status = key$status))  # Executes this documented step of the workflow.
  }  # Executes this documented step of the workflow.

  region_list <- get_region_list_calendar(region_name)  # Executes this documented step of the workflow.

  if (!dataset_name %in% names(region_list)) {  # Executes this documented step of the workflow.
    return(list(status = "environment_dataset_missing"))  # Executes this documented step of the workflow.
  }  # Executes this documented step of the workflow.

  env_df <- region_list[[dataset_name]]  # Executes this documented step of the workflow.

  if (!key$analysis_column %in% names(env_df)) {  # Executes this documented step of the workflow.
    return(list(status = "environment_analysis_column_missing"))  # Executes this documented step of the workflow.
  }  # Executes this documented step of the workflow.

  dat <- env_df %>%  # Executes this documented step of the workflow.
    dplyr::transmute(  # Executes this documented step of the workflow.
      year = as.integer(year),  # Executes this documented step of the workflow.
      month = as.integer(month),  # Executes this documented step of the workflow.
      value = suppressWarnings(as.numeric(.data[[key$analysis_column]]))  # Executes this documented step of the workflow.
    ) %>%  # Executes this documented step of the workflow.
    dplyr::filter(is.finite(year), is.finite(month), is.finite(value)) %>%  # Executes this documented step of the workflow.
    dplyr::arrange(year, month)  # Executes this documented step of the workflow.

  if (anyDuplicated(paste(dat$year, dat$month, sep = "-")) > 0L) {  # Executes this documented step of the workflow.
    return(list(status = "duplicated_environment_year_month"))  # Executes this documented step of the workflow.
  }  # Executes this documented step of the workflow.

  list(  # Executes this documented step of the workflow.
    status = "ok",  # Executes this documented step of the workflow.
    data = dat,  # Executes this documented step of the workflow.
    analysis_column = key$analysis_column,  # Executes this documented step of the workflow.
    was_deseasonalised = key$was_deseasonalised  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.
}  # Executes this documented step of the workflow.

# ------------------------------------------------------------------------------
# 28.3 CALENDAR-AWARE CONDITIONAL GRANGER FUNCTION
# ------------------------------------------------------------------------------
# candidate = variable whose additional predictive information is being tested
# conditioner = variable already included in both restricted and full models

calendar_conditional_granger_safe <- function(  # Executes this documented step of the workflow.
    response,  # Executes this documented step of the workflow.
    candidate,  # Executes this documented step of the workflow.
    conditioner,  # Executes this documented step of the workflow.
    year,  # Executes this documented step of the workflow.
    month,  # Executes this documented step of the workflow.
    lags = 1:3) {  # Executes this documented step of the workflow.

  dat <- tibble::tibble(  # Executes this documented step of the workflow.
    year = as.integer(year),  # Executes this documented step of the workflow.
    month = as.integer(month),  # Executes this documented step of the workflow.
    response = suppressWarnings(as.numeric(response)),  # Executes this documented step of the workflow.
    candidate = suppressWarnings(as.numeric(candidate)),  # Executes this documented step of the workflow.
    conditioner = suppressWarnings(as.numeric(conditioner))  # Executes this documented step of the workflow.
  ) %>%  # Executes this documented step of the workflow.
    dplyr::filter(  # Executes this documented step of the workflow.
      is.finite(year),  # Executes this documented step of the workflow.
      is.finite(month),  # Executes this documented step of the workflow.
      is.finite(response),  # Executes this documented step of the workflow.
      is.finite(candidate),  # Executes this documented step of the workflow.
      is.finite(conditioner)  # Executes this documented step of the workflow.
    ) %>%  # Executes this documented step of the workflow.
    dplyr::mutate(time_id = year * 12L + month) %>%  # Executes this documented step of the workflow.
    dplyr::arrange(time_id)  # Executes this documented step of the workflow.

  if (nrow(dat) == 0L || anyDuplicated(dat$time_id) > 0L) {  # Executes this documented step of the workflow.
    return(  # Executes this documented step of the workflow.
      purrr::map_dfr(lags, function(p) {  # Executes this documented step of the workflow.
        tibble::tibble(  # Executes this documented step of the workflow.
          Lag = p,  # Executes this documented step of the workflow.
          n_obs = 0L,  # Executes this documented step of the workflow.
          n_years = 0L,  # Executes this documented step of the workflow.
          F_stat = NA_real_,  # Executes this documented step of the workflow.
          p_value = NA_real_,  # Executes this documented step of the workflow.
          restricted_R2 = NA_real_,  # Executes this documented step of the workflow.
          full_R2 = NA_real_,  # Executes this documented step of the workflow.
          delta_R2 = NA_real_,  # Executes this documented step of the workflow.
          partial_R2 = NA_real_,  # Executes this documented step of the workflow.
          delta_AIC = NA_real_,  # Executes this documented step of the workflow.
          candidate_conditioner_cor = NA_real_,  # Executes this documented step of the workflow.
          test_status = if (nrow(dat) == 0L) "no_complete_data" else "duplicated_year_month",  # Executes this documented step of the workflow.
          interpretation = "AMBIGUOUS"  # Executes this documented step of the workflow.
        )  # Executes this documented step of the workflow.
      })  # Executes this documented step of the workflow.
    )  # Executes this documented step of the workflow.
  }  # Executes this documented step of the workflow.

  contemporaneous_cor <- suppressWarnings(  # Executes this documented step of the workflow.
    stats::cor(dat$candidate, dat$conditioner, use = "complete.obs")  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.

  purrr::map_dfr(lags, function(p) {  # Executes this documented step of the workflow.

    model_df <- dat  # Executes this documented step of the workflow.

    for (k in seq_len(p)) {  # Executes this documented step of the workflow.
      previous_row <- match(model_df$time_id - k, dat$time_id)  # Executes this documented step of the workflow.
      model_df[[paste0("response_lag", k)]] <- dat$response[previous_row]  # Executes this documented step of the workflow.
      model_df[[paste0("candidate_lag", k)]] <- dat$candidate[previous_row]  # Executes this documented step of the workflow.
      model_df[[paste0("conditioner_lag", k)]] <- dat$conditioner[previous_row]  # Executes this documented step of the workflow.
    }  # Executes this documented step of the workflow.

    response_lags <- paste0("response_lag", seq_len(p))  # Executes this documented step of the workflow.
    candidate_lags <- paste0("candidate_lag", seq_len(p))  # Executes this documented step of the workflow.
    conditioner_lags <- paste0("conditioner_lag", seq_len(p))  # Executes this documented step of the workflow.

    required <- c(  # Executes this documented step of the workflow.
      "response",  # Executes this documented step of the workflow.
      response_lags,  # Executes this documented step of the workflow.
      candidate_lags,  # Executes this documented step of the workflow.
      conditioner_lags  # Executes this documented step of the workflow.
    )  # Executes this documented step of the workflow.

    model_df <- model_df[  # Executes this documented step of the workflow.
      stats::complete.cases(model_df[, required, drop = FALSE]),  # Executes this documented step of the workflow.
      ,  # Executes this documented step of the workflow.
      drop = FALSE  # Executes this documented step of the workflow.
    ]  # Executes this documented step of the workflow.

    # Full model contains 3p predictors plus an intercept. Require at least
    # two residual degrees of freedom.
    minimum_n <- 3L * p + 3L  # Executes this documented step of the workflow.

    if (  # Executes this documented step of the workflow.
      nrow(model_df) < minimum_n ||  # Executes this documented step of the workflow.
      !is.finite(stats::var(model_df$response)) ||  # Executes this documented step of the workflow.
      stats::var(model_df$response) == 0 ||  # Executes this documented step of the workflow.
      any(vapply(  # Executes this documented step of the workflow.
        model_df[, c(response_lags, candidate_lags, conditioner_lags), drop = FALSE],  # Executes this documented step of the workflow.
        function(z) !is.finite(stats::var(z)) || stats::var(z) == 0,  # Executes this documented step of the workflow.
        logical(1)  # Executes this documented step of the workflow.
      ))  # Executes this documented step of the workflow.
    ) {  # Executes this documented step of the workflow.
      return(  # Executes this documented step of the workflow.
        tibble::tibble(  # Executes this documented step of the workflow.
          Lag = p,  # Executes this documented step of the workflow.
          n_obs = nrow(model_df),  # Executes this documented step of the workflow.
          n_years = dplyr::n_distinct(model_df$year),  # Executes this documented step of the workflow.
          F_stat = NA_real_,  # Executes this documented step of the workflow.
          p_value = NA_real_,  # Executes this documented step of the workflow.
          restricted_R2 = NA_real_,  # Executes this documented step of the workflow.
          full_R2 = NA_real_,  # Executes this documented step of the workflow.
          delta_R2 = NA_real_,  # Executes this documented step of the workflow.
          partial_R2 = NA_real_,  # Executes this documented step of the workflow.
          delta_AIC = NA_real_,  # Executes this documented step of the workflow.
          candidate_conditioner_cor = contemporaneous_cor,  # Executes this documented step of the workflow.
          test_status = "not_testable",  # Executes this documented step of the workflow.
          interpretation = "AMBIGUOUS"  # Executes this documented step of the workflow.
        )  # Executes this documented step of the workflow.
      )  # Executes this documented step of the workflow.
    }  # Executes this documented step of the workflow.

    restricted_formula <- stats::reformulate(  # Executes this documented step of the workflow.
      c(response_lags, conditioner_lags),  # Executes this documented step of the workflow.
      response = "response"  # Executes this documented step of the workflow.
    )  # Executes this documented step of the workflow.

    full_formula <- stats::reformulate(  # Executes this documented step of the workflow.
      c(response_lags, conditioner_lags, candidate_lags),  # Executes this documented step of the workflow.
      response = "response"  # Executes this documented step of the workflow.
    )  # Executes this documented step of the workflow.

    tryCatch(  # Executes this documented step of the workflow.
      {  # Executes this documented step of the workflow.
        restricted_fit <- stats::lm(restricted_formula, data = model_df)  # Executes this documented step of the workflow.
        full_fit <- stats::lm(full_formula, data = model_df)  # Executes this documented step of the workflow.

        # Near-identical temperature predictors can make the full model rank
        # deficient. In that case the scientific interpretation is ambiguous,
        # not evidence for absence of an effect.
        if (  # Executes this documented step of the workflow.
          restricted_fit$rank < length(stats::coef(restricted_fit)) ||  # Executes this documented step of the workflow.
          full_fit$rank < length(stats::coef(full_fit))  # Executes this documented step of the workflow.
        ) {  # Executes this documented step of the workflow.
          stop("singular_or_collinear_model")  # Executes this documented step of the workflow.
        }  # Executes this documented step of the workflow.

        comparison <- stats::anova(restricted_fit, full_fit)  # Executes this documented step of the workflow.

        rss_restricted <- sum(stats::residuals(restricted_fit)^2)  # Executes this documented step of the workflow.
        rss_full <- sum(stats::residuals(full_fit)^2)  # Executes this documented step of the workflow.

        r2_restricted <- summary(restricted_fit)$r.squared  # Executes this documented step of the workflow.
        r2_full <- summary(full_fit)$r.squared  # Executes this documented step of the workflow.

        p_value <- as.numeric(comparison$`Pr(>F)`[2])  # Executes this documented step of the workflow.

        tibble::tibble(  # Executes this documented step of the workflow.
          Lag = p,  # Executes this documented step of the workflow.
          n_obs = nrow(model_df),  # Executes this documented step of the workflow.
          n_years = dplyr::n_distinct(model_df$year),  # Executes this documented step of the workflow.
          F_stat = as.numeric(comparison$F[2]),  # Executes this documented step of the workflow.
          p_value = p_value,  # Executes this documented step of the workflow.
          restricted_R2 = r2_restricted,  # Executes this documented step of the workflow.
          full_R2 = r2_full,  # Executes this documented step of the workflow.
          delta_R2 = r2_full - r2_restricted,  # Executes this documented step of the workflow.
          partial_R2 = if (is.finite(rss_restricted) && rss_restricted > 0) {  # Executes this documented step of the workflow.
            (rss_restricted - rss_full) / rss_restricted  # Executes this documented step of the workflow.
          } else {  # Executes this documented step of the workflow.
            NA_real_  # Executes this documented step of the workflow.
          },  # Executes this documented step of the workflow.
          delta_AIC = stats::AIC(restricted_fit) - stats::AIC(full_fit),  # Executes this documented step of the workflow.
          candidate_conditioner_cor = contemporaneous_cor,  # Executes this documented step of the workflow.
          test_status = "tested",  # Executes this documented step of the workflow.
          interpretation = dplyr::if_else(  # Executes this documented step of the workflow.
            is.finite(p_value) && p_value < conditional_alpha,  # Executes this documented step of the workflow.
            "SUPPORTED",  # Executes this documented step of the workflow.
            "AMBIGUOUS"  # Executes this documented step of the workflow.
          )  # Executes this documented step of the workflow.
        )  # Executes this documented step of the workflow.
      },  # Executes this documented step of the workflow.
      error = function(e) {  # Executes this documented step of the workflow.
        tibble::tibble(  # Executes this documented step of the workflow.
          Lag = p,  # Executes this documented step of the workflow.
          n_obs = nrow(model_df),  # Executes this documented step of the workflow.
          n_years = dplyr::n_distinct(model_df$year),  # Executes this documented step of the workflow.
          F_stat = NA_real_,  # Executes this documented step of the workflow.
          p_value = NA_real_,  # Executes this documented step of the workflow.
          restricted_R2 = NA_real_,  # Executes this documented step of the workflow.
          full_R2 = NA_real_,  # Executes this documented step of the workflow.
          delta_R2 = NA_real_,  # Executes this documented step of the workflow.
          partial_R2 = NA_real_,  # Executes this documented step of the workflow.
          delta_AIC = NA_real_,  # Executes this documented step of the workflow.
          candidate_conditioner_cor = contemporaneous_cor,  # Executes this documented step of the workflow.
          test_status = paste0("conditional_granger_error: ", conditionMessage(e)),  # Executes this documented step of the workflow.
          interpretation = "AMBIGUOUS"  # Executes this documented step of the workflow.
        )  # Executes this documented step of the workflow.
      }  # Executes this documented step of the workflow.
    )  # Executes this documented step of the workflow.
  })  # Executes this documented step of the workflow.
}  # Executes this documented step of the workflow.

# ------------------------------------------------------------------------------
# 28.4 RUN ONE CONDITIONAL TEST CASE
# ------------------------------------------------------------------------------

conditional_no_test_rows <- function(  # Executes this documented step of the workflow.
    region_name,  # Executes this documented step of the workflow.
    family_name,  # Executes this documented step of the workflow.
    candidate_dataset,  # Executes this documented step of the workflow.
    candidate_column,  # Executes this documented step of the workflow.
    conditioner_dataset,  # Executes this documented step of the workflow.
    conditioner_column,  # Executes this documented step of the workflow.
    direction,  # Executes this documented step of the workflow.
    analysis_class,  # Executes this documented step of the workflow.
    reason) {  # Executes this documented step of the workflow.

  tibble::tibble(  # Executes this documented step of the workflow.
    region = region_name,  # Executes this documented step of the workflow.
    Family = family_name,  # Executes this documented step of the workflow.
    direction = direction,  # Executes this documented step of the workflow.
    analysis_class = analysis_class,  # Executes this documented step of the workflow.
    candidate_dataset = candidate_dataset,  # Executes this documented step of the workflow.
    candidate_column = candidate_column,  # Executes this documented step of the workflow.
    candidate_analysis_column = NA_character_,  # Executes this documented step of the workflow.
    candidate_was_deseasonalised = NA,  # Executes this documented step of the workflow.
    conditioner_dataset = conditioner_dataset,  # Executes this documented step of the workflow.
    conditioner_column = conditioner_column,  # Executes this documented step of the workflow.
    conditioner_analysis_column = NA_character_,  # Executes this documented step of the workflow.
    conditioner_was_deseasonalised = NA,  # Executes this documented step of the workflow.
    insect_analysis_column = NA_character_,  # Executes this documented step of the workflow.
    insect_was_deseasonalised = NA,  # Executes this documented step of the workflow.
    Lag = conditional_lags,  # Executes this documented step of the workflow.
    n_obs = NA_integer_,  # Executes this documented step of the workflow.
    n_years = NA_integer_,  # Executes this documented step of the workflow.
    F_stat = NA_real_,  # Executes this documented step of the workflow.
    p_value = NA_real_,  # Executes this documented step of the workflow.
    restricted_R2 = NA_real_,  # Executes this documented step of the workflow.
    full_R2 = NA_real_,  # Executes this documented step of the workflow.
    delta_R2 = NA_real_,  # Executes this documented step of the workflow.
    partial_R2 = NA_real_,  # Executes this documented step of the workflow.
    delta_AIC = NA_real_,  # Executes this documented step of the workflow.
    candidate_conditioner_cor = NA_real_,  # Executes this documented step of the workflow.
    test_status = reason,  # Executes this documented step of the workflow.
    interpretation = "AMBIGUOUS"  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.
}  # Executes this documented step of the workflow.

run_one_conditional_case <- function(  # Executes this documented step of the workflow.
    region_name,  # Executes this documented step of the workflow.
    family_name,  # Executes this documented step of the workflow.
    candidate_dataset,  # Executes this documented step of the workflow.
    candidate_column,  # Executes this documented step of the workflow.
    conditioner_dataset,  # Executes this documented step of the workflow.
    conditioner_column,  # Executes this documented step of the workflow.
    direction,  # Executes this documented step of the workflow.
    analysis_class) {  # Executes this documented step of the workflow.

  insect_obj <- extract_insect_series_conditional(region_name, family_name)  # Executes this documented step of the workflow.
  candidate_obj <- extract_environment_series_conditional(  # Executes this documented step of the workflow.
    region_name,  # Executes this documented step of the workflow.
    candidate_dataset,  # Executes this documented step of the workflow.
    candidate_column  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.
  conditioner_obj <- extract_environment_series_conditional(  # Executes this documented step of the workflow.
    region_name,  # Executes this documented step of the workflow.
    conditioner_dataset,  # Executes this documented step of the workflow.
    conditioner_column  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.

  if (!identical(insect_obj$status, "ok")) {  # Executes this documented step of the workflow.
    return(conditional_no_test_rows(  # Executes this documented step of the workflow.
      region_name, family_name,  # Executes this documented step of the workflow.
      candidate_dataset, candidate_column,  # Executes this documented step of the workflow.
      conditioner_dataset, conditioner_column,  # Executes this documented step of the workflow.
      direction, analysis_class,  # Executes this documented step of the workflow.
      insect_obj$status  # Executes this documented step of the workflow.
    ))  # Executes this documented step of the workflow.
  }  # Executes this documented step of the workflow.

  if (!identical(candidate_obj$status, "ok")) {  # Executes this documented step of the workflow.
    return(conditional_no_test_rows(  # Executes this documented step of the workflow.
      region_name, family_name,  # Executes this documented step of the workflow.
      candidate_dataset, candidate_column,  # Executes this documented step of the workflow.
      conditioner_dataset, conditioner_column,  # Executes this documented step of the workflow.
      direction, analysis_class,  # Executes this documented step of the workflow.
      paste0("candidate_", candidate_obj$status)  # Executes this documented step of the workflow.
    ))  # Executes this documented step of the workflow.
  }  # Executes this documented step of the workflow.

  if (!identical(conditioner_obj$status, "ok")) {  # Executes this documented step of the workflow.
    return(conditional_no_test_rows(  # Executes this documented step of the workflow.
      region_name, family_name,  # Executes this documented step of the workflow.
      candidate_dataset, candidate_column,  # Executes this documented step of the workflow.
      conditioner_dataset, conditioner_column,  # Executes this documented step of the workflow.
      direction, analysis_class,  # Executes this documented step of the workflow.
      paste0("conditioner_", conditioner_obj$status)  # Executes this documented step of the workflow.
    ))  # Executes this documented step of the workflow.
  }  # Executes this documented step of the workflow.

  pair <- insect_obj$data %>%  # Executes this documented step of the workflow.
    dplyr::inner_join(  # Executes this documented step of the workflow.
      candidate_obj$data %>% dplyr::rename(candidate = value),  # Executes this documented step of the workflow.
      by = c("year", "month")  # Executes this documented step of the workflow.
    ) %>%  # Executes this documented step of the workflow.
    dplyr::inner_join(  # Executes this documented step of the workflow.
      conditioner_obj$data %>% dplyr::rename(conditioner = value),  # Executes this documented step of the workflow.
      by = c("year", "month")  # Executes this documented step of the workflow.
    ) %>%  # Executes this documented step of the workflow.
    dplyr::filter(  # Executes this documented step of the workflow.
      is.finite(insect),  # Executes this documented step of the workflow.
      is.finite(candidate),  # Executes this documented step of the workflow.
      is.finite(conditioner)  # Executes this documented step of the workflow.
    ) %>%  # Executes this documented step of the workflow.
    dplyr::arrange(year, month)  # Executes this documented step of the workflow.

  if (nrow(pair) == 0L) {  # Executes this documented step of the workflow.
    return(conditional_no_test_rows(  # Executes this documented step of the workflow.
      region_name, family_name,  # Executes this documented step of the workflow.
      candidate_dataset, candidate_column,  # Executes this documented step of the workflow.
      conditioner_dataset, conditioner_column,  # Executes this documented step of the workflow.
      direction, analysis_class,  # Executes this documented step of the workflow.
      "no_matched_complete_months"  # Executes this documented step of the workflow.
    ))  # Executes this documented step of the workflow.
  }  # Executes this documented step of the workflow.

  calendar_conditional_granger_safe(  # Executes this documented step of the workflow.
    response = pair$insect,  # Executes this documented step of the workflow.
    candidate = pair$candidate,  # Executes this documented step of the workflow.
    conditioner = pair$conditioner,  # Executes this documented step of the workflow.
    year = pair$year,  # Executes this documented step of the workflow.
    month = pair$month,  # Executes this documented step of the workflow.
    lags = conditional_lags  # Executes this documented step of the workflow.
  ) %>%  # Executes this documented step of the workflow.
    dplyr::mutate(  # Executes this documented step of the workflow.
      region = region_name,  # Executes this documented step of the workflow.
      Family = family_name,  # Executes this documented step of the workflow.
      direction = direction,  # Executes this documented step of the workflow.
      analysis_class = analysis_class,  # Executes this documented step of the workflow.
      candidate_dataset = candidate_dataset,  # Executes this documented step of the workflow.
      candidate_column = candidate_column,  # Executes this documented step of the workflow.
      candidate_analysis_column = candidate_obj$analysis_column,  # Executes this documented step of the workflow.
      candidate_was_deseasonalised = candidate_obj$was_deseasonalised,  # Executes this documented step of the workflow.
      conditioner_dataset = conditioner_dataset,  # Executes this documented step of the workflow.
      conditioner_column = conditioner_column,  # Executes this documented step of the workflow.
      conditioner_analysis_column = conditioner_obj$analysis_column,  # Executes this documented step of the workflow.
      conditioner_was_deseasonalised = conditioner_obj$was_deseasonalised,  # Executes this documented step of the workflow.
      insect_analysis_column = insect_obj$analysis_column,  # Executes this documented step of the workflow.
      insect_was_deseasonalised = insect_obj$was_deseasonalised,  # Executes this documented step of the workflow.
      .before = 1  # Executes this documented step of the workflow.
    )  # Executes this documented step of the workflow.
}  # Executes this documented step of the workflow.

# ------------------------------------------------------------------------------
# 28.5 TEMPERATURE TESTS: SOIL GIVEN AIR + REVERSE AIR GIVEN SOIL
# ------------------------------------------------------------------------------

temperature_conditional_results <- purrr::map_dfr(  # Executes this documented step of the workflow.
  conditional_regions,  # Executes this documented step of the workflow.
  function(region_name) {  # Executes this documented step of the workflow.
    purrr::map_dfr(  # Executes this documented step of the workflow.
      conditional_temperature_families,  # Executes this documented step of the workflow.
      function(family_name) {  # Executes this documented step of the workflow.
        purrr::map_dfr(  # Executes this documented step of the workflow.
          conditional_soil_columns,  # Executes this documented step of the workflow.
          function(soil_column) {  # Executes this documented step of the workflow.
            dplyr::bind_rows(  # Executes this documented step of the workflow.
              run_one_conditional_case(  # Executes this documented step of the workflow.
                region_name = region_name,  # Executes this documented step of the workflow.
                family_name = family_name,  # Executes this documented step of the workflow.
                candidate_dataset = conditional_soil_dataset,  # Executes this documented step of the workflow.
                candidate_column = soil_column,  # Executes this documented step of the workflow.
                conditioner_dataset = conditional_air_dataset,  # Executes this documented step of the workflow.
                conditioner_column = conditional_air_column,  # Executes this documented step of the workflow.
                direction = "soil_given_air",  # Executes this documented step of the workflow.
                analysis_class = "soil temperature | air temperature"  # Executes this documented step of the workflow.
              ),  # Executes this documented step of the workflow.
              run_one_conditional_case(  # Executes this documented step of the workflow.
                region_name = region_name,  # Executes this documented step of the workflow.
                family_name = family_name,  # Executes this documented step of the workflow.
                candidate_dataset = conditional_air_dataset,  # Executes this documented step of the workflow.
                candidate_column = conditional_air_column,  # Executes this documented step of the workflow.
                conditioner_dataset = conditional_soil_dataset,  # Executes this documented step of the workflow.
                conditioner_column = soil_column,  # Executes this documented step of the workflow.
                direction = "air_given_soil",  # Executes this documented step of the workflow.
                analysis_class = "air temperature | soil temperature"  # Executes this documented step of the workflow.
              )  # Executes this documented step of the workflow.
            ) %>%  # Executes this documented step of the workflow.
              dplyr::mutate(  # Executes this documented step of the workflow.
                soil_column = soil_column,  # Executes this documented step of the workflow.
                soil_depth_label = unname(conditional_soil_labels[soil_column])  # Executes this documented step of the workflow.
              )  # Executes this documented step of the workflow.
          }  # Executes this documented step of the workflow.
        )  # Executes this documented step of the workflow.
      }  # Executes this documented step of the workflow.
    )  # Executes this documented step of the workflow.
  }  # Executes this documented step of the workflow.
)  # Executes this documented step of the workflow.

# ------------------------------------------------------------------------------
# 28.6 DROUGHT/SMI GIVEN AIR TEMPERATURE
# ------------------------------------------------------------------------------

drought_conditional_results <- purrr::map_dfr(  # Executes this documented step of the workflow.
  conditional_regions,  # Executes this documented step of the workflow.
  function(region_name) {  # Executes this documented step of the workflow.
    purrr::map_dfr(  # Executes this documented step of the workflow.
      conditional_drought_families,  # Executes this documented step of the workflow.
      function(family_name) {  # Executes this documented step of the workflow.
        purrr::map_dfr(  # Executes this documented step of the workflow.
          conditional_drought_datasets,  # Executes this documented step of the workflow.
          function(drought_dataset) {  # Executes this documented step of the workflow.
            run_one_conditional_case(  # Executes this documented step of the workflow.
              region_name = region_name,  # Executes this documented step of the workflow.
              family_name = family_name,  # Executes this documented step of the workflow.
              candidate_dataset = drought_dataset,  # Executes this documented step of the workflow.
              candidate_column = region_name,  # Executes this documented step of the workflow.
              conditioner_dataset = conditional_air_dataset,  # Executes this documented step of the workflow.
              conditioner_column = conditional_air_column,  # Executes this documented step of the workflow.
              direction = "drought_given_air",  # Executes this documented step of the workflow.
              analysis_class = "drought index | air temperature"  # Executes this documented step of the workflow.
            ) %>%  # Executes this documented step of the workflow.
              dplyr::mutate(  # Executes this documented step of the workflow.
                drought_dataset = drought_dataset,  # Executes this documented step of the workflow.
                drought_label = dplyr::case_when(  # Executes this documented step of the workflow.
                  drought_dataset == "SMI_upsoil" ~ "SMI upper soil",  # Executes this documented step of the workflow.
                  drought_dataset == "SMI_total" ~ "SMI total profile",  # Executes this documented step of the workflow.
                  TRUE ~ drought_dataset  # Executes this documented step of the workflow.
                )  # Executes this documented step of the workflow.
              )  # Executes this documented step of the workflow.
          }  # Executes this documented step of the workflow.
        )  # Executes this documented step of the workflow.
      }  # Executes this documented step of the workflow.
    )  # Executes this documented step of the workflow.
  }  # Executes this documented step of the workflow.
)  # Executes this documented step of the workflow.

conditional_granger_all <- dplyr::bind_rows(  # Executes this documented step of the workflow.
  temperature_conditional_results,  # Executes this documented step of the workflow.
  drought_conditional_results  # Executes this documented step of the workflow.
) %>%  # Executes this documented step of the workflow.
  dplyr::mutate(  # Executes this documented step of the workflow.
    raw_significant = is.finite(p_value) & p_value < conditional_alpha,  # Executes this documented step of the workflow.
    interpretation = dplyr::if_else(  # Executes this documented step of the workflow.
      raw_significant,  # Executes this documented step of the workflow.
      "SUPPORTED",  # Executes this documented step of the workflow.
      "AMBIGUOUS"  # Executes this documented step of the workflow.
    )  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.

# ------------------------------------------------------------------------------
# 28.7 FOUR-OUTCOME TEMPERATURE GRID
# ------------------------------------------------------------------------------
# IMPORTANT: "NEITHER_RESOLVED" means neither conditional test crossed alpha.
# It does NOT mean either temperature has no effect.

temperature_soil <- temperature_conditional_results %>%  # Executes this documented step of the workflow.
  dplyr::filter(direction == "soil_given_air") %>%  # Executes this documented step of the workflow.
  dplyr::select(  # Executes this documented step of the workflow.
    region, Family, soil_column, soil_depth_label, Lag,  # Executes this documented step of the workflow.
    soil_p = p_value,  # Executes this documented step of the workflow.
    soil_F = F_stat,  # Executes this documented step of the workflow.
    soil_delta_R2 = delta_R2,  # Executes this documented step of the workflow.
    soil_n = n_obs,  # Executes this documented step of the workflow.
    soil_test_status = test_status,  # Executes this documented step of the workflow.
    soil_interpretation = interpretation  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.

temperature_air <- temperature_conditional_results %>%  # Executes this documented step of the workflow.
  dplyr::filter(direction == "air_given_soil") %>%  # Executes this documented step of the workflow.
  dplyr::select(  # Executes this documented step of the workflow.
    region, Family, soil_column, soil_depth_label, Lag,  # Executes this documented step of the workflow.
    air_p = p_value,  # Executes this documented step of the workflow.
    air_F = F_stat,  # Executes this documented step of the workflow.
    air_delta_R2 = delta_R2,  # Executes this documented step of the workflow.
    air_n = n_obs,  # Executes this documented step of the workflow.
    air_test_status = test_status,  # Executes this documented step of the workflow.
    air_interpretation = interpretation  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.

temperature_outcome_grid <- dplyr::full_join(  # Executes this documented step of the workflow.
  temperature_soil,  # Executes this documented step of the workflow.
  temperature_air,  # Executes this documented step of the workflow.
  by = c("region", "Family", "soil_column", "soil_depth_label", "Lag")  # Executes this documented step of the workflow.
) %>%  # Executes this documented step of the workflow.
  dplyr::mutate(  # Executes this documented step of the workflow.
    soil_supported = is.finite(soil_p) & soil_p < conditional_alpha,  # Executes this documented step of the workflow.
    air_supported = is.finite(air_p) & air_p < conditional_alpha,  # Executes this documented step of the workflow.
    both_tested = is.finite(soil_p) & is.finite(air_p),  # Executes this documented step of the workflow.
    outcome4 = dplyr::case_when(  # Executes this documented step of the workflow.
      !both_tested ~ "NOT_TESTABLE",  # Executes this documented step of the workflow.
      soil_supported & air_supported ~ "BOTH_SUPPORTED",  # Executes this documented step of the workflow.
      soil_supported & !air_supported ~ "SOIL_ONLY_SUPPORTED",  # Executes this documented step of the workflow.
      !soil_supported & air_supported ~ "AIR_ONLY_SUPPORTED",  # Executes this documented step of the workflow.
      TRUE ~ "NEITHER_RESOLVED"  # Executes this documented step of the workflow.
    ),  # Executes this documented step of the workflow.
    interpretation_note = dplyr::case_when(  # Executes this documented step of the workflow.
      outcome4 == "BOTH_SUPPORTED" ~ "Both temperatures add predictive information conditionally.",  # Executes this documented step of the workflow.
      outcome4 == "SOIL_ONLY_SUPPORTED" ~ "Soil adds predictive information beyond air; reverse test is ambiguous.",  # Executes this documented step of the workflow.
      outcome4 == "AIR_ONLY_SUPPORTED" ~ "Air adds predictive information beyond soil; soil-given-air test is ambiguous.",  # Executes this documented step of the workflow.
      outcome4 == "NEITHER_RESOLVED" ~ "Both conditional tests are ambiguous; do not interpret as no effect.",  # Executes this documented step of the workflow.
      TRUE ~ "At least one conditional model was not testable; result is ambiguous."  # Executes this documented step of the workflow.
    )  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.

# ------------------------------------------------------------------------------
# 28.8 DIAGNOSTIC AND SUMMARY TABLES
# ------------------------------------------------------------------------------

conditional_input_audit <- conditional_granger_all %>%  # Executes this documented step of the workflow.
  dplyr::distinct(  # Executes this documented step of the workflow.
    region,  # Executes this documented step of the workflow.
    Family,  # Executes this documented step of the workflow.
    direction,  # Executes this documented step of the workflow.
    analysis_class,  # Executes this documented step of the workflow.
    candidate_dataset,  # Executes this documented step of the workflow.
    candidate_column,  # Executes this documented step of the workflow.
    candidate_analysis_column,  # Executes this documented step of the workflow.
    candidate_was_deseasonalised,  # Executes this documented step of the workflow.
    conditioner_dataset,  # Executes this documented step of the workflow.
    conditioner_column,  # Executes this documented step of the workflow.
    conditioner_analysis_column,  # Executes this documented step of the workflow.
    conditioner_was_deseasonalised,  # Executes this documented step of the workflow.
    insect_analysis_column,  # Executes this documented step of the workflow.
    insect_was_deseasonalised,  # Executes this documented step of the workflow.
    test_status  # Executes this documented step of the workflow.
  ) %>%  # Executes this documented step of the workflow.
  dplyr::arrange(region, analysis_class, Family, candidate_dataset, candidate_column)  # Executes this documented step of the workflow.

conditional_summary_region_direction_lag <- conditional_granger_all %>%  # Executes this documented step of the workflow.
  dplyr::group_by(region, direction, analysis_class, Lag) %>%  # Executes this documented step of the workflow.
  dplyr::summarise(  # Executes this documented step of the workflow.
    n_planned = dplyr::n(),  # Executes this documented step of the workflow.
    n_tested = sum(is.finite(p_value)),  # Executes this documented step of the workflow.
    n_supported = sum(is.finite(p_value) & p_value < conditional_alpha),  # Executes this documented step of the workflow.
    n_ambiguous = sum(!(is.finite(p_value) & p_value < conditional_alpha)),  # Executes this documented step of the workflow.
    pct_supported = dplyr::if_else(  # Executes this documented step of the workflow.
      n_tested > 0,  # Executes this documented step of the workflow.
      100 * n_supported / n_tested,  # Executes this documented step of the workflow.
      NA_real_  # Executes this documented step of the workflow.
    ),  # Executes this documented step of the workflow.
    median_n_obs = stats::median(n_obs[is.finite(n_obs)], na.rm = TRUE),  # Executes this documented step of the workflow.
    median_delta_R2 = stats::median(delta_R2[is.finite(delta_R2)], na.rm = TRUE),  # Executes this documented step of the workflow.
    .groups = "drop"  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.

conditional_summary_family <- conditional_granger_all %>%  # Executes this documented step of the workflow.
  dplyr::group_by(region, Family, direction, analysis_class) %>%  # Executes this documented step of the workflow.
  dplyr::summarise(  # Executes this documented step of the workflow.
    n_tests = sum(is.finite(p_value)),  # Executes this documented step of the workflow.
    n_supported = sum(is.finite(p_value) & p_value < conditional_alpha),  # Executes this documented step of the workflow.
    min_raw_p = if (any(is.finite(p_value))) min(p_value, na.rm = TRUE) else NA_real_,  # Executes this documented step of the workflow.
    max_delta_R2 = if (any(is.finite(delta_R2))) max(delta_R2, na.rm = TRUE) else NA_real_,  # Executes this documented step of the workflow.
    .groups = "drop"  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.

conditional_sample_size_table <- conditional_granger_all %>%  # Executes this documented step of the workflow.
  dplyr::select(  # Executes this documented step of the workflow.
    region, Family, direction, analysis_class,  # Executes this documented step of the workflow.
    candidate_dataset, candidate_column,  # Executes this documented step of the workflow.
    conditioner_dataset, conditioner_column,  # Executes this documented step of the workflow.
    Lag, n_obs, n_years, test_status  # Executes this documented step of the workflow.
  ) %>%  # Executes this documented step of the workflow.
  dplyr::arrange(region, analysis_class, Family, Lag)  # Executes this documented step of the workflow.

conditional_collinearity_table <- conditional_granger_all %>%  # Executes this documented step of the workflow.
  dplyr::filter(is.finite(candidate_conditioner_cor)) %>%  # Executes this documented step of the workflow.
  dplyr::distinct(  # Executes this documented step of the workflow.
    region,  # Executes this documented step of the workflow.
    direction,  # Executes this documented step of the workflow.
    candidate_dataset,  # Executes this documented step of the workflow.
    candidate_column,  # Executes this documented step of the workflow.
    conditioner_dataset,  # Executes this documented step of the workflow.
    conditioner_column,  # Executes this documented step of the workflow.
    candidate_conditioner_cor  # Executes this documented step of the workflow.
  ) %>%  # Executes this documented step of the workflow.
  dplyr::arrange(region, direction, candidate_dataset, candidate_column)  # Executes this documented step of the workflow.

conditional_outcome_counts <- temperature_outcome_grid %>%  # Executes this documented step of the workflow.
  dplyr::count(region, Lag, outcome4, name = "n") %>%  # Executes this documented step of the workflow.
  dplyr::group_by(region, Lag) %>%  # Executes this documented step of the workflow.
  dplyr::mutate(pct = 100 * n / sum(n)) %>%  # Executes this documented step of the workflow.
  dplyr::ungroup()  # Executes this documented step of the workflow.

write.csv(  # Executes this documented step of the workflow.
  conditional_granger_all,  # Executes this documented step of the workflow.
  file.path(out_conditional_tables, "conditional_granger_all_tests.csv"),  # Executes this documented step of the workflow.
  row.names = FALSE  # Executes this documented step of the workflow.
)  # Executes this documented step of the workflow.

write.csv(  # Executes this documented step of the workflow.
  temperature_conditional_results,  # Executes this documented step of the workflow.
  file.path(out_conditional_tables, "conditional_granger_temperature_bidirectional.csv"),  # Executes this documented step of the workflow.
  row.names = FALSE  # Executes this documented step of the workflow.
)  # Executes this documented step of the workflow.

write.csv(  # Executes this documented step of the workflow.
  drought_conditional_results,  # Executes this documented step of the workflow.
  file.path(out_conditional_tables, "conditional_granger_drought_given_air.csv"),  # Executes this documented step of the workflow.
  row.names = FALSE  # Executes this documented step of the workflow.
)  # Executes this documented step of the workflow.

write.csv(  # Executes this documented step of the workflow.
  temperature_outcome_grid,  # Executes this documented step of the workflow.
  file.path(out_conditional_tables, "conditional_granger_temperature_outcome_grid.csv"),  # Executes this documented step of the workflow.
  row.names = FALSE  # Executes this documented step of the workflow.
)  # Executes this documented step of the workflow.

write.csv(  # Executes this documented step of the workflow.
  conditional_input_audit,  # Executes this documented step of the workflow.
  file.path(out_conditional_diagnostics, "conditional_granger_input_audit.csv"),  # Executes this documented step of the workflow.
  row.names = FALSE  # Executes this documented step of the workflow.
)  # Executes this documented step of the workflow.

write.csv(  # Executes this documented step of the workflow.
  conditional_summary_region_direction_lag,  # Executes this documented step of the workflow.
  file.path(out_conditional_tables, "conditional_granger_summary_region_direction_lag.csv"),  # Executes this documented step of the workflow.
  row.names = FALSE  # Executes this documented step of the workflow.
)  # Executes this documented step of the workflow.

write.csv(  # Executes this documented step of the workflow.
  conditional_summary_family,  # Executes this documented step of the workflow.
  file.path(out_conditional_tables, "conditional_granger_summary_family.csv"),  # Executes this documented step of the workflow.
  row.names = FALSE  # Executes this documented step of the workflow.
)  # Executes this documented step of the workflow.

write.csv(  # Executes this documented step of the workflow.
  conditional_sample_size_table,  # Executes this documented step of the workflow.
  file.path(out_conditional_diagnostics, "conditional_granger_sample_sizes.csv"),  # Executes this documented step of the workflow.
  row.names = FALSE  # Executes this documented step of the workflow.
)  # Executes this documented step of the workflow.

write.csv(  # Executes this documented step of the workflow.
  conditional_collinearity_table,  # Executes this documented step of the workflow.
  file.path(out_conditional_diagnostics, "conditional_granger_predictor_collinearity.csv"),  # Executes this documented step of the workflow.
  row.names = FALSE  # Executes this documented step of the workflow.
)  # Executes this documented step of the workflow.

write.csv(  # Executes this documented step of the workflow.
  conditional_outcome_counts,  # Executes this documented step of the workflow.
  file.path(out_conditional_tables, "conditional_granger_temperature_outcome_counts.csv"),  # Executes this documented step of the workflow.
  row.names = FALSE  # Executes this documented step of the workflow.
)  # Executes this documented step of the workflow.

# ------------------------------------------------------------------------------
# 28.9 PLOTS: TEMPERATURE FOUR-OUTCOME GRID
# ------------------------------------------------------------------------------

if (nrow(temperature_outcome_grid) > 0L) {  # Executes this documented step of the workflow.

  p_outcome_grid <- ggplot2::ggplot(  # Executes this documented step of the workflow.
    temperature_outcome_grid,  # Executes this documented step of the workflow.
    ggplot2::aes(  # Executes this documented step of the workflow.
      x = Family,  # Executes this documented step of the workflow.
      y = soil_depth_label,  # Executes this documented step of the workflow.
      fill = outcome4  # Executes this documented step of the workflow.
    )  # Executes this documented step of the workflow.
  ) +  # Executes this documented step of the workflow.
    ggplot2::geom_tile(colour = "white") +  # Executes this documented step of the workflow.
    ggplot2::facet_grid(region ~ Lag) +  # Executes this documented step of the workflow.
    ggplot2::labs(  # Executes this documented step of the workflow.
      title = "Conditional Granger outcome grid: soil temperature versus air temperature",  # Executes this documented step of the workflow.
      subtitle = "Non-significant directions are AMBIGUOUS; NEITHER_RESOLVED does not mean no effect",  # Executes this documented step of the workflow.
      x = "Insect family",  # Executes this documented step of the workflow.
      y = "Soil-temperature predictor",  # Executes this documented step of the workflow.
      fill = "Outcome"  # Executes this documented step of the workflow.
    ) +  # Executes this documented step of the workflow.
    ggplot2::theme_minimal(base_size = 10) +  # Executes this documented step of the workflow.
    ggplot2::theme(  # Executes this documented step of the workflow.
      panel.grid = ggplot2::element_blank(),  # Executes this documented step of the workflow.
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)  # Executes this documented step of the workflow.
    )  # Executes this documented step of the workflow.

  ggplot2::ggsave(  # Executes this documented step of the workflow.
    file.path(out_conditional_plots, "conditional_temperature_four_outcome_grid.png"),  # Executes this documented step of the workflow.
    p_outcome_grid,  # Executes this documented step of the workflow.
    width = 14,  # Executes this documented step of the workflow.
    height = 9,  # Executes this documented step of the workflow.
    dpi = 300,  # Executes this documented step of the workflow.
    bg = "white"  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.

  p_outcome_counts <- ggplot2::ggplot(  # Executes this documented step of the workflow.
    conditional_outcome_counts,  # Executes this documented step of the workflow.
    ggplot2::aes(x = outcome4, y = n, fill = outcome4)  # Executes this documented step of the workflow.
  ) +  # Executes this documented step of the workflow.
    ggplot2::geom_col() +  # Executes this documented step of the workflow.
    ggplot2::facet_grid(region ~ Lag, scales = "free_y") +  # Executes this documented step of the workflow.
    ggplot2::labs(  # Executes this documented step of the workflow.
      title = "Counts of the four conditional temperature outcomes",  # Executes this documented step of the workflow.
      x = "Outcome",  # Executes this documented step of the workflow.
      y = "Number of family-depth comparisons",  # Executes this documented step of the workflow.
      fill = "Outcome"  # Executes this documented step of the workflow.
    ) +  # Executes this documented step of the workflow.
    ggplot2::theme_minimal(base_size = 10) +  # Executes this documented step of the workflow.
    ggplot2::theme(  # Executes this documented step of the workflow.
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),  # Executes this documented step of the workflow.
      panel.grid.minor = ggplot2::element_blank()  # Executes this documented step of the workflow.
    )  # Executes this documented step of the workflow.

  ggplot2::ggsave(  # Executes this documented step of the workflow.
    file.path(out_conditional_plots, "conditional_temperature_outcome_counts.png"),  # Executes this documented step of the workflow.
    p_outcome_counts,  # Executes this documented step of the workflow.
    width = 14,  # Executes this documented step of the workflow.
    height = 8,  # Executes this documented step of the workflow.
    dpi = 300,  # Executes this documented step of the workflow.
    bg = "white"  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.
}  # Executes this documented step of the workflow.

# ------------------------------------------------------------------------------
# 28.10 PLOTS: RAW P-VALUES, F STATISTICS AND EFFECT-SIZE ADDITION
# ------------------------------------------------------------------------------

conditional_plot_data <- conditional_granger_all %>%  # Executes this documented step of the workflow.
  dplyr::filter(is.finite(p_value)) %>%  # Executes this documented step of the workflow.
  dplyr::mutate(  # Executes this documented step of the workflow.
    neglog10_p = -log10(pmax(p_value, .Machine$double.xmin)),  # Executes this documented step of the workflow.
    predictor_label = dplyr::case_when(  # Executes this documented step of the workflow.
      direction %in% c("soil_given_air", "air_given_soil") ~  # Executes this documented step of the workflow.
        unname(conditional_soil_labels[soil_column]),  # Executes this documented step of the workflow.
      direction == "drought_given_air" ~ drought_label,  # Executes this documented step of the workflow.
      TRUE ~ candidate_column  # Executes this documented step of the workflow.
    )  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.

if (nrow(conditional_plot_data) > 0L) {  # Executes this documented step of the workflow.

  p_pvalue_heatmap <- ggplot2::ggplot(  # Executes this documented step of the workflow.
    conditional_plot_data,  # Executes this documented step of the workflow.
    ggplot2::aes(x = Family, y = predictor_label, fill = neglog10_p)  # Executes this documented step of the workflow.
  ) +  # Executes this documented step of the workflow.
    ggplot2::geom_tile(colour = "white") +  # Executes this documented step of the workflow.
    ggplot2::facet_grid(direction + region ~ Lag, scales = "free_y", space = "free_y") +  # Executes this documented step of the workflow.
    ggplot2::labs(  # Executes this documented step of the workflow.
      title = "Conditional Granger evidence across directions",  # Executes this documented step of the workflow.
      subtitle = "Fill is -log10(raw p); larger values indicate stronger conditional evidence",  # Executes this documented step of the workflow.
      x = "Insect family",  # Executes this documented step of the workflow.
      y = "Candidate/conditioning comparison",  # Executes this documented step of the workflow.
      fill = "-log10(p)"  # Executes this documented step of the workflow.
    ) +  # Executes this documented step of the workflow.
    ggplot2::theme_minimal(base_size = 9) +  # Executes this documented step of the workflow.
    ggplot2::theme(  # Executes this documented step of the workflow.
      panel.grid = ggplot2::element_blank(),  # Executes this documented step of the workflow.
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)  # Executes this documented step of the workflow.
    )  # Executes this documented step of the workflow.

  ggplot2::ggsave(  # Executes this documented step of the workflow.
    file.path(out_conditional_plots, "conditional_granger_raw_p_heatmap.png"),  # Executes this documented step of the workflow.
    p_pvalue_heatmap,  # Executes this documented step of the workflow.
    width = 15,  # Executes this documented step of the workflow.
    height = 16,  # Executes this documented step of the workflow.
    dpi = 300,  # Executes this documented step of the workflow.
    bg = "white",  # Executes this documented step of the workflow.
    limitsize = FALSE  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.

  p_F_heatmap <- ggplot2::ggplot(  # Executes this documented step of the workflow.
    conditional_plot_data %>% dplyr::filter(is.finite(F_stat)),  # Executes this documented step of the workflow.
    ggplot2::aes(x = Family, y = predictor_label, fill = F_stat)  # Executes this documented step of the workflow.
  ) +  # Executes this documented step of the workflow.
    ggplot2::geom_tile(colour = "white") +  # Executes this documented step of the workflow.
    ggplot2::facet_grid(direction + region ~ Lag, scales = "free_y", space = "free_y") +  # Executes this documented step of the workflow.
    ggplot2::labs(  # Executes this documented step of the workflow.
      title = "Conditional Granger F statistics",  # Executes this documented step of the workflow.
      x = "Insect family",  # Executes this documented step of the workflow.
      y = "Candidate/conditioning comparison",  # Executes this documented step of the workflow.
      fill = "F"  # Executes this documented step of the workflow.
    ) +  # Executes this documented step of the workflow.
    ggplot2::theme_minimal(base_size = 9) +  # Executes this documented step of the workflow.
    ggplot2::theme(  # Executes this documented step of the workflow.
      panel.grid = ggplot2::element_blank(),  # Executes this documented step of the workflow.
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)  # Executes this documented step of the workflow.
    )  # Executes this documented step of the workflow.

  ggplot2::ggsave(  # Executes this documented step of the workflow.
    file.path(out_conditional_plots, "conditional_granger_F_heatmap.png"),  # Executes this documented step of the workflow.
    p_F_heatmap,  # Executes this documented step of the workflow.
    width = 15,  # Executes this documented step of the workflow.
    height = 16,  # Executes this documented step of the workflow.
    dpi = 300,  # Executes this documented step of the workflow.
    bg = "white",  # Executes this documented step of the workflow.
    limitsize = FALSE  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.

  p_deltaR2_heatmap <- ggplot2::ggplot(  # Executes this documented step of the workflow.
    conditional_plot_data %>% dplyr::filter(is.finite(delta_R2)),  # Executes this documented step of the workflow.
    ggplot2::aes(x = Family, y = predictor_label, fill = delta_R2)  # Executes this documented step of the workflow.
  ) +  # Executes this documented step of the workflow.
    ggplot2::geom_tile(colour = "white") +  # Executes this documented step of the workflow.
    ggplot2::facet_grid(direction + region ~ Lag, scales = "free_y", space = "free_y") +  # Executes this documented step of the workflow.
    ggplot2::labs(  # Executes this documented step of the workflow.
      title = "Additional variance explained by the conditional candidate",  # Executes this documented step of the workflow.
      subtitle = "Delta R-squared = full model R-squared minus restricted model R-squared",  # Executes this documented step of the workflow.
      x = "Insect family",  # Executes this documented step of the workflow.
      y = "Candidate/conditioning comparison",  # Executes this documented step of the workflow.
      fill = "Delta R2"  # Executes this documented step of the workflow.
    ) +  # Executes this documented step of the workflow.
    ggplot2::theme_minimal(base_size = 9) +  # Executes this documented step of the workflow.
    ggplot2::theme(  # Executes this documented step of the workflow.
      panel.grid = ggplot2::element_blank(),  # Executes this documented step of the workflow.
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)  # Executes this documented step of the workflow.
    )  # Executes this documented step of the workflow.

  ggplot2::ggsave(  # Executes this documented step of the workflow.
    file.path(out_conditional_plots, "conditional_granger_delta_R2_heatmap.png"),  # Executes this documented step of the workflow.
    p_deltaR2_heatmap,  # Executes this documented step of the workflow.
    width = 15,  # Executes this documented step of the workflow.
    height = 16,  # Executes this documented step of the workflow.
    dpi = 300,  # Executes this documented step of the workflow.
    bg = "white",  # Executes this documented step of the workflow.
    limitsize = FALSE  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.

  p_partialR2_vs_p <- ggplot2::ggplot(  # Executes this documented step of the workflow.
    conditional_plot_data %>% dplyr::filter(is.finite(partial_R2)),  # Executes this documented step of the workflow.
    ggplot2::aes(x = partial_R2, y = neglog10_p, shape = direction)  # Executes this documented step of the workflow.
  ) +  # Executes this documented step of the workflow.
    ggplot2::geom_point(alpha = 0.7) +  # Executes this documented step of the workflow.
    ggplot2::geom_hline(  # Executes this documented step of the workflow.
      yintercept = -log10(conditional_alpha),  # Executes this documented step of the workflow.
      linetype = 2  # Executes this documented step of the workflow.
    ) +  # Executes this documented step of the workflow.
    ggplot2::facet_wrap(~ region) +  # Executes this documented step of the workflow.
    ggplot2::labs(  # Executes this documented step of the workflow.
      title = "Conditional effect size versus statistical evidence",  # Executes this documented step of the workflow.
      x = "Partial R-squared of candidate lags",  # Executes this documented step of the workflow.
      y = "-log10(raw p)",  # Executes this documented step of the workflow.
      shape = "Direction"  # Executes this documented step of the workflow.
    ) +  # Executes this documented step of the workflow.
    ggplot2::theme_minimal(base_size = 10)  # Executes this documented step of the workflow.

  ggplot2::ggsave(  # Executes this documented step of the workflow.
    file.path(out_conditional_plots, "conditional_granger_partial_R2_vs_p.png"),  # Executes this documented step of the workflow.
    p_partialR2_vs_p,  # Executes this documented step of the workflow.
    width = 10,  # Executes this documented step of the workflow.
    height = 6,  # Executes this documented step of the workflow.
    dpi = 300,  # Executes this documented step of the workflow.
    bg = "white"  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.
}  # Executes this documented step of the workflow.

# ------------------------------------------------------------------------------
# 28.11 PLOTS: SAMPLE SIZE, COLLINEARITY, DROUGHT AND STATUS
# ------------------------------------------------------------------------------

sample_plot_data <- conditional_granger_all %>%  # Executes this documented step of the workflow.
  dplyr::filter(is.finite(n_obs)) %>%  # Executes this documented step of the workflow.
  dplyr::mutate(  # Executes this documented step of the workflow.
    predictor_label = dplyr::case_when(  # Executes this documented step of the workflow.
      direction %in% c("soil_given_air", "air_given_soil") ~  # Executes this documented step of the workflow.
        unname(conditional_soil_labels[soil_column]),  # Executes this documented step of the workflow.
      direction == "drought_given_air" ~ drought_label,  # Executes this documented step of the workflow.
      TRUE ~ candidate_column  # Executes this documented step of the workflow.
    )  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.

if (nrow(sample_plot_data) > 0L) {  # Executes this documented step of the workflow.
  p_sample_size <- ggplot2::ggplot(  # Executes this documented step of the workflow.
    sample_plot_data,  # Executes this documented step of the workflow.
    ggplot2::aes(x = Family, y = predictor_label, fill = n_obs)  # Executes this documented step of the workflow.
  ) +  # Executes this documented step of the workflow.
    ggplot2::geom_tile(colour = "white") +  # Executes this documented step of the workflow.
    ggplot2::facet_grid(direction + region ~ Lag, scales = "free_y", space = "free_y") +  # Executes this documented step of the workflow.
    ggplot2::labs(  # Executes this documented step of the workflow.
      title = "Conditional Granger effective sample sizes",  # Executes this documented step of the workflow.
      subtitle = "Counts include only observations with the exact calendar lags required by each model",  # Executes this documented step of the workflow.
      x = "Insect family",  # Executes this documented step of the workflow.
      y = "Comparison",  # Executes this documented step of the workflow.
      fill = "n"  # Executes this documented step of the workflow.
    ) +  # Executes this documented step of the workflow.
    ggplot2::theme_minimal(base_size = 9) +  # Executes this documented step of the workflow.
    ggplot2::theme(  # Executes this documented step of the workflow.
      panel.grid = ggplot2::element_blank(),  # Executes this documented step of the workflow.
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)  # Executes this documented step of the workflow.
    )  # Executes this documented step of the workflow.

  ggplot2::ggsave(  # Executes this documented step of the workflow.
    file.path(out_conditional_plots, "conditional_granger_sample_size_heatmap.png"),  # Executes this documented step of the workflow.
    p_sample_size,  # Executes this documented step of the workflow.
    width = 15,  # Executes this documented step of the workflow.
    height = 16,  # Executes this documented step of the workflow.
    dpi = 300,  # Executes this documented step of the workflow.
    bg = "white",  # Executes this documented step of the workflow.
    limitsize = FALSE  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.
}  # Executes this documented step of the workflow.

if (nrow(conditional_collinearity_table) > 0L) {  # Executes this documented step of the workflow.
  p_collinearity <- ggplot2::ggplot(  # Executes this documented step of the workflow.
    conditional_collinearity_table,  # Executes this documented step of the workflow.
    ggplot2::aes(  # Executes this documented step of the workflow.
      x = candidate_conditioner_cor,  # Executes this documented step of the workflow.
      y = paste(candidate_dataset, candidate_column, sep = ":")  # Executes this documented step of the workflow.
    )  # Executes this documented step of the workflow.
  ) +  # Executes this documented step of the workflow.
    ggplot2::geom_point(size = 2) +  # Executes this documented step of the workflow.
    ggplot2::facet_grid(direction ~ region, scales = "free_y") +  # Executes this documented step of the workflow.
    ggplot2::labs(  # Executes this documented step of the workflow.
      title = "Correlation between candidate and conditioning environmental predictors",  # Executes this documented step of the workflow.
      subtitle = "Very high correlation can make a non-significant conditional test uninformative rather than evidence of absence",  # Executes this documented step of the workflow.
      x = "Contemporaneous Pearson correlation",  # Executes this documented step of the workflow.
      y = "Candidate predictor"  # Executes this documented step of the workflow.
    ) +  # Executes this documented step of the workflow.
    ggplot2::theme_minimal(base_size = 10)  # Executes this documented step of the workflow.

  ggplot2::ggsave(  # Executes this documented step of the workflow.
    file.path(out_conditional_plots, "conditional_granger_predictor_collinearity.png"),  # Executes this documented step of the workflow.
    p_collinearity,  # Executes this documented step of the workflow.
    width = 12,  # Executes this documented step of the workflow.
    height = 8,  # Executes this documented step of the workflow.
    dpi = 300,  # Executes this documented step of the workflow.
    bg = "white"  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.
}  # Executes this documented step of the workflow.

if (nrow(drought_conditional_results) > 0L) {  # Executes this documented step of the workflow.
  drought_plot_data <- drought_conditional_results %>%  # Executes this documented step of the workflow.
    dplyr::filter(is.finite(p_value)) %>%  # Executes this documented step of the workflow.
    dplyr::mutate(neglog10_p = -log10(pmax(p_value, .Machine$double.xmin)))  # Executes this documented step of the workflow.

  if (nrow(drought_plot_data) > 0L) {  # Executes this documented step of the workflow.
    p_drought <- ggplot2::ggplot(  # Executes this documented step of the workflow.
      drought_plot_data,  # Executes this documented step of the workflow.
      ggplot2::aes(x = Family, y = drought_label, fill = neglog10_p)  # Executes this documented step of the workflow.
    ) +  # Executes this documented step of the workflow.
      ggplot2::geom_tile(colour = "white") +  # Executes this documented step of the workflow.
      ggplot2::facet_grid(region ~ Lag) +  # Executes this documented step of the workflow.
      ggplot2::labs(  # Executes this documented step of the workflow.
        title = "Drought index given air temperature",  # Executes this documented step of the workflow.
        subtitle = "Pentatomidae and Scarabaeidae; fill is -log10(raw p)",  # Executes this documented step of the workflow.
        x = "Insect family",  # Executes this documented step of the workflow.
        y = "Drought/soil-moisture index",  # Executes this documented step of the workflow.
        fill = "-log10(p)"  # Executes this documented step of the workflow.
      ) +  # Executes this documented step of the workflow.
      ggplot2::theme_minimal(base_size = 10) +  # Executes this documented step of the workflow.
      ggplot2::theme(panel.grid = ggplot2::element_blank())  # Executes this documented step of the workflow.

    ggplot2::ggsave(  # Executes this documented step of the workflow.
      file.path(out_conditional_plots, "conditional_drought_given_air_p_heatmap.png"),  # Executes this documented step of the workflow.
      p_drought,  # Executes this documented step of the workflow.
      width = 10,  # Executes this documented step of the workflow.
      height = 8,  # Executes this documented step of the workflow.
      dpi = 300,  # Executes this documented step of the workflow.
      bg = "white"  # Executes this documented step of the workflow.
    )  # Executes this documented step of the workflow.
  }  # Executes this documented step of the workflow.
}  # Executes this documented step of the workflow.

status_plot_data <- conditional_granger_all %>%  # Executes this documented step of the workflow.
  dplyr::count(region, direction, interpretation, name = "n")  # Executes this documented step of the workflow.

if (nrow(status_plot_data) > 0L) {  # Executes this documented step of the workflow.
  p_status <- ggplot2::ggplot(  # Executes this documented step of the workflow.
    status_plot_data,  # Executes this documented step of the workflow.
    ggplot2::aes(x = direction, y = n, fill = interpretation)  # Executes this documented step of the workflow.
  ) +  # Executes this documented step of the workflow.
    ggplot2::geom_col(position = "dodge") +  # Executes this documented step of the workflow.
    ggplot2::facet_wrap(~ region, scales = "free_y") +  # Executes this documented step of the workflow.
    ggplot2::labs(  # Executes this documented step of the workflow.
      title = "Supported versus ambiguous conditional Granger results",  # Executes this documented step of the workflow.
      subtitle = "Every non-significant or non-testable result is labelled AMBIGUOUS",  # Executes this documented step of the workflow.
      x = "Conditional direction",  # Executes this documented step of the workflow.
      y = "Number of tests",  # Executes this documented step of the workflow.
      fill = "Interpretation"  # Executes this documented step of the workflow.
    ) +  # Executes this documented step of the workflow.
    ggplot2::theme_minimal(base_size = 10) +  # Executes this documented step of the workflow.
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))  # Executes this documented step of the workflow.

  ggplot2::ggsave(  # Executes this documented step of the workflow.
    file.path(out_conditional_plots, "conditional_granger_supported_vs_ambiguous.png"),  # Executes this documented step of the workflow.
    p_status,  # Executes this documented step of the workflow.
    width = 11,  # Executes this documented step of the workflow.
    height = 6,  # Executes this documented step of the workflow.
    dpi = 300,  # Executes this documented step of the workflow.
    bg = "white"  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.
}  # Executes this documented step of the workflow.

# Raw conditional p-value histogram.
conditional_p_hist_data <- conditional_granger_all %>%  # Executes this documented step of the workflow.
  dplyr::filter(is.finite(p_value))  # Executes this documented step of the workflow.

if (nrow(conditional_p_hist_data) > 0L) {  # Executes this documented step of the workflow.
  p_conditional_hist <- ggplot2::ggplot(  # Executes this documented step of the workflow.
    conditional_p_hist_data,  # Executes this documented step of the workflow.
    ggplot2::aes(x = p_value)  # Executes this documented step of the workflow.
  ) +  # Executes this documented step of the workflow.
    ggplot2::geom_histogram(binwidth = 0.05, boundary = 0, closed = "left") +  # Executes this documented step of the workflow.
    ggplot2::geom_vline(xintercept = conditional_alpha, linetype = 2) +  # Executes this documented step of the workflow.
    ggplot2::facet_grid(direction ~ region) +  # Executes this documented step of the workflow.
    ggplot2::labs(  # Executes this documented step of the workflow.
      title = "Raw conditional Granger p-value distributions",  # Executes this documented step of the workflow.
      x = "Raw p-value",  # Executes this documented step of the workflow.
      y = "Count"  # Executes this documented step of the workflow.
    ) +  # Executes this documented step of the workflow.
    ggplot2::theme_minimal(base_size = 10)  # Executes this documented step of the workflow.

  ggplot2::ggsave(  # Executes this documented step of the workflow.
    file.path(out_conditional_plots, "conditional_granger_pvalue_histograms.png"),  # Executes this documented step of the workflow.
    p_conditional_hist,  # Executes this documented step of the workflow.
    width = 12,  # Executes this documented step of the workflow.
    height = 9,  # Executes this documented step of the workflow.
    dpi = 300,  # Executes this documented step of the workflow.
    bg = "white"  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.
}  # Executes this documented step of the workflow.

# ------------------------------------------------------------------------------
# 28.12 SAVE REUSABLE OBJECT AND OUTPUT MANIFEST
# ------------------------------------------------------------------------------

saveRDS(  # Executes this documented step of the workflow.
  list(  # Executes this documented step of the workflow.
    all_tests = conditional_granger_all,  # Executes this documented step of the workflow.
    temperature_bidirectional = temperature_conditional_results,  # Executes this documented step of the workflow.
    drought_given_air = drought_conditional_results,  # Executes this documented step of the workflow.
    temperature_outcome_grid = temperature_outcome_grid,  # Executes this documented step of the workflow.
    input_audit = conditional_input_audit,  # Executes this documented step of the workflow.
    collinearity = conditional_collinearity_table,  # Executes this documented step of the workflow.
    sample_sizes = conditional_sample_size_table,  # Executes this documented step of the workflow.
    region_direction_lag_summary = conditional_summary_region_direction_lag,  # Executes this documented step of the workflow.
    family_summary = conditional_summary_family  # Executes this documented step of the workflow.
  ),  # Executes this documented step of the workflow.
  file.path(out_conditional_objects, "conditional_granger_calendar_aware_results.rds")  # Executes this documented step of the workflow.
)  # Executes this documented step of the workflow.

conditional_output_manifest <- c(  # Executes this documented step of the workflow.
  "CALENDAR-AWARE CONDITIONAL GRANGER OUTPUTS",  # Executes this documented step of the workflow.
  "==========================================",  # Executes this documented step of the workflow.
  "",  # Executes this documented step of the workflow.
  "MAIN TABLES",  # Executes this documented step of the workflow.
  "tables/conditional_granger_all_tests.csv",  # Executes this documented step of the workflow.
  "tables/conditional_granger_temperature_bidirectional.csv",  # Executes this documented step of the workflow.
  "tables/conditional_granger_drought_given_air.csv",  # Executes this documented step of the workflow.
  "tables/conditional_granger_temperature_outcome_grid.csv",  # Executes this documented step of the workflow.
  "tables/conditional_granger_temperature_outcome_counts.csv",  # Executes this documented step of the workflow.
  "tables/conditional_granger_summary_region_direction_lag.csv",  # Executes this documented step of the workflow.
  "tables/conditional_granger_summary_family.csv",  # Executes this documented step of the workflow.
  "",  # Executes this documented step of the workflow.
  "DIAGNOSTICS",  # Executes this documented step of the workflow.
  "diagnostics/conditional_granger_input_audit.csv",  # Executes this documented step of the workflow.
  "diagnostics/conditional_granger_sample_sizes.csv",  # Executes this documented step of the workflow.
  "diagnostics/conditional_granger_predictor_collinearity.csv",  # Executes this documented step of the workflow.
  "",  # Executes this documented step of the workflow.
  "PLOTS",  # Executes this documented step of the workflow.
  "plots/conditional_temperature_four_outcome_grid.png",  # Executes this documented step of the workflow.
  "plots/conditional_temperature_outcome_counts.png",  # Executes this documented step of the workflow.
  "plots/conditional_granger_raw_p_heatmap.png",  # Executes this documented step of the workflow.
  "plots/conditional_granger_F_heatmap.png",  # Executes this documented step of the workflow.
  "plots/conditional_granger_delta_R2_heatmap.png",  # Executes this documented step of the workflow.
  "plots/conditional_granger_partial_R2_vs_p.png",  # Executes this documented step of the workflow.
  "plots/conditional_granger_sample_size_heatmap.png",  # Executes this documented step of the workflow.
  "plots/conditional_granger_predictor_collinearity.png",  # Executes this documented step of the workflow.
  "plots/conditional_drought_given_air_p_heatmap.png",  # Executes this documented step of the workflow.
  "plots/conditional_granger_supported_vs_ambiguous.png",  # Executes this documented step of the workflow.
  "plots/conditional_granger_pvalue_histograms.png",  # Executes this documented step of the workflow.
  "",  # Executes this documented step of the workflow.
  "R OBJECT",  # Executes this documented step of the workflow.
  "objects/conditional_granger_calendar_aware_results.rds"  # Executes this documented step of the workflow.
)  # Executes this documented step of the workflow.

writeLines(  # Executes this documented step of the workflow.
  conditional_output_manifest,  # Executes this documented step of the workflow.
  file.path(out_conditional, "README_conditional_granger_outputs.txt")  # Executes this documented step of the workflow.
)  # Executes this documented step of the workflow.

cat(  # Executes this documented step of the workflow.
  "\nSECTION 28 CONDITIONAL GRANGER ANALYSIS DONE.\n",  # Executes this documented step of the workflow.
  "Outputs: ", normalizePath(out_conditional, mustWork = FALSE), "\n",  # Executes this documented step of the workflow.
  sep = ""  # Executes this documented step of the workflow.
)  # Executes this documented step of the workflow.

# ==============================================================================
# END OF SECTION 28
# ==============================================================================


# ==============================================================================
# 29. BY MULTIPLE-TESTING CORRECTION ACROSS ALL OBSERVED GRANGER TESTS
# ==============================================================================
# PURPOSE
# - Apply the Benjamini-Yekutieli (BY) correction to ALL observed Granger
#   p-values from the calendar-aware pairwise analysis (Section 27) and the
#   conditional analyses (Section 28) in one joint family of tests.
# - Summarise how many raw signals survive BY by region and test class.
# - Produce reviewer-facing p-value diagnostics.
# - Save a complete supplementary table containing every raw and BY-corrected
#   p-value.
#
# IMPORTANT
# Surrogate empirical p-values from Section 27 are NOT mixed into this BY family.
# They test a different level of hypothesis (null-distribution/global robustness)
# from the observed Granger tests. Section 29 corrects the observed Granger
# tests themselves: pairwise + conditional.
# ============================================================================== 

# ------------------------------------------------------------------------------
# 29.1 CONFIGURATION AND OUTPUT DIRECTORIES
# ------------------------------------------------------------------------------

multiple_testing_alpha <- 0.05  # Executes this documented step of the workflow.

out_multiple <- file.path(out_root, "multiple_testing_BY_20261005")  # Executes this documented step of the workflow.
out_multiple_tables <- file.path(out_multiple, "tables")  # Executes this documented step of the workflow.
out_multiple_plots <- file.path(out_multiple, "plots")  # Executes this documented step of the workflow.
out_multiple_objects <- file.path(out_multiple, "objects")  # Executes this documented step of the workflow.

dir.create(out_multiple, recursive = TRUE, showWarnings = FALSE)  # Executes this documented step of the workflow.
dir.create(out_multiple_tables, recursive = TRUE, showWarnings = FALSE)  # Executes this documented step of the workflow.
dir.create(out_multiple_plots, recursive = TRUE, showWarnings = FALSE)  # Executes this documented step of the workflow.
dir.create(out_multiple_objects, recursive = TRUE, showWarnings = FALSE)  # Executes this documented step of the workflow.

if (!exists("observed_calendar_aware_tests")) {  # Executes this documented step of the workflow.
  stop("Section 29 requires observed_calendar_aware_tests from Section 27.")  # Executes this documented step of the workflow.
}  # Executes this documented step of the workflow.

if (!exists("conditional_granger_all")) {  # Executes this documented step of the workflow.
  stop("Section 29 requires conditional_granger_all from Section 28.")  # Executes this documented step of the workflow.
}  # Executes this documented step of the workflow.

# ------------------------------------------------------------------------------
# 29.2 STANDARDISE PAIRWISE AND CONDITIONAL TESTS INTO ONE SUPPLEMENTARY TABLE
# ------------------------------------------------------------------------------

pairwise_for_BY <- observed_calendar_aware_tests %>%  # Executes this documented step of the workflow.
  dplyr::filter(is.finite(p_value)) %>%  # Executes this documented step of the workflow.
  dplyr::transmute(  # Executes this documented step of the workflow.
    analysis_type = "pairwise",  # Executes this documented step of the workflow.
    region = region,  # Executes this documented step of the workflow.
    test_class = dplyr::if_else(  # Executes this documented step of the workflow.
      is.na(var_class) | var_class == "",  # Executes this documented step of the workflow.
      "unclassified pairwise",  # Executes this documented step of the workflow.
      var_class  # Executes this documented step of the workflow.
    ),  # Executes this documented step of the workflow.
    Family = Family,  # Executes this documented step of the workflow.
    Lag = Lag,  # Executes this documented step of the workflow.
    direction = "environment_to_insect",  # Executes this documented step of the workflow.
    predictor_dataset = EnvDataset,  # Executes this documented step of the workflow.
    predictor_variable = EnvColumn,  # Executes this documented step of the workflow.
    predictor_analysis_column = environment_analysis_column,  # Executes this documented step of the workflow.
    conditioning_dataset = NA_character_,  # Executes this documented step of the workflow.
    conditioning_variable = NA_character_,  # Executes this documented step of the workflow.
    conditioning_analysis_column = NA_character_,  # Executes this documented step of the workflow.
    insect_analysis_column = insect_analysis_column,  # Executes this documented step of the workflow.
    n_obs = n_obs,  # Executes this documented step of the workflow.
    n_years = n_years,  # Executes this documented step of the workflow.
    F_stat = F_stat,  # Executes this documented step of the workflow.
    delta_R2 = NA_real_,  # Executes this documented step of the workflow.
    partial_R2 = NA_real_,  # Executes this documented step of the workflow.
    raw_p = p_value,  # Executes this documented step of the workflow.
    original_test_status = test_status  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.

conditional_for_BY <- conditional_granger_all %>%  # Executes this documented step of the workflow.
  dplyr::filter(is.finite(p_value)) %>%  # Executes this documented step of the workflow.
  dplyr::transmute(  # Executes this documented step of the workflow.
    analysis_type = "conditional",  # Executes this documented step of the workflow.
    region = region,  # Executes this documented step of the workflow.
    test_class = analysis_class,  # Executes this documented step of the workflow.
    Family = Family,  # Executes this documented step of the workflow.
    Lag = Lag,  # Executes this documented step of the workflow.
    direction = direction,  # Executes this documented step of the workflow.
    predictor_dataset = candidate_dataset,  # Executes this documented step of the workflow.
    predictor_variable = candidate_column,  # Executes this documented step of the workflow.
    predictor_analysis_column = candidate_analysis_column,  # Executes this documented step of the workflow.
    conditioning_dataset = conditioner_dataset,  # Executes this documented step of the workflow.
    conditioning_variable = conditioner_column,  # Executes this documented step of the workflow.
    conditioning_analysis_column = conditioner_analysis_column,  # Executes this documented step of the workflow.
    insect_analysis_column = insect_analysis_column,  # Executes this documented step of the workflow.
    n_obs = n_obs,  # Executes this documented step of the workflow.
    n_years = n_years,  # Executes this documented step of the workflow.
    F_stat = F_stat,  # Executes this documented step of the workflow.
    delta_R2 = delta_R2,  # Executes this documented step of the workflow.
    partial_R2 = partial_R2,  # Executes this documented step of the workflow.
    raw_p = p_value,  # Executes this documented step of the workflow.
    original_test_status = test_status  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.

supplementary_BY_results <- dplyr::bind_rows(  # Executes this documented step of the workflow.
  pairwise_for_BY,  # Executes this documented step of the workflow.
  conditional_for_BY  # Executes this documented step of the workflow.
) %>%  # Executes this documented step of the workflow.
  dplyr::mutate(  # Executes this documented step of the workflow.
    p_BY_all = stats::p.adjust(raw_p, method = "BY"),  # Executes this documented step of the workflow.
    raw_significant = raw_p < multiple_testing_alpha,  # Executes this documented step of the workflow.
    BY_significant = p_BY_all < multiple_testing_alpha,  # Executes this documented step of the workflow.
    BY_interpretation = dplyr::case_when(  # Executes this documented step of the workflow.
      BY_significant ~ "SUPPORTED_AFTER_BY",  # Executes this documented step of the workflow.
      analysis_type == "conditional" ~ "AMBIGUOUS",  # Executes this documented step of the workflow.
      TRUE ~ "NOT_SUPPORTED_AFTER_BY"  # Executes this documented step of the workflow.
    )  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.

# Additional sensitivity columns are useful descriptively, but the primary
# manuscript-level correction remains p_BY_all above, which treats every
# observed Granger test as one global multiple-testing family.
supplementary_BY_results <- supplementary_BY_results %>%  # Executes this documented step of the workflow.
  dplyr::group_by(region) %>%  # Executes this documented step of the workflow.
  dplyr::mutate(  # Executes this documented step of the workflow.
    p_BY_within_region = stats::p.adjust(raw_p, method = "BY")  # Executes this documented step of the workflow.
  ) %>%  # Executes this documented step of the workflow.
  dplyr::ungroup() %>%  # Executes this documented step of the workflow.
  dplyr::group_by(region, test_class) %>%  # Executes this documented step of the workflow.
  dplyr::mutate(  # Executes this documented step of the workflow.
    p_BY_within_region_class = stats::p.adjust(raw_p, method = "BY")  # Executes this documented step of the workflow.
  ) %>%  # Executes this documented step of the workflow.
  dplyr::ungroup()  # Executes this documented step of the workflow.

write.csv(  # Executes this documented step of the workflow.
  supplementary_BY_results,  # Executes this documented step of the workflow.
  file.path(out_multiple_tables, "supplementary_all_granger_BY_results.csv"),  # Executes this documented step of the workflow.
  row.names = FALSE  # Executes this documented step of the workflow.
)  # Executes this documented step of the workflow.

# ------------------------------------------------------------------------------
# 29.3 SUMMARY TABLES: HOW MANY SURVIVE?
# ------------------------------------------------------------------------------

BY_counts_region_class <- supplementary_BY_results %>%  # Executes this documented step of the workflow.
  dplyr::group_by(region, test_class) %>%  # Executes this documented step of the workflow.
  dplyr::summarise(  # Executes this documented step of the workflow.
    n_tests = dplyr::n(),  # Executes this documented step of the workflow.
    n_raw_significant = sum(raw_significant),  # Executes this documented step of the workflow.
    n_BY_significant = sum(BY_significant),  # Executes this documented step of the workflow.
    n_lost_after_BY = sum(raw_significant & !BY_significant),  # Executes this documented step of the workflow.
    pct_raw_significant = 100 * n_raw_significant / n_tests,  # Executes this documented step of the workflow.
    pct_BY_significant = 100 * n_BY_significant / n_tests,  # Executes this documented step of the workflow.
    pct_raw_signals_surviving_BY = dplyr::if_else(  # Executes this documented step of the workflow.
      n_raw_significant > 0,  # Executes this documented step of the workflow.
      100 * n_BY_significant / n_raw_significant,  # Executes this documented step of the workflow.
      NA_real_  # Executes this documented step of the workflow.
    ),  # Executes this documented step of the workflow.
    .groups = "drop"  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.

BY_counts_region <- supplementary_BY_results %>%  # Executes this documented step of the workflow.
  dplyr::group_by(region) %>%  # Executes this documented step of the workflow.
  dplyr::summarise(  # Executes this documented step of the workflow.
    n_tests = dplyr::n(),  # Executes this documented step of the workflow.
    n_raw_significant = sum(raw_significant),  # Executes this documented step of the workflow.
    n_BY_significant = sum(BY_significant),  # Executes this documented step of the workflow.
    n_lost_after_BY = sum(raw_significant & !BY_significant),  # Executes this documented step of the workflow.
    pct_raw_significant = 100 * n_raw_significant / n_tests,  # Executes this documented step of the workflow.
    pct_BY_significant = 100 * n_BY_significant / n_tests,  # Executes this documented step of the workflow.
    .groups = "drop"  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.

BY_counts_class <- supplementary_BY_results %>%  # Executes this documented step of the workflow.
  dplyr::group_by(test_class) %>%  # Executes this documented step of the workflow.
  dplyr::summarise(  # Executes this documented step of the workflow.
    n_tests = dplyr::n(),  # Executes this documented step of the workflow.
    n_raw_significant = sum(raw_significant),  # Executes this documented step of the workflow.
    n_BY_significant = sum(BY_significant),  # Executes this documented step of the workflow.
    n_lost_after_BY = sum(raw_significant & !BY_significant),  # Executes this documented step of the workflow.
    pct_raw_significant = 100 * n_raw_significant / n_tests,  # Executes this documented step of the workflow.
    pct_BY_significant = 100 * n_BY_significant / n_tests,  # Executes this documented step of the workflow.
    .groups = "drop"  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.

BY_counts_analysis_type <- supplementary_BY_results %>%  # Executes this documented step of the workflow.
  dplyr::group_by(analysis_type) %>%  # Executes this documented step of the workflow.
  dplyr::summarise(  # Executes this documented step of the workflow.
    n_tests = dplyr::n(),  # Executes this documented step of the workflow.
    n_raw_significant = sum(raw_significant),  # Executes this documented step of the workflow.
    n_BY_significant = sum(BY_significant),  # Executes this documented step of the workflow.
    n_lost_after_BY = sum(raw_significant & !BY_significant),  # Executes this documented step of the workflow.
    pct_raw_significant = 100 * n_raw_significant / n_tests,  # Executes this documented step of the workflow.
    pct_BY_significant = 100 * n_BY_significant / n_tests,  # Executes this documented step of the workflow.
    .groups = "drop"  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.

BY_counts_lag <- supplementary_BY_results %>%  # Executes this documented step of the workflow.
  dplyr::group_by(region, Lag) %>%  # Executes this documented step of the workflow.
  dplyr::summarise(  # Executes this documented step of the workflow.
    n_tests = dplyr::n(),  # Executes this documented step of the workflow.
    n_raw_significant = sum(raw_significant),  # Executes this documented step of the workflow.
    n_BY_significant = sum(BY_significant),  # Executes this documented step of the workflow.
    pct_raw_significant = 100 * n_raw_significant / n_tests,  # Executes this documented step of the workflow.
    pct_BY_significant = 100 * n_BY_significant / n_tests,  # Executes this documented step of the workflow.
    .groups = "drop"  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.

BY_survivors <- supplementary_BY_results %>%  # Executes this documented step of the workflow.
  dplyr::filter(BY_significant) %>%  # Executes this documented step of the workflow.
  dplyr::arrange(p_BY_all, raw_p)  # Executes this documented step of the workflow.

BY_lost_after_correction <- supplementary_BY_results %>%  # Executes this documented step of the workflow.
  dplyr::filter(raw_significant, !BY_significant) %>%  # Executes this documented step of the workflow.
  dplyr::arrange(raw_p)  # Executes this documented step of the workflow.

pvalue_distribution_summary <- supplementary_BY_results %>%  # Executes this documented step of the workflow.
  dplyr::group_by(region, test_class) %>%  # Executes this documented step of the workflow.
  dplyr::summarise(  # Executes this documented step of the workflow.
    n = dplyr::n(),  # Executes this documented step of the workflow.
    min_raw_p = min(raw_p),  # Executes this documented step of the workflow.
    q05_raw_p = stats::quantile(raw_p, 0.05, names = FALSE),  # Executes this documented step of the workflow.
    median_raw_p = stats::median(raw_p),  # Executes this documented step of the workflow.
    q95_raw_p = stats::quantile(raw_p, 0.95, names = FALSE),  # Executes this documented step of the workflow.
    min_BY_p = min(p_BY_all),  # Executes this documented step of the workflow.
    median_BY_p = stats::median(p_BY_all),  # Executes this documented step of the workflow.
    .groups = "drop"  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.

write.csv(  # Executes this documented step of the workflow.
  BY_counts_region_class,  # Executes this documented step of the workflow.
  file.path(out_multiple_tables, "BY_counts_by_region_and_class.csv"),  # Executes this documented step of the workflow.
  row.names = FALSE  # Executes this documented step of the workflow.
)  # Executes this documented step of the workflow.

write.csv(  # Executes this documented step of the workflow.
  BY_counts_region,  # Executes this documented step of the workflow.
  file.path(out_multiple_tables, "BY_counts_by_region.csv"),  # Executes this documented step of the workflow.
  row.names = FALSE  # Executes this documented step of the workflow.
)  # Executes this documented step of the workflow.

write.csv(  # Executes this documented step of the workflow.
  BY_counts_class,  # Executes this documented step of the workflow.
  file.path(out_multiple_tables, "BY_counts_by_class.csv"),  # Executes this documented step of the workflow.
  row.names = FALSE  # Executes this documented step of the workflow.
)  # Executes this documented step of the workflow.

write.csv(  # Executes this documented step of the workflow.
  BY_counts_analysis_type,  # Executes this documented step of the workflow.
  file.path(out_multiple_tables, "BY_counts_by_analysis_type.csv"),  # Executes this documented step of the workflow.
  row.names = FALSE  # Executes this documented step of the workflow.
)  # Executes this documented step of the workflow.

write.csv(  # Executes this documented step of the workflow.
  BY_counts_lag,  # Executes this documented step of the workflow.
  file.path(out_multiple_tables, "BY_counts_by_region_and_lag.csv"),  # Executes this documented step of the workflow.
  row.names = FALSE  # Executes this documented step of the workflow.
)  # Executes this documented step of the workflow.

write.csv(  # Executes this documented step of the workflow.
  BY_survivors,  # Executes this documented step of the workflow.
  file.path(out_multiple_tables, "BY_significant_results_only.csv"),  # Executes this documented step of the workflow.
  row.names = FALSE  # Executes this documented step of the workflow.
)  # Executes this documented step of the workflow.

write.csv(  # Executes this documented step of the workflow.
  BY_lost_after_correction,  # Executes this documented step of the workflow.
  file.path(out_multiple_tables, "raw_significant_lost_after_BY.csv"),  # Executes this documented step of the workflow.
  row.names = FALSE  # Executes this documented step of the workflow.
)  # Executes this documented step of the workflow.

write.csv(  # Executes this documented step of the workflow.
  pvalue_distribution_summary,  # Executes this documented step of the workflow.
  file.path(out_multiple_tables, "pvalue_distribution_summary_by_region_class.csv"),  # Executes this documented step of the workflow.
  row.names = FALSE  # Executes this documented step of the workflow.
)  # Executes this documented step of the workflow.

# ------------------------------------------------------------------------------
# 29.4 BY-CORRECTED CONDITIONAL TEMPERATURE OUTCOME GRID
# ------------------------------------------------------------------------------

conditional_BY_only <- supplementary_BY_results %>%  # Executes this documented step of the workflow.
  dplyr::filter(  # Executes this documented step of the workflow.
    analysis_type == "conditional",  # Executes this documented step of the workflow.
    direction %in% c("soil_given_air", "air_given_soil")  # Executes this documented step of the workflow.
  ) %>%  # Executes this documented step of the workflow.
  dplyr::mutate(  # Executes this documented step of the workflow.
    soil_column = dplyr::if_else(  # Executes this documented step of the workflow.
      direction == "soil_given_air",  # Executes this documented step of the workflow.
      predictor_variable,  # Executes this documented step of the workflow.
      conditioning_variable  # Executes this documented step of the workflow.
    ),  # Executes this documented step of the workflow.
    soil_depth_label = unname(conditional_soil_labels[soil_column])  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.

soil_BY <- conditional_BY_only %>%  # Executes this documented step of the workflow.
  dplyr::filter(direction == "soil_given_air") %>%  # Executes this documented step of the workflow.
  dplyr::select(  # Executes this documented step of the workflow.
    region, Family, soil_column, soil_depth_label, Lag,  # Executes this documented step of the workflow.
    soil_raw_p = raw_p,  # Executes this documented step of the workflow.
    soil_BY_p = p_BY_all,  # Executes this documented step of the workflow.
    soil_BY_supported = BY_significant  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.

air_BY <- conditional_BY_only %>%  # Executes this documented step of the workflow.
  dplyr::filter(direction == "air_given_soil") %>%  # Executes this documented step of the workflow.
  dplyr::select(  # Executes this documented step of the workflow.
    region, Family, soil_column, soil_depth_label, Lag,  # Executes this documented step of the workflow.
    air_raw_p = raw_p,  # Executes this documented step of the workflow.
    air_BY_p = p_BY_all,  # Executes this documented step of the workflow.
    air_BY_supported = BY_significant  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.

temperature_outcome_grid_BY <- dplyr::full_join(  # Executes this documented step of the workflow.
  soil_BY,  # Executes this documented step of the workflow.
  air_BY,  # Executes this documented step of the workflow.
  by = c("region", "Family", "soil_column", "soil_depth_label", "Lag")  # Executes this documented step of the workflow.
) %>%  # Executes this documented step of the workflow.
  dplyr::mutate(  # Executes this documented step of the workflow.
    both_available = !is.na(soil_BY_supported) & !is.na(air_BY_supported),  # Executes this documented step of the workflow.
    outcome4_BY = dplyr::case_when(  # Executes this documented step of the workflow.
      !both_available ~ "NOT_TESTABLE",  # Executes this documented step of the workflow.
      soil_BY_supported & air_BY_supported ~ "BOTH_SUPPORTED",  # Executes this documented step of the workflow.
      soil_BY_supported & !air_BY_supported ~ "SOIL_ONLY_SUPPORTED",  # Executes this documented step of the workflow.
      !soil_BY_supported & air_BY_supported ~ "AIR_ONLY_SUPPORTED",  # Executes this documented step of the workflow.
      TRUE ~ "NEITHER_RESOLVED"  # Executes this documented step of the workflow.
    ),  # Executes this documented step of the workflow.
    interpretation_note = dplyr::case_when(  # Executes this documented step of the workflow.
      outcome4_BY == "BOTH_SUPPORTED" ~ "Both directions survive global BY correction.",  # Executes this documented step of the workflow.
      outcome4_BY == "SOIL_ONLY_SUPPORTED" ~ "Soil-given-air survives BY; reverse direction remains ambiguous.",  # Executes this documented step of the workflow.
      outcome4_BY == "AIR_ONLY_SUPPORTED" ~ "Air-given-soil survives BY; soil-given-air remains ambiguous.",  # Executes this documented step of the workflow.
      outcome4_BY == "NEITHER_RESOLVED" ~ "Neither direction survives BY; do not interpret as evidence of no effect.",  # Executes this documented step of the workflow.
      TRUE ~ "At least one direction was unavailable; interpretation is ambiguous."  # Executes this documented step of the workflow.
    )  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.

write.csv(  # Executes this documented step of the workflow.
  temperature_outcome_grid_BY,  # Executes this documented step of the workflow.
  file.path(out_multiple_tables, "conditional_temperature_outcome_grid_after_BY.csv"),  # Executes this documented step of the workflow.
  row.names = FALSE  # Executes this documented step of the workflow.
)  # Executes this documented step of the workflow.

# ------------------------------------------------------------------------------
# 29.5 PLOT: RAW VERSUS BY-SURVIVING COUNTS BY REGION AND CLASS
# ------------------------------------------------------------------------------

BY_counts_long <- BY_counts_region_class %>%  # Executes this documented step of the workflow.
  dplyr::select(  # Executes this documented step of the workflow.
    region,  # Executes this documented step of the workflow.
    test_class,  # Executes this documented step of the workflow.
    n_raw_significant,  # Executes this documented step of the workflow.
    n_BY_significant  # Executes this documented step of the workflow.
  ) %>%  # Executes this documented step of the workflow.
  tidyr::pivot_longer(  # Executes this documented step of the workflow.
    cols = c(n_raw_significant, n_BY_significant),  # Executes this documented step of the workflow.
    names_to = "criterion",  # Executes this documented step of the workflow.
    values_to = "n_significant"  # Executes this documented step of the workflow.
  ) %>%  # Executes this documented step of the workflow.
  dplyr::mutate(  # Executes this documented step of the workflow.
    criterion = dplyr::recode(  # Executes this documented step of the workflow.
      criterion,  # Executes this documented step of the workflow.
      n_raw_significant = "Raw p < 0.05",  # Executes this documented step of the workflow.
      n_BY_significant = "BY-adjusted p < 0.05"  # Executes this documented step of the workflow.
    )  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.

if (nrow(BY_counts_long) > 0L) {  # Executes this documented step of the workflow.
  p_BY_counts <- ggplot2::ggplot(  # Executes this documented step of the workflow.
    BY_counts_long,  # Executes this documented step of the workflow.
    ggplot2::aes(x = test_class, y = n_significant, fill = criterion)  # Executes this documented step of the workflow.
  ) +  # Executes this documented step of the workflow.
    ggplot2::geom_col(position = "dodge") +  # Executes this documented step of the workflow.
    ggplot2::facet_wrap(~ region, scales = "free_y") +  # Executes this documented step of the workflow.
    ggplot2::labs(  # Executes this documented step of the workflow.
      title = "Granger results surviving BY correction by region and class",  # Executes this documented step of the workflow.
      x = "Test class",  # Executes this documented step of the workflow.
      y = "Number of significant tests",  # Executes this documented step of the workflow.
      fill = "Criterion"  # Executes this documented step of the workflow.
    ) +  # Executes this documented step of the workflow.
    ggplot2::theme_minimal(base_size = 10) +  # Executes this documented step of the workflow.
    ggplot2::theme(  # Executes this documented step of the workflow.
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),  # Executes this documented step of the workflow.
      panel.grid.minor = ggplot2::element_blank()  # Executes this documented step of the workflow.
    )  # Executes this documented step of the workflow.

  ggplot2::ggsave(  # Executes this documented step of the workflow.
    file.path(out_multiple_plots, "BY_survival_counts_by_region_class.png"),  # Executes this documented step of the workflow.
    p_BY_counts,  # Executes this documented step of the workflow.
    width = 13,  # Executes this documented step of the workflow.
    height = 7,  # Executes this documented step of the workflow.
    dpi = 300,  # Executes this documented step of the workflow.
    bg = "white"  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.
}  # Executes this documented step of the workflow.

# Percent surviving after BY.
if (nrow(BY_counts_region_class) > 0L) {  # Executes this documented step of the workflow.
  p_BY_pct <- ggplot2::ggplot(  # Executes this documented step of the workflow.
    BY_counts_region_class,  # Executes this documented step of the workflow.
    ggplot2::aes(x = test_class, y = pct_BY_significant)  # Executes this documented step of the workflow.
  ) +  # Executes this documented step of the workflow.
    ggplot2::geom_col() +  # Executes this documented step of the workflow.
    ggplot2::facet_wrap(~ region, scales = "free_y") +  # Executes this documented step of the workflow.
    ggplot2::labs(  # Executes this documented step of the workflow.
      title = "Percentage of all tests significant after global BY correction",  # Executes this documented step of the workflow.
      x = "Test class",  # Executes this documented step of the workflow.
      y = "% BY-significant"  # Executes this documented step of the workflow.
    ) +  # Executes this documented step of the workflow.
    ggplot2::theme_minimal(base_size = 10) +  # Executes this documented step of the workflow.
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))  # Executes this documented step of the workflow.

  ggplot2::ggsave(  # Executes this documented step of the workflow.
    file.path(out_multiple_plots, "BY_survival_percentage_by_region_class.png"),  # Executes this documented step of the workflow.
    p_BY_pct,  # Executes this documented step of the workflow.
    width = 13,  # Executes this documented step of the workflow.
    height = 7,  # Executes this documented step of the workflow.
    dpi = 300,  # Executes this documented step of the workflow.
    bg = "white"  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.
}  # Executes this documented step of the workflow.

# ------------------------------------------------------------------------------
# 29.6 P-VALUE HISTOGRAMS
# ------------------------------------------------------------------------------

if (nrow(supplementary_BY_results) > 0L) {  # Executes this documented step of the workflow.

  p_hist_overall <- ggplot2::ggplot(  # Executes this documented step of the workflow.
    supplementary_BY_results,  # Executes this documented step of the workflow.
    ggplot2::aes(x = raw_p)  # Executes this documented step of the workflow.
  ) +  # Executes this documented step of the workflow.
    ggplot2::geom_histogram(  # Executes this documented step of the workflow.
      binwidth = 0.025,  # Executes this documented step of the workflow.
      boundary = 0,  # Executes this documented step of the workflow.
      closed = "left"  # Executes this documented step of the workflow.
    ) +  # Executes this documented step of the workflow.
    ggplot2::geom_vline(xintercept = multiple_testing_alpha, linetype = 2) +  # Executes this documented step of the workflow.
    ggplot2::labs(  # Executes this documented step of the workflow.
      title = "Raw p-value distribution across all observed Granger tests",  # Executes this documented step of the workflow.
      subtitle = "A strong accumulation near zero is consistent with signal beyond a uniform-null pattern",  # Executes this documented step of the workflow.
      x = "Raw p-value",  # Executes this documented step of the workflow.
      y = "Count"  # Executes this documented step of the workflow.
    ) +  # Executes this documented step of the workflow.
    ggplot2::theme_minimal(base_size = 11)  # Executes this documented step of the workflow.

  ggplot2::ggsave(  # Executes this documented step of the workflow.
    file.path(out_multiple_plots, "all_raw_pvalue_histogram.png"),  # Executes this documented step of the workflow.
    p_hist_overall,  # Executes this documented step of the workflow.
    width = 9,  # Executes this documented step of the workflow.
    height = 6,  # Executes this documented step of the workflow.
    dpi = 300,  # Executes this documented step of the workflow.
    bg = "white"  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.

  p_hist_faceted <- ggplot2::ggplot(  # Executes this documented step of the workflow.
    supplementary_BY_results,  # Executes this documented step of the workflow.
    ggplot2::aes(x = raw_p)  # Executes this documented step of the workflow.
  ) +  # Executes this documented step of the workflow.
    ggplot2::geom_histogram(  # Executes this documented step of the workflow.
      binwidth = 0.05,  # Executes this documented step of the workflow.
      boundary = 0,  # Executes this documented step of the workflow.
      closed = "left"  # Executes this documented step of the workflow.
    ) +  # Executes this documented step of the workflow.
    ggplot2::geom_vline(xintercept = multiple_testing_alpha, linetype = 2) +  # Executes this documented step of the workflow.
    ggplot2::facet_grid(region ~ test_class, scales = "free_y") +  # Executes this documented step of the workflow.
    ggplot2::labs(  # Executes this documented step of the workflow.
      title = "Raw Granger p-value distributions by region and test class",  # Executes this documented step of the workflow.
      x = "Raw p-value",  # Executes this documented step of the workflow.
      y = "Count"  # Executes this documented step of the workflow.
    ) +  # Executes this documented step of the workflow.
    ggplot2::theme_minimal(base_size = 9) +  # Executes this documented step of the workflow.
    ggplot2::theme(  # Executes this documented step of the workflow.
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),  # Executes this documented step of the workflow.
      strip.text.x = ggplot2::element_text(size = 7)  # Executes this documented step of the workflow.
    )  # Executes this documented step of the workflow.

  ggplot2::ggsave(  # Executes this documented step of the workflow.
    file.path(out_multiple_plots, "raw_pvalue_histograms_by_region_class.png"),  # Executes this documented step of the workflow.
    p_hist_faceted,  # Executes this documented step of the workflow.
    width = 18,  # Executes this documented step of the workflow.
    height = 10,  # Executes this documented step of the workflow.
    dpi = 300,  # Executes this documented step of the workflow.
    bg = "white",  # Executes this documented step of the workflow.
    limitsize = FALSE  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.

  p_BY_hist <- ggplot2::ggplot(  # Executes this documented step of the workflow.
    supplementary_BY_results,  # Executes this documented step of the workflow.
    ggplot2::aes(x = p_BY_all)  # Executes this documented step of the workflow.
  ) +  # Executes this documented step of the workflow.
    ggplot2::geom_histogram(  # Executes this documented step of the workflow.
      binwidth = 0.05,  # Executes this documented step of the workflow.
      boundary = 0,  # Executes this documented step of the workflow.
      closed = "left"  # Executes this documented step of the workflow.
    ) +  # Executes this documented step of the workflow.
    ggplot2::geom_vline(xintercept = multiple_testing_alpha, linetype = 2) +  # Executes this documented step of the workflow.
    ggplot2::facet_wrap(~ region) +  # Executes this documented step of the workflow.
    ggplot2::labs(  # Executes this documented step of the workflow.
      title = "BY-adjusted p-value distributions",  # Executes this documented step of the workflow.
      x = "Global BY-adjusted p-value",  # Executes this documented step of the workflow.
      y = "Count"  # Executes this documented step of the workflow.
    ) +  # Executes this documented step of the workflow.
    ggplot2::theme_minimal(base_size = 10)  # Executes this documented step of the workflow.

  ggplot2::ggsave(  # Executes this documented step of the workflow.
    file.path(out_multiple_plots, "BY_adjusted_pvalue_histograms.png"),  # Executes this documented step of the workflow.
    p_BY_hist,  # Executes this documented step of the workflow.
    width = 10,  # Executes this documented step of the workflow.
    height = 6,  # Executes this documented step of the workflow.
    dpi = 300,  # Executes this documented step of the workflow.
    bg = "white"  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.
}  # Executes this documented step of the workflow.

# ------------------------------------------------------------------------------
# 29.7 RAW P VERSUS BY P, QQ, ECDF AND SURVIVAL HEATMAP
# ------------------------------------------------------------------------------

if (nrow(supplementary_BY_results) > 0L) {  # Executes this documented step of the workflow.

  p_raw_vs_BY <- ggplot2::ggplot(  # Executes this documented step of the workflow.
    supplementary_BY_results,  # Executes this documented step of the workflow.
    ggplot2::aes(x = raw_p, y = p_BY_all, shape = analysis_type)  # Executes this documented step of the workflow.
  ) +  # Executes this documented step of the workflow.
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2) +  # Executes this documented step of the workflow.
    ggplot2::geom_point(alpha = 0.6) +  # Executes this documented step of the workflow.
    ggplot2::geom_vline(xintercept = multiple_testing_alpha, linetype = 3) +  # Executes this documented step of the workflow.
    ggplot2::geom_hline(yintercept = multiple_testing_alpha, linetype = 3) +  # Executes this documented step of the workflow.
    ggplot2::facet_wrap(~ region) +  # Executes this documented step of the workflow.
    ggplot2::labs(  # Executes this documented step of the workflow.
      title = "Raw versus BY-adjusted Granger p-values",  # Executes this documented step of the workflow.
      x = "Raw p-value",  # Executes this documented step of the workflow.
      y = "Global BY-adjusted p-value",  # Executes this documented step of the workflow.
      shape = "Analysis"  # Executes this documented step of the workflow.
    ) +  # Executes this documented step of the workflow.
    ggplot2::theme_minimal(base_size = 10)  # Executes this documented step of the workflow.

  ggplot2::ggsave(  # Executes this documented step of the workflow.
    file.path(out_multiple_plots, "raw_p_vs_BY_p.png"),  # Executes this documented step of the workflow.
    p_raw_vs_BY,  # Executes this documented step of the workflow.
    width = 10,  # Executes this documented step of the workflow.
    height = 7,  # Executes this documented step of the workflow.
    dpi = 300,  # Executes this documented step of the workflow.
    bg = "white"  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.

  p_neglog <- ggplot2::ggplot(  # Executes this documented step of the workflow.
    supplementary_BY_results,  # Executes this documented step of the workflow.
    ggplot2::aes(  # Executes this documented step of the workflow.
      x = -log10(pmax(raw_p, .Machine$double.xmin)),  # Executes this documented step of the workflow.
      y = -log10(pmax(p_BY_all, .Machine$double.xmin)),  # Executes this documented step of the workflow.
      shape = analysis_type  # Executes this documented step of the workflow.
    )  # Executes this documented step of the workflow.
  ) +  # Executes this documented step of the workflow.
    ggplot2::geom_point(alpha = 0.6) +  # Executes this documented step of the workflow.
    ggplot2::geom_hline(yintercept = -log10(multiple_testing_alpha), linetype = 2) +  # Executes this documented step of the workflow.
    ggplot2::facet_wrap(~ region) +  # Executes this documented step of the workflow.
    ggplot2::labs(  # Executes this documented step of the workflow.
      title = "Strength of evidence before and after BY correction",  # Executes this documented step of the workflow.
      x = "-log10(raw p)",  # Executes this documented step of the workflow.
      y = "-log10(BY p)",  # Executes this documented step of the workflow.
      shape = "Analysis"  # Executes this documented step of the workflow.
    ) +  # Executes this documented step of the workflow.
    ggplot2::theme_minimal(base_size = 10)  # Executes this documented step of the workflow.

  ggplot2::ggsave(  # Executes this documented step of the workflow.
    file.path(out_multiple_plots, "neglog10_raw_vs_BY.png"),  # Executes this documented step of the workflow.
    p_neglog,  # Executes this documented step of the workflow.
    width = 10,  # Executes this documented step of the workflow.
    height = 7,  # Executes this documented step of the workflow.
    dpi = 300,  # Executes this documented step of the workflow.
    bg = "white"  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.

  qq_data <- supplementary_BY_results %>%  # Executes this documented step of the workflow.
    dplyr::group_by(region) %>%  # Executes this documented step of the workflow.
    dplyr::arrange(raw_p, .by_group = TRUE) %>%  # Executes this documented step of the workflow.
    dplyr::mutate(  # Executes this documented step of the workflow.
      expected_uniform = stats::ppoints(dplyr::n()),  # Executes this documented step of the workflow.
      observed_p = raw_p  # Executes this documented step of the workflow.
    ) %>%  # Executes this documented step of the workflow.
    dplyr::ungroup()  # Executes this documented step of the workflow.

  p_qq <- ggplot2::ggplot(  # Executes this documented step of the workflow.
    qq_data,  # Executes this documented step of the workflow.
    ggplot2::aes(x = expected_uniform, y = observed_p)  # Executes this documented step of the workflow.
  ) +  # Executes this documented step of the workflow.
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2) +  # Executes this documented step of the workflow.
    ggplot2::geom_point(alpha = 0.6) +  # Executes this documented step of the workflow.
    ggplot2::facet_wrap(~ region) +  # Executes this documented step of the workflow.
    ggplot2::coord_equal() +  # Executes this documented step of the workflow.
    ggplot2::labs(  # Executes this documented step of the workflow.
      title = "Observed Granger p-value QQ plots",  # Executes this documented step of the workflow.
      subtitle = "Departure below the diagonal indicates an excess of small p-values",  # Executes this documented step of the workflow.
      x = "Expected Uniform(0,1) quantile",  # Executes this documented step of the workflow.
      y = "Observed raw p-value"  # Executes this documented step of the workflow.
    ) +  # Executes this documented step of the workflow.
    ggplot2::theme_minimal(base_size = 10)  # Executes this documented step of the workflow.

  ggplot2::ggsave(  # Executes this documented step of the workflow.
    file.path(out_multiple_plots, "raw_pvalue_QQ.png"),  # Executes this documented step of the workflow.
    p_qq,  # Executes this documented step of the workflow.
    width = 10,  # Executes this documented step of the workflow.
    height = 6,  # Executes this documented step of the workflow.
    dpi = 300,  # Executes this documented step of the workflow.
    bg = "white"  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.

  p_ecdf <- ggplot2::ggplot(  # Executes this documented step of the workflow.
    supplementary_BY_results,  # Executes this documented step of the workflow.
    ggplot2::aes(x = raw_p, linetype = analysis_type)  # Executes this documented step of the workflow.
  ) +  # Executes this documented step of the workflow.
    ggplot2::stat_ecdf(geom = "step") +  # Executes this documented step of the workflow.
    ggplot2::geom_vline(xintercept = multiple_testing_alpha, linetype = 3) +  # Executes this documented step of the workflow.
    ggplot2::facet_grid(region ~ test_class) +  # Executes this documented step of the workflow.
    ggplot2::labs(  # Executes this documented step of the workflow.
      title = "Empirical cumulative distribution of raw Granger p-values",  # Executes this documented step of the workflow.
      x = "Raw p-value",  # Executes this documented step of the workflow.
      y = "Cumulative proportion",  # Executes this documented step of the workflow.
      linetype = "Analysis"  # Executes this documented step of the workflow.
    ) +  # Executes this documented step of the workflow.
    ggplot2::theme_minimal(base_size = 8)  # Executes this documented step of the workflow.

  ggplot2::ggsave(  # Executes this documented step of the workflow.
    file.path(out_multiple_plots, "raw_pvalue_ECDF_by_region_class.png"),  # Executes this documented step of the workflow.
    p_ecdf,  # Executes this documented step of the workflow.
    width = 18,  # Executes this documented step of the workflow.
    height = 10,  # Executes this documented step of the workflow.
    dpi = 300,  # Executes this documented step of the workflow.
    bg = "white",  # Executes this documented step of the workflow.
    limitsize = FALSE  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.
}  # Executes this documented step of the workflow.

if (nrow(BY_counts_region_class) > 0L) {  # Executes this documented step of the workflow.
  p_survival_heatmap <- ggplot2::ggplot(  # Executes this documented step of the workflow.
    BY_counts_region_class,  # Executes this documented step of the workflow.
    ggplot2::aes(x = region, y = test_class, fill = pct_BY_significant)  # Executes this documented step of the workflow.
  ) +  # Executes this documented step of the workflow.
    ggplot2::geom_tile(colour = "white") +  # Executes this documented step of the workflow.
    ggplot2::geom_text(  # Executes this documented step of the workflow.
      ggplot2::aes(label = sprintf("%.1f%%", pct_BY_significant)),  # Executes this documented step of the workflow.
      size = 3  # Executes this documented step of the workflow.
    ) +  # Executes this documented step of the workflow.
    ggplot2::labs(  # Executes this documented step of the workflow.
      title = "Percentage of Granger tests surviving global BY correction",  # Executes this documented step of the workflow.
      x = "Region",  # Executes this documented step of the workflow.
      y = "Test class",  # Executes this documented step of the workflow.
      fill = "% surviving"  # Executes this documented step of the workflow.
    ) +  # Executes this documented step of the workflow.
    ggplot2::theme_minimal(base_size = 10) +  # Executes this documented step of the workflow.
    ggplot2::theme(panel.grid = ggplot2::element_blank())  # Executes this documented step of the workflow.

  ggplot2::ggsave(  # Executes this documented step of the workflow.
    file.path(out_multiple_plots, "BY_survival_heatmap_region_class.png"),  # Executes this documented step of the workflow.
    p_survival_heatmap,  # Executes this documented step of the workflow.
    width = 9,  # Executes this documented step of the workflow.
    height = 8,  # Executes this documented step of the workflow.
    dpi = 300,  # Executes this documented step of the workflow.
    bg = "white"  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.
}  # Executes this documented step of the workflow.

if (nrow(temperature_outcome_grid_BY) > 0L) {  # Executes this documented step of the workflow.
  p_outcome_BY <- ggplot2::ggplot(  # Executes this documented step of the workflow.
    temperature_outcome_grid_BY,  # Executes this documented step of the workflow.
    ggplot2::aes(x = Family, y = soil_depth_label, fill = outcome4_BY)  # Executes this documented step of the workflow.
  ) +  # Executes this documented step of the workflow.
    ggplot2::geom_tile(colour = "white") +  # Executes this documented step of the workflow.
    ggplot2::facet_grid(region ~ Lag) +  # Executes this documented step of the workflow.
    ggplot2::labs(  # Executes this documented step of the workflow.
      title = "Conditional temperature outcome grid after global BY correction",  # Executes this documented step of the workflow.
      subtitle = "A non-surviving direction remains AMBIGUOUS; it is not evidence of no effect",  # Executes this documented step of the workflow.
      x = "Insect family",  # Executes this documented step of the workflow.
      y = "Soil-temperature predictor",  # Executes this documented step of the workflow.
      fill = "BY outcome"  # Executes this documented step of the workflow.
    ) +  # Executes this documented step of the workflow.
    ggplot2::theme_minimal(base_size = 10) +  # Executes this documented step of the workflow.
    ggplot2::theme(  # Executes this documented step of the workflow.
      panel.grid = ggplot2::element_blank(),  # Executes this documented step of the workflow.
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)  # Executes this documented step of the workflow.
    )  # Executes this documented step of the workflow.

  ggplot2::ggsave(  # Executes this documented step of the workflow.
    file.path(out_multiple_plots, "conditional_temperature_outcome_grid_after_BY.png"),  # Executes this documented step of the workflow.
    p_outcome_BY,  # Executes this documented step of the workflow.
    width = 14,  # Executes this documented step of the workflow.
    height = 9,  # Executes this documented step of the workflow.
    dpi = 300,  # Executes this documented step of the workflow.
    bg = "white"  # Executes this documented step of the workflow.
  )  # Executes this documented step of the workflow.
}  # Executes this documented step of the workflow.

# ------------------------------------------------------------------------------
# 29.8 SAVE OBJECT AND MANIFEST
# ------------------------------------------------------------------------------

saveRDS(  # Executes this documented step of the workflow.
  list(  # Executes this documented step of the workflow.
    supplementary_results = supplementary_BY_results,  # Executes this documented step of the workflow.
    counts_region_class = BY_counts_region_class,  # Executes this documented step of the workflow.
    counts_region = BY_counts_region,  # Executes this documented step of the workflow.
    counts_class = BY_counts_class,  # Executes this documented step of the workflow.
    counts_analysis_type = BY_counts_analysis_type,  # Executes this documented step of the workflow.
    counts_lag = BY_counts_lag,  # Executes this documented step of the workflow.
    survivors = BY_survivors,  # Executes this documented step of the workflow.
    lost_after_correction = BY_lost_after_correction,  # Executes this documented step of the workflow.
    pvalue_distribution_summary = pvalue_distribution_summary,  # Executes this documented step of the workflow.
    conditional_temperature_outcome_grid_BY = temperature_outcome_grid_BY  # Executes this documented step of the workflow.
  ),  # Executes this documented step of the workflow.
  file.path(out_multiple_objects, "all_granger_BY_correction_results.rds")  # Executes this documented step of the workflow.
)  # Executes this documented step of the workflow.

multiple_output_manifest <- c(  # Executes this documented step of the workflow.
  "BY MULTIPLE-TESTING OUTPUTS",  # Executes this documented step of the workflow.
  "===========================",  # Executes this documented step of the workflow.
  "",  # Executes this documented step of the workflow.
  "SUPPLEMENTARY/MAIN TABLES",  # Executes this documented step of the workflow.
  "tables/supplementary_all_granger_BY_results.csv",  # Executes this documented step of the workflow.
  "tables/BY_counts_by_region_and_class.csv",  # Executes this documented step of the workflow.
  "tables/BY_counts_by_region.csv",  # Executes this documented step of the workflow.
  "tables/BY_counts_by_class.csv",  # Executes this documented step of the workflow.
  "tables/BY_counts_by_analysis_type.csv",  # Executes this documented step of the workflow.
  "tables/BY_counts_by_region_and_lag.csv",  # Executes this documented step of the workflow.
  "tables/BY_significant_results_only.csv",  # Executes this documented step of the workflow.
  "tables/raw_significant_lost_after_BY.csv",  # Executes this documented step of the workflow.
  "tables/pvalue_distribution_summary_by_region_class.csv",  # Executes this documented step of the workflow.
  "tables/conditional_temperature_outcome_grid_after_BY.csv",  # Executes this documented step of the workflow.
  "",  # Executes this documented step of the workflow.
  "PLOTS",  # Executes this documented step of the workflow.
  "plots/BY_survival_counts_by_region_class.png",  # Executes this documented step of the workflow.
  "plots/BY_survival_percentage_by_region_class.png",  # Executes this documented step of the workflow.
  "plots/all_raw_pvalue_histogram.png",  # Executes this documented step of the workflow.
  "plots/raw_pvalue_histograms_by_region_class.png",  # Executes this documented step of the workflow.
  "plots/BY_adjusted_pvalue_histograms.png",  # Executes this documented step of the workflow.
  "plots/raw_p_vs_BY_p.png",  # Executes this documented step of the workflow.
  "plots/neglog10_raw_vs_BY.png",  # Executes this documented step of the workflow.
  "plots/raw_pvalue_QQ.png",  # Executes this documented step of the workflow.
  "plots/raw_pvalue_ECDF_by_region_class.png",  # Executes this documented step of the workflow.
  "plots/BY_survival_heatmap_region_class.png",  # Executes this documented step of the workflow.
  "plots/conditional_temperature_outcome_grid_after_BY.png",  # Executes this documented step of the workflow.
  "",  # Executes this documented step of the workflow.
  "R OBJECT",  # Executes this documented step of the workflow.
  "objects/all_granger_BY_correction_results.rds"  # Executes this documented step of the workflow.
)  # Executes this documented step of the workflow.

writeLines(  # Executes this documented step of the workflow.
  multiple_output_manifest,  # Executes this documented step of the workflow.
  file.path(out_multiple, "README_BY_multiple_testing_outputs.txt")  # Executes this documented step of the workflow.
)  # Executes this documented step of the workflow.

cat(  # Executes this documented step of the workflow.
  "\nSECTION 29 BY MULTIPLE-TESTING ANALYSIS DONE.\n",  # Executes this documented step of the workflow.
  "Total observed Granger p-values corrected together: ",  # Executes this documented step of the workflow.
  nrow(supplementary_BY_results), "\n",  # Executes this documented step of the workflow.
  "Outputs: ", normalizePath(out_multiple, mustWork = FALSE), "\n",  # Executes this documented step of the workflow.
  sep = ""  # Executes this documented step of the workflow.
)  # Executes this documented step of the workflow.

# ==============================================================================
# END OF SECTION 29
# ==============================================================================
