################################################################################
# FINAL MONTHLY DATA-BUILD SCRIPT FOR DS22007
# PURPOSE: build one and only one row per biological or environmental time point.
# IMPORTANT: this script does not append transformed rows underneath original rows.
# IMPORTANT: all downstream transformations are handled in the manuscript-analysis script.
################################################################################

required_packages <- c("dplyr", "readr", "stringr", "lubridate", "purrr", "tibble") # Lists the packages required by this data-build script.
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)] # Identifies required packages that are not installed.
if (length(missing_packages) > 0L) { # Stops early when one or more required packages are missing.
  stop("Install the missing packages before running this script: ", paste(missing_packages, collapse = ", ")) # Reports the exact missing package names.
} # Closes the missing-package check.
suppressPackageStartupMessages(library(dplyr)) # Loads dplyr for data manipulation.
suppressPackageStartupMessages(library(readr)) # Loads readr for robust delimited-file input.
suppressPackageStartupMessages(library(stringr)) # Loads stringr for string parsing.
suppressPackageStartupMessages(library(lubridate)) # Loads lubridate for calendar parsing.
suppressPackageStartupMessages(library(purrr)) # Loads purrr for list iteration.
suppressPackageStartupMessages(library(tibble)) # Loads tibble for modern data frames.

input_paths <- list( # Stores every raw-data path in one explicit configuration object.
  smi_total = "./data/smi/smi_means_total_soil.csv", # Points to total-soil SMI data.
  smi_upsoil = "./data/smi/smi_means_upsoil.csv", # Points to upper-soil SMI data.
  ndvi_alb = "./data/NDVI/Landsat7_Monthly_NDVI_ALB_merged.csv", # Points to ALB NDVI data.
  ndvi_hai = "./data/NDVI/Landsat7_Monthly_NDVI_HAI_merged.csv", # Points to HAI NDVI data.
  ndvi_sch = "./data/NDVI/Landsat7_Monthly_NDVI_SCH_merged.csv", # Points to SCH NDVI data.
  ndmi_alb = "./data/NDMI/Landsat7_Monthly_NDMI_ALB_merged.csv", # Points to ALB NDMI data.
  ndmi_hai = "./data/NDMI/Landsat7_Monthly_NDMI_HAI_merged.csv", # Points to HAI NDMI data.
  ndmi_sch = "./data/NDMI/Landsat7_Monthly_NDMI_SCH_merged.csv", # Points to SCH NDMI data.
  nirv_alb = "./data/NIRv/Landsat7_Monthly_NIRv_ALB_merged.csv", # Points to ALB NIRv data.
  nirv_hai = "./data/NIRv/Landsat7_Monthly_NIRv_HAI_merged.csv", # Points to HAI NIRv data.
  nirv_sch = "./data/NIRv/Landsat7_Monthly_NIRv_SCH_merged.csv", # Points to SCH NIRv data.
  weather = "./data/bexis_weather_plots.csv", # Points to plot-level weather data.
  insects = "./data/insects/ds22007.csv" # Points to the raw ds22007 insect data.
) # Closes the input-path configuration.
missing_input_files <- unlist(input_paths)[!file.exists(unlist(input_paths))] # Identifies raw input files that cannot be found.
if (length(missing_input_files) > 0L) { # Stops before analysis when any raw input file is missing.
  stop("Missing required input file(s): ", paste(missing_input_files, collapse = ", ")) # Reports every missing raw-data file.
} # Closes the raw-file existence check.

out_root <- "./final_monthly_inputs_20261005" # Defines the output directory for the corrected monthly inputs.
out_tables <- file.path(out_root, "tables") # Defines the directory for corrected monthly CSV files.
out_objects <- file.path(out_root, "objects") # Defines the directory for reusable R objects.
out_diagnostics <- file.path(out_root, "diagnostics") # Defines the directory for data-structure diagnostics.
dir.create(out_tables, recursive = TRUE, showWarnings = FALSE) # Creates the table-output directory.
dir.create(out_objects, recursive = TRUE, showWarnings = FALSE) # Creates the object-output directory.
dir.create(out_diagnostics, recursive = TRUE, showWarnings = FALSE) # Creates the diagnostic-output directory.

assert_has_columns <- function(df, required, object_name) { # Defines a helper that fails immediately if expected columns are missing.
  missing <- setdiff(required, names(df)) # Finds required columns absent from the supplied data frame.
  if (length(missing) > 0L) { # Checks whether any required columns are absent.
    stop(object_name, " is missing required column(s): ", paste(missing, collapse = ", ")) # Reports the exact structural problem.
  } # Closes the missing-column check.
  invisible(TRUE) # Returns invisibly after a successful structural check.
} # Closes the required-column helper.

assert_unique_key <- function(df, key, object_name) { # Defines a helper that requires exactly one row per stated key.
  assert_has_columns(df, key, object_name) # Confirms that all key columns exist before counting duplicates.
  duplicate_rows <- df %>% count(across(all_of(key)), name = "n_rows") %>% filter(n_rows > 1L) # Finds key combinations represented by more than one row.
  if (nrow(duplicate_rows) > 0L) { # Stops when duplicate time points remain after aggregation.
    write.csv(duplicate_rows, file.path(out_diagnostics, paste0("DUPLICATES_", object_name, ".csv")), row.names = FALSE) # Saves the offending keys for debugging.
    stop(object_name, " contains duplicated key rows after canonical aggregation; see diagnostics output.") # Prevents downstream analysis of duplicated time points.
  } # Closes the duplicate-key check.
  invisible(TRUE) # Returns invisibly after uniqueness is confirmed.
} # Closes the unique-key helper.

finite_mean <- function(x) { # Defines a mean that returns NA instead of NaN when every value is missing.
  x <- suppressWarnings(as.numeric(x)) # Coerces the input to numeric values.
  if (!any(is.finite(x))) return(NA_real_) # Returns a true missing value when no finite observation exists.
  mean(x[is.finite(x)]) # Calculates the arithmetic mean over finite observations only.
} # Closes the finite-mean helper.

aggregate_numeric_monthly <- function(df, group_cols, exclude_cols = character()) { # Defines a generic monthly aggregation for numeric environmental variables.
  assert_has_columns(df, group_cols, "aggregate_numeric_monthly input") # Confirms the requested grouping columns exist.
  numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))] # Identifies numeric variables that can be aggregated.
  numeric_cols <- setdiff(numeric_cols, unique(c(group_cols, exclude_cols))) # Removes grouping and explicitly excluded numeric columns.
  if (length(numeric_cols) == 0L) stop("No numeric value columns were available for monthly aggregation.") # Prevents silently returning an empty environmental table.
  df %>% group_by(across(all_of(group_cols))) %>% summarise(across(all_of(numeric_cols), finite_mean), .groups = "drop") # Produces one row per group with mean values for each environmental variable.
} # Closes the generic monthly aggregation helper.

standardize_monthly_satellite <- function(path, value_col, region_name, dataset_name) { # Defines a helper for monthly Landsat-derived indices.
  df <- readr::read_csv(path, show_col_types = FALSE) %>% select(-any_of("system:index")) # Reads one satellite file and removes the Earth-Engine index when present.
  assert_has_columns(df, c("year", "month", value_col), paste0(dataset_name, "_", region_name)) # Confirms calendar and value columns are available.
  out <- df %>% transmute(year = as.integer(year), month = as.integer(month), value = suppressWarnings(as.numeric(.data[[value_col]]))) %>% filter(month %in% 4:10) %>% group_by(year, month) %>% summarise(value = finite_mean(value), .groups = "drop") %>% arrange(year, month) # Collapses any accidental duplicate satellite rows to one monthly regional value.
  names(out)[names(out) == "value"] <- value_col # Restores the original environmental variable name after aggregation.
  assert_unique_key(out, c("year", "month"), paste0(dataset_name, "_", region_name)) # Verifies one satellite value per year-month.
  out # Returns the canonical monthly satellite table.
} # Closes the satellite standardization helper.

standardize_smi <- function(path, dataset_name) { # Defines a helper that converts SMI source data to one regional value per month.
  df <- read.csv(path, sep = ",", header = TRUE, stringsAsFactors = FALSE) # Reads one soil-moisture-index source file.
  assert_has_columns(df, c("time", "ALB", "HAI", "SCH"), dataset_name) # Confirms the expected date and regional SMI columns exist.
  df <- df %>% mutate(time = as.Date(time), year = lubridate::year(time), month = lubridate::month(time)) # Derives integer calendar coordinates from the source date.
  out <- df %>% filter(month %in% 4:10) %>% group_by(year, month) %>% summarise(ALB = finite_mean(ALB), HAI = finite_mean(HAI), SCH = finite_mean(SCH), .groups = "drop") %>% arrange(year, month) # Aggregates daily or repeated SMI observations to one monthly value for each region.
  assert_unique_key(out, c("year", "month"), dataset_name) # Verifies that SMI now has exactly one row per month.
  out # Returns the canonical monthly SMI table.
} # Closes the SMI standardization helper.

SMI_total_monthly <- standardize_smi(input_paths$smi_total, "SMI_total") # Builds duplicate-free monthly total-soil SMI.
SMI_upsoil_monthly <- standardize_smi(input_paths$smi_upsoil, "SMI_upsoil") # Builds duplicate-free monthly upper-soil SMI.
NDVI_ALB_monthly <- standardize_monthly_satellite(input_paths$ndvi_alb, "mean_NDVI", "ALB", "NDVI") # Builds duplicate-free ALB NDVI.
NDVI_HAI_monthly <- standardize_monthly_satellite(input_paths$ndvi_hai, "mean_NDVI", "HAI", "NDVI") # Builds duplicate-free HAI NDVI.
NDVI_SCH_monthly <- standardize_monthly_satellite(input_paths$ndvi_sch, "mean_NDVI", "SCH", "NDVI") # Builds duplicate-free SCH NDVI.
NDMI_ALB_monthly <- standardize_monthly_satellite(input_paths$ndmi_alb, "mean_NDMI", "ALB", "NDMI") # Builds duplicate-free ALB NDMI.
NDMI_HAI_monthly <- standardize_monthly_satellite(input_paths$ndmi_hai, "mean_NDMI", "HAI", "NDMI") # Builds duplicate-free HAI NDMI.
NDMI_SCH_monthly <- standardize_monthly_satellite(input_paths$ndmi_sch, "mean_NDMI", "SCH", "NDMI") # Builds duplicate-free SCH NDMI.
NIRv_ALB_monthly <- standardize_monthly_satellite(input_paths$nirv_alb, "mean_NIRv", "ALB", "NIRv") # Builds duplicate-free ALB NIRv.
NIRv_HAI_monthly <- standardize_monthly_satellite(input_paths$nirv_hai, "mean_NIRv", "HAI", "NIRv") # Builds duplicate-free HAI NIRv.
NIRv_SCH_monthly <- standardize_monthly_satellite(input_paths$nirv_sch, "mean_NIRv", "SCH", "NIRv") # Builds duplicate-free SCH NIRv.

weather_raw <- read.csv(input_paths$weather, stringsAsFactors = FALSE) # Reads the plot-level weather source table.
assert_has_columns(weather_raw, c("datetime", "plotID"), "weather_raw") # Confirms the weather table contains date and plot identifiers.
weather_calendar <- weather_raw %>% mutate(datetime = as.Date(datetime), year = lubridate::year(datetime), month = lubridate::month(datetime), region = case_when(str_starts(as.character(plotID), "A") ~ "ALB", str_starts(as.character(plotID), "H") ~ "HAI", str_starts(as.character(plotID), "S") ~ "SCH", TRUE ~ NA_character_)) %>% filter(!is.na(region), month %in% 4:10) # Adds region and calendar columns and retains the biological sampling season.
weather_monthly <- aggregate_numeric_monthly(weather_calendar, c("region", "year", "month"), exclude_cols = c("day", "plotID")) %>% arrange(region, year, month) # Averages plot-level weather observations to one regional monthly value per variable.
assert_unique_key(weather_monthly, c("region", "year", "month"), "weather_monthly") # Verifies that weather has exactly one row per region-month.

mapvals <- setNames(4:10, LETTERS[1:7]) # Defines the collection-run-to-calendar-month mapping used by ds22007.
decode_collection_run <- function(x) { # Defines a helper that converts collection-run labels to numeric months.
  run_letter <- stringr::str_sub(as.character(x), 1, 1) # Extracts the leading collection-run letter.
  as.integer(unname(mapvals[run_letter])) # Converts the run letter to its April-through-October month number.
} # Closes the collection-run decoder.
insects_raw <- readr::read_csv(input_paths$insects, show_col_types = FALSE) # Reads the raw ds22007 insect observations.
assert_has_columns(insects_raw, c("CollectionRun", "CollectionYear", "Exploratory", "Family", "NumberAdults"), "insects_raw") # Confirms the raw insect table contains all fields needed for monthly aggregation.
insects_monthly <- insects_raw %>% mutate(month = decode_collection_run(CollectionRun), year = as.integer(CollectionYear), NumberAdults = suppressWarnings(as.numeric(NumberAdults))) %>% filter(!is.na(Exploratory), !is.na(Family), Family != "Fam.", !is.na(year), month %in% 4:10) %>% group_by(Exploratory, Family, year, month) %>% summarise(NumberAdults = sum(NumberAdults, na.rm = TRUE), .groups = "drop") %>% arrange(Exploratory, Family, year, month) # Sums all within-region family observations to one abundance value per family-month.
assert_unique_key(insects_monthly, c("Exploratory", "Family", "year", "month"), "insects_monthly") # Verifies one insect-abundance row per family-region-month.

insect_year_range <- range(insects_monthly$year, na.rm = TRUE) # Derives the observed ds22007 year range used for environmental clipping.
clip_years <- function(df) { # Defines a helper that restricts environmental tables to the insect observation years.
  df %>% filter(year >= insect_year_range[1], year <= insect_year_range[2]) # Keeps only environmental months within the insect year span.
} # Closes the year-clipping helper.
SMI_total_monthly <- clip_years(SMI_total_monthly) # Restricts total-soil SMI to the ds22007 period.
SMI_upsoil_monthly <- clip_years(SMI_upsoil_monthly) # Restricts upper-soil SMI to the ds22007 period.
NDVI_ALB_monthly <- clip_years(NDVI_ALB_monthly) # Restricts ALB NDVI to the ds22007 period.
NDVI_HAI_monthly <- clip_years(NDVI_HAI_monthly) # Restricts HAI NDVI to the ds22007 period.
NDVI_SCH_monthly <- clip_years(NDVI_SCH_monthly) # Restricts SCH NDVI to the ds22007 period.
NDMI_ALB_monthly <- clip_years(NDMI_ALB_monthly) # Restricts ALB NDMI to the ds22007 period.
NDMI_HAI_monthly <- clip_years(NDMI_HAI_monthly) # Restricts HAI NDMI to the ds22007 period.
NDMI_SCH_monthly <- clip_years(NDMI_SCH_monthly) # Restricts SCH NDMI to the ds22007 period.
NIRv_ALB_monthly <- clip_years(NIRv_ALB_monthly) # Restricts ALB NIRv to the ds22007 period.
NIRv_HAI_monthly <- clip_years(NIRv_HAI_monthly) # Restricts HAI NIRv to the ds22007 period.
NIRv_SCH_monthly <- clip_years(NIRv_SCH_monthly) # Restricts SCH NIRv to the ds22007 period.
weather_monthly <- clip_years(weather_monthly) # Restricts monthly weather to the ds22007 period.

make_region_list <- function(region_name, ndvi, ndmi, nirv) { # Defines a helper that assembles one canonical regional analysis object.
  smi_total_region <- SMI_total_monthly %>% select(year, month, all_of(region_name)) # Selects the region-specific total-soil SMI series.
  smi_upsoil_region <- SMI_upsoil_monthly %>% select(year, month, all_of(region_name)) # Selects the region-specific upper-soil SMI series.
  weather_region <- weather_monthly %>% filter(region == region_name) %>% arrange(year, month) # Selects duplicate-free monthly weather for the region.
  insect_region <- insects_monthly %>% filter(Exploratory == region_name) %>% arrange(Family, year, month) # Selects duplicate-free monthly insects for the region.
  assert_unique_key(smi_total_region, c("year", "month"), paste0("SMI_total_", region_name)) # Confirms total-soil SMI uniqueness for the region.
  assert_unique_key(smi_upsoil_region, c("year", "month"), paste0("SMI_upsoil_", region_name)) # Confirms upper-soil SMI uniqueness for the region.
  assert_unique_key(ndvi, c("year", "month"), paste0("NDVI_", region_name)) # Confirms NDVI uniqueness for the region.
  assert_unique_key(ndmi, c("year", "month"), paste0("NDMI_", region_name)) # Confirms NDMI uniqueness for the region.
  assert_unique_key(nirv, c("year", "month"), paste0("NIRv_", region_name)) # Confirms NIRv uniqueness for the region.
  assert_unique_key(weather_region, c("year", "month"), paste0("weather_", region_name)) # Confirms weather uniqueness for the region.
  assert_unique_key(insect_region, c("Exploratory", "Family", "year", "month"), paste0("insects_", region_name)) # Confirms insect uniqueness for the region.
  list(SMI_total = smi_total_region, SMI_upsoil = smi_upsoil_region, NDVI = ndvi, NDMI = ndmi, NIRv = nirv, weather = weather_region, ds22007 = insect_region) # Returns the complete duplicate-free regional object.
} # Closes the regional-object constructor.

ds22007_monthly_ALB <- make_region_list("ALB", NDVI_ALB_monthly, NDMI_ALB_monthly, NIRv_ALB_monthly) # Builds the corrected ALB monthly object.
ds22007_monthly_HAI <- make_region_list("HAI", NDVI_HAI_monthly, NDMI_HAI_monthly, NIRv_HAI_monthly) # Builds the corrected HAI monthly object.
ds22007_monthly_SCH <- make_region_list("SCH", NDVI_SCH_monthly, NDMI_SCH_monthly, NIRv_SCH_monthly) # Builds the corrected SCH monthly object.

saveRDS(ds22007_monthly_ALB, file.path(out_objects, "ds22007_monthly_ALB_clean.rds")) # Saves the corrected ALB object.
saveRDS(ds22007_monthly_HAI, file.path(out_objects, "ds22007_monthly_HAI_clean.rds")) # Saves the corrected HAI object.
saveRDS(ds22007_monthly_SCH, file.path(out_objects, "ds22007_monthly_SCH_clean.rds")) # Saves the corrected SCH object.
write.csv(ds22007_monthly_ALB$ds22007, file.path(out_tables, "ds22007_ALB_insects_monthly_clean.csv"), row.names = FALSE) # Exports ALB monthly insect abundances.
write.csv(ds22007_monthly_HAI$ds22007, file.path(out_tables, "ds22007_HAI_insects_monthly_clean.csv"), row.names = FALSE) # Exports HAI monthly insect abundances.
write.csv(ds22007_monthly_SCH$ds22007, file.path(out_tables, "ds22007_SCH_insects_monthly_clean.csv"), row.names = FALSE) # Exports SCH monthly insect abundances.
write.csv(weather_monthly, file.path(out_tables, "weather_monthly_clean.csv"), row.names = FALSE) # Exports duplicate-free regional monthly weather.
write.csv(SMI_total_monthly, file.path(out_tables, "SMI_total_monthly_clean.csv"), row.names = FALSE) # Exports duplicate-free monthly total-soil SMI.
write.csv(SMI_upsoil_monthly, file.path(out_tables, "SMI_upsoil_monthly_clean.csv"), row.names = FALSE) # Exports duplicate-free monthly upper-soil SMI.

structure_audit <- bind_rows( # Combines uniqueness diagnostics for every regional object into one audit table.
  tibble(object = "ALB_insects", n_rows = nrow(ds22007_monthly_ALB$ds22007), n_duplicate_keys = sum(duplicated(ds22007_monthly_ALB$ds22007[c("Exploratory", "Family", "year", "month")]))) , # Summarizes ALB insect key uniqueness.
  tibble(object = "HAI_insects", n_rows = nrow(ds22007_monthly_HAI$ds22007), n_duplicate_keys = sum(duplicated(ds22007_monthly_HAI$ds22007[c("Exploratory", "Family", "year", "month")]))) , # Summarizes HAI insect key uniqueness.
  tibble(object = "SCH_insects", n_rows = nrow(ds22007_monthly_SCH$ds22007), n_duplicate_keys = sum(duplicated(ds22007_monthly_SCH$ds22007[c("Exploratory", "Family", "year", "month")]))) , # Summarizes SCH insect key uniqueness.
  tibble(object = "weather", n_rows = nrow(weather_monthly), n_duplicate_keys = sum(duplicated(weather_monthly[c("region", "year", "month")]))) , # Summarizes weather key uniqueness.
  tibble(object = "SMI_total", n_rows = nrow(SMI_total_monthly), n_duplicate_keys = sum(duplicated(SMI_total_monthly[c("year", "month")]))) , # Summarizes total-soil SMI key uniqueness.
  tibble(object = "SMI_upsoil", n_rows = nrow(SMI_upsoil_monthly), n_duplicate_keys = sum(duplicated(SMI_upsoil_monthly[c("year", "month")]))) # Summarizes upper-soil SMI key uniqueness.
) # Closes the combined structure-audit table.
write.csv(structure_audit, file.path(out_diagnostics, "canonical_monthly_structure_audit.csv"), row.names = FALSE) # Saves the monthly uniqueness audit.
if (any(structure_audit$n_duplicate_keys > 0L)) stop("Duplicate keys remain in the canonical monthly inputs; analysis aborted.") # Refuses to finish if any duplicate time point survived.

cat("\nFINAL MONTHLY INPUT BUILD COMPLETE.\n") # Prints a completion marker to the console.
cat("All canonical monthly objects contain zero duplicated analysis keys.\n") # Confirms the central data-structure guarantee.
cat("Run FINAL_MANUSCRIPT_ANALYSIS_ds22007.R next.\n") # States the required next analysis step.
