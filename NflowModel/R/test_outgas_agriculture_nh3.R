script_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_argument) != 1L) {
  stop("Could not determine the path of test_outgas_agriculture_nh3.R.")
}

script_file <- normalizePath(
  sub("^--file=", "", script_argument),
  winslash = "/",
  mustWork = TRUE
)
make_result_file <- file.path(dirname(script_file), "Make_Result_csv.r")

# Load definitions only; do not start the expensive scenario calculations.
expressions <- parse(file = make_result_file)
for (expression in expressions) {
  starts_run_outputs <-
    is.call(expression) &&
    identical(as.character(expression[[1]]), "<-") &&
    identical(as.character(expression[[2]]), "run_outputs")

  if (starts_run_outputs) {
    break
  }

  eval(expression, envir = globalenv())
}

test_env <- new.env(parent = globalenv())
test_scenario <- "SSP2_BaU_NoCC"

add_gas_total <- function(object_name, value_column, value) {
  object <- data.frame(
    YEMF = "2010",
    SCENARIO = test_scenario,
    value = value,
    stringsAsFactors = FALSE
  )
  names(object)[3] <- value_column
  assign(object_name, object, envir = test_env)
}

add_gas_total("OUTgascrop", "OUTgascrop", 24)
add_gas_total("OUTgaspas", "OUTgaspas", 11)
add_gas_total("OUTgas", "OUTgas", 35)
add_gas_total("OUTgascrop_NH3", "OUTgascrop", 14)
add_gas_total("OUTgaspas_NH3", "OUTgaspas", 4)
add_gas_total("OUTgas_NH3", "OUTgas", 18)
add_gas_total("OUTgascrop_N2", "OUTgascrop", 6)
add_gas_total("OUTgaspas_N2", "OUTgaspas", 5)
add_gas_total("OUTgas_N2", "OUTgas", 11)
add_gas_total("OUTgascrop_NOx", "OUTgascrop", 4)
add_gas_total("OUTgaspas_NOx", "OUTgaspas", 2)
add_gas_total("OUTgas_NOx", "OUTgas", 6)

test_env$NH3_loss1 <- data.frame(ManureStable = 3)
test_env$NH3_loss2 <- data.frame(ManurePas = 2)

result <- extract_outgas_species_one_case(
  env = test_env,
  scenario_label = "Base2010",
  scenario_set = test_scenario,
  year = "2010"
)

agriculture_total <- result[
  result$Species == "NH3_agriculture_total",
  ,
  drop = FALSE
]

stopifnot(
  nrow(agriculture_total) == 1L,
  agriculture_total$Cropland == 14,
  agriculture_total$Pasture == 4,
  agriculture_total$Manure_management_systems == 3,
  agriculture_total$Direct_excretion == 2,
  agriculture_total$Total == 23
)

test_env$MAP_RG <- data.frame(G = "1", R = "USA")

add_gas_grid <- function(object_name, value_column, value) {
  object <- data.frame(
    G = "1",
    YEMF = "2010",
    value = value,
    stringsAsFactors = FALSE
  )
  names(object)[3] <- value_column
  assign(object_name, object, envir = test_env)
}

for (suffix in c("N2", "NH3", "NOx", "")) {
  suffix_text <- if (nzchar(suffix)) paste0("_", suffix) else ""
  add_gas_grid(
    paste0("OUTgascropdis", suffix_text),
    "OUTgascropgrid",
    1
  )
  add_gas_grid(
    paste0("OUTgaspasdis", suffix_text),
    "OUTgaspasgrid",
    1
  )
  add_gas_grid(
    paste0("OUTgasdis", suffix_text),
    "OUTgasgrid",
    2
  )
}

region_result <- extract_outgas_species_region_one_case(
  env = test_env,
  scenario_label = "Base2010",
  scenario_set = test_scenario,
  year = "2010"
)

stopifnot(
  nrow(region_result) == 15L,
  !anyNA(region_result$Value_Tg_N_yr)
)

message("Agricultural NH3 aggregation and scenario-scoped grid tests passed.")
