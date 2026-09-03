source("manure_future_helpers.R")

if (!requireNamespace("dplyr", quietly = TRUE) ||
    !requireNamespace("tidyr", quietly = TRUE)) {
  stop("dplyr and tidyr are required for this test.")
}

region_map <- data.frame(
  I = c(1L, 2L, 3L),
  J = c(1L, 1L, 1L),
  R = c("A", "A", "B")
)
body_weight <- data.frame(
  I = c(1L, 2L, 3L),
  J = c(1L, 1L, 1L),
  slnum = c(1L, 1L, 1L),
  BodyWeight = c(100, -99, 0)
)
reference_heads <- data.frame(
  I = c(1L, 2L),
  J = c(1L, 1L),
  slnum = c(1L, 1L),
  Livestock_number = c(10, 5)
)
target_heads <- data.frame(
  I = c(1L, 2L, 3L),
  J = c(1L, 1L, 1L),
  slnum = c(1L, 1L, 1L),
  Livestock_number = c(8, 4, 2)
)

filled <- nflow_impute_body_weight(
  target_heads,
  reference_heads,
  body_weight,
  region_map,
  2050
)
stopifnot(
  nrow(filled$data) == 3L,
  all(!is.na(filled$diagnostics$imputed)),
  all(is.finite(filled$data$BodyWeight)),
  all(filled$data$BodyWeight > 0),
  identical(as.numeric(filled$data$BodyWeight), c(100, 100, 100)),
  identical(
    as.character(filled$data$bw_fill_source),
    c("grid_2010", "region_species_2010", "global_species_2010")
  )
)

target_with_outside <- rbind(
  target_heads,
  data.frame(I = 4L, J = 1L, slnum = 1L, Livestock_number = 6)
)
reference_with_outside <- rbind(
  reference_heads,
  data.frame(I = 4L, J = 1L, slnum = 1L, Livestock_number = 3)
)
body_weight_with_outside <- rbind(
  body_weight,
  data.frame(I = 4L, J = 1L, slnum = 1L, BodyWeight = 50)
)
filled_with_outside <- nflow_impute_body_weight(
  target_with_outside,
  reference_with_outside,
  body_weight_with_outside,
  region_map,
  2050
)
outside_target_audit <- filled_with_outside$diagnostics |>
  dplyr::filter(
    year == 2050L,
    bw_fill_source == "outside_model_domain"
  )
stopifnot(
  nrow(filled_with_outside$data) == 3L,
  nrow(outside_target_audit) == 1L,
  identical(outside_target_audit$cells, 1L),
  isTRUE(all.equal(outside_target_audit$livestock_heads, 6)),
  isTRUE(all.equal(outside_target_audit$livestock_head_share, 0.3))
)

body_weight_test_file <- tempfile(fileext = ".csv")
writeLines(
  c("BodyWeight=", " 1, 2, 3,", " 4, 5, 6;"),
  body_weight_test_file
)
body_weight_test_values <- nflow_read_body_weight_table(
  body_weight_test_file,
  n_species = 1L,
  n_latitude = 2L,
  n_longitude = 3L
)
unlink(body_weight_test_file)
stopifnot(identical(body_weight_test_values, as.numeric(1:6)))

direct_source <- data.frame(
  RegionCode = c("A", "A", "A", "B"),
  G = c("g1", "gX", NA_character_, "b1"),
  tN = c(4, 2, 1, 1)
)
direct_regional_total <- data.frame(
  RegionCode = c("A", "B"),
  YEMF = c(2050L, 2050L),
  SCENARIO = c("BaU", "BaU"),
  tN = c(10, 4)
)
future_pasture <- data.frame(
  RegionCode = c("A", "A", "B", "B"),
  YEMF = c(2050L, 2050L, 2050L, 2050L),
  G = c("g1", "g2", "b1", "b2"),
  LURATIO = c(0.25, 0.75, 0.4, 0.6)
)

direct_allocation <- nflow_allocate_direct_excretion(
  direct_source,
  direct_regional_total,
  future_pasture
)
direct_a <- direct_allocation[direct_allocation$RegionCode == "A", ]
direct_b <- direct_allocation[direct_allocation$RegionCode == "B", ]
allocated_regional <- direct_allocation |>
  dplyr::group_by(RegionCode, YEMF, SCENARIO) |>
  dplyr::summarise(tN = sum(tN), .groups = "drop") |>
  dplyr::arrange(RegionCode)
stopifnot(
  isTRUE(all.equal(direct_a$source_tN, c(4, 0))),
  isTRUE(all.equal(direct_a$redistributed_tN, c(1.5, 4.5))),
  isTRUE(all.equal(direct_a$tN, c(5.5, 4.5))),
  isTRUE(all.equal(direct_b$source_tN, c(1, 0))),
  isTRUE(all.equal(direct_b$redistributed_tN, c(1.2, 1.8))),
  isTRUE(all.equal(direct_b$tN, c(2.2, 1.8))),
  isTRUE(all.equal(allocated_regional$tN, c(10, 4))),
  all(direct_allocation$tN >= 0)
)

expect_error <- function(expression) {
  tryCatch(
    {
      force(expression)
      FALSE
    },
    error = function(error) TRUE
  )
}

duplicate_source <- rbind(direct_source, direct_source[1, ])
duplicate_regional <- rbind(
  direct_regional_total,
  direct_regional_total[1, ]
)
duplicate_pasture <- rbind(future_pasture, future_pasture[1, ])
negative_source <- direct_source
negative_source$tN[1] <- -1
negative_regional <- direct_regional_total
negative_regional$tN[1] <- -1
negative_ratio <- future_pasture
negative_ratio$LURATIO[1] <- -0.25
no_pasture_regional <- rbind(
  direct_regional_total,
  data.frame(RegionCode = "C", YEMF = 2050L, SCENARIO = "BaU", tN = 1)
)
source_exceeds_total <- direct_regional_total
source_exceeds_total$tN[source_exceeds_total$RegionCode == "A"] <- 6

stopifnot(
  expect_error(nflow_allocate_direct_excretion(
    duplicate_source, direct_regional_total, future_pasture
  )),
  expect_error(nflow_allocate_direct_excretion(
    direct_source, duplicate_regional, future_pasture
  )),
  expect_error(nflow_allocate_direct_excretion(
    direct_source, direct_regional_total, duplicate_pasture
  )),
  expect_error(nflow_allocate_direct_excretion(
    negative_source, direct_regional_total, future_pasture
  )),
  expect_error(nflow_allocate_direct_excretion(
    direct_source, negative_regional, future_pasture
  )),
  expect_error(nflow_allocate_direct_excretion(
    direct_source, direct_regional_total, negative_ratio
  )),
  expect_error(nflow_allocate_direct_excretion(
    direct_source, no_pasture_regional, future_pasture
  )),
  expect_error(nflow_allocate_direct_excretion(
    direct_source, source_exceeds_total, future_pasture
  ))
)

message("Future-manure helper tests passed.")
