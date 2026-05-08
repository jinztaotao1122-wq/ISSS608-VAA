portfolio_cols <- c(
  "CC" = "#1b4965",
  "COMP_E" = "#ca6702",
  "COMP_N" = "#bb3e03",
  "TP" = "#0a9396",
  "TPG" = "#94d2bd"
)

accent_cols <- c(
  "primary" = "#1b4965",
  "secondary" = "#ca6702",
  "highlight" = "#bb3e03",
  "muted" = "#5c677d"
)

portfolio_theme <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14, colour = accent_cols["primary"]),
      plot.subtitle = element_text(size = 11, colour = accent_cols["muted"]),
      plot.caption = element_text(size = 9, colour = accent_cols["muted"]),
      axis.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.title = element_text(face = "bold")
    )
}

prepare_motor_data <- function(path) {
  raw <- readr::read_delim(path, delim = ";", show_col_types = FALSE)

  loss_ratio_cap_value <- raw |>
    dplyr::filter(total_premium > 0) |>
    dplyr::mutate(loss_ratio = total_incurred / total_premium) |>
    dplyr::summarise(cap = stats::quantile(loss_ratio, 0.99, na.rm = TRUE)) |>
    dplyr::pull(cap)

  claim_freq_cap_value <- raw |>
    dplyr::filter(total_exposure > 0) |>
    dplyr::mutate(claim_frequency = total_claims / total_exposure) |>
    dplyr::summarise(cap = stats::quantile(claim_frequency, 0.99, na.rm = TRUE)) |>
    dplyr::pull(cap)

  raw |>
    dplyr::mutate(
      license_tenure = year - age_driving_licence,
      claim_flag = total_claims > 0,
      total_margin = total_premium - total_incurred,
      premium_per_exposure = dplyr::if_else(total_exposure > 0, total_premium / total_exposure, NA_real_),
      pure_premium = dplyr::if_else(total_exposure > 0, total_incurred / total_exposure, NA_real_),
      loss_ratio = dplyr::if_else(total_premium > 0, total_incurred / total_premium, NA_real_),
      claim_frequency = dplyr::if_else(total_exposure > 0, total_claims / total_exposure, NA_real_),
      claim_severity = dplyr::if_else(total_claims > 0, total_incurred / total_claims, NA_real_),
      loss_ratio_cap = pmin(loss_ratio, loss_ratio_cap_value, na.rm = TRUE),
      claim_frequency_cap = pmin(claim_frequency, claim_freq_cap_value, na.rm = TRUE),
      driver_age_band = cut(
        driver_age,
        breaks = c(17, 25, 35, 45, 55, 65, 100),
        include.lowest = TRUE,
        right = FALSE
      ),
      vehicle_age_band = cut(
        vehicle_age,
        breaks = c(-1, 3, 6, 10, 15, 100),
        include.lowest = TRUE,
        right = FALSE
      ),
      vehicle_value_band = cut(
        vehicle_value,
        breaks = stats::quantile(vehicle_value, probs = seq(0, 1, 0.2), na.rm = TRUE),
        include.lowest = TRUE
      )
    )
}

portfolio_snapshot <- function(data) {
  dplyr::tibble(
    metric = c(
      "Rows",
      "Unique insured IDs",
      "Policy years",
      "Rows with zero premium",
      "Rows with zero exposure",
      "Rows with at least one claim",
      "Average premium",
      "Exposure-weighted loss ratio",
      "Exposure-weighted claim frequency"
    ),
    value = c(
      scales::comma(nrow(data)),
      scales::comma(dplyr::n_distinct(data$insured_id)),
      paste(range(data$year), collapse = " to "),
      scales::comma(sum(data$total_premium <= 0, na.rm = TRUE)),
      scales::comma(sum(data$total_exposure <= 0, na.rm = TRUE)),
      scales::percent(mean(data$claim_flag, na.rm = TRUE), accuracy = 0.1),
      scales::dollar(mean(data$total_premium, na.rm = TRUE)),
      scales::percent(sum(data$total_incurred, na.rm = TRUE) / sum(data$total_premium, na.rm = TRUE), accuracy = 0.1),
      scales::number(sum(data$total_claims, na.rm = TRUE) / sum(data$total_exposure, na.rm = TRUE), accuracy = 0.001)
    )
  )
}

segment_metrics <- function(data, group_var) {
  data |>
    dplyr::group_by(.data[[group_var]]) |>
    dplyr::summarise(
      policies = dplyr::n(),
      exposure = sum(total_exposure, na.rm = TRUE),
      avg_premium = mean(total_premium, na.rm = TRUE),
      claim_rate = mean(claim_flag, na.rm = TRUE),
      claim_frequency = sum(total_claims, na.rm = TRUE) / sum(total_exposure, na.rm = TRUE),
      loss_ratio = sum(total_incurred, na.rm = TRUE) / sum(total_premium, na.rm = TRUE),
      severity = sum(total_incurred, na.rm = TRUE) / sum(total_claims, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::rename(group = 1)
}

metric_test_table <- function(data, group_vars) {
  purrr::map_dfr(group_vars, function(group_var) {
    claim_formula <- stats::as.formula(paste("claim_flag ~", group_var))
    loss_formula <- stats::as.formula(paste("loss_ratio_cap ~", group_var))
    freq_formula <- stats::as.formula(paste("claim_frequency_cap ~", group_var))

    dplyr::tibble(
      factor = group_var,
      claim_flag_p = stats::chisq.test(stats::xtabs(claim_formula, data = data))$p.value,
      loss_ratio_p = stats::kruskal.test(loss_formula, data = data)$p.value,
      claim_frequency_p = stats::kruskal.test(freq_formula, data = data)$p.value
    )
  }) |>
    dplyr::mutate(
      across(ends_with("_p"), ~ dplyr::if_else(.x < 2.2e-16, "< 2.2e-16", format(.x, scientific = TRUE)))
    )
}

sample_for_distribution <- function(data, n = 60000) {
  set.seed(608)
  sampled <- data |>
    dplyr::filter(!is.na(loss_ratio_cap), !is.na(claim_severity)) |>
    dplyr::slice_sample(n = min(nrow(dplyr::filter(data, !is.na(loss_ratio_cap), !is.na(claim_severity))), n))

  sampled
}
