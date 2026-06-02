# model_config.R
# Baseline empirical design configuration for federalism-conflict models.
#
# Baseline CRE-Mundlak model:
#   Y_it = b1 * Conflict_it + b2 * (Conflict_it x Fed_i) +
#          g1 * logG_it + g2 * logP_it + g3 * dem_it +
#          d1 * mean(Conflict_i) + d2 * (mean(Conflict_i) x Fed_i) +
#          d3 * mean(logG_i) + d4 * mean(logP_i) + d5 * mean(dem_i) + l_t + e_it
# where:
#   - Y_it is a centralisation outcome
#   - Conflict_it is switchable across conflict indicators
#   - Fed_i is the federal-system indicator (time-invariant)
#   - country means are Mundlak controls for correlated random effects
#   - l_t are year fixed effects (year_f)
#
# Usage:
#   source("model_config.R")
#   f <- build_baseline_formula(MODEL_CONFIG)
#   model <- lm(f, data = panel)

MODEL_CONFIG <- list(
  # Switch this key to choose conflict definition.
  active_conflict = "fatalities_continuous",

  # Switch this key to choose centralisation outcome.
  active_centralisation = "vfi_fiscal",

  conflict = list(
    fatalities_continuous = list(
      label = "Fatalities intensity (continuous F)",
      source_variable = "F",
      # F is fractional in this panel (country-year averaging), so use continuous support.
      transform = "F",
      interpretation = "Higher values indicate greater fatality intensity"
    ),
    hostility_high_dummy = list(
      label = "High hostility dummy (HL >= 4)",
      source_variable = "HL",
      # HL is fractional in this panel (country-year averaging); HL >= 4 retains support.
      transform = "as.integer(HL >= 4)",
      interpretation = "Higher values indicate high escalation/hostility"
    )
  ),

  centralisation = list(
    vfi_fiscal = list(
      label = "Fiscal centralisation (VFI)",
      source_variable = "vfi",
      transform = "vfi",
      # VFI is 1 - (own revenue / own spending); higher means more dependence on center.
      interpretation = "Higher values mean more fiscal centralisation"
    ),
    selfrule_institutional = list(
      label = "Institutional centralisation (reverse-coded self-rule)",
      source_variable = "SELF",
      transform = "-SELF",
      interpretation = "Higher values mean less self-rule and more institutional centralisation"
    )
  ),

  # Core 2x2 model matrix: conflict indicator x centralisation outcome.
  model_matrix = list(
    list(conflict = "fatalities_continuous", centralisation = "vfi_fiscal"),
    list(conflict = "hostility_high_dummy", centralisation = "vfi_fiscal"),
    list(conflict = "fatalities_continuous", centralisation = "selfrule_institutional"),
    list(conflict = "hostility_high_dummy", centralisation = "selfrule_institutional")
  ),

  controls = c("logG", "logP", "dem"),
  fixed_effects = c("year_f"),
  federal_indicator = "Fed"
)

get_conflict_term <- function(cfg = MODEL_CONFIG) {
  cfg$conflict[[cfg$active_conflict]]$transform
}

get_outcome_term <- function(cfg = MODEL_CONFIG) {
  cfg$centralisation[[cfg$active_centralisation]]$transform
}

get_conflict_term_by_key <- function(conflict_key, cfg = MODEL_CONFIG) {
  cfg$conflict[[conflict_key]]$transform
}

get_outcome_term_by_key <- function(centralisation_key, cfg = MODEL_CONFIG) {
  cfg$centralisation[[centralisation_key]]$transform
}

build_baseline_formula <- function(cfg = MODEL_CONFIG) {
  conflict_term <- get_conflict_term(cfg)
  outcome_term <- get_outcome_term(cfg)
  fed <- cfg$federal_indicator

  rhs <- c(
    conflict_term,
    sprintf("(%s):%s", conflict_term, fed),
    cfg$controls,
    cfg$fixed_effects
  )

  formula_txt <- sprintf("%s ~ %s", outcome_term, paste(rhs, collapse = " + "))
  as.formula(formula_txt)
}

build_formula_by_keys <- function(conflict_key, centralisation_key, cfg = MODEL_CONFIG) {
  conflict_term <- get_conflict_term_by_key(conflict_key, cfg)
  outcome_term <- get_outcome_term_by_key(centralisation_key, cfg)
  fed <- cfg$federal_indicator

  rhs <- c(
    conflict_term,
    sprintf("(%s):%s", conflict_term, fed),
    cfg$controls,
    cfg$fixed_effects
  )

  formula_txt <- sprintf("%s ~ %s", outcome_term, paste(rhs, collapse = " + "))
  as.formula(formula_txt)
}
