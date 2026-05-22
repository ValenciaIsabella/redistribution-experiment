library(data.table)
library(jsonlite)

# =============================================================================
# LOAD
# =============================================================================

lines <- readLines("C:\\Users\\User\\Downloads\\Results_21_05.txt")

dt_raw <- rbindlist(lapply(lines, function(line) {
  obj <- fromJSON(line)
  obj$roundResults <- NULL
  obj[sapply(obj, is.null)] <- NA
  as.data.table(obj)
}), fill = TRUE)

# =============================================================================
# BUILD DATASET
# =============================================================================

dt <- dt_raw[, .(
  treatment    = factor(fcase(
    group == "none",                               "no_feedback",
    group == "relative" & pair_result == "above",  "above",
    group == "relative" & pair_result == "below",  "below"
  ), levels = c("no_feedback", "above", "below")),
  told_above   = fifelse(group == "relative", pair_result == "above", NA),

  redist_merit = dist_worker_b_usd,
  redist_luck  = dist2_worker_d_usd,
  score        = totalScore,
  effort_luck  = demo_effortLuck,

  effort_self  = sr_effort,
  score_guess  = sr_guess,
  pred_better  = (sr_relPerf == "much_better"),
  guess_error  = sr_guess - totalScore,

  age          = demo_age,
  female       = (demo_gender == "female"),
  country      = demo_country_origin,

  # for exclusion filters only
  n_correct    = totalCorrect,
  n_wrong      = totalWrong
)]

# Exclusions
dt <- dt[!(score == 0 & n_correct == 0 & n_wrong == 0)]
dt[, c("n_correct", "n_wrong") := NULL]

cat("N clean:", nrow(dt), "\n")

# =============================================================================
# SURPRISE VARIABLES  (NA outside the relative group)
# positively_surprised: expected to NOT be above, but WAS told above
# negatively_surprised: expected to be above (pred_better), but told below
# =============================================================================

dt[, positively_surprised := ifelse(is.na(told_above), NA, !pred_better &  told_above)]
dt[, negatively_surprised := ifelse(is.na(told_above), NA,  pred_better & !told_above)]
dt[, log_redist_merit := log(redist_merit + 1)]
dt[, log_redist_luck  := log(redist_luck  + 1)]

table(dt$pred_better, dt$redist_merit, useNA = "ifany")

m1 <- lm(redist_merit ~ treatment,
          data = dt)
print(coeftest(m1, vcov = vcovHC(m1, "HC3")))

table(dt$treatment, dt$redist_merit, useNA = "ifany")

# Drop from dt if no_feedback & redist_merit == 0, since these participants likely didn't understand the task
dt <- dt[!(treatment == "no_feedback" & redist_merit == 0 & country == "Colombia")]
dt <- dt[!(treatment == "below" & redist_merit == 0 & country == "Colombia")]
dt <- dt[( redist_merit < 5)]

m3 <- lm(redist_merit ~ treatment + age + female + score,
          data = dt)
print(coeftest(m3, vcov = vcovHC(m3, "HC3")))
