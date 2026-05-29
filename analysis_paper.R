# =====================================================================
# analysis_paper.R
# Relative Performance Feedback and Redistribution Preferences
# Baseline: Told Above
# =====================================================================

library(data.table)
library(jsonlite)
library(lmtest)
library(sandwich)
# install.packages(c("car","ggplot2", "patchwork", "modelsummary", "kableExtra"))
library(car)
library(ggplot2)
library(patchwork)
library(modelsummary)
library(kableExtra)

# =====================================================================
# 0.  LOAD & BUILD
# =====================================================================

lines  <- readLines("C:\\Users\\User\\Downloads\\Results_29_05.txt")
dt_raw <- rbindlist(lapply(lines, function(line) {
  obj <- fromJSON(line)
  obj$roundResults <- NULL
  obj[sapply(obj, is.null)] <- NA
  as.data.table(obj)
}), fill = TRUE)

dt <- dt_raw[, .(
  treatment = factor(fcase(
    group == "none",                               "no_feedback",
    group == "relative" & pair_result == "above",  "above",
    group == "relative" & pair_result == "below",  "below"
  ), levels = c("above", "no_feedback", "below")),   # above = baseline
  told_above   = fifelse(group == "relative", pair_result == "above", NA),
  redist_merit = dist_worker_b_usd,
  score        = totalScore,
  effort_luck  = demo_effortLuck / 10,
  effort_self  = sr_effort,
  score_guess  = sr_guess,
  pred_better  = (sr_relPerf == "much_better"),   # TRUE = predicted to outperform
  guess_error  = sr_guess - totalScore,
  age          = demo_age,
  female       = (demo_gender == "female"),
  country      = demo_country_origin,
  education    = factor(demo_education,
                        levels  = c("high-school","some-college","bachelor","master","phd"),
                        ordered = TRUE),
  n_correct    = totalCorrect,
  n_wrong      = totalWrong
)]
table(dt_raw$demo_country_origin, useNA = "ifany")
# Exclusions
dt <- dt[!(score == 0 & n_correct == 0 & n_wrong == 0)]
dt[, c("n_correct", "n_wrong") := NULL]
dt <- dt[redist_merit < 5]
dt[is.na(effort_luck), effort_luck := 5]

cat("N clean:", nrow(dt), "\n")
print(table(dt$treatment, useNA = "ifany"))

# Derived variables
dt[, edu_num   := as.integer(education)]   # 1=high-school … 5=PhD
dt[, colombian := (!is.na(country) & country == "Colombia")]
dt[, edu_3 := factor(fcase(
  education %in% c("high-school", "some-college"), "Less than bachelor",
  education == "bachelor",                          "Bachelor",
  education %in% c("master", "phd"),               "Master or above"
), levels = c("Less than bachelor", "Bachelor", "Master or above"))]
# Binary indicators for Table 1
dt[, edu_lt_ba := (!is.na(edu_3) & edu_3 == "Less than bachelor")]
dt[, edu_ba    := (!is.na(edu_3) & edu_3 == "Bachelor")]
dt[, edu_ma_up := (!is.na(edu_3) & edu_3 == "Master or above")]

dt[, above_positively_surprised := fifelse(told_above == TRUE  & pred_better == FALSE, 1L, 0L)]
dt[, below_negatively_surprised := fifelse(told_above == FALSE & pred_better == TRUE,  1L, 0L)]

# 6-category feedback_type: crosses treatment × prediction direction
# No-feedback group split by pred_better (optimistic vs pessimistic)
# Baseline for full-sample regression: negatively surprised (told below, predicted above)
dt[, feedback_type := factor(fcase(
  treatment == "no_feedback" & pred_better == TRUE,  "nf_optimistic",
  treatment == "no_feedback" & pred_better == FALSE, "nf_pessimistic",
  told_above == FALSE & pred_better == TRUE,         "neg_surprised",
  told_above == FALSE & pred_better == FALSE,        "neg_reassured",
  told_above == TRUE  & pred_better == FALSE,        "pos_surprised",
  told_above == TRUE  & pred_better == TRUE,         "pos_reassured"
), levels = c("neg_surprised", "neg_reassured", "pos_surprised", "pos_reassured",
              "nf_optimistic", "nf_pessimistic"))]

dt[, log_redist_merit := log(redist_merit + 1)]
dt[, log_effort_luck  := log(effort_luck  + 1)]

# Subsamples
dt_eff <- dt[!is.na(effort_luck)]
dt_rel <- dt[!is.na(told_above)]

# Helper: HC3 vcov and coeftest
hc3   <- function(m) vcovHC(m, type = "HC3")
ctest <- function(m) coeftest(m, vcov = hc3(m))

# =====================================================================
# 1.  SUMMARY STATS  (omnibus)
# =====================================================================

cat("\n--- Summary by treatment ---\n")
dt[, .(
  n             = .N,
  redist_merit  = mean(redist_merit, na.rm = TRUE),
  effort_luck   = mean(effort_luck,  na.rm = TRUE),
  score         = mean(score,        na.rm = TRUE),
  age           = mean(age,          na.rm = TRUE),
  pct_female    = mean(female,       na.rm = TRUE)
), by = treatment]

# ANOVA omnibus p-values
cat("ANOVA redist_merit:", summary(aov(redist_merit ~ treatment, data = dt))[[1]][["Pr(>F)"]][1], "\n")
cat("ANOVA effort_luck: ", summary(aov(effort_luck  ~ treatment, data = dt_eff))[[1]][["Pr(>F)"]][1], "\n")

# Non-parametric check
cat("Kruskal-Wallis redist_merit:", kruskal.test(redist_merit ~ treatment, data = dt)$p.value, "\n")
cat("Kruskal-Wallis effort_luck: ", kruskal.test(effort_luck  ~ treatment, data = dt_eff)$p.value, "\n")

# =====================================================================
# 2.  MAIN REGRESSIONS  (baseline = above)
# =====================================================================

# Effort-luck belief
m1 <- lm(effort_luck  ~ treatment,                                              data = dt_eff)
m2 <- lm(effort_luck  ~ treatment + age + female + score + colombian + edu_3,   data = dt_eff)

# Merit redistribution
m3 <- lm(redist_merit ~ treatment,                                              data = dt)
m4 <- lm(redist_merit ~ treatment + age + female + score + colombian + edu_3,   data = dt)

cat("\n=== Effort-luck belief ===\n");   print(ctest(m1))
cat("\n=== Merit redistribution ===\n"); print(ctest(m3))

# =====================================================================
# 3.  FORMAL TESTS OF ASYMMETRY
#     H0: coeff(no_feedback) = coeff(below)  [both vs. above]
#     Rejection means the two non-above groups ARE different from each other
# =====================================================================

cat("\n--- Asymmetry test H0: no_feedback = below (merit) ---\n")
print(linearHypothesis(m3, "treatmentno_feedback = treatmentbelow", vcov = hc3(m3)))

cat("\n--- Asymmetry test H0: no_feedback = below (effort-luck) ---\n")
print(linearHypothesis(m1, "treatmentno_feedback = treatmentbelow", vcov = hc3(m1)))

# =====================================================================
# 4.  MECHANISM: effort_luck as mediator
#     Compare treatment coefficients in m3 vs. m9 to gauge mediation.
#     If the above-group effect shrinks when effort_luck is included,
#     part of the redistribution effect operates through belief updating.
# =====================================================================

m9  <- lm(redist_merit ~ treatment + effort_luck,
          data = dt_eff)
m10 <- lm(redist_merit ~ treatment + effort_luck + age + female + score + colombian + edu_3,
          data = dt_eff)

cat("\n=== Mechanism: treatment + effort_luck → redist_merit ===\n")
print(ctest(m9))
cat("\n(Compare treatment coefficients with m3/m4 to assess mediation)\n")

# =====================================================================
# 5.  SURPRISE HETEROGENEITY  (within relative-feedback group)
#     positively surprised: told above but predicted below
#     negatively surprised: told below but predicted above
# =====================================================================

has_surprise <- sum(!is.na(dt_rel$pred_better)) > 10   # enough non-NA obs

# Diagnostic: distribution of pre-feedback predictions
cat("\n--- Pre-feedback prediction (TRUE = expected to outperform) ---\n")
print(table(dt$treatment, dt$pred_better, useNA = "ifany"))
cat("\n--- % predicting above average by treatment ---\n")
pred_pct <- dt[!is.na(pred_better), .(
  n_total              = .N,
  n_predicted_better   = sum(pred_better),
  pct_predicted_better = round(mean(pred_better) * 100, 1)
), by = treatment]
print(pred_pct)

cat("\n--- Variation in surprise indicators ---\n")
cat("above_positively_surprised:", table(dt_rel$above_positively_surprised), "\n")
cat("below_negatively_surprised:", table(dt_rel$below_negatively_surprised), "\n")

if (has_surprise) {
  m7 <- lm(redist_merit ~ above_positively_surprised + below_negatively_surprised,
           data = dt_rel)
  m8 <- lm(redist_merit ~ above_positively_surprised + below_negatively_surprised
           + age + female + score + colombian + edu_3, data = dt_rel)
  cat("\n=== Surprise heterogeneity (pooled, dt_rel) ===\n"); print(ctest(m7))
} else {
  cat("\nNOTE: pred_better is missing — surprise models skipped.\n")
}

# =====================================================================
# 5b. WITHIN-TREATMENT SURPRISE ANALYSIS
#     Compare surprised vs. not-surprised WITHIN each treatment arm.
#     Above group (n=47): 5 positively surprised, 42 not surprised.
#     Below group (n=48): 37 negatively surprised, 11 not surprised.
#     Outcome: effort_luck AND redist_merit.
# =====================================================================

dt_above     <- dt[treatment == "above"]
dt_below     <- dt[treatment == "below"]
dt_above_eff <- dt_above[!is.na(effort_luck)]
dt_below_eff <- dt_below[!is.na(effort_luck)]

# Descriptive means by surprise status
cat("\n--- Means by treatment × surprise status ---\n")
cat("Above group:\n")
print(dt_above[, .(n = .N, effort_luck = mean(effort_luck, na.rm=TRUE),
                   redist = mean(redist_merit, na.rm=TRUE)),
               by = above_positively_surprised])
cat("Below group:\n")
print(dt_below[, .(n = .N, effort_luck = mean(effort_luck, na.rm=TRUE),
                   redist = mean(redist_merit, na.rm=TRUE)),
               by = below_negatively_surprised])

# Within-treatment regressions (no controls — cell sizes too small for controls)
ms_el_above <- lm(effort_luck  ~ above_positively_surprised, data = dt_above_eff)
ms_el_below <- lm(effort_luck  ~ below_negatively_surprised, data = dt_below_eff)
ms_rd_above <- lm(redist_merit ~ above_positively_surprised, data = dt_above)
ms_rd_below <- lm(redist_merit ~ below_negatively_surprised, data = dt_below)

cat("\n=== Within above: effort_luck ~ pos_surprised ===\n"); print(ctest(ms_el_above))
cat("\n=== Within below: effort_luck ~ neg_surprised ===\n"); print(ctest(ms_el_below))
cat("\n=== Within above: redist_merit ~ pos_surprised ===\n"); print(ctest(ms_rd_above))
cat("\n=== Within below: redist_merit ~ neg_surprised ===\n"); print(ctest(ms_rd_below))

# =====================================================================
# 5c. FULL-SAMPLE FEEDBACK-TYPE REGRESSION
#     All 5 mutually exclusive categories in one model.
#     Baseline: negatively surprised (told below, predicted above)
# =====================================================================

cat("\n--- feedback_type distribution ---\n")
print(table(dt$feedback_type, useNA = "ifany"))

ms_ft_el <- lm(effort_luck  ~ feedback_type, data = dt[!is.na(effort_luck) & !is.na(feedback_type)])
ms_ft_rd <- lm(redist_merit ~ feedback_type, data = dt[!is.na(feedback_type)])
cat("\n=== Full-sample feedback_type → effort_luck ===\n");  print(ctest(ms_ft_el))
cat("\n=== Full-sample feedback_type → redist_merit ===\n"); print(ctest(ms_ft_rd))

# Table 4: Within-treatment surprise + full-sample feedback-type effects
coef_map_surpr <- c(
  "(Intercept)"                    = "Constant",
  "above_positively_surprised"     = "Pos.\\ surprised (within above arm)",
  "below_negatively_surprised"     = "Neg.\\ surprised (within below arm)",
  "feedback_typeneg_reassured"     = "Negatively reassured",
  "feedback_typepos_surprised"     = "Positively surprised",
  "feedback_typepos_reassured"     = "Positively reassured",
  "feedback_typenf_optimistic"     = "No feedback -- optimistic",
  "feedback_typenf_pessimistic"    = "No feedback -- pessimistic"
)

# USD Redistributed first (cols 1-3), Effort Beliefs second (cols 4-6)
models_surpr <- list(
  "(1)" = ms_rd_above,
  "(2)" = ms_rd_below,
  "(3)" = ms_ft_rd,
  "(4)" = ms_el_above,
  "(5)" = ms_el_below,
  "(6)" = ms_ft_el
)

modelsummary(
  models_surpr,
  vcov     = lapply(models_surpr, hc3),
  coef_map = coef_map_surpr,
  gof_map  = list(
    list(raw = "nobs",      clean = "$N$",     fmt = 0),
    list(raw = "r.squared", clean = "$R^{2}$", fmt = 3)
  ),
  stars    = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  output   = "table4.tex",
  title    = "Surprise Effects: Within-Arm and Full-Sample Comparisons \\label{tab:surprise}",
  notes    = paste0(
    "\\textit{Notes.} OLS with HC3-robust SEs in parentheses. ",
    "Cols.~(1) and (4): above group only ($N = 47$); positively surprised = told above but predicted below ($n_{\\text{surp}} = 5$). ",
    "Cols.~(2) and (5): below group only ($N = 48$); negatively surprised = told below but predicted above ($n_{\\text{surp}} = 37$). ",
    "Constant in cols.~(1)--(2) and (4)--(5) = mean of the not-surprised sub-group within each arm. ",
    "Cols.~(3) and (6): full sample ($N = 145$); baseline = negatively surprised. ",
    "Negatively reassured = told below, predicted below ($n = 11$); ",
    "positively surprised = told above, predicted below ($n = 5$); ",
    "positively reassured = told above, predicted above ($n = 42$); ",
    "no feedback -- optimistic = no feedback, predicted above ($n = 43$); ",
    "no feedback -- pessimistic = no feedback, predicted below ($n = 7$). ",
    "Constant in cols.~(3) and (6) = mean of the negatively surprised group. ",
    "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
  ),
  escape = FALSE
)
cat("Saved: table4.tex\n")

# Post-process table4.tex: replace the single column-number header row with a
# two-row spanning header: Row 1 = outcome group labels, Row 2 = (1)...(6)
tex4 <- readLines("table4.tex")
hdr_idx <- grep("^& \\(1\\)", tex4)
tex4[hdr_idx] <- paste0(
  "& \\SetCell[c=3]{c} USD Redistributed & & ",
  "& \\SetCell[c=3]{c} Effort Beliefs & & \\\\\n",
  "& (1) & (2) & (3) & (4) & (5) & (6) \\\\"
)
# Shift hline row numbers (one extra row added at the top)
tex4 <- gsub("hline\\{18\\}", "hline{19}", tex4)
tex4 <- gsub("hline\\{20\\}", "hline{21}", tex4)
tex4 <- gsub("hline\\{2\\}=",  "hline{3}=",  tex4)
# Add partial underlines under each spanning label (cmidrule-style, skips row-label col)
tex4 <- gsub(
  "hline\\{3\\}=\\{1-7\\}\\{solid, black, 0\\.05em\\}",
  paste0("hline{2}={2-4}{solid, black, 0.05em},\n",
         "hline{2}={5-7}{solid, black, 0.05em},\n",
         "hline{3}={1-7}{solid, black, 0.05em}"),
  tex4
)
writeLines(tex4, "table4.tex")
cat("Post-processed: table4.tex (two-level spanning header)\n")

# =====================================================================
# 6.  HETEROGENEITY: treatment x score interaction
#     Test whether the above-group effect is stronger for
#     higher-scoring participants (who "deserve" the signal more)
# =====================================================================

dt[, score_c := scale(score)]   # center for interpretability

m11 <- lm(redist_merit ~ treatment * score_c, data = dt)
cat("\n=== Treatment x score interaction ===\n"); print(ctest(m11))

# =====================================================================
# 7.  ROBUSTNESS: log outcome
# =====================================================================

m_log <- lm(log_redist_merit ~ treatment, data = dt)
cat("\n=== Robustness: log(redist_merit + 1) ===\n"); print(ctest(m_log))

# =====================================================================
# 8.  TABLE 1: Balance / Descriptive Statistics
#     Column order: Told Above | No Feedback | Told Below | p-value
# =====================================================================

vars   <- c("redist_merit", "effort_luck", "score", "age", "female", "colombian")
labels <- c(
  "Redistribution -- merit (\\$)",
  "Effort--luck belief (0--10)",
  "Task score",
  "Age",
  "Female (\\%)",
  "Colombian (\\%)"
)
pct_vars <- c("female", "colombian", "pred_better", "edu_lt_ba", "edu_ba", "edu_ma_up")

fmt_cell <- function(x, pct = FALSE) {
  m <- mean(x, na.rm = TRUE) * if (pct) 100 else 1
  s <- sd(x,   na.rm = TRUE) * if (pct) 100 else 1
  if (is.nan(m) || is.na(m)) return("--")
  sprintf("%.2f (%.2f)", m, s)
}

tbl1 <- rbindlist(lapply(seq_along(vars), function(i) {
  v   <- vars[i]
  pct <- v %in% pct_vars
  x   <- dt[[v]]
  tr  <- dt$treatment
  pv  <- tryCatch({
    if (pct) chisq.test(table(x, tr))$p.value
    else     summary(aov(x ~ tr))[[1]][["Pr(>F)"]][1]
  }, error = function(e) NA_real_)
  data.table(
    Variable      = labels[i],
    `Told: Above` = fmt_cell(dt[treatment == "above"][[v]],       pct),
    `No Feedback` = fmt_cell(dt[treatment == "no_feedback"][[v]], pct),
    `Told: Below` = fmt_cell(dt[treatment == "below"][[v]],       pct),
    `$p$-value`   = ifelse(is.na(pv), "--", sprintf("%.3f", pv))
  )
}))

# Education group: header row with chi-sq p-value on edu_3, then 3 sub-rows
p_edu3 <- tryCatch(
  chisq.test(table(dt$edu_3, dt$treatment))$p.value,
  error = function(e) NA_real_
)
edu_vars   <- c("edu_lt_ba", "edu_ba", "edu_ma_up")
edu_labels <- c("\\quad Less than bachelor", "\\quad Bachelor's", "\\quad Master's or above")

tbl1 <- rbind(tbl1,
  data.table(
    Variable      = "Education (\\%)",
    `Told: Above` = "", `No Feedback` = "", `Told: Below` = "",
    `$p$-value`   = ifelse(is.na(p_edu3), "--", sprintf("%.3f", p_edu3))
  ),
  rbindlist(lapply(seq_along(edu_vars), function(i) {
    v <- edu_vars[i]
    data.table(
      Variable      = edu_labels[i],
      `Told: Above` = fmt_cell(dt[treatment == "above"][[v]],       TRUE),
      `No Feedback` = fmt_cell(dt[treatment == "no_feedback"][[v]], TRUE),
      `Told: Below` = fmt_cell(dt[treatment == "below"][[v]],       TRUE),
      `$p$-value`   = ""
    )
  }))
)

tbl1 <- rbind(tbl1, data.table(
  Variable      = "$N$",
  `Told: Above` = as.character(nrow(dt[treatment == "above"])),
  `No Feedback` = as.character(nrow(dt[treatment == "no_feedback"])),
  `Told: Below` = as.character(nrow(dt[treatment == "below"])),
  `$p$-value`   = as.character(nrow(dt))
))

tbl1_tex <- kable(
  tbl1, format = "latex", booktabs = TRUE, escape = FALSE,
  caption = "Descriptive Statistics by Treatment Group \\label{tab:balance}",
  col.names = c("", "Told: Above", "No Feedback", "Told: Below", "$p$-value")
) |>
  kable_styling(latex_options = "hold_position") |>
  add_footnote(
    "Mean (SD) for continuous variables; mean $\\times$ 100 (SD $\\times$ 100) for binary. $p$-values from one-way ANOVA (continuous) or chi-squared test (binary). Redistribution in USD.",
    notation = "none", escape = FALSE
  )
writeLines(tbl1_tex, "table1.tex")
cat("Saved: table1.tex\n")

# =====================================================================
# 9.  TABLE 2: Main treatment effects
#     Cols (1)-(2): effort_luck; (3)-(4): redist_merit
# =====================================================================

coef_map_main <- c(
  "(Intercept)"                  = "Constant",
  "treatmentno_feedback"         = "No Feedback",
  "treatmentbelow"               = "Told: Below",
  "age"                          = "Age",
  "femaleTRUE"                   = "Female",
  "colombianTRUE"                = "Colombian",
  "score"                        = "Task score",
  "edu_3Bachelor"                = "Bachelor's degree",
  "edu_3Master or above"         = "Master's or above"
)

models_main <- list(
  "(1) Effort belief"                       = m1,
  "(2) Effort belief\\phantom{x}"           = m2,
  "(3) Merit redistrib.~(\\$)"              = m3,
  "(4) Merit redistrib.~(\\$)\\phantom{x}"  = m4
)

ctrl_row_main <- as.data.frame(as.list(setNames(
  c("Demographic controls", "No", "Yes", "No", "Yes"),
  c("term", names(models_main))
)))

modelsummary(
  models_main,
  vcov     = lapply(models_main, hc3),
  coef_map = coef_map_main,
  gof_map  = list(
    list(raw = "nobs",      clean = "$N$",     fmt = 0),
    list(raw = "r.squared", clean = "$R^{2}$", fmt = 3)
  ),
  stars    = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  add_rows = ctrl_row_main,
  output   = "table2.tex",
  title    = "Treatment Effects on Effort Attribution and Redistribution \\label{tab:main}",
  notes    = paste0(
    "\\textit{Notes.} OLS with HC3-robust SEs in parentheses. ",
    "Baseline: Told Above. ",
    "Cols.~(1)--(2): effort--luck belief (0 = luck, 10 = effort). ",
    "Cols.~(3)--(4): USD redistributed to lower-earning worker (merit scenario). ",
    "Controls: age, female indicator, Colombian indicator, task score, and education (Less than bachelor = baseline). ",
    "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
  ),
  escape = FALSE
)
cat("Saved: table2.tex\n")

# =====================================================================
# 10. TABLE 3: Mechanism and surprise heterogeneity
#     Cols (1)-(2): merit redist + effort_luck (mediation test)
#     Cols (3)-(4): surprise indicators within relative-feedback group
# =====================================================================

coef_map_mech <- c(
  "(Intercept)"                = "Constant",
  "treatmentno_feedback"       = "No Feedback",
  "treatmentbelow"             = "Told: Below",
  "effort_luck"                = "Effort--luck belief",
  "above_positively_surprised" = "Positively surprised",
  "below_negatively_surprised" = "Negatively surprised",
  "age"                        = "Age",
  "femaleTRUE"                 = "Female",
  "colombianTRUE"              = "Colombian",
  "score"                      = "Task score",
  "edu_3Bachelor"              = "Bachelor's degree",
  "edu_3Master or above"       = "Master's or above"
)

models_mech <- list(
  "(1) Merit redistrib.~(\\$)"              = m9,
  "(2) Merit redistrib.~(\\$)\\phantom{x}"  = m10
)
ctrl_row_mech <- as.data.frame(as.list(setNames(
  c("Demographic controls", "No", "Yes"),
  c("term", names(models_mech))
)))
tbl3_notes <- paste0(
  "\\textit{Notes.} OLS with HC3-robust SEs in parentheses. ",
  "Dependent variable: USD redistributed (merit scenario). ",
  "Includes effort--luck belief as control to assess mediation; ",
  "compare treatment coefficients with Table~\\ref{tab:main} cols.~(3)--(4). ",
  "Baseline: Told Above. ",
  "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
)

modelsummary(
  models_mech,
  vcov     = lapply(models_mech, hc3),
  coef_map = coef_map_mech,
  gof_map  = list(
    list(raw = "nobs",      clean = "$N$",     fmt = 0),
    list(raw = "r.squared", clean = "$R^{2}$", fmt = 3)
  ),
  stars    = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  add_rows = ctrl_row_mech,
  output   = "table3.tex",
  title    = "Mediation Analysis \\label{tab:mechanism}",
  notes    = tbl3_notes,
  escape   = FALSE
)
cat("Saved: table3.tex\n")

# =====================================================================
# 11. FIGURES
# =====================================================================

group_colors <- c("above" = "#7AB648", "no_feedback" = "#E07B6A", "below" = "#6BAED4")
group_labels <- c("above" = "Told:\nAbove", "no_feedback" = "No\nFeedback", "below" = "Told:\nBelow")
group_order  <- c("above", "no_feedback", "below")

# ── Figure 2: effort attribution + merit redistribution by treatment ──

sum_effort <- dt[!is.na(treatment) & !is.na(effort_luck), .(
  mean = mean(effort_luck),
  se   = sd(effort_luck) / sqrt(.N)
), by = treatment][, treatment := factor(treatment, levels = group_order)]

sum_merit <- dt[!is.na(treatment) & !is.na(redist_merit), .(
  mean = mean(redist_merit),
  se   = sd(redist_merit) / sqrt(.N)
), by = treatment][, treatment := factor(treatment, levels = group_order)]

make_bar <- function(data, title, ylab) {
  ymax <- ceiling(max(data$mean + data$se, na.rm = TRUE) * 1.25)
  ggplot(data, aes(x = treatment, y = mean, fill = treatment)) +
    geom_col(width = 0.55) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                  width = 0.12, linewidth = 0.8) +
    geom_text(aes(y = ymax * 0.04, label = round(mean, 2)),
              color = "white", fontface = "bold", size = 4.5) +
    scale_fill_manual(values = group_colors, guide = "none") +
    scale_x_discrete(labels = group_labels) +
    scale_y_continuous(limits = c(0, ymax), breaks = pretty(c(0, ymax), n = 6)) +
    labs(title = title, x = NULL, y = ylab) +
    theme_classic(base_size = 13) +
    theme(
      aspect.ratio = 0.65,
      plot.title   = element_text(face = "bold", hjust = 0.5),
      axis.text.x  = element_text(color = "black", size = 11)
    )
}

fig2 <- make_bar(sum_merit,  "Merit Redistribution by Treatment",
                 "USD to Worker B") +
        make_bar(sum_effort, "Effort Attribution by Treatment",
                 "Effort-luck belief (0=luck, 10=effort)") +
        plot_annotation(
          caption = sprintf("Notes. Means ± 1 SE. N = %d.", nrow(dt))
        )
ggsave("figure2.pdf", fig2, width = 10, height = 5, device = cairo_pdf)
cat("Saved: figure2.pdf\n")

# ── Figure 3: Coefficient plot (OLS point estimates + 95% CI) ──
# Shows the two treatment effects relative to the Told-Above baseline.

build_coef_df <- function(model, dep_var_label) {
  ct   <- coeftest(model, vcov = hc3(model))
  keep <- c("treatmentno_feedback", "treatmentbelow")
  df   <- as.data.frame(ct[keep, , drop = FALSE])
  setDT(df, keep.rownames = "term")
  setnames(df, c("Estimate","Std. Error","t value","Pr(>|t|)"),
               c("est", "se", "t", "p"))
  df[, `:=`(
    ci_lo    = est - 1.96 * se,
    ci_hi    = est + 1.96 * se,
    label    = c("No Feedback", "Told: Below"),
    outcome  = dep_var_label
  )]
  df
}

coef_df <- rbindlist(list(
  build_coef_df(m1, "Effort attribution (0--10)"),
  build_coef_df(m3, "Merit redistribution (USD)")
))

fig3 <- ggplot(coef_df, aes(x = label, y = est, color = outcome)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi),
                width = 0.12, linewidth = 0.9,
                position = position_dodge(width = 0.4)) +
  geom_point(size = 3.5,
             position = position_dodge(width = 0.4)) +
  scale_color_manual(values = c("#2C7BB6", "#D7191C"), name = "Outcome") +
  labs(
    title   = "Treatment Effects Relative to Told: Above",
    x       = NULL,
    y       = "Coefficient vs. Told: Above (95% CI)",
    caption = "OLS, no controls. HC3 standard errors."
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title      = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom"
  )
ggsave("figure3.pdf", fig3, width = 7, height = 5, device = cairo_pdf)
cat("Saved: figure3.pdf\n")

# ── Figure 4: Mechanism scatter — effort_luck vs redist_merit ──

fig4 <- ggplot(dt_eff[!is.na(redist_merit)],
               aes(x = effort_luck, y = redist_merit, color = treatment)) +
  geom_jitter(alpha = 0.45, width = 0.15, height = 0.04, size = 1.8) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1, alpha = 0.15) +
  scale_color_manual(values = group_colors,
                     labels = c("Told: Above", "No Feedback", "Told: Below"),
                     name   = NULL) +
  scale_x_continuous(breaks = 0:10) +
  labs(
    title   = "Mechanism: Effort Attribution and Merit Redistribution",
    x       = "Effort-luck belief (0 = luck, 10 = effort)",
    y       = "USD redistributed (merit scenario)",
    caption = "Shaded bands: 95% CI from linear fit."
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title      = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom"
  )
ggsave("figure4.pdf", fig4, width = 8, height = 6, device = cairo_pdf)
cat("Saved: figure4.pdf\n")

# ── Figure 5: Pre-feedback prediction distribution by treatment ──
# Shows the share of participants who predicted they would outperform their peer,
# by treatment group. Illustrates the near-universal overconfidence pattern.

pred_summary <- dt[!is.na(pred_better), .(
  pct_better = mean(pred_better) * 100,
  se_pct     = sqrt(mean(pred_better) * (1 - mean(pred_better)) / .N) * 100,
  n          = .N
), by = treatment][, treatment := factor(treatment, levels = group_order)]

fig5 <- ggplot(pred_summary, aes(x = treatment, y = pct_better, fill = treatment)) +
  geom_col(width = 0.55) +
  geom_errorbar(aes(ymin = pct_better - se_pct, ymax = pct_better + se_pct),
                width = 0.12, linewidth = 0.8) +
  geom_text(aes(y = 4, label = sprintf("%.0f%%", pct_better)),
            color = "white", fontface = "bold", size = 4.5) +
  scale_fill_manual(values = group_colors, guide = "none") +
  scale_x_discrete(labels = group_labels) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20),
                     labels = function(x) paste0(x, "%")) +
  labs(title   = "Share Predicting Above-Average Performance (Pre-Feedback)",
       x       = NULL,
       y       = "% predicting to outperform matched peer",
       caption = sprintf("Notes. Proportion ± 1 SE. N = %d.", sum(pred_summary$n))) +
  theme_classic(base_size = 13) +
  theme(
    aspect.ratio = 0.65,
    plot.title   = element_text(face = "bold", hjust = 0.5),
    axis.text.x  = element_text(color = "black", size = 11)
  )
ggsave("figure5.pdf", fig5, width = 6, height = 4.5, device = cairo_pdf)
cat("Saved: figure5.pdf\n")

cat("\nDone. Outputs: table1.tex, table2.tex, table3.tex,",
    "figure2.pdf, figure3.pdf, figure4.pdf, figure5.pdf\n")


table(dt$country)
