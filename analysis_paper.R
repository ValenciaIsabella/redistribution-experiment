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

lines  <- readLines("C:\\Users\\User\\Downloads\\Final_Results.txt")
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

# Exclusions
dt <- dt[!(score == 0 & n_correct == 0 & n_wrong == 0)]
dt[, c("n_correct", "n_wrong") := NULL]
dt <- dt[redist_merit < 5]
dt[is.na(effort_luck), effort_luck := 5]
dt <- dt[score > 16 & score < 200]   # exclude 3 low outliers (score ≤ 16) and 1 high outlier (score = 40)


hh <- dt[ pred_better == FALSE & told_above == FALSE, ]
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
tex4 <- gsub("(\\\\begin\\{talltblr\\})", "\\\\small\n\\1", tex4)
writeLines(tex4, "table4.tex")
cat("Post-processed: table4.tex (two-level spanning header, \\small)\n")

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

vars   <- c("score", "age", "female", "colombian", "pred_better")
labels <- c(
  "Task score",
  "Age",
  "Female (\\%)",
  "Colombian (\\%)",
  "Predicted above average (\\%)"
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
    "Mean (SD) for continuous variables; mean $\\times$ 100 (SD $\\times$ 100) for binary. $p$-values from one-way ANOVA (continuous) or chi-squared test (binary). Predicted above average: share expecting to outperform matched peer, elicited before feedback.",
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
  "(1) Merit redistrib.~(\\$)"              = m3,
  "(2) Merit redistrib.~(\\$)\\phantom{x}"  = m4,
  "(3) Effort belief"                       = m1,
  "(4) Effort belief\\phantom{x}"           = m2
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
    "Cols.~(1)--(2): USD redistributed to lower-earning worker (merit scenario). ",
    "Cols.~(3)--(4): effort--luck belief (0 = luck, 10 = effort). ",
    "Controls: age, female indicator, Colombian indicator, task score, and education (Less than bachelor = baseline). ",
    "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
  ),
  escape = FALSE
)
cat("Saved: table2.tex\n")

# Post-process table2.tex: two-level spanning header + \small for page fit
tex2 <- readLines("table2.tex")
hdr_idx2 <- grep("^& \\(1\\)", tex2)
tex2[hdr_idx2] <- paste0(
  "& \\SetCell[c=2]{c} USD Redistributed & & \\SetCell[c=2]{c} Effort Beliefs & \\\\\n",
  "& (1) & (2) & (3) & (4) \\\\"
)
tex2 <- gsub("hline\\{20\\}", "hline{21}", tex2)
tex2 <- gsub("hline\\{23\\}", "hline{24}", tex2)
tex2 <- gsub(
  "hline\\{2\\}=\\{1-5\\}\\{solid, black, 0\\.05em\\}",
  paste0("hline{2}={2-3}{solid, black, 0.05em},\n",
         "hline{2}={4-5}{solid, black, 0.05em},\n",
         "hline{3}={1-5}{solid, black, 0.05em}"),
  tex2
)
tex2 <- gsub("(\\\\begin\\{talltblr\\})", "\\\\small\n\\1", tex2)
writeLines(tex2, "table2.tex")
cat("Post-processed: table2.tex (two-level header, \\small)\n")

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
group_order  <- c("above", "no_feedback", "below")

# ── Figure 2: merit redistribution + effort attribution by treatment ──

sum_merit <- dt[!is.na(treatment) & !is.na(redist_merit), .(
  n    = .N,
  mean = mean(redist_merit),
  se   = sd(redist_merit) / sqrt(.N)
), by = treatment][, treatment := factor(treatment, levels = group_order)]

sum_effort <- dt[!is.na(treatment) & !is.na(effort_luck), .(
  n    = .N,
  mean = mean(effort_luck),
  se   = sd(effort_luck) / sqrt(.N)
), by = treatment][, treatment := factor(treatment, levels = group_order)]

# Helpers shared with figure 2
fmt_p2 <- function(p) {
  if (p < 0.001) "(p<0.001)" else sprintf("(%.3f)", p)
}

stars_str2 <- function(p) {
  if      (p < 0.01) "***"
  else if (p < 0.05) "**"
  else if (p < 0.10) "*"
  else               ""
}

pw_tests2 <- function(dt_sub, var) {
  pairs <- list(c("above", "no_feedback"),
                c("no_feedback", "below"),
                c("above", "below"))
  lapply(pairs, function(p) {
    y1 <- dt_sub[treatment == p[1], get(var)]
    y2 <- dt_sub[treatment == p[2], get(var)]
    tt <- t.test(y1, y2)
    list(g1 = p[1], g2 = p[2], diff = mean(y1) - mean(y2), p = tt$p.value)
  })
}

xp2 <- setNames(1:3, group_order)

add_bracket2 <- function(plt, x1, x2, y, label, ymax_ext) {
  tick <- ymax_ext * 0.025
  plt +
    annotate("segment", x = x1, xend = x1,
             y = y - tick, yend = y, linewidth = 0.35, color = "grey30") +
    annotate("segment", x = x1, xend = x2,
             y = y,       yend = y, linewidth = 0.35, color = "grey30") +
    annotate("segment", x = x2, xend = x2,
             y = y, yend = y - tick, linewidth = 0.35, color = "grey30") +
    annotate("text", x = (x1 + x2) / 2, y = y + ymax_ext * 0.018,
             label = label, size = 2.7, hjust = 0.5, vjust = 0, color = "grey20")
}

make_bar <- function(sdata, dt_sub, var, title, ylab) {
  ymax_base <- ceiling(max(sdata$mean + sdata$se, na.rm = TRUE) * 1.25)
  err_top   <- max(sdata$mean + 1.96 * sdata$se, na.rm = TRUE)
  ymax_ext  <- err_top * 1.58
  n_map     <- setNames(sdata$n, as.character(sdata$treatment))
  xlabels   <- setNames(
    paste0(c("Told:\nAbove", "No\nFeedback", "Told:\nBelow"),
           "\n(n=", n_map[group_order], ")"),
    group_order
  )
  heights <- c(err_top * 1.08, err_top * 1.22, err_top * 1.38)
  tests   <- pw_tests2(dt_sub, var)

  plt <- ggplot(sdata, aes(x = treatment, y = mean, fill = treatment)) +
    geom_col(width = 0.55) +
    geom_errorbar(aes(ymin = mean - 1.96 * se, ymax = mean + 1.96 * se),
                  width = 0.12, linewidth = 0.8) +
    geom_text(aes(y = ymax_ext * 0.035, label = round(mean, 2)),
              color = "white", fontface = "bold", size = 4.5) +
    scale_fill_manual(values = group_colors, guide = "none") +
    scale_x_discrete(labels = xlabels) +
    scale_y_continuous(limits = c(0, ymax_ext),
                       breaks = pretty(c(0, ymax_base), n = 6)) +
    labs(title = title, x = NULL, y = ylab) +
    theme_classic(base_size = 13) +
    theme(
      aspect.ratio = 0.80,
      plot.title   = element_text(face = "bold", hjust = 0.5),
      axis.text.x  = element_text(color = "black", size = 10)
    )

  for (i in seq_along(tests)) {
    t   <- tests[[i]]
    s   <- stars_str2(t$p)
    lbl <- paste0(sprintf("Δ=%.2f", t$diff), s, "\n", fmt_p2(t$p))
    plt <- add_bracket2(plt, xp2[t$g1], xp2[t$g2], heights[i], lbl, ymax_ext)
  }
  plt
}

fig2 <- make_bar(sum_merit,  dt[!is.na(treatment) & !is.na(redist_merit)],
                 "redist_merit",
                 "Merit Redistribution by Treatment",
                 "USD to Worker B") +
        make_bar(sum_effort, dt[!is.na(treatment) & !is.na(effort_luck)],
                 "effort_luck",
                 "Effort Attribution by Treatment",
                 "Effort-luck belief (0=luck, 10=effort)") +
        plot_annotation(caption = NULL)
ggsave("figure2.pdf", fig2, width = 10, height = 6, device = cairo_pdf)
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
  n    = .N,
  mean = mean(as.numeric(pred_better)) * 100,
  se   = sqrt(mean(as.numeric(pred_better)) * (1 - mean(as.numeric(pred_better))) / .N) * 100
), by = treatment][, treatment := factor(treatment, levels = group_order)]

ymax_base5 <- 100
err_top5   <- max(pred_summary$mean + 1.96 * pred_summary$se, na.rm = TRUE)
ymax_ext5  <- err_top5 * 1.58
n_map5     <- setNames(pred_summary$n, as.character(pred_summary$treatment))
xlabels5   <- setNames(
  paste0(c("Told:\nAbove", "No\nFeedback", "Told:\nBelow"),
         "\n(n=", n_map5[group_order], ")"),
  group_order
)
heights5 <- c(err_top5 * 1.08, err_top5 * 1.22, err_top5 * 1.38)
tests5   <- lapply(
  list(c("above", "no_feedback"), c("no_feedback", "below"), c("above", "below")),
  function(p) {
    y1 <- as.numeric(dt[treatment == p[1] & !is.na(pred_better), pred_better])
    y2 <- as.numeric(dt[treatment == p[2] & !is.na(pred_better), pred_better])
    tt <- t.test(y1, y2)
    list(g1 = p[1], g2 = p[2], diff = (mean(y1) - mean(y2)) * 100, p = tt$p.value)
  }
)

fig5 <- ggplot(pred_summary, aes(x = treatment, y = mean, fill = treatment)) +
  geom_col(width = 0.55) +
  geom_errorbar(aes(ymin = mean - 1.96 * se, ymax = mean + 1.96 * se),
                width = 0.12, linewidth = 0.8) +
  geom_text(aes(y = ymax_ext5 * 0.035, label = sprintf("%.0f%%", mean)),
            color = "white", fontface = "bold", size = 4.5) +
  scale_fill_manual(values = group_colors, guide = "none") +
  scale_x_discrete(labels = xlabels5) +
  scale_y_continuous(limits = c(0, ymax_ext5),
                     breaks = seq(0, 100, 20),
                     labels = function(x) paste0(x, "%")) +
  labs(title   = "Share Predicting Above-Average Performance",
       x       = NULL,
       y       = "% predicting to outperform matched peer",
       caption = NULL) +
  theme_classic(base_size = 13) +
  theme(
    aspect.ratio = 0.65,
    plot.title   = element_text(face = "bold", hjust = 0.5),
    axis.text.x  = element_text(color = "black", size = 10)
  )

for (i in seq_along(tests5)) {
  t5  <- tests5[[i]]
  s5  <- stars_str2(t5$p)
  lbl <- paste0(sprintf("Δ=%.1f%%", t5$diff), s5, "\n", fmt_p2(t5$p))
  fig5 <- add_bracket2(fig5, xp2[t5$g1], xp2[t5$g2], heights5[i], lbl, ymax_ext5)
}

ggsave("figure5.pdf", fig5, width = 6, height = 5.5, device = cairo_pdf)
cat("Saved: figure5.pdf\n")

cat("\nDone. Outputs: table1.tex, table2.tex, table3.tex,",
    "figure2.pdf, figure3.pdf, figure4.pdf, figure5.pdf\n")


# =====================================================================
# 12.  APPENDIX TABLES: same as 2--4 with baseline = No Feedback
# =====================================================================

dt[,     treatment_nfb := relevel(treatment, ref = "no_feedback")]
dt_eff[, treatment_nfb := relevel(treatment, ref = "no_feedback")]

# ── Appendix Table A2: main effects (baseline = No Feedback) ──
ma1 <- lm(effort_luck  ~ treatment_nfb,
          data = dt_eff)
ma2 <- lm(effort_luck  ~ treatment_nfb + age + female + score + colombian + edu_3,
          data = dt_eff)
ma3 <- lm(redist_merit ~ treatment_nfb,
          data = dt)
ma4 <- lm(redist_merit ~ treatment_nfb + age + female + score + colombian + edu_3,
          data = dt)

coef_map_main_nfb <- c(
  "(Intercept)"          = "Constant",
  "treatment_nfbabove"   = "Told: Above",
  "treatment_nfbbelow"   = "Told: Below",
  "age"                  = "Age",
  "femaleTRUE"           = "Female",
  "colombianTRUE"        = "Colombian",
  "score"                = "Task score",
  "edu_3Bachelor"        = "Bachelor's degree",
  "edu_3Master or above" = "Master's or above"
)
models_main_nfb <- list(
  "(1) Merit redistrib.~(\\$)"             = ma3,
  "(2) Merit redistrib.~(\\$)\\phantom{x}" = ma4,
  "(3) Effort belief"                      = ma1,
  "(4) Effort belief\\phantom{x}"          = ma2
)
ctrl_row_main_nfb <- as.data.frame(as.list(setNames(
  c("Demographic controls", "No", "Yes", "No", "Yes"),
  c("term", names(models_main_nfb))
)))
modelsummary(
  models_main_nfb,
  vcov     = lapply(models_main_nfb, hc3),
  coef_map = coef_map_main_nfb,
  gof_map  = list(
    list(raw = "nobs",      clean = "$N$",     fmt = 0),
    list(raw = "r.squared", clean = "$R^{2}$", fmt = 3)
  ),
  stars    = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  add_rows = ctrl_row_main_nfb,
  output   = "tableA2.tex",
  title    = paste0("Treatment Effects on Effort Attribution and Redistribution ",
                    "(Baseline: No Feedback) \\label{tab:main_nfb}"),
  notes    = paste0(
    "\\textit{Notes.} OLS with HC3-robust SEs in parentheses. ",
    "Baseline: No Feedback. ",
    "Cols.~(1)--(2): USD redistributed to lower-earning worker (merit scenario). ",
    "Cols.~(3)--(4): effort--luck belief (0 = luck, 10 = effort). ",
    "Controls: age, female indicator, Colombian indicator, task score, ",
    "and education (Less than bachelor = baseline). ",
    "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
  ),
  escape = FALSE
)
cat("Saved: tableA2.tex\n")

# Post-process tableA2.tex: two-level header + \small
texA2 <- readLines("tableA2.tex")
hdr_A2 <- grep("^& \\(1\\)", texA2)
texA2[hdr_A2] <- paste0(
  "& \\SetCell[c=2]{c} USD Redistributed & ",
  "& \\SetCell[c=2]{c} Effort Beliefs & \\\\\n",
  "& (1) & (2) & (3) & (4) \\\\"
)
texA2 <- gsub("hline\\{20\\}", "hline{21}", texA2)
texA2 <- gsub("hline\\{23\\}", "hline{24}", texA2)
texA2 <- gsub(
  "hline\\{2\\}=\\{1-5\\}\\{solid, black, 0\\.05em\\}",
  paste0("hline{2}={2-3}{solid, black, 0.05em},\n",
         "hline{2}={4-5}{solid, black, 0.05em},\n",
         "hline{3}={1-5}{solid, black, 0.05em}"),
  texA2
)
texA2 <- gsub("(\\\\begin\\{talltblr\\})", "\\\\small\n\\1", texA2)
writeLines(texA2, "tableA2.tex")
cat("Post-processed: tableA2.tex\n")

# ── Appendix Table A3: mediation (baseline = No Feedback) ──
ma9  <- lm(redist_merit ~ treatment_nfb + effort_luck,
           data = dt_eff)
ma10 <- lm(redist_merit ~ treatment_nfb + effort_luck
           + age + female + score + colombian + edu_3,
           data = dt_eff)

coef_map_mech_nfb <- c(
  "(Intercept)"          = "Constant",
  "treatment_nfbabove"   = "Told: Above",
  "treatment_nfbbelow"   = "Told: Below",
  "effort_luck"          = "Effort--luck belief",
  "age"                  = "Age",
  "femaleTRUE"           = "Female",
  "colombianTRUE"        = "Colombian",
  "score"                = "Task score",
  "edu_3Bachelor"        = "Bachelor's degree",
  "edu_3Master or above" = "Master's or above"
)
models_mech_nfb <- list(
  "(1) Merit redistrib.~(\\$)"             = ma9,
  "(2) Merit redistrib.~(\\$)\\phantom{x}" = ma10
)
ctrl_row_mech_nfb <- as.data.frame(as.list(setNames(
  c("Demographic controls", "No", "Yes"),
  c("term", names(models_mech_nfb))
)))
modelsummary(
  models_mech_nfb,
  vcov     = lapply(models_mech_nfb, hc3),
  coef_map = coef_map_mech_nfb,
  gof_map  = list(
    list(raw = "nobs",      clean = "$N$",     fmt = 0),
    list(raw = "r.squared", clean = "$R^{2}$", fmt = 3)
  ),
  stars    = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  add_rows = ctrl_row_mech_nfb,
  output   = "tableA3.tex",
  title    = "Mediation Analysis (Baseline: No Feedback) \\label{tab:mechanism_nfb}",
  notes    = paste0(
    "\\textit{Notes.} OLS with HC3-robust SEs in parentheses. ",
    "Dependent variable: USD redistributed (merit scenario). ",
    "Includes effort--luck belief as control to assess mediation; ",
    "compare treatment coefficients with Table~\\ref{tab:main_nfb} cols.~(1)--(2). ",
    "Baseline: No Feedback. ",
    "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
  ),
  escape = FALSE
)
cat("Saved: tableA3.tex\n")

# Post-process tableA3.tex: \small only
texA3 <- readLines("tableA3.tex")
texA3 <- gsub("(\\\\begin\\{talltblr\\})", "\\\\small\n\\1", texA3)
writeLines(texA3, "tableA3.tex")
cat("Post-processed: tableA3.tex\n")

# ── Appendix Table A4: surprise effects, full-sample baseline = nf_optimistic ──
dt[, feedback_type_nf := factor(
  feedback_type,
  levels = c("nf_optimistic", "nf_pessimistic",
             "neg_surprised",  "neg_reassured",
             "pos_surprised",  "pos_reassured")
)]
ms_ft_el_nf <- lm(
  effort_luck  ~ feedback_type_nf,
  data = dt[!is.na(effort_luck) & !is.na(feedback_type_nf)]
)
ms_ft_rd_nf <- lm(
  redist_merit ~ feedback_type_nf,
  data = dt[!is.na(feedback_type_nf)]
)

coef_map_surpr_nf <- c(
  "(Intercept)"                    = "Constant",
  "above_positively_surprised"     = "Pos.\\ surprised (within above arm)",
  "below_negatively_surprised"     = "Neg.\\ surprised (within below arm)",
  "feedback_type_nfnf_pessimistic" = "No feedback -- pessimistic",
  "feedback_type_nfneg_surprised"  = "Negatively surprised",
  "feedback_type_nfneg_reassured"  = "Negatively reassured",
  "feedback_type_nfpos_surprised"  = "Positively surprised",
  "feedback_type_nfpos_reassured"  = "Positively reassured"
)
models_surpr_nf <- list(
  "(1)" = ms_rd_above,
  "(2)" = ms_rd_below,
  "(3)" = ms_ft_rd_nf,
  "(4)" = ms_el_above,
  "(5)" = ms_el_below,
  "(6)" = ms_ft_el_nf
)
modelsummary(
  models_surpr_nf,
  vcov     = lapply(models_surpr_nf, hc3),
  coef_map = coef_map_surpr_nf,
  gof_map  = list(
    list(raw = "nobs",      clean = "$N$",     fmt = 0),
    list(raw = "r.squared", clean = "$R^{2}$", fmt = 3)
  ),
  stars    = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  output   = "tableA4.tex",
  title    = paste0(
    "Surprise Effects: Within-Arm and Full-Sample Comparisons ",
    "(Baseline: No Feedback -- Optimistic) \\label{tab:surprise_nfb}"
  ),
  notes    = paste0(
    "\\textit{Notes.} OLS with HC3-robust SEs in parentheses. ",
    "Cols.~(1) and (4): above group only ($N = 47$); ",
    "positively surprised = told above but predicted below ($n_{\\text{surp}} = 5$). ",
    "Cols.~(2) and (5): below group only ($N = 48$); ",
    "negatively surprised = told below but predicted above ($n_{\\text{surp}} = 37$). ",
    "Cols.~(3) and (6): full sample ($N = 145$); ",
    "baseline = no feedback -- optimistic (no feedback, predicted above; $n = 43$). ",
    "No feedback -- pessimistic = no feedback, predicted below ($n = 7$); ",
    "negatively surprised = told below, predicted above ($n = 37$); ",
    "negatively reassured = told below, predicted below ($n = 11$); ",
    "positively surprised = told above, predicted below ($n = 5$); ",
    "positively reassured = told above, predicted above ($n = 42$). ",
    "Constant in cols.~(3) and (6) = mean of the no-feedback optimistic group. ",
    "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
  ),
  escape = FALSE
)
cat("Saved: tableA4.tex\n")

# Post-process tableA4.tex: two-level header + \small
texA4 <- readLines("tableA4.tex")
hdr_A4 <- grep("^& \\(1\\)", texA4)
texA4[hdr_A4] <- paste0(
  "& \\SetCell[c=3]{c} USD Redistributed & & ",
  "& \\SetCell[c=3]{c} Effort Beliefs & & \\\\\n",
  "& (1) & (2) & (3) & (4) & (5) & (6) \\\\"
)
texA4 <- gsub("hline\\{18\\}", "hline{19}", texA4)
texA4 <- gsub("hline\\{20\\}", "hline{21}", texA4)
texA4 <- gsub("hline\\{2\\}=",  "hline{3}=",  texA4)
texA4 <- gsub(
  "hline\\{3\\}=\\{1-7\\}\\{solid, black, 0\\.05em\\}",
  paste0("hline{2}={2-4}{solid, black, 0.05em},\n",
         "hline{2}={5-7}{solid, black, 0.05em},\n",
         "hline{3}={1-7}{solid, black, 0.05em}"),
  texA4
)
texA4 <- gsub("(\\\\begin\\{talltblr\\})", "\\\\small\n\\1", texA4)
writeLines(texA4, "tableA4.tex")
cat("Post-processed: tableA4.tex\n")

# ── Appendix Table A5: no-intercept estimation ──
mn1 <- lm(effort_luck  ~ treatment - 1,
          data = dt_eff)
mn2 <- lm(effort_luck  ~ treatment - 1 + age + female + score + colombian + edu_3,
          data = dt_eff)
mn3 <- lm(redist_merit ~ treatment - 1,
          data = dt)
mn4 <- lm(redist_merit ~ treatment - 1 + age + female + score + colombian + edu_3,
          data = dt)

coef_map_main_nocst <- c(
  "treatmentabove"               = "Told: Above",
  "treatmentno_feedback"         = "No Feedback",
  "treatmentbelow"               = "Told: Below",
  "age"                          = "Age",
  "femaleTRUE"                   = "Female",
  "colombianTRUE"                = "Colombian",
  "score"                        = "Task score",
  "edu_3Bachelor"                = "Bachelor's degree",
  "edu_3Master or above"         = "Master's or above"
)
models_main_nocst <- list(
  "(1) Merit redistrib.~(\\$)"             = mn3,
  "(2) Merit redistrib.~(\\$)\\phantom{x}" = mn4,
  "(3) Effort belief"                      = mn1,
  "(4) Effort belief\\phantom{x}"          = mn2
)
ctrl_row_nocst <- as.data.frame(as.list(setNames(
  c("Demographic controls", "No", "Yes", "No", "Yes"),
  c("term", names(models_main_nocst))
)))
modelsummary(
  models_main_nocst,
  vcov     = lapply(models_main_nocst, hc3),
  coef_map = coef_map_main_nocst,
  gof_map  = list(
    list(raw = "nobs",      clean = "$N$",     fmt = 0),
    list(raw = "r.squared", clean = "$R^{2}$", fmt = 3)
  ),
  stars    = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  add_rows = ctrl_row_nocst,
  output   = "tableA5.tex",
  title    = paste0(
    "Treatment Effects on Effort Attribution and Redistribution ",
    "(No Intercept) \\label{tab:main_nocst}"
  ),
  notes    = paste0(
    "\\textit{Notes.} OLS without intercept (\\texttt{-1}) with HC3-robust SEs in parentheses. ",
    "Each treatment coefficient equals the conditional group mean. ",
    "Cols.~(1)--(2): USD redistributed to lower-earning worker (merit scenario). ",
    "Cols.~(3)--(4): effort--luck belief (0 = luck, 10 = effort). ",
    "Controls: age, female indicator, Colombian indicator, task score, ",
    "and education (Less than bachelor = baseline). ",
    "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
  ),
  escape = FALSE
)
cat("Saved: tableA5.tex\n")

# Post-process tableA5.tex: two-level header + \small
texA5 <- readLines("tableA5.tex")
hdr_A5 <- grep("^& \\(1\\)", texA5)
texA5[hdr_A5] <- paste0(
  "& \\SetCell[c=2]{c} USD Redistributed & ",
  "& \\SetCell[c=2]{c} Effort Beliefs & \\\\\n",
  "& (1) & (2) & (3) & (4) \\\\"
)
texA5 <- gsub("hline\\{20\\}", "hline{21}", texA5)
texA5 <- gsub("hline\\{23\\}", "hline{24}", texA5)
texA5 <- gsub(
  "hline\\{2\\}=\\{1-5\\}\\{solid, black, 0\\.05em\\}",
  paste0("hline{2}={2-3}{solid, black, 0.05em},\n",
         "hline{2}={4-5}{solid, black, 0.05em},\n",
         "hline{3}={1-5}{solid, black, 0.05em}"),
  texA5
)
texA5 <- gsub("(\\\\begin\\{talltblr\\})", "\\\\small\n\\1", texA5)
writeLines(texA5, "tableA5.tex")
cat("Post-processed: tableA5.tex\n")

