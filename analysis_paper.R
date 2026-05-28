# =====================================================================
# analysis_paper.R
# Relative Performance Feedback and Redistribution Preferences
# Baseline: Told Above
# =====================================================================

library(data.table)
library(jsonlite)
library(lmtest)
library(sandwich)
install.packages(c("car","ggplot2", "patchwork", "modelsummary", "kableExtra"))
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
  pred_better  = sr_relPerf,
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

if (has_surprise) {
  m7 <- lm(redist_merit ~ above_positively_surprised + below_negatively_surprised,
           data = dt_rel)
  m8 <- lm(redist_merit ~ above_positively_surprised + below_negatively_surprised
           + age + female + score + colombian + edu_3, data = dt_rel)
  cat("\n=== Surprise heterogeneity ===\n"); print(ctest(m7))
} else {
  cat("\nNOTE: pred_better is missing — surprise models skipped.\n")
}

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

ctrl_row_main <- as.data.frame(as.list(setNames(
  c("Controls (age, female, score, Colombian, edu.)", "No", "Yes", "No", "Yes"),
  c("term", "(1)", "(2)", "(3)", "(4)")
)))

models_main <- list(
  "(1)" = m1, "(2)" = m2,
  "(3)" = m3, "(4)" = m4
)

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

if (has_surprise) {
  models_mech   <- list("(1)" = m9, "(2)" = m10, "(3)" = m7, "(4)" = m8)
  ctrl_row_mech <- as.data.frame(as.list(setNames(
    c("Controls (age, female, score, Colombian, edu.)", "No", "Yes", "No", "Yes"),
    c("term", "(1)", "(2)", "(3)", "(4)")
  )))
  tbl3_notes <- paste0(
    "\\textit{Notes.} OLS with HC3-robust SEs in parentheses. ",
    "Dependent variable: USD redistributed (merit scenario). ",
    "Cols.~(1)--(2): full sample; includes effort--luck belief to assess mediation; ",
    "compare treatment coefficients with Table~\\ref{tab:main} cols.~(3)--(4). ",
    "Cols.~(3)--(4): relative-feedback group only. ",
    "``Positively surprised'': told above but predicted below. ",
    "``Negatively surprised'': told below but predicted above. ",
    "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
  )
} else {
  models_mech   <- list("(1)" = m9, "(2)" = m10)
  ctrl_row_mech <- as.data.frame(as.list(setNames(
    c("Controls (age, female, score, Colombian, edu.)", "No", "Yes"),
    c("term", "(1)", "(2)")
  )))
  tbl3_notes <- paste0(
    "\\textit{Notes.} OLS with HC3-robust SEs in parentheses. ",
    "Dependent variable: USD redistributed (merit scenario). ",
    "Includes effort--luck belief as control to assess mediation; ",
    "compare treatment coefficients with Table~\\ref{tab:main} cols.~(3)--(4). ",
    "Baseline: Told Above. ",
    "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
  )
}

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

fig2 <- make_bar(sum_effort, "Effort Attribution by Treatment",
                 "Effort-luck belief (0=luck, 10=effort)") +
        make_bar(sum_merit,  "Merit Redistribution by Treatment",
                 "USD to Worker B") +
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

cat("\nDone. Outputs: table1.tex, table2.tex, table3.tex,",
    "figure2.pdf, figure3.pdf, figure4.pdf\n")


table(dt$country)
