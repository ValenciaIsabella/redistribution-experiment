library(data.table)
library(jsonlite)
library(lmtest)
library(sandwich)
library(ggplot2)
library(patchwork)

# =============================================================================
# LOAD
# =============================================================================

lines <- readLines("C:\\Users\\User\\Downloads\\Results_26_05.txt")

dt_raw <- rbindlist(lapply(lines, function(line) {
  obj <- fromJSON(line)
  obj$roundResults <- NULL
  obj[sapply(obj, is.null)] <- NA
  as.data.table(obj)
}), fill = TRUE)
table(dt_raw$demo_country_origin)
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
  effort_luck  = demo_effortLuck/10,

  effort_self  = sr_effort,
  score_guess  = sr_guess,
  pred_better  = sr_relPerf ,
  guess_error  = sr_guess - totalScore,

  age              = demo_age,
  female           = (demo_gender == "female"),
  country          = demo_country_origin,
  education        = factor(demo_education,
                            levels  = c("high-school","some-college","bachelor","master","phd"),
                            ordered = TRUE),
  parent_education = factor(demo_parent_education,
                            levels  = c("high-school","some-college","bachelor","master"),
                            ordered = TRUE),

  # for exclusion filters only
  n_correct    = totalCorrect,
  n_wrong      = totalWrong
)]

# Exclusions
dt <- dt[!(score == 0 & n_correct == 0 & n_wrong == 0)]
dt[, c("n_correct", "n_wrong") := NULL]
dt <- dt[redist_merit < 5]

########### CRITICO
#dt <- dt[!(treatment == "no_feedback" & redist_merit == 0 & country == "Colombia" & female == TRUE)]

cat("N clean:", nrow(dt), "\n")
table(dt$treatment, useNA = "ifany")
# =============================================================================
# SURPRISE VARIABLES  (NA outside the relative group)
# positively_surprised: expected to NOT be above, but WAS told above
# negatively_surprised: expected to be above (pred_better), but told below
# =============================================================================

dt[, above_positively_surprised := fifelse(told_above == TRUE  & pred_better == FALSE, 1, 0)]
dt[, below_negatively_surprised := fifelse(told_above == FALSE & pred_better == TRUE, 1, 0)]
dt[, log_redist_merit := log(redist_merit + 1)]
dt[, log_redist_luck  := log(redist_luck  + 1)]
dt[, log_effort_luck   := log(effort_luck   + 1)]
dt[, redist_gap := redist_merit - redist_luck]
dt[, feedback := fifelse(treatment == "no_feedback", "No Feedback", "Feedback")]
dt[, pos_neg_feedback := fifelse(told_above == TRUE, "Told: Above", "Told: Below")]
dt[, feedback := factor(feedback, levels = c("No Feedback", "Feedback"))]
dt[, pos_neg_feedback := factor(pos_neg_feedback, levels = c("Told: Above", "Told: Below"))]

# =============================================================================
# MAIN ANALYSIS:
# =============================================================================

# H1: effort attribution (redist_merit) is correlated with distributive prefereces in the merit based condition (redist_merit ~ effor_luck)
summary(dt$redist_merit)
summary(dt$effort_luck)
h1 <- lm(redist_merit ~ effort_luck, data = dt)
print(coeftest(h1, vcov = vcovHC(h1, "HC3")))
h1 <- lm(log_redist_merit ~ log_effort_luck, data = dt)
print(coeftest(h1, vcov = vcovHC(h1, "HC3")))


# H2: effort attribution (redist_merit) is higher in the above vs below condition
# Muestra completa, pero cambias la referencia explícitamente
dt$treatment <- relevel(factor(dt$treatment), ref = "below")
h2 <- lm(effort_luck ~ treatment, data = dt)
coeftest(h2, vcov = vcovHC(h2, "HC3"))

table(dt$pred_better, dt$treatment)
chisq.test(dt$pred_better, dt$treatment)
# H3: effort attribution (redist_merit) is higher in the above vs below condition
summary(dt$redist_merit)
summary(dt$treatment)
h3 <- lm(redist_merit ~ treatment, data = dt)
print(coeftest(h3, vcov = vcovHC(h3, "HC3")))
h3_controls <- lm(redist_merit ~ treatment + age + female, data = dt)
print(coeftest(h3_controls, vcov = vcovHC(h3_controls, "HC3")))

# H4: effort attribution (redist_merit) is (not) heterogeneously affected in the above vs below condition
h4 <- lm(redist_merit ~  treatment, data = dt)

h4 <- lm(redist_merit ~ treatment +above_positively_surprised + below_negatively_surprised, data = dt)
print(coeftest(h4, vcov = vcovHC(h4, "HC3"))) 

h4_ <- lm(redist_merit ~ above_positively_surprised + below_negatively_surprised, data = dt)
print(coeftest(h4_, vcov = vcovHC(h4_, "HC3"))) 


# =============================================================================
# FIGURE: Redistribution by treatment × scenario (Cappelen-style)
# =============================================================================

group_colors <- c("no_feedback" = "#E07B6A", "above" = "#7AB648", "below" = "#6BAED4")
group_labels <- c("no_feedback" = "No Feedback", "above" = "Told:\nAbove", "below" = "Told:\nBelow")

sum_merit <- dt[!is.na(treatment) & !is.na(redist_merit), .(
  mean = mean(redist_merit),
  se   = sd(redist_merit) / sqrt(.N)
), by = treatment]

sum_luck <- dt[!is.na(treatment) & !is.na(redist_luck), .(
  mean = mean(redist_luck),
  se   = sd(redist_luck) / sqrt(.N)
), by = treatment]

ymax <- ceiling(max(c(sum_merit$mean + sum_merit$se,
                      sum_luck$mean  + sum_luck$se), na.rm = TRUE) * 1.2)

make_panel <- function(data, title, ylab) {
  ggplot(data, aes(x = treatment, y = mean, fill = treatment)) +
    geom_col(width = 0.55) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                  width = 0.12, linewidth = 0.8) +
    geom_text(aes(y = ymax * 0.04, label = round(mean, 1)),
              color = "white", fontface = "bold", size = 4.5) +
    scale_fill_manual(values = group_colors, guide = "none") +
    scale_x_discrete(labels = group_labels) +
    scale_y_continuous(limits = c(0, ymax),
                       breaks = pretty(c(0, ymax), n = 6)) +
    labs(title = title, x = NULL, y = ylab) +
    theme_classic(base_size = 13) +
    theme(
      aspect.ratio = 0.65,
      plot.title  = element_text(face = "bold", hjust = 0.5),
      axis.text.x = element_text(color = "black", size = 11)
    )
}

p_merit <- make_panel(sum_merit,
                      "Redistributive decisions\nfor merit scenario",
                      "USD to Worker B")
p_luck  <- make_panel(sum_luck,
                      "Redistributive decisions\nfor luck scenario",
                      "USD to Worker D")

fig1 <- p_merit + p_luck +
  plot_annotation(
    caption = paste0(
      "Notes. Bar heights = group means. Whiskers = +1 SE. N = ", nrow(dt), "."
    )
  )
print(fig1)
ggsave("figure1.pdf", fig1, width = 10, height = 5, device = cairo_pdf)

# Figure 2: effort_luck (left) + redist_merit (right)

sum_effort <- dt[!is.na(treatment) & !is.na(effort_luck), .(
  mean = mean(effort_luck),
  se   = sd(effort_luck) / sqrt(.N)
), by = treatment]

ymax_effort <- ceiling(max(sum_effort$mean + sum_effort$se, na.rm = TRUE) * 1.2)
ymax_merit2 <- ceiling(max(sum_merit$mean  + sum_merit$se,  na.rm = TRUE) * 1.2)

make_panel2 <- function(data, title, ylab, ymax) {
  ggplot(data, aes(x = treatment, y = mean, fill = treatment)) +
    geom_col(width = 0.55) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                  width = 0.12, linewidth = 0.8) +
    geom_text(aes(y = ymax * 0.04, label = round(mean, 1)),
              color = "white", fontface = "bold", size = 4.5) +
    scale_fill_manual(values = group_colors, guide = "none") +
    scale_x_discrete(labels = group_labels) +
    scale_y_continuous(limits = c(0, ymax),
                       breaks = pretty(c(0, ymax), n = 6)) +
    labs(title = title, x = NULL, y = ylab) +
    theme_classic(base_size = 13) +
    theme(
      aspect.ratio = 0.65,
      plot.title  = element_text(face = "bold", hjust = 0.5),
      axis.text.x = element_text(color = "black", size = 11)
    )
}

p_effort <- make_panel2(sum_effort,
                        "Effort attribution\nby treatment",
                        "Effort-luck belief (0 = luck, 10 = effort)",
                        ymax_effort)
p_merit2 <- make_panel2(sum_merit,
                        "Redistributive decisions\nfor merit scenario",
                        "USD to Worker B",
                        ymax_merit2)

fig2 <- p_effort + p_merit2 +
  plot_annotation(
    caption = paste0(
      "Notes. Bar heights = group means. Whiskers = ±1 SE. N = ", nrow(dt), "."
    )
  )
print(fig2)
ggsave("figure2.pdf", fig2, width = 10, height = 5, device = cairo_pdf)

# =============================================================================
# LATEX TABLES
# =============================================================================
library(modelsummary)
library(kableExtra)

# ── Table 1: Balance / Descriptive Statistics by treatment ───────────────────

vars <- c("redist_merit", "redist_luck", "effort_luck", "score", "age", "female", "pred_better")
labels <- c("Redistribution -- merit (\\$)", "Redistribution -- luck (\\$)",
            "Effort--luck belief (0--10)", "Task score",
            "Age", "Female (\\%)", "Predicted better (\\%)")

fmt_cell <- function(x, pct = FALSE) {
  m <- mean(x, na.rm = TRUE) * if (pct) 100 else 1
  s <- sd(x,   na.rm = TRUE) * if (pct) 100 else 1
  sprintf("%.2f (%.2f)", m, s)
}

# Build one row per variable
tbl1 <- rbindlist(lapply(seq_along(vars), function(i) {
  v   <- vars[i]
  pct <- v %in% c("female", "pred_better")

  row_nf  <- fmt_cell(dt[treatment == "no_feedback"][[v]], pct)
  row_ab  <- fmt_cell(dt[treatment == "above"][[v]],       pct)
  row_bl  <- fmt_cell(dt[treatment == "below"][[v]],       pct)

  # p-value from one-way ANOVA (or chi-sq for binary)
  x  <- dt[[v]]
  tr <- dt$treatment
  pv <- tryCatch({
    if (pct) chisq.test(table(x, tr))$p.value
    else     summary(aov(x ~ tr))[[1]][["Pr(>F)"]][1]
  }, error = function(e) NA_real_)

  data.table(
    Variable     = labels[i],
    `No Feedback` = row_nf,
    Above        = row_ab,
    Below        = row_bl,
    `$p$-value`  = ifelse(is.na(pv), "--", sprintf("%.3f", pv))
  )
}))

# Add N row
n_row <- data.table(
  Variable      = "$N$",
  `No Feedback` = as.character(nrow(dt[treatment == "no_feedback"])),
  Above         = as.character(nrow(dt[treatment == "above"])),
  Below         = as.character(nrow(dt[treatment == "below"])),
  `$p$-value`   = as.character(nrow(dt))
)
tbl1 <- rbind(tbl1, n_row)

tbl1_tex <- kable(tbl1, format = "latex", booktabs = TRUE, escape = FALSE,
          caption = "Descriptive Statistics by Treatment Group \\label{tab:balance}",
          col.names = c("", "No Feedback", "Told: Above", "Told: Below", "$p$-value")) |>
  kable_styling(latex_options = "hold_position") |>
  add_footnote("Mean (SD) for continuous variables; mean $\\\\times$ 100 (SD $\\\\times$ 100) for binary. $p$-values from one-way ANOVA (continuous) or chi-squared test (binary). Redistribution in USD.",
               notation = "none", escape = FALSE)
cat(tbl1_tex)
writeLines(tbl1_tex, "table1.tex")

# ── Table 2: OLS Regressions ─────────────────────────────────────────────────

dt_rel   <- dt[!is.na(told_above)]
dt_eff   <- dt[!is.na(effort_luck)]

m1 <- lm(effort_luck  ~ treatment,                        data = dt_eff)
m2 <- lm(effort_luck  ~ treatment + age + female + score, data = dt_eff)
m3 <- lm(redist_merit ~ treatment,                        data = dt)
m4 <- lm(redist_merit ~ treatment + age + female + score, data = dt)
m5 <- lm(redist_merit ~ above_positively_surprised + below_negatively_surprised,
          data = dt_rel)

vcov_hc3 <- function(m) vcovHC(m, type = "HC3")

# Column headers embed the outcome name.
# \phantom{} makes R names unique while rendering invisibly in LaTeX.
models <- list(
  "(1) Effort--luck belief"                      = m1,
  "(2) Effort--luck belief\\phantom{x}"          = m2,
  "(3) Merit redistribution (\\$)"               = m3,
  "(4) Merit redistribution (\\$)\\phantom{x}"   = m4,
  "(5) Merit redistribution (\\$)\\phantom{xx}"  = m5
)

coef_map <- c(
  "treatmentabove"              = "Told: Above",
  "treatmentbelow"              = "Told: Below",
  "above_positively_surprised"  = "Positively surprised",
  "below_negatively_surprised"  = "Negatively surprised",
  "age"                         = "Age",
  "femaleTRUE"                  = "Female",
  "score"                       = "Task score"
)

ctrl_row <- as.data.frame(as.list(setNames(
  c("Controls (age, female, score)", "No", "Yes", "No", "Yes", "No"),
  c("term", names(models))
)))

modelsummary(
  models,
  vcov      = lapply(models, vcov_hc3),
  coef_map  = coef_map,
  gof_map   = list(
    list(raw = "nobs",      clean = "$N$",       fmt = 0),
    list(raw = "r.squared", clean = "$R^{2}$",   fmt = 3)
  ),
  stars     = c("*" = 0.05, "**" = 0.01),
  output    = "table2.tex",
  title     = "OLS Regressions \\label{tab:regressions}",
  add_rows  = ctrl_row,
  notes     = paste0(
    "\\textit{Notes.} OLS regressions with HC3-robust standard errors in parentheses. ",
    "In columns (1)--(2), the dependent variable is the effort--luck belief (0 = 100\\% luck, ",
    "10 = 100\\% effort), measuring participants' attribution of task earnings to effort versus luck. ",
    "In columns (3)--(5), the dependent variable is the amount redistributed to the lower-earning ",
    "worker in the merit scenario (USD). Columns (1)--(4) include the full sample; the omitted ",
    "baseline treatment is no feedback. Column (5) restricts to participants in the relative-feedback ",
    "group: ``positively surprised'' denotes participants who expected to score below average but ",
    "were told they scored above; ``negatively surprised'' denotes participants who expected to score ",
    "above average but were told they scored below. Controls are age, a female indicator, and task score. ",
    "$^{*}p<0.05$, $^{**}p<0.01$."
  ),
  escape    = FALSE
)
cat("Saved: table1.tex, table2.tex, figure1.pdf, figure2.pdf\n")

# =============================================================================
# TABLE 0: BACKGROUND CHARACTERISTICS BY TREATMENT GROUP
# =============================================================================

# Binary indicators for each education/nationality category
dt[, colombian  := (!is.na(country) & country == "Colombia")]
dt[, edu_hs     := (!is.na(education) & education == "high-school")]
dt[, edu_sc     := (!is.na(education) & education == "some-college")]
dt[, edu_ba     := (!is.na(education) & education == "bachelor")]
dt[, edu_ma     := (!is.na(education) & education == "master")]
dt[, edu_phd    := (!is.na(education) & education == "phd")]
dt[, peduc_hs   := (!is.na(parent_education) & parent_education == "high-school")]
dt[, peduc_sc   := (!is.na(parent_education) & parent_education == "some-college")]
dt[, peduc_ba   := (!is.na(parent_education) & parent_education == "bachelor")]
dt[, peduc_ma   := (!is.na(parent_education) & parent_education == "master")]

# Format helpers
fmt_ms  <- function(x) sprintf("%.1f (%.1f)", mean(x, na.rm=TRUE), sd(x, na.rm=TRUE))
fmt_pct <- function(x) sprintf("%.1f",         mean(x, na.rm=TRUE) * 100)

build_row <- function(label, fn, var, pval = "") {
  data.table(
    Variable      = label,
    `No Feedback` = fn(dt[treatment == "no_feedback"][[var]]),
    `Told: Above` = fn(dt[treatment == "above"][[var]]),
    `Told: Below` = fn(dt[treatment == "below"][[var]]),
    Total         = fn(dt[[var]]),
    `$p$-value`   = pval
  )
}

blank_row <- function(label, pval = "") {
  data.table(Variable=label, `No Feedback`="", `Told: Above`="", `Told: Below`="", Total="", `$p$-value`=pval)
}

# p-values
p_age    <- sprintf("%.3f", summary(aov(age    ~ treatment, data=dt))[[1]][["Pr(>F)"]][1])
p_female <- sprintf("%.3f", tryCatch(chisq.test(table(dt$female,    dt$treatment))$p.value, error=function(e) NA_real_))
p_col    <- sprintf("%.3f", tryCatch(chisq.test(table(dt$colombian, dt$treatment))$p.value, error=function(e) NA_real_))
p_edu    <- sprintf("%.3f", tryCatch(chisq.test(table(dt$education, dt$treatment))$p.value, error=function(e) NA_real_))
p_pedu   <- sprintf("%.3f", tryCatch(chisq.test(table(dt$parent_education, dt$treatment))$p.value, error=function(e) NA_real_))

tbl0 <- rbindlist(list(
  build_row("Age",                       fmt_ms,  "age",      p_age),
  build_row("Female (\\%)",              fmt_pct, "female",   p_female),
  build_row("Colombian (\\%)",           fmt_pct, "colombian",p_col),
  blank_row("Education (\\%)",           p_edu),
  build_row("\\quad High School",        fmt_pct, "edu_hs",   ""),
  build_row("\\quad Some College",       fmt_pct, "edu_sc",   ""),
  build_row("\\quad Bachelor's",         fmt_pct, "edu_ba",   ""),
  build_row("\\quad Master's",           fmt_pct, "edu_ma",   ""),
  build_row("\\quad PhD+",               fmt_pct, "edu_phd",  ""),
  blank_row("Parents' Education (\\%)",  p_pedu),
  build_row("\\quad High School or less",fmt_pct, "peduc_hs", ""),
  build_row("\\quad Some College",       fmt_pct, "peduc_sc", ""),
  build_row("\\quad Bachelor's",         fmt_pct, "peduc_ba", ""),
  build_row("\\quad Master's or higher", fmt_pct, "peduc_ma", ""),
  data.table(
    Variable      = "$N$",
    `No Feedback` = as.character(nrow(dt[treatment=="no_feedback"])),
    `Told: Above` = as.character(nrow(dt[treatment=="above"])),
    `Told: Below` = as.character(nrow(dt[treatment=="below"])),
    Total         = as.character(nrow(dt)),
    `$p$-value`   = ""
  )
))

tbl0_tex <- kable(tbl0, format = "latex", booktabs = TRUE, escape = FALSE,
      caption = "Background Characteristics by Treatment Group \\label{tab:background}",
      col.names = c("", "No Feedback", "Told: Above", "Told: Below", "Total", "$p$-value")) |>
  kable_styling(latex_options = "hold_position") |>
  row_spec(which(tbl0$Variable %in% c("Education (\\%)", "Parents' Education (\\%)")),
           bold = TRUE) |>
  add_footnote(
    "Age: mean (SD). All other variables: column percentages. $p$-values from one-way ANOVA (age) or chi-squared test (all others). Colombian: country of origin = Colombia.",
    notation = "none", escape = FALSE
  )

cat(tbl0_tex)
writeLines(tbl0_tex, "table0.tex")
cat("Saved: table0.tex\n")

