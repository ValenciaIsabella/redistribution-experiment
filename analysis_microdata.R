# =============================================================================
# Relative Performance Feedback and Redistribution Preferences
# Micro-data analysis — one row per participant
# Isabella Valencia, Paris School of Economics
# =============================================================================
# This file keeps one observation per participant. Both redistribution
# decisions (merit and luck) are retained as separate columns. All regressions
# run on the participant level; no long-format pivoting.
# =============================================================================

library(tidyverse)
library(jsonlite)
library(lmtest)
library(sandwich)
library(mediation)
library(patchwork)

select <- dplyr::select

# =============================================================================
# 1. DATA LOADING AND PREPARATION
# =============================================================================

lines  <- readLines("C:\\Users\\User\\Downloads\\Results_21_05.txt")

# fromJSON expands the `roundResults` field (5 rounds per participant) into
# 5 rows when binding. We drop it before binding so 1 line = 1 participant row.
df_raw <- map_dfr(lines, function(line) {
  obj <- fromJSON(line)
  obj$roundResults <- NULL
  # JSON null becomes NULL in R; replace with NA so as_tibble() accepts it
  obj[sapply(obj, is.null)] <- NA
  as_tibble(obj)
})

df <- df_raw %>%
  mutate(
    pid = row_number(),

    # ----- Treatment -----
    treatment = case_when(
      group == "none"                              ~ "no_feedback",
      group == "relative" & pair_result == "above" ~ "above",
      group == "relative" & pair_result == "below" ~ "below"
    ),
    treatment    = factor(treatment, levels = c("no_feedback", "above", "below")),
    has_feedback = (group == "relative"),
    # Binary: above = 1, below = 0 (only within relative group)
    told_above   = if_else(group == "relative", pair_result == "above", NA),

    # ----- Outcomes -----
    # Decision 1 — merit scenario
    redist_merit     = dist_worker_b_usd,
    no_redist_merit  = dist_no_redist,

    # Decision 2 — luck scenario (NA for participants who did not reach it)
    redist_luck      = dist2_worker_d_usd,
    no_redist_luck   = dist2_no_redist,
    has_luck         = !is.na(dist2_worker_d_usd),

    # Within-person gap: how much MORE does this person give under luck vs merit?
    # Positive = more egalitarian when income is luck-determined
    redist_gap       = redist_luck - redist_merit,

    # ----- Mediator -----
    # Slider 0 (100% luck) to 100 (100% effort)
    effort_luck      = demo_effortLuck,

    # ----- Pre-feedback self-report -----
    effort_self      = sr_effort,
    score_guess      = sr_guess,
    pred_better      = (sr_relPerf == "much_better"),
    guess_error      = sr_guess - totalScore,     # positive = overestimated score

    # ----- Task performance -----
    score            = totalScore,
    n_correct        = totalCorrect,
    n_wrong          = totalWrong,
    accuracy         = totalCorrect / (totalCorrect + totalWrong + 1e-9),

    # ----- Demographics -----
    age       = demo_age,
    female    = (demo_gender == "female"),
    country   = demo_country_residence,
    education = factor(demo_education,
                       levels  = c("high-school","some-college","bachelor","master","phd"),
                       ordered = TRUE),
    student   = grepl("student", demo_occupationStatus),
    mobile    = (device_type == "mobile"),

    # ----- Quality flags -----
    flag_no_clicks = (totalScore == 0 & n_correct == 0 & n_wrong == 0),
    flag_above_max = (totalScore > 200)
  )

# Exclusions
cat("--- Exclusions ---\n")
cat("Did not engage (zero clicks):", sum(df$flag_no_clicks), "\n")
cat("Score above max (> 200):",      sum(df$flag_above_max), "\n")

df_clean <- df %>% filter(!flag_no_clicks & !flag_above_max)
cat("Clean sample N:", nrow(df_clean), "\n\n")

# Convenience subsets (still one row per participant)
df_both    <- df_clean %>% filter(has_luck)          # have both decisions
df_beliefs <- df_clean %>% filter(!is.na(effort_luck))
df_rel     <- df_clean %>% filter(group == "relative")

# =============================================================================
# 2. SAMPLE OVERVIEW
# =============================================================================

cat("========== SAMPLE OVERVIEW ==========\n\n")

cat("Group sizes:\n")
df_clean %>%
  count(treatment) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  print()

cat("\nParticipants with luck decision:", nrow(df_both), "\n")
cat("Participants with effort-luck belief:", nrow(df_beliefs), "\n\n")

cat("Participant-level descriptives:\n")
df_clean %>%
  summarise(
    N              = n(),
    score_mean     = round(mean(score), 1),
    score_sd       = round(sd(score), 1),
    score_min      = min(score),
    score_max      = max(score),
    age_mean       = round(mean(age, na.rm = TRUE), 1),
    pct_female     = round(100 * mean(female, na.rm = TRUE), 1),
    pct_mobile     = round(100 * mean(mobile), 1),
    pct_has_luck   = round(100 * mean(has_luck), 1),
    pct_has_belief = round(100 * mean(!is.na(effort_luck)), 1)
  ) %>%
  print()

# =============================================================================
# 3. BALANCE CHECK
# =============================================================================

cat("\n========== BALANCE CHECK (feedback vs no feedback) ==========\n\n")

balance_tbl <- map_dfr(
  c("score","age","female","mobile","effort_self","pred_better","guess_error"),
  function(v) {
    x    <- df_clean[[v]]
    grp  <- df_clean$has_feedback
    tt   <- t.test(x[grp], x[!grp])
    tibble(
      variable        = v,
      mean_feedback   = round(mean(x[grp],  na.rm = TRUE), 2),
      mean_nofeedback = round(mean(x[!grp], na.rm = TRUE), 2),
      diff            = round(tt$estimate[1] - tt$estimate[2], 2),
      p_value         = round(tt$p.value, 3)
    )
  }
)
print(balance_tbl)

# =============================================================================
# 4. H1 — INCOME SOURCE AND REDISTRIBUTION
#    Both outcomes as columns; one row per participant
# =============================================================================

cat("\n\n========== H1: MERIT vs LUCK (participant-level) ==========\n\n")

cat("Means (participants with both decisions, N =", nrow(df_both), "):\n")
df_both %>%
  summarise(
    merit_mean = round(mean(redist_merit), 3),
    merit_sd   = round(sd(redist_merit), 3),
    luck_mean  = round(mean(redist_luck), 3),
    luck_sd    = round(sd(redist_luck), 3),
    gap_mean   = round(mean(redist_gap), 3),
    gap_sd     = round(sd(redist_gap), 3)
  ) %>%
  print()

# Paired t-test (participant is the unit; no long format needed)
cat("\nPaired t-test on participant-level gap (luck - merit):\n")
t.test(df_both$redist_luck, df_both$redist_merit, paired = TRUE) %>% print()

# Regression of luck redistribution on merit redistribution
# (how tightly correlated are the two decisions within persons?)
cat("\nCorrelation between merit and luck decisions:\n")
cor.test(df_both$redist_merit, df_both$redist_luck) %>% print()

# OLS: luck ~ merit + treatment (within-person correlation controlling for group)
h1_ols <- lm(redist_luck ~ redist_merit + treatment, data = df_both)
cat("\nOLS: luck decision ~ merit decision + treatment:\n")
print(coeftest(h1_ols, vcov = vcovHC(h1_ols, "HC3")))

# OLS: gap ~ treatment (treatment effects on the within-person difference)
cat("\nOLS: luck-merit gap ~ treatment:\n")
h1_gap <- lm(redist_gap ~ treatment, data = df_both)
print(coeftest(h1_gap, vcov = vcovHC(h1_gap, "HC3")))

# Share of participants who gave more under luck than merit
cat("\nShare redistributing MORE under luck than merit:\n")
df_both %>%
  summarise(
    more_luck  = round(100 * mean(redist_gap > 0), 1),
    same       = round(100 * mean(redist_gap == 0), 1),
    more_merit = round(100 * mean(redist_gap < 0), 1)
  ) %>%
  print()

# =============================================================================
# 5. H2 — RELATIVE POSITION AND MERIT REDISTRIBUTION
#    Primary outcome; one row per participant
# =============================================================================

cat("\n========== H2: RELATIVE POSITION AND REDISTRIBUTION ==========\n\n")

cat("Means by treatment group:\n")
df_clean %>%
  group_by(treatment) %>%
  summarise(
    n    = n(),
    mean = round(mean(redist_merit), 3),
    sd   = round(sd(redist_merit), 3),
    se   = round(sd(redist_merit) / sqrt(n()), 3),
    .groups = "drop"
  ) %>%
  print()

cat("\nPairwise t-tests (Bonferroni):\n")
pairwise.t.test(df_clean$redist_merit, df_clean$treatment,
                p.adjust.method = "bonferroni") %>% print()

# OLS specifications (each participant = one observation)
cat("\nOLS (1): feedback dummy only\n")
m1 <- lm(redist_merit ~ has_feedback, data = df_clean)
print(coeftest(m1, vcov = vcovHC(m1, "HC3")))

cat("\nOLS (2): three treatment arms\n")
m2 <- lm(redist_merit ~ treatment, data = df_clean)
print(coeftest(m2, vcov = vcovHC(m2, "HC3")))

cat("\nOLS (3): three arms + demographic controls\n")
m3 <- lm(redist_merit ~ treatment + age + female + score + mobile, data = df_clean)
print(coeftest(m3, vcov = vcovHC(m3, "HC3")))

# Within relative group only: above vs below
cat("\nWithin relative group — above vs below:\n")
t.test(redist_merit ~ pair_result, data = df_rel) %>% print()

m_rel <- lm(redist_merit ~ told_above, data = df_rel)
cat("OLS (within relative group):\n")
print(coeftest(m_rel, vcov = vcovHC(m_rel, "HC3")))

# =============================================================================
# 6. H3 — SELF-SERVING ATTRIBUTION
#    effort_luck is a participant-level variable; one row per participant
# =============================================================================

cat("\n========== H3: SELF-SERVING ATTRIBUTION ==========\n\n")

cat("Effort-vs-luck belief by treatment (N =", nrow(df_beliefs), "):\n")
df_beliefs %>%
  group_by(treatment) %>%
  summarise(
    n    = n(),
    mean = round(mean(effort_luck), 1),
    sd   = round(sd(effort_luck), 1),
    se   = round(sd(effort_luck) / sqrt(n()), 1),
    .groups = "drop"
  ) %>%
  print()

cat("\nOLS: treatment → effort-luck belief:\n")
m_h3 <- lm(effort_luck ~ treatment, data = df_beliefs)
print(coeftest(m_h3, vcov = vcovHC(m_h3, "HC3")))

cat("\nt-test above vs below (within relative group):\n")
df_rel_beliefs <- df_beliefs %>% filter(group == "relative")
t.test(effort_luck ~ pair_result, data = df_rel_beliefs) %>% print()

# Score is a participant-level covariate: does actual performance predict beliefs?
cat("\nOLS: effort-luck belief ~ actual score (performance → attribution):\n")
m_score_belief <- lm(effort_luck ~ score + treatment, data = df_beliefs)
print(coeftest(m_score_belief, vcov = vcovHC(m_score_belief, "HC3")))

# Self-serving: did people who expected to do better (pred_better) end up
# attributing more to effort — independent of actual rank?
cat("\nOLS: effort-luck ~ pre-feedback expectations + actual rank:\n")
m_expect <- lm(effort_luck ~ pred_better + told_above + score,
               data = df_beliefs %>% filter(group == "relative"))
print(coeftest(m_expect, vcov = vcovHC(m_expect, "HC3")))

# =============================================================================
# 7. H4 — MEDIATION
#    Participant-level; no reshaping needed
# =============================================================================

cat("\n========== H4: MEDIATION (participant-level) ==========\n\n")

# --- 7a. Above vs no_feedback ---
df_med_above <- df_beliefs %>%
  filter(treatment %in% c("no_feedback", "above")) %>%
  mutate(treat_bin = as.integer(treatment == "above"))

cat("N (above vs no_feedback):", nrow(df_med_above), "\n")

step1a <- lm(effort_luck ~ treat_bin, data = df_med_above)
step2a <- lm(redist_merit ~ treat_bin, data = df_med_above)
step3a <- lm(redist_merit ~ treat_bin + effort_luck, data = df_med_above)

cat("\nStep 1  treat → mediator:              ",
    sprintf("b = %.3f, SE = %.3f, p = %.3f",
            coef(step1a)["treat_bin"],
            sqrt(vcovHC(step1a,"HC3")["treat_bin","treat_bin"]),
            coeftest(step1a, vcovHC(step1a,"HC3"))["treat_bin","Pr(>|t|)"]), "\n")
cat("Step 2  treat → outcome (total effect):  ",
    sprintf("b = %.3f, SE = %.3f, p = %.3f",
            coef(step2a)["treat_bin"],
            sqrt(vcovHC(step2a,"HC3")["treat_bin","treat_bin"]),
            coeftest(step2a, vcovHC(step2a,"HC3"))["treat_bin","Pr(>|t|)"]), "\n")
cat("Step 3a treat → outcome (direct effect): ",
    sprintf("b = %.3f, SE = %.3f, p = %.3f",
            coef(step3a)["treat_bin"],
            sqrt(vcovHC(step3a,"HC3")["treat_bin","treat_bin"]),
            coeftest(step3a, vcovHC(step3a,"HC3"))["treat_bin","Pr(>|t|)"]), "\n")
cat("Step 3b mediator → outcome:              ",
    sprintf("b = %.3f, SE = %.3f, p = %.3f",
            coef(step3a)["effort_luck"],
            sqrt(vcovHC(step3a,"HC3")["effort_luck","effort_luck"]),
            coeftest(step3a, vcovHC(step3a,"HC3"))["effort_luck","Pr(>|t|)"]), "\n\n")

set.seed(42)
med_a <- mediate(lm(effort_luck ~ treat_bin, data = df_med_above),
                 lm(redist_merit ~ treat_bin + effort_luck, data = df_med_above),
                 treat = "treat_bin", mediator = "effort_luck",
                 robustSE = TRUE, sims = 1000)
cat("Formal mediation (above vs no_feedback):\n")
summary(med_a)

# --- 7b. Below vs no_feedback ---
df_med_below <- df_beliefs %>%
  filter(treatment %in% c("no_feedback", "below")) %>%
  mutate(treat_bin = as.integer(treatment == "below"))

cat("\nN (below vs no_feedback):", nrow(df_med_below), "\n")
set.seed(42)
med_b <- mediate(lm(effort_luck ~ treat_bin, data = df_med_below),
                 lm(redist_merit ~ treat_bin + effort_luck, data = df_med_below),
                 treat = "treat_bin", mediator = "effort_luck",
                 robustSE = TRUE, sims = 1000)
cat("Formal mediation (below vs no_feedback):\n")
summary(med_b)

# =============================================================================
# 8. PARTICIPANT-LEVEL TYPOLOGY
#    Classify each participant by their fairness type (Cappelen-style)
# =============================================================================

cat("\n========== FAIRNESS TYPOLOGY (participant-level) ==========\n\n")
# Based on merit decision alone (primary outcome):
#   Libertarian  : gives 0 to Worker B (keeps full inequality)
#   Meritocratic : gives 1–3 (reduces but preserves some inequality)
#   Egalitarian  : gives 4 (equal split)
#   Redistributive: gives 5–8 (reverses inequality toward loser)

df_clean <- df_clean %>%
  mutate(
    fairness_type = case_when(
      redist_merit == 0        ~ "Libertarian",
      redist_merit %in% 1:3   ~ "Meritocratic",
      redist_merit == 4        ~ "Egalitarian",
      redist_merit %in% 5:8   ~ "Redistributive"
    ),
    fairness_type = factor(fairness_type,
                           levels = c("Libertarian","Meritocratic",
                                      "Egalitarian","Redistributive"))
  )

cat("Fairness type distribution (merit decision):\n")
df_clean %>%
  count(fairness_type) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  print()

cat("\nFairness type by treatment group:\n")
df_clean %>%
  count(treatment, fairness_type) %>%
  group_by(treatment) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  print()

# Chi-squared test: is type distribution independent of treatment?
cat("\nChi-squared test: fairness type ~ treatment:\n")
chisq.test(table(df_clean$fairness_type, df_clean$treatment)) %>% print()

# =============================================================================
# 9. OVERCONFIDENCE AND SELF-SERVING PREDICTION
# =============================================================================

cat("\n========== OVERCONFIDENCE / CALIBRATION ==========\n\n")

cat("Participants who predicted 'much_better' (pre-feedback):",
    round(100 * mean(df_clean$pred_better, na.rm = TRUE), 1), "%\n")

cat("\nAmong relative group — prediction vs actual rank:\n")
df_rel %>%
  mutate(pred_correct = (pred_better == (pair_result == "above"))) %>%
  group_by(pair_result) %>%
  summarise(
    n            = n(),
    pct_predicted_better = round(100 * mean(pred_better), 1),
    pct_correct          = round(100 * mean(pred_correct), 1),
    .groups = "drop"
  ) %>%
  print()

cat("\nDoes guess error (overconfidence) predict redistribution?\n")
m_conf <- lm(redist_merit ~ guess_error + treatment, data = df_clean)
print(coeftest(m_conf, vcov = vcovHC(m_conf, "HC3")))

# =============================================================================
# 10. REGRESSION SUMMARY TABLE (participant-level, columns = outcomes)
# =============================================================================

cat("\n========== REGRESSION TABLE ==========\n\n")

# Panel A: merit redistribution
specs_merit <- list(
  "(M1)"  = lm(redist_merit ~ has_feedback,                           data = df_clean),
  "(M2)"  = lm(redist_merit ~ treatment,                              data = df_clean),
  "(M3)"  = lm(redist_merit ~ treatment + age + female + score + mobile, data = df_clean),
  "(M4)"  = lm(redist_merit ~ treatment + effort_luck,               data = df_beliefs),
  "(M5)"  = lm(redist_merit ~ treatment + effort_luck + age + female + score + mobile,
                                                                      data = df_beliefs)
)

cat("Panel A — Outcome: merit redistribution (amount to Worker B, USD)\n")
map_dfr(names(specs_merit), function(nm) {
  m   <- specs_merit[[nm]]
  rob <- coeftest(m, vcov = vcovHC(m, "HC3"))
  fmt <- function(var) {
    if (!var %in% rownames(rob)) return(NA_character_)
    sprintf("%.3f (%.3f)%s",
            rob[var, "Estimate"],
            rob[var, "Std. Error"],
            ifelse(rob[var, "Pr(>|t|)"] < .05, "*", ""))
  }
  tibble(
    spec         = nm,
    N            = nobs(m),
    R2           = round(summary(m)$r.squared, 3),
    feedback     = fmt("has_feedbackTRUE"),
    above        = fmt("treatmentabove"),
    below        = fmt("treatmentbelow"),
    effort_luck  = fmt("effort_luck")
  )
}) %>% print(width = Inf)

# Panel B: luck redistribution (among those with both decisions)
specs_luck <- list(
  "(L1)" = lm(redist_luck ~ has_feedback,                              data = df_both),
  "(L2)" = lm(redist_luck ~ treatment,                                 data = df_both),
  "(L3)" = lm(redist_luck ~ treatment + age + female + score + mobile, data = df_both)
)

cat("\nPanel B — Outcome: luck redistribution (amount to Worker D, USD)\n")
map_dfr(names(specs_luck), function(nm) {
  m   <- specs_luck[[nm]]
  rob <- coeftest(m, vcov = vcovHC(m, "HC3"))
  fmt <- function(var) {
    if (!var %in% rownames(rob)) return(NA_character_)
    sprintf("%.3f (%.3f)%s",
            rob[var, "Estimate"],
            rob[var, "Std. Error"],
            ifelse(rob[var, "Pr(>|t|)"] < .05, "*", ""))
  }
  tibble(
    spec     = nm,
    N        = nobs(m),
    R2       = round(summary(m)$r.squared, 3),
    feedback = fmt("has_feedbackTRUE"),
    above    = fmt("treatmentabove"),
    below    = fmt("treatmentbelow")
  )
}) %>% print(width = Inf)

# Panel C: within-person gap (luck - merit)
specs_gap <- list(
  "(G1)" = lm(redist_gap ~ has_feedback,                              data = df_both),
  "(G2)" = lm(redist_gap ~ treatment,                                 data = df_both),
  "(G3)" = lm(redist_gap ~ treatment + age + female + score + mobile, data = df_both)
)

cat("\nPanel C — Outcome: within-person luck-merit gap\n")
map_dfr(names(specs_gap), function(nm) {
  m   <- specs_gap[[nm]]
  rob <- coeftest(m, vcov = vcovHC(m, "HC3"))
  fmt <- function(var) {
    if (!var %in% rownames(rob)) return(NA_character_)
    sprintf("%.3f (%.3f)%s",
            rob[var, "Estimate"],
            rob[var, "Std. Error"],
            ifelse(rob[var, "Pr(>|t|)"] < .05, "*", ""))
  }
  tibble(
    spec     = nm,
    N        = nobs(m),
    R2       = round(summary(m)$r.squared, 3),
    feedback = fmt("has_feedbackTRUE"),
    above    = fmt("treatmentabove"),
    below    = fmt("treatmentbelow")
  )
}) %>% print(width = Inf)

cat("\nNote: Robust SE (HC3) in parentheses. * p < 0.05.\n")
cat("Reference group: no_feedback. Luck and gap panels restricted to N =",
    nrow(df_both), "(participants who completed Decision 2).\n")

# =============================================================================
# 11. FIGURES (participant-level, no reshaping)
# =============================================================================

group_colors <- c("no_feedback" = "#6baed6", "above" = "#74c476", "below" = "#fb6a4a")
group_labels <- c("no_feedback" = "No feedback", "above" = "Told: Above", "below" = "Told: Below")
theme_paper  <- theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

# Fig 1: Merit redistribution — dot plot (one dot per participant) + group mean
fig1 <- df_clean %>%
  group_by(treatment) %>%
  mutate(group_mean = mean(redist_merit)) %>%
  ungroup() %>%
  ggplot(aes(x = treatment, y = redist_merit, color = treatment)) +
  geom_jitter(width = 0.18, height = 0.12, alpha = 0.45, size = 1.8) +
  stat_summary(fun = mean, geom = "crossbar", width = 0.4,
               linewidth = 0.9, color = "black") +
  geom_hline(yintercept = 4, linetype = "dashed", color = "gray40") +
  scale_x_discrete(labels = group_labels) +
  scale_color_manual(values = group_colors, guide = "none") +
  scale_y_continuous(breaks = 0:8) +
  labs(
    title    = "Merit Redistribution — One Dot per Participant",
    subtitle = "Crossbar = group mean | Dashed line = equal split ($4)",
    x = NULL, y = "Amount to Worker B (USD)",
    caption = paste0("N = ", nrow(df_clean))
  ) +
  theme_paper
print(fig1)

# Fig 2: Scatter — merit vs luck decision (one dot per participant, color = treatment)
if (nrow(df_both) > 0) {
  fig2 <- df_both %>%
    ggplot(aes(x = redist_merit, y = redist_luck, color = treatment)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
    geom_jitter(width = 0.15, height = 0.15, alpha = 0.6, size = 2.2) +
    scale_color_manual(values = group_colors, labels = group_labels) +
    scale_x_continuous(breaks = 0:8) +
    scale_y_continuous(breaks = 0:8) +
    labs(
      title    = "Merit vs Luck Redistribution (per participant)",
      subtitle = "Points above the diagonal = gave more under luck | N = participants with both decisions",
      x = "Merit decision — amount to Worker B (USD)",
      y = "Luck decision — amount to Worker D (USD)",
      color = NULL,
      caption = paste0("N = ", nrow(df_both))
    ) +
    theme_paper
  print(fig2)
}

# Fig 3: Scatter — effort-luck belief vs merit redistribution (one dot per participant)
fig3 <- df_beliefs %>%
  ggplot(aes(x = effort_luck, y = redist_merit, color = treatment)) +
  geom_jitter(width = 1.2, height = 0.15, alpha = 0.55, size = 2) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.12, linewidth = 1) +
  scale_color_manual(values = group_colors, labels = group_labels) +
  scale_y_continuous(breaks = 0:8) +
  labs(
    title    = "Effort-vs-Luck Belief and Merit Redistribution",
    subtitle = "Each dot is one participant | Lines: group OLS fit",
    x = "Effort-vs-luck belief (0 = all luck, 100 = all effort)",
    y = "Amount to Worker B — merit scenario (USD)",
    color = NULL,
    caption = paste0("N = ", nrow(df_beliefs))
  ) +
  theme_paper
print(fig3)

# Fig 4: Fairness type shares by treatment (stacked bar, participant counts)
fig4 <- df_clean %>%
  count(treatment, fairness_type) %>%
  group_by(treatment) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  ggplot(aes(x = treatment, y = pct, fill = fairness_type)) +
  geom_col(width = 0.55, color = "white", linewidth = 0.3) +
  scale_x_discrete(labels = group_labels) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Fairness Type Distribution by Treatment Group",
    subtitle = "Based on merit allocation decision",
    x = NULL, y = "Share of participants", fill = NULL,
    caption = paste0("N = ", nrow(df_clean))
  ) +
  theme_paper
print(fig4)

# Fig 5: Within-person gap (redist_luck - redist_merit) by treatment
if (nrow(df_both) > 0) {
  gap_means <- df_both %>%
    group_by(treatment) %>%
    summarise(mean_gap = mean(redist_gap), .groups = "drop")

  fig5 <- df_both %>%
    ggplot(aes(x = redist_gap, fill = treatment)) +
    geom_histogram(binwidth = 1, color = "white", alpha = 0.85) +
    geom_vline(data = gap_means, aes(xintercept = mean_gap),
               linetype = "dashed", linewidth = 0.9) +
    geom_vline(xintercept = 0, color = "black", linewidth = 0.5) +
    facet_wrap(~ treatment, ncol = 1, labeller = as_labeller(group_labels)) +
    scale_fill_manual(values = group_colors, guide = "none") +
    labs(
      title    = "Within-Person Luck–Merit Gap",
      subtitle = "Dashed line = group mean | Positive = more redistribution under luck",
      x = "Luck allocation − Merit allocation (USD)",
      y = "Number of participants",
      caption = paste0("N = ", nrow(df_both), " participants with both decisions")
    ) +
    theme_paper
  print(fig5)
}
