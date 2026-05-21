# =============================================================================
# Relative Performance Feedback and Redistribution Preferences
# Analysis script — Results_19_05.txt
# Isabella Valencia, Paris School of Economics
# =============================================================================

library(tidyverse)
library(jsonlite)
library(lmtest)
library(sandwich)
#install.packages("mediation")
library(mediation)
library(broom)
library(patchwork)

# Ensure dplyr::select takes precedence over MASS::select or other conflicts
select <- dplyr::select

# =============================================================================
# 1. DATA LOADING
# =============================================================================

lines    <- readLines("C:\\Users\\User\\Downloads\\Results_19_05.txt")
df_raw   <- map_dfr(lines, ~ fromJSON(.x, flatten = TRUE))

# =============================================================================
# 2. DATA PREPARATION
# =============================================================================

df <- df_raw %>%
  mutate(
    pid = row_number(),

    # Treatment arms
    treatment = case_when(
      group == "none"                             ~ "no_feedback",
      group == "relative" & pair_result == "above" ~ "above",
      group == "relative" & pair_result == "below" ~ "below"
    ),
    treatment = factor(treatment, levels = c("no_feedback", "above", "below")),
    has_feedback = (group == "relative"),

    # Redistribution outcomes (amount to the lower-earning worker)
    redist_merit = dist_worker_b_usd,           # Decision 1 — merit scenario
    redist_luck  = dist2_worker_d_usd,          # Decision 2 — luck scenario (NA if not reached)

    # Within-person gap: how much MORE does this person redistribute under luck?
    redist_gap   = redist_luck - redist_merit,

    # Mediator: effort-vs-luck belief (0 = all luck, 100 = all effort)
    effort_luck  = demo_effortLuck,

    # Demographics
    age         = demo_age,
    female      = (demo_gender == "female"),
    country     = demo_country_residence,
    education   = factor(demo_education,
                         levels = c("high-school","some-college","bachelor","master","phd"),
                         ordered = TRUE),
    student     = grepl("student", demo_occupationStatus),
    mobile      = (device_type == "mobile"),

    # Task performance
    score       = totalScore,
    n_correct   = totalCorrect,
    n_wrong     = totalWrong,

    # Self-report (pre-feedback)
    effort_self   = sr_effort,
    score_guess   = sr_guess,
    pred_better   = (sr_relPerf == "much_better"),

    # Prediction error (positive = overconfident)
    guess_error   = sr_guess - totalScore,

    # Data quality flags
    # Zero-score with zero clicks = did not engage with the task
    flag_no_clicks = (totalScore == 0 & n_correct == 0 & n_wrong == 0),
    # Score above theoretical max (~200) = suspicious automation
    flag_above_max = (totalScore > 200),
    # Wildly implausible score guess
    flag_bad_guess = (sr_guess < -50 | sr_guess > 200),

    has_luck_decision = !is.na(dist2_worker_d_usd)
  )

# -----------------------------------------------------------------------------
# 2a. Exclusions
# -----------------------------------------------------------------------------

# Participants who never clicked anything (all scores 0, clearly didn't engage)
df_excl <- df %>% filter(flag_no_clicks | flag_above_max)

cat("\n--- Exclusions ---\n")
cat("Did not engage (zero clicks):", sum(df$flag_no_clicks), "\n")
cat("Score above max (> 200):",      sum(df$flag_above_max), "\n")

df_clean <- df %>% filter(!flag_no_clicks & !flag_above_max)
cat("Clean sample N:", nrow(df_clean), "\n\n")

# =============================================================================
# 3. DESCRIPTIVE STATISTICS
# =============================================================================

cat("========== DESCRIPTIVE STATISTICS ==========\n\n")

cat("Group sizes:\n")
df_clean %>%
  count(treatment) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  print()

cat("\nHas luck decision (Decision 2):", sum(df_clean$has_luck_decision), "/", nrow(df_clean), "\n")

cat("\nSummary by treatment group:\n")
df_clean %>%
  group_by(treatment) %>%
  summarise(
    n            = n(),
    score_mean   = round(mean(score), 1),
    score_sd     = round(sd(score), 1),
    age_mean     = round(mean(age, na.rm = TRUE), 1),
    pct_female   = round(100 * mean(female, na.rm = TRUE), 1),
    pct_mobile   = round(100 * mean(mobile), 1),
    merit_mean   = round(mean(redist_merit), 2),
    merit_sd     = round(sd(redist_merit), 2),
    .groups = "drop"
  ) %>%
  print()

# Distribution of merit allocation choices
cat("\nMerit allocation choice distribution:\n")
df_clean %>%
  count(redist_merit) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  arrange(redist_merit) %>%
  print()

# =============================================================================
# 4. BALANCE CHECK (randomization quality)
# =============================================================================

cat("\n========== BALANCE CHECK ==========\n\n")

balance_vars <- c("score", "age", "female", "mobile", "effort_self",
                  "pred_better", "guess_error")

balance_tbl <- map_dfr(balance_vars, function(v) {
  vals <- df_clean[[v]]
  grp  <- df_clean$has_feedback
  tt   <- t.test(vals[grp], vals[!grp])
  tibble(
    variable  = v,
    mean_feedback   = round(mean(vals[grp],  na.rm = TRUE), 2),
    mean_nofeedback = round(mean(vals[!grp], na.rm = TRUE), 2),
    diff     = round(tt$estimate[1] - tt$estimate[2], 2),
    p_value  = round(tt$p.value, 3)
  )
})
print(balance_tbl)

# =============================================================================
# 5. H1 — INCOME SOURCE AND REDISTRIBUTION (Merit vs Luck, within-subjects)
# =============================================================================

cat("\n========== H1: MERIT vs LUCK (within-subjects) ==========\n\n")

df_both <- df_clean %>% filter(has_luck_decision)
cat("N for H1 (have both decisions):", nrow(df_both), "\n\n")

cat("Mean redistribution:\n")
cat("  Merit (Decision 1):", round(mean(df_both$redist_merit), 3),
    "  SD =", round(sd(df_both$redist_merit), 3), "\n")
cat("  Luck  (Decision 2):", round(mean(df_both$redist_luck),  3),
    "  SD =", round(sd(df_both$redist_luck),  3), "\n")
cat("  Within-person gap (luck - merit):", round(mean(df_both$redist_gap), 3),
    "  SD =", round(sd(df_both$redist_gap), 3), "\n\n")

# Paired t-test
h1_ttest <- t.test(df_both$redist_luck, df_both$redist_merit, paired = TRUE)
cat("Paired t-test (luck - merit):\n")
print(h1_ttest)

# Fixed-effects (individual FE) panel regression
df_long <- df_both %>%
  dplyr::select(pid, treatment, redist_merit, redist_luck) %>%
  pivot_longer(cols = c(redist_merit, redist_luck),
               names_to  = "scenario",
               values_to = "redistribution") %>%
  mutate(luck = (scenario == "redist_luck"))

h1_fe <- lm(redistribution ~ luck + factor(pid), data = df_long)
cat("\nIndividual FE regression — coefficient on luck dummy:\n")
coeftest(h1_fe, vcov = vcovHC(h1_fe, "HC3"))["luckTRUE", , drop = FALSE] %>% print()

# Gap by treatment group
cat("\nWithin-person luck gap (luck - merit) by treatment group:\n")
df_both %>%
  group_by(treatment) %>%
  summarise(
    n        = n(),
    gap_mean = round(mean(redist_gap), 3),
    gap_sd   = round(sd(redist_gap), 3),
    gap_se   = round(sd(redist_gap) / sqrt(n()), 3),
    .groups  = "drop"
  ) %>%
  print()

# =============================================================================
# 6. H2 — RELATIVE POSITION AND REDISTRIBUTION (Merit decision)
# =============================================================================

cat("\n========== H2: RELATIVE POSITION AND REDISTRIBUTION ==========\n\n")

cat("Mean redistribution (merit) by group:\n")
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

# Pairwise t-tests
cat("\nPairwise t-tests (Bonferroni):\n")
pairwise.t.test(df_clean$redist_merit, df_clean$treatment,
                p.adjust.method = "bonferroni") %>% print()

# OLS specifications
cat("\nOLS (1): feedback indicator only\n")
m1 <- lm(redist_merit ~ has_feedback, data = df_clean)
print(coeftest(m1, vcov = vcovHC(m1, "HC3")))

cat("\nOLS (2): three-group treatment (ref = no_feedback)\n")
m2 <- lm(redist_merit ~ treatment, data = df_clean)
print(coeftest(m2, vcov = vcovHC(m2, "HC3")))

cat("\nOLS (3): three-group + controls\n")
m3 <- lm(redist_merit ~ treatment + age + female + score + mobile,
          data = df_clean)
print(coeftest(m3, vcov = vcovHC(m3, "HC3")))

# Test for above vs below difference
cat("\nContrast: above vs below\n")
df_rel <- df_clean %>% filter(group == "relative")
t.test(redist_merit ~ pair_result, data = df_rel) %>% print()

# =============================================================================
# 7. H3 — SELF-SERVING ATTRIBUTION (effort-vs-luck beliefs)
# =============================================================================

cat("\n========== H3: SELF-SERVING ATTRIBUTION ==========\n\n")

df_beliefs <- df_clean %>% filter(!is.na(effort_luck))
cat("N with effort-luck belief:", nrow(df_beliefs), "\n\n")

cat("Mean effort-vs-luck belief (higher = more effort-driven):\n")
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

cat("\nOLS: treatment → effort-luck belief\n")
m_med <- lm(effort_luck ~ treatment, data = df_beliefs)
print(coeftest(m_med, vcov = vcovHC(m_med, "HC3")))

cat("\nt-test within relative group: above vs below on effort-luck belief\n")
df_rel_beliefs <- df_beliefs %>% filter(group == "relative")
t.test(effort_luck ~ pair_result, data = df_rel_beliefs) %>% print()

# Correlation between effort-luck belief and redistribution
cat("\nCorrelation: effort-luck belief ~ merit redistribution\n")
cor.test(df_beliefs$effort_luck, df_beliefs$redist_merit) %>% print()

# =============================================================================
# 8. H4 — MEDIATION (effort-luck belief mediates feedback → redistribution)
# =============================================================================

cat("\n========== H4: MEDIATION ANALYSIS ==========\n\n")

# Use the above vs no_feedback contrast (sharpest directional prediction)
df_med_above <- df_beliefs %>%
  filter(treatment %in% c("no_feedback", "above")) %>%
  mutate(treat_bin = as.integer(treatment == "above"))

cat("N (above vs no_feedback, with belief):", nrow(df_med_above), "\n\n")

# Baron & Kenny steps
cat("Step 1 — Treatment → Mediator (effort-luck belief):\n")
step1 <- lm(effort_luck ~ treat_bin, data = df_med_above)
print(summary(step1)$coefficients)

cat("\nStep 2 — Treatment → Outcome (merit redistribution):\n")
step2a <- lm(redist_merit ~ treat_bin, data = df_med_above)
print(summary(step2a)$coefficients)

cat("\nStep 3 — Treatment + Mediator → Outcome:\n")
step3 <- lm(redist_merit ~ treat_bin + effort_luck, data = df_med_above)
print(summary(step3)$coefficients)

# Causal mediation (Imai et al. 2010)
set.seed(42)
med_m <- lm(effort_luck ~ treat_bin, data = df_med_above)
med_y <- lm(redist_merit ~ treat_bin + effort_luck, data = df_med_above)

med_out <- mediate(med_m, med_y,
                   treat    = "treat_bin",
                   mediator = "effort_luck",
                   robustSE = TRUE,
                   sims     = 1000)
cat("\nFormal mediation (Imai et al. 2010) — above vs no_feedback:\n")
summary(med_out)

# Repeat with below vs no_feedback
df_med_below <- df_beliefs %>%
  filter(treatment %in% c("no_feedback", "below")) %>%
  mutate(treat_bin = as.integer(treatment == "below"))

cat("\nFormal mediation — below vs no_feedback:\n")
med_m2  <- lm(effort_luck ~ treat_bin, data = df_med_below)
med_y2  <- lm(redist_merit ~ treat_bin + effort_luck, data = df_med_below)
med_out2 <- mediate(med_m2, med_y2,
                    treat    = "treat_bin",
                    mediator = "effort_luck",
                    robustSE = TRUE,
                    sims     = 1000)
summary(med_out2)

# =============================================================================
# 9. ADDITIONAL EXPLORATORY ANALYSES
# =============================================================================

cat("\n========== ADDITIONAL ANALYSES ==========\n\n")

# 9a. Pre-feedback belief accuracy (were people well-calibrated?)
cat("Pre-feedback relative performance prediction vs actual outcome:\n")
df_clean %>%
  filter(group == "relative") %>%
  mutate(pred_correct = (pred_better == (pair_result == "above"))) %>%
  summarise(
    pct_correct = round(100 * mean(pred_correct), 1),
    pct_predicted_better = round(100 * mean(pred_better), 1),
    pct_actual_above     = round(100 * mean(pair_result == "above"), 1)
  ) %>%
  print()

# 9b. Overconfidence: most people predict they'll do better
cat("\nSelf-reported relative performance (pre-feedback):\n")
df_clean %>%
  count(group, sr_relPerf) %>%
  group_by(group) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  print()

# 9c. Heterogeneity: does the treatment effect differ by education or country?
cat("\nTreatment effect heterogeneity — by higher education:\n")
df_clean %>%
  mutate(high_edu = education >= "master") %>%
  group_by(high_edu, treatment) %>%
  summarise(mean = round(mean(redist_merit), 2), n = n(), .groups = "drop") %>%
  print()

# 9d. Libertarian types: who chose full no-redistribution?
cat("\nShare choosing no redistribution (merit), by group:\n")
df_clean %>%
  group_by(treatment) %>%
  summarise(
    pct_no_redist = round(100 * mean(dist_no_redist), 1),
    n = n(),
    .groups = "drop"
  ) %>%
  print()

# 9e. Country breakdown (main countries)
cat("\nRedistribution by country of residence (top countries):\n")
df_clean %>%
  count(country) %>%
  arrange(desc(n)) %>%
  head(6) %>%
  left_join(
    df_clean %>%
      group_by(country) %>%
      summarise(merit_mean = round(mean(redist_merit), 2), .groups = "drop"),
    by = "country"
  ) %>%
  print()

# =============================================================================
# 10. FIGURES
# =============================================================================

theme_paper <- theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position  = "bottom"
  )

group_colors <- c("no_feedback" = "#6baed6",
                  "above"       = "#74c476",
                  "below"       = "#fb6a4a")
group_labels <- c("no_feedback" = "No feedback",
                  "above"       = "Told: Above",
                  "below"       = "Told: Below")

# --- Figure 1: Mean merit redistribution by group (bar + CI) ---
sum_merit <- df_clean %>%
  group_by(treatment) %>%
  summarise(
    mean = mean(redist_merit),
    se   = sd(redist_merit) / sqrt(n()),
    n    = n(),
    .groups = "drop"
  )

fig1 <- ggplot(sum_merit, aes(x = treatment, y = mean, fill = treatment)) +
  geom_col(width = 0.55, color = "black", linewidth = 0.4, alpha = 0.85) +
  geom_errorbar(aes(ymin = mean - 1.96 * se, ymax = mean + 1.96 * se),
                width = 0.2, linewidth = 0.8) +
  geom_hline(yintercept = 4, linetype = "dashed", color = "gray40") +
  annotate("text", x = 0.55, y = 4.1, label = "Equal split", size = 3.5,
           color = "gray40", hjust = 0) +
  scale_x_discrete(labels = group_labels) +
  scale_y_continuous(limits = c(0, 5), breaks = 0:5) +
  scale_fill_manual(values = group_colors, guide = "none") +
  labs(
    title    = "Merit Redistribution by Treatment Group",
    subtitle = "Amount allocated to lower-earning worker (Decision 1) | Error bars: 95% CI",
    x        = NULL,
    y        = "Amount to Worker B (USD, 0–8)",
    caption  = paste0("N = ", sum(sum_merit$n))
  ) +
  theme_paper
print(fig1)

# --- Figure 2: Merit vs Luck redistribution (within-subjects, by group) ---
if (nrow(df_both) > 0) {
  sum_both <- df_both %>%
    group_by(treatment) %>%
    summarise(
      merit_mean = mean(redist_merit),
      merit_se   = sd(redist_merit) / sqrt(n()),
      luck_mean  = mean(redist_luck),
      luck_se    = sd(redist_luck) / sqrt(n()),
      n          = n(),
      .groups    = "drop"
    ) %>%
    pivot_longer(cols = c(merit_mean, luck_mean, merit_se, luck_se),
                 names_to      = c("scenario", ".value"),
                 names_pattern = "^(merit|luck)_(mean|se)$")

  fig2 <- ggplot(sum_both,
                 aes(x = scenario, y = mean, group = treatment, color = treatment)) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = mean - 1.96 * se, ymax = mean + 1.96 * se),
                  width = 0.08, linewidth = 0.8) +
    geom_hline(yintercept = 4, linetype = "dashed", color = "gray50") +
    scale_x_discrete(labels = c("merit" = "Merit\n(Decision 1)",
                                 "luck"  = "Luck\n(Decision 2)")) +
    scale_color_manual(values = group_colors, labels = group_labels) +
    labs(
      title    = "Redistribution: Merit vs Luck, by Treatment Group",
      subtitle = paste0("N = ", nrow(df_both), " participants with both decisions"),
      x        = "Scenario",
      y        = "Amount to lower-earning worker (USD)",
      color    = NULL
    ) +
    theme_paper
  print(fig2)
}

# --- Figure 3: Effort-vs-luck belief distribution by group ---
fig3 <- ggplot(df_beliefs, aes(x = treatment, y = effort_luck, fill = treatment)) +
  geom_violin(alpha = 0.55, trim = FALSE) +
  geom_boxplot(width = 0.14, outlier.shape = NA, alpha = 0.9) +
  geom_jitter(width = 0.07, alpha = 0.35, size = 1.5) +
  scale_x_discrete(labels = group_labels) +
  scale_fill_manual(values = group_colors, guide = "none") +
  labs(
    title    = "Effort-vs-Luck Beliefs by Treatment Group",
    subtitle = "Slider: 0 = 100% luck, 100 = 100% effort",
    x        = NULL,
    y        = "Effort-vs-luck belief (0–100)",
    caption  = paste0("N = ", nrow(df_beliefs))
  ) +
  theme_paper
print(fig3)

# --- Figure 4: Redistribution ~ effort-luck belief (scatter + fit) ---
fig4 <- ggplot(df_beliefs,
               aes(x = effort_luck, y = redist_merit, color = treatment)) +
  geom_jitter(width = 1, height = 0.15, alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.15, linewidth = 1) +
  scale_color_manual(values = group_colors, labels = group_labels) +
  scale_y_continuous(breaks = 0:8) +
  labs(
    title    = "Merit Redistribution vs Effort-vs-Luck Belief",
    subtitle = "Each dot is one participant | Lines: group-specific OLS fit",
    x        = "Effort-vs-luck belief (0 = luck, 100 = effort)",
    y        = "Amount to Worker B — merit scenario (USD)",
    color    = NULL
  ) +
  theme_paper
print(fig4)

# --- Figure 5: Within-person luck gap distribution ---
if (nrow(df_both) > 0) {
  fig5 <- ggplot(df_both, aes(x = redist_gap, fill = treatment)) +
    geom_histogram(binwidth = 1, color = "white", alpha = 0.8,
                   position = "identity") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    facet_wrap(~ treatment, labeller = as_labeller(group_labels), ncol = 1) +
    scale_fill_manual(values = group_colors, guide = "none") +
    labs(
      title    = "Within-Person Redistribution Gap (Luck − Merit)",
      subtitle = "Positive = redistributed more under luck than merit",
      x        = "Luck redistribution − Merit redistribution (USD)",
      y        = "Count",
      caption  = paste0("N = ", nrow(df_both))
    ) +
    theme_paper
  print(fig5)
}

# =============================================================================
# 11. REGRESSION TABLE (summary for paper)
# =============================================================================

cat("\n========== REGRESSION TABLE SUMMARY ==========\n\n")

models <- list(
  "(1) Feedback only"    = lm(redist_merit ~ has_feedback, data = df_clean),
  "(2) Three groups"     = lm(redist_merit ~ treatment, data = df_clean),
  "(3) + controls"       = lm(redist_merit ~ treatment + age + female + score + mobile,
                               data = df_clean),
  "(4) + belief"         = lm(redist_merit ~ treatment + effort_luck,
                               data = df_beliefs),
  "(5) + belief + ctrls" = lm(redist_merit ~ treatment + effort_luck +
                                 age + female + score + mobile,
                               data = df_beliefs)
)

map_dfr(names(models), function(nm) {
  m   <- models[[nm]]
  rob <- coeftest(m, vcov = vcovHC(m, "HC3"))
  tibble(
    model         = nm,
    N             = nobs(m),
    R2            = round(summary(m)$r.squared, 3),
    adj_R2        = round(summary(m)$adj.r.squared, 3),
    has_feedback  = if ("has_feedbackTRUE" %in% rownames(rob))
      sprintf("%.3f (%.3f)", rob["has_feedbackTRUE","Estimate"],
              rob["has_feedbackTRUE","Std. Error"]) else NA,
    above         = if ("treatmentabove" %in% rownames(rob))
      sprintf("%.3f (%.3f)", rob["treatmentabove","Estimate"],
              rob["treatmentabove","Std. Error"]) else NA,
    below         = if ("treatmentbelow" %in% rownames(rob))
      sprintf("%.3f (%.3f)", rob["treatmentbelow","Estimate"],
              rob["treatmentbelow","Std. Error"]) else NA,
    effort_luck   = if ("effort_luck" %in% rownames(rob))
      sprintf("%.3f (%.3f)", rob["effort_luck","Estimate"],
              rob["effort_luck","Std. Error"]) else NA
  )
}) %>%
  print(width = Inf)

cat("\nNote: Robust SE (HC3) in parentheses. Reference group: no_feedback.\n")
cat("Outcome: amount allocated to lower-earning worker (USD, 0-8).\n")
