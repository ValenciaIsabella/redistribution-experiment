# Study Design — Redistribution Experiment

**Researcher**: Isabella Valencia, Paris School of Economics
**Supervisor**: Prof. Andrea FM Martinangeli
**Platform**: JATOS (browser-based, single HTML file `redist.html`)
**Duration**: ~7–10 minutes
**Languages**: English / Spanish (auto-detected or manual toggle)

---

## Overview

Two-part experiment examining how people make redistribution decisions. In Part 1, participants complete a real-effort task and answer self-report questions. In Part 2, they decide how to allocate earnings between two workers under different scenarios.

---

## Flow of Screens

### Screen 0 — Welcome & Consent
- Study introduction (2 parts: Performance Task + Decision Task)
- Honesty statement: no correct answers, no trick questions
- Informed consent (GDPR, voluntary participation, anonymity)
- Participant must click "Accept and Start Experiment" to continue

---

### Screen 0.5 — Early Demographics (mandatory)
Collected before the task to avoid anchoring effects from performance feedback.

| Variable | Question | Type |
|---|---|---|
| `demo_gender` | Gender | Male / Female / Non-binary / Prefer not to say |
| `demo_age` | Age | Integer ≥ 18 |
| `demo_country_residence` | Country of Residence | Dropdown |

---

### Screen 1 — Task Instructions
- **Task**: Code recognition — find and click all instances of a 3-digit target code in a grid
- **Scoring**: +1 per correct marking, −1 per wrong marking, 0 for missed codes
- **Structure**: 5 rounds × 45 seconds = 3 min 45 sec
- **Max possible score**: ~200 points
- Grid dimensions: 20×16 (desktop) or 16×20 (mobile)

---

### Screen 1.3 — Visual Example
- Static simplified grid showing what the task looks like
- Warm-up before the practice round

---

### Screen 1.5 — Practice Round (20 seconds)
- Same format as the real task but with a random target code
- **Does not count** toward final score
- Purpose: familiarize participant with the interface

---

### Screen 1.6 — Practice Feedback
- Shows correct markings, wrong markings, and practice score
- Stored as `practiceScore`, `practiceCorrect`, `practiceWrong`

---

### Screen 2 — Real Task (5 rounds × 45 seconds)
- Each round has a new randomly generated 3-digit target code
- Timer bar visible; matrix resets between rounds
- Per-round data stored in `roundResults` array:
  - `round`, `target`, `correct`, `wrong`, `score`
- Aggregates: `totalScore`, `totalCorrect`, `totalWrong`

---

### Screen 3 — Self-Report (shown before feedback)
Elicits beliefs and effort perceptions **before** participants see any results.

| Variable | Question | Scale |
|---|---|---|
| `sr_effort` | How much effort did you put into the task? | Slider 0–10 |
| `sr_guess` | How many points do you think you got? (max 200) | Integer −200 to 200 |
| *(unrecorded)* | In a group of 100 participants, how many performed better than you? | Integer 0–100 |
| `sr_relPerf` | Would you perform better or worse than a randomly paired participant? | `much_better` / `much_worse` |

> Note: the "percentile" question (Q2.2) is displayed but not currently saved to the results payload.

---

### Screen 4 — Feedback (between-subjects)

Participants are randomly assigned to one of **two feedback conditions** at study load:

| Group | Probability | What participant sees |
|---|---|---|
| `none` | 1/3 | "You are done with Part 1" — no score shown |
| `relative` | 2/3 | Comparison to a randomly paired previous participant |

**Pairing logic (group `relative`)**:
- Participant is paired with a score drawn from `PAIR_SCORES = [16, 198]`
- **50% probability** of being paired with the best score (198)
- **50% probability** of being paired with the worst score (16)
- `pair_result` is `"above"` if participant scored higher, `"below"` if lower
- Participant only sees above/below — **the pair's actual score is not shown**

> Two additional feedback variants exist in the code (`score` = score only; `full` = score + pair score shown numerically) but are currently inactive.

---

### Screen 5 — Distribution Decision 1 (merit scenario)

Participant allocates **$8 total** between two fictitious workers.

**Scenario (currently always merit)**:
> Worker A got the highest score in the task and initially earned $8. Worker B earned $0.

Participant chooses from **9 options** (in $1 increments):

| Option | Worker A | Worker B | No-redistribution flag |
|---|---|---|---|
| 1 | $8 | $0 | ✓ (no redistribution) |
| 2 | $7 | $1 | |
| 3 | $6 | $2 | |
| 4 | $5 | $3 | |
| 5 | $4 | $4 | |
| 6 | $3 | $5 | |
| 7 | $2 | $6 | |
| 8 | $1 | $7 | |
| 9 | $0 | $8 | |

Recorded as: `dist_treatment`, `dist_choice_idx`, `dist_worker_a_usd`, `dist_worker_b_usd`, `dist_no_redist`

> A luck condition (`dist_treatment = "luck"`) is implemented in the code but currently disabled (`Math.random() < 0` is always false). Under luck, the framing would be: "Worker A won the lottery and earned $8; Worker B earned $0."

---

### Screen 6 — Late Demographics

Collected after the distribution decision, before the second distribution task.

| Variable | Question | Type |
|---|---|---|
| `demo_education` | Highest level of education | 5 levels (HS → PhD) |
| `demo_parent_education` | Parents' highest education | 4 levels (HS → Master+) |
| `demo_country_origin` | Country of Origin | Dropdown (optional) |
| `demo_occupationStatus` | Current occupation status | 5 categories |
| `demo_occupationField` | Field of work/study | Free text |
| `demo_effortLuck` | To what extent was income in the task determined by effort vs. luck? | Slider 0 (100% luck) – 100 (100% effort) |
| `demo_comments` | Additional comments | Free text (optional) |
| `demo_knowSelection` | Want to know if your decision was selected? | Yes / No (optional) |

---

### Screen 7 — Distribution Decision 2 (luck scenario, always)

Identical format to Screen 5, but the framing is **always luck-based**.

**Scenario**:
> Worker C and Worker D completed the same task. Initial earnings were assigned by lottery — Worker C won and received $8; Worker D received $0.

Same 9 allocation options as Distribution 1.

Recorded as: `dist2_choice_idx`, `dist2_worker_c_usd`, `dist2_worker_d_usd`, `dist2_no_redist`

> The comparison between Distribution 1 (merit) and Distribution 2 (luck) is a **within-subjects** manipulation: every participant faces both scenarios, allowing direct measurement of whether the justification (merit vs. luck) affects redistribution preferences.

---

### Screen 8 — Study Complete
- Thank-you message + confirmation that responses were recorded
- Performance feedback shown (correct, wrong, total score)
- If `group ≠ "none"` and pairing occurred, the comparison result is shown
- Results submitted to JATOS **immediately** when participant clicks the final button on Screen 7
- Screen 8 is displayed for **20 seconds** before the study closes automatically

---

## Randomization Summary

| Factor | Type | Levels | Assignment |
|---|---|---|---|
| Feedback group (`GROUP`) | Between-subjects | `none` (1/3), `relative` (2/3) | Random at page load |
| Pair score | Within `relative` group | 198 (50%), 16 (50%) | Random at feedback screen |
| Distribution treatment (`DIST_TREATMENT`) | Fixed | Always `merit` | Hardcoded (luck disabled) |
| Distribution 2 | Fixed | Always luck | Hardcoded |

---

## Key Research Comparisons

1. **Effect of relative performance feedback** on redistribution: compare `none` vs `relative` groups on `dist_choice_idx`
2. **Effect of income source** (merit vs. luck) on redistribution: within-subjects comparison of Distribution 1 (`merit`) vs. Distribution 2 (`luck`)
3. **Role of beliefs about effort/luck** (`demo_effortLuck`) as a mediator of redistribution decisions
4. **Overconfidence / self-serving bias**: compare `sr_relPerf` and `sr_guess` against actual `totalScore`

---

## Technical Notes

- Single HTML file; no server-side logic beyond JATOS result submission
- `jatos.submitResultData()` sends the full JSON payload on Screen 7 button click
- Device type detected by screen width (≤768px = mobile); affects grid dimensions
- All monetary values in USD; all times in seconds
