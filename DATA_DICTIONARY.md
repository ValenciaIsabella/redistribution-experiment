# Data Dictionary - Results Variables

Complete explanation of all variables collected in the JATOS study results.

---

## Task Performance (Part 1: Code Recognition)

### `group` (string)
- **Type**: Categorical
- **Values**: `"none"`, `"relative"`
- **Meaning**: Feedback group assignment (determines which results screen participant sees)
  - `"none"` → Participant sees only "Assignment Complete"
  - `"relative"` → Participant sees their score + random pairing result

### `totalScore` (number)
- **Type**: Integer
- **Meaning**: Total points earned across all 5 rounds
- **Calculation**: Sum of (correct markings - wrong markings) for all rounds
- **Example**: 19 points

### `totalCorrect` (number)
- **Type**: Integer
- **Meaning**: Total number of correct codes marked across all 5 rounds
- **Example**: 20 correct markings

### `totalWrong` (number)
- **Type**: Integer
- **Meaning**: Total number of incorrect codes marked across all 5 rounds
- **Example**: 1 wrong marking

### `roundResults` (array of objects)
- **Type**: Array
- **Meaning**: Detailed breakdown of performance for each of the 5 task rounds
- **Structure**: Each object contains:
  - `round` (number): Round number (1-5)
  - `target` (string): The 3-digit target code for that round
  - `correct` (number): Correct markings in that round
  - `wrong` (number): Wrong markings in that round
  - `score` (number): Points for that round (correct - wrong)

### `practiceScore` (number)
- **Type**: Integer
- **Meaning**: Points earned during the 15-second practice round
- **Calculation**: practice_correct - practice_wrong
- **Note**: -2 means participant made 2 wrong markings with 0 correct

### `practiceCorrect` (number)
- **Type**: Integer
- **Meaning**: Number of correct codes marked during practice

### `practiceWrong` (number)
- **Type**: Integer
- **Meaning**: Number of incorrect codes marked during practice

---

## Self-Report Questions (Before Feedback - Part 1)

### `sr_effort` (number)
- **Type**: Integer (0-10 scale)
- **Question**: "How much effort did you put into the task?"
- **Range**: 0 (No effort at all) to 10 (Maximum effort)
- **Example**: 1 (very low effort)

### `sr_guess` (number)
- **Type**: Integer
- **Question**: "The maximum amount of points you could have obtained was 200. How many do you think you got?"
- **Meaning**: Participant's self-reported guess of their total score
- **Example**: 2 (participant guessed they scored 2 points)

### `sr_relPerf` (string)
- **Type**: Categorical
- **Question**: "If you were randomly paired with another participant, do you think you would likely perform better or worse than them?"
- **Values**: 
  - `"much_better"` → Better or the same as the other participant
  - `"much_worse"` → Worse than the other participant
- **Example**: `"much_better"`

---

## Feedback & Pairing (Part 1 Results Distribution)

### `pair_score` (number)
- **Type**: Integer
- **Meaning**: Score of the randomly paired previous participant for comparison
- **Only populated if**: `group == "relative"`
- **Example**: 200 (the pair scored 200 points)

### `pair_result` (string)
- **Type**: Categorical
- **Meaning**: Whether participant scored above or below their randomly paired partner
- **Values**:
  - `"above"` → Participant scored HIGHER than pair
  - `"below"` → Participant scored LOWER than pair
  - `null` → If `group == "none"` (no pairing shown)
- **Example**: `"below"` (participant's 19 < pair's 200)

---

## Distribution Phase (Part 2: Allocation Decision)

### `dist_treatment` (string)
- **Type**: Categorical
- **Meaning**: Which scenario the participant was assigned to
- **Values**:
  - `"luck"` → Both workers earned randomly (luck-based)
  - `"merit"` → Both workers earned based on task performance (merit-based)
- **Example**: `"merit"`

### `dist_choice_idx` (number)
- **Type**: Integer (0-based index)
- **Meaning**: Which allocation option the participant selected
- **How to interpret**: Index into the DIST_OPTIONS array
- **Example**: 3 (means they selected the 4th option in the list)

### `dist_worker_a_usd` (number)
- **Type**: Float/Currency
- **Unit**: USD
- **Meaning**: Amount allocated to Worker A
- **Range**: 0 to 8
- **Example**: 5 (allocated $5.00 to Worker A)

### `dist_worker_b_usd` (number)
- **Type**: Float/Currency
- **Unit**: USD
- **Meaning**: Amount allocated to Worker B
- **Range**: 0 to 8
- **Note**: Worker A + Worker B should equal 8 (or less if choosing no redistribution)
- **Example**: 3 (allocated $3.00 to Worker B)

### `dist_no_redist` (boolean)
- **Type**: Boolean
- **Meaning**: Whether participant chose "no redistribution" option
- **Values**:
  - `true` → Participant chose not to redistribute any money
  - `false` → Participant chose one of the redistribution options
- **Example**: `false` (they did redistribute)

---

## Demographics (Part 2: Background Information)

### `demo_gender` (string)
- **Type**: Categorical
- **Question**: Gender
- **Values**: `"male"`, `"female"`, `"other"`, `"prefer-not"`
- **Example**: `"prefer-not"`

### `demo_age` (number)
- **Type**: Integer
- **Question**: Age
- **Range**: 18+
- **Example**: 22

### `demo_country` (string)
- **Type**: Categorical (Country selection)
- **Question**: Country of Residence
- **Values**: Any country from dropdown list
- **Example**: `"Bahrain"`

### `demo_education` (string)
- **Type**: Categorical
- **Question**: What is your highest level of education?
- **Values**:
  - `"high-school"` → High School
  - `"some-college"` → Some College
  - `"bachelor"` → Bachelor's Degree
  - `"master"` → Master's Degree
  - `"phd"` → PhD or higher
- **Example**: `"master"`

### `demo_parent_education` (string)
- **Type**: Categorical
- **Question**: What is the highest level of education of your parents?
- **Values**:
  - `"high-school"` → High School or less
  - `"some-college"` → Some College
  - `"bachelor"` → Bachelor's Degree
  - `"master"` → Master's Degree or higher
- **Example**: `"bachelor"`

### `demo_occupationStatus` (string)
- **Type**: Categorical
- **Question**: What best describes your current occupation status?
- **Values**:
  - `"employed"` → Employed
  - `"self-employed"` → Self-employed
  - `"student"` → Student
  - `"working-and-studying"` → Working and studying
  - `"unemployed"` → Unemployed
- **Example**: `"working-and-studying"`

### `demo_occupationField` (string)
- **Type**: Text
- **Question**: What field do you work or study in?
- **Values**: Free text (e.g., Engineering, Healthcare, Business)
- **Example**: `"Economics"`

### `demo_emailResults` (string or null)
- **Type**: Email string or null
- **Question**: Would you like us to let you know if your allocation decision was selected? (Optional)
- **Meaning**: Email address to contact participant if their decision was selected
- **Example**: `null` (participant did not provide email)

### `demo_comments` (string or null)
- **Type**: Text or null
- **Question**: Additional Comments (Optional)
- **Meaning**: Feedback or suggestions about the study
- **Example**: `null` (no comments provided)

### `demo_knowSelection` (string or null)
- **Type**: Categorical or null
- **Question**: Would you like to know if your allocation decision was selected? (Optional)
- **Values**: `"yes"`, `"no"`, or `null` (if not answered)
- **Example**: `null`

---

## Technical/Device Information

### `device_type` (string)
- **Type**: Categorical
- **Meaning**: Type of device used to complete the study
- **Values**:
  - `"mobile"` → Smartphone/tablet (detected by screen width ≤ 768px or mobile user agent)
  - `"desktop"` → Desktop/laptop computer
- **Implication**: Affects matrix dimensions:
  - Desktop: 20 columns × 16 rows
  - Mobile: 16 columns × 20 rows
- **Example**: `"desktop"`

---

## Summary Statistics Example

Based on the example data:
```json
{
  "participantPerformance": {
    "totalScore": 19,
    "correctRate": "20/21 = 95.2%",
    "roundsAboveZero": 2,
    "roundsZeroScore": 3
  },
  "comparisonToPair": {
    "participantScore": 19,
    "pairScore": 200,
    "percentile": "bottom 5%"
  },
  "allocation": {
    "toWorkerA": 5,
    "toWorkerB": 3,
    "totalAllocated": 8,
    "pattern": "More to worker A than B"
  }
}
```

---

## Data Quality Notes

- **Null values** indicate optional questions that participant did not answer
- **Validation**: Email fields should match email format if provided
- **Pairing**: Only populated for `group == "relative"`
- **roundResults**: Always contains exactly 5 objects (one per round)
- All monetary values are in USD
- All times are in seconds

