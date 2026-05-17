library(survival)

# 1. Create a unique ID for the strata combinations in Training
train_strata_combos <- unique(interaction(
  df_train_split$profession, 
  df_train_split$traffic, 
  df_train_split$novator_q, 
  drop = TRUE
))

# 2. Create a unique ID for the strata combinations in Test
test_strata_combos <- interaction(
  df_test_split$profession, 
  df_test_split$traffic, 
  df_test_split$novator_q, 
  drop = TRUE
)

# 1. Filter the test set again (now that NAs are fixed)
df_test_filtered <- df_test_split[test_strata_combos %in% train_strata_combos, ]

# 2. Remove any rows that still have NAs in the model variables 
# (Cox models cannot predict on NAs)
vars_in_model <- c("age", "industry", "head_gender", "greywage", "way", 
                   "profession", "traffic", "novator_q", "start", "stop", "event")
df_test_filtered <- na.omit(df_test_filtered[, vars_in_model])

df_test_filtered$risk_score <- predict(step_model, newdata = df_test_filtered, type = "lp")

test_concordance <- concordance(Surv(start, stop, event) ~ risk_score, data = df_test_filtered)
print(test_concordance)
