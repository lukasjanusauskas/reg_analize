library("survival")
library("survminer")
library("tidyr")
library("dplyr")
library(car)
library(StepReg)
library(RcmdrPlugin.survival)
library(MASS)
library(knitr)
# duomenys iš https://www.kaggle.com/datasets/davinwijaya/employee-turnover?resource=download
df <- read.csv('turnover.csv', header = TRUE)

df %>% 
  mutate(
    event = ifelse(event == 1, 'Paliko komp.', 'Pasiliko komp.'),
    profession = iconv(profession, from = "latin1", to = "UTF-8")) %>% 
  with(table(profession, event)) %>% 
  kable(format='latex')

df %>% 
  mutate(event = ifelse(event == 1, 'Paliko komp.', 'Pasiliko komp.')) %>% 
  with(table(industry, event)) %>%
  kable(format='latex')

table(df$traffic, df$event)
table(df$industry, df$event)

prof_counts <- table(df$profession)
df <- df[df$profession %in% names(prof_counts[prof_counts >= 50]), ]
df$profession <- droplevels(as.factor(df$profession))

industry_counts <- table(df$industry)
df <- df[df$industry %in% names(industry_counts[industry_counts >= 50]), ]
df$industry <- droplevels(as.factor(df$industry))

table(df$profession)
table(df$industry)

#Mokymo testine aibe

set.seed(5)

train_prop <- 0.8

train_idx <- df %>%
  mutate(row_id = row_number()) %>%
  group_by(event) %>%
  sample_frac(train_prop) %>%
  pull(row_id)

df_train <- df[train_idx, ]
df_test  <- df[-train_idx, ]
table(df_train$traffic, df_train$event)


# Multikolinearumo tyrimas ######################

model <- coxph(Surv(stag, event, type = "right") ~ ., data = df_train)
vif(model)

# aSGIF (GVIF^(1/(2*Df))) riba yra 2 - mes neturime kolinearumo
resid(model)

# Išskirčių tyrimas ############################

dfbeta <- residuals(model, type="dfbeta")

# for (j in 1:49) {
#   plot(dfbeta[, j],
#        ylab=names(coef(model))[j])
#   abline(h=0, lty=2)
# }

konstanta = 2 / sqrt(nrow(df_train))

# Kiek stebėjimų viršija konstantą kiekvienam kintamajam
exceeded <- abs(dfbeta) > konstanta

# Procentai pagal kintamąjį
pct_by_var <- colMeans(exceeded) * 100
names(pct_by_var) <- names(coef(model))

# Bent vienas kintamasis viršija
pct_any <- mean(apply(exceeded, 1, any)) * 100

cat("Procentas stebėjimų viršijančių konstantą pagal kintamąjį:\n")
for (nm in names(pct_by_var)) {
  cat(sprintf("  %-20s: %.1f%%\n", nm, pct_by_var[nm]))
}
cat(sprintf("\n  %-20s: %.1f%%\n", "Bent vienas", pct_any))

outlier_idx  <- which(apply(exceeded, 1, any))

df_train <- df_train[-outlier_idx, ]

model <- coxph(Surv(stag, event, type = "right") ~ ., data = df_train)
summary(model)

#Steowise

formula <- Surv(stag,event, type = "right") ~ .


# Homogeniškumo hipotezė #####################
cox.zph(model)
summary(model)

interval_len <- 1

max_time <- max(df_train$stag, na.rm = TRUE)

cuts <- seq(
  from = interval_len,
  to = max_time,
  by = interval_len
)

cuts <- cuts[cuts < max_time]

df_train_split <- survSplit(
  Surv(stag, event) ~ .,
  data = df_train,
  cut = cuts,
  start = "start",
  end = "stop"
)



qs <- quantile(
  df_train_split$novator,
  probs = c(0, 0.25, 0.5, 0.75, 1),
  na.rm = TRUE
)

qs <- unique(qs)

df_train_split$novator_q <- cut(
  df_train_split$novator,
  breaks = qs,
  include.lowest = TRUE,
  labels = paste0("Q", seq_len(length(qs) - 1))
)

df_train_split$profession <- factor(as.character(df_train_split$profession))
df_train_split$traffic <- factor(as.character(df_train_split$traffic))
df_train_split$novator_q <- factor(as.character(df_train_split$novator_q))

model_tv1 <- coxph(
  Surv(start, stop, event) ~ 
    strata(profession) +
    strata(traffic) +
    strata(novator_q) +
    . - start - stop - event - profession - traffic - novator - novator_q,
  data = df_train_split,
  ties = "efron",
  x = TRUE
)

summary(model_tv1)

table(df_train$industry,df_train$event)

summary(model_tv1)
cox.zph(model_tv1)

# summary(model_tv2)
# cox.zph(model_tv2)


model_tv3 <- coxph(
  Surv(start, stop, event) ~ 
    strata(profession) +
    strata(traffic) +
    strata(novator_q) +
    . - start - stop - event - profession - traffic - novator - novator_q +
    age:industry +
    age:way,
  data = df_train_split,
  ties = "efron",
  x = TRUE
)

summary(model_tv3)
cox.zph(model_tv3)

step_model <- MASS::stepAIC(model_tv3, direction = "both")
cox.zph(step_model)
summary(step_model)

# Išėmus profesiją - all good

# Tiesiškumo tikrinimas

kiekybiniai_kintamieji <- c("age", "extraversion", "independ", "selfcontrol", "anxiety", "novator")

res_martingale <- residuals(model_tv1, type = "martingale")
X <- as.matrix(df_train_split[, kiekybiniai_kintamieji])
b <- coef(model_tv1)[kiekybiniai_kintamieji]

par(mfrow = c(2, 3))

# Liekanos ir kovariantės
for (j in seq_along(kiekybiniai_kintamieji)) {
  plot(X[, j], res_martingale,
       xlab = kiekybiniai_kintamieji[j],
       ylab = "Residuals")
  abline(h = 0, lty = 2)
  lines(lowess(X[, j], res_martingale, iter = 0), col = "red")
}

par(mfrow = c(2, 3))

# Kompnentė + liekana ir kovariantės
for (j in seq_along(kiekybiniai_kintamieji)) {
  component_resid <- b[j] * X[, j] + res_martingale
  plot(X[, j], component_resid,
       xlab = kiekybiniai_kintamieji[j],
       ylab = "Component + residual")
  abline(lm(component_resid ~ X[, j]), lty = 2)
  lines(lowess(X[, j], component_resid, iter = 0), col = "red")
}

par(mfrow = c(1, 1))

