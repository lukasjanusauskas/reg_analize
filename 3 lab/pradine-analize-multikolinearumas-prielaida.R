library("survival")
library("survminer")
library("tidyr")
library("dplyr")
library(car)
library(StepReg)
library(Rcmdr)
library(RcmdrPlugin.survival)
library(MASS)
# duomenys iš https://www.kaggle.com/datasets/davinwijaya/employee-turnover?resource=download
df <- read.csv('turnover.csv', header = TRUE)

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

# Duomenu pradine analize: tvarkymas, klaidos

colnames(df)

# https://www.perplexity.ai/search/5a1bb40a-8d2f-4925-81c7-1ce626b4292f#2

# stag: Darbuotojo darbo stažas (mėnesiais)
# event: Pasitraukimo įvykio indikatorius (1=išėjo, 0=cenzūruotas/liko) 
# gender: Darbuotojo lytis (vyras/moteris)
# age: Darbuotojo amžius (metai), kelerių pradėjo dirbti
# industry: Pramonės šaka
# profession: Darbo pozicija/profesija
# traffic: Verbavimo šaltinis/kanalas
# coach: Turėjo mentorių bandomuoju laikotarpiu (taip/ne)
# head_gender: Vadovo/viršininko lytis
# greywage: Neoficialus šešėlinis atlyginimas (taip/ne arba suma)
# way: Važiavimo į darbą būdas (pėstute/autobusu/automašina/ir t.t.)
# extraversion: Big5 ekstraversijos balas (bendraujantis)
# independ: Nepriklausomybės/savarankiškumo balas
# selfcontrol: Savikontrolės/sąžiningumo balas
# anxiety: Neuroticizmo/nerimo balas
# novator: Novatoriškumo/atvirumo patirčiai balas
  

summary(df)

nrow(df)

# Nepastebetos iskirtys

any( is.na(df) )

# Nera praleistu reiksniu

# sklaidos diagrama

# Pivot the predictor variables into long format
df_long <- df %>%
  pivot_longer(
    cols = c(age, extraversion, independ, selfcontrol, anxiety, novator),
    names_to  = "variable",
    values_to = "value"
  )

# Plot
ggplot(df_long, aes(x = value, y = stag, color = factor(event))) +
  geom_point(alpha = 0.6, size = 1.8) +
  geom_smooth(method='lm') +
  facet_wrap(~ variable, scales = "free_x") +
  scale_color_brewer(palette = "Set1", name = "Event") +
  labs(
    x     = "Reikšmė",
    y     = "Stažas"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    strip.text       = element_text(face = "bold"),
    legend.position  = "bottom"
  )

# Skirtingomis spalvomis cenzuruotus ir necenzutuotus

table(df$event)
cat( "Cenzūruotų duomenų dalis:", mean( 1-df$event ) )


# kovarianciu tyrimo:
colnames(df)

table(df$industry)
table(df$gender)
table(df$profession)
table(df$traffic)
table(df$coach)
table(df$head_gender)
table(df$greywage)
table(df$way)

cols <- c("industry", "gender", "profession", "traffic", "coach", "head_gender", "greywage", "way")
df[cols] <- lapply(df[cols], as.factor)

# Identify numeric columns (excluding the event column)
numeric_cols <- df %>%
  select(-event) %>%                  # Step 1: drop event
  select(where(is.numeric)) %>%       # Step 2: keep only numeric columns
  colnames()

# Reshape data to long format for faceting
df_long <- df %>%
  select(event, all_of(numeric_cols)) %>%
  pivot_longer(
    cols      = all_of(numeric_cols),
    names_to  = "variable",
    values_to = "value"
  )

# Plot
ggplot(df_long, aes(x = factor(event), y = value, fill = factor(event))) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 1.5, alpha = 0.7) +
  stat_summary(                        # <-- mean marker
    fun       = mean,
    geom      = "point",
    shape     = 23,                    # diamond shape
    size      = 3,
    fill      = "#FF0000",             # bright red fill
    color     = "black",               # black border for contrast
    position  = position_dodge(0.75)
  ) +
  facet_wrap(~ variable, scales = "free_y") +
  scale_fill_manual(
    values = c("left" = "#E74C3C", "stayed" = "#2ECC71"),
    labels = c("left" = "Left", "stayed" = "Stayed")
  ) +
  labs(
    title = "Požymių pasiskirstymas pagal įvykį",
    x     = "Įvykis",
    y     = "Kintamojo vertė",
    fill  = "Įvykis"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title      = element_text(face = "bold", hjust = 0.5),
    strip.text      = element_text(face = "bold"),
    legend.position = "bottom"
  )

# K-M kreivės ############################################
# Bendra
fit <- coxph(Surv(stag, event, type = "right") ~ ., data = df)
ggadjustedcurves(fit, data = df)

# Pagal lyti
fit <- survfit(Surv(stag, event) ~ gender, data = df)
ggsurvplot(fit, data = df)

# Pagal algos apmokejima (grey - )
fit <- survfit(Surv(stag, event) ~ greywage, data = df)
ggsurvplot(fit, data = df)

# 
fit <- survfit(Surv(stag, event) ~ way, data = df)
ggsurvplot(fit, data = df)

fit <- survfit(Surv(stag, event) ~ profession, data = df)
ggsurvplot(fit, data = df)

# Diskretizuojam kikekybines kovariantes 
numeric_cols <- df %>%
  select(stag, where(is.numeric)) %>% 
  colnames()

numeric_cols

draw_numeric_discretized_km <- function(
    df_numeric_col,
    df_stag,
    df_event,
    col,
    na.rm = TRUE
) {
  breaks <- quantile(df_numeric_col, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = na.rm)
  
  df_numeric_col <- cut(df_numeric_col,
                        breaks         = breaks,
                        labels = c(
                          paste0(col, "Q1"),
                          paste0(col, "Q2"),
                          paste0(col, "Q3"),
                          paste0(col, "Q4")
                        ),
                        include.lowest = TRUE)
  
  temp_df <- data.frame(
    stag  = df_stag,
    event = df_event,
    group = df_numeric_col
  )
  
  fit <- survfit(Surv(stag, event) ~ group, data = temp_df)
  ggsurvplot(fit, data = temp_df)
}

draw_numeric_discretized_km(df$age, df$stag, df$event, col="Amžius")
draw_numeric_discretized_km(df$extraversion, df$stag, df$event, col="Extraversija")
draw_numeric_discretized_km(df$independ, df$stag, df$event, col="Savarankiškumas")
draw_numeric_discretized_km(df$selfcontrol, df$stag, df$event, col="Savikontrolė")
draw_numeric_discretized_km(df$anxiety, df$stag, df$event, col="Nerimas")
draw_numeric_discretized_km(df$novator, df$stag, df$event, col="Novatoriškumas")

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

step_model <- StepReg::stepwise(
  formula = formula,
  data = df_train,
  type = "cox",
  strategy = "backward",
  metric = "SL",
  sle = 0.1,
  sls = 0.1
)
final_model <- step_model$backward$SL
summary(final_model)
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



model_tv2 <- coxph(
  Surv(start, stop, event) ~ . + novator:stop + selfcontrol:stop + profession:stop + industry:stop + traffic:stop + extraversion:stop + head_gender:stop + coach:stop + independ:stop,
  data = df_train_split,
  ties = "efron",
  x = TRUE
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
model_tv1 <- coxph(
  Surv(start, stop, event) ~ 
    strata(profession) +
    strata(traffic) +
    strata(novator_q) +
    . - start - stop - event - profession - traffic - novator - novator_q - t_stop,
  data = df_train_split,
  ties = "efron",
  x = TRUE,
  control = coxph.control(iter.max = 100)
)

summary(model_tv1)

table(df_train$industry,df_train$event)

summary(model_tv1)
cox.zph(model_tv1)

summary(model_tv2)
cox.zph(model_tv2)

table(df_train$industry, df_train$event)

step_model <- MASS::stepAIC(model_tv1, direction = "both")
cox.zph(step_model)
summary(step_model)

# Su profesija p < 0.05 - neatitinka 

# Susitvarkyti su profesija taip:
# 1. Įvesti sąveiką su laiku - kovariantė lieka
# 2. Sluoksniuoti (atskiros lygtys) - apie kovariantę daryti išvadų negalim. 
#    Mums netinka



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

