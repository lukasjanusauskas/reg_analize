library(RcmdrPlugin.survival)
library("survival")
library("survminer")
library("tidyr")
library("dplyr")
library(car)
library(StepReg)
library(MASS)
library(knitr)
library(ggplot2)
library(patchwork)
library(knitr)
library(kableExtra)

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

df_test_split <- survSplit(
  Surv(stag, event) ~ .,
  data = df_test,
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

df_test_split$novator_q <- cut(
  df_test_split$novator,
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

#Palyginimas

get_concordance <- function(model) {
  s <- summary(model)
  
  data.frame(
    `Konkordancijos koeficientas` = round(unname(s$concordance[1]), 3),
    `Standartinė paklaida` = round(unname(s$concordance[2]), 3),
    check.names = FALSE
  )
}

concordance_table <- bind_rows(
  cbind(Modelis = "Modelis 1", get_concordance(model_tv1)),
  cbind(Modelis = "Modelis 2", get_concordance(model_tv3)),
  cbind(Modelis = "Modelis 3", get_concordance(step_model))
)

rownames(concordance_table) <- NULL

latex_table <- concordance_table %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    caption = "Cox modelių konkordancijos koeficientų palyginimas",
    label = "tab:cox_concordance",
    align = c("l", "c", "c"),
    row.names = FALSE
  ) %>%
  kable_styling(
    latex_options = c("hold_position", "striped"),
    full_width = FALSE
  )

save_kable(latex_table, file = "cox_concordance_table.tex")


# Išėmus profesiją - all good

# Tiesiškumo tikrinimas

kiekybiniai_kintamieji <- c("age")
res_martingale <- residuals(step_model, type = "martingale")
X <- as.matrix(df_train_split[, kiekybiniai_kintamieji])
b <- coef(step_model)[kiekybiniai_kintamieji]

vertimai <- c(
  "age"          = "Amžius",
  "extraversion" = "Ekstraversiškumas",
  "independ"     = "Savarankiškumas",
  "selfcontrol"  = "Sąžiningumas",
  "anxiety"      = "Nerimastingumas",
  "novator"      = "Novatoriškumas"
)

res_martingale <- residuals(step_model, type = "martingale")
X <- as.matrix(df_train_split[, kiekybiniai_kintamieji])

lt_format <- function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)

plots <- lapply(seq_along(kiekybiniai_kintamieji), function(j) {
  df_plot <- data.frame(x = X[, j], res = res_martingale)
  
  ggplot(df_plot, aes(x = x, y = res)) +
    geom_point(size = 0.6, alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_smooth(method = "loess", span = 0.75, se = FALSE,
                color = "red", linewidth = 0.7) +
    labs(x = vertimai[kiekybiniai_kintamieji[j]], y = "Martingalo liekanos") +
    scale_x_continuous(labels = lt_format) +
    scale_y_continuous(labels = lt_format) +
    theme_bw(base_size = 9) +
    theme(
      panel.grid.minor = element_blank(),
      axis.title = element_text(size = 8),
      axis.text  = element_text(size = 7)
    )
})

combined <- wrap_plots(plots, ncol = 1)

ggsave("martingale-residuals.png", plot = combined,
       width = 4, height = 4, dpi = 750, units = "in")

plots_cr <- lapply(seq_along(kiekybiniai_kintamieji), function(j) {
  component_resid <- b[j] * X[, j] + res_martingale
  df_plot <- data.frame(x = X[, j], cr = component_resid)
  df_plot <- na.omit(df_plot)
  
  lm_fit <- lm(cr ~ x, data = df_plot)
  df_plot$lm_pred <- fitted(lm_fit)
  
  ggplot(df_plot, aes(x = x, y = cr)) +
    geom_point(size = 0.6, alpha = 0.5) +
    geom_line(aes(y = lm_pred), linetype = "dashed", linewidth = 0.6) +
    geom_smooth(method = "loess", span = 0.75, se = FALSE,
                color = "red", linewidth = 0.7) +
    labs(x = vertimai[kiekybiniai_kintamieji[j]],
         y = "Komponentė + liekana") +
    scale_x_continuous(labels = lt_format) +
    scale_y_continuous(labels = lt_format) +
    theme_bw(base_size = 9) +
    theme(
      panel.grid.minor = element_blank(),
      axis.title = element_text(size = 8),
      axis.text  = element_text(size = 7)
    )
})

combined_cr <- wrap_plots(plots_cr, ncol = 1)

ggsave("component-residuals.png", plot = combined_cr,
       width = 4, height = 4, dpi = 750, units = "in")



