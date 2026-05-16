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
df_long %>% 
  mutate(
    event = ifelse(event == 1, 'Paliko komp.', 'Pasiliko komp.'),
    variable = case_when(
      variable == "age"          ~ "Amžius",
      variable == "extraversion" ~ "Ekstraversija",
      variable == "independ"     ~ "Savarankiškumas",
      variable == "selfcontrol"  ~ "Savikontrolė",
      variable == "anxiety"      ~ "Nerimas",
      variable == "novator"      ~ "Novatoriškas"
    )
  ) %>% 
  ggplot(aes(x = value, y = stag, color = factor(event))) +
  geom_point(alpha = 0.6, size = 1.8) +
  geom_smooth(method = 'lm') +
  facet_wrap(~ variable, scales = "free_x") +
  scale_color_brewer(palette = "Set1", name = "Įvykis") +
  labs(
    x = "Reikšmė",
    y = "Stažas"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    strip.text      = element_text(face = "bold"),
    legend.position = "bottom"
  )

ggsave('initial-anal-scatter-plot.png', dpi=750)

# Skirtingomis spalvomis cenzuruotus ir necenzutuotus

table(df$event)
cat( "Cenzūruotų duomenų dalis:", mean( 1-df$event ) )


# kovarianciu tyrimo:
colnames(df)

table(df$industry)
table(df$profession)

table(df$gender)
table(df$traffic)
table(df$coach)
table(df$head_gender)
table(df$greywage)
table(df$way)

cols <- c("industry", "gender", "profession", "traffic", "coach", "head_gender", "greywage", "way")
df[cols] <- lapply(df[cols], as.factor)

# Identify numeric columns (excluding the event column)
numeric_cols <- df %>%
  dplyr::select(-event) %>%                  # Step 1: drop event
  dplyr::select(where(is.numeric)) %>%       # Step 2: keep only numeric columns
  colnames()

# Reshape data to long format for faceting
df_long <- df %>%
  dplyr::select(event, all_of(numeric_cols)) %>%
  pivot_longer(
    cols      = all_of(numeric_cols),
    names_to  = "variable",
    values_to = "value"
  )

# Plot
df_long %>% 
  filter(variable != "stag") %>% 
  mutate(
    event = ifelse(event == 1, 'Paliko komp.', 'Pasiliko komp.'),
    # variable = case_when(
    #   variable == "age"          ~ "Amžius",
    #   variable == "extraversion" ~ "Ekstraversija",
    #   variable == "independ"     ~ "Savarankiškumas",
    #   variable == "selfcontrol"  ~ "Savikontrolė",
    #   variable == "anxiety"      ~ "Nerimas",
    #   variable == "novator"      ~ "Novatoriškas"
    # )
  ) %>% 
ggplot(., aes(x = factor(event), y = value, fill = factor(event))) +
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

ggsave('initial-anal-box.png', dpi=750)

# K-M kreivės ############################################
# Bendra
fit <- coxph(Surv(stag, event, type = "right") ~ ., data = df)
ggadjustedcurves(fit, data = df)

# Pagal lyti
fit <- survfit(Surv(stag, event) ~ gender, data = df)
ggsurvplot(fit, data = df)

ggsave('initial-anal-km-lytis.png', dpi=750)



# Pagal algos apmokejima (grey - )
df_grey <- df %>%
  mutate(greywage = ifelse(greywage == 'grey', 'Neoficialus', 'Legalus')) %>%
  rename("Atlyginimas" = greywage)

df_grey$Atlyginimas <- as.factor(df_grey$Atlyginimas)

annotation_text <- paste(
  c(header, separator, rows),
  collapse = "\n"
)

counts    <- table(df_grey$Atlyginimas)
leg_labs  <- paste0(levels(df_grey$Atlyginimas), " (n = ", counts[levels(df_grey$Atlyginimas)], ")")

fit <- survfit(Surv(stag, event) ~ Atlyginimas, data = df_grey)

p <- ggsurvplot(
  fit,
  data         = df_grey,
  palette      = c("#E7B800", "#2E9FDF"),
  legend       = c(0.65, 0.85),   # top-right (x, y) in 0-1 plot coordinates
  legend.title = "Atlyginimas",
  legend.labs  = leg_labs,         # "Legalus (n = 120)" / "Neoficialus (n = 95)"
  ggtheme      = theme_bw()
)

p$plot <- p$plot + theme(legend.text = element_text(size = 18))

p$plot <- p$plot + theme(
  legend.text = element_text(size = 18),
  legend.title = element_text(size = 18),
  axis.title.x = element_text(size=18),
  axis.text.x = element_text(size=14),
  axis.title.y = element_text(size=18),
  axis.text.y = element_text(size=14)
)

print(p)

ggsave('initial-anal-km-atlyginimas.png', dpi=750)




# 
fit <- survfit(Surv(stag, event) ~ way, data = df)
ggsurvplot(fit, data = df)

fit <- survfit(Surv(stag, event) ~ profession, data = df)
ggsurvplot(fit, data = df)

# Diskretizuojam kikekybines kovariantes 
numeric_cols <- df %>%
  dplyr::select(stag, where(is.numeric)) %>% 
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