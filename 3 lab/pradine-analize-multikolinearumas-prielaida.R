library("survival")
library("survminer")
library("tidyr")
library("dplyr")
library(car)

df <- read.csv('turnover.csv', header = TRUE)

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

# Nepastebetos iskirtys

any( is.na(df) )

# Nera praleistu reiksniu


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
  
  # Bug 1 fixed: assign the cut result
  df_numeric_col <- cut(df_numeric_col,
                        breaks         = breaks,
                        labels = c(
                          paste0(col, "Q1"),
                          paste0(col, "Q2"),
                          paste0(col, "Q3"),
                          paste0(col, "Q4")
                        ),
                        include.lowest = TRUE)
  
  # Bug 2 fixed: build a local data frame
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

# Multikolinearumo tyrimas ######################

model <- coxph(Surv(stag, event, type = "right") ~ ., data = df)
vif(model)

# aSGIF (GVIF^(1/(2*Df))) riba yra 2 - mes neturime kolinearumo
resid(model)

# Išskirčių tyrimas ############################


#TODO: pridėti

# Homogeniškumo hipotezė #####################

cox.zph(model)

table(df$profession)

# Su profesija p < 0.05 - neatitinka 

df_no_prof <- df%>% 
  select(-c("profession"))
  
model_no_prof <- coxph(Surv(stag, event, type = "right") ~ ., data = df_no_prof)

cox.zph(model_no_prof)

# Išėmus profesiją - all good


