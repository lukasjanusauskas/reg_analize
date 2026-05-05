library("survival")
library("survminer")

df <- read.csv('turnover.csv', header = TRUE)

df

# Cenzuruotu stebejimu dalis
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

# K-M kreivės
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
