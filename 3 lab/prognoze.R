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

test_concordance <- concordance(Surv(start, stop, event) ~ risk_score, data = df_test_filtered, reverse = TRUE)
print(test_concordance)

# ---

# Find the stratum with the most events
dense_strata <- df_train_split %>%
  group_by(profession, traffic, novator_q) %>%
  summarise(event_count = sum(event), .groups = 'drop') %>%
  arrange(desc(event_count)) %>%
  head(1)

print(dense_strata)

personas_clear <- data.frame(
  profession  = dense_strata$profession,
  traffic     = dense_strata$traffic,
  novator_q   = dense_strata$novator_q,
  age         = c(25, 65), 
  industry    = c("manufacture", "manufacture"),
  head_gender = c("f", "m"),
  greywage    = c("white", "grey"),
  way         = c("car", "car"),
  
  start = 0, stop = 1, event = 0
)

fit_clear <- survfit(step_model, newdata = personas_clear)

prog_table <- summary(fit_clear, times = c(6, 12, 24))

p_final <- ggsurvplot(
  fit_clear, 
  data = personas_clear,
  conf.int = FALSE,
  palette = c("#00AFBB", "#E7B800"),
  xlim = c(0, 24),
  break.time.by = 2,
  legend.labs = c("Jaunas, legalus atlyginimas, moteris vadovė", 
                  "Vyresnis, neoficialus atlyginimas, vyras vadovas"),
  xlab = "Mėnesiai",
  ylab = "Tikimybė pasilikti",
  title = "Amžiaus, atlyginimo tipo ir vadovo įtaka pasilikimui",
  
  legend = "top", 
  
  ggtheme = theme_bw(base_size = 17) + 
    theme(
      plot.title = element_text(face = "bold", size = 18),
      axis.title = element_text(face = "bold"),
      legend.text = element_text(size = 13),
      legend.title = element_blank(), 
      legend.direction = "vertical",
      legend.box = "vertical"
    )
)

p_final$plot <- p_final$plot + guides(color = guide_legend(ncol = 1))

plot(p_final$plot)

ggsave(
  filename = "darbuotoju_islikimo_prognoze_v.png", 
  plot = p_final$plot, 
  width = 8,           
  height = 6,
  dpi = 750,            
)
