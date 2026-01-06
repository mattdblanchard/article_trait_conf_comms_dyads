# load packages and custom functions
source("R/publish/functions.R")

# read data
d <- read_csv("confidence_matching_data.csv")

# aggregate to participant × condition
x <- d %>% 
  mutate(
    group_id      = factor(group_id),
    part_id    = factor(part_id),
    comm_cond = factor(comm_cond, levels = c("Isolated", "Passive", "Active")),
    comm_cond2 = factor(comm_cond, levels = c("Passive", "Isolated", "Active")),
    conf_cond = factor(conf_cond, levels = c("Low", "Mixed", "High")),
    conf_cond2 = factor(conf_cond, levels = c("Mixed", "Low", "High"))
  ) %>% 
  summarise(
    acc      = 100 * mean(resp_acc, na.rm = TRUE),
    conf     = mean(conf,     na.rm = TRUE),
    rapm_acc = (rapm_acc)[1],
    eat_acc  = (eat_acc)[1],
    .by = c(part_id, group_id, conf_cond, comm_cond, grouping)
  )

# mean center cognitive ability
cen_cog <- x %>%
  filter(grouping == "ind", comm_cond == "Isolated") %>%
  transmute(
    part_id,
    rapm_acc_cen = as.numeric(scale(rapm_acc, center = TRUE, scale = FALSE)),
    eat_acc_cen  = as.numeric(scale(eat_acc,  center = TRUE, scale = FALSE))
  )

# join and create the accuracy percent variable used in the LMM
x <- x %>% 
  left_join(cen_cog, by = "part_id")

# compare accuracy between conditions ---------------------------------------------------------------------
# identify appropriate random effects structure
model1 <- lm(
  acc ~ grouping * comm_cond * conf_cond + rapm_acc_cen + eat_acc_cen,
  data = x
)

model2 <- lmer(
  acc ~ grouping * comm_cond * conf_cond + rapm_acc_cen + eat_acc_cen + 
    (1 | part_id) ,
  data = x
)

model3 <- lmer(
  acc ~ grouping * comm_cond * conf_cond + rapm_acc_cen + eat_acc_cen + 
    (1 | group_id) ,
  data = x
)

model4 <- lmer(
  acc ~ grouping * comm_cond * conf_cond + rapm_acc_cen + eat_acc_cen + 
    (1 | part_id) + (1 | group_id) ,
  data = x
)


model5 <- lmer(
  acc ~ grouping * comm_cond * conf_cond + rapm_acc_cen + eat_acc_cen +
    (1 + comm_cond | part_id) ,
  data = x
)

model6 <- lmer(
  acc ~ grouping * comm_cond * conf_cond + rapm_acc_cen + eat_acc_cen +
    (1 + comm_cond | group_id) ,
  data = x
)

model7 <- lmer(
  acc ~ grouping * comm_cond * conf_cond + rapm_acc_cen + eat_acc_cen +
    (1 + comm_cond | part_id) + (1 | group_id),
  data = x
)

model8 <- lmer(
  acc ~ grouping * comm_cond * conf_cond + rapm_acc_cen + eat_acc_cen +
    (1 + comm_cond | part_id) + (1 + comm_cond | group_id),
  data = x
)


# compare models
anova(model2,model3,model4,model5,model6,model7,model8,model1) %>% 
  tidy() %>% 
  arrange(term) %>% 
  mutate(
    AIC = round(AIC),
    BIC = round(BIC),
    logLik = paste0(round(logLik), sig_stars(p.value))) %>% 
  select(term, AIC, BIC, logLik, statistic, p.value)


# compare best model (final_model) to null model
acc_null <- lmer(
  acc ~ 1 + (1 + comm_cond | part_id) + (1 + comm_cond | group_id),
  data = x
)

acc_final <- lmer(
  acc ~ grouping * comm_cond * conf_cond + rapm_acc_cen + eat_acc_cen +
    (1 + comm_cond | part_id) + (1 + comm_cond | group_id),
  data = x
)

anova(acc_null, acc_final)

# check r2 for final_model
performance::r2(acc_final)

# accuracy full results
table_full_results(acc_final)

# compare accuracy at baseline
baseline_comparisons(acc_final)



# compare confidence between conditions ---------------------------------------------------------------------
# identify appropriate random effects structure
model1 <- lm(
  conf ~ grouping * comm_cond * conf_cond + rapm_acc_cen + eat_acc_cen,
  data = x
)

model2 <- lmer(
  conf ~ grouping * comm_cond * conf_cond + rapm_acc_cen + eat_acc_cen + 
    (1 | part_id) ,
  data = x
)

model3 <- lmer(
  conf ~ grouping * comm_cond * conf_cond + rapm_acc_cen + eat_acc_cen + 
    (1 | group_id) ,
  data = x
)

model4 <- lmer(
  conf ~ grouping * comm_cond * conf_cond + rapm_acc_cen + eat_acc_cen + 
    (1 | part_id) + (1 | group_id) ,
  data = x
)

model5 <- lmer(
  conf ~ grouping * comm_cond * conf_cond + rapm_acc_cen + eat_acc_cen +
    (1 + comm_cond | part_id) ,
  data = x
)

model6 <- lmer(
  conf ~ grouping * comm_cond * conf_cond + rapm_acc_cen + eat_acc_cen +
    (1 + comm_cond | group_id) ,
  data = x
)

model7 <- lmer(
  conf ~ grouping * comm_cond * conf_cond + rapm_acc_cen + eat_acc_cen +
    (1 + comm_cond | part_id) + (1 | group_id),
  data = x
)

model8 <- lmer(
  conf ~ grouping * comm_cond * conf_cond + rapm_acc_cen + eat_acc_cen +
    (1 + comm_cond | part_id) + (1 + comm_cond | group_id),
  data = x
)


# compare models
anova(model2,model3,model4,model5,model6,model7,model8,model1) %>% 
  tidy() %>% 
  arrange(term) %>% 
  mutate(
    AIC = round(AIC),
    BIC = round(BIC),
    logLik = paste0(round(logLik), sig_stars(p.value))) %>% 
  select(term, AIC, BIC, logLik, statistic, p.value)

# compare best model (final_model) to null model
conf_null <- lmer(
  conf ~ 1 + (1 + comm_cond | part_id) + (1 + comm_cond | group_id) ,
  data = x
)

conf_final <- lmer(
  conf ~ grouping * comm_cond * conf_cond + rapm_acc_cen + eat_acc_cen + 
    (1 + comm_cond | part_id) + (1 + comm_cond | group_id),
  data = x
)

anova(conf_null, conf_final)

# check r2 for final_model
performance::r2(conf_final)

# confidence full results
table_full_results(conf_final)

# compare confidence at baseline
baseline_comparisons(conf_final)


# produce plots for acc and conf ----------------------------------------------
# estimated marginal means for acc
emm_acc <- emmeans(acc_final, ~ grouping | comm_cond * conf_cond) %>%
  contrast(method = "pairwise") %>% 
  tidy() %>%
  filter(contrast == "grp - ind") %>% 
  mutate(t_star = qt(0.975, df = df),
         outcome = "Decision Accuracy")

# estimated marginal means for conf
emm_conf <- emmeans(conf_final, ~ grouping | comm_cond * conf_cond) %>%
  contrast(method = "pairwise") %>% 
  tidy() %>%
  filter(contrast == "grp - ind") %>% 
  mutate(t_star = qt(0.975, df = df),
         outcome = "Decision Confidence")


my_colours <- c("#DD8D29", "#E2D200", "#46ACC8")

# get pairwise contrasts between Ind vs Dyad within each comm_cond x conf_cond condition
contrast_df <- bind_rows(emm_acc, emm_conf) %>%
  group_by(comm_cond, conf_cond, outcome) %>% 
  mutate(
    sig = sig_stars(p.value),
    lower.CL = estimate - t_star * std.error,
    upper.CL = estimate + t_star * std.error,
    comm_cond = factor(comm_cond, levels = c("Isolated", "Passive", "Active")),
    conf_cond = factor(conf_cond, levels = c("Low", "Mixed", "High"))
  )

# make plots
plot_acc <- contrast_df %>% 
  filter(outcome == "Decision Accuracy") %>% 
  ggplot(aes(x = conf_cond, y = estimate, colour = conf_cond)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_point(size = 5) +
  scale_colour_manual(values = my_colours) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2, linewidth = .75) +
  facet_grid(outcome ~ comm_cond) +
  geom_text(
    aes(x = conf_cond, y = pmin(upper.CL + 0.03, 1), label = sig, colour = conf_cond),
    inherit.aes = FALSE,
    show.legend = FALSE,
    size = 5,
    vjust = 0.5
  ) +
  labs(
    x = NULL,
    y = NULL,
    subtitle = "Communication Type",
    colour = NULL
  ) +
  theme(
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    plot.title = element_text(hjust = 0.5),
    strip.text.x = element_text(size = 12),
    strip.text.y = element_text(size = 12),
    strip.background = element_blank(),
    panel.background = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    axis.title.y = element_blank(),
    axis.line.y = element_line(colour = "black", linewidth = 0.5),
    plot.margin = margin(10, 10, 10, 10),
    legend.position = "none"
  )


plot_conf <- contrast_df %>% 
  filter(outcome == "Decision Confidence") %>% 
  ggplot(aes(x = conf_cond, y = estimate, colour = conf_cond)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_point(size = 5) +
  scale_colour_manual(values = my_colours) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2, linewidth = .75) +
  facet_grid(outcome ~ comm_cond) +
  geom_text(
    aes(x = conf_cond, y = pmin(upper.CL + 0.05), label = sig, colour = conf_cond),
    inherit.aes = FALSE,
    show.legend = FALSE,
    size = 5,
    vjust = 0.1
  ) +
  labs(
    x = "Trait Confidence",
    y = NULL,
    subtitle = NULL,
    colour = "Trait Confidence"
  ) +
  theme(
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    plot.title = element_text(hjust = 0.5),
    strip.text.x = element_blank(),
    strip.text.y = element_text(size = 12),
    strip.background = element_blank(),
    panel.background = element_blank(),
    axis.title.x = element_text(size = 12),
    axis.text.x = element_text(size = 12, colour = "black"),
    axis.line.x = element_line(colour = "black", linewidth = 0.5),
    axis.title.y = element_blank(),
    axis.line.y = element_line(colour = "black", linewidth = 0.5),
    legend.text = element_text(size = 12),
    plot.margin = margin(10, 10, 10, 10),
    legend.position = "bottom"
  )

# combine the plots
stacked_plot <- cowplot::plot_grid(
  plot_acc, plot_conf,
  ncol = 1,
  align = "v",
  rel_heights = c(1, 1.2)
)

# add a shared y-axis label
ggdraw() +
  draw_label("Dyad – Individual Difference", x = 0.02, angle = 90, fontface = "plain", size = 12) +
  draw_plot(stacked_plot, x = 0.05, width = 0.95)


