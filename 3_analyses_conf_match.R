# load packages and custom functions
source("R/publish/functions.R")

# read data
d <- read_csv("confidence_matching_data.csv") %>% 
  mutate(
    group_id   = factor(group_id),
    part_id    = factor(part_id),
    comm_cond  = factor(comm_cond, levels = c("Isolated", "Passive", "Active")),
    comm_cond2 = factor(comm_cond, levels = c("Passive", "Isolated", "Active")),
    conf_cond  = factor(conf_cond, levels = c("Low", "Mixed", "High")),
    conf_cond2 = factor(conf_cond, levels = c("Mixed", "Low", "High")),
    itemnum = factor(itemnum)
  )

# compute confidence matching within each dyad for each item
x <- d %>% 
  select(group_id, member_id, grouping, itemnum, conf_cond, comm_cond, conf) %>%
  pivot_wider(names_from = member_id, values_from = conf) %>% 
  group_by(group_id, grouping, comm_cond, itemnum) %>% 
  mutate(conf_diff = abs(a - b)) %>% 
  select(-a, -b) %>% 
  pivot_wider(names_from = grouping, names_glue = "{grouping}_conf_diff", values_from = conf_diff) %>% 
  group_by(group_id, comm_cond, itemnum) %>% 
  mutate(conf_match = ind_conf_diff - grp_conf_diff)

# mean confidence differences
x %>% 
  group_by(conf_cond, comm_cond) %>% 
  summarise(ind_conf_diff = mean(ind_conf_diff, na.rm = T),
            grp_conf_diff = mean(grp_conf_diff, na.rm = T),
            conf_match = mean(conf_match, na.rm = T))

# mean center cognitive ability
cen_cog <- d %>%
  filter(grouping == "ind", comm_cond == "Isolated") %>%
  select(group_id, member_id, rapm_acc, eat_acc) %>% 
  distinct() %>% 
  group_by(group_id) %>% 
  summarise(rapm_acc = mean(rapm_acc),
            eat_acc = mean(eat_acc)) %>% 
  transmute(
    group_id,
    rapm_acc_cen = as.numeric(scale(rapm_acc, center = TRUE, scale = FALSE)),
    eat_acc_cen  = as.numeric(scale(eat_acc,  center = TRUE, scale = FALSE))
  )

# add mean centered cog ability to data
x <- x %>% 
  left_join(cen_cog, by = "group_id")


# compare confidence matching between conditions ---------------------------------------------------------------------
# identify random effects structure
model1 <- lm(
  conf_match ~ comm_cond * conf_cond + rapm_acc_cen + eat_acc_cen,
  data = x
)

model2 <- lmer(
  conf_match ~ comm_cond * conf_cond + rapm_acc_cen + eat_acc_cen +
    (1 | group_id)  + (1 | itemnum),
  data = x
)

model3 <- lmer(
  conf_match ~ comm_cond * conf_cond + rapm_acc_cen + eat_acc_cen +
    (1 + comm_cond | group_id)  + (1 | itemnum),
  data = x
)

# compare models
anova(model3,model2,model1) %>% 
  broom::tidy()


# examine interaction structure for model
# main effects only
model1 <- lmer(
  conf_match ~ comm_cond + conf_cond + rapm_acc_cen + eat_acc_cen +
    (1 | group_id) + (1 | itemnum),
  data = x
)

# main + interaction effects
model2 <- lmer(
  conf_match ~ comm_cond * conf_cond + rapm_acc_cen + eat_acc_cen +
    (1 | group_id) + (1 | itemnum),
  data = x
)

# compare models
anova(model2,model1) %>% 
  tidy() %>% 
  arrange(term) %>% 
  mutate(
    AIC = round(AIC),
    BIC = round(BIC),
    logLik = paste0(round(logLik), sig_stars(p.value))) %>% 
  select(term, AIC, BIC, logLik, statistic, p.value)

# compare best model (final_model) to null model
model_null <- lmer(
  conf_match ~ 1 + (1 | group_id)  + (1 | itemnum),
  data = x
)

anova(model_null, model1)

# check r2 for final_model
performance::r2(model1)


# main effect estimates ---------------------------------------------------
pb_limit <- 3140 # manually set dfs

# compute estimated marginal means and save for contrasts
emm_comm <- emmeans(model1, ~ comm_cond, pbkrtest.limit = pb_limit)
emm_conf <- emmeans(model1, ~ conf_cond, pbkrtest.limit = pb_limit)
emm_rapm <- emtrends(model1, ~1, var = "rapm_acc_cen", pbkrtest.limit = pb_limit)
emm_eat <- emtrends(model1, ~1, var = "eat_acc_cen", pbkrtest.limit = pb_limit)

# main effects table
main_effects <- bind_rows(
  tidy(emm_comm) %>% rename(var1 = comm_cond),
  tidy(emm_conf) %>% rename(var1 = conf_cond),
  tidy(emm_rapm) %>% rename(var1 = `1`, estimate = rapm_acc_cen.trend) %>% 
    mutate(var1 = "rapm_acc_cen"),
  tidy(emm_eat) %>% rename(var1 = `1`, estimate = eat_acc_cen.trend) %>% 
    mutate(var1 = "eat_acc_cen")
) %>%
  select(var1, estimate, std.error, statistic, p.value, df) %>%
  add_sig_formatting()

main_effects

# contrasts table
contrasts <- bind_rows(
  contrast(emm_comm, method = "pairwise", adjust = "holm") %>% as_tibble(),
  contrast(emm_conf, method = "pairwise", adjust = "holm") %>% as_tibble()
) %>% 
  mutate(t.ratio = paste0(round(t.ratio,2), sig_stars(p.value)))

contrasts
  

# plots -------------------------------------------------------------------
# figure 3
# prepare communication condition data
emm_comm_plot <- emm_comm %>% 
  as_tibble() %>% 
  left_join(emm_comm %>% tidy() %>% select(comm_cond, statistic, p.value), by = "comm_cond") %>% 
  mutate(comm_cond = factor(comm_cond, levels = c("Isolated", "Passive", "Active")),
         sig = sig_stars(p.value))

# prepare confidence condition data
emm_conf_plot <- emm_conf %>% 
  as_tibble() %>% 
  left_join(emm_conf %>% tidy() %>% select(conf_cond, statistic, p.value), by = "conf_cond") %>% 
  mutate(
    sig = sig_stars(p.value))

# main effect of communication plot
p_comm <- ggplot(emm_comm_plot, aes(x = comm_cond, y = emmean, colour = comm_cond)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2, linewidth = .75) +
  labs(x = "Communication", y = "",
       title = "Main Effect of Communication") +
  scale_colour_manual(
    values = wes_palette("FantasticFox1", n = 3)) +
  geom_text(
    aes(x = comm_cond, y = pmin(upper.CL + 0.03, 1), label = sig, colour = comm_cond),
    inherit.aes = FALSE,
    show.legend = FALSE,
    size = 5,
    vjust = 0.5
  ) +
  conf_match_plot_theme() +
  theme(legend.position = "none")


# distribution of confidence difference (matching)
p_dist <- ggplot(x, aes(x = comm_cond, y = conf_match, fill = comm_cond, colour = comm_cond)) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  stat_summary(fun = mean, geom = "point", size = 2, colour = "grey50") +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  labs(x = "Communication", y = "Δ (Pre - Post) in Decision Confidence Difference\n Positive = More Matching",
       title = "Distribution of Confidence Matching Scores") +
  scale_colour_manual(
    values = wes_palette("FantasticFox1", n = 3)) +
  scale_fill_manual(
    values = wes_palette("FantasticFox1", n = 3)) +
  conf_match_plot_theme() +
  theme(legend.position = "none")


# combine plots
cowplot::plot_grid(p_dist, p_comm, labels = c('A', 'B'), ncol = 2)



# accuracy and confidence matching ----------------------------------------
# compute mean confidence matching for each dyad
conf_match <- x %>% 
  group_by(group_id, comm_cond, conf_cond) %>%
  summarise(conf_match = mean(conf_match, na.rm = T),
            rapm_acc_cen = rapm_acc_cen[1],
            eat_acc_cen = eat_acc_cen[1])

# compute mean accuracy change for each dyad
acc_change <- d %>%
  select(group_id, member_id, grouping, comm_cond, conf_cond, itemnum, resp_acc) %>% 
  group_by(group_id, grouping, comm_cond, conf_cond) %>% 
  summarise(acc = mean(resp_acc, na.rm=T),
            group_id = group_id[1],
            .groups = "drop") %>% 
  pivot_longer(acc) %>% 
  unite(var, grouping, name) %>% 
  pivot_wider(names_from = var, values_from = value) %>% 
  group_by(group_id, comm_cond, conf_cond) %>% 
  mutate(acc_change = 100 * (grp_acc-ind_acc)) %>% 
  select(-grp_acc, -ind_acc)

x <- conf_match %>% left_join(acc_change, by = c("group_id", "comm_cond", "conf_cond"))


# identify appropriate random effects structure
model1 <- lm(
  acc_change ~ conf_match * comm_cond * conf_cond + rapm_acc_cen + eat_acc_cen,
  data = x
)

model2 <- lmer(
  acc_change ~ conf_match * comm_cond * conf_cond + rapm_acc_cen + eat_acc_cen +
    (1 | group_id),
  data = x
)

model3 <- lmer(
  acc_change ~ conf_match * comm_cond * conf_cond + rapm_acc_cen + eat_acc_cen +
    (1 | comm_cond),
  data = x,
)

model4 <- lmer(
  acc_change ~ conf_match * comm_cond * conf_cond + rapm_acc_cen + eat_acc_cen +
    (1 | comm_cond) + (1 | group_id),
  data = x
)

model5 <- lmer(
  acc_change ~ conf_match * comm_cond * conf_cond + rapm_acc_cen + eat_acc_cen +
    (1 + comm_cond | group_id),
  data = x,
  control = lmerControl(optimizer = "bobyqa")
)

# compare models
anova(model4,model3,model2,model1) %>% tidy()


# examine interaction structure
# main effects only
model1 <- lm(acc_change ~ conf_match + comm_cond + conf_cond + rapm_acc_cen + eat_acc_cen,
             data = x)

# addition of 2-way interactions
model2 <- lm(acc_change ~ 
               conf_match * comm_cond +
               conf_match * conf_cond + 
               comm_cond * conf_cond +
               rapm_acc_cen + eat_acc_cen,
             data = x)

# addition of 3-way interactions
model3 <- lm(acc_change ~ conf_match * comm_cond * conf_cond + rapm_acc_cen + eat_acc_cen,
             data = x)

# compare models
anova_results <- anova(model1, model2, model3)

# combine all results into a table
tibble(
  Model = c("Model 1", "Model 2", "Model 3"),
  AIC = round(c(AIC(model1), AIC(model2), AIC(model3))),
  BIC = round(c(BIC(model1), BIC(model2), BIC(model3))),
  LogLik = round(c(logLik(model1), logLik(model2), logLik(model3))),
  Res.Df = anova_results$Res.Df,
  RSS = anova_results$RSS,
  Df = c(NA, anova_results$Df[-1]),
  `F` = round(c(NA, anova_results$F[-1]),2),
  `Pr(>F)` = round(c(NA, anova_results$`Pr(>F)`[-1]),3)
)

# overall best fitting model results
car::Anova(model3, type = "III", test.statistic = "F") %>% tidy() %>% 
  mutate(
    statistic = paste0(round(statistic, 2), sig_stars(p.value)),
    p.value = round(p.value, 4))

# three-way interaction effects for conf_match predicting acc_change 
slopes_3way <- emtrends(model3,
                        specs = ~ comm_cond * conf_cond,
                        var = "conf_match")

summary(slopes_3way, infer = TRUE) %>% as_tibble() %>% 
  mutate(t.ratio = paste0(round(t.ratio,2), sig_stars(p.value))) %>% 
  arrange(comm_cond, conf_cond)

# pairwise comparisons for three-way interaction effects
pair_comm_within_conf <- contrast(slopes_3way,
                                  method = "pairwise",
                                  by = "conf_cond",
                                  adjust = "holm")

pair_conf_within_comm <- contrast(slopes_3way,
                                  method = "pairwise",
                                  by = "comm_cond",
                                  adjust = "holm")


bind_rows(pair_comm_within_conf %>% as.data.frame() %>% as_tibble() %>% rename(variable2 = conf_cond),
          pair_conf_within_comm %>% as.data.frame() %>% as_tibble() %>% rename(variable2 = comm_cond)) %>% 
  mutate(t.ratio = paste0(round(t.ratio,2), sig_stars(p.value)))


# plot confidence matching and accuracy change relationships
# estimate predicted marginal means for each confidence condition
em_conf_match <- emmeans(
  model3,
  ~ conf_match * comm_cond * conf_cond,
  at = list(conf_match = seq(
    min(x$conf_match, na.rm = TRUE),
    max(x$conf_match, na.rm = TRUE),
    length.out = 100))
)

# convert to dataframe with confidence intervals
df_conf_match <- summary(em_conf_match, infer = TRUE)

# make plot
ggplot(df_conf_match, aes(x = conf_match, y = emmean, color = comm_cond, fill = comm_cond)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.2, color = NA) +
  scale_colour_manual(values = wes_palette("FantasticFox1", n = 3)) +
  scale_fill_manual(values = wes_palette("FantasticFox1", n = 3)) +
  facet_wrap( ~ conf_cond) +
  labs(
    x = "Confidence Matching",
    y = "Decision Accuracy Change",
    colour = "Communication",
    fill = "Communication",
    subtitle = "Trait Confidence"
  ) +
  conf_match_plot_theme()
