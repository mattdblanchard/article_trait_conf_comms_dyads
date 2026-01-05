# load packages and custom functions
source("R/publish/functions.R")

# read raw data
d <- read_csv("confidence_matching_data.csv") %>% 
  mutate(group_id = factor(group_id),
         comm_cond = factor(comm_cond, levels = c("Isolated", "Passive", "Active")),
         conf_cond = factor(conf_cond, levels = c("Low", "Mixed", "High")))


# descriptive statistics acc and conf -----------------------------------------------
# table 1
d %>% 
  group_by(group_id, conf_cond, comm_cond, grouping) %>% 
  summarise(acc = 100*mean(resp_acc, na.rm=T),
            conf = mean(conf, na.rm=T),
            .groups = "drop") %>% 
  pivot_longer(acc:conf) %>% 
  group_by(conf_cond, comm_cond, grouping, name) %>% 
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd   = sd(value, na.rm = TRUE),
    val  = paste0(sprintf("%.2f", mean), " (", sprintf("%.2f", sd), ")"),
    .groups = "drop"
  ) %>% 
  select(-mean, -sd) %>% 
  unite(var, grouping, conf_cond) %>% 
  pivot_wider(names_from = var, values_from = val) %>% 
  mutate(name = factor(name, levels = c("acc", "conf"))) %>% 
  arrange(name, comm_cond) %>% 
  select(name, comm_cond, ind_Low, ind_Mixed, ind_High, grp_Low, grp_Mixed, grp_High)


# descriptives for ID and demographics ------------------------------------
# table 2
# select variables to include
vars <- c("age","education","sex","trait_acc","rapm_acc","eat_acc",
          "trait_conf","rapm_conf","eat_conf","agreeableness","conscientiousness","extraversion",
          "intellect","neuroticism")

# prepare data
x <- d %>% 
  filter(grouping == "ind", comm_cond == "Isolated") %>% 
  select(group_id,conf_cond,all_of(vars)) %>% 
  distinct()

# compute means
means <- x %>% 
  pivot_longer(-c(group_id, conf_cond)) %>% 
  group_by(name, conf_cond) %>% 
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd   = sd(value, na.rm = TRUE),
    val  = paste0(sprintf("%.2f", mean), " (", sprintf("%.2f", sd), ")"),
    .groups = "drop"
  ) %>% 
  select(-mean, -sd) %>% 
  pivot_wider(names_from = conf_cond, values_from = val) %>% 
  mutate(name = factor(name, levels = vars)) %>% 
  arrange(name)

# prepare data for pairwise tests of trait confidence conditions
nested <- x %>% 
  pivot_longer(-c(group_id, conf_cond)) %>% 
  group_by(name) %>% 
  nest()

# conduct pairwise tests
test <- nested %>%
  mutate(
    fit           = map(data, ~ aov(value ~ conf_cond, data = .x)),
    tidy_fit      = map(fit, tidy),
    contrast      = map(fit, TukeyHSD),
    tidy_contrast = map(contrast, tidy)
  ) %>% 
  unnest(tidy_fit) %>% 
  select(-data, -fit, -contrast) %>%
  group_by(name) %>%
  mutate(
    dfB = df[term == "conf_cond"],
    dfW = df[term == "Residuals"],
    SSB = sumsq[term == "conf_cond"],
    SSW = sumsq[term == "Residuals"],
    MSB = meansq[term == "conf_cond"],
    MSW = meansq[term == "Residuals"]
  ) %>%
  filter(term == "conf_cond") %>% 
  mutate(
    statistic = paste0(sprintf("%.2f", statistic), sig_stars(p.value)),
    p.value   = round(p.value, 4)
  ) %>% 
  select(name, df, dfW, statistic, p.value, tidy_contrast) %>% 
  unnest(tidy_contrast) %>% 
  mutate(
    estimate     = paste0(sprintf("%.2f", estimate), sig_stars(adj.p.value)),
    adj.p.value  = round(adj.p.value, 3)
  ) %>% 
  select(-null.value, -conf.low, -conf.high, -term) %>% 
  pivot_wider(
    names_from  = contrast,
    values_from = c(estimate, adj.p.value)
  )

# combine means and test results
means %>% left_join(test, by = "name") %>% 
  select(name, Low, Mixed, High, statistic, p.value, df, dfW, `estimate_Mixed-Low`:`adj.p.value_High-Mixed`)

# compute within-individual correlation -----------------------------------
# correlations between conf and acc within each communication and confidence condition
within_corrs <- d %>% 
  filter(grouping == "ind") %>%
  group_by(group_id, comm_cond) %>% 
  mutate(
    sd_conf = sd(conf),
    sd_resp_acc = sd(resp_acc),
    resp_acc = as.numeric(resp_acc)
  ) %>%
  filter(sd_conf != 0 & sd_resp_acc != 0) %>%
  summarise(r = cor(conf, resp_acc, method = "pearson"), .groups = "drop") %>%
  mutate(z = atanh(r))  # Fisher transformation

within_corrs %>%
  group_by(comm_cond) %>%
  summarise(
    mean_z = mean(z),
    se_z = sd(z) / sqrt(n()),
    ci_low_z = mean_z - 1.96 * se_z,
    ci_high_z = mean_z + 1.96 * se_z,
    mean_r = tanh(mean_z),         # back-transform
    ci_low_r = tanh(ci_low_z),
    ci_high_r = tanh(ci_high_z)
  ) %>% 
  select(comm_cond, mean_r, ci_low_r, ci_high_r) %>% 
  left_join(
    within_corrs %>%
      group_by(comm_cond) %>%
      summarise(p = t.test(z)$p.value),
    by = "comm_cond"
  )


# correlation decision confidence and trait confidence --------------------
x <- d %>% 
  filter(grouping == "ind") %>% 
  select(group_id, member_id, comm_cond, conf, trait_conf) %>% 
  group_by(group_id, member_id) %>% 
  summarise(conf = mean(conf, na.rm = T),
            trait_conf = trait_conf[1])

cor.test(x$conf, x$trait_conf)
