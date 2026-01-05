
packages <- c("tidyverse", "performance", "broom", "emmeans", "broom.mixed", "lme4", "lmerTest", "cowplot", "wesanderson", "ggeffects")

# install any packages not currently installed
if (any(!packages %in% installed.packages())) {
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
}

lapply(packages, library, character.only = TRUE)

# custom function to add significance stars
sig_stars <- function(p) {
  ifelse(
    is.na(p), "",
    ifelse(p < 0.001, "***",
           ifelse(p < 0.01,  "**",
                  ifelse(p < 0.05,  "*",
                         ifelse(p < 0.1,   "†", ""))))
  )
}



table_full_results <- function(model) {
  # Get EMMs
  emm <- emmeans(model, ~ grouping * comm_cond * conf_cond)
  
  # Perform pairwise comparisons for main effects
  main_contrasts <- list(
    grouping = contrast(emmeans(model, ~ grouping), method = "pairwise"),
    comm_cond = contrast(emmeans(model, ~ comm_cond), method = "revpairwise"),
    conf_cond = contrast(emmeans(model, ~ conf_cond), method = "revpairwise")
  )
  
  main_table <- bind_rows(
    main_contrasts$grouping %>% tidy() %>% mutate(effect = "Grouping"),
    main_contrasts$comm_cond %>% tidy() %>% mutate(effect = "Communication") %>% rename(p.value = adj.p.value),
    main_contrasts$conf_cond %>% tidy() %>% mutate(effect = "Confidence") %>% rename(p.value = adj.p.value),
    model %>% tidy() %>% filter(term == "rapm_acc_cen") %>% mutate(effect = "RAPM"),
    model %>% tidy() %>% filter(term == "eat_acc_cen") %>% mutate(effect = "EAT"),
    model %>% tidy() %>% filter(term == "education") %>% mutate(effect = "Education")
  ) %>%
    mutate(
      sig = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01  ~ "**",
        p.value < 0.05  ~ "*",
        p.value < 0.1   ~ "†",
        TRUE            ~ ""
      ),
      estimate = paste0(round(estimate, 2), sig),
      statistic = round(statistic, 2),
      p.value = round(p.value, 3),
      conf_cond = NA,
      comm_cond = NA,
      SE = round(std.error, 2)
    ) %>% 
    select(effect, contrast, comm_cond, conf_cond, estimate, SE, statistic, p.value)
  
  # Get Dyad vs Individual contrasts by comm_cond × conf_cond
  simple_results <- contrast(
    emm,
    method = "pairwise",
    by = c("comm_cond", "conf_cond"),
    adjust = "holm"
  ) %>%
    tidy() %>%
    mutate(
      sig = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01  ~ "**",
        p.value < 0.05  ~ "*",
        p.value < 0.1   ~ "†",
        TRUE            ~ ""
      ),
      estimate = paste0(round(estimate, 2), sig),
      SE = round(std.error,2),
      statistic = round(statistic, 2),
      p.value = round(p.value, 3),
      contrast = "grp - ind",
      effect = "simple",
      comm_cond = factor(comm_cond, levels = c("Isolated", "Passive", "Active")),
      conf_cond = factor(conf_cond, levels = c("Low", "Mixed", "High"))
    ) %>% 
    arrange(comm_cond, conf_cond)
  
  # DID contrasts: subset by conf_cond
  do_did_comms <- function(conf_level) {
    emm_sub <- emm[emm@grid$conf_cond == conf_level, ]
    
    contrast(emm_sub, method = list(
      "DID: (grp Passive - ind Passive) - (grp Isolated - ind Isolated)" = c(-1, 1, 1, -1, 0, 0),
      "DID: (grp Active - ind Active) - (grp Isolated - ind Isolated)" = c(-1, 1, 0, 0, 1, -1),
      "DID: (grp Active - ind Active) - (grp Passive - ind Passive)" = c(0, 0, -1, 1, 1, -1)
    )) %>%
      tidy() %>%
      mutate(conf_cond = conf_level)
  }
  
  did_comms <- bind_rows(
    do_did_comms("Low"),
    do_did_comms("Mixed"),
    do_did_comms("High")
  ) %>%
    mutate(
      sig = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01  ~ "**",
        p.value < 0.05  ~ "*",
        p.value < 0.1   ~ "†",
        TRUE            ~ ""
      ),
      estimate = paste0(round(estimate, 2), sig),
      SE = round(std.error,2),
      effect = "did",
      statistic = round(statistic, 2),
      p.value = round(p.value, 3),
      comm_cond = NA
    ) %>%
    select(effect, contrast, comm_cond, conf_cond, estimate, SE, statistic, p.value)
  
  # DID interactions across conf_cond levels (Low vs Mixed) for each comm_cond condition
  do_did_conf <- function(comm_level) {
    emm_sub <- emm[emm@grid$comm_cond == comm_level, ]
    
    contrast(emm_sub, method = list(
      "DID: (grp - ind) Mixed - Low" = c(-1, 1, 1, -1, 0, 0),
      "DID: (grp - ind) High - Low"  = c(-1, 1, 0, 0, 1, -1),
      "DID: (grp - ind) High - Mixed" = c(0, 0, -1, 1, 1, -1)
    )) %>%
      tidy() %>%
      mutate(comm_cond = comm_level)
  }
  
  did_conf <- bind_rows(
    do_did_conf("Isolated"),
    do_did_conf("Passive"),
    do_did_conf("Active")
  ) %>%
    mutate(
      sig = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01  ~ "**",
        p.value < 0.05  ~ "*",
        p.value < 0.1   ~ "†",
        TRUE            ~ ""
      ),
      estimate = paste0(round(estimate, 2), sig),
      SE = round(std.error, 2),
      effect = "did (inter-conf)",
      statistic = round(statistic, 2),
      p.value = round(p.value, 3),
      conf_cond = NA
    ) %>%
    select(effect, contrast, comm_cond, conf_cond, estimate, SE, statistic, p.value)
  
  # random effects
  random <- model %>% tidy() %>% filter(effect == "ran_pars") %>% 
    select(term, estimate) %>% 
    rename(effect = term) %>% 
    mutate(estimate = as.character(round(estimate,2)),
           contrast = "random effects",
           comm_cond = NA,
           conf_cond = NA,
           SE = NA,
           Z = NA,
           p.value = NA,
           predicted_diff_acc = NA)
  
  # combine
  table <- bind_rows(main_table,
                     simple_results,
                     did_conf,
                     did_comms,
                     random)
  
  return(table)
}




baseline_comparisons <- function(model) {
  
  emm <- emmeans(model, ~ grouping * comm_cond * conf_cond)
  
  base <- contrast(
    emm,
    method = "revpairwise",
    by = c("grouping", "conf_cond"),
    adjust = "holm"
  ) %>%
    tidy() %>%
    filter(grouping == "ind") %>% 
    mutate(
      sig = case_when(
        adj.p.value < 0.001 ~ "***",
        adj.p.value < 0.01  ~ "**",
        adj.p.value < 0.05  ~ "*",
        adj.p.value < 0.1   ~ "†",
        TRUE            ~ ""
      ),
      estimate = paste0(round(estimate, 2), sig),
      SE = round(std.error,2),
      statistic = round(statistic, 2),
      p.value = round(adj.p.value, 3),
      effect = "baseline"
    ) %>%
    select(effect, grouping, conf_cond, contrast, estimate, SE, statistic, p.value)
  
  return(base)
  
}

# Helper function for formatting confidence matching results
add_sig_formatting <- function(df) {
  df %>%
    mutate(
      sig = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01  ~ "**",
        p.value < 0.05  ~ "*",
        p.value < 0.1   ~ "†",
        TRUE ~ ""
      ),
      across(where(is.numeric) & matches("estimate|SE|std.error"), ~round(., 2)),
      df = round(df),
      statistic = paste0(round(statistic, 2), sig),
      p.value = round(p.value, 4)
    ) %>% 
    select(-sig)
}


# Confidence matching plot theme
conf_match_plot_theme <- function() {
  theme(
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    strip.text = element_text(size = 12),
    panel.background = element_blank(),
    strip.background = element_blank(),
    axis.title.x = element_text(size = 12),
    axis.text.x = element_text(size = 12, colour = "black"),
    axis.line.x = element_line(colour = "black", linewidth = 0.5),
    axis.title.y = element_text(size = 12),
    axis.text.y = element_text(size = 12, colour = "black"),
    axis.line.y = element_line(colour = "black", linewidth = 0.5),
    legend.text = element_text(size = 12),
    plot.margin = margin(10, 10, 10, 10),
    legend.key = element_rect(fill = "white", colour = "white"))
}
