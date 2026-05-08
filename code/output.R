source("code/source.R")

treat_labels <- c(
  FM  = "Male - Female",
  MW  = "Male - Worker (same)",
  MW2 = "Male - Worker (other)",
  SM  = "Male - Soldier"
)

# data prep
{
  load("data_fmt/df_pair.rda")
  
  df_pair <- df_pair |> filter(treat != "FW") |>
    group_by(video, well_id) |>  mutate(
      pre_step_0  = euclid_dis(x_body_0, y_body_0, lag(x_body_0),  lag(y_body_0)),
      pre_step_1  = euclid_dis(x_body_1, y_body_1, lag(x_body_1),  lag(y_body_1)),
      post_step_0 = euclid_dis(x_body_0, y_body_0, lead(x_body_0), lead(y_body_0)),
      post_step_1 = euclid_dis(x_body_1, y_body_1, lead(x_body_1), lead(y_body_1)),
      acc_0 = post_step_0 - pre_step_0,
      acc_1 = post_step_1 - pre_step_1
    )
}

# interaction transition ---- 
{
  df_state <- df_pair |> mutate(
    tandem = follow_dis < 1.5 & lead_dis > 1.5,
    interact = partner_dis < 2) |> 
    dplyr::select(video, well_id, time_sec, treat, tandem, interact) |>
    mutate(state = case_when(
      tandem ~ "tandem",
      !tandem &interact ~ "interact",
      !tandem &!interact ~ "sep"
    )) |>
    group_by(treat) %>%
    mutate(next_state = lead(state)) %>%
    ungroup() 
  
  df_choice <- df_state %>%
    filter(!is.na(next_state)) |>
    filter(state == "interact", next_state %in% c("tandem", "sep"), state != next_state) %>%
    mutate(pair_id = paste(video, well_id, sep = "_")) %>%
    count(treat, pair_id, next_state) %>%
    mutate(prob = n / sum(n),.by = c(treat, pair_id))
  
  df_choice_tandem <- df_choice %>%
    filter(next_state == "tandem")
  
  # plot
  {
    ggplot(df_choice_tandem, aes(treat, prob, fill = treat, col = treat)) +
      geom_jitter(width = 0.1, alpha = 0.6, size = 1) +
      stat_summary(fun = median, geom = "crossbar", width = 0.5) +
      scale_color_manual(values = c("#0072B2", "#D55E00", "#009E73", "#CC79A7"))  +
      labs(y = "P(tandem | interact)", x = NULL) +
      scale_x_discrete(labels = treat_labels) +
      coord_cartesian(ylim = c(0, 1)) +
      scale_y_continuous(breaks = c(0, 0.5, 1), labels = c(0, 0.5, 1)) +
      theme_classic(base_size = 10) +
      theme(legend.position = "none", aspect.ratio = 1)
    
    ggsave( filename = "output/Interaction_transition.pdf", 
            device = cairo_pdf, family = "Arial",
            width = 3.5, height = 3.5)
  }
  
  # stat
  df_stat <- df_state %>%
    filter(state == "interact", next_state %in% c("tandem", "sep"), state != next_state) %>%
    mutate(pair_id = paste(video, well_id, sep = "_"),
           tandem_transition = next_state == "tandem")
  
  r <- glmer(tandem_transition ~ treat + (1 | pair_id), data = df_stat,
             family = binomial)
  r_sum <- summary(r)
  res <- Anova(r)
  posthoc <- summary(glht(r, linfct = mcp(treat = "Tukey")))
  
  df_res <- tibble(tibble(res), formula = deparse(formula(r) ))
  write.csv(df_res, "output/Interaction_transition_glmer.csv")
  df_res <- tidy(posthoc)
  write.csv(df_res, "output/Interaction_transition_glmer_posthoc_Tukey.csv")
}  

# tandem prop time development ----
{
  df_tandem_time <- df_pair |>
    filter(treat != "FW") |>
    mutate(
      pair_id = paste(video, well_id, sep = "_"),
      tandem = follow_dis < 0.5 & lead_dis > 0.5,
      time_min = floor(time_sec / 60)
    ) |>
    group_by(time_min, treat, pair_id, colony) |>
    summarize(
      tandem_prop = mean(tandem),
      .groups = "drop"
    )
  
  # plot
  {
    ggplot(df_tandem_time,
           aes(x = time_min, y = tandem_prop, col = treat)) +
      stat_summary(fun = mean, geom = "line", linewidth = 1) +
      stat_summary(fun.data = mean_se, geom = "ribbon",
                   aes(fill = treat), alpha = 0.2, color = NA) +
      scale_color_manual(values = c(
        "#0072B2", "#D55E00", "#009E73", "#CC79A7"), labels = treat_labels)  +
      scale_fill_manual(values = c(
        "#0072B2", "#D55E00", "#009E73", "#CC79A7"), labels = treat_labels)  +
      scale_y_continuous(limits = c(0, 1), breaks = c(0,0.5,1), labels = c(0,0.5,1)) +
      theme_classic(base_size = 10) +
      labs(x = "Time (minutes)", y = "Propotion of tandem")+
      theme(aspect.ratio = 3/4,
            legend.position = c(0.7,0.9),
            legend.title = element_blank())
    
    ggsave( filename = "output/tandem_prop.pdf", 
            device = cairo_pdf, family = "Arial",
            width = 4, height = 3)
    }
  
  # stat
  {
    r <- lmer(tandem_prop ~ time_min * treat + (1 | pair_id), data = df_tandem_time)
    df_res <- tibble(tidy(Anova(r)), formula = deparse(formula(r) ))
    res <- summary(r)
    write.csv(df_res, "output/tandem_prop_lmer.csv")
    write.csv(res$coefficients, "output/tandem_prop_lmer_coef.csv")
  }
}

# tandem duration ----
{
  # data prep
  {
    gap_max <- 1  # number of frames allowed as gap (2 sec = 10)
    min_len <- 1  # minimum frames to count as tandem (2 sec = 10)
    
    df_tandem <- df_pair|> dplyr::select(video, well_id, time_sec, colony, treat,
                                         partner_dis, follow_dis, lead_dis, post_step_0, post_step_1) |>
      group_by(video, well_id) |>
      arrange(time_sec, .by_group = TRUE) |>
      mutate(
        tandem_raw = follow_dis < 1 & lead_dis > 1,
        run_id = rleid(tandem_raw),
        run_len = ave(tandem_raw, run_id, FUN = length)
      ) |>
      mutate(
        tandem = if_else(tandem_raw & run_len >= min_len, TRUE, FALSE)
      ) |>
      mutate(
        cluster_start = tandem & lag(!tandem, default = TRUE),
        cluster_idx = cumsum(cluster_start),
        cluster_idx = if_else(tandem, cluster_idx, NA_integer_),
        pair_event = if_else(
          !is.na(cluster_idx),
          sprintf("%s_%d_%02d", video, well_id, cluster_idx),
          NA_character_
        )
      ) |>
      dplyr::select(-cluster_start, -cluster_idx, -run_id, -run_len, -tandem_raw) |>
      ungroup()
    
    df_pair_dur <- df_tandem |> filter(time_sec < 1800.1) |>
      filter(!is.na(pair_event)) |>
      group_by(pair_event, treat, well_id, video) |>
      summarise(
        start_time = first(time_sec),
        end_time = last(time_sec),
        duration = (n() * 0.2), 
        cens = !(dplyr::near(last(time_sec), 1800) | dplyr::near(first(time_sec), 0)),
        leader_moved_dis = sum(post_step_0, na.rm = T),
        follower_moved_dis = sum(post_step_1, na.rm = T),
        .groups = "drop"
      )
    
    df_plot <- df_pair_dur |> filter(duration > 0) |> filter(treat != "FW")
  }
  
  # tandem duration
  {
    ggsurvplot(
      survfit(Surv(duration, cens) ~ treat, data = df_plot),
      data = df_plot,
      censor = FALSE,
      conf.int.style = "ribbon",
      conf.int.fill = TRUE,
      ggtheme = theme_classic(),
      color = "treat"
    )$plot + 
      scale_x_continuous(trans = "pseudo_log", breaks = c(0, 1, 10, 100, 1000))+
      scale_color_manual(values = c("#0072B2", "#D55E00", "#009E73", "#CC79A7"), labels = treat_labels)  +
      scale_fill_manual(values = c("#0072B2", "#D55E00", "#009E73", "#CC79A7"), labels = treat_labels) +
      scale_y_continuous(breaks = c(0,1))+
      labs(x = "Duration (sec)", y = "Tandem probability") +
      theme(aspect.ratio = 3/4,
            legend.title = element_blank(),
            legend.position = c(0.8,0.8))
    
    ggsave( filename = "output/tandem_duration.pdf", 
            device = cairo_pdf, family = "Arial",
            width = 4, height = 3)
  }
  
  #cumulative hazard plot
  {
    ggsurvplot(
      survfit(Surv(duration, cens) ~ treat, data = df_plot),
      data = df_plot,
      fun = "cumhaz",
      censor = TRUE,
      ggtheme = theme_classic(),
      color = "treat"
    )$plot+ 
      scale_color_manual(values = c("#0072B2", "#D55E00", "#009E73", "#CC79A7"), labels = treat_labels)  +
      scale_fill_manual(values = c("#0072B2", "#D55E00", "#009E73", "#CC79A7"), labels = treat_labels) +
      scale_y_continuous(breaks = c(0, 5))+
      coord_cartesian(ylim = c(0, 6.5)) +
      labs(x = "Duration (sec)", y = "Cumulative hazard") +
      theme(aspect.ratio = 3/4,
            legend.title = element_blank(),
            legend.position = c(0.8,0.4))
    
    ggsave( filename = "output/tandem_duration_cum_hazard.pdf", 
            device = cairo_pdf, family = "Arial",
            width = 4, height = 3)
  }
  
  # stat
  {
    df_plot$treat <- as.factor(df_plot$treat)
    fit_cox <- coxme(Surv(duration, cens) ~ treat + (1|video/well_id), data = df_plot)
    res <- Anova(fit_cox)
    posthoc <- summary(glht(fit_cox, linfct = mcp(treat = "Tukey")))
    
    df_res <- tibble(tibble(res), formula = deparse(formula(fit_cox) ))
    write.csv(df_res, "output/tandem_duration_coxme.csv")
    df_res <- tidy(posthoc)
    write.csv(df_res, "output/tandem_duration_coxme_posthoc_Tukey.csv")
  }
}

# speed ----
{
  df_dis_dist <- df_pair |> pivot_longer(cols = starts_with("post_step")) |> 
    dplyr::select(name, value, treat)
  
  ggplot(df_dis_dist |> filter(value < 1)) +
    geom_density_ridges(aes(x = value, y = treat, fill= name), stat = "binline", 
                        alpha = 0.5, binwidth = 0.04, scale = 0.85) +
    labs(x = "Step length (BL)", y = "") +
    scale_y_discrete(expand = c(0, 0.1),labels = treat_labels) +
    scale_x_continuous(breaks = c(0,0.5,1), labels =  c(0,0.5,1)) +
    scale_fill_manual(values = c(post_step_0 = "#1B7837", post_step_1 = "#D8B58A")) +
    coord_cartesian(xlim = c(0,1)) +
    theme_classic(base_size = 10) +
    theme(aspect.ratio = 3,
          legend.position = "none")
  
  ggsave("output/step_distribution.pdf", device = cairo_pdf, family = "Arial",
         width = 3, height = 5.5)
}

# acc ----
{
  df_dis_acc <- df_pair |> select(follow_dis, acc_0, acc_1, treat) |>
    mutate(follow_dis_bin = round(follow_dis, 1)) |>
    pivot_longer(cols = starts_with("acc"))
  
  ggplot(df_dis_acc |> filter(follow_dis_bin < 1.5), 
         aes(x = follow_dis_bin, y = value)) + 
    stat_summary(geom = "ribbon", fun.data = mean_se, alpha = 0.2, aes(fill = name) ) +
    stat_summary(geom = "line", fun = mean, linewidth = 1, aes(col = name)) +
    scale_color_manual(values = c(acc_0 = "#1B7837", acc_1 = "#D8B58A")) +
    scale_fill_manual(values = c(acc_0 = "#1B7837", acc_1 = "#D8B58A")) +
    geom_hline(yintercept = 0)+
    scale_x_continuous(breaks = c(0,0.5,1), labels =  c(0,0.5,1)) +
    scale_y_continuous(breaks = c(-0.03, 0, 0.03), 
                       labels = c(-0.03, 0, 0.03)) +
    coord_cartesian(xlim = c(0, 1.1), ylim = c(-0.035, 0.035))+
    theme_bw(base_size = 11) +
    facet_wrap(~ treat, labeller = labeller(treat = treat_labels)) +
    labs(x = "Distance (BL)", y = "Accerelation (BL/sec2)") +
    theme(strip.placement = "outside",
          strip.background = element_blank(),
          legend.position = "none",
          legend.title = element_blank(),
          aspect.ratio = 3/4
          )
  
  ggsave("output/accerelation.pdf", device = cairo_pdf, family = "Arial",
         width = 5, height = 4)
}
