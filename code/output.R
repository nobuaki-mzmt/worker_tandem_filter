# output.R
# load format data created by procesing.R
# create plots and perform stats

# config ----
{
  source("code/source.R")
  
  treat_labels <- c(
    FM  = "Male - Female",
    MW  = "Male - Worker (same)",
    MW2 = "Male - Worker (other)",
    SM  = "Male - Soldier",
    MM  = "Male - Male"
  )
  
  treat_colors <- c(
    FM  = "#D55E00",  # vermillion
    MM  = "#0072B2",  # blue
    MW  = "#009E73",  # green (same colony worker)
    SM  = "#66A61E",  # olive green (same colony soldier)
    MW2 = "#CC79A7"   # purple (different colony worker)
  )

  load("data_fmt/df_pair.rda")
  load("data_fmt/df_dish.rda")
  
  df_attack <- read.csv("data_raw/master_well.csv")
  df_attack <- df_attack |> filter(!is.na(tandem_before_attack ), treat == "MW2") |> 
    mutate(pair_id = paste(Video, well, sep = "_")) |> 
    dplyr::select(pair_id, treat, tandem_before_attack, severe_attack_sec, fetal_attack_sec, tandem_after_attack)
    
  # remove sec after the fetal attack
  df_pair <- df_pair |> 
    left_join(df_attack |> dplyr::select(pair_id, fetal_attack_sec), by = "pair_id") |> 
    mutate(fetal_attack_sec = if_else(is.na(fetal_attack_sec), 1801, fetal_attack_sec)) |> 
    filter(time_sec < fetal_attack_sec) |> dplyr::select(-fetal_attack_sec)
}

# plot relative positions ----
{
  df_rel <- df_pair |>
    mutate(rx0 = partner_dis * cos(dir_1to0),
           ry0 = partner_dis * sin(dir_1to0),
           rx1 = partner_dis * cos(dir_0to1),
           ry1 = partner_dis * sin(dir_0to1))
  
  video_list <- unique(df_rel$video )
  
  for(i_v in video_list){
    df_temp <- df_rel |> filter(video == i_v)
    p1 <- ggplot(df_temp, aes(x = rx0, y = ry0)) +
      geom_bin_2d(binwidth = 0.1) +
      facet_wrap(~well_id) +
      scale_fill_viridis() +
      coord_cartesian(xlim = c(-2,2), ylim = c(-2,2)) +
      theme(legend.position = "none", aspect.ratio = 1) +
      labs(x = "", y = "", title = i_v)
    ggsave(p1, file = sprintf("output/relative_pos_each/%s.pdf", i_v), width = 4, height = 3)
  }
  
  ggplot(df_rel, aes(x = rx0, y = ry0)) +
    geom_bin_2d(binwidth = 0.1) +
    facet_wrap(~treat) +
    scale_fill_viridis() +
    coord_cartesian(xlim = c(-2,2), ylim = c(-2,2)) +
    theme(legend.position = "none", aspect.ratio = 1) +
    labs(x = "", y = "")
  ggsave(file = sprintf("output/relative_pos.pdf"), width = 4, height = 3)
}

# tandem prop time development ----
{
  # data prep + plot
  {
    plot_tandem_prop_time <- function(df, time_bin = 60){ 
      df_tandem_time <- df |>
        mutate(time_min = floor(time_sec / time_bin)) |>
        group_by(time_min, treat, pair_id, colony) |>
        summarize(
          interact_prop = mean(interact),
          tandem_prop = mean(tandem),
          non_tandem_interact_prop = mean(!tandem & interact),
          .groups = "drop"
        )
      p <- ggplot(df_tandem_time,
             aes(x = time_min, y = tandem_prop, col = treat)) +
        stat_summary(fun = mean, geom = "line", linewidth = 1) +
        stat_summary(fun.data = mean_se, geom = "ribbon",
                     aes(fill = treat), alpha = 0.2, color = NA) +
        scale_color_manual(values = treat_colors, labels = treat_labels)+
        scale_fill_manual(values = treat_colors, labels = treat_labels)  +
        scale_y_continuous(limits = c(0, 1), breaks = c(0,0.5,1), labels = c(0,0.5,1)) +
        theme_classic(base_size = 10) +
        labs(x = "Time (minutes)", y = "Propotion of tandem")+
        theme(aspect.ratio = 1,
              legend.position = c(0.7,0.9),
              legend.title = element_blank(),
              legend.background = element_blank())
      list(df_tandem_time, p)
    }
    
    df_out <- plot_tandem_prop_time(df_pair, time_bin = 60)
    df_tandem_prop_well = df_out[[1]]
    ggsave(df_out[[2]], filename = "output/tandem_prop_well.pdf", 
            device = cairo_pdf, family = "Arial",
            width = 3, height = 3)
    
    df_out <- plot_tandem_prop_time(df_dish, time_bin = 60)
    df_tandem_prop_dish = df_out[[1]]
    ggsave(df_out[[2]], filename = "output/tandem_prop_dish.pdf", 
           device = cairo_pdf, family = "Arial",
           width = 3, height = 3)
    
  }
  
  # stat
  {
    # well
    {
      df_tandem_prop_well |> filter(tandem_prop > 0, tandem_prop < 1) |> pull(tandem_prop) |> range()
      df_tandem_prop_well <- df_tandem_prop_well |> 
        mutate(logit_tandem_prop = car::logit(tandem_prop, adjust = 0.003, percents = FALSE))
      r <- lmer(logit_tandem_prop ~ time_min * treat + (1 | colony/pair_id), data = df_tandem_prop_well)
      df_res <- tibble(tidy(Anova(r)), formula = deparse(formula(r) ))
      res <- summary(r)
      pairwise_res <- emtrends(r, pairwise ~ treat, var = "time_min")
      
      wb <- createWorkbook()
      addWorksheet(wb, "anova")
      addWorksheet(wb, "slopes")
      addWorksheet(wb, "contrasts")
      
      writeData(wb, "anova",     df_res)
      writeData(wb, "slopes",    as.data.frame(pairwise_res$emtrends))
      writeData(wb, "contrasts", as.data.frame(pairwise_res$contrasts))
      
      saveWorkbook(wb, "output/tandem_prop_well_lmer.xlsx", overwrite = TRUE)
    }
    
    # dish
    {
      df_tandem_prop_dish |> filter(tandem_prop > 0, tandem_prop < 1) |> pull(tandem_prop) |> range()
      df_tandem_prop_dish <- df_tandem_prop_dish |> 
        mutate(logit_tandem_prop = car::logit(tandem_prop, adjust = 0.003, percents = FALSE))
      r <- lmer(logit_tandem_prop ~ time_min * treat + (1 | colony/pair_id), data = df_tandem_prop_dish)
      df_res <- tibble(tidy(Anova(r)), formula = deparse(formula(r) ))
      res <- summary(r)
      pairwise_res <- emtrends(r, pairwise ~ treat, var = "time_min")
      slope_tests <- test(pairwise_res$emtrends)
      
      wb <- createWorkbook()
      addWorksheet(wb, "anova")
      addWorksheet(wb, "slopes")
      addWorksheet(wb, "slopes_P")
      addWorksheet(wb, "contrasts")
      
      writeData(wb, "anova",     df_res)
      writeData(wb, "slopes",    as.data.frame(pairwise_res$emtrends))
      writeData(wb, "slopes_P",    as.data.frame(slope_tests ))
      writeData(wb, "contrasts", as.data.frame(pairwise_res$contrasts))
      
      saveWorkbook(wb, "output/tandem_prop_dish_lmer.xlsx", overwrite = TRUE)
    }
  }
  
}

# tandem event ----
{
  # create tandem event data
  tandem_df <- function(df,
                        gap_max = 1, # number of frames allowed as gap 
                        min_len = 1 ){ # minimum frames to count as tandem
    df_tandem <- df|>
      group_by(pair_id) |>
      arrange(time_sec, .by_group = TRUE) |>
      mutate(
        run_id = rleid(tandem),
        run_len = ave(tandem, run_id, FUN = length)
      ) |>
      mutate(
        tandem = if_else(tandem & run_len >= min_len, TRUE, FALSE)
      ) |>
      mutate(
        cluster_start = tandem & lag(!tandem, default = TRUE),
        cluster_idx = cumsum(cluster_start),
        cluster_idx = if_else(tandem, cluster_idx, NA_integer_),
        pair_event = if_else(
          !is.na(cluster_idx),
          sprintf("%s_%02d", pair_id, cluster_idx),
          NA_character_
        )
      ) |>
      dplyr::select(-cluster_start, -run_id, -run_len) |>
      ungroup()
    
    df_pair_dur <- df_tandem |> filter(time_sec < 1800.1) |>
      filter(!is.na(pair_event)) |>
      group_by(pair_event, treat, pair_id) |>
      summarise(
        start_time = first(time_sec),
        end_time = last(time_sec),
        tandem_event = first(cluster_idx),
        duration = (n() * 0.2), 
        cens = !(dplyr::near(last(time_sec), max(df$time_sec)) | dplyr::near(first(time_sec), 0)),
        leader_moved_dis = sum(post_step_0, na.rm = T),
        follower_moved_dis = sum(post_step_1, na.rm = T),
        .groups = "drop"
      )
    df_pair_dur
  }
  
  # dish analysis
  {
    df_tandem <- tandem_df(df_dish, gap_max = 1, min_len = 1)
    
    wb <- createWorkbook()
    addWorksheet(wb, "short_tandem_anova")
    addWorksheet(wb, "short_tandem_coeff")
    addWorksheet(wb, "longest_tandem_time_anova")
    addWorksheet(wb, "longest_tandem_time_coeff")
    addWorksheet(wb, "longest_tandem_time_cox.zph")
    addWorksheet(wb, "longest_tandem_event_anova")
    addWorksheet(wb, "longest_tandem_event_coeff")
    addWorksheet(wb, "longest_tandem_duration_anova")
    addWorksheet(wb, "longest_tandem_duration_coeff")
    addWorksheet(wb, "longest_tandem_duration_cox.zph")
    addWorksheet(wb, "overlap")
    
    # distribution of tandem duration
    ggplot(df_tandem, aes(x = duration, fill = treat)) +
      geom_histogram(bins = 80, alpha = 0.5) +
      scale_fill_manual(values = treat_colors) +
      scale_x_log10() +
      geom_vline(xintercept = c(1.5), linetype = "dashed") +
      facet_wrap(~treat, labeller = as_labeller(treat_labels)) +
      theme_bw(base_size = 10) +
      labs(x = "Tandem duration (sec)", y = "Count") +
      theme(aspect.ratio = 2/3,
            legend.position = "none",
            panel.grid = element_blank(),
            strip.background = element_blank())  
    ggsave(filename = "output/tandem_duration_hist_dish.pdf", 
          device = cairo_pdf, family = "Arial",
          width = 5, height = 3)
    
    # short tandem events (< 1.6 sec = 8 frames) are different events
    df_tandem <- df_tandem |>
      mutate(tandem_type = if_else(duration < 1.5, "short", "long"),
             start_time_min = start_time/60)
    
    # short tandem events happen a lot at the beginning in MM, but not in FM
    # use min to avoid convergence error
    fit <- glmer(tandem_type == "short" ~ treat * start_time_min + (1|pair_id),
                 family = binomial, data = df_tandem)
    df_res <- tibble(tidy(Anova(fit)), formula = deparse(formula(fit) ))
    res <- summary(fit)
    
    writeData(wb, "short_tandem_anova", df_res)
    writeData(wb, "short_tandem_coeff", res$coefficients |> round(3))
    
    # only focus on long tandem events
    # df_tandem_long <- df_tandem |> filter(tandem_type == "long")
    # I decided remove this analysis
    # fit_cox <- coxme(Surv(duration, cens) ~ start_time * treat + (1|pair_id), data = df_tandem_long)
    # as duration and start_time have to be correlated in design for sampling patterns, even with cens
    
    # longest tandem
    df_longest_tandem <- df_tandem |> group_by(pair_id) |>
      slice_max(duration, n = 1, with_ties = TRUE) |>
      ungroup()
    
    # longest tandem start later in MM (in sec)
    ggplot(df_longest_tandem, aes(x = treat, y = start_time, col = treat, fill = treat)) +
      geom_boxplot(width = 0.1, outliers = F, alpha = 0.1) +
      geom_jitter(width = 0.1) +
      scale_color_manual(values = treat_colors, labels = treat_labels)+
      scale_fill_manual(values = treat_colors, labels = treat_labels)  +
      scale_x_discrete(labels = treat_labels)+
      theme_classic(base_size = 9) +
      labs(x = "", y = "Start time of the logest tandem running (sec)")+
      theme(aspect.ratio = 1,
            legend.position = "none")
    
    ggsave(filename = "output/longest_tandem_start_sec_dish.pdf", 
           device = cairo_pdf, family = "Arial",
           width = 3, height = 3)
    
    fit_cox <- coxph(Surv(start_time) ~ treat, data = df_longest_tandem)
    df_res <- tibble(tidy(Anova(fit_cox)), formula = deparse(formula(fit_cox) ))
    res <- summary(fit_cox)
    res$coefficients
    
    writeData(wb, "longest_tandem_time_anova", df_res)
    writeData(wb, "longest_tandem_time_coeff", res$coefficients |> round(3))
    writeData(wb, "longest_tandem_time_cox.zph", cox.zph(fit_cox))
    
    # longest tandem start later in MM (in event number)
    ggplot(df_longest_tandem, aes(x = treat, y = tandem_event, col = treat, fill = treat)) +
      geom_boxplot(width = 0.1, outliers = F, alpha = 0.1) +
      geom_jitter(width = 0.1) +
      scale_color_manual(values = treat_colors, labels = treat_labels)+
      scale_fill_manual(values = treat_colors, labels = treat_labels)  +
      scale_x_discrete(labels = treat_labels)+
      scale_y_continuous(breaks = c(0,50,100))+
      theme_classic(base_size = 9) +
      labs(x = "", y = "The logest tandem running event")+
      theme(aspect.ratio = 1,
            legend.position = "none")
    
    ggsave(filename = "output/longest_tandem_start_event_dish.pdf", 
           device = cairo_pdf, family = "Arial",
           width = 3, height = 3)
    
    r <- glm.nb(tandem_event ~ treat,  data = df_longest_tandem)
    df_res <- tibble(tidy(Anova(r)), formula = deparse(formula(r) ))
    res <- summary(r)
    res$coefficients
    
    writeData(wb, "longest_tandem_event_anova", df_res)
    writeData(wb, "longest_tandem_event_coeff", res$coefficients |> round(3))

    # overlap of the distribution
    ggplot(df_longest_tandem, aes(x = duration, y = treat, fill = treat, color = treat)) +
      stat_halfeye( width = 0.4, position = position_nudge(y = 0.11),
                    .width = 0, point_colour = NA, alpha = 0.7, scale = 0.5) +
      stat_dots( side = "bottom", dotsize = 0.75, binwidth = 0.08,
                 alpha = 0.6, position = position_nudge(y = -0.1) ) +
      geom_boxplot( width = 0.06, outlier.shape = NA,
                    fill = "white", alpha = 0.6, color = "grey30", linewidth = 0.4 ) +
      scale_x_log10( breaks = c(1, 10, 100, 1000), labels = scales::label_number()) +
      scale_y_discrete(labels = treat_labels)+ 
      coord_cartesian(ylim = c(1,  2.2)) +
      scale_color_manual(values = treat_colors, labels = treat_labels)+
      scale_fill_manual(values = treat_colors, labels = treat_labels)  +
      labs(x = "Longest tandem duration (s)", y = NULL) +
      theme_classic(base_size = 10) +
      theme(aspect.ratio = 1.25, legend.position = "none")
    
    ggsave(filename = "output/longest_tandem_duration_dish.pdf", 
           device = cairo_pdf, family = "Arial",
           width = 3, height = 3)
    
    fit_cox <- coxph(Surv(duration, cens) ~ treat, data = df_longest_tandem)
    df_res <- tibble(tidy(Anova(fit_cox)), formula = deparse(formula(fit_cox) ))
    res <- summary(fit_cox)
    res$coefficients
    
    writeData(wb, "longest_tandem_duration_anova", df_res)
    writeData(wb, "longest_tandem_duration_coeff", res$coefficients |> round(3))
    writeData(wb, "longest_tandem_duration_cox.zph", cox.zph(fit_cox))
    
    
    # OVL from KDE
    d1 <- density(log10(df_longest_tandem$duration[df_longest_tandem$treat == "FM"]))
    d2 <- density(log10(df_longest_tandem$duration[df_longest_tandem$treat == "MM"]))
    # Interpolate to common grid
    x_grid <- seq(min(d1$x, d2$x), max(d1$x, d2$x), length.out = 1000)
    f1 <- approx(d1$x, d1$y, x_grid)$y
    f2 <- approx(d2$x, d2$y, x_grid)$y
    ovl <- sum(pmin(f1, f2, na.rm = TRUE)) * diff(x_grid[1:2]) / 
      (0.5 * (sum(f1, na.rm = TRUE) + sum(f2, na.rm = TRUE)) * diff(x_grid[1:2]))
    ovl
    writeData(wb, "overlap", ovl)
    
    saveWorkbook(wb, "output/tandem_duration_dish.xlsx", overwrite = TRUE)
    
  }
  
  # well analysis
  {
    df_tandem <- tandem_df(df_pair, gap_max = 1, min_len = 1)
    
    wb <- createWorkbook()
    addWorksheet(wb, "short_tandem_anova")
    addWorksheet(wb, "short_tandem_coeff")
    addWorksheet(wb, "longest_tandem_time_anova")
    addWorksheet(wb, "longest_tandem_time_coeff")
    addWorksheet(wb, "longest_tandem_time_cox.zph")
    addWorksheet(wb, "longest_tandem_event_anova")
    addWorksheet(wb, "longest_tandem_event_coeff")
    addWorksheet(wb, "longest_tandem_duration_anova")
    addWorksheet(wb, "longest_tandem_duration_coeff")
    addWorksheet(wb, "longest_tandem_duration_cox.zph")
    addWorksheet(wb, "tandem_duration_anova")
    addWorksheet(wb, "tandem_duration_coeff")
    addWorksheet(wb, "tandem_duration_cox.zph")
    
    # distribution of tandem duration
    ggplot(df_tandem, aes(x = duration, fill = treat)) +
      geom_histogram(bins = 80, alpha = 0.5) +
      scale_fill_manual(values = treat_colors) +
      scale_x_log10() +
      geom_vline(xintercept = c(1.5), linetype = "dashed") +
      facet_wrap(~treat, labeller = as_labeller(treat_labels)) +
      theme_bw(base_size = 10) +
      labs(x = "Tandem duration (sec)", y = "Count") +
      theme(aspect.ratio = 2/3,
            legend.position = "none",
            panel.grid = element_blank(),
            strip.background = element_blank())  
    ggsave(filename = "output/tandem_duration_hist_well.pdf", 
           device = cairo_pdf, family = "Arial",
           width = 5, height = 3)
    
    # short tandem events (< 1.6 sec = 8 frames) are different events
    df_tandem <- df_tandem |>
      mutate(tandem_type = if_else(duration < 1.5, "short", "long"),
             start_time_min = start_time/60)
    
    # short tandem events happen a lot at the beginning in MM, but not in FM
    # use min to avoid convergence error
    fit <- glmer(tandem_type == "short" ~ treat * start_time_min + (1|pair_id),
                 family = binomial, data = df_tandem)
    df_res <- tibble(tidy(Anova(fit)), formula = deparse(formula(fit) ))
    res <- summary(fit)
    pairwise_res <- emtrends(fit, pairwise ~ treat, var = "start_time_min")
    
    writeData(wb, "short_tandem_anova", df_res)
    writeData(wb, "short_tandem_coeff", res$coefficients |> round(3))
    
    pred_df <- expand.grid(
      treat = unique(df_tandem$treat),
      start_time_min = seq(0, max(df_tandem$start_time_min), length.out = 100)
    )
    pred_df$prob <- predict(fit, pred_df, re.form = NA, type = "response")
    
    ggplot(pred_df, aes(x = start_time_min, y = prob, color = treat)) +
      geom_line(linewidth = 1) +
      scale_color_manual(values = treat_colors, labels = treat_labels) +
      labs(x = "Time (min)", y = "P(short tandem)", color = NULL) +
      theme_classic(base_size = 10) +
      theme(aspect.ratio = 1, legend.position = c(0.8, 0.8))
    
    # only focus on long tandem events
    df_tandem_long <- df_tandem |> filter(tandem_type == "long")
    
    ggsurvplot(
      survfit(Surv(duration, cens) ~ treat, data = df_tandem_long),
      data = df_tandem_long,
      fun = "cumhaz",
      censor = TRUE,
      ggtheme = theme_classic(base_size = 10),
      color = "treat",
    )$plot+ 
      scale_color_manual(values = treat_colors, labels = treat_labels)  +
      labs(x = "Duration (sec)", y = "Cumulative hazard") +
      theme(aspect.ratio = 3/4,
            legend.title = element_blank(),
            legend.position = c(0.8,0.4))
    
    ggsave( filename = "output/tandem_duration_cum_hazard_well.pdf", 
            device = cairo_pdf, family = "Arial",
            width = 4, height = 3)
  
    fit_cox <- coxme(Surv(duration, cens) ~ treat+ (1|pair_id), data = df_longest_tandem)
    df_res <- tibble(tidy(Anova(fit_cox)), formula = deparse(formula(fit_cox) ))
    res <- summary(fit_cox)
    
    writeData(wb, "tandem_duration_anova", df_res)
    writeData(wb, "tandem_duration_coeff", res$coefficients |> round(3))
    writeData(wb, "tandem_duration_cox.zph", cox.zph(fit_cox))
    
    
    # longest tandem
    df_longest_tandem <- df_tandem |> group_by(pair_id) |>
      slice_max(duration, n = 1, with_ties = TRUE) |>
      ungroup()
    
    # longest tandem start later in MM (in sec)
    ggplot(df_longest_tandem, aes(x = treat, y = start_time, col = treat, fill = treat)) +
      geom_boxplot(width = 0.1, outliers = F, alpha = 0.1) +
      geom_jitter(width = 0.1) +
      scale_color_manual(values = treat_colors, labels = treat_labels)+
      scale_fill_manual(values = treat_colors, labels = treat_labels)  +
      scale_x_discrete(labels = treat_labels)+
      theme_classic(base_size = 9) +
      labs(x = "", y = "Start time of the logest tandem running (sec)")+
      theme(aspect.ratio = 1,
            legend.position = "none")
    
    ggsave(filename = "output/longest_tandem_start_sec_well.pdf", 
           device = cairo_pdf, family = "Arial",
           width = 3, height = 3)
    
    fit_cox <- coxph(Surv(start_time) ~ treat, data = df_longest_tandem)
    df_res <- tibble(tidy(Anova(fit_cox)), formula = deparse(formula(fit_cox) ))
    res <- summary(fit_cox)

    writeData(wb, "longest_tandem_time_anova", df_res)
    writeData(wb, "longest_tandem_time_coeff", res$coefficients |> round(3))
    writeData(wb, "longest_tandem_time_cox.zph", cox.zph(fit_cox))
    
    # longest tandem start later in MM (in event number)
    ggplot(df_longest_tandem, aes(x = treat, y = tandem_event, col = treat, fill = treat)) +
      geom_boxplot(width = 0.1, outliers = F, alpha = 0.1) +
      geom_jitter(width = 0.1) +
      scale_color_manual(values = treat_colors, labels = treat_labels)+
      scale_fill_manual(values = treat_colors, labels = treat_labels)  +
      scale_x_discrete(labels = treat_labels)+
      scale_y_continuous(breaks = c(0,50,100))+
      theme_classic(base_size = 9) +
      labs(x = "", y = "The logest tandem running event")+
      theme(aspect.ratio = 1,
            legend.position = "none")
    
    ggsave(filename = "output/longest_tandem_start_event_well.pdf", 
           device = cairo_pdf, family = "Arial",
           width = 3, height = 3)
    
    r <- glm.nb(tandem_event ~ treat,  data = df_longest_tandem)
    df_res <- tibble(tidy(Anova(r)), formula = deparse(formula(r) ))
    res <- summary(r)
    
    writeData(wb, "longest_tandem_event_anova", df_res)
    writeData(wb, "longest_tandem_event_coeff", res$coefficients |> round(3))
    
    # overlap of the distribution
    ggplot(df_longest_tandem, aes(x = duration, y = treat, fill = treat, color = treat)) +
      stat_halfeye( width = 0.4, position = position_nudge(y = 0.11),
                    .width = 0, point_colour = NA, alpha = 0.7, scale = 0.5) +
      stat_dots( side = "bottom", dotsize = 0.75, binwidth = 0.08,
                 alpha = 0.6, position = position_nudge(y = -0.1) ) +
      geom_boxplot( width = 0.06, outlier.shape = NA,
                    fill = "white", alpha = 0.6, color = "grey30", linewidth = 0.4 ) +
      scale_x_log10( breaks = c(1, 10, 100, 1000), labels = scales::label_number()) +
      scale_y_discrete(labels = treat_labels)+ 
      #coord_cartesian(ylim = c(1,  2.2)) +
      scale_color_manual(values = treat_colors, labels = treat_labels)+
      scale_fill_manual(values = treat_colors, labels = treat_labels)  +
      labs(x = "Longest tandem duration (s)", y = NULL) +
      theme_classic(base_size = 10) +
      theme(aspect.ratio = 1.25, legend.position = "none")
    
    ggsave(filename = "output/longest_tandem_duration_well.pdf", 
           device = cairo_pdf, family = "Arial",
           width = 3, height = 3)
    
    fit_cox <- coxph(Surv(duration, cens) ~ treat, data = df_longest_tandem)
    df_res <- tibble(tidy(Anova(fit_cox)), formula = deparse(formula(fit_cox) ))
    res <- summary(fit_cox)
    res$coefficients
    
    writeData(wb, "longest_tandem_duration_anova", df_res)
    writeData(wb, "longest_tandem_duration_coeff", res$coefficients |> round(3))
    writeData(wb, "longest_tandem_duration_cox.zph", cox.zph(fit_cox))
    
    saveWorkbook(wb, "output/tandem_duration_well.xlsx", overwrite = TRUE)
    
  }
  
  
  df_tandem <- tandem_df(df_pair, gap_max = 1, min_len = 1)
  ggplot(df_tandem |> filter(duration > 1.5), 
         aes(x = leader_moved_dis/duration, y  = log10(duration))) +
    geom_point() +
    stat_smooth()+
    facet_wrap(~treat)
}

# speed ----
{
  df_dis_dist <- df_pair |> pivot_longer(cols = starts_with("post_step")) |> 
    dplyr::select(name, value, treat, tandem)
  
  ggplot(df_dis_dist |> filter(tandem)) +
    geom_density_ridges(aes(x = value, y = treat, fill= name), stat = "binline", 
                        alpha = 0.5, binwidth = 0.04, scale = 0.85) +
    labs(x = "Step length (BL)", y = "") +
    scale_y_discrete(expand = c(0, 0.1),labels = treat_labels) +
    scale_x_continuous(breaks = c(0,0.5,1), labels =  c(0,0.5,1)) +
    scale_fill_manual(values = c(post_step_0 = "#1B7837", post_step_1 = "#D8B58A")) +
    coord_cartesian(xlim = c(0,1.5)) +
    theme_classic(base_size = 10) +
    theme(aspect.ratio = 3,
          legend.position = "none")
  
  ggsave("output/step_distribution.pdf", device = cairo_pdf, family = "Arial",
         width = 3, height = 5.5)
  
  df_dis_dist <- df_dish |> pivot_longer(cols = starts_with("post_step")) |> 
    dplyr::select(name, value, treat, tandem)
  
  ggplot(df_dis_dist |> filter(tandem)) +
    geom_density_ridges(aes(x = value, y = treat, fill= name), stat = "binline", 
                        alpha = 0.5, binwidth = 0.04, scale = 0.85) +
    labs(x = "Step length (BL)", y = "") +
    scale_y_discrete(expand = c(0, 0.1),labels = treat_labels) +
    scale_x_continuous(breaks = c(0,0.5,1), labels =  c(0,0.5,1)) +
    scale_fill_manual(values = c(post_step_0 = "#1B7837", post_step_1 = "#D8B58A")) +
    coord_cartesian(xlim = c(0,1.5)) +
    theme_classic(base_size = 10) +
    theme(aspect.ratio = 3,
          legend.position = "none")
  
  df_pair |>  group_by(pair_id) |> 
    dplyr::select(pair_id, time_sec, treat, tandem, post_step_0, post_step_1) |>
    arrange(time_sec, .by_group = TRUE) |>
    mutate(
      run_id = rleid(tandem),
      run_len = ave(tandem, run_id, FUN = length)
    ) |>
    mutate(
      cluster_start = tandem & lag(!tandem, default = TRUE),
      cluster_idx = cumsum(cluster_start),
      cluster_idx = if_else(tandem, cluster_idx, NA_integer_),
      pair_event = if_else(
        !is.na(cluster_idx),
        sprintf("%s_%02d", pair_id, cluster_idx),
        NA_character_
        )
      )
}

df_analysis <- df_pair |> 
  group_by(pair_id) |> 
  dplyr::select(pair_id, time_sec, treat, tandem, post_step_0, post_step_1, follow_dis) |>
  arrange(time_sec, .by_group = TRUE) |>
  mutate(
    run_id = rleid(tandem)
  ) |> 
  group_by(pair_id, run_id) |> 
  mutate(
    run_duration = max(time_sec) - min(time_sec),
    tandem = if_else(tandem & run_duration < 1.5, FALSE, tandem)
  ) |> 
  group_by(pair_id) |> 
  mutate(
    run_id = rleid(tandem), # Recalculate IDs since some TRUE states became FALSE
    is_onset = tandem & lag(!tandem, default = FALSE),
    is_offset = !tandem & lag(tandem, default = FALSE)
  ) |> 
  ungroup()

extract_window <- function(df, flag_col, window_size = 10) {
  event_indices <- which(df[[flag_col]])
  if(length(event_indices) == 0) return(tibble())
  
  lapply(seq_along(event_indices), function(i) {
    idx <- event_indices[i]
    start_idx <- max(1, idx - window_size)
    end_idx <- min(nrow(df), idx + window_size)
    
    df[start_idx:end_idx, ] |> 
      mutate(
        event_instance = i,
        relative_step = (start_idx:end_idx) - idx
      )
  }) |> bind_rows()
}

df_onsets <- df_analysis |> 
  group_split() |> 
  lapply(extract_window, flag_col = "is_onset", window_size = 5) |> 
  bind_rows()

df_onsets <- df_onsets |> filter( ((relative_step < 0) & !tandem) | ((relative_step >= 0) & tandem) )   

df_plot_onset <- df_onsets |>
  pivot_longer(cols = c(post_step_0, post_step_1), names_to = "step_type", values_to = "step_length") |>
  #pivot_longer(cols = c(acc_0, acc_1), names_to = "step_type", values_to = "step_length") |>
  group_by(relative_step, step_type, treat) |>
  summarise(mean_step = mean(step_length, na.rm = TRUE), .groups = 'drop')

ggplot(df_plot_onset, aes(x = relative_step, y = mean_step, color = step_type)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  geom_line(size = 1) +
  facet_wrap(~treat) +
  labs(
    title = "Step Length Dynamics Heading Into Tandem Run",
    x = "Relative Steps (0 = Tandem Starts)",
    y = "Mean Step Length"
  ) +
  theme_minimal()

df_offsets <- df_analysis |> 
  group_split() |> 
  lapply(extract_window, flag_col = "is_offset", window_size = 15) |> 
  bind_rows()


df_offsets <- df_offsets |> filter( ((relative_step < 0) & tandem) | ((relative_step >= 0) & !tandem) )   

df_dis_dist <- df_offsets |> pivot_longer(cols = starts_with("post_step")) |> 
  dplyr::select(name, value, treat, tandem)

ggplot(df_dis_dist) +
  geom_density_ridges(aes(x = value, y = treat, fill= name), stat = "binline", 
                      alpha = 0.5, binwidth = 0.04, scale = 0.85) +
  labs(x = "Step length (BL)", y = "") +
  scale_y_discrete(expand = c(0, 0.1),labels = treat_labels) +
  scale_x_continuous(breaks = c(0,0.5,1), labels =  c(0,0.5,1)) +
  scale_fill_manual(values = c(post_step_0 = "#1B7837", post_step_1 = "#D8B58A")) +
  coord_cartesian(xlim = c(0,1.5)) +
  theme_classic(base_size = 10) +
  theme(aspect.ratio = 3,
        legend.position = "none")


df_plot_offset <- df_offsets |>
  pivot_longer(cols = c(post_step_0, post_step_1), names_to = "step_type", values_to = "step_length") |>
  #pivot_longer(cols = c(acc_0, acc_1), names_to = "step_type", values_to = "step_length") |>
  group_by(relative_step, step_type, treat) |>
  summarise(mean_step = mean(step_length, na.rm = TRUE), .groups = 'drop')

ggplot(df_plot_offset, aes(x = relative_step, y = mean_step, color = step_type)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  geom_line(size = 1) +
  facet_wrap(~treat) +
  labs(
    title = "Step Length Dynamics Heading Into Tandem Run",
    x = "Relative Steps (0 = Tandem Starts)",
    y = "Mean Step Length"
  ) +
  theme_minimal()

df_offsets |> filter(relative_step < 0)


# acc ----1
{
  df_dis_acc <- df_analysis |> dplyr::select(follow_dis, acc_0, acc_1, treat, tandem) |>
    mutate(follow_dis_bin = round(follow_dis, 1)) |>
    pivot_longer(cols = starts_with("acc"))
  
  ggplot(df_dis_acc |> filter(tandem), 
         aes(x = follow_dis_bin, y = value)) + 
    stat_summary(geom = "ribbon", fun.data = mean_se, alpha = 0.2, aes(fill = name) ) +
    stat_summary(geom = "line", fun = mean, linewidth = 1, aes(col = name)) +
    scale_color_manual(values = c(acc_0 = "#1B7837", acc_1 = "#D8B58A")) +
    scale_fill_manual(values = c(acc_0 = "#1B7837", acc_1 = "#D8B58A")) +
    geom_hline(yintercept = 0)+
    scale_x_continuous(breaks = c(0,0.5,1), labels =  c(0,0.5,1)) +
    scale_y_continuous(breaks = c(-0.03, 0, 0.03), 
                       labels = c(-0.03, 0, 0.03)) +
    #coord_cartesian(xlim = c(0, 1.1), ylim = c(-0.035, 0.035))+
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
