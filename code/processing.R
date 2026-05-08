
{
  source("code/source.R")
  scale_factor <- 112/1440 #mm/pix
}
# ------------------------------------------------------------------------------

# data prep ----
{
  df_FM <-     arrow::read_feather("data_fmt/trajectory/FM_df.feather")
  df_alates <- arrow::read_feather("data_fmt/trajectory/alates_df.feather")
  df_worker <- arrow::read_feather("data_fmt/trajectory/worker_df.feather")
  df_soldier <- arrow::read_feather("data_fmt/trajectory/soldier_df.feather")
  
  pattern <- "^.*\\d+-\\d+"
  df_FM <- df_FM %>% mutate(video = str_extract(video, pattern))
  df_alates <- df_alates %>% mutate(video = str_extract(video, pattern))
  df_worker <- df_worker %>% mutate(video = str_extract(video, pattern))
  df_soldier <- df_soldier %>% mutate(video = str_extract(video, pattern))
  
  # skip_list
  df_alates <- df_alates %>% filter(!(video == "Ret_ama_FW_F_1-6" & ind_id %in% c(4, 5)))
  df_alates <- df_alates %>% filter(!(video == "Ret_ama_FW_H_7-12" & ind_id %in% c(4)))
  df_alates <- df_alates %>% filter(!(video == "Ret_ama_FW_I_1-6" & ind_id %in% c(1,4)))
  df_alates <- df_alates %>% filter(!(video == "Ret_ama_FW_I_7-12" & ind_id %in% c(1))) 
  df_alates <- df_alates %>% filter(!(video == "Ret_ama_MW_G_7-12" & ind_id %in% c(4,5)))
  df_alates <- df_alates %>% filter(!(video == "Ret_ama_SM_I_7-12"& ind_id %in% c(0, 3)))
  
  df_worker <- df_worker %>% filter(!(video == "Ret_ama_FW_F_1-6" & ind_id %in% c(4, 5)))
  df_worker <- df_worker %>% filter(!(video == "Ret_ama_FW_G_1-6" & ind_id %in% c(1,2,4,5)))
  df_worker <- df_worker %>% filter(!(video == "Ret_ama_FW_H_7-12" & ind_id %in% c(4)))
  df_worker <- df_worker %>% filter(!(video == "Ret_ama_FW_I_1-6" & ind_id %in% c(1,4)))
  df_worker <- df_worker %>% filter(!(video == "Ret_ama_FW_I_7-12" & ind_id %in% c(1))) 
  df_worker <- df_worker %>% filter(!(video == "Ret_ama_MW_G_7-12" & ind_id %in% c(4,5)))
  df_soldier <- df_soldier %>% filter(!(video == "Ret_ama_SM_I_7-12"& ind_id %in% c(0, 3)))
}

# plot all trajectories ----
{
  plot_traj <- function(df, ...){
    ggplot(df, aes(x = x_body, y = y_body, col = as.factor(ind_id) ))+
      scale_color_viridis(discrete = T, option = "D")+
      geom_path(alpha = 1)+
      coord_cartesian(xlim = c(0, 2100), ylim=c(0, 1400)) +
      scale_y_reverse() +
      facet_wrap(~video)+
      theme_classic()+
      theme(aspect.ratio = 2/3, legend.position = "none")+
      labs(...)
  }
  
  save_comparison_plot <- function(df1, df2, video_name) {
    p1 <- plot_traj(df1, title = video_name)
    p2 <- plot_traj(df2)
    ggsave(
      filename = file.path("output/trajectory", paste0(video_name, ".png")),
      plot = p1 + p2, width = 6, height = 4
    )
  }
  
  # FM
  video_list <- unique(df_FM$video)
  for(i in 1:length(video_list)){
    save_comparison_plot(
      df1 = df_FM %>% filter(video == video_list[i] & ind_id %% 2 == 0),
      df2 = df_FM %>% filter(video == video_list[i] & ind_id %% 2 == 1),
      video_name = video_list[i])
  }
  
  # alate-worker
  video_list_a <- unique(df_alates$video)
  video_list_w <- unique(df_worker$video)
  video_list <- video_list_a[video_list_a %in% video_list_w]
  for(i in 1:length(video_list)){
    save_comparison_plot(
      df1 = df_alates %>% filter(video == video_list[i]),
      df2 = df_worker %>% filter(video == video_list[i]),
      video_name = video_list[i])
  }
  
  # alate-soldier
  video_list_s <- unique(df_soldier$video)
  video_list <- video_list_a[video_list_a %in% video_list_s]
  for(i in 1:length(video_list)){
    save_comparison_plot(
      df1 = df_alates %>% filter(video == video_list[i]),
      df2 = df_soldier %>% filter(video == video_list[i]),
      video_name = video_list[i])
  }
}

# data pairing ----
{
  df_FM <- df_FM |> mutate(
    colony = str_split_i(video, "_", 4),
    treat  = str_split_i(video, "_", 3),
    well_id = ind_id %/% 2,
    role = if_else( ind_id %% 2 == 0, "female", "male" ),
    role_id = if_else( ind_id %% 2 == 0, 0, 1)
  )
  
  df_alates <- df_alates |> group_by(video) |>
    mutate(
    colony = str_split_i(video, "_", 4),
    treat  = str_split_i(video, "_", 3),
    well_id = dense_rank(ind_id),
    role = if_else( treat == "FW", "female", "male"),
    role_id = 1
  )
  
  df_worker <- df_worker |> group_by(video) |>
    mutate(
      colony = str_split_i(video, "_", 4),
      treat  = str_split_i(video, "_", 3),
      well_id = dense_rank(ind_id),
      role = "worker", role_id = 0
    )
  
  df_soldier <- df_soldier |> group_by(video) |>
    mutate(
      colony = str_split_i(video, "_", 4),
      treat  = str_split_i(video, "_", 3),
      well_id = dense_rank(ind_id),
      role = "soldier", role_id = 0
    )
  
  df_all <- bind_rows(df_FM, df_alates, df_worker, df_soldier)
  df_all <- df_all |> mutate(ind_name = paste(video, well_id, role, sep = "_"))

  df_body <- df_all |> mutate(body_length = euclid_dis(x_head, y_head, x_body, y_body) +
                     euclid_dis(x_tip, y_tip, x_body, y_body)) |>
    group_by(video, well_id, role, ind_name) |>
    summarise(body_length = mean(body_length, na.rm = T), .groups = "drop")
  
  # some tracking errors fix
  df_all <- df_all |>
    mutate(across(c(starts_with("x_"), starts_with("y_")), ~ if_else(
        video == "Ret_ama_MW2_I_1-6" & time_sec == 1719.0 & well_id == 2 & role_id == 0,
        mean(.[video == "Ret_ama_MW2_I_1-6" & time_sec %in% c(1718.8, 1719.2) & well_id == 2 & role_id == 0]),
        .
      ))
    )
  
  save(df_all, df_body, file = "data_fmt/df_all.rda")
  
  
}

# plot relative positions ----
{
  load("data_fmt/df_all.rda")
  
  df_pair <- df_all |> pivot_wider(
    id_cols = c(video, well_id, time_sec, colony, treat), 
    names_from = role_id,
    values_from = starts_with("x_") | starts_with("y_")
  )
  
  
  df_body <- df_body |> group_by(video, well_id) |> 
    summarise(body_length = mean(body_length))
  
  df_pair <- df_pair |> left_join(df_body, by = c("video", "well_id")) |>
    mutate(across(starts_with("x_"), ~ .x / body_length),
           across(starts_with("y_"), ~ .x / body_length),
           partner_dis = euclid_dis(x_body_0, y_body_0, x_body_1, y_body_1),
           follow_dis = euclid_dis(x_tip_0, y_tip_0, x_head_1, y_head_1),
           lead_dis   = euclid_dis(x_tip_1, y_tip_1, x_head_0, y_head_0),
           ang_0 = atan2(y_head_0 - y_tip_0, x_head_0 - x_tip_0),
           ang_1 = atan2(y_head_1 - y_tip_1, x_head_1 - x_tip_1),
           ang_1to0 = atan2(y_body_0 - y_body_1, x_body_0 - x_body_1),
           ang_0to1 = ang_1to0 + pi,
           dir_1to0 = ang_1to0 - ang_1,
           dir_0to1 = ang_0to1 - ang_0
    ) 
  
  
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
    ggsave(p1, file = sprintf("output/relative_pos/%s.pdf", i_v), width = 4, height = 3)
  }
  
  ggplot(df_rel, aes(x = rx0, y = ry0)) +
    geom_bin_2d(binwidth = 0.1) +
    facet_wrap(~treat) +
    scale_fill_viridis() +
    coord_cartesian(xlim = c(-2,2), ylim = c(-2,2)) +
    theme(legend.position = "none", aspect.ratio = 1) +
    labs(x = "", y = "")
  
  save(df_pair, file = "data_fmt/df_pair.rda")
}


