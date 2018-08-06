
# Preparatory settings ----------------------------------------------------

sett_query <- c()
sett_query$db_name <- "URBAN-MV-VIE_UniBw_Study-1"
sett_query$db_conn_name <- dbFindConnObj(sett_query$db_name, output = F)
sett_query$src_name_prefix <- "t_glances_rates1st"
sett_query$src_name_suffix <- "dti_rnd1"
sett_query$df_name_prefix <- "study1"
sett_query$pxx <- c(1:18)
sett_query$round <- c("normal", "stress")
sett_query$subject <- NULL
sett_query$col_name_am_suffix <- "dti_m_rnd1"
sett_query$col_name_am <- paste_("pxx", sett_query$col_name_am_suffix)
sett_query$am_limit1 <- -50
sett_query$am_limit2 <- 0
sett_query$am_buffer <- 0
sett_query$col_names_session <-
  c("round_txt")

sett_query$col_names_data <-
  c("act_nr_PER_act_level_1st_ON_act_n_PER_act_level_1st__0",
    "act_nr_PER_act_level_1st_ON_act_n_PER_act_level__0",
    "act_nr_PER_act_level_1st_ON_act_n__0",
    "act_nr_PER_act_level_1st_ON_cases_n__0",
    "act_nr_PER_act_level_1st_ON_act_n_PER_act_level_1st__1",
    "act_nr_PER_act_level_1st_ON_act_n_PER_act_level__1",
    "act_nr_PER_act_level_1st_ON_act_n__1",
    "act_nr_PER_act_level_1st_ON_cases_n__1",
    "act_nr_PER_act_level_1st_ON_act_n_PER_act_level_1st__2",
    "act_nr_PER_act_level_1st_ON_act_n_PER_act_level__2",
    "act_nr_PER_act_level_1st_ON_act_n__2",
    "act_nr_PER_act_level_1st_ON_cases_n__2",
    "act_nr_PER_act_level_1st_ON_act_n_PER_act_level_1st__3",
    "act_nr_PER_act_level_1st_ON_act_n_PER_act_level__3",
    "act_nr_PER_act_level_1st_ON_act_n__3",
    "act_nr_PER_act_level_1st_ON_cases_n__3",
    "act_nr_PER_act_level_1st_ON_act_n_PER_act_level_1st__4",
    "act_nr_PER_act_level_1st_ON_act_n_PER_act_level__4",
    "act_nr_PER_act_level_1st_ON_act_n__4",
    "act_nr_PER_act_level_1st_ON_cases_n__4",
    "act_nr_PER_act_level_1st_ON_act_n_PER_act_level_1st__6",
    "act_nr_PER_act_level_1st_ON_act_n_PER_act_level__6",
    "act_nr_PER_act_level_1st_ON_act_n__6",
    "act_nr_PER_act_level_1st_ON_cases_n__6")

sett_query$filter$sets <-
  list(
    list(sett_id_names$active$round, sett_query$round, "=", "OR"),
    list(sett_id_names$active$subject, sett_query$subject, "=", "OR"),
    list(sett_query$col_name_am, 
         sett_query$am_limit1 - sett_query$dist_buffer, ">="),
    list(sett_query$col_name_am, 
         sett_query$am_limit2 + sett_query$dist_buffer, "<=")
  )
sett_query$filter$bool_op_between <- c("OR")
sett_query$filter$bool_op_between <- c("AND")



# Fetch data --------------------------------------------------------------

dbGetQuery_pxxBatch(sett_query$db_conn_name, 
                    sett_query, 
                    bind_rows = T, 
                    include_other_am = F)



# Prepare data ------------------------------------------------------------

dat_study1_t_glances_rates1st_pxx_dti_rnd1_long <- 
  dat_study1_t_glances_rates1st_pxx_dti_rnd1 %>% 
  gather(variable, 
         value, 
         -one_of("passing", "pxx", "round_txt", "pxx_dti_m_rnd1"))
  


# Load visibility data ----------------------------------------------------

dat_visibility_perc_smoothed <- 
  dbGetSrc(dbFindConnObj("Study-1"), 
           "t_visibility_perc_smoothed") %>% 
  mutate(pxx = position_id)

dat_visibility_perc_smoothed_long <- 
  dat_visibility_perc_smoothed %>% 
  select(-position_id) %>% 
  gather(variable, value, -one_of("pxx", "pxx_dti_m_rnd1"))


# Viz ---------------------------------------------------------------------


ggplot() + 
  # geom_area(data = dat_visibility_perc_smoothed_long,
  #           aes_string(x = sett_dat$col_name_am,
  #                      y = "value",
  #                      fill = "variable"),
  #           alpha = 0.5,
  #           position = "identity") +
  # geom_line(data = dat_visibility_perc_smoothed_long,
  #           aes_string(x = sett_dat$col_name_am,
  #                      y = "value",
  #                      color = "variable"),
  #           alpha = 0.7,
  #           size = 0.1) +
  geom_line(data = dat_study1_t_glances_rates1st_pxx_dti_rnd1_long %>% 
              filter(pxx != 3) %>% 
              filter(variable %in% 
                       c("act_nr_PER_act_level_1st_ON_cases_n__4",
                         "act_nr_PER_act_level_1st_ON_cases_n__6")),
            aes_string(x = sett_dat$col_name_am,
                       y = "value",
                       color = "variable",
                       linetype = "round_txt")) + 
  #facet_grid(.~round_txt)
  facet_grid(pxx~.) +
  guides(color = F, fill = F, linetype = F)
