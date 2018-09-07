
# Settings ----------------------------------------------------------------

sett <- list()
sett$db$db_names$study2 <- "URBAN-MV-VIE_UniBw_Study-2"
sett$db$conn_names$study2 <- dbFindConnObj(sett$db$db_names$study2, output = F)
sett$db$src_names$demo <- "v_q_demography"
sett$db$src_names$exp <- "v_q_driving_experience"
sett$db$src_names$tloc <- "t_q_tloc_scores"  
sett$db$src_names$bsss <- "t_q_bsss_scores" 
sett$db$src_names$ds <- "t_q_ds_scores" 
sett$db$src_names$bfi10 <- "t_q_bfi10_scores" 
sett$db$src_names$vip <- "v_q_vip_scores"



# Query data --------------------------------------------------------------

dat_demo <- dbGetSrc(sett$db$conn_names$study2, sett$db$src_names$demo)
dat_exp <- dbGetSrc(sett$db$conn_names$study2, sett$db$src_names$exp)
dat_tloc <- dbGetSrc(sett$db$conn_names$study2, sett$db$src_names$tloc)
dat_bsss <- dbGetSrc(sett$db$conn_names$study2, sett$db$src_names$bsss)
dat_ds <- dbGetSrc(sett$db$conn_names$study2, sett$db$src_names$ds)
dat_bfi10 <- dbGetSrc(sett$db$conn_names$study2, sett$db$src_names$bfi10)
dat_vip <- dbGetSrc(sett$db$conn_names$study2, sett$db$src_names$vip)



# Preprocess data ---------------------------------------------------------

dat_exp <- 
  dat_exp %>% 
  mutate(driving_licence_years = 2015 - driving_licence_year) %>% 
  mutate(driving_licence_years = 
           ifelse(subject_id == 19,
                  2015 - (2015-driving_licence_year),
                  driving_licence_years)) %>% 
  mutate(driving_frequency_per_week = 
           factor(driving_frequency_per_week,
                  labels = c(1, 3, 5, 7))) %>% 
  mutate(driving_frequency_per_week = 
           as.numeric(
             as.character(driving_frequency_per_week)
             )) %>% 
  select(subject_id,
         driving_licence_years,
         regular_driving_years,
         km_overall,
         km_recent_year,
         driving_frequency_per_week)
  
dat_tloc <-
  dat_tloc %>% 
  select(subject_id,
         tloc_f,
         tloc_od,
         tloc_s,
         tloc_ve)

dat_vip <- 
  dat_vip %>% 
  select(subject_id,
         vip_ai_pr,
         vip_ed_pr,
         vip_sd_pr,
         vip_us_pr)

dat_all <- left_join(dat_exp, dat_tloc)
dat_all <- left_join(dat_all, dat_bsss)
dat_all <- left_join(dat_all, dat_ds)
dat_all <- left_join(dat_all, dat_bfi10)
dat_all <- left_join(dat_all, dat_vip)
# dat_all <- 
#   apply(dat_all, 2, function(x) {
#     if (is.character(x)) {
#       x = as.numeric(as.factor(x))
#     }
#   }) %>% 
#   data.frame()


# Correlation visualization -----------------------------------------------

library(psych)
## Visually (only works for selection of variables)
pairs.panels(
  dat_all[, 2:ncol(dat_all)],
  method = "pearson",
  hist.col = "#00AFBB",
  density = TRUE,
  ellipses = TRUE,
  stars = TRUE
)



# Correlation matrix ------------------------------------------------------

## Compute correlation matrix
results <- corr.test2(dat_all[,2:ncol(dat_all)])

results_df <- results$ci
results_df$pair_name <- rownames(results_df)
results_df <- 
  results_df %>%
  tidyr::separate(pair_name, c("var_name1", "var_name2"), sep = "-", remove = FALSE) %>% 
  select(var_name1, var_name2, r, p, lower, upper, pair_name)
results_df$r <- round(results_df$r, 2)
results_df$p<- round(results_df$p, 3)
results_df$lower <- round(results_df$lower, 2)
results_df$upper <- round(results_df$upper, 2)

dat_r_bfi10_bsss <-
  results_df %>% 
  filter(grepl("bfi10", pair_name)) %>% 
  filter(grepl("bsss", pair_name)) %>% 
  arrange(var_name2) %>% 
  select(BFI10 = var_name2, BSSS = var_name1, r, p, lower, upper)

write.table(test, "clipboard", sep="\t", row.names=FALSE, quote = FALSE)

dat_r_bfi10_vip <-
  results_df %>% 
  filter(grepl("bfi10", pair_name)) %>% 
  filter(grepl("vip", pair_name)) %>% 
  arrange(var_name1) %>% 
  select("BFI-10" = var_name1, VIP = var_name2, r, p, lower, upper)

write.table(dat_r_bfi10_vip, "clipboard", sep="\t", row.names=FALSE, quote = FALSE)


dat_r_bfi10_tloc <-
  results_df %>% 
  filter(grepl("bfi10", pair_name)) %>% 
  filter(grepl("tloc", pair_name)) %>% 
  arrange(var_name2) %>% 
  select("BFI-10" = var_name2, "T-LOC" = var_name1, r, p, lower, upper)

write.table(dat_r_bfi10_tloc, "clipboard", sep="\t", row.names=FALSE, quote = FALSE)

dat_r_bfi10_ds <-
  results_df %>% 
  filter(grepl("bfi10", pair_name)) %>% 
  filter(grepl("ds", pair_name)) %>% 
  arrange(var_name2) %>% 
  select(BFI10 = var_name2, DS = var_name1, r, p, lower, upper) %>% 
  mutate(DS = substr(DS, 4, 5)) %>% 
  mutate(color = ifelse(r > 0, "green3", "red")) %>% 
  mutate(color_p = ifelse(p < 0.05, "blue", "black"))

write.table(dat_r_bfi10_ds, "clipboard", sep="\t", row.names=FALSE, quote = FALSE)

ggplot() + 
  geom_bar(data = dat_r_bfi10_ds,
           aes(x = DS,
               y = abs(r),
               fill = color),
           stat = "identity",
           fill = dat_r_bfi10_ds$color) + 
  geom_text(data = dat_r_bfi10_ds,
            aes(x = DS,
                y = -0.22,
                #y = abs(r) + 0.01, 
                label = paste0("r = ", r),
                color = color_p),
            vjust = 0.25,
            hjust = 0,
            size = 3,
            color = dat_r_bfi10_ds$color_p) + 
  geom_text(data = dat_r_bfi10_ds,
            aes(x = DS,
                y = -0.13,
                #y = abs(r) + 0.01, 
                label = paste0(", p = ", p),
                color = color_p),
            vjust = 0.25,
            hjust = 0,
            size = 3,
            color = dat_r_bfi10_ds$color_p) + 
  coord_flip(ylim = c(-0.2, 1)) + 
  facet_grid(BFI10~.)

ggplot() + 
  # geom_bar(data = dat_r_bfi10_ds,
  #          aes(x = DS,
  #              y = p),
  #          stat = "identity") + 
  geom_errorbar(data = dat_r_bfi10_ds,
                aes(x = DS, 
                    ymin = p - lower, 
                    ymax = p + upper)) +
  coord_flip() +
  facet_grid(.~BFI10)



# ## Extract correlation coefficients and p-values
# results_r <- round(results$r, 2)
# results_p <- round(results$p, 3)
# results_ci <- round(results$ci)
# results_ci <- 
#   results_ci %>% 
#   mutate(var1 = names(dat_all[, 2:(ncol(dat_all)-1)]),
#          var2 = names(dat_all[, 3:ncol(dat_all)])) %>% 
#   select(var1, var2, r, p, lower, upper)
# 
# ## Extract upper triangular part of correlation coefficient matrix
# results_r_upper <- results_r[upper.tri(results_r)]
# 
# ## Extract lower triangular part of the matrix for non-adjusted p-values
# ## Step 1: Mirror lower half to upper triangular part
# ## Step 2: Extract upper triangular part of matrix
# ## The mirroring was done to keep control over variable names
# results_p_lower <- Matrix::forceSymmetric(results_p, uplo="L")
# results_p_lower <- results_p_lower[upper.tri(results_p_lower)]
# 
# ## Extract upper triangular part of the matrix for adjusted p-values
# results_p_upper <- results_p[upper.tri(results_p)]
# 
# ## Create flat dataframe
# results_df <- 
#   data.frame(
#     r = results_r_upper,
#     p = results_p_lower,
#     p_adj = results_p_upper)
# 
# ## Add variable information
# ## Step 1: Extract unique variable names
# ## Step 2: Build list of row names by order of their appearance
# ## Step 3: Build list of column names by order of their appearance
# names <- colnames(results$r)
# names_n <- length(names)
# row_names <- lapply(seq(1, names_n-1), function(x) { names[1:x] })
# row_names <- unlist(row_names)
# col_names <- lapply(seq(2, names_n), function(x) { rep(names[x], x-1) })
# col_names <- unlist(col_names)
# results_df$var1 <- row_names
# results_df$var2 <- col_names
# 
# ## Print pairs with signficant p-values
# test2 <- results_df %>% 
#   filter(p <= .05) %>% 
#   ## Remove pairs coming from same overall measure
#   #mutate(temp = ifelse(grepl("style", var1) & grepl("style", var2), 1, 0)) %>% 
#   #filter(temp != 1) %>% 
#   arrange(var1)
# 
# ## Print pairs with signficant adjusted p-values
# results_df %>% 
#   filter(p_adj <= .05) %>% 
#   arrange(var1)


# ECDF --------------------------------------------------------------------

mu <- mean(dat_exp$km_overall)
sigma <- sd(dat_exp$km_overall)
test <- computeECDF(dat_exp$km_overall)
plot(test, xlim = c(0,15e+5))
test2 <- computeECDF(rnorm(100000, mu, sigma))
lines(test2, col = "red")
test3 <- computeECDF(rexp(10000, 1/mu))
lines(test3, col = "blue")

