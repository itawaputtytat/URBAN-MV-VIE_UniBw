
# Settings ----------------------------------------------------------------

sett <- list()
sett$db$db_names$study2 <- "URBAN-MV-VIE_UniBw_Study-2"
sett$db$conn_names$study2 <- dbFindConnObj(sett$db$db_names$study2, output = F)
sett$db$src_names$demo <- "t_q_demography_screening"
sett$db$src_names$drex <- "t_q_driving_experience_screening"
sett$db$src_names$drst <- "t_q_driving_style_screening" 
sett$db$src_names$vip <- "t_q_vip_screening_scores"



# Query data --------------------------------------------------------------

dat_demo <- dbGetSrc(sett$db$conn_names$study2, sett$db$src_names$demo)
dat_drex <- dbGetSrc(sett$db$conn_names$study2, sett$db$src_names$drex)
dat_drst <- dbGetSrc(sett$db$conn_names$study2, sett$db$src_names$drst)
dat_vip <- dbGetSrc(sett$db$conn_names$study2, sett$db$src_names$vip)



# Preprocess data ---------------------------------------------------------

dat_drex <- 
  dat_drex %>% 
  mutate(driving_licence_years = 2015 - driving_licence_year) %>% 
  mutate(driving_licence_years = 
           ifelse(screening_subject_id %in% c(180, 186, 279),
                  2015 - (2015-driving_licence_year),
                  driving_licence_years)) %>% 
  mutate(driving_frequency_per_week_fct = 
           factor(driving_frequency_per_week,
                  labels = c(1, 3, 5, 7, 0))) %>% 
  mutate(driving_frequency_per_week_fct = 
           as.numeric(
             as.character(driving_frequency_per_week_fct)
             )) %>% 
  select(screening_subject_id,
         driving_licence_years,
         regular_driving_years,
         km_overall,
         km_recent_year,
         #driving_frequency_per_week,
         driving_frequency_per_week_fct)
  
dat_vip <- 
  dat_vip %>% 
  select(screening_subject_id,
         vip_ai_pr,
         vip_ed_pr,
         vip_sd_pr,
         vip_us_pr)

dat_all <- dat_drex
dat_all <- left_join(dat_all, dat_vip)
dat_all <- left_join(dat_all, dat_drst)

# dat_all <- 
#   apply(dat_all, 2, function(x) {
#     if (is.character(x)) {
#       x = as.numeric(as.factor(x))
#     }
#   }) %>% 
#   data.frame()



# Correlation visualization -----------------------------------------------

library(psych)
# Visually (only works for selection of variables)
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
results <- corr.test(dat_all[,2:ncol(dat_all)])

## Extract correlation coefficients and p-values
results_r <- round(results$r, 2)
results_p <- round(results$p, 3)

## Extract upper triangular part of correlation coefficient matrix
results_r_upper <- results_r[upper.tri(results_r)]

## Extract lower triangular part of the matrix for non-adjusted p-values
## Step 1: Mirror lower half to upper triangular part
## Step 2: Extract upper triangular part of matrix
## The mirroring was done to keep control over variable names
results_p_lower <- Matrix::forceSymmetric(results_p, uplo="L")
results_p_lower <- results_p_lower[upper.tri(results_p_lower)]

## Extract upper triangular part of the matrix for adjusted p-values
results_p_upper <- results_p[upper.tri(results_p)]

## Create flat dataframe
results_df <- 
  data.frame(
    r = results_r_upper,
    p = results_p_lower,
    p_adj = results_p_upper)

## Add variable information
## Step 1: Extract unique variable names
## Step 2: Build list of row names by order of their appearance
## Step 3: Build list of column names by order of their appearance
names <- colnames(results$r)
names_n <- length(names)
row_names <- lapply(seq(1, names_n-1), function(x) { names[1:x] })
row_names <- unlist(row_names)
col_names <- lapply(seq(2, names_n), function(x) { rep(names[x], x-1) })
col_names <- unlist(col_names)
results_df$var1 <- row_names
results_df$var2 <- col_names

## Print pairs
results_df %>% 
  arrange(var1)

## Print pairs with signficant p-values
results_df %>% 
  filter(p <= .05) %>% 
  ## Remove pairs coming from same overall measure
  #mutate(temp = ifelse(grepl("style", var1) & grepl("style", var2), 1, 0)) %>% 
  #filter(temp != 1) %>% 
  arrange(var2)

## Print pairs with signficant adjusted p-values
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

