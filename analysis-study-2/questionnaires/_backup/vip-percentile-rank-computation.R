test <- data.frame(score = sort(dat_vip_scores$vip_us))
test$fx <- 1; 
test %>% 
  group_by(score) %>% 
  summarize(n = n()) %>% 
  ungroup() %>% 
  mutate(n_cum = cumsum(n)) %>%  
  mutate(pr = 100 * (n_cum - n/2)/sum(n)) %>% 
  data.frame()
