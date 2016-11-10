tfinder <- dbListTables(dbconn_secdata_blaschke)
tfinder <- tfinder[grep("abb", tfinder)]

dat <- lapply(tfinder, function(x) {
  dat2proc <- dbGetSrc("dbconn_secdata_blaschke", x)
  dat2proc <- data.frame(x, dat2proc)
}) %>% bind_rows()

dat <- 
  dat %>% 
  group_by(x) %>% 
  mutate(Index = row_number()) %>% 
  mutate(cond_speed = ifelse(grepl("abb80", x), "80", "100")) %>% 
  mutate(id = substr(x, 3, 4)) %>% 
  data.frame()

str(dat)
ggplot() + 
  geom_line(data = dat,
            aes(x = Index,
                y = Geschwindigkeit_15Bit,
                group = x)) +
  facet_grid(.~cond_speed)

dat %>% 
  group_by(id) %>% 
  summarise(n()) %>% plot(type = "l")





library(R.matlab)
test <- readMat(file.choose())
test2 <- as.data.frame(test$GROUP12)

plot(test2$V17, type = "l")
