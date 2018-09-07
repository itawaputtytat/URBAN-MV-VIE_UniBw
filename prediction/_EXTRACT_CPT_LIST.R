test <- bn$cptlist$O
test_df <- adply(test, c(1,2,3,4))
test_df <- 
  test_df %>% 
  arrange(S, A, I) %>% 
  unite(unite_col, S, A, I)


test_df_spread <- spread(test_df, unite_col, V1)
test_df_spread
write.table(test_df_spread[,-1], "clipboard", row.names = FALSE)

write.table(test_df_spread[,-1], "clipboard", row.names = FALSE, col.names = FALSE)




test <- bn$cptlist$A
test_df <- adply(test, c(1,2,3))

test_df <- 
  test_df %>% 
  arrange(A, S, stress) %>% 
  unite(unite_col, stress, S)

names <- unique(test_df$unite_col)

test_df_spread <- spread(test_df, unite_col, V1)
test_df_spread

test_df_spread <- test_df_spread[c("A", names)]

write.table(test_df_spread[,-1], "clipboard", row.names = FALSE)
write.table(test_df_spread[,-1], "clipboard", row.names = FALSE, col.names = FALSE)

