dat_q <- 
  data.frame(dat_bfi$subid,
             dat_bfi$bfi_e.z,
             dat_bfi$bfi_n.z,
             dat_bfi$bfi_o.z,
             dat_bfi$bfi_c.z,
             dat_bfi$bfi_a.z,
             dat_bsss$bsss_e.z,
             dat_bsss$bsss_b.z,
             dat_bsss$bsss_t.z,
             dat_bsss$bsss_d.z,
             dat_tloc$tloc_os.z,
             dat_tloc$tloc_ob.z,
             dat_tloc$tloc_od.z,
             dat_tloc$tloc_ve.z,
             dat_tloc$tloc_f.z)

library(psych)
pairs.panels(dat_q[, 2:ncol(dat_q)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)
