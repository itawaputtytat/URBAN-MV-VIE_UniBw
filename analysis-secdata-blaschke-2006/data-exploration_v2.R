library(R.matlab)
dat <- readMat(file.choose())
dat_df <- data.frame(matrix(unlist(dat), 
 nrow=132, byrow=T),
stringsAsFactors=FALSE)

col_names <- c(
"CAN1_ACC_S1_Mul_Mode",
"CAN1_ACC_S1_TO_vr",
"CAN1_ACC_S1_TO_dyv",
"CAN1_ACC_S1_TO_dx",
"CAN1_ACC_S5_axRef",
"CAN1_Bremse_1_Aktiver_BKV",
"CAN1_Bremse_1_Geschwindigkeit_15Bit",
"CAN1_Bremse_1_Bremslichtschalter_ESP",
"CAN1_Bremse_2_Mux_Code_Bremse2",
"CAN1_Bremse_2_Querbeschleunigung",
"CAN1_Bremse_5_Bremsdruck",
"CAN1_Bremse_5_Signum_Giergeschwindigkeit",
"CAN1_Bremse_5_Giergeschwindigkeit",
"CAN1_Gateway_Komfort_1_Blinken_rechts",
"CAN1_Gateway_Komfort_1_Blinken_links",
"CAN1_LDW_intern_inp_1_ldw_bvs_GierwiFehl_ii16",
"CAN1_LDW_intern_inp_1_ldw_bvs_GueteSpur_iu8",
"CAN1_LDW_intern_inp_1_ldw_bvs_Querabwei_ii16",
"CAN1_LDW_intern_inp_1_ldw_bvs_Spurbreite_iu16",
"CAN1_LDW_intern_inp_3_ldw_bvs_PraedSpur_ib",
"CAN1_LDW_intern_inp_3_ldw_bvs_SpurmarkL_ib",
"CAN1_LDW_intern_inp_3_ldw_bvs_SpurmarkR_ib",
"CAN1_LDW_intern_inp_3_ldw_bvs_Kruemmung_ii16",
"CAN1_Lenkung_1_Winkeldifferenz",
"CAN1_LWS_1_Lenkradwinkelgeschw_Sign",
"CAN1_LWS_1_Lenkradwinkelgeschwindigkeit",
"CAN1_LWS_1_Lenkradwinkel_Sign",
"CAN1_LWS_1_Lenkradwinkel",
"CAN1_Motor_1_Fahrpedalwert_DK_Poti",
"CAN1_PSD_Mux_Code_PSD",
"CAN1_PSD_Segmentlaenge_2",
"CAN1_PSD_Segmentlaenge_1",
"CAN1_PSD_Segmentlaenge_4",
"CAN1_PSD_Segmentlaenge_3",
"CAN1_PSD_Kreuzung",
"CAN1_PSD_Segmentlaenge_6",
"CAN1_PSD_Segmentlaenge_5"
)

template <- data.frame(time_s = seq(0, 1181.416, 0.01))
for(i in names(dat)) {
  dat_temp <- data.frame(dat[[i]])
  names(dat_temp) <- paste_(i, c(1:ncol(dat_temp)))
  names(dat_temp)[ncol(dat_temp)] <- "time_s"
  print(names(dat_temp))
  template <-
    left_join(template,
              dat_temp)
}

library(ggplot2)

ggplot() + 
  geom_line(data = data.frame(dat$GROUP4),
            aes(x = dat$GROUP4[, 3],
                y = dat$GROUP4[, 2])) + 
  geom_vline(data = data.frame(dat$Environment.Marker),
             aes(xintercept = dat$Environment.Marker[, 1]))
