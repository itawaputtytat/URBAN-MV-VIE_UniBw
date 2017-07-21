outputSectionTitle("Pre-loading data ...")

## Initialise t_sxx_critdist
if (set4db$input == 1) {
  outputString("* Loading t_sxx_critdist from database ...")
  t_sxx_critdist <- dbGetSrc("dbconn_study1", "t_sxx_critdist")
}
outputDone(step = T)