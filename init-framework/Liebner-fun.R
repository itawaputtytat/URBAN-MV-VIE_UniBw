sourceWithEcho("fun_Liebner_2013/predLiebner_computeIDM.R")

sourceWithEcho("fun_Liebner_2013/idm_createSimDat.R")
sourceWithEcho("fun_Liebner_2013/idmGap.R")
sourceWithEcho("fun_Liebner_2013/idmGap_act.R")
sourceWithEcho("fun_Liebner_2013/idmGap_des.R")
sourceWithEcho("fun_Liebner_2013/idmSpeed.R")
sourceWithEcho("fun_Liebner_2013/idmDistance.R")
sourceWithEcho("fun_Liebner_2013/idmAcc.R")

sourceWithEcho("fun_Liebner_2013/predLiebner_setMaxIDM.R")

ptm <- proc.time()
sourceWithEcho("fun_Liebner_2013/predLiebner_compProb_Mk.R")
#list.files(list.dirs(), "predLiebner_compProb_Mk.R", recursive = T)
#source("fun_Liebner_2013/predLiebner_compProb_Mk.R")
outputProcTime(ptm)
sourceWithEcho("fun_Liebner_2013/predLiebner_compProb_al_Mk.R")
sourceWithEcho("fun_Liebner_2013/predLiebner_compProb_O_Hi.R")

sourceWithEcho("fun_Liebner_2013/predLiebner_initObj4Set.R")
sourceWithEcho("fun_Liebner_2013/predLiebner_initBN_Liebner.R")
sourceWithEcho("fun_Liebner_2013/predLiebner_initBN_A_ST.R")
sourceWithEcho("fun_Liebner_2013/predLiebner_initBN_A_S_ST.R")
sourceWithEcho("fun_Liebner_2013/predLiebner_initBN_A_S_DS_ST.R")

sourceWithEcho("fun_Liebner_2013/predLiebner_updateBN.R")

sourceWithEcho("fun_Liebner_2013/predLiebner_findSimStartValues.R")
sourceWithEcho("fun_Liebner_2013/predLiebner_modelDrivBehav.R")
sourceWithEcho("fun_Liebner_2013/predLiebner_modelDrivBehav_batch.R")
sourceWithEcho("fun_Liebner_2013/predLiebner_modelDrivBehav_batch_cpp.R")
sourceWithEcho("fun_Liebner_2013/predLiebner_getSimTails.R")
sourceWithEcho("fun_Liebner_2013/predLiebner_getu.R")
sourceWithEcho("fun_Liebner_2013/predLiebner_pdf4sim.R")
sourceWithEcho("fun_Liebner_2013/predLiebner_pdf4comp.R")

# sourceWithEcho("fun_Liebner_2013/predLiebner_visDVM.R")
# sourceWithEcho("fun_Liebner_2013/predLiebner_visPassing.R")
# sourceWithEcho("fun_Liebner_2013/predLiebner_visPos.R")
# sourceWithEcho("fun_Liebner_2013/predLiebner_visProf.R")

## Init prediction framework
sourceWithEcho("fun_Liebner_2013/predLiebner_initDSM.R")
sourceWithEcho("fun_Liebner_2013/settings/sett_bn_Liebner.R") # predLiebner_initSettingsForBN
sourceWithEcho("fun_Liebner_2013/settings/sett_bn_A_ST.R")
sourceWithEcho("fun_Liebner_2013/settings/sett_bn_A_S_ST.R")
sourceWithEcho("fun_Liebner_2013/settings/sett_bn_A_S_DS_ST.R")

sourceWithEcho("fun_Liebner_2013/getCPTForGenie.R")
sourceWithEcho("fun_Liebner_2013/deriveOrderofHypotheses.R")


## DSM
sourceWithEcho("fun_Liebner_2013/computePathRadius.R")
sourceWithEcho("fun_Liebner_2013/computePathCurvature.R")
sourceWithEcho("fun_Liebner_2013/computeDSMFromPathCurvature.R")