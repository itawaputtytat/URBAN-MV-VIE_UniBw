outputString("* Declare \"select\" as function of dplyr: dplyr::select")
select <- dplyr::select

# outputString("* Force R to use non-exponential notations (scipen)")
# options("scipen" = 100, "digits" = 4)

outputString("* Deactivate outputFunProc")
.outputFunProc_status(F, print = T)
#.outputFunProc_status(T, print = T)
