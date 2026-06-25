#require("fields")
library(maps)

# =========================================================
# Compile C++ only once
# =========================================================
if (!exists("VISIToRN", mode = "function")) {
  source("VISIToRN_func.R")
}

# =========================================================
# Required external inputs
# =========================================================
if (!exists("visitnl_object_name")) {
  stop("visitnl_object_name is not provided.")
}
if (!exists("humanN_year")) {
  stop("No humanN_year")
}

if (!exists("dat_dir")) {
  stop("No dat_dir")
}

if (!exists("ParFit_input")) {
  ParFit_input <- c(982.2591, 12.22721, 2.961817) 
}
if (length(ParFit_input) != 3 || any(!is.finite(ParFit_input))) {
  stop("ParFit_input needs to be a numeric vector of length 3 with finite values.")
}

visitnl_input <- get(visitnl_object_name, inherits = TRUE)
humanN_input  <- HumanN_future[["SSP2"]][[as.character(humanN_year)]]

ParFit_use <- as.numeric(ParFit_input)

#### Prepare init  value ####

Init_0 <- matrix(0, nrow = 720, ncol = 360)

# For getting initial Sriv and SrivN

test_init <-  VISIToRN(dim3 = c(720, 360, 361),
                         dat_dir = dat_dir,
                         Land_mask = LandMask_Loop,
                         dat_dis = DistMat,
                         Sriv = Init_0,
                         SrivN = Init_0,
                         VISITro = VISITro_nat_ag,
                         VISITnl = visitnl_input,
                         RivArea = RivArea,
                         dat_humanN = humanN_input,
                         dat_tas = dat_tas, 
                         ParFit = ParFit_use
                         )

#### Simulation ####

test_sim <-  VISIToRN(dim3 = c(720, 360, 361),
                       dat_dir = dat_dir,
                       Land_mask = LandMask_Loop,
                       dat_dis = DistMat,
                       Sriv = test_init$Sriv[,,360],
                       SrivN = test_init$SrivN[,,360],
                       VISITro = VISITro_nat_ag,
                       VISITnl = visitnl_input, #leach input
                       RivArea = RivArea,
                       dat_humanN = humanN_input,
                       dat_tas = dat_tas,
                       ParFit = ParFit_use
                     )  


