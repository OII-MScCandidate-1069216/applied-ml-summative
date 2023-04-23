# Author: MSc candidate 
# Maintainers: MSc candidate 
# ===========================================================
# 


# load package
require(Amelia)
require(here)
require(beepr)

# set.seed
set.seed(2018)

# --------------------------------- #
# load data to be imputed
# --------------------------------- #
load('applied-ml-summative/import/output/new_d3.RData')


# --------------------------------- #
# carry out imputation
# check arguments!!
# --------------------------------- #
a.out.more <- amelia(new_d3, m = 1, idvars = "challengeID", empri = 01*nrow(new_d3),
                     parallel = "multicore", ncpus = 7)

beep(sound = 8, expr = NULL)


### store df 
imputed <- a.out.more$imputations$imp1

# --------------------------------- #
# save outcomes
# --------------------------------- #
readr::write_rds(imputed, here("impute/output/imputed.rds"))


# Done