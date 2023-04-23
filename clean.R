# Author: MSc candidate 
# Maintainers: MSc candidate 
# ===========================================================
# 

## packages
require(readr)
require(dplyr)
require(tidyr)
require(glmnet)
require(here)

## ***************************************************** ##
## load data
## 1. background dataset (covariates)
## 2. training dataset (outcomes )
## ***************************************************** ##
## background
bg <- read_csv('applied-ml-summative/import/input/background.csv')

## training
train <- read_csv('applied-ml-summative/import/input/train.csv')


# set.seed
set.seed(2018)

## use all the background data for pruning and imputing
bg_train   <- bg

# as.data.frame the training outcomes and all the right hand side variables
train    <- train %>% as.data.frame()
bg_train <- bg_train %>% as.data.frame()

## save var names for figures 
# write.csv(colnames(bg), 'figures/bg_varnames.csv')

## ***************************************************** ##
## First stage of variable screening 
##
## Delete any variable for which 
##  more than 60 percent is missing (NA)
## ***************************************************** ##

# delete any variable for which more than 60 percent is missing (NA)
v_names <- vector()
for (i in 2:length(bg_train)) {
  if (sum(is.na(bg_train[, i])) < (nrow(bg_train)*.6)) {
    v_names[i] <- colnames(bg_train)[i] 
  }
}

v_names[1] <- "challengeID"
bg_train_screen_tmp <- bg_train[na.omit(v_names)]

# as.numeric
bg_train_screen_tmp[1:(length(bg_train_screen_tmp))] <- lapply(bg_train_screen_tmp[1:(length(bg_train_screen_tmp))], 
                                                               function(x) as.numeric(as.character(x))) # as.numeric



### delete any variable with more than 60% missing
# by being smaller than 0
v_names <- vector()
for (i in 2:length(bg_train_screen_tmp)) {
  if (sum(bg_train_screen_tmp[, i] < 0, na.rm = T) < (nrow(bg_train_screen_tmp)*.6)) {
    v_names[i] <- colnames(bg_train_screen_tmp)[i] 
  }
}

v_names[1] <- "challengeID"
bg_train_screen_tmp <- bg_train_screen_tmp[na.omit(v_names)]



## ***************************************************** ##
## Clean the data 
##
## 1. We replace all negative values to be NA
## 2. Further eliminate vars with NA 
## 3. Remove variables with small sd (<.01).
## ***************************************************** ##

new_d <- bg_train_screen_tmp

# 1.
# Now, for the variables that remain, we are going to make all the negative
# values NA. THis is for Amelia implementation 
new_d <- apply(new_d, 2, function(x) x <- ifelse(x < 0, NA, x))
new_d <- as.data.frame(new_d) # as.data.frame


# 2. 
# further screening out NAs as there are variables that made it through our previous
# two stages. This is possibly because certain character variables became NAs
# when running line 45 (as.numeric)
v_names <- vector()
for (i in 2:length(new_d)) {
  if (sum(is.na(new_d[, i])) < (nrow(new_d)*.6)) {
    v_names[i] <- colnames(new_d)[i] 
  }
}

v_names[1] <- "challengeID"
new_d2<- new_d[na.omit(v_names)]
#save(new_d2, file = "tables/new_d2.RData")

# 3.
# delete variables that don't vary (this is for Amelia)
sd_id <- apply(new_d2, 2, function(x) sd(x, na.rm = TRUE) < .01)
new_d3 <- new_d2[!sd_id]


table_new_d3 <- new_d2[!sd_id] # for n table only
#save(table_new_d3, file = "tables/table_new_d3.RData")

# check 
check_na_id <- apply(new_d3, 2, function (x) sum(is.na(x)) > .6*nrow(new_d3))
TRUE %in% check_na_id

## ***************************************************** ##
## run lasso to futher reduce the number of variables
## -- 1) run lasso for each outcome 
## -- 2) pick vars that are selected more than two times
## ***************************************************** ##

new_d3_tmp <- new_d3

# construct training set to run lasso
train_outs   <- colnames(train[,-1])
train_pred   <- colnames(new_d3_tmp[,-1])
new_d3_train <- left_join(train, new_d3_tmp, by = 'challengeID') %>% as.data.frame()

# mean imputation
new_d3_train <- apply(new_d3_train, 2, function(x){
  miss_idx    <- is.na(x)
  mean_x      <- mean(x, na.rm = TRUE)
  x[miss_idx] <- mean_x
  return(x)
})

new_d3_train <- as.data.frame(new_d3_train)

# run lasso for each outcome
lasso_out <- list()
for(j in 1:length(train_outs)){
  cat('now ', j, '\n')
  var_names     <- c(train_outs[j], train_pred)
  new_d3_LASSO  <- new_d3_train %>% select(one_of(var_names)) %>% na.omit()
  new_d3_X      <- new_d3_LASSO %>% select(one_of(train_pred)) %>% as.matrix()
  new_d3_Y      <- new_d3_LASSO %>% select_(train_outs[j]) %>% as.matrix()
  
  # glmnet
  cv_lasso       <- cv.glmnet(y = new_d3_Y[,1], x = new_d3_X, 
                              family = 'gaussian', alpha = 1, nfolds = 5)
  lasso_out[[j]] <- coef(cv_lasso, s = "lambda.min")
}


# variable selection
var_list <- var_select <- list()
for(j in 1:length(train_outs)){
  var_list[[j]]   <- which(abs(as.matrix(lasso_out[[j]])) > 0)
  var_select[[j]] <- lasso_out[[j]]@Dimnames[[1]][var_list[[j]]]
}

# join the selected vars and substract "intercept index"
var_list <- unique(unlist(var_select))[-1]

# select cols
var_listF <- c('challengeID', var_list)
new_d3 <- new_d3[,var_listF]

## ***************************************************** ##
## save
## this output will be imputed 
## see Run_Amelia.R
## ***************************************************** ##
save(new_d3, file = "applied-ml-summative/import/output/new_d3.RData")


# Done
