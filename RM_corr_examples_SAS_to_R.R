
#####################
## Load libraries. ##
#####################
# ----
pacman::p_load(
  nlme
  ,Matrix
  ,tidyverse
)
# ----

###############
## The data. ##
###############
#
# The data is from Boyd et al. (1993) but was first used for the purpose
# of illustrating correlation in the presence of repeated measures in Bland
# and Altman (1995). Below, I present the Bland and Altman layout.
#
# Sources:
# 1. Boyd et al. (1993) doi: 10.1016/0140-6736(93)90005-2
# 2. Bland and Altman (1995) Part 1, doi: 10.1136/bmj.310.6980.633
# ----
phPACO2 <-
  data.frame(
    ID =
      c(
        1, 1, 1, 1
        ,2, 2, 2, 2
        ,3, 3, 3, 3, 3, 3, 3, 3, 3
        ,4, 4, 4, 4, 4
        ,5, 5, 5, 5, 5, 5, 5, 5
        ,6, 6, 6, 6, 6, 6
        ,7 , 7, 7
        ,8, 8, 8, 8, 8, 8, 8, 8
      )
    ,visit = 
      c(
        1:4
        ,1:4
        ,1:9
        ,1:5
        ,1:8
        ,1:6
        ,1:3
        ,1:8
      )
    ,pH =
      c(
        6.68, 6.53, 6.43, 6.33
        ,6.85, 7.06, 7.13, 7.17
        ,7.40, 7.42, 7.41, 7.37, 7.34, 7.35, 7.28, 7.30, 7.34
        ,7.36, 7.33, 7.29, 7.30, 7.35
        ,7.35, 7.30, 7.30, 7.37, 7.27, 7.28, 7.32, 7.32
        ,7.38, 7.30, 7.29, 7.33, 7.31, 7.33
        ,6.86, 6.94, 6.92
        ,7.19, 7.29, 7.21, 7.25, 7.20, 7.19, 6.77, 6.82
      )
    ,P_aCO2 =
      c(
        3.97, 4.12, 4.09, 3.97
        ,5.27, 5.37, 5.41, 5.44
        ,5.67, 3.64, 4.32, 4.73, 4.96, 5.04, 5.22, 4.82, 5.07
        ,5.67, 5.10, 5.53, 4.75, 5.51
        ,4.28, 4.44, 4.32, 3.23, 4.46, 4.72, 4.75, 4.99
        ,4.78, 4.73, 5.12, 4.93, 5.03, 4.93
        ,6.85, 6.44, 6.52
        ,5.28, 4.56, 4.34, 4.32, 4.41, 3.69, 6.09, 5.58
      )
  )
#saveRDS(phPACO2, file = "phPACO2.rds")

phPACO2_long <-
  phPACO2 %>%
  pivot_longer(
    cols = c(pH, P_aCO2)
    ,names_to = "mvar"
    ,values_to = "Outcome"
  ) %>%
  mutate(
    mvar =
      factor(
        mvar
        ,levels = c("pH", "P_aCO2")
        ,labels = c("pH", "P_aCO2")
      )
    ,mvar_num = 0 + ( mvar == "pH" )
    ,ID = as.factor( ID )
    ,visit_num = visit
    ,visit = as.ordered( visit_num )
  ) 
# ----

#####################################
## Bland and Altman (1995) models. ##
#####################################
#
# Firstly, Bland and Altman's suggest to get the Pearson correlation of the group
# means. It ignores any dependency or repeated observations.
#
# As Roy (2006) says: "The Pearson correlation coefficient gives a rough
# estimate of the true overall correlation coefficient between the variables. It
# needs to be corrected by incorporating the subject effect, and by taking into
# consideration that the repeated measurements themselves are correlated."
#
# Bland and Altman (1995) calculate the correlation between means excluding
# data from various "subjects" who might be outliers.
#
# Sources:
# 1. Bland and Altman (1995) Part 2, doi: 10.1136/bmj.310.6980.633
# 2. Roy (2006) doi: 10.1002/bimj.200510192
# ----
bland_and_altman_Pearson_of_means <-
  phPACO2 %>%
  dplyr::group_by( ID ) %>%
  dplyr::summarise(
    mean_ph = mean( pH )
    ,mean_PACO2 = mean( P_aCO2 )
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select( - ID ) %>%
  cor() %>%
  purrr::pluck(2)
excl_1_bland_and_altman_Pearson_of_means <-
  phPACO2 %>%
  dplyr::filter( ID != 1 ) %>%
  dplyr::group_by( ID ) %>%
  dplyr::summarise(
    mean_ph = mean( pH )
    ,mean_PACO2 = mean( P_aCO2 )
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select( - ID ) %>%
  cor() %>%
  purrr::pluck(2)
excl_7_bland_and_altman_Pearson_of_means <-
  phPACO2 %>%
  dplyr::filter( ID != 7 ) %>%
  dplyr::group_by( ID ) %>%
  dplyr::summarise(
    mean_ph = mean( pH )
    ,mean_PACO2 = mean( P_aCO2 )
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select( - ID ) %>%
  cor() %>%
  purrr::pluck(2)
rbind(
  bland_and_altman_Pearson_of_means
  ,excl_1_bland_and_altman_Pearson_of_means
  ,excl_7_bland_and_altman_Pearson_of_means
)
# ----

##################################
## Hamlett et al. (2003) model. ##
##################################
#
# The SAS model specified by Hamlett et al. (2003) is as follows:
#   proc mixed method = ml;
#   class ID mvar visit;
#   model Outcome = mvar / solution ddfm = kr;
#   random mvar / type = un subject = ID;
#   repeated mvar / type = un subject = visit( ID );
#   run;
#
# Vital help from @PBulls on stackexchange to convert this SAS code to R code:
# https://stackoverflow.com/questions/79615507
#
# Sources:
# 1. Hamlett et al. (2003) doi: 10.1080/10473289.2003.10466174
# ----
model_hamlett <- nlme::lme(
  fixed = Outcome ~ mvar
  ,random = list( "ID" = pdSymm( form = ~1 + mvar_num ) )
  ,weights = varIdent( form = ~1 | mvar )
  ,correlation = corSymm(form = ~1 + mvar_num | ID/visit )
  ,method = "ML"
  ,data = phPACO2_long
)
summary( model_hamlett )
# ----

###############################################################
## Extract parameters and matrices from Hamlet et al. model. ##
###############################################################
# 
# The code below is an adaptation of the `nlme::getVarCov()` function to handle
# nested grouping.
#
# Sources:
# 1. https://github.com/cran/nlme/blob/master/R/VarCov.R
# 2. https://stats.stackexchange.com/questions/643382/mixed-effect-model-in-r-with-unstructured-covariance
# ----

# Define function to extract parameters.
get_parameters <- function( model, person_idx = 1)
{
  has_R_and_Varstr <- !is.null( model$modelStruct$corStruct ) && !is.null( model$modelStruct$varStruct )
  
  if( has_R_and_Varstr )
  {
    corStruct_matrix <- nlme::corMatrix( model$modelStruct$corStruct )[[ person_idx ]]
    if( length( corStruct_matrix ) == 1 )
    {
      for( i in 1:length( nlme::corMatrix( model$modelStruct$corStruct ) ) )
      {
        i_corStruct <- nlme::corMatrix( model$modelStruct$corStruct )[i]
        if( length( unlist( i_corStruct ) ) > 1 ) { corStruct_matrix <- i_corStruct[[1]]; break }
      }
      
    }
    varStruct_matrix <-
      stats::coef(
        model$modelStruct$varStruct
        ,uncons = FALSE
        ,allCoef = TRUE
      )
    R <-
      corStruct_matrix *
      model$sigma ^ 2 *
      Matrix::t( Matrix::t( varStruct_matrix ) ) %*%
      Matrix::t( varStruct_matrix )
  }
  # Get G matrix, the m-by-m random-effect covariance matrix.
  G <-
    model$modelStruct$reStruct[[ 1 ]] %>%
    as.matrix() %>%
    `*`( model$sigma^2 ) %>%
    matrix( ncol = 2 )
  
  # Get Z matrix, the n_i-by-m matrix of random-effect covariates.
  ind <- model$groups[[ 1 ]] == person_idx
  ni <- sum( ind, na.rm = TRUE )
  Z <- model.matrix( model$modelStruct$reStruct, getData( model ) )[ind, drop = FALSE]
  Z <- matrix( Z, ncol = 2 )
  # Calculate the general covariance matrix.
  if( has_R_and_Varstr )
  {
    CovMatrix <-
      Z %*% G %*% t( Z ) + Matrix::.bdiag( rep( list( R ), ni/2 ) ) %>%
      matrix( ncol = ni )
  } else {
    CovMatrix <-
      Z %*% G %*% t( Z ) %>%
      matrix( ncol = ni )
  }
  # Convert to correlation.
  CorrMatrix <- stats::cov2cor( CovMatrix )
  # Get the 2-by-2 covariance matrix between the variables.
  covar_between_vars <- CovMatrix[ 1:2, 1:2 ] %>% matrix( ncol = 2)
  # Tabulate parameter estimates.
  var_levels_names <- attr( model$modelStruct$varStruct, "groupNames" )
  df_params_ests <-
    data.frame(
      fixef(model)[[ 1 ]]
      ,sum( fixef(model) )
      ,CovMatrix[[ 1 ]]
      ,CovMatrix[[ 2, 2 ]]
      ,ifelse( nrow(CorrMatrix) > 2, CorrMatrix[ 1, 3 ], NA )
      ,ifelse( nrow(CorrMatrix) > 2, CorrMatrix[ 2, 4 ], NA )
      ,CorrMatrix[ 2 ]
      ,ifelse(
        has_R_and_Varstr
        ,1 - ( R[[ 2 ]] / CovMatrix[[ 2 ]] )
        ,NA
      )
    ) %>%
    round( 4 ) %>%
    `colnames<-`(
      c(
        paste0("mean_", var_levels_names[ 1 ] )
        ,paste0("mean_", var_levels_names[ 2 ] )
        ,paste0("var_", var_levels_names[ 1 ] )
        ,paste0("var_", var_levels_names[ 2 ] )
        ,paste0("corr_", var_levels_names[ 1 ] )
        ,paste0("corr_", var_levels_names[ 2 ] )
        ,paste0("corr_", var_levels_names[ 1 ], "_", var_levels_names[ 2 ] )
        ,"delta"
      )
    ) %>%
    t() %>%
    as.data.frame()
  
  # Add row and column names to the matrices.
  rcnames <- rep( var_levels_names, ni / 2 )
  colnames( CovMatrix ) <- rcnames
  rownames( CovMatrix ) <- rcnames
  colnames( CorrMatrix ) <- rcnames
  rownames( CorrMatrix ) <- rcnames
  
  # Return value.
  return(
    list(
      params_ests = df_params_ests
      ,G = G
      ,R = R
      ,Z_1 = Z
      ,CovMatrix = CovMatrix
      ,CorrMatrix = CorrMatrix
    )
    )
}

# Get the parameters for the Hamlet et al. (2006) model.
parameters_hamlett <- get_parameters( model_hamlett )
# ----

###############################################
## Roy (2006) and Shan et al. (2020) models. ##
###############################################
#
# The SAS models specified by Roy (2006) and Shan et al. (2020) are as follows:
#
#   proc mixed method = ml;
#   class ID mvar visit;
#   model Outcome = mvar / solution ddfm = kr;
#   random mvar / type = un subject = ID;
#   repeated mvar visit / type = un@ar(1) subject = ID;
#   run;
#
#   proc mixed method = ml;
#   class ID mvar visit;
#   model Outcome = mvar / solution ddfm = kr;
#   random mvar / type = un subject = ID;
#   repeated mvar visit / type = un@cs subject = ID;
#   run;
#
# Sources:
# 1. Roy (2006) doi: 10.1002/bimj.200510192
# 2. Shan et al. (2020) doi: 10.1155/2020/7398324
# ----

# Get the parameters for the Roy (2006) models.
# ***specify models***

parameters_roy_AR1 <- get_parameters( model_roy_AR1, person_idx = 2 )
parameters_roy_CS <- get_parameters( model_roy_CS, person_idx = 2 )

# ----
