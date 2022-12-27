#===============================================================================
#                            Econometrics
# Name: JR
# Date: december 14, 2022
# Source: https://www.youtube.com/watch?v=xOK0UlrR3Ks&t=4s
#===============================================================================

# Section 0 ---------------------------------------------------------------
  ## Remove 
    rm(list = ls())
  ## install.packages("name")
    install.packages("vars")
    install.packages("tseries")
    install.packages("tidyverse")
    install.packages("stargazer")
  ## Another bulk package installation option
    PackagesInstall <- c("vars",
                         "tseries",
                         "tidyverse",
                         "stargazer")
    install.packages(PackagesInstall)
  ## library(name)
    library(vars)
    library(tseries)
    library(tidyverse)
    library(stargazer)
  ## Remember
    ### 1.- I(0)
    ### 2.- Optimal lag length
    ### 3.- Estimate
    ### 4.- Stable model
    ### 5.- Granger Causality Test
    ### 6.- IRFs
    ### 7.- Variance decomposition
    
# Section 1 ---------------------------------------------------------------
  ## Load the dataset
    df <- read.csv(file.choose())
    
# Section 2 ---------------------------------------------------------------
  ## head(), tail() and str()
    head(df); tail(df); str(df)
    
# Section 3 ---------------------------------------------------------------
  ## Transformations on time series variables
    y1 <- ts(df$y1, 
             start = c(2000,1), 
             frequency = 12)
    y2 <- ts(df$y2,
             start = c(2000,1),
             frequency = 12)
  ## Autoplot()
    plot(y1) # autoplot(y1)
    plot(y2) # autoplot(y2)
    
# Section 4 ---------------------------------------------------------------
  ## 1.- I(0) ADF
    adfy1 <- adf.test(y1)
    adfy1
    adfy2 <- adf.test(y2)
    adfy2
  ## 2.- Optimal lag length
    lag <- VARselect(df, 
                     lag.max = 3)
    lag$selection
    lag$criteria
  ## 3.- Estimate
    Estim <- VAR(df, 
                 p = 1, 
                 type = "none")
    summary(Estim)
    stargazer(Estim[["varresult"]], 
              type = "text")
  ## 4.- Stable model
    roots(Estim, 
          modulus = TRUE)
  ## 5.- Granger Causality Test
    grangery1 <- causality(Estim, 
                           cause = "y1")
    grangery1$Granger 
    
    grangery2 <- causality(Estim, 
                           cause = "y2")
    grangery2$Granger
  ## 6.- IRFs
    ### y2 response to y1 shock
    irf1 <- irf(Estim, 
                impulse = "y1", 
                response = "y2",
                n.ahead = 20,
                boot = TRUE,
                run = 200,
                ci = 0.95)
    plot(irf1, 
         ylab = "y2",
         main = "y2 response to y1 shock")
    
    ### y1 response to y2 shock
    irf2 <- irf(Estim, 
                impulse = "y2", 
                response = "y1",
                n.ahead = 20,
                boot = TRUE,
                run = 200,
                ci = 0.95)
    plot(irf2, 
         ylab = "y1",
         main = "y1 response to y2 shock")
  ## 7.- Variance decomposition
    vd <- fevd(Estim, n.ahead = 10)
    plot(vd)
    
