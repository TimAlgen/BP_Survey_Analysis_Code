################################################################################
### BSc Project Survey Analysis ###############################################

  # [1] Raw Data will be imported and cleaned.
  # [2] The answers will be analyzed in isolation
  # [3] Correlations and potential dependencies will be analyzed 

################################################################################
################################################################################

### [0] Loading Packages needed

library(car)
library(nnet)
library(readxl)
library(psych)
library(dplyr)
library(emmeans)
library(ggplot2)
library(lmtest)
library(explore)
library(tidyverse)

################################################################################
### [1] DATA CLEANING ##########################################################
################################################################################

# Importing raw data 
  RawData <- read_excel("/Users/timfischmann/Desktop/BP Final Survey Data - May 26 12am.xlsx")

# Selecting and renaming columns we'll use, getting rid of all others such as IP address
  CleaningData <- RawData[, c(7, 8, 9, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 56, 57, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68)]
  colnames(CleaningData) <- c("X1_Finished", "X2_Recorded", "X3_ID", "G1_Invested", "G2a_Stocks", "G2a_Bonds", "G2a_ETFs", "G2a_Commodities", "G2a_Crypto", "G2a_Derivatives", "G2a_Other", "G2a_OtherText", "G2b_Knowledge", "G2b_Time", "G2b_Money", "G2b_Interest", "G2b_Risk", "G2b_Other", "G2b_OtherText", "G3_Experience", "G4_Risk1", "G4_Risk2", "G4_Risk3", "G4_Risk4", "G5_Time1", "G5_Time2", "G5_Time3", "G5_Time4", "E1_Consider", "E2_Information", "E3_Reputation", "E3_History", "E3_KIID", "E3_Managers", "E3_NoInfo", "E3_Opinions", "E3_Media", "E3_Other", "E3_OtherText", "E4_Holding", "E5_Withdrawal", "E6_Trust1", "E6_Trust2", "E6_Trust3", "E6_Trust4", "D1_Age", "D2_Gender", "D3_Nationality", "D4_Education", "D4_OtherText", "D5_Portfolio")

# Only going forward with finished answers, removing incomplete ones
  CleaningData$X1_Finished <- as.numeric(CleaningData$X1_Finished)
  CleaningData <- CleaningData[!is.na(CleaningData$X1_Finished), ]
  CleaningData <- CleaningData[CleaningData$X1_Finished == 1, ]
  
# Our data doesn't contain responses of minors, otherwise ages between 1 and 17 would need to be excluded here

# Converting datatypes char to numeric & reverse code items
  CleaningData[4:11] <- lapply(CleaningData[4:11], as.numeric)
  CleaningData[13:18] <- lapply(CleaningData[13:18], as.numeric)
  CleaningData[20] <- lapply(CleaningData[20], as.numeric)
  CleaningData$G4_Risk1 <- as.numeric(CleaningData$G4_Risk1)
  CleaningData$G4_Risk2 <- 8 - as.numeric(CleaningData$G4_Risk2)                # G4_Risk 2 is reverse coded
  CleaningData$G4_Risk3 <- as.numeric(CleaningData$G4_Risk3)
  CleaningData$G4_Risk4 <- as.numeric(CleaningData$G4_Risk4)
  CleaningData$G5_Time1 <- 8 - as.numeric(CleaningData$G5_Time1)                # G5_Time1 is reverse coded
  CleaningData$G5_Time2 <- as.numeric(CleaningData$G5_Time2)
  CleaningData$G5_Time3 <- as.numeric(CleaningData$G5_Time3)
  CleaningData$G5_Time4 <- as.numeric(CleaningData$G5_Time4)
  CleaningData[29:38] <- lapply(CleaningData[29:38], as.numeric)
  CleaningData[40:45] <- lapply(CleaningData[40:45], as.numeric)
  CleaningData$E2_Information <- 6 - CleaningData$E2_Information                # E2_Information is reverse coded (makes more sense to have little information = 1, more information = 5)


# Cleaning & standardizing Demographics inputs & converting them to numeric data
  # D1_Age: using unique() function and re-coding all non-numbers manually
    CleaningData$D1_Age <- ifelse(CleaningData$D1_Age == "45-55", 50, CleaningData$D1_Age) # converting "45-55" to 50
    CleaningData$D1_Age <- ifelse(CleaningData$D1_Age == "40+", 45, CleaningData$D1_Age) # converting "40+" to 45
    CleaningData$D1_Age <- ifelse(CleaningData$D1_Age == "&gt;50", 55, CleaningData$D1_Age) # converting "&gt;50" to 55
    CleaningData$D1_Age <- ifelse(CleaningData$D1_Age %in% c("Fine", "0"), NA, CleaningData$D1_Age) # converting non-identifiable ages to NA
  
    CleaningData$D1_Age <- as.numeric(CleaningData$D1_Age)
    CleaningData$D2_Gender <- as.numeric(CleaningData$D2_Gender)

  # D3_Nationality: standardizing & grouping responses
    unique(CleaningData$D3_Nationality)                                         # Analyzing Data to find what needs to be cleaned
    CleaningData$D3_Nationality <- ifelse(CleaningData$D3_Nationality %in% c("DE", "germany", "Deutschland", "GERMANY"), "Germany", CleaningData$D3_Nationality) # Standardizing all synonyms of Germany
    CleaningData$D3_Nationality <- ifelse(CleaningData$D3_Nationality %in% c("NL", "netherlands", "The netherlands", "The Netherlands"), "Netherlands", CleaningData$D3_Nationality) # Standardizing all synonyms of Netherlands
    CleaningData$D3_Nationality <- ifelse(CleaningData$D3_Nationality %in% c("HR", "Hrvatska"), "Croatia", CleaningData$D3_Nationality) # Standardizing all synonyms of Croatia
    CleaningData$D3_Nationality <- ifelse(CleaningData$D3_Nationality %in% c("Ecuador", "Brazil", "Israel", "Canada", "United States", "Australia", "uk", "United Kingdom", "UK", "Switzerland"), "nonEU", CleaningData$D3_Nationality) # Standardizing all non-Europe countries
    CleaningData$D3_Nationality <- ifelse(CleaningData$D3_Nationality %in% c("n/a", "€"), NA, CleaningData$D3_Nationality) # Standardizing all non-identifiable ones as NA

    CleaningData$D4_Education <- as.numeric(CleaningData$D4_Education)
  
  # Adding "0" as Portfolio size for those who stated that they do not invest
    CleaningData$D5_Portfolio <- as.numeric(CleaningData$D5_Portfolio)
    CleaningData$D5_Portfolio[CleaningData$G1_Invested == "2"] <- 0
  
# Cleaning the "OtherText" data fields
  
  # G2a_OtherText (asset types invested in)
    # "Anleihen" changed to regular "Bonds" input
    CleaningData$G2a_Bonds[CleaningData$G2a_OtherText == "Anleihen"] <- 1
    CleaningData$G2a_Other[CleaningData$G2a_OtherText == "Anleihen"] <- NA
    CleaningData$G2a_OtherText <- ifelse(CleaningData$G2a_OtherText == "Anleihen", NA, CleaningData$G2a_OtherText)
    # "The future of my children ;-)" is deleted, doesn't qualify
    CleaningData$G2a_Other[CleaningData$G2a_OtherText == "The future of my children ;-)"] <- NA
    CleaningData$G2a_OtherText <- ifelse(CleaningData$G2a_OtherText == "The future of my children ;-)", NA, CleaningData$G2a_OtherText)
    # Increasing value for "Other" if people invested in multiple other asset types
    CleaningData$G2a_Other[CleaningData$G2a_OtherText == "Partnership interests, closed funds"] <- 2
    CleaningData$G2a_Other[CleaningData$G2a_OtherText == "Rents, buildings, fonds"] <- 2
  
  # G2b_OtherText - no adjustments made
  
  # E3_OtherText - no adjustments made
  
  # D4_OtherText (education of respondents)
    # Synonyms for Secondary School
    CleaningData$D4_Education[CleaningData$D4_OtherText %in% c("High school", "High school IBDP")] <- 1
    CleaningData$D4_OtherText <- ifelse(CleaningData$D4_OtherText %in% c("High school", "High school IBDP"), NA, CleaningData$D4_OtherText)
    # Synonyms for Bachelor
    CleaningData$D4_Education[CleaningData$D4_OtherText %in% c("Fachhochschule")] <- 2
    CleaningData$D4_OtherText <- ifelse(CleaningData$D4_OtherText %in% c("Fachhochschule"), NA, CleaningData$D4_OtherText)
    # Synonyms for Master
    CleaningData$D4_Education[CleaningData$D4_OtherText %in% c("Diplom", "Diplom-Betriebswirt", "State exam", "2nd state examination")] <- 3
    CleaningData$D4_OtherText <- ifelse(CleaningData$D4_OtherText %in% c("Diplom", "Diplom-Betriebswirt", "State exam", "2nd state examination"), NA, CleaningData$D4_OtherText)
    
################################################################################  
### [2] ISOLATED ANALYSIS ######################################################
################################################################################
  
  # Copying cleaned Data and working with "cd" from now on, to distinguish steps but also because it's shorter :)
  cd <- CleaningData
    
  # Before starting the isolated analysis, using the following tool from the explore package to analyze the data on a high level. It was also used to plot graphs later.
  eda <- cd[, -c(1, 2, 3, 54)]
  eda %>% explore()
  
### [2a] Computing interesting Scores ##########################################
      
  # Cronbach Alphas for Risk, Decision Process ("Time"), Trust
    risk_df <- cd %>% select(G4_Risk1, G4_Risk2, G4_Risk3, G4_Risk4)            # Step 1: Creating new dataframe with risk variables
    alpha_risk <- psych::alpha(risk_df)                                         # Step 2: Computing Cronbach alpha using `alpha` function from package `psych` 
    alpha_risk$total                                                            # Cronbach alpha is displayed under "raw alpha" in the output below, should be above 0.7 (I think)
    alpha_risk$alpha.drop                                                       # Dropping item 2 would increase alpha from < 0.75 to > 0.84
  
    time_df <- cd %>% select(G5_Time1, G5_Time2, G5_Time3, G5_Time4)            # Step 1: Creating new dataframe with time (decision process) variables
    alpha_time <- psych::alpha(time_df)                                         # Step 2: Computing Cronbach alpha using `alpha` function from package `psych` 
    alpha_time$total                                                            # Cronbach alpha is displayed under "raw alpha" in the output below, should be above 0.7 (I think)
    alpha_time$alpha.drop                                                       # Looks like dropping any item would not increase the alpha
    
    trust_df <- cd %>% select(E6_Trust1, E6_Trust2, E6_Trust3, E6_Trust4)       # Step 1: Creating new dataframe with trust variables
    alpha_trust <- psych::alpha(trust_df)                                       # Step 2: Computing Cronbach alpha using `alpha` function from package `psych` 
    alpha_trust$total                                                           # Cronbach alpha is displayed under "raw alpha" in the output below, should be above 0.7 (I think)
    alpha_trust$alpha.drop                                                      # Dropping item 4 would slightly increase alpha from 0.707 to 0.714
  
  # Computing average Risk / Time / Trust score per person and adding them as additional columns
    G4_RiskScore <- rowMeans(cd[, c(21, 23, 24)])                               # Computing average risk score, dropping Risk2 because of Cronbach Alpha
    G5_TimeScore <- rowMeans(cd[, 25:28])                                       # Computing average time score, out of all 4 values
    E6_TrustScore <- rowMeans(cd[, 42:44])                                      # Computing average trust score, dropping Trust4 because of Cronbach Alpha
    cd <- cbind(cd[, 1:24], G4_RiskScore, cd[, 25:28], G5_TimeScore, cd[, 29:45], E6_TrustScore, cd[, 46:51])   # adding previously calculated scores to the dataframe
  
  # Calculate overall Number of asset types someone invested in for G2a
    G2a_AssetTypes <- rowSums(apply(cd[, c("G2a_Stocks", "G2a_Bonds", "G2a_ETFs", "G2a_Commodities", "G2a_Crypto", "G2a_Derivatives", "G2a_Other")], 2, function(x) replace(x, is.na(x), 0)))                                        # Computing the number
    cd <- cbind(cd[, 1:12], G2a_AssetTypes, cd[, 13:54])                        # adding previously calculated score to the dataframe

  # Cleaned Data only from People who'd consider investing into ELTIFs  
    cdPositive <- cd[cd$E1_Consider == 1, ]   
    
  # Summary of all variables in the cleaned Data, including above-calculated Scores
    summary(cd)
    
### [2b] Isolated Analysis of each Variable ####################################
  
# [G] General Investor Behavior    
  # G1 / G2a
    # Numbers of Asset Types
    ggplot(cd, aes(x = G2a_AssetTypes)) +                                       # plotting the distribution as a histogram
      geom_histogram(alpha = 1, position = "identity", binwidth = 1, color = "black")
    table(G2a_AssetTypes)
    prop.table(table(cd$G2a_AssetTypes))
    summary(cd$G2a_AssetTypes)
  
    # Number of People per Asset Type
    table(cd$G2a_Stocks)                                                        # Absolute - Stocks
    prop.table(table(cd$G2a_Stocks))                                            # Relative - Stocks
    table(cd$G2a_Bonds)                                                         # Absolute - Bonds
    prop.table(table(cd$G2a_Bonds))                                             # Relative - Bonds
    table(cd$G2a_ETFs)                                                          # Absolute - ETFs
    prop.table(table(cd$G2a_ETFs))                                              # Relative - ETFs
    table(cd$G2a_Commodities)                                                   # Absolute - Commodities
    prop.table(table(cd$G2a_Commodities))                                       # Relative - Commodities
    table(cd$G2a_Crypto)                                                        # Absolute - Cryptocurrencies
    prop.table(table(cd$G2a_Crypto))                                            # Relative - Cryptocurrencies
    table(cd$G2a_Derivatives)                                                   # Absolute - Derivatives
    prop.table(table(cd$G2a_Derivatives))                                       # Relative - Derivatives
    table(cd$G2a_Other)                                                         # Absolute - Other
    prop.table(table(cd$G2a_Other))                                             # Relative - Other
    table(cd$G2a_OtherText)                                                        # Text - Other
    
  # G2b  
    # Mentions per reasons not to invest
    table(cd$G2b_Knowledge)                                                     # Absolute - Knowledge
    prop.table(table(cd$G2b_Knowledge))                                         # Relative - Knowledge
    table(cd$G2b_Time)                                                          # Absolute - Time
    prop.table(table(cd$G2b_Time))                                              # Relative - Time
    table(cd$G2b_Money)                                                         # Absolute - Money
    prop.table(table(cd$G2b_Money))                                             # Relative - Money
    table(cd$G2b_Interest)                                                      # Absolute - Interest
    prop.table(table(cd$G2b_Interest))                                          # Relative - Interest
    table(cd$G2b_Risk)                                                          # Absolute - Risk
    prop.table(table(cd$G2b_Risk))                                              # Relative - Risk
    table(cd$G2b_Other)                                                         # Absolute - Other
    prop.table(table(cd$G2b_Other))                                             # Relative - Other
    table(cd$G2b_OtherText)                                                        # Text - Other
    
  # G3
    table(cd$G3_Experience)
    prop.table(table(cd$G3_Experience))
    
  # G4 (only taking overall score into account)
    ggplot(cd, aes(x = G4_RiskScore)) +
      geom_histogram(alpha = 1, position = "identity", binwidth = 0.33333333333333, color = "black")
    table(cd$G4_RiskScore)
    round(prop.table(table(cd$G4_RiskScore)), 4)
    summary(cd$G4_RiskScore)
    
  # G5 (only taking overall score into account)
    ggplot(cd, aes(x = G5_TimeScore)) +
      geom_histogram(alpha = 1, position = "identity", binwidth = 0.25, color = "black")
    table(cd$G5_TimeScore)
    round(prop.table(table(cd$G5_TimeScore)), 4) 
    summary(cd$G5_TimeScore)
  
# [E] Hypothetical Scenario of being able to invest into PE (ELTIF)
  # E1
    table(cd$E1_Consider)
    round(prop.table(table(cd$E1_Consider)), 4) 
    
  # E2
    ggplot(cd, aes(x = E2_Information)) +
      geom_histogram(alpha = 1, position = "identity", binwidth = 1, color = "black")
    table(cd$E2_Information)
    round(prop.table(table(cd$E2_Information)), 4) 
    summary(cd$E2_Information)
    
  # E3
    # Mentions per piece of information considered
    table(cd$E3_NoInfo)                                                         # Absolute - NoInfo
    prop.table(table(cd$E3_NoInfo))                                             # Relative - NoInfo
    
    table(cd$E3_Reputation)                                                     # Absolute - Reputation
    prop.table(table(cd$E3_Reputation))                                         # Relative - Reputation
    table(cd$E3_History)                                                        # Absolute - History
    prop.table(table(cd$E3_History))                                            # Relative - History
    table(cd$E3_KIID)                                                           # Absolute - KIID
    prop.table(table(cd$E3_KIID))                                               # Relative - KIID
    table(cd$E3_Managers)                                                       # Absolute - Managers
    prop.table(table(cd$E3_Managers))                                           # Relative - Managers
    table(cd$E3_Opinions)                                                       # Absolute - Opinions
    prop.table(table(cd$E3_Opinions))                                           # Relative - Opinions
    table(cd$E3_Media)                                                          # Absolute - Media
    prop.table(table(cd$E3_Media))                                              # Relative - Media
    table(cd$E3_Other)                                                          # Absolute - Other
    prop.table(table(cd$E3_Other))                                              # Relative - Other
    table(cd$E3_OtherText)                                                        # Text - Other
    
    # Mentions only among E1_Consider = yes people
    table(cdPositive$E3_NoInfo)                                                         # Absolute - NoInfo
    prop.table(table(cdPositive$E3_NoInfo))                                             # Relative - NoInfo
    
    table(cdPositive$E3_Reputation)                                                     # Absolute - Reputation
    prop.table(table(cdPositive$E3_Reputation))                                         # Relative - Reputation
    table(cdPositive$E3_History)                                                        # Absolute - History
    prop.table(table(cdPositive$E3_History))                                            # Relative - History
    table(cdPositive$E3_KIID)                                                           # Absolute - KIID
    prop.table(table(cdPositive$E3_KIID))                                               # Relative - KIID
    table(cdPositive$E3_Managers)                                                       # Absolute - Managers
    prop.table(table(cdPositive$E3_Managers))                                           # Relative - Managers
    table(cdPositive$E3_Opinions)                                                       # Absolute - Opinions
    prop.table(table(cdPositive$E3_Opinions))                                           # Relative - Opinions
    table(cdPositive$E3_Media)                                                          # Absolute - Media
    prop.table(table(cdPositive$E3_Media))                                              # Relative - Media
    table(cdPositive$E3_Other)                                                          # Absolute - Other
    prop.table(table(cdPositive$E3_Other))                                              # Relative - Other
    table(cdPositive$E3_OtherText)                                                        # Text - Other
    
  # E4
    ggplot(cd, aes(x = E4_Holding)) +
      geom_histogram(alpha = 1, position = "identity", binwidth = 1, color = "black")
    table(cd$E4_Holding)
    round(prop.table(table(cd$E4_Holding)), 4) 
    summary(cd$E4_Holding)
    
  # E5
    ggplot(cd, aes(x = E5_Withdrawal)) +
      geom_histogram(alpha = 1, position = "identity", binwidth = 1, color = "black")
    table(cd$E5_Withdrawal)
    round(prop.table(table(cd$E5_Withdrawal)), 4) 
    summary(cd$E5_Withdrawal)
    
  # E6 (only taking overall score into account)
    ggplot(cd, aes(x = E6_TrustScore)) +
      geom_histogram(alpha = 1, position = "identity", binwidth = 0.33333333333334, color = "black")
    table(cd$E6_TrustScore)
    round(prop.table(table(cd$E6_TrustScore)), 4)
    summary(cd$E6_TrustScore)
    
# [D] Demographics
  # D1 - Age
    ggplot(cd, aes(x = D1_Age)) +
      geom_histogram(alpha = 1, position = "identity", binwidth = 5, color = "black")
    table(cd$D1_Age)
    prop.table(table(cd$D1_Age))
    summary(cd$D1_Age)
    
  # D2 - Gender
    table(cd$D2_Gender)
    round(prop.table(table(cd$D2_Gender)), 4)
    
  # D3 - Nationality
    table(cd$D3_Nationality)
    round(prop.table(table(cd$D3_Nationality)), 4)
    
  # D4 - Highest level of education (enrolled or completed)
    ggplot(cd, aes(x = D4_Education)) +
      geom_histogram(alpha = 1, position = "identity", binwidth = 1, color = "black")
    table(cd$D4_Education)
    round(prop.table(table(cd$D4_Education)), 4)
    
  # D5 - Portfolio Size in €
    ggplot(cd, aes(x = D5_Portfolio)) +
      geom_histogram(alpha = 1, position = "identity", binwidth = 1, color = "black")
    table(cd$D5_Portfolio)
    round(prop.table(table(cd$D5_Portfolio)), 4)
    
################################################################################
### [3] COMPARATIVE ANALYSIS ###################################################
 
  # The comparative Analysis is structured like the isolated analysis, 
  # by the appearance of questions in the survey
    
################################################################################
    
  # G1_Invested -> E1_Consider
    Chi2_G1_E1 <- table(cd$E1_Consider, cd$G1_Invested)
      Chi2R_G1_E1 <- chisq.test(Chi2_G1_E1)
      print(Chi2R_G1_E1)                                                        # p value = 0.0001057 (***)
    table(cd$E1_Consider, cd$G1_Invested)
    prop.table(table(cd$E1_Consider, cd$G1_Invested), 2)
    
  # G2a_AssetTypes -> E1_Consider
    table(cd$E1_Consider, cd$G2a_AssetTypes)
    prop.table(table(cd$E1_Consider, cd$G2a_AssetTypes), 2)
    aggregate(G2a_AssetTypes ~ E1_Consider, cd, mean)
    
    Chi2_G2aSt_E1 <- table(cd$E1_Consider, cd$G2a_Stocks)                       # Stocks
      Chi2R_G2aSt_E1 <- chisq.test(Chi2_G2aSt_E1)
      print(Chi2R_G2aSt_E1)                                                     # p value = 0.3965
    table(cd$E1_Consider, cd$G2a_Stocks)
    prop.table(table(cd$E1_Consider, cd$G2a_Stocks), 2)

    Chi2_G2aBo_E1 <- table(cd$E1_Consider, cd$G2a_Bonds)                        # Bonds
      Chi2R_G2aBo_E1 <- chisq.test(Chi2_G2aBo_E1)
      print(Chi2R_G2aBo_E1)                                                     # p value = 0.2297
    table(cd$E1_Consider, cd$G2a_Bonds)
    prop.table(table(cd$E1_Consider, cd$G2a_Bonds), 2)
      
    Chi2_G2aET_E1 <- table(cd$E1_Consider, cd$G2a_ETFs)                         # ETFs
      Chi2R_G2aET_E1 <- chisq.test(Chi2_G2aET_E1)
      print(Chi2R_G2aET_E1)                                                     # p value = 0.8006
    table(cd$E1_Consider, cd$G2a_ETFs)
    prop.table(table(cd$E1_Consider, cd$G2a_ETFs), 2)
      
    Chi2_G2aCo_E1 <- table(cd$E1_Consider, cd$G2a_Commodities)                  # Commodities
      Chi2R_G2aCo_E1 <- chisq.test(Chi2_G2aCo_E1)
      print(Chi2R_G2aCo_E1)                                                     # p value = 0.8368
    table(cd$E1_Consider, cd$G2a_Commodities)
    prop.table(table(cd$E1_Consider, cd$G2a_Commodities), 2) 
      
    Chi2_G2aCr_E1 <- table(cd$E1_Consider, cd$G2a_Crypto)                       # Cryptocurrencies
      Chi2R_G2aCr_E1 <- chisq.test(Chi2_G2aCr_E1)
      print(Chi2R_G2aCr_E1)                                                     # p value = 0.3857
      
    Chi2_G2aDe_E1 <- table(cd$E1_Consider, cd$G2a_Derivatives)                  # Derivatives
      Chi2R_G2aDe_E1 <- chisq.test(Chi2_G2aDe_E1)
      print(Chi2R_G2aDe_E1)                                                     # p value = 0.1012   
      
    Chi2_G2aOt_E1 <- table(cd$E1_Consider, cd$G2a_Other)                        # Other Assets
      Chi2R_G2aOt_E1 <- chisq.test(Chi2_G2aOt_E1)
      print(Chi2R_G2aOt_E1)                                                     # p value = 0.9105    
      
  # G2a_AssetTypes -> E2_Information
    CorR_G2a_E2 <- cor(cd$G2a_AssetTypes, cd$E2_Information)
      print (CorR_G2a_E2)                                                       # Correlation Coefficient = - 0.08756947
    CorTR_G2a_E2 <- cor.test(cd$G2a_AssetTypes, cd$E2_Information)
      print (CorTR_G2a_E2)                                                      # p value = 0.141 (tested with G1 aswell, also not significant at p value > 0.2)
      
  # G2a and E6
    cor.test(cd$G2a_AssetTypes, cd$E6_TrustScore)                               # p value = 0.795
      
  # G3_Experience -> E1_Consider
    # Chi2
    Chi2_G3_E1 <- table(cd$E1_Consider, cd$G3_Experience)
      Chi2R_G3_E1 <- chisq.test(Chi2_G3_E1)
      print(Chi2R_G3_E1)                                                        # p value = 1.88e-06 (***)          
    # Tables
    table(cd$E1_Consider, cd$G3_Experience)
    prop.table(table(cd$E1_Consider, cd$G3_Experience), 2) 
   
  # G3_Experience and E2_Information 
    spearmanR_G3_E2 <- cor.test(cd$G3_Experience, cd$E2_Information, method = "spearman")
      print(spearmanR_G3_E2)                                                    # p value = 0.05855; rho = -0.1124
  
  # G3_Experience and E4_Holding
    spearmanR_G3_E4 <- cor.test(cd$E4_Holding, cd$G3_Experience, method = "spearman")
      print(spearmanR_G3_E4)                                                    # p value = 7.867e-06 (***)
    
  # G3_Experience and E6_TrustScore
    cor.test(cd$G3_Experience, cd$E6_TrustScore)                                # p-value = 0.302
      
  # G4_RiskScore -> E1_Consider
    # Anova
    anovaR_G4_E1 <- aov(G4_RiskScore ~ E1_Consider, data = cd)
      summary(anovaR_G4_E1)                                                     # p value = 1.42e-06 (***)
    # Tables
    table(cd$E1_Consider, cd$G4_RiskScore)
    prop.table(table(cd$E1_Consider, cd$G4_RiskScore), 2)          
 
  # G4_RiskScore -> E2_Information
    CorTR_G4_E2 <- cor.test(cd$G4_RiskScore, cd$E2_Information)
      print (CorTR_G4_E2)                                                       # p value = 0.1221
    
  # G4_RiskScore -> E4_Holding
    spearmanR_G4_E4 <- cor.test(cd$E4_Holding, cd$G4_RiskScore, method = "spearman")
      print(spearmanR_G4_E4)                                                    # p value = 0.01252 (*); rho = 0.1480
      
  # G5_TimeScore -> E1_Consider
    anovaR_G5_E1 <- aov(G5_TimeScore ~ E1_Consider, data = cd)
      summary(anovaR_G5_E1)                                                     # p value = 0.114
      
  # G5_TimeScore and E2_Information
    spearmanR_G5_E2 <- cor.test(cd$G5_TimeScore, cd$E2_Information, method = "spearman")
      print(spearmanR_G5_E2)                                                    # p value = 0.002053 (**); rho = 0.1822
    table(cd$E2_Information, cd$G5_TimeScore)
    round(prop.table(table(cd$E2_Information, cd$G5_TimeScore), 2), 4)
    
  # E1_Consider -> E4_Holding
    chisq.test(table(cd$E1_Consider, cd$E4_Holding))                            # p value = 0.001947 (**)
    table(cd$E1_Consider, cd$E4_Holding)
    round(prop.table(table(cd$E1_Consider, cd$E4_Holding), 2), 4)
    
  # E1_Consider -> E5_Withdrawal
    chisq.test(table(cd$E1_Consider, cd$E5_Withdrawal))                         # p value = 0.2388
    table(cd$E1_Consider, cd$E5_Withdrawal)
    round(prop.table(table(cd$E1_Consider, cd$E5_Withdrawal), 2), 4) 
    
  # E1_Consider and E6_TrustScore
    Chi2_E1_E6 <- table(cd$E1_Consider, cd$E6_TrustScore)
      Chi2R_E1_E6 <- chisq.test(Chi2_E1_E6)
      print(Chi2R_E1_E6)                                                        # p value = 0.001297 (**)
    table(cd$E1_Consider, cd$E6_TrustScore)
    round(prop.table(table(cd$E1_Consider, cd$E6_TrustScore), 2), 4)  
    aggregate(E6_TrustScore ~ E1_Consider, cd, mean)
  
  # E2_Information and E6_TrustScore
    CorTR_E2_E6 <- cor.test(cd$E2_Information, cd$E6_TrustScore)
      print(CorTR_E2_E6)                                                        # p value = 0.44
      
  # E4_Holding and E5_Withdrawal
    spearmanR_E4_E5 <- cor.test(cd$E4_Holding, cd$E5_Withdrawal, method = "spearman")
      print(spearmanR_E4_E5)                                                    # p value = 7.691e-13 (***); rho = 0.408373 which indicates positive correlation
    table(cd$E4_Holding, cd$E5_Withdrawal)
    round(prop.table(table(cd$E4_Holding, cd$E5_Withdrawal), 2), 4) 
    
### [3c] Other non-contributing Analyses #######################################   
    
  # G2a_AssetTypes and G3_Experience
    cor.test(cd$G2a_AssetTypes, cd$G3_Experience)                               # p value = 2.2e-16; Correlation coefficient = 0.6359141
    
  # G3_Experience and G4_RiskScore
    anovaR_G3_G4 <- aov(G4_RiskScore ~ G3_Experience, data = cd)
      summary(anovaR_G3_G4)                                                     # p value = 5.44e-15
    cor.test(cd$G3_Experience, cd$G4_RiskScore)                                 # correlation coefficient = 0.4416843
    table(cd$G3_Experience, cd$G4_RiskScore)
    prop.table(table(cd$G3_Experience, cd$G4_RiskScore), 2)                    
  
  # G3_Experience and G5_TimeScore
    cor.test(cd$G3_Experience, cd$G5_TimeScore)                                 # p value = 0.6899
    
  # G3_Experience and D2_Gender
    anovaR_G3_D2 <- aov(G3_Experience ~ D2_Gender, data = cd)
      summary(anovaR_G3_D2)                                                     # p value = 6.03e-09 (***)
    Chi2_G3_D2 <- table(cd$G3_Experience, cd$D2_Gender)
      Chi2R_G3_D2 <- chisq.test(Chi2_G3_D2)
      print(Chi2R_G3_D2)                                                        # p-value = 3.655e-08 (***)
    table(cd$G3_Experience, cd$D2_Gender)
    round(prop.table(table(cd$G3_Experience, cd$D2_Gender), 2), 4)              # Women rated themselves lower on experience scale

  # G4_RiskScore and G5_TimeScore
    CorR_G4_G5 <- cor(cd$G4_RiskScore, cd$G5_TimeScore)
    print (CorR_G4_G5)                                                        # Correlation Coefficient = - 0.2037205
    CorTR_G4_G5 <- cor.test(cd$G4_RiskScore, cd$G5_TimeScore)
    print (CorTR_G4_G5)                                                       # p value = 0.0005517 (***)  
    
  # E1_Consider and all Demographics
    anovaR_E1_D1 <- aov(D1_Age ~ E1_Consider, data = cd)                        # Age
    summary(anovaR_E1_D1)                                                     # p value = 0.00991 (**)
    
    Chi2_E1_D2 <- table(cd$E1_Consider, cd$D2_Gender)                         
    Chi2R_E1_D2 <- chisq.test(Chi2_E1_D2)                                     # Gender
    print(Chi2R_E1_D2)                                                        # p-value = 3.129e-06 (***)
    
    Chi2_E1_D3 <- table(cd$E1_Consider, cd$D3_Nationality)                         
    Chi2R_E1_D3 <- chisq.test(Chi2_E1_D3)                                     # Nationality
    print(Chi2R_E1_D3)                                                        # p-value = 0.1943
    
    Chi2_E1_D4 <- table(cd$E1_Consider, cd$D4_Education)                         
    Chi2R_E1_D4 <- chisq.test(Chi2_E1_D4)                                     # Education
    print(Chi2R_E1_D4)                                                        # p-value = 0.124
    
    Chi2_E1_D5 <- table(cd$E1_Consider, cd$D5_Portfolio)                         
    Chi2R_E1_D5 <- chisq.test(Chi2_E1_D5)                                     # Portfolio
    print(Chi2R_E1_D5)                                                        # p-value = 0.007774 (**)
    
  # E2_Information and D5_Portfolio
    Chi2_E2_D5 <- table(cd$E2_Information, cd$D5_Portfolio)                         
      Chi2R_E2_D5 <- chisq.test(Chi2_E2_D5)
      print(Chi2R_E2_D5)                                                        # p-value = 0.7475
      
################################################################################
### END OF CODE ################################################################
################################################################################    