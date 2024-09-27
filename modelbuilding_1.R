library(readr) # To read CSV files

df<- read.csv("C:/Users/kuppa/Downloads/Nomissingfinal_030123.csv")
names (df)

sum(is.na (df))
sapply(df,class)
#####################################################
##Initially explain variation in Complete Series Vaccination rates by demographics (including age, race,gender) of the county's residents.
#Report the percent of variation explained.  

# Fit the model with rescaled variables
model_xa <- lm(Series_Complete_Pop_Pct_x ~
                 Census2019_5PlusPop_x+Census2019_5to17Pop_x+Census2019_12PlusPop_x+ Census2019_18PlusPop_x+Census2019_65PlusPop_x +
                 below18yearsofage2019_x+older65over2019_x+below18yearsofage2020+ 
                 older65over2020 + below18yearsofageApril2022+
                 older65overApril2022 +Asian2019_x + Asian2020 + AsianApril2022 +
                 AmericanIndian_AlaskaNative2019+AmericanIndian_AlaskaNative2020+ 
                 AmericanIndian_AlaskaNativeApri +Females2019_x + Females2020 +
                 FemalesApril2022,
               data = df)

# Print summary of the model
summary (model_xa)

# Calculate the percent of variation explained 
total_variation <- var (df$Series_Complete_Pop_Pct_x)
explained_variation <- 1 - (var (model_xa$residuals) / total_variation)
# Print the percent of variation explained
cat ("Percent of variation explained by demographics: ",
     round (explained_variation * 100, 2), "%\n")

################################################################################
#Explain variation in Complete Series Vaccination rates by demographics (age, race, gender), 
#and social determinants (including high school completion rate, percent nor
#proficient in English, percent employed, percent of children in poverty, 
#and median household income).  Report the percent of variation explained.


names (df)

# Fit the Linear Regression Model 
model_xb <- lm(Series_Complete_Pop_Pct_x ~
                 Census2019_5PlusPop_x+Census2019_5to17Pop_x+Census2019_12PlusPop_x+ 
                 Census2019_18PlusPop_x+Census2019_65PlusPop_x +
                 below18yearsofage2019_x+older65over2019_x+below18yearsofage2020+
                 older65over2020+below18yearsofageApril2022+older65overApril2022+
                 Asian2019_x+Asian2020+AsianApril2022+AmericanIndian_AlaskaNative2019+ 
                 AmericanIndian_AlaskaNative2020 + AmericanIndian_AlaskaNativeApri + 
                 Females2019_x+Females2020+FemalesApril2022+Highschoolcompletion2019_x+
                 Highschoolcompletion2020+HighschoolcompletionApril2022+
                 notproficientinEnglish2019_x+notproficientinEnglish2020+ 
                 notproficientinEnglishApril2022+Unemployment2019_x+
                 Unemployment2020+UnemploymentApril2022+
                 Trafficvolume2019_x+ Trafficvolume2020+TrafficvolumeApril2022+Childreninpoverty2019_x+
                 Childreninpoverty2020+ChildreninpovertyApril2022+
                 Childreninsingleparenthousehold+Childreninsingleparenthouseho_1+
                 Childreninsingleparenthouseho_2+Medianhouseholdincome2019_x+
                 Medianhouseholdincome2020+MedianhouseholdincomeApril2022,
               data = df)

#Fit the Model
summary(model_xb)

# Include interaction items
model_xb_interaction<<- lm(Series_Complete_Pop_Pct_x~
                             Census2019_5PlusPop_x+Census2019_5to17Pop_x+
                             Census2019_12PlusPop_x+Census2019_18PlusPop_x+
                             Census2019_65PlusPop_x + below18yearsofage2019_x+
                             older65over2019_x+below18yearsofage2020+
                             older65over2020 + below18yearsofageApril2022+ 
                             older65overApril2022 +Asian2019_x + Asian2020 +
                             AsianApril2022 +AmericanIndian_AlaskaNative2019 +
                             AmericanIndian_AlaskaNative2020 +
                             AmericanIndian_AlaskaNativeApri +
                             Females2019_x + Females2020 + FemalesApril2022+
                             Highschoolcompletion2019_x+Highschoolcompletion2020+
                             HighschoolcompletionApril2022+
                             notproficientinEnglish2019_x+notproficientinEnglish2020+
                             notproficientinEnglishApril2022+Unemployment2019_x+
                             Unemployment2020+UnemploymentApril2022+Trafficvolume2019_x+
                             Trafficvolume2020+TrafficvolumeApril2022+
                             Childreninpoverty2019_x+Childreninpoverty2020+
                             ChildreninpovertyApril2022+Childreninsingleparenthousehold+
                             Childreninsingleparenthouseho_1+
                             Childreninsingleparenthouseho_2+Medianhouseholdincome2019_x+ 
                             Medianhouseholdincome2020+MedianhouseholdincomeApril2022+ 
                             Unemployment2019_x*Trafficvolume2019_x+
                             Unemployment2020*Trafficvolume2020+
                             UnemploymentApril2022*TrafficvolumeApril2022+
                             Childreninpoverty2019_x*Childreninsingleparenthousehold+
                             Childreninpoverty2020*Childreninsingleparenthouseho_1+ 
                             ChildreninpovertyApril2022*Childreninsingleparenthouseho_2+
                             Childreninpoverty2019_x*Medianhouseholdincome2019_x+ 
                             Childreninpoverty2020*Medianhouseholdincome2020+
                             ChildreninpovertyApril2022*MedianhouseholdincomeApril2022,
                           data = df)
summary (model_xb_interaction)


# Calculate the percent of variation explained
total_variation <- var(df$Series_Complete_Pop_Pct_x)
explained_variation <- 1 - (var(model_xb$residuals) / total_variation)

# Print the percent of variation explained
cat("Percent of variation explained by the model:", 
    round(explained_variation * 100, 2), "%\n")


# Calculate the percent of variation explained
total_variation <- var(df$Series_Complete_Pop_Pct_x)
explained_variation <- 1 - (var(model_xb_interaction$residuals) / total_variation)

# Print the percent of variation explained
cat("Percent of variation explained by demographics and social determinants
     including interaction items: ", round(explained_variation * 100, 2), "%\n")


###########################################################################################
# Explain variation in Complete Series Vaccination rates by demographics (age, race, gender), 
#social determinants (including high school completion rate, percent nor 
#proficient in English, percent employed, percent of children in poverty, 
#median household income) and health of residents 
#(including percent population disabled, life expectancy, percent population having premature morbidity).  
#Report the percent of variation explained.

names(df)



# Fit the Linear Regression Model without Alias
model_xc <- lm(Series_Complete_Pop_Pct_x ~
                 Census2019_5PlusPop_x + Census2019_5to17Pop_x + Census2019_12PlusPop_x +
                 Census2019_18PlusPop_x + Census2019_65PlusPop_x +
                 below18yearsofage2019_x + older65over2019_x + below18yearsofage2020 + 
                 older65over2020 + below18yearsofageApril2022 + older65overApril2022 + 
                 Asian2019_x + Asian2020 + AsianApril2022 +
                 AmericanIndian_AlaskaNative2019 + AmericanIndian_AlaskaNative2020 +
                 AmericanIndian_AlaskaNativeApri + Females2019_x + Females2020 + 
                 FemalesApril2022 + Highschoolcompletion2019_x +
                 Highschoolcompletion2020 + HighschoolcompletionApril2022 +
                 notproficientinEnglish2019_x + notproficientinEnglish2020 + 
                 notproficientinEnglishApril2022 + Unemployment2019_x + 
                 Unemployment2020 + UnemploymentApril2022 + Trafficvolume2019_x +
                 Trafficvolume2020 + TrafficvolumeApril2022 + Childreninpoverty2019_x +
                 Childreninpoverty2020 + ChildreninpovertyApril2022 +
                 Childreninsingleparenthousehold + Childreninsingleparenthouseho_1 +
                 Childreninsingleparenthouseho_2 + Medianhouseholdincome2019_x +
                 Medianhouseholdincome2020 + MedianhouseholdincomeApril2022 + 
                 Disability2019_x + Disability2022 + DisabilityApril2022 +
                 Lifeexpectancy2019_x + Lifeexpectancy2020 + LifeexpectancyApril2022 +
                 Prematureageadjustedmortality20 + Prematureageadjustedmortality_1 +
                 PrematureageadjustedmortalityAp,
               data = df)

#Fit the model
summary(model_xc)


# Calculate the percent of variation explained
total_variation <- var(df$Series_Complete_Pop_Pct_x)
explained_variation <- 1 - (var(model_xc$residuals) / total_variation)

# Print the percent of variation explained
cat("Percent of variation explained by demographics, social determinants,
    and health of residents: ", round(explained_variation * 100, 2), "%\n")


################################################################################
#Explain variation in Complete Series Vaccination rates by demographics (age, race, gender), 
#social determinants (including high school completion rate, percent nor proficient in English,
#percent employed, percent of children in poverty, median household income), health of residents 
#(including percent population disabled, life expectancy, percent population having premature morbidity), 
#and political leaning of the population (including republican leaning, democrat leaning). 
#Report the percent of variation explained.

names(df)

# Fit the Linear Regression Model without Alias
model_xd <- lm(Series_Complete_Pop_Pct_x ~
                 Census2019_5PlusPop_x+Census2019_5to17Pop_x+Census2019_12PlusPop_x+ 
                 Census2019_18PlusPop_x+Census2019_65PlusPop_x +
                 below18yearsofage2019_x+older65over2019_x+below18yearsofage2020+
                 older65over2020 + below18yearsofageApril2022+
                 older65overApril2022 +Asian2019_x + Asian2020 + AsianApril2022 +
                 AmericanIndian_AlaskaNative2019 + AmericanIndian_AlaskaNative2020 + 
                 AmericanIndian_AlaskaNativeApri +Females2019_x + Females2020 +
                 FemalesApril2022+Highschoolcompletion2019_x+Highschoolcompletion2020+
                 HighschoolcompletionApril2022+notproficientinEnglish2019_x+
                 notproficientinEnglish2020+notproficientinEnglishApril2022+ 
                 Unemployment2019_x+Unemployment2020+UnemploymentApril2022+ Trafficvolume2019_x+
                 Trafficvolume2020+TrafficvolumeApril2022+ Childreninpoverty2019_x+
                 Childreninpoverty2020+ ChildreninpovertyApril2022+
                 Childreninsingleparenthousehold+Childreninsingleparenthouseho_1+
                 Childreninsingleparenthouseho_2+Medianhouseholdincome2019_x+
                 Medianhouseholdincome2020+MedianhouseholdincomeApril2022+ 
                 Disability2019_x+Disability2022+DisabilityApril2022+
                 Lifeexpectancy2019_x+Lifeexpectancy2020+LifeexpectancyApril2022+ 
                 Prematureageadjustedmortality20+Prematureageadjustedmortality_1+
                 PrematureageadjustedmortalityAp+Republicanpercent+DemocraticPercent+
                 Homeownership2019_x+Homeownership2020+HomeownershipApril2022+
                 Povertyrat_2019+Povertyrat2020+PovertyratApril2022+ 
                 Severehousingcostburden2019_x+Severehousingcostburden2020+
                 SeverehousingcostburdenApril202+ResidentialsegregationBlackWhit+
                 ResidentialsegregationBlackWh_1+ResidentialsegregationBlackWh_2+
                 Rural2010_x+Rural2010_1+RuralApril2022,
               data=df)



#Fit the model
summary(model_xd)

model_xd_interaction <- lm(Series_Complete_Pop_Pct_x ~
                             Census2019_5PlusPop_x+Census2019_5to17Pop_x+Census2019_12PlusPop_x+ 
                             Census2019_18PlusPop_x+Census2019_65PlusPop_x +
                             below18yearsofage2019_x+older65over2019_x+below18yearsofage2020+
                             older65over2020 + below18yearsofageApril2022+
                             older65overApril2022 +Asian2019_x + Asian2020 + AsianApril2022 +
                             AmericanIndian_AlaskaNative2019 + AmericanIndian_AlaskaNative2020 + 
                             AmericanIndian_AlaskaNativeApri +Females2019_x + Females2020 +
                             FemalesApril2022+Highschoolcompletion2019_x+Highschoolcompletion2020+
                             HighschoolcompletionApril2022+notproficientinEnglish2019_x+
                             notproficientinEnglish2020+notproficientinEnglishApril2022+ 
                             Unemployment2019_x+Unemployment2020+UnemploymentApril2022+ Trafficvolume2019_x+
                             Trafficvolume2020+TrafficvolumeApril2022+ Childreninpoverty2019_x+
                             Childreninpoverty2020+ ChildreninpovertyApril2022+
                             Childreninsingleparenthousehold+Childreninsingleparenthouseho_1+
                             Childreninsingleparenthouseho_2+Medianhouseholdincome2019_x+
                             Medianhouseholdincome2020+MedianhouseholdincomeApril2022+ 
                             Disability2019_x+Disability2022+DisabilityApril2022+
                             Lifeexpectancy2019_x+Lifeexpectancy2020+LifeexpectancyApril2022+ 
                             Prematureageadjustedmortality20+Prematureageadjustedmortality_1+
                             PrematureageadjustedmortalityAp+Republicanpercent+DemocraticPercent+
                             Homeownership2019_x+Homeownership2020+HomeownershipApril2022+
                             Povertyrat_2019+Povertyrat2020+PovertyratApril2022+ 
                             Severehousingcostburden2019_x+Severehousingcostburden2020+
                             SeverehousingcostburdenApril202+ResidentialsegregationBlackWhit+
                             ResidentialsegregationBlackWh_1+ResidentialsegregationBlackWh_2+
                             Rural2010_x+Rural2010_1+RuralApril2022+
                             Unemployment2019_x*Trafficvolume2019_x+ 
                             Unemployment2020*Trafficvolume2020+
                             UnemploymentApril2022*TrafficvolumeApril2022+
                             Childreninpoverty2019_x*Childreninsingleparenthousehold+
                             Childreninpoverty2020*Childreninsingleparenthouseho_1+                   ChildreninpovertyApril2022*Childreninsingleparenthouseho_2+ 
                             Childreninpoverty2019_x*Medianhouseholdincome2019_x+
                             Childreninpoverty2020*Medianhouseholdincome2020+
                             ChildreninpovertyApril2022*MedianhouseholdincomeApril2022+
                             Republicanpercent*Trafficvolume2019_x+
                             Republicanpercent*Trafficvolume2020+
                             Republicanpercent*TrafficvolumeApril2022+
                             Republicanpercent*Homeownership2019_x+
                             Republicanpercent*Homeownership2020+
                             Republicanpercent*HomeownershipApril2022+
                             Republicanpercent*Severehousingcostburden2019_x+
                             Republicanpercent*Severehousingcostburden2020+
                             Republicanpercent*SeverehousingcostburdenApril202+
                             Republicanpercent*older65over2019_x+
                             Republicanpercent*older65over2020+
                             Republicanpercent*older65overApril2022+
                             Republicanpercent*below18yearsofage2019_x+ 
                             Republicanpercent*below18yearsofage2020+
                             Republicanpercent*below18yearsofageApril2022+ 
                             Republicanpercent*ResidentialsegregationBlackWhit+ 
                             Republicanpercent*ResidentialsegregationBlackWh_1+ 
                             Republicanpercent*ResidentialsegregationBlackWh_2+
                             Republicanpercent*Rural2010_x+
                             Republicanpercent*Rural2010_1+ 
                             Republicanpercent*RuralApril2022,
                           data = df)
summary (model_xd_interaction)
# Calculate the percent of variation explained
total_variation <- var (df$Series_Complete_Pop_Pct_x)
explained_variation <- 1 - (var (model_xd$residuals) / total_variation)

# Print the percent of variation explained
cat ("Percent of variation explained by demographics, social determinants,
      health of residents and political leaning of the populatio including interaction items:", 
     round(explained_variation * 100, 2), "%\n")
# Percent of variation explained by demographics, social determinants, health of residents
# and political leaning of the population including interaction items: 50.9 %

# Calculate the percent of variation explained
total_variation <- var (df$Series_Complete_Pop_Pct_x)
explained_variation <- 1 - (var (model_xd_interaction$residuals) / total_variation)

# Print the percent of variation explained
cat ("Percent of variation explained by demographics, social determinants,
      health of residents and political leaning of the populatio including interaction items:", 
     round(explained_variation * 100, 2), "%\n")
# Percent of variation explained by demographics, social determinants, health of residents
# and political leaning of the population including interaction items: 52.49 %


#########
# Republicanpercent: Estimate = 57.80, t value = 1.440, Pr(t) = 0.150
# DemocraticPercent: Estimate = 18.24, t value = 2.485, Pr(t) = 0.013
#########
#Given the p-values, DemocraticPercent appears to be statistically significant (p < 0.05), 
# suggesting that there is evidence to reject the null hypothesis that the coefficient
# for DemocraticPercent is zero. In other words, there is evidence to suggest that
# a county's Democratic leaning is associated with changes in the vaccination rate.

# On the other hand, the p-value for Republicanpercent is 0.150, which is greater than 0.05. 
#While the coefficient is positive, suggesting a positive association,
# the evidence for the association is not strong enough to reject the null hypothesis.
# Q5 ## In conclusion, based on the results of the regression, there is some evidence to
# suggest that a county's political leaning, specifically leaning towards Democratic,
# is associated with changes in the COVID-19 vaccination rate. However,
# the association with Republican leaning is not statistically significant at the
# conventional significance level (p > 0.05).	







