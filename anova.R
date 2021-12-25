library(lmtest)

anova <- function(ind_var, ind_var_name, dep_var, dep_var_name){
  df <- data.frame(ind_var, dep_var)
  # remove states with only one observation
  df <- df[ave(rep(1, nrow(df)), ind_var, FUN=length)>1,]
  boxplot(dep_var~ind_var, data=df, xlab = ind_var_name, ylab = dep_var_name)
  
  anova_data <- aov(dep_var~ind_var, data=df)
  summary(anova_data)
  
  res <- anova_data$residuals
  
  # Verification of the assumptions
  shapiro.test(res)
  bartlett.test(res, df$ind_var)
  dwtest(unemployed.anova)
}

data <- read.csv(file = 'CommViolPredUnnormalizedData.csv')

states <- data[["state"]]

# Encode states name to numbers
states_set <- unique(states)
numbers <- seq(1:length(states_set))
names(numbers) <- states_set
states_map <-c()
for (item in states) {
  if (item %in% names(numbers)) {
    states_map <- append(states_map, numbers[item])
  }
}


## Variables

# medIncome: Median Household Income
medIncome <- data[["medIncome"]]

df_income <- data.frame(states_map, medIncome)
df_income <- df_income[ave(rep(1, nrow(df_income)), df_income$states_map, FUN=length)>1,]
boxplot(medIncome~states_map, data=df_income, xlab = "States", ylab = "Median household income")

medIncome.anova <- aov(medIncome~states_map, data=df_income)
summary(medIncome.anova)

res <- medIncome.anova$residuals

# Verification of the assumptions
shapiro.test(res)
bartlett.test(res, df_income$states_map)
dwtest(medIncome.anova)

# PctPopUnderPov: Percentage of people under the poverty level
popUnderPov <- data[["PctPopUnderPov"]]
df_popPov <- data.frame(states_map, popUnderPov)
df_popPov <- df_popPov[ave(rep(1, nrow(df_popPov)), df_popPov$states_map, FUN=length)>1,]
boxplot(popUnderPov~states_map, data=df_popPov, xlab = "States", ylab = "People percentage under poverty")

popUnderPov.anova <- aov(popUnderPov~states_map, data=df_popPov)
summary(popUnderPov.anova)

res <- popUnderPov.anova$residuals

# Verification of the assumptions
shapiro.test(res)
bartlett.test(res, df_popPov$states_map)
dwtest(popUnderPov.anova)

# PctUnemployed: Percentage of people 16 and over, in the labor force, and unemployed
unemployed <- data[["PctUnemployed"]]
df_unemp <- data.frame(states_map, unemployed)
df_unemp <- df_unemp[ave(rep(1, nrow(df_unemp)), df_unemp$states_map, FUN=length)>1,]
boxplot(unemployed~states_map, data=df_unemp, xlab = "States", ylab = "Percentage of unemployed people")

unemployed.anova <- aov(unemployed~states_map, data=df_unemp)
summary(unemployed.anova)

res <- unemployed.anova$residuals

# Verification of the assumptions
shapiro.test(res)
bartlett.test(res, df_unemp$states_map)
dwtest(unemployed.anova)

# ViolentCrimesPerPop: Violent crimes per population
vCrimes <- data[["ViolentCrimesPerPop"]]
df_vc <- data.frame(states_map, vCrimes)
df_vc <- subset(df_vc, df_vc$vCrimes!="?")
df_vc <- df_vc[ave(rep(1, nrow(df_vc)), df_vc$states_map, FUN=length)>1,]
df_vc$vCrimes <- as.numeric(as.character(df_vc$vCrimes))
boxplot(vCrimes~states_map, data=df_vc, xlab = "States", ylab = "Violents Crimes per Population")

vCrimes.anova <- aov(vCrimes~states_map, data=df_vc)
summary(vCrimes.anova)

res <- vCrimes.anova$residuals

# Verification of the assumptions
shapiro.test(res)
bartlett.test(res, df_vc$states_map)
dwtest(vCrimes.anova)

# nonViolPerPop: Non violent crimes per population
nvCrimes <- data[["nonViolPerPop"]]
df_nvc <- data.frame(states_map, nvCrimes)
df_nvc <- subset(df_nvc, df_nvc$nvCrimes!="?")
df_nvc <- df_nvc[ave(rep(1, nrow(df_nvc)), df_nvc$states_map, FUN=length)>1,]
df_nvc$nvCrimes <- as.numeric(as.character(df_nvc$nvCrimes))
boxplot(nvCrimes~states_map, data=df_nvc, xlab = "States", ylab = "Non Violents Crimes per Population")

nvCrimes.anova <- aov(nvCrimes~states_map, data=df_nvc)
summary(nvCrimes.anova)

res <- nvCrimes.anova$residuals

# Verification of the assumptions
shapiro.test(res)
bartlett.test(res, df_nvc$states_map)
dwtest(nvCrimes.anova)