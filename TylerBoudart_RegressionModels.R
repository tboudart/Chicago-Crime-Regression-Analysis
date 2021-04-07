library(tidyr)
library(dplyr)
library(writexl)
library(magrittr)

summary(table)
summary(Final.Table_v6)

cor(Final.Table_v6)
plot(Final.Table_v6$Number.Of.Battery)
plot(Final.Table_v6$Number.Of.Robbery)

table <- Final.Table_v6 %>%
  select(c(-1))

### Milestone 1 ###

tableB <- table %>%
  mutate(Month = as.factor(Month), 
         Weekday = as.factor(Weekday..Monday...1.), 
         Average.TempatureSQ = (Average.Tempature)^2, 
         Daily.PrecipitationSQ = (Daily.Precipitation)^2, 
         Unemployment_totalSQ = (Unemployment_total)^2, 
         Unemployment_males_20_and_olderSQ = (Unemployment_males_20_and_older)^2, 
         Unemployment_females_20_and_olderSQ = (Unemployment_females_20_and_older)^2, 
         Unemployment_16_to_19_yrsSQ = (Unemployment_16_to_19_yrs)^2, 
         Unemployment_whiteSQ = (Unemployment_white)^2, 
         Unemployment_blackSQ = (Unemployment_black)^2, 
         Unemployment_asianSQ = (Unemployment_asian)^2, 
         Unemployment_hispanicSQ = (Unemployment_hispanic)^2,
         Average.TempatureDaily.Precipitation = Average.Tempature*Daily.Precipitation,
         Average.TempatureUnemployment_total = Average.Tempature*Unemployment_total,
         Average.TempatureUnemployment_males_20_and_older = Average.Tempature*Unemployment_males_20_and_older,
         Average.TempatureUnemployment_females_20_and_older = Average.Tempature* Unemployment_females_20_and_older,
         Average.TempatureUnemployment_16_to_19_yrs = Average.Tempature*Unemployment_16_to_19_yrs,
         Average.TempatureUnemployment_white = Average.Tempature*Unemployment_white,
         Average.TempatureUnemployment_black = Average.Tempature*Unemployment_black,
         Average.TempatureUnemployment_asian = Average.Tempature*Unemployment_asian,
         Average.TempatureUnemployment_hispanic = Average.Tempature*Unemployment_hispanic,
         Daily.PrecipitationUnemployment_total = Daily.Precipitation*Unemployment_total,
         Daily.PrecipitationUnemployment_males_20_and_older = Daily.Precipitation*Unemployment_males_20_and_older,
         Daily.PrecipitationUnemployment_females_20_and_older = Daily.Precipitation* Unemployment_females_20_and_older,
         Daily.PrecipitationUnemployment_16_to_19_yrs = Daily.Precipitation*Unemployment_16_to_19_yrs,
         Daily.PrecipitationUnemployment_white = Daily.Precipitation*Unemployment_white,
         Daily.PrecipitationUnemployment_black = Daily.Precipitation*Unemployment_black,
         Daily.PrecipitationUnemployment_asian = Daily.Precipitation*Unemployment_asian,
         Daily.PrecipitationUnemployment_hispanic = Daily.Precipitation*Unemployment_hispanic,
         Unemployment_totalUnemployment_males_20_and_older = Unemployment_total*Unemployment_males_20_and_older,
         Unemployment_totalUnemployment_females_20_and_older = Unemployment_total* Unemployment_females_20_and_older,
         Unemployment_totalUnemployment_16_to_19_yrs = Unemployment_total*Unemployment_16_to_19_yrs,
         Unemployment_totalUnemployment_white = Unemployment_total*Unemployment_white,
         Unemployment_totalUnemployment_black = Unemployment_total*Unemployment_black,
         Unemployment_totalUnemployment_asian = Unemployment_total*Unemployment_asian,
         Unemployment_totalUnemployment_hispanic = Unemployment_total*Unemployment_hispanic,
         Unemployment_males_20_and_olderUnemployment_females_20_and_older = Unemployment_males_20_and_older* Unemployment_females_20_and_older,
         Unemployment_males_20_and_olderUnemployment_16_to_19_yrs = Unemployment_males_20_and_older*Unemployment_16_to_19_yrs,
         Unemployment_males_20_and_olderUnemployment_white = Unemployment_males_20_and_older*Unemployment_white,
         Unemployment_males_20_and_olderUnemployment_black = Unemployment_males_20_and_older*Unemployment_black,
         Unemployment_males_20_and_olderUnemployment_asian = Unemployment_males_20_and_older*Unemployment_asian,
         Unemployment_males_20_and_olderUnemployment_hispanic = Unemployment_males_20_and_older*Unemployment_hispanic,
         Unemployment_females_20_and_olderUnemployment_16_to_19_yrs =  Unemployment_females_20_and_older*Unemployment_16_to_19_yrs,
         Unemployment_females_20_and_olderUnemployment_white =  Unemployment_females_20_and_older*Unemployment_white,
         Unemployment_females_20_and_olderUnemployment_black =  Unemployment_females_20_and_older*Unemployment_black,
         Unemployment_females_20_and_olderUnemployment_asian =  Unemployment_females_20_and_older*Unemployment_asian,
         Unemployment_females_20_and_olderUnemployment_hispanic =  Unemployment_females_20_and_older*Unemployment_hispanic,
         Unemployment_16_to_19_yrsUnemployment_white = Unemployment_16_to_19_yrs*Unemployment_white,
         Unemployment_16_to_19_yrsUnemployment_black = Unemployment_16_to_19_yrs*Unemployment_black,
         Unemployment_16_to_19_yrsUnemployment_asian = Unemployment_16_to_19_yrs*Unemployment_asian,
         Unemployment_16_to_19_yrsUnemployment_hispanic = Unemployment_16_to_19_yrs*Unemployment_hispanic,
         Unemployment_whiteUnemployment_black = Unemployment_white*Unemployment_black,
         Unemployment_whiteUnemployment_asian = Unemployment_white*Unemployment_asian,
         Unemployment_whiteUnemployment_hispanic = Unemployment_white*Unemployment_hispanic,
         Unemployment_blackUnemployment_asian = Unemployment_black*Unemployment_asian,
         Unemployment_blackUnemployment_hispanic = Unemployment_black*Unemployment_hispanic,
         Unemployment_asianUnemployment_hispanic = Unemployment_asian*Unemployment_hispanic) %>%
  select(-c(2,4,5,6,7,8,9,10,12,13,14,15,16,17,18,19,20,21)) 

tableB$NY <- ifelse(tableB$New.Year.s.Day == 1 & tableB$Month == 1, 1, 0)
tableB$NY[2] <- 0

my_data <- Final.Table[-1]
corrlations <- data.frame(cor(my_data))
write_xlsx(corrlations,"C:\\Users\\t.boudart\\Documents\\2_Personal\\DePaul\\DSC 423 - Data Analysis and Regression\\Project\\correlations.xlsx")
write_xlsx(fiveNumberSum,"C:\\Users\\t.boudart\\Documents\\2_Personal\\DePaul\\DSC 423 - Data Analysis and Regression\\Project\\fiveNumberSum.xlsx")


library(DAAG)
library(MASS)
library(car)

model_empty <- lm(Number.Of.B ~1, data = table)
model_full <- lm(Number.Of.Battery ~., data = table)


step1 <- stepAIC(model_empty, direction = "forward", scope=list(upper=model_full, lower=model_empty))
step1$anova  # display results

step2 <- stepAIC(model_full, direction = "backward")
step2$anova  # display results

plot(table$Number.Of.Battery, table$Month)
plot(table$Number.Of.Battery, table$Weekday..Monday...1.)
plot(table$Number.Of.Battery, table$Year)

model <- lm(Number.Of.Battery ~ ArrestsAverage.Tempature + Weekday + Arrest.RateAverage.Tempature + 
              Month + Average.Tempature + New.Year.s.Day + Unemployment_hispanicSQ + 
              Arrests + ArrestsArrest.Rate + Christmas.Day + Average.TempatureSQ + 
              Daily.Precipitation + Number.Of.Level.1.Events + Independence.Day + 
              Memorial.Day + Arrest.Rate + Number.Of.Athletic.Training.Events + 
              Thanksgiving.Day + Average.TempatureUnemployment_females_20_and_older + 
              ArrestsUnemployment_females_20_and_older + Arrest.RateUnemployment_total + 
              Rain. + Labor.Day + Number.Of.Festival.Performance.12.001..Events + 
              Number.Of.Level.3.Events + Arrest.RateSQ + Unemployment_white + 
              Unemployment_whiteUnemployment_black + Number.Of.Promotions.Events + 
              Number.Of.Level.5.Events + Number.Of.Media..Other.Events + 
              Arrest.RateDaily.Precipitation + ArrestsUnemployment_hispanic + 
              Arrest.RateUnemployment_black + Arrest.RateUnemployment_females_20_and_older + 
              ArrestsUnemployment_total + ArrestsUnemployment_black + Number.Of.Media..Filming.Events + 
              Average.TempatureUnemployment_black + Daily.PrecipitationSQ, data = table)
summary(model)
vif(model)

model2 <- lm(Number.Of.Battery ~ Month + Arrests + Arrest.Rate + Average.Tempature + 
               Daily.Precipitation + Rain. + Number.Of.Level.1.Events + 
               Number.Of.Level.3.Events + Number.Of.Level.4.Events + Number.Of.Level.5.Events + 
               Number.Of.Athletic.Training.Events + Number.Of.Festival.Performance.12.001..Events + 
               Number.Of.Media..Filming.Events + Number.Of.Media..Other.Events + 
               Number.Of.Promotions.Events + Unemployment_males_20_and_older + 
               Unemployment_females_20_and_older + Unemployment_16_to_19_yrs + 
               Unemployment_white + Unemployment_black + Unemployment_asian + 
               Unemployment_hispanic + New.Year.s.Day + Memorial.Day + Independence.Day + 
               Labor.Day + Thanksgiving.Day + Christmas.Day + Weekday + 
               Arrest.RateSQ + Average.TempatureSQ + Daily.PrecipitationSQ + 
               Unemployment_totalSQ + Unemployment_females_20_and_olderSQ + 
               Unemployment_16_to_19_yrsSQ + Unemployment_whiteSQ + Unemployment_blackSQ + 
               ArrestsArrest.Rate + ArrestsUnemployment_total + ArrestsUnemployment_females_20_and_older + 
               ArrestsUnemployment_16_to_19_yrs + ArrestsUnemployment_black + 
               Arrest.RateAverage.Tempature + Arrest.RateDaily.Precipitation + 
               Arrest.RateUnemployment_total + Arrest.RateUnemployment_males_20_and_older + 
               Arrest.RateUnemployment_females_20_and_older + Arrest.RateUnemployment_16_to_19_yrs + 
               Arrest.RateUnemployment_white + Arrest.RateUnemployment_asian + 
               Average.TempatureUnemployment_males_20_and_older + Average.TempatureUnemployment_females_20_and_older + 
               Average.TempatureUnemployment_16_to_19_yrs + Average.TempatureUnemployment_white + 
               Average.TempatureUnemployment_asian + Daily.PrecipitationUnemployment_total + 
               Daily.PrecipitationUnemployment_males_20_and_older + Daily.PrecipitationUnemployment_16_to_19_yrs + 
               Daily.PrecipitationUnemployment_white + Daily.PrecipitationUnemployment_black + 
               Unemployment_totalUnemployment_males_20_and_older + Unemployment_totalUnemployment_females_20_and_older + 
               Unemployment_totalUnemployment_white + Unemployment_totalUnemployment_black + 
               Unemployment_males_20_and_olderUnemployment_females_20_and_older + 
               Unemployment_males_20_and_olderUnemployment_16_to_19_yrs + 
               Unemployment_males_20_and_olderUnemployment_white + Unemployment_males_20_and_olderUnemployment_black + 
               Unemployment_females_20_and_olderUnemployment_16_to_19_yrs + 
               Unemployment_females_20_and_olderUnemployment_asian + Unemployment_16_to_19_yrsUnemployment_white + 
               Unemployment_16_to_19_yrsUnemployment_black + Unemployment_16_to_19_yrsUnemployment_hispanic + 
               Unemployment_whiteUnemployment_black + Unemployment_whiteUnemployment_asian + 
               Unemployment_whiteUnemployment_hispanic + Unemployment_blackUnemployment_hispanic, data = table)

summary(model2)
vif(model2)

#_________________________

# Model w/0 ArrestsUnemployment_total  


model <- lm(Number.Of.Battery ~ ArrestsAverage.Tempature + Weekday + Arrest.RateAverage.Tempature + 
              Month + Average.Tempature + New.Year.s.Day + Unemployment_hispanicSQ + 
              Arrests + ArrestsArrest.Rate + Christmas.Day + Average.TempatureSQ + 
              Daily.Precipitation + Number.Of.Level.1.Events + Independence.Day + 
              Memorial.Day + Arrest.Rate + Number.Of.Athletic.Training.Events + 
              Thanksgiving.Day + Average.TempatureUnemployment_females_20_and_older + 
              ArrestsUnemployment_females_20_and_older + Arrest.RateUnemployment_total + 
              Rain. + Labor.Day + Number.Of.Festival.Performance.12.001..Events + 
              Number.Of.Level.3.Events + Arrest.RateSQ + Unemployment_white + 
              Unemployment_whiteUnemployment_black + Number.Of.Promotions.Events + 
              Number.Of.Level.5.Events + Number.Of.Media..Other.Events + 
              Arrest.RateDaily.Precipitation + ArrestsUnemployment_hispanic + 
              Arrest.RateUnemployment_black + Arrest.RateUnemployment_females_20_and_older + 
              ArrestsUnemployment_black + Number.Of.Media..Filming.Events + 
              Average.TempatureUnemployment_black + Daily.PrecipitationSQ, data = table)
summary(model)
vif(model)

#_______________________________


# Model w/0 ArrestsUnemployment_black
model <- lm(Number.Of.Battery ~ ArrestsAverage.Tempature + Weekday + Arrest.RateAverage.Tempature + 
              Month + Average.Tempature + New.Year.s.Day + Unemployment_hispanicSQ + 
              Arrests + ArrestsArrest.Rate + Christmas.Day + Average.TempatureSQ + 
              Daily.Precipitation + Number.Of.Level.1.Events + Independence.Day + 
              Memorial.Day + Arrest.Rate + Number.Of.Athletic.Training.Events + 
              Thanksgiving.Day + Average.TempatureUnemployment_females_20_and_older + 
              ArrestsUnemployment_females_20_and_older + Arrest.RateUnemployment_total + 
              Rain. + Labor.Day + Number.Of.Festival.Performance.12.001..Events + 
              Number.Of.Level.3.Events + Arrest.RateSQ + Unemployment_white + 
              Unemployment_whiteUnemployment_black + Number.Of.Promotions.Events + 
              Number.Of.Level.5.Events + Number.Of.Media..Other.Events + 
              Arrest.RateDaily.Precipitation + ArrestsUnemployment_hispanic + 
              Arrest.RateUnemployment_black + Arrest.RateUnemployment_females_20_and_older + 
              Number.Of.Media..Filming.Events + 
              Average.TempatureUnemployment_black + Daily.PrecipitationSQ, data = table)
summary(model)
vif(model)

#_______________________________

# Model w/0 Arrest.RateUnemployment_females_20_and_older
model <- lm(Number.Of.Battery ~ ArrestsAverage.Tempature + Weekday + Arrest.RateAverage.Tempature + 
              Month + Average.Tempature + New.Year.s.Day + Unemployment_hispanicSQ + 
              Arrests + ArrestsArrest.Rate + Christmas.Day + Average.TempatureSQ + 
              Daily.Precipitation + Number.Of.Level.1.Events + Independence.Day + 
              Memorial.Day + Arrest.Rate + Number.Of.Athletic.Training.Events + 
              Thanksgiving.Day + Average.TempatureUnemployment_females_20_and_older + 
              ArrestsUnemployment_females_20_and_older + Arrest.RateUnemployment_total + 
              Rain. + Labor.Day + Number.Of.Festival.Performance.12.001..Events + 
              Number.Of.Level.3.Events + Arrest.RateSQ + Unemployment_white + 
              Unemployment_whiteUnemployment_black + Number.Of.Promotions.Events + 
              Number.Of.Level.5.Events + Number.Of.Media..Other.Events + 
              Arrest.RateDaily.Precipitation + ArrestsUnemployment_hispanic + 
              Arrest.RateUnemployment_black + 
              Number.Of.Media..Filming.Events + 
              Average.TempatureUnemployment_black + Daily.PrecipitationSQ, data = table)
summary(model)
vif(model)



# Model w/0 Arrest.RateUnemployment_total (Choosen)
model <- lm(Number.Of.Battery ~ ArrestsAverage.Tempature + Weekday + Arrest.RateAverage.Tempature + 
              Month + Average.Tempature + New.Year.s.Day + Unemployment_hispanicSQ + 
              Arrests + ArrestsArrest.Rate + Christmas.Day + Average.TempatureSQ + 
              Daily.Precipitation + Number.Of.Level.1.Events + Independence.Day + 
              Memorial.Day + Arrest.Rate + Number.Of.Athletic.Training.Events + 
              Thanksgiving.Day + Average.TempatureUnemployment_females_20_and_older + 
              ArrestsUnemployment_females_20_and_older + 
              Rain. + Labor.Day + Number.Of.Festival.Performance.12.001..Events + 
              Number.Of.Level.3.Events + Arrest.RateSQ + Unemployment_white + 
              Unemployment_whiteUnemployment_black + Number.Of.Promotions.Events + 
              Number.Of.Level.5.Events + Number.Of.Media..Other.Events + 
              Arrest.RateDaily.Precipitation + ArrestsUnemployment_hispanic + 
              Arrest.RateUnemployment_black + Arrest.RateUnemployment_females_20_and_older + 
              Number.Of.Media..Filming.Events + 
              Average.TempatureUnemployment_black + Daily.PrecipitationSQ, data = table)
summary(model)
vif(model)

#_________________________________

# Model w/0 ArrestsUnemployment_females_20_and_older
model <- lm(Number.Of.Battery ~ ArrestsAverage.Tempature + Weekday + Arrest.RateAverage.Tempature + 
              Month + Average.Tempature + New.Year.s.Day + Unemployment_hispanicSQ + 
              Arrests + ArrestsArrest.Rate + Christmas.Day + Average.TempatureSQ + 
              Daily.Precipitation + Number.Of.Level.1.Events + Independence.Day + 
              Memorial.Day + Arrest.Rate + Number.Of.Athletic.Training.Events + 
              Thanksgiving.Day + Average.TempatureUnemployment_females_20_and_older + 
              Rain. + Labor.Day + Number.Of.Festival.Performance.12.001..Events + 
              Number.Of.Level.3.Events + Arrest.RateSQ + Unemployment_white + 
              Unemployment_whiteUnemployment_black + Number.Of.Promotions.Events + 
              Number.Of.Level.5.Events + Number.Of.Media..Other.Events + 
              Arrest.RateDaily.Precipitation + ArrestsUnemployment_hispanic + 
              Arrest.RateUnemployment_black + Arrest.RateUnemployment_females_20_and_older + 
              Number.Of.Media..Filming.Events + 
              Average.TempatureUnemployment_black + Daily.PrecipitationSQ, data = table)
summary(model)
vif(model)


# Model w/0 Unemployment_whiteUnemployment_black (Choosen)
model <- lm(Number.Of.Battery ~ ArrestsAverage.Tempature + Weekday + Arrest.RateAverage.Tempature + 
              Month + Average.Tempature + New.Year.s.Day + Unemployment_hispanicSQ + 
              Arrests + ArrestsArrest.Rate + Christmas.Day + Average.TempatureSQ + 
              Daily.Precipitation + Number.Of.Level.1.Events + Independence.Day + 
              Memorial.Day + Arrest.Rate + Number.Of.Athletic.Training.Events + 
              Thanksgiving.Day + Average.TempatureUnemployment_females_20_and_older + 
              ArrestsUnemployment_females_20_and_older + 
              Rain. + Labor.Day + Number.Of.Festival.Performance.12.001..Events + 
              Number.Of.Level.3.Events + Arrest.RateSQ + Unemployment_white + 
              Number.Of.Promotions.Events + 
              Number.Of.Level.5.Events + Number.Of.Media..Other.Events + 
              Arrest.RateDaily.Precipitation + ArrestsUnemployment_hispanic + 
              Arrest.RateUnemployment_black + Arrest.RateUnemployment_females_20_and_older + 
              Number.Of.Media..Filming.Events + 
              Average.TempatureUnemployment_black + Daily.PrecipitationSQ, data = table)
summary(model)
vif(model)

#_________________________________

# Model w/0 Arrest.RateSQ 
model <- lm(Number.Of.Battery ~ ArrestsAverage.Tempature + Weekday + Arrest.RateAverage.Tempature + 
              Month + Average.Tempature + New.Year.s.Day + Unemployment_hispanicSQ + 
              Arrests + ArrestsArrest.Rate + Christmas.Day + Average.TempatureSQ + 
              Daily.Precipitation + Number.Of.Level.1.Events + Independence.Day + 
              Memorial.Day + Arrest.Rate + Number.Of.Athletic.Training.Events + 
              Thanksgiving.Day + Average.TempatureUnemployment_females_20_and_older + 
              ArrestsUnemployment_females_20_and_older + 
              Rain. + Labor.Day + Number.Of.Festival.Performance.12.001..Events + 
              Number.Of.Level.3.Events + Unemployment_white + 
              Number.Of.Promotions.Events + 
              Number.Of.Level.5.Events + Number.Of.Media..Other.Events + 
              Arrest.RateDaily.Precipitation + ArrestsUnemployment_hispanic + 
              Arrest.RateUnemployment_black + Arrest.RateUnemployment_females_20_and_older + 
              Number.Of.Media..Filming.Events + 
              Average.TempatureUnemployment_black + Daily.PrecipitationSQ, data = table)
summary(model)
vif(model)


#_________________________________

# Model w/0 ArrestsUnemployment_females_20_and_older
model <- lm(Number.Of.Battery ~ ArrestsAverage.Tempature + Weekday + Arrest.RateAverage.Tempature + 
              Month + Average.Tempature + New.Year.s.Day + Unemployment_hispanicSQ + 
              Arrests + ArrestsArrest.Rate + Christmas.Day + Average.TempatureSQ + 
              Daily.Precipitation + Number.Of.Level.1.Events + Independence.Day + 
              Memorial.Day + Arrest.Rate + Number.Of.Athletic.Training.Events + 
              Thanksgiving.Day + Average.TempatureUnemployment_females_20_and_older + 
              Rain. + Labor.Day + Number.Of.Festival.Performance.12.001..Events + 
              Number.Of.Level.3.Events + Unemployment_white + 
              Number.Of.Promotions.Events + 
              Number.Of.Level.5.Events + Number.Of.Media..Other.Events + 
              Arrest.RateDaily.Precipitation + ArrestsUnemployment_hispanic + 
              Arrest.RateUnemployment_black + Arrest.RateUnemployment_females_20_and_older + 
              Number.Of.Media..Filming.Events + 
              Average.TempatureUnemployment_black + Daily.PrecipitationSQ, data = table)
summary(model)
vif(model)

#_________________________________

# Model w/0 Arrest.RateUnemployment_black  
model <- lm(Number.Of.Battery ~ ArrestsAverage.Tempature + Weekday + Arrest.RateAverage.Tempature + 
              Month + Average.Tempature + New.Year.s.Day + Unemployment_hispanicSQ + 
              Arrests + ArrestsArrest.Rate + Christmas.Day + Average.TempatureSQ + 
              Daily.Precipitation + Number.Of.Level.1.Events + Independence.Day + 
              Memorial.Day + Arrest.Rate + Number.Of.Athletic.Training.Events + 
              Thanksgiving.Day + Average.TempatureUnemployment_females_20_and_older + 
              Rain. + Labor.Day + Number.Of.Festival.Performance.12.001..Events + 
              Number.Of.Level.3.Events + Unemployment_white + 
              Number.Of.Promotions.Events + 
              Number.Of.Level.5.Events + Number.Of.Media..Other.Events + 
              Arrest.RateDaily.Precipitation + ArrestsUnemployment_hispanic + 
              Arrest.RateUnemployment_females_20_and_older + 
              Number.Of.Media..Filming.Events + 
              Average.TempatureUnemployment_black + Daily.PrecipitationSQ, data = table)
summary(model)
vif(model)

#_________________________________

# Model w/0 Arrest.RateAverage.Tempature  
model <- lm(Number.Of.Battery ~ ArrestsAverage.Tempature + Weekday + 
              Month + Average.Tempature + New.Year.s.Day + Unemployment_hispanicSQ + 
              Arrests + ArrestsArrest.Rate + Christmas.Day + Average.TempatureSQ + 
              Daily.Precipitation + Number.Of.Level.1.Events + Independence.Day + 
              Memorial.Day + Arrest.Rate + Number.Of.Athletic.Training.Events + 
              Thanksgiving.Day + Average.TempatureUnemployment_females_20_and_older + 
              Rain. + Labor.Day + Number.Of.Festival.Performance.12.001..Events + 
              Number.Of.Level.3.Events + Unemployment_white + 
              Number.Of.Promotions.Events + 
              Number.Of.Level.5.Events + Number.Of.Media..Other.Events + 
              Arrest.RateDaily.Precipitation + ArrestsUnemployment_hispanic + 
              Arrest.RateUnemployment_females_20_and_older + 
              Number.Of.Media..Filming.Events + 
              Average.TempatureUnemployment_black + Daily.PrecipitationSQ, data = table)
summary(model)
vif(model)

# Model w/0 ArrestsAverage.Tempature (Choosen) 
model <- lm(Number.Of.Battery ~ Weekday + Arrest.RateAverage.Tempature + 
              Month + Average.Tempature + New.Year.s.Day + Unemployment_hispanicSQ + 
              Arrests + ArrestsArrest.Rate + Christmas.Day + Average.TempatureSQ + 
              Daily.Precipitation + Number.Of.Level.1.Events + Independence.Day + 
              Memorial.Day + Arrest.Rate + Number.Of.Athletic.Training.Events + 
              Thanksgiving.Day + Average.TempatureUnemployment_females_20_and_older + 
              Rain. + Labor.Day + Number.Of.Festival.Performance.12.001..Events + 
              Number.Of.Level.3.Events + Unemployment_white + 
              Number.Of.Promotions.Events + 
              Number.Of.Level.5.Events + Number.Of.Media..Other.Events + 
              Arrest.RateDaily.Precipitation + ArrestsUnemployment_hispanic + 
              Arrest.RateUnemployment_females_20_and_older + 
              Number.Of.Media..Filming.Events + 
              Average.TempatureUnemployment_black + Daily.PrecipitationSQ, data = table)
summary(model)
vif(model)

#_________________________________

# Model w/0 Average.TempatureUnemployment_females_20_and_older (Choosen)
model <- lm(Number.Of.Battery ~ Weekday + Arrest.RateAverage.Tempature + 
              Month + Average.Tempature + New.Year.s.Day + Unemployment_hispanicSQ + 
              Arrests + ArrestsArrest.Rate + Christmas.Day + Average.TempatureSQ + 
              Daily.Precipitation + Number.Of.Level.1.Events + Independence.Day + 
              Memorial.Day + Arrest.Rate + Number.Of.Athletic.Training.Events + 
              Thanksgiving.Day + 
              Rain. + Labor.Day + Number.Of.Festival.Performance.12.001..Events + 
              Number.Of.Level.3.Events + Unemployment_white + 
              Number.Of.Promotions.Events + 
              Number.Of.Level.5.Events + Number.Of.Media..Other.Events + 
              Arrest.RateDaily.Precipitation + ArrestsUnemployment_hispanic + 
              Arrest.RateUnemployment_females_20_and_older + 
              Number.Of.Media..Filming.Events + 
              Average.TempatureUnemployment_black + Daily.PrecipitationSQ, data = table)
summary(model)
vif(model)


# Model w/0 Average.TempatureUnemployment_black
model <- lm(Number.Of.Battery ~ Weekday + Arrest.RateAverage.Tempature + 
              Month + Average.Tempature + New.Year.s.Day + Unemployment_hispanicSQ + 
              Arrests + ArrestsArrest.Rate + Christmas.Day + Average.TempatureSQ + 
              Daily.Precipitation + Number.Of.Level.1.Events + Independence.Day + 
              Memorial.Day + Arrest.Rate + Number.Of.Athletic.Training.Events + 
              Thanksgiving.Day + Average.TempatureUnemployment_females_20_and_older + 
              Rain. + Labor.Day + Number.Of.Festival.Performance.12.001..Events + 
              Number.Of.Level.3.Events + Unemployment_white + 
              Number.Of.Promotions.Events + 
              Number.Of.Level.5.Events + Number.Of.Media..Other.Events + 
              Arrest.RateDaily.Precipitation + ArrestsUnemployment_hispanic + 
              Arrest.RateUnemployment_females_20_and_older + 
              Number.Of.Media..Filming.Events + 
              Daily.PrecipitationSQ, data = table)
summary(model)
vif(model)


# Model w/0 Unemployment_white
model <- lm(Number.Of.Battery ~ Weekday + Arrest.RateAverage.Tempature + 
              Month + Average.Tempature + New.Year.s.Day + Unemployment_hispanicSQ + 
              Arrests + ArrestsArrest.Rate + Christmas.Day + Average.TempatureSQ + 
              Daily.Precipitation + Number.Of.Level.1.Events + Independence.Day + 
              Memorial.Day + Arrest.Rate + Number.Of.Athletic.Training.Events + 
              Thanksgiving.Day + Average.TempatureUnemployment_females_20_and_older + 
              Rain. + Labor.Day + Number.Of.Festival.Performance.12.001..Events + 
              Number.Of.Level.3.Events +
              Number.Of.Promotions.Events + 
              Number.Of.Level.5.Events + Number.Of.Media..Other.Events + 
              Arrest.RateDaily.Precipitation + ArrestsUnemployment_hispanic + 
              Arrest.RateUnemployment_females_20_and_older + 
              Number.Of.Media..Filming.Events + 
              Average.TempatureUnemployment_black + Daily.PrecipitationSQ, data = table)
summary(model)
vif(model)


#_________________________________
# Model w/0 Unemployment_white
model <- lm(Number.Of.Battery ~ Weekday + Arrest.RateAverage.Tempature + 
              Month + Average.Tempature + New.Year.s.Day + Unemployment_hispanicSQ + 
              Arrests + ArrestsArrest.Rate + Christmas.Day + Average.TempatureSQ + 
              Daily.Precipitation + Number.Of.Level.1.Events + Independence.Day + 
              Memorial.Day + Arrest.Rate + Number.Of.Athletic.Training.Events + 
              Thanksgiving.Day + 
              Rain. + Labor.Day + Number.Of.Festival.Performance.12.001..Events + 
              Number.Of.Level.3.Events + 
              Number.Of.Promotions.Events + 
              Number.Of.Level.5.Events + Number.Of.Media..Other.Events + 
              Arrest.RateDaily.Precipitation + ArrestsUnemployment_hispanic + 
              Arrest.RateUnemployment_females_20_and_older + 
              Number.Of.Media..Filming.Events + 
              Average.TempatureUnemployment_black + Daily.PrecipitationSQ, data = table)
summary(model)
vif(model)

#_________________________________
# Model w/0 Number.Of.Media..Filming.Events 
model <- lm(Number.Of.Battery ~ Weekday + Arrest.RateAverage.Tempature + 
              Month + Average.Tempature + New.Year.s.Day + Unemployment_hispanicSQ + 
              Arrests + ArrestsArrest.Rate + Christmas.Day + Average.TempatureSQ + 
              Daily.Precipitation + Number.Of.Level.1.Events + Independence.Day + 
              Memorial.Day + Arrest.Rate + Number.Of.Athletic.Training.Events + 
              Thanksgiving.Day + 
              Rain. + Labor.Day + Number.Of.Festival.Performance.12.001..Events + 
              Number.Of.Level.3.Events + 
              Number.Of.Promotions.Events + 
              Number.Of.Level.5.Events + Number.Of.Media..Other.Events + 
              Arrest.RateDaily.Precipitation + ArrestsUnemployment_hispanic + 
              Arrest.RateUnemployment_females_20_and_older + 
              Average.TempatureUnemployment_black + Daily.PrecipitationSQ, data = table)
summary(model)
vif(model)


#_________________________________
# Model w/0 Number.Of.Media..Other.Events 
model <- lm(Number.Of.Battery ~ Weekday + Arrest.RateAverage.Tempature + 
              Month + Average.Tempature + New.Year.s.Day + Unemployment_hispanicSQ + 
              Arrests + ArrestsArrest.Rate + Christmas.Day + Average.TempatureSQ + 
              Daily.Precipitation + Number.Of.Level.1.Events + Independence.Day + 
              Memorial.Day + Arrest.Rate + Number.Of.Athletic.Training.Events + 
              Thanksgiving.Day + 
              Rain. + Labor.Day + Number.Of.Festival.Performance.12.001..Events + 
              Number.Of.Level.3.Events + 
              Number.Of.Promotions.Events + 
              Number.Of.Level.5.Events + 
              Arrest.RateDaily.Precipitation + ArrestsUnemployment_hispanic + 
              Arrest.RateUnemployment_females_20_and_older + 
              Average.TempatureUnemployment_black + Daily.PrecipitationSQ, data = table)
summary(model)
vif(model)



#_________________________________
# Model w/0 Arrest.RateUnemployment_females_20_and_older
model <- lm(Number.Of.Battery ~ Weekday + Arrest.RateAverage.Tempature + 
              Month + Average.Tempature + New.Year.s.Day + Unemployment_hispanicSQ + 
              Arrests + ArrestsArrest.Rate + Christmas.Day + Average.TempatureSQ + 
              Daily.Precipitation + Number.Of.Level.1.Events + Independence.Day + 
              Memorial.Day + Arrest.Rate + Number.Of.Athletic.Training.Events + 
              Thanksgiving.Day + 
              Rain. + Labor.Day + Number.Of.Festival.Performance.12.001..Events + 
              Number.Of.Level.3.Events + 
              Number.Of.Promotions.Events + 
              Number.Of.Level.5.Events + 
              Arrest.RateDaily.Precipitation + ArrestsUnemployment_hispanic + 
              Average.TempatureUnemployment_black + Daily.PrecipitationSQ, data = table)
summary(model)
vif(model)

#_________________________________
# Model w/0 Daily.PrecipitationSQ
model <- lm(Number.Of.Battery ~ Weekday + Arrest.RateAverage.Tempature + 
              Month + Average.Tempature + New.Year.s.Day + Unemployment_hispanicSQ + 
              Arrests + ArrestsArrest.Rate + Christmas.Day + Average.TempatureSQ + 
              Daily.Precipitation + Number.Of.Level.1.Events + Independence.Day + 
              Memorial.Day + Arrest.Rate + Number.Of.Athletic.Training.Events + 
              Thanksgiving.Day + 
              Rain. + Labor.Day + Number.Of.Festival.Performance.12.001..Events + 
              Number.Of.Level.3.Events + 
              Number.Of.Promotions.Events + 
              Number.Of.Level.5.Events + 
              Arrest.RateDaily.Precipitation + ArrestsUnemployment_hispanic + 
              Average.TempatureUnemployment_black, data = table)
summary(model)
vif(model)


#_________________________________
# Model w/0 Number.Of.Level.5.Events
model <- lm(Number.Of.Battery ~ Weekday + Arrest.RateAverage.Tempature + 
              Month + Average.Tempature + New.Year.s.Day + Unemployment_hispanicSQ + 
              Arrests + ArrestsArrest.Rate + Christmas.Day + Average.TempatureSQ + 
              Daily.Precipitation + Number.Of.Level.1.Events + Independence.Day + 
              Memorial.Day + Arrest.Rate + Number.Of.Athletic.Training.Events + 
              Thanksgiving.Day + 
              Rain. + Labor.Day + Number.Of.Festival.Performance.12.001..Events + 
              Number.Of.Level.3.Events + 
              Number.Of.Promotions.Events + 
              ArrestsUnemployment_hispanic + 
              Average.TempatureUnemployment_black, data = table)
summary(model)
vif(model)

#_________________________________
# Model w/0 Average.Tempature
model <- lm(Number.Of.Battery ~ Weekday + Arrest.RateAverage.Tempature + 
              Month + New.Year.s.Day + Unemployment_hispanicSQ + 
              Arrests + ArrestsArrest.Rate + Christmas.Day + Average.TempatureSQ + 
              Daily.Precipitation + Number.Of.Level.1.Events + Independence.Day + 
              Memorial.Day + Arrest.Rate + Number.Of.Athletic.Training.Events + 
              Thanksgiving.Day + 
              Rain. + Labor.Day + Number.Of.Festival.Performance.12.001..Events + 
              Number.Of.Level.3.Events + 
              Number.Of.Promotions.Events + 
              ArrestsUnemployment_hispanic + 
              Average.TempatureUnemployment_black, data = table)
summary(model)
vif(model)

#_________________________________
# Model w/0 Number.Of.Promotions.Events
model <- lm(Number.Of.Battery ~ Weekday + Arrest.RateAverage.Tempature + 
              Month + New.Year.s.Day + Unemployment_hispanicSQ + 
              Arrests + ArrestsArrest.Rate + Christmas.Day + Average.TempatureSQ + 
              Daily.Precipitation + Number.Of.Level.1.Events + Independence.Day + 
              Memorial.Day + Arrest.Rate + Number.Of.Athletic.Training.Events + 
              Thanksgiving.Day + 
              Rain. + Labor.Day + Number.Of.Festival.Performance.12.001..Events + 
              Number.Of.Level.3.Events + 
              ArrestsUnemployment_hispanic + 
              Average.TempatureUnemployment_black, data = table)
summary(model)
vif(model)

#_________________________________
# Model w/0 Number.Of.Level.3.Events
model <- lm(Number.Of.Battery ~ Weekday + Arrest.RateAverage.Tempature + 
              Month + New.Year.s.Day + Unemployment_hispanicSQ + 
              Arrests + ArrestsArrest.Rate + Christmas.Day + Average.TempatureSQ + 
              Daily.Precipitation + Number.Of.Level.1.Events + Independence.Day + 
              Memorial.Day + Arrest.Rate + Number.Of.Athletic.Training.Events + 
              Thanksgiving.Day + 
              Rain. + Labor.Day + Number.Of.Festival.Performance.12.001..Events + 
              ArrestsUnemployment_hispanic + 
              Average.TempatureUnemployment_black, data = table)
summary(model)
vif(model)

#_________________________________
# Model w/0 Number.Of.Festival.Performance.12.001..Events
model <- lm(Number.Of.Battery ~ Weekday + Arrest.RateAverage.Tempature + 
              Month + New.Year.s.Day + Unemployment_hispanicSQ + 
              Arrests + ArrestsArrest.Rate + Christmas.Day + Average.TempatureSQ + 
              Daily.Precipitation + Number.Of.Level.1.Events + Independence.Day + 
              Memorial.Day + Arrest.Rate + Number.Of.Athletic.Training.Events + 
              Thanksgiving.Day + 
              Rain. + Labor.Day + 
              ArrestsUnemployment_hispanic + 
              Average.TempatureUnemployment_black, data = table)
summary(model)
vif(model)

#_________________________________
# Model w/0 Labor Day
model <- lm(Number.Of.Battery ~ Weekday + Arrest.RateAverage.Tempature + 
              Month + New.Year.s.Day + Unemployment_hispanicSQ + 
              Arrests + ArrestsArrest.Rate + Christmas.Day + Average.TempatureSQ + 
              Daily.Precipitation + Number.Of.Level.1.Events + Independence.Day + 
              Memorial.Day + Arrest.Rate + Number.Of.Athletic.Training.Events + 
              Thanksgiving.Day + 
              Rain. + 
              ArrestsUnemployment_hispanic + 
              Average.TempatureUnemployment_black, data = table)
summary(model)
vif(model)


#_________________________________
# Tweak model and finalize


model <- lm(Number.Of.Battery ~ Weekday + Month +
              Arrest.Rate +
              Average.Tempature +
              Daily.Precipitation + 
              New.Year.s.Day + 
              Arrest.RateAverage.Tempature, data = table)
summary(model)
vif(model)




test2 <- table %>%
  select(c(Number.Of.Battery, Weekday,
             Unemployment_hispanic, 
             Unemployment_black,
             Arrest.Rate,
             Average.Tempature,
             Daily.Precipitation,
             New.Year.s.Day,
             Independence.Day, 
             Number.Of.Athletic.Training.Events,
             Arrest.RateAverage.Tempature))

plot(test2)

corrlations <- data.frame(cor(test2))
write_xlsx(corrlations,"C:\\Users\\t.boudart\\Documents\\2_Personal\\DePaul\\DSC 423 - Data Analysis and Regression\\Project\\correlations.xlsx")
write_xlsx(fiveNumberSum,"C:\\Users\\t.boudart\\Documents\\2_Personal\\DePaul\\DSC 423 - Data Analysis and Regression\\Project\\fiveNumberSum.xlsx")


###MORE TESTING

finalModel1 <- lm(log(Number.Of.Battery) ~ Weekday + Month +
  Average.Tempature + NY + 
  as.factor(Year), data=tableB)
summary(finalModel1)
vif(finalModel1)
plot(finalModel1)

finalModel2 <- lm(Number.Of.Battery ~ Weekday + Month +
                    Average.Tempature + NY +
                    as.factor(Year), data=tableB)
summary(finalModel2)
vif(finalModel2)
plot(finalModel2)

finalModel3 <- lm(log(Number.Of.Battery) ~ Weekday + Month +
                    Average.Tempature + New.Year.s.Day + 
                    as.factor(Year), data=tableB)
summary(finalModel3)
vif(finalModel3)
plot(finalModel3)


plot(tableB$Weekday, finalModel$residuals)
plot(tableB$Month, finalModel$residuals)
plot(tableB$New.Year.s.Day, finalModel$residuals)
plot(tableB$Daily.Precipitation, finalModel$residuals)
plot(tableB$Average.Tempature, finalModel$residuals)
plot(tableB$Unemployment_hispanic, finalModel$residuals)
plot(tableB$Unemployment_black, finalModel$residuals)
plot(as.factor(tableB$Year), finalModel$residuals)


## Testing if z score normalization on secord-order terms removes multicolleanrity


summary(table$ZUnemployment_blackSQ)
summary(table$ZUnemployment_hispanicSQ)
sd(table$Unemployment_hispanicSQ)


table$ZUnemployment_blackSQ <- (table$Unemployment_blackSQ - 103.88)/(61.61803)
table$ZUnemployment_hispanicSQ <- (table$Unemployment_hispanicSQ - 57.7)/(51.93597)


finalModel <- lm(Number.Of.Battery ~ Weekday + Month + New.Year.s.Day +
                   Daily.Precipitation + Average.Tempature + 
                   Unemployment_hispanic + Unemployment_black +  
                   ZUnemployment_blackSQ + 
                   ZUnemployment_hispanicSQ, data=table)
summary(finalModel)
vif(finalModel)





### Milestone 2 ###

tableR <- table %>%
  mutate(Month = as.factor(Month), 
         Weekday = as.factor(Weekday..Monday...1.), 
         Average.TempatureSQ = (Average.Tempature)^2, 
         Daily.PrecipitationSQ = (Daily.Precipitation)^2, 
         Unemployment_totalSQ = (Unemployment_total)^2, 
         Unemployment_males_20_and_olderSQ = (Unemployment_males_20_and_older)^2, 
         Unemployment_females_20_and_olderSQ = (Unemployment_females_20_and_older)^2, 
         Unemployment_16_to_19_yrsSQ = (Unemployment_16_to_19_yrs)^2, 
         Unemployment_whiteSQ = (Unemployment_white)^2, 
         Unemployment_blackSQ = (Unemployment_black)^2, 
         Unemployment_asianSQ = (Unemployment_asian)^2, 
         Unemployment_hispanicSQ = (Unemployment_hispanic)^2,
         Average.TempatureDaily.Precipitation = Average.Tempature*Daily.Precipitation,
         Average.TempatureUnemployment_total = Average.Tempature*Unemployment_total,
         Average.TempatureUnemployment_males_20_and_older = Average.Tempature*Unemployment_males_20_and_older,
         Average.TempatureUnemployment_females_20_and_older = Average.Tempature* Unemployment_females_20_and_older,
         Average.TempatureUnemployment_16_to_19_yrs = Average.Tempature*Unemployment_16_to_19_yrs,
         Average.TempatureUnemployment_white = Average.Tempature*Unemployment_white,
         Average.TempatureUnemployment_black = Average.Tempature*Unemployment_black,
         Average.TempatureUnemployment_asian = Average.Tempature*Unemployment_asian,
         Average.TempatureUnemployment_hispanic = Average.Tempature*Unemployment_hispanic,
         Daily.PrecipitationUnemployment_total = Daily.Precipitation*Unemployment_total,
         Daily.PrecipitationUnemployment_males_20_and_older = Daily.Precipitation*Unemployment_males_20_and_older,
         Daily.PrecipitationUnemployment_females_20_and_older = Daily.Precipitation* Unemployment_females_20_and_older,
         Daily.PrecipitationUnemployment_16_to_19_yrs = Daily.Precipitation*Unemployment_16_to_19_yrs,
         Daily.PrecipitationUnemployment_white = Daily.Precipitation*Unemployment_white,
         Daily.PrecipitationUnemployment_black = Daily.Precipitation*Unemployment_black,
         Daily.PrecipitationUnemployment_asian = Daily.Precipitation*Unemployment_asian,
         Daily.PrecipitationUnemployment_hispanic = Daily.Precipitation*Unemployment_hispanic,
         Unemployment_totalUnemployment_males_20_and_older = Unemployment_total*Unemployment_males_20_and_older,
         Unemployment_totalUnemployment_females_20_and_older = Unemployment_total* Unemployment_females_20_and_older,
         Unemployment_totalUnemployment_16_to_19_yrs = Unemployment_total*Unemployment_16_to_19_yrs,
         Unemployment_totalUnemployment_white = Unemployment_total*Unemployment_white,
         Unemployment_totalUnemployment_black = Unemployment_total*Unemployment_black,
         Unemployment_totalUnemployment_asian = Unemployment_total*Unemployment_asian,
         Unemployment_totalUnemployment_hispanic = Unemployment_total*Unemployment_hispanic,
         Unemployment_males_20_and_olderUnemployment_females_20_and_older = Unemployment_males_20_and_older* Unemployment_females_20_and_older,
         Unemployment_males_20_and_olderUnemployment_16_to_19_yrs = Unemployment_males_20_and_older*Unemployment_16_to_19_yrs,
         Unemployment_males_20_and_olderUnemployment_white = Unemployment_males_20_and_older*Unemployment_white,
         Unemployment_males_20_and_olderUnemployment_black = Unemployment_males_20_and_older*Unemployment_black,
         Unemployment_males_20_and_olderUnemployment_asian = Unemployment_males_20_and_older*Unemployment_asian,
         Unemployment_males_20_and_olderUnemployment_hispanic = Unemployment_males_20_and_older*Unemployment_hispanic,
         Unemployment_females_20_and_olderUnemployment_16_to_19_yrs =  Unemployment_females_20_and_older*Unemployment_16_to_19_yrs,
         Unemployment_females_20_and_olderUnemployment_white =  Unemployment_females_20_and_older*Unemployment_white,
         Unemployment_females_20_and_olderUnemployment_black =  Unemployment_females_20_and_older*Unemployment_black,
         Unemployment_females_20_and_olderUnemployment_asian =  Unemployment_females_20_and_older*Unemployment_asian,
         Unemployment_females_20_and_olderUnemployment_hispanic =  Unemployment_females_20_and_older*Unemployment_hispanic,
         Unemployment_16_to_19_yrsUnemployment_white = Unemployment_16_to_19_yrs*Unemployment_white,
         Unemployment_16_to_19_yrsUnemployment_black = Unemployment_16_to_19_yrs*Unemployment_black,
         Unemployment_16_to_19_yrsUnemployment_asian = Unemployment_16_to_19_yrs*Unemployment_asian,
         Unemployment_16_to_19_yrsUnemployment_hispanic = Unemployment_16_to_19_yrs*Unemployment_hispanic,
         Unemployment_whiteUnemployment_black = Unemployment_white*Unemployment_black,
         Unemployment_whiteUnemployment_asian = Unemployment_white*Unemployment_asian,
         Unemployment_whiteUnemployment_hispanic = Unemployment_white*Unemployment_hispanic,
         Unemployment_blackUnemployment_asian = Unemployment_black*Unemployment_asian,
         Unemployment_blackUnemployment_hispanic = Unemployment_black*Unemployment_hispanic,
         Unemployment_asianUnemployment_hispanic = Unemployment_asian*Unemployment_hispanic) %>%
  select(-c(2,4,5,6,7,8,9,10,11,12,13,14,15,16,17,19,20,21)) 

tableR$NY <- ifelse(tableR$New.Year.s.Day == 1 & tableR$Month == 1, 1, 0)
tableR$NY[2] <- 0


library(DAAG)
library(MASS)
library(car)


model_empty <- lm(Number.Of.Robbery ~1, data = tableR)
model_full <- lm(Number.Of.Robbery ~., data = tableR)


step1 <- stepAIC(model_empty, direction = "forward", scope=list(upper=model_full, lower=model_empty))
step1$anova  # display results

step2 <- stepAIC(model_full, direction = "backward")
step2$anova  # display results

model.R.1 <- lm(log(Number.Of.Robbery) ~ as.factor(Year) + Month + 
                  Average.Tempature, data = tableR)
summary(model.R.1)
vif(model.R.1)
plot(model.R.1)


plot(as.factor(tableR$Year), model.R.1$residuals)
plot(tableR$Month, model.R.1$residuals)
plot(tableR$Average.Tempature, model.R.1$residuals)

## MS 1 Cleaning ##
summary(tableB$Number.Of.Battery)
boxplot(tableB$Number.Of.Battery)
tableB$boxoutlier <- ifelse(tableB$Number.Of.Battery > 210 | tableB$Number.Of.Battery < 58,1,0)
tableB$z <- ((tableB$Number.Of.Battery - mean(tableB$Number.Of.Battery))/sd(tableB$Number.Of.Battery))
tableB$zoutlier <- ifelse(abs(tableB$z) > 3,1,0)
sum(tableB$boxoutlier)
sum(tableB$zoutlier)
tableBZ<-tableB[!(tableB$zoutlier == 1),]
tableBBox<-tableB[!(tableB$boxoutlier== 1),]
tableB3 <- tableB[-c(3036, 3025, 1829, 3027),]
tableBZ <- tableBZ %>% select(-c(102,103,104))
tableBBox <- tableBBox %>% select(-c(102,103,104))
tableB3 <- tableB3 %>% select(-c(102,103,104))
tableB <- tableB %>% select(-c(102,103,104))


finalModel1 <- lm(Number.Of.Battery ~ Weekday + Month +
                    Average.Tempature + NY + 
                    as.factor(Year), data=tableB)
summary(finalModel1)
plot(finalModel1)

finalModel2 <- lm(Number.Of.Battery ~ Weekday + Month +
                    Average.Tempature + NY + 
                    as.factor(Year), data=tableB3)
summary(finalModel2)
plot(finalModel2)

finalModel3 <- lm(log(Number.Of.Battery) ~ Weekday + Month +
                    Average.Tempature + NY + 
                    as.factor(Year), data=tableBBox)
summary(finalModel3)
plot(finalModel3)

finalModel4 <- lm(log(Number.Of.Battery) ~ Weekday + Month +
                    Average.Tempature + NY + 
                    as.factor(Year), data=tableB)
summary(finalModel4)
durbinWatsonTest(finalModel2)

crossvalidation1 <- cv.lm(data=tableB, form.lm = formula(log(Number.Of.Battery) ~ Weekday + Month +
                                                           Average.Tempature + NY + 
                                                           as.factor(Year)), 
                          plotit="Observed", m=10)  

crossvalidation2 <- cv.lm(data=tableBZ, form.lm = formula(log(Number.Of.Battery) ~ Weekday + Month +
                                                            Average.Tempature + NY + 
                                                            as.factor(Year)), 
                          plotit="Observed", m=10)  

crossvalidation3 <- cv.lm(data=tableBBox, form.lm = formula(log(Number.Of.Battery) ~ Weekday + Month +
                                                              Average.Tempature + NY + 
                                                              as.factor(Year)), 
                          plotit="Observed", m=10)  
crossvalidation4 <- cv.lm(data=tableB3, form.lm = formula(log(Number.Of.Battery) ~ Weekday + Month +
                                                            Average.Tempature + NY + 
                                                            as.factor(Year)), 
                          plotit="Observed", m=10) 
crossvalidation5 <- cv.lm(data=tableB3, form.lm = formula(Number.Of.Battery ~ Weekday + Month +
                                                                Average.Tempature + NY + 
                                                                as.factor(Year)), 
                          plotit="Observed", m=10) 


plot(Final.Table$Month, Final.Table$Number.Of.Battery)
boxplot(Final.Table$Number.Of.Robbery ~ Final.Table$Month)

averagemonth <- tableB %>%  group_by(Month) %>%
  summarise(avg = mean(log(Number.Of.Battery)))

## MS 2 Cleaning ##
summary(tableR$Number.Of.Robbery)
boxplot(tableR$Number.Of.Robbery)
tableR$boxoutlier <- ifelse(tableR$Number.Of.Robbery > 52,1,0)
tableR$z <- ((tableR$Number.Of.Robbery - mean(tableR$Number.Of.Robbery))/sd(tableR$Number.Of.Robbery))
tableR$zoutlier <- ifelse(abs(tableR$z) > 3,1,0)
sum(tableR$boxoutlier)
sum(tableR$zoutlier)
tableRZ<-tableR[!(tableR$zoutlier== 1),]
tableRB<-tableR[!(tableR$boxoutlier== 1),]
tableR3 <- tableR[-c(3074, 3271, 2684),] 
tableRZ <- tableRZ %>% select(-c(102,103,104))
tableRB <- tableRB %>% select(-c(102,103,104))
tableR3 <- tableR3 %>% select(-c(102,103,104))
tableR <- tableR %>% select(-c(102,103,104))
model.R.1 <- lm(Number.Of.Robbery ~ as.factor(Year) + Month + 
                  Average.Tempature, data = tableR)
summary(model.R.1)
vif(model.R.1)
plot(model.R.1)


model.R.1 <- lm(Number.Of.Robbery ~ as.factor(Year) + Month + Average.Tempature + Number.Of.Venue.Rental.Events + 
                  Average.TempatureSQ + Number.Of.Athletic.Training.Events + 
                  Unemployment_black + Average.TempatureUnemployment_males_20_and_older + 
                  Number.Of.Festival.Performance.12.001..Events + Average.TempatureUnemployment_hispanic + 
                  Thanksgiving.Day + Unemployment_white + Unemployment_males_20_and_olderSQ + 
                  Unemployment_total + Snow. + Unemployment_whiteUnemployment_hispanic + 
                  Unemployment_females_20_and_olderUnemployment_hispanic + 
                  Average.TempatureUnemployment_16_to_19_yrs + Unemployment_hispanic + 
                  Average.TempatureUnemployment_white + Unemployment_females_20_and_older + 
                  Unemployment_males_20_and_olderUnemployment_females_20_and_older + 
                  Unemployment_males_20_and_olderUnemployment_hispanic + Unemployment_whiteSQ + 
                  Unemployment_asianSQ + Average.TempatureUnemployment_females_20_and_older + 
                  Unemployment_16_to_19_yrs + Unemployment_blackSQ + Unemployment_blackUnemployment_hispanic + 
                  Unemployment_males_20_and_older + Average.TempatureUnemployment_black + 
                  Average.TempatureUnemployment_total + Unemployment_asian + 
                  Unemployment_females_20_and_olderUnemployment_white + Unemployment_females_20_and_olderSQ + 
                  Unemployment_hispanicSQ + Weekday + New.Year.s.Day + Average.TempatureUnemployment_asian + 
                  Daily.PrecipitationUnemployment_asian + Christmas.Day + Unemployment_females_20_and_olderUnemployment_asian + 
                  Unemployment_whiteUnemployment_asian + Unemployment_whiteUnemployment_black + 
                  Number.Of.Administrative.Events + Unemployment_males_20_and_olderUnemployment_white + 
                  Unemployment_asianUnemployment_hispanic + Unemployment_females_20_and_olderUnemployment_black + 
                  Unemployment_16_to_19_yrsUnemployment_black + Unemployment_females_20_and_olderUnemployment_16_to_19_yrs + 
                  Unemployment_16_to_19_yrsSQ + Unemployment_16_to_19_yrsUnemployment_hispanic + 
                  Unemployment_totalUnemployment_asian + Unemployment_16_to_19_yrsUnemployment_asian + 
                  Unemployment_totalUnemployment_hispanic + Unemployment_totalUnemployment_white + 
                  Unemployment_totalUnemployment_females_20_and_older + Unemployment_totalUnemployment_males_20_and_older + 
                  Unemployment_males_20_and_olderUnemployment_asian + Unemployment_totalUnemployment_black + 
                  Unemployment_blackUnemployment_asian + Unemployment_males_20_and_olderUnemployment_black + 
                  Unemployment_males_20_and_olderUnemployment_16_to_19_yrs + 
                  Daily.PrecipitationUnemployment_hispanic + Daily.Precipitation + 
                  Daily.PrecipitationUnemployment_black + Presidents..Day., data = tableR)
summary(model.R.1)

model.R.4 <- lm(Number.Of.Robbery ~ as.factor(Year) + Month + 
                  Average.Tempature, data = tableR3)
summary(model.R.4)
plot(model.R.4)


plot(model.R.4$fitted.values, model.R.4$residuals)

durbinWatsonTest(model.R.4)
model_empty <- lm(Number.Of.Robbery ~1, data = tableR3)
model_full <- lm(Number.Of.Robbery ~., data = tableR3)
plot(tableR$Number.Of.Robbery)
plot(tableRZ$Number.Of.Robbery)
plot(tableRB$Number.Of.Robbery)
plot(tableR3$Number.Of.Robbery)

step1 <- stepAIC(model_empty, direction = "forward", scope=list(upper=model_full, lower=model_empty))
step1$anova 

crossvalidation1 <- cv.lm(data=tableR, form.lm = formula(log(Number.Of.Robbery) ~ as.factor(Year) + Month + 
                                                           Average.Tempature), 
                          plotit="Observed", m=10)  

crossvalidation2 <- cv.lm(data=tableRZ, form.lm = formula(log(Number.Of.Robbery) ~ as.factor(Year) + Month + 
                                                            Average.Tempature), 
                          plotit="Observed", m=10)  

crossvalidation3 <- cv.lm(data=tableRB, form.lm = formula(log(Number.Of.Robbery) ~ as.factor(Year) + Month + 
                                                            Average.Tempature), 
                          plotit="Observed", m=10)  
crossvalidation4 <- cv.lm(data=tableR3, form.lm = formula(log(Number.Of.Robbery) ~ as.factor(Year) + Month + 
                                                            Average.Tempature), 
                          plotit="Observed", m=10) 

new_data1B <- tableB3[!(tableB$Year == 2020),]
new_data1.5B <- tableB3[!(tableB$Year == 2020 & as.numeric(tableB$Month) > 3),]
new_data2B <- new_data1.5B[-c(3003,3005,3004, 3006, 3007, 3008, 3009, 
                                          3010, 3011, 3014, 3012, 3013),]
new_data1R <- tableR3[!(tableB$Year == 2020),]
new_data1.5R <- tableR3[!(tableB$Year == 2020 & as.numeric(tableB$Month) > 3),]
new_data2R <- new_data1.5R[-c(3003,3005,3004, 3006, 3007, 3008, 3009, 
                              3010, 3011, 3014, 3012, 3013),]
#### Robbery###

### Reg ###
finalModel2 <- lm(Number.Of.Robbery ~ as.factor(Year) + Month + 
                    Average.Tempature, data = tableR3)
summary(finalModel2)

###No 2020
finalModel2 <- lm(Number.Of.Robbery ~ as.factor(Year) + Month + 
                    Average.Tempature, data = new_data1R)
summary(finalModel2)


### No lockdown###
finalModel2 <- lm(Number.Of.Robbery ~ as.factor(Year) + Month + 
                    Average.Tempature, data = new_data2R)
summary(finalModel2)

#### Battery###

### Reg ###
finalModel1 <- lm(Number.Of.Battery ~ Weekday + Month +
                    Average.Tempature + NY + 
                    as.factor(Year), data = tableB3)
summary(finalModel1)
###No 2020
finalModel1 <- lm(Number.Of.Battery ~ Weekday + Month +
                    Average.Tempature + NY + 
                    as.factor(Year), data = new_data1B)
summary(finalModel1)

### No lockdown###

finalModel1 <- lm(Number.Of.Battery ~ Weekday + Month +
                    Average.Tempature + NY + 
                    as.factor(Year), data = new_data2B)
summary(finalModel1)

plot(tableR3$Weekday, tableR3$Number.Of.Robbery, xlab="Month", ylab="Number of Batteries")
