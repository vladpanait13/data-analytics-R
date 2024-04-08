install.packages("dplyr")
install.packages("pastecs")
install.packages("ggplot2")
install.packages("data.table")
install.packages("moments")

library(dplyr)
library(pastecs)
library(ggplot2)
library(data.table)
library (moments)

df = read.csv('framingham.csv')

#descrierea bazei de date
str(df)
#transformare variabile categoriale care erau trecute cu int in categoriale
df[,c(1, 3, 4, 6,7,8,9, 16)] = lapply(df[,c(1, 3, 4, 6,7,8,9, 16)], factor)
str(df)
#verificam daca sunt valori lipsa
sum(is.na(df))
#eliminare linii cu valori lipsa
df = na.omit(df)

#analiza descriptiva a variabilelor numerice si nenumerice
#variabilelor numerice
stat.desc(df[-c(1, 3, 4, 6,7,8,9, 16)], basic=F)

#variabilele categoriale 
summary(df[c(1, 3, 4, 6,7,8,9, 16)])

#identificare outlieri
box_plot <- function(variabila, main, xlab) {
  boxplot(variabila,
          main = main,
          xlab = xlab,
          col = "orange",
          border = "brown",
          horizontal = TRUE,
          notch = TRUE
  )
}

box_plot(df$age, 'Box-plot varsta', 'Varsta')
box_plot(df$cigsPerDay, 'Box-plot numar de tigari pe zi', 'Numar de tigari pe zi')
box_plot(df$totChol, 'Box-plot total colesterol', 'Total colesterol')
box_plot(df$sysBP, 'Box-plot presiune sistolica a sangelui', 'Presiune sistolica a sangelui')
box_plot(df$diaBP, 'Box-plot tensiune arteriala diastolica', 'Tensiune arteriala diastolica')
box_plot(df$BMI, 'Box-plot BMI', 'BMI')
box_plot(df$heartRate, 'Box-plot frecventa cardiaca', 'Frecventa cardiaca')
box_plot(df$glucose, 'Box-plot nivel glucoza', 'Nivel glucoza')


#eliminare valori extreme 
valori_extreme_cigsPerDay <- boxplot.stats(df$cigsPerDay)$out
df$cigsPerDay <- ifelse(df$cigsPerDay %in% valori_extreme_cigsPerDay, NA, df$cigsPerDay)

valori_extreme_totChol <- boxplot.stats(df$totChol)$out
df$totChol <- ifelse(df$totChol %in% valori_extreme_totChol, NA, df$totChol)

valori_extreme_sysBP <- boxplot.stats(df$sysBP)$out
df$sysBP <- ifelse(df$sysBP %in% valori_extreme_sysBP, NA, df$sysBP)

valori_extreme_diaBP <- boxplot.stats(df$diaBP)$out
df$diaBP <- ifelse(df$diaBP %in% valori_extreme_diaBP, NA, df$diaBP)

valori_extreme_BMI <- boxplot.stats(df$BMI)$out
df$BMI <- ifelse(df$BMI %in% valori_extreme_BMI, NA, df$BMI)

valori_extreme_heartRate <- boxplot.stats(df$heartRate)$out
df$heartRate <- ifelse(df$heartRate %in% valori_extreme_heartRate, NA, df$heartRate)

valori_extreme_glucose <- boxplot.stats(df$glucose)$out
df$glucose <- ifelse(df$glucose %in% valori_extreme_glucose, NA, df$glucose)

df <- na.omit(df)


#analiza grafica a variabilelor numerice si nenumerice
histograma <- function (variabila, main, xlab) {
  hist(variabila,
       main = main,
       xlab = xlab)
}

histograma(df$age, 'Histograma varsta', 'Varsta')
histograma(df$cigsPerDay, 'Histograma numar de tigari pe zi', 'Numar de tigari pe zi' )
histograma(df$totChol, 'Histograma total colesterol', 'Total colesterol')
histograma(df$sysBP, 'Histograma presiune sistolica a sangelui', 'Presiune sistolica a sangelui')
histograma(df$diaBP, 'Histograma tensiune arteriala diastolica', 'Tensiune arteriala diastolica')
histograma(df$BMI, 'Histograma BMI', 'BMI')
histograma(df$heartRate, 'Histograma frecventa cardiaca', 'Frecventa cardiaca')
histograma(df$glucose, 'Histograma nivel glucoza', 'Nivel glucoza')


ggplot(df, aes(male)) + geom_bar(fill='pink', col='black')
ggplot(df, aes(education)) + geom_bar(fill='pink', col='black')
ggplot(df, aes(currentSmoker)) + geom_bar(fill='pink', col='black')
ggplot(df, aes(BPMeds)) + geom_bar(fill='pink', col='black')
ggplot(df, aes(prevalentStroke)) + geom_bar(fill='pink', col='black')
ggplot(df, aes(prevalentHyp)) + geom_bar(fill='pink', col='black')
ggplot(df, aes(diabetes)) + geom_bar(fill='pink', col='black')
ggplot(df, aes(TenYearCHD)) + geom_bar(fill='pink', col='black')

#____________________________

#impartire date in training si test (80% training)
trainset <- df[1:2658, ]
testset <- df[2659:3194, ]

#model regresie logistica ~ coeficientii sunt log odds
mylogit <- glm(TenYearCHD ~ cigsPerDay + age + totChol + sysBP + diaBP + BMI + heartRate + glucose + male + education + currentSmoker + BPMeds + prevalentStroke + prevalentHyp + diabetes, data = trainset, family = "binomial")
summary(mylogit)

# transformare coeificneti in odds
odds <- exp(coef(mylogit))

# transformare odds in probabilitati 
prob <- odds / (1 + odds)

#modelul fara variabilele nesemnificative
mylogit2 <- glm(TenYearCHD ~ cigsPerDay + age + sysBP + diaBP + BMI+ male + prevalentStroke, data = trainset, family = "binomial")
summary(mylogit2)

#comparatie model complet si model incomplet
anova(mylogit, mylogit2, test="Chisq")

#verificarea linearitatii logaritmului
library(car)
boxTidwell(TenYearCHD ~ cigsPerDay + age + sysBP + diaBP + BMI+ male + prevalentStroke, data = trainset)

#verificarea acuratetii modelului ~ se realizeaza pe setul de test
#prezicere clasa
predicted <- plogis(predict(mylogit, testset))

#se decide care e cel mai bun punct care delimiteaza cele doua categorii
library(InformationValue)
optCutOff <- optimalCutoff(testset$TenYearCHD, predicted)[1] 

# verificarea acuratetii modelului cu ROC
plotROC(testset$TenYearCHD, predicted)

#verificarea acuratetii modelului cu consufion matrix
confusionMatrix(testset$TenYearCHD, predicted, threshold = optCutOff)
