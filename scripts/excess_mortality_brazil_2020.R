# Title: Script Excess mortality in Brazil 2020

# Authors: Antônio Augusto Moura da Silva
#          Alcione Miranda dos Santos
#          Bruno Feres de Souza
# Federal University of Maranhao - Brazil
# contact details: aamouradasilva@gmail.com


# Ago 10 2021 ------------------------------

# load packages
library(tidyverse)
library(readxl)
library(knitr)
library(data.table)
library(lubridate)
library(descr)
library(surveillance)
library(gridExtra)
library(grid)
library(ggpubr)

# Import 2015-2019 death data from opendatasus
# Select variables
morte15 <- fread("https://s3-sa-east-1.amazonaws.com/ckan.saude.gov.br/SIM/Mortalidade_Geral_2015.csv")
morte15 <- morte15 %>%
  select(CODMUNRES, DTOBITO, CAUSABAS, SEXO, IDADE, RACACOR)
morte16 <- fread("https://s3-sa-east-1.amazonaws.com/ckan.saude.gov.br/SIM/Mortalidade_Geral_2016.csv")
morte16 <- morte16 %>%
  select(CODMUNRES, DTOBITO, CAUSABAS, SEXO, IDADE, RACACOR)
morte17 <- fread("https://s3-sa-east-1.amazonaws.com/ckan.saude.gov.br/SIM/Mortalidade_Geral_2017.csv")
morte17 <- morte17 %>%
  select(CODMUNRES, DTOBITO, CAUSABAS, SEXO, IDADE, RACACOR)
morte18 <- fread("https://s3-sa-east-1.amazonaws.com/ckan.saude.gov.br/SIM/Mortalidade_Geral_2018.csv")
morte18 <- morte18 %>%
  select(CODMUNRES, DTOBITO, CAUSABAS, SEXO, IDADE, RACACOR)
morte19 <- fread("https://s3-sa-east-1.amazonaws.com/ckan.saude.gov.br/SIM/Mortalidade_Geral_2019.csv")
morte19 <- morte19 %>%
  select(CODMUNRES, DTOBITO, CAUSABAS, SEXO, IDADE, RACACOR)

# Merge data into a single file from 2015 to 2019
morte15_19 <- rbind(morte15, morte16, morte17, morte18, morte19)
rm(morte15, morte16, morte17, morte18, morte19)
save(morte15_19, file = "data/processed_data/morte15_19.RData")

# Import 2020 and 1st trimester of 2021 death data from opendatasus
morte20 <- fread("https://opendatasus.saude.gov.br/dataset/57c4b23e-6ffa-43ef-a1e7-23103f73f376/resource/8d947ac1-addb-49f2-85ab-824a7408a432/download/dobrano_opendatasus.csv")
morte21 <- fread("https://opendatasus.saude.gov.br/dataset/57c4b23e-6ffa-43ef-a1e7-23103f73f376/resource/da17c5f6-aa89-4e7d-b7e2-ec4b77a5dc31/download/do2021opendata.csv")

# Select variables
# Drop stillbirths (TIPOBITO == 1) filter deaths (TIPOBITO == 2)
morte20 <- morte20 %>%
  filter(TIPOBITO == 2) %>%
  select(CODMUNRES, DTOBITO, CAUSABAS, SEXO, IDADE, RACACOR)

morte21 <- morte21 %>%
  filter(TIPOBITO == 2) %>%
  select(CODMUNRES, DTOBITO, CAUSABAS, SEXO, IDADE, RACACOR)

# Merge data into a single file from 2015 to 2021
morte <- rbind(morte15_19, morte20, morte21)
rm(morte20, morte21, morte15_19)
save(morte, file = "data/processed_data/morte.RData")


# generating new variables 
# date_of_death, week (epidemiological week), year, estado (Brazilian states), sex
# filter date_of_death 
mort <- morte %>%
  mutate(
    date_of_death = dmy(DTOBITO),
    week = epiweek(date_of_death),
    year = lubridate::year(date_of_death),
    estado = substring(CODMUNRES, 1, 2),
    sex = recode_factor(as.character(SEXO), "1" = "Male", "2" = "Female", "0" = "Missing")
  ) %>%
  filter(date_of_death >= "2015-01-04" & date_of_death <= "2021-01-02")

# Recoding age in 5 groups
mort$age[mort$IDADE >= 480] <- "Age_80_or_older"
mort$age[mort$IDADE >= 460 & mort$IDADE <= 479] <- "Age_60_to_79"
mort$age[mort$IDADE >= 440 & mort$IDADE <= 459] <- "Age_40_to_59"
mort$age[mort$IDADE >= 420 & mort$IDADE <= 439] <- "Age_20_to_39"
mort$age[mort$IDADE >= 0 & mort$IDADE <= 419] <- "Age_00_to_19"
mort$age[mort$IDADE == 999] <- NA
mort$age <- as.factor(mort$age)

# Generating variable to identify covid-19 deaths
mort$covid <- 0
mort$covid[mort$CAUSABAS %in% c("B342", "U071", "U072")] <- 1

# Groups of ICD - International Classification of Diseases - Abridged
mort$icd[mort$CAUSABAS >= "A00" & mort$CAUSABAS <= "B999"] <- 1
mort$icd[mort$CAUSABAS == "B342"] <- 0
mort$icd[mort$CAUSABAS >= "C00" & mort$CAUSABAS <= "D489"] <- 2
mort$icd[mort$CAUSABAS >= "D50" & mort$CAUSABAS <= "D899"] <- 99
mort$icd[mort$CAUSABAS >= "E00" & mort$CAUSABAS <= "E909"] <- 99
mort$icd[mort$CAUSABAS >= "F00" & mort$CAUSABAS <= "F999"] <- 99
mort$icd[mort$CAUSABAS >= "G00" & mort$CAUSABAS <= "G999"] <- 99
mort$icd[mort$CAUSABAS >= "H00" & mort$CAUSABAS <= "H599"] <- 99
mort$icd[mort$CAUSABAS >= "H60" & mort$CAUSABAS <= "H959"] <- 99
mort$icd[mort$CAUSABAS >= "I00" & mort$CAUSABAS <= "I999"] <- 9
mort$icd[mort$CAUSABAS >= "J00" & mort$CAUSABAS <= "J999"] <- 10
mort$icd[mort$CAUSABAS >= "K00" & mort$CAUSABAS <= "K939"] <- 99
mort$icd[mort$CAUSABAS >= "L00" & mort$CAUSABAS <= "L999"] <- 99
mort$icd[mort$CAUSABAS >= "M00" & mort$CAUSABAS <= "M999"] <- 99
mort$icd[mort$CAUSABAS >= "N00" & mort$CAUSABAS <= "N999"] <- 99
mort$icd[mort$CAUSABAS >= "O00" & mort$CAUSABAS <= "O999"] <- 99
mort$icd[mort$CAUSABAS >= "P00" & mort$CAUSABAS <= "P969"] <- 99
mort$icd[mort$CAUSABAS >= "Q00" & mort$CAUSABAS <= "Q999"] <- 99
mort$icd[mort$CAUSABAS >= "R00" & mort$CAUSABAS <= "R999"] <- 18
mort$icd[mort$CAUSABAS >= "S00" & mort$CAUSABAS <= "T989"] <- 99
mort$icd[mort$CAUSABAS >= "V01" & mort$CAUSABAS <= "Y989"] <- 20
mort$icd[mort$CAUSABAS >= "Z00" & mort$CAUSABAS <= "Z999"] <- 99

mort$icd <- as.factor(mort$icd)

levels(mort$icd) <- c(
  "COVID-19",
  "Other infectious diseases",
  "Neoplasms",
  "Cardiovascular diseases",
  "Respiratory diseases",
  "Ill-defined causes",
  "External causes",
  "Others"
)

# Recoding race
mort$race[mort$RACACOR == 1] <- 1
mort$race[mort$RACACOR == 2] <- 2
mort$race[mort$RACACOR == 3] <- 4
mort$race[mort$RACACOR == 4] <- 3
mort$race[mort$RACACOR == 5] <- 4
mort$race[mort$RACACOR == 9] <- NA
mort$race[is.na(mort$RACACOR)] <- NA
mort$race <- as.factor(mort$race)
levels(mort$race) <- c("White", "Black", "Brown", "Others")

# recoding dates into correct epidemiological years based on epidemiological weeks
mort$year[mort$date_of_death >= "2016-01-01" & mort$date_of_death <= "2016-01-02"] <- 2015
mort$year[mort$date_of_death == "2017-12-31"] <- 2018
mort$year[mort$date_of_death >= "2018-12-30" & mort$date_of_death <= "2018-12-31"] <- 2019
mort$year[mort$date_of_death >= "2019-12-29" & mort$date_of_death <= "2019-12-31"] <- 2020
mort$year[mort$date_of_death >= "2021-01-01" & mort$date_of_death <= "2021-01-02"] <- 2020

# creating factor variable state
# descriptions are acronyms of brazilian states
mort$state <- as.factor(mort$estado)
levels(mort$state) <- c("RO", "AC", "AM", "RR", "PA", "AP", "TO", "MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA", "MG", "ES", "RJ", "SP", "PR", "SC", "RS", "MS", "MT", "GO", "DF")

# dropping unnecessary columns
mort <- subset(mort, select = -c(CODMUNRES, DTOBITO, CAUSABAS, SEXO, IDADE, RACACOR, estado))
rm(morte)
save(mort, file = "data/processed_data/mort.RData")

# Percent of missing data
freq(mort$sex)
freq(mort$age)
freq(mort$race)

####################################
#############  EXCESS MORTALITY
#############  BRAZIL
####################################

load(file = "data/processed_data/mort.RData")

# Excess deaths - BRAZIL
# observed overall deaths and deaths due to COVID-19 by epidemiological week - 2015 to 2020
brtotal <- mort %>%
  mutate(category = "Brazil") %>%
  mutate(category = as.factor(category)) %>%
  group_by(category, year, week) %>%
  summarise(outcome = sum(n = n()), covid = sum(covid))

# Excess deaths by state - BRAZIL
# observed overall deaths and deaths due to COVID-19 by epidemiological week and state - 2015 to 2020
brstate <- mort %>%
  group_by(state, year, week) %>%
  summarise(outcome = sum(n = n()), covid = sum(covid)) %>%
  rename(category = state)

# Excess deaths by sex - BRAZIL
# observed overall deaths and deaths due to COVID-19 by epidemiological week and sex - 2015 to 2020
brsex <- mort %>%
  filter(sex != "Missing") %>%
  mutate(sex = factor(sex)) %>%
  group_by(sex, year, week) %>%
  summarise(outcome = sum(n = n()), covid = sum(covid)) %>%
  rename(category = sex)

# Excess deaths by age - BRAZIL
# observed overall deaths and deaths due to COVID-19 by epidemiological week and age - 2015 to 2020
brage <- mort %>%
  filter(is.na(age) == FALSE) %>%
  mutate(age = factor(age)) %>%
  group_by(age, year, week) %>%
  summarise(outcome = sum(n = n()), covid = sum(covid)) %>%
  rename(category = age)

# Excess deaths by race - BRAZIL
# observed overall deaths and deaths due to COVID-19 by epidemiological week and race - 2015 to 2020
brrace <- mort %>%
  filter(is.na(race) == FALSE) %>%
  mutate(race = factor(race)) %>%
  group_by(race, year, week) %>%
  summarise(outcome = sum(n = n()), covid = sum(covid)) %>%
  rename(category = race)

# Joining data in one file
br <- rbind(brstate, brtotal, brsex, brage, brrace)
rm(brtotal, brstate, brsex, brage, brrace, mort)

# adding dates
dates <- as.Date("2015-01-04") + 7 * 0:(313 - 1)
br$date <- rep(dates, 39)

# change column order
br <- br %>%
  relocate(date, .before = outcome)

# reading population data
pop <- read.csv(file = "data/raw_data/pop.csv", sep = ";")
names(pop) <- c("year", "category", "population")
pop <- pop %>%
  arrange(year)
pop$category <- fct_inorder(pop$category)

# joining data on deaths with population data
brpop <- merge(br, pop)
rm(br, pop)

save(brpop, file = "data/processed_data/brpop.RData")


##################################################
##### data preparation to estimate excess deaths
##################################################

# Surveillance Package
## generating matrix with the number of observed deaths by epidemiological week by category of each variable - Surveillance Package

# excluding week 53 of 2020
brazil <- subset(brpop, week != 53)
category <- c(
  "RO", "AC", "AM", "RR", "PA", "AP", "TO", "MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA", "MG", "ES", "RJ", "SP", "PR", "SC", "RS", "MS", "MT", "GO", "DF",
  "Brazil", "Female", "Male", "Age_00_to_19", "Age_20_to_39", "Age_40_to_59", "Age_60_to_79", "Age_80_or_older", "White", "Black", "Brown", "Others"
)

BRAZIL <- tibble(date = dates[1:312])

for (i in category) {
  BRASIL <- subset(brazil, category == i)
  BRASIL <- BRASIL[c(5)]
  names(BRASIL) <- i
  BRAZIL <- cbind(BRAZIL, BRASIL)
}

BRAZIL <- BRAZIL[-c(1)]
write.csv(BRAZIL, file = "data/processed_data/brasil.csv", row.names = FALSE)
rm(BRAZIL, BRASIL)


## generating matrix with the population of each category of each variable - Surveillance Package

BRAZIL <- tibble(date = dates[1:312])

for (i in category) {
  BRASIL <- subset(brazil, category == i)
  BRASIL <- BRASIL[c(7)]
  names(BRASIL) <- i
  BRAZIL <- cbind(BRAZIL, BRASIL)
}

BRAZIL <- BRAZIL[-c(1)]
write.csv(BRAZIL, file = "data/processed_data/population.csv", row.names = FALSE)
rm(BRAZIL, BRASIL, brazil, brpop)



###################################################################
####################### CALCULATING EXCESS MORTALITY BY STATE,
#######################    AGE AND SEX
###################################################################

# Surveillance package
# Reading data and generating object sts
# brasil.csv - 27 columns containing the number of observed deaths by epidemiological week in each state
# plus 18 columns including total for Brazil, by age (5 groups), by sex, and by sex and age combined (10 groups)
# population.csv - 27 columns containing the population in each state
# plus 18 columns including total population for Brazil, by sex, by age (5 groups) and by race (5 groups), totalling 39 columns
# from 2015 to 2020
# week 53 of 2020 excluded
brasil.ts <- read.csv("data/processed_data/brasil.csv", header = TRUE, check.names = FALSE)
population.ts <- read.csv(file = "data/processed_data/population.csv", header = TRUE, check.names = FALSE)
dates <- as.Date("2015-01-04") + 7 * 0:(nrow(brasil.ts) - 1)
brasil <- sts(as.matrix(brasil.ts),
  epoch = as.numeric(dates), start = c(2015, 1),
  freq = 52, population = as.matrix(population.ts), epochAsDate = TRUE
)
rm(brasil.ts, population.ts)


########################################################################
# Negative binomial GLM modelling using the population size as covariate
########################################################################

# Monitoring starts in week 1, 2015

# defining phase 2 as 2020
phase2 <- which(epoch(brasil) >= "2019-12-29")
length(phase2)

# defining phase 1 as 2015 to 2019
phase1 <- which(year(brasil) == 2015 & epochInYear(brasil) == 1):(phase2[1] - 1)
length(phase1)


# creating dataframes and matrices to save data
brasil.df <- as.data.frame(brasil)
category <- c(
  "RO", "AC", "AM", "RR", "PA", "AP", "TO", "MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA", "MG", "ES", "RJ", "SP", "PR", "SC", "RS", "MS", "MT", "GO", "DF",
  "Brazil", "Male", "Female", "Age_00_to_19", "Age_20_to_39", "Age_40_to_59", "Age_60_to_79", "Age_80_or_older", "White", "Black", "Brown", "Others"
)
esperados <- rep(1, 39)
liminf <- rep(1, 39)
limsup <- rep(1, 39)
df.esperados <- data.frame(category, esperados, liminf, limsup)
matriz1 <- matrix(NA, nrow = 39, ncol = 53)
matriz2 <- matrix(NA, nrow = 39, ncol = 53)
matriz3 <- matrix(NA, nrow = 39, ncol = 53)
matriz1[, 1] <- df.esperados$category
matriz2[, 1] <- df.esperados$category
matriz3[, 1] <- df.esperados$category

# calculating excess mortalily in the binomial model
for (e in category) {
  estado.df <- brasil.df[phase1, c(paste("observed.", e, sep = ""), "epoch", "epochInPeriod", paste("population.", e, sep = ""))]
  colnames(estado.df) <- c("observed", "epoch", "epochInPeriod", "population")
  m <- MASS::glm.nb(observed ~ 1 + epoch + sin(2 * pi * epochInPeriod) + cos(2 * pi * epochInPeriod)
    + population, data = estado.df)
  pred_estado.df <- brasil.df[phase2, c(paste("observed.", e, sep = ""), "epoch", "epochInPeriod", paste("population.", e, sep = ""))]
  colnames(pred_estado.df) <- c("observed", "epoch", "epochInPeriod", "population")
  # grad the inverse link function
  ilink <- family(m)$linkinv
  ## add fit and se.fit on the **link** scale
  pred_estado.df <- bind_cols(pred_estado.df, setNames(as_tibble(predict(m, pred_estado.df,
    se.fit = TRUE
  )[1:2]), c("fit_link", "se_link")))
  ## create the interval and backtransform
  pred_estado.df <- mutate(pred_estado.df,
    fit_resp = round(ilink(fit_link)),
    right_upr = round(ilink(fit_link + (2 * se_link))),
    right_lwr = round(ilink(fit_link - (2 * se_link)))
  )
  df.esperados[df.esperados$category == e, 2] <- sum(pred_estado.df$fit_resp)
  df.esperados[df.esperados$category == e, 3] <- sum(pred_estado.df$right_lwr)
  df.esperados[df.esperados$category == e, 4] <- sum(pred_estado.df$right_upr)
  matriz1[matriz1[, 1] == e, 2:53] <- pred_estado.df$fit_resp
  matriz2[matriz2[, 1] == e, 2:53] <- pred_estado.df$right_lwr
  matriz3[matriz3[, 1] == e, 2:53] <- pred_estado.df$right_upr
}

rm(brasil, brasil.df, estado.df, m, pred_estado.df)


# Table 1
load(file = "data/processed_data/brpop.RData")
br20 <- brpop %>%
  filter(year == 2020) %>%
  group_by(category, year) %>%
  summarise(outcome = sum(outcome), covid = sum(covid))
br20 <- subset(br20, select = -c(year, category))
br20 <- cbind(df.esperados, br20)
names(br20) <- c("state", "expected", "lower_limit_expected", "upper_limit_expected", "observed", "covid")
br20$excess <- br20$observed - br20$expected
br20$pexcess <- round(br20$excess / br20$expected * 100, digits = 1)
br20$rcovid <- round(br20$excess / br20$covid, digits = 2)
br201 <- br20 %>%
  select(state, expected, observed, excess, pexcess, covid, rcovid)
br201s <- br20 %>%
  select(state, expected, lower_limit_expected, upper_limit_expected)

tab1 <- br201[c(1:28), ]
tab1sup <- br201s[c(1:28), ]
kable(tab1)
kable(tab1sup)
write.csv2(tab1, file = "output/tab1.csv", row.names = FALSE)
write.csv2(tab1sup, file = "output/tab1sup.csv", row.names = FALSE)
rm(tab1, tab1sup, br20, df.esperados)

# Table 2
br201 <- br201 %>%
  rename(category = state)
br201s <- br201s %>%
  rename(Category = state)
tab2 <- br201[c(29:39), ]
tab2sup <- br201s[c(29:39), ]
kable(tab2)
kable(tab2sup)
write.csv2(tab2, file = "output/tab2.csv", row.names = FALSE)
write.csv2(tab2sup, file = "output/tab2sup.csv", row.names = FALSE)
rm(tab2, tab2sup, br201, br201s)



# expected deaths by epidemiological week and state, sex, age and race

observ <- brpop %>%
  filter(year == 2020) %>%
  select(category, date, week, outcome, covid) %>%
  arrange(category, week)

number <- data.frame(as.numeric(c(matriz1[1, 2:53], matriz1[1, 53])), as.numeric(c(matriz2[1, 2:53], matriz2[1, 53])), as.numeric(c(matriz3[1, 2:53], matriz3[1, 53])))
names(number) <- c("expected", "lower_limit_expected", "upper_limit_expected")
for (i in 2:39) {
  expected <- data.frame(as.numeric(c(matriz1[i, 2:53], matriz1[i, 53])), as.numeric(c(matriz2[i, 2:53], matriz2[i, 53])), as.numeric(c(matriz3[i, 2:53], matriz3[i, 53])))
  names(expected) <- c("expected", "lower_limit_expected", "upper_limit_expected")
  number <- rbind(number, expected)
}

brasil <- cbind(observ, number)
brasil$covid_excess <- brasil$covid + brasil$expected
save(brasil, file = "data/processed_data/brasil.RData")

rm(expected, number, observ, matriz1, matriz2, matriz3, brpop)



# Figure 1
# Excess mortality - BRAZIL 2020

load(file = "data/processed_data/brasil.RData")
grafico <- subset(brasil, category == "Brazil")
graf_line.long <- pivot_longer(grafico,
  cols = c(expected, lower_limit_expected, upper_limit_expected, outcome, covid_excess),
  names_to = "variable", values_to = "value"
)
figure1 <- ggplot(graf_line.long, aes(x = date, y = value, color = variable)) +
  stat_summary(fun = sum, geom = "line", size = 1.2) +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%d/%m") +
  theme_bw() +
  theme(axis.text = element_text(size = 9), plot.caption = element_text(size = 10), legend.text = element_text(size = 10), axis.title = element_blank()) +
  labs(caption = "Excess deaths are the distance between the number of observed and the number of expected deaths. 
       Excess deaths by COVID-19 are the distance between the sum of expected and COVID-19 deaths and the number of expected deaths.") +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
  scale_color_manual(name = "", values = c("blue", "black", "gray", "red", "gray"), labels = c(
    "Sum of expected and COVID-19 deaths",
    "Number of expected deaths", "Lower limit of the number of expected deaths",
    "Number of observed deaths", "Upper limit of the number of expected deaths"
  ))
g <- grid.arrange(figure1, bottom= textGrob("Figure 1. Excess deaths due to all causes and to COVID-19 by epidemiological week, Brazil, 2020", gp=gpar(fontsize=17)))

ggsave("output/figure1.svg", width = 30, height = 15, units = "cm", g)


# Supplementary figures 1 to 7
# Figures showing excess mortality by all causes, by COVID-19 by state and epidemiological week
category <- c(
  "RO", "AC", "AM", "RR", "PA", "AP", "TO", "MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA", "MG", "ES", "RJ", "SP", "PR", "SC", "RS", "MS", "MT", "GO", "DF",
  "Brazil", "Male", "Female", "Age_00_to_19", "Age_20_to_39", "Age_40_to_59", "Age_60_to_79", "Age_80_or_older", "White", "Black", "Brown", "Others"
)
for (i in category) {
  grafico <- subset(brasil, category == i)
  print(paste("Processing", i))
  graf_line.long <- pivot_longer(grafico,
    cols = c(expected, lower_limit_expected, upper_limit_expected, outcome, covid_excess),
    names_to = "variable", values_to = "value"
  )
  test <- ggplot(graf_line.long, aes(x = date, y = value, color = variable)) +
    stat_summary(fun = sum, geom = "line", size = 1.2) +
    scale_x_date(date_breaks = "4 weeks", date_labels = "%d/%m") +
    theme_bw() +
    theme(
      axis.text = element_text(size = 9),
      legend.text = element_text(size = 11),
      axis.title = element_blank()
    ) +
    labs(title = i) +
    theme(legend.position = "bottom") +
    guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
    scale_color_manual(name = "", values = c("blue", "black", "gray", "red", "gray"), labels = c(
      "Sum of expected and COVID-19 deaths",
      "Number of expected deaths", "Lower limit of the number of expected deaths",
      "Number of observed deaths", "Upper limit of the number of expected deaths"
    ))
  assign(i, test)
}


# Figure 3
# Excess Deaths - Brazil by age
age <- ggarrange(Age_00_to_19, Age_20_to_39, Age_40_to_59, Age_60_to_79, Age_80_or_older, ncol=2, nrow=3, common.legend = TRUE, legend="bottom")
age <- annotate_figure(age, 
                           bottom=text_grob("Excess deaths are the distance between the number of observed and the number of expected deaths. \n Excess deaths by COVID-19 are the distance between the sum of expected and COVID-19 deaths and the number of expected deaths.", size=12))
g <- grid.arrange(age, bottom= textGrob("\n Figure 3. Excess deaths by all causes and by COVID=19 by epidemiological week \n according to age, Brazil, 2020", gp=gpar(fontsize=20)))
ggsave("output/figure3.svg", width = 30, height = 30, units = "cm", g)


# Supplementary Figure 1
# Excess Deaths - North region
norte <- ggarrange(RO, AC, AM, RR, PA, AP, TO, ncol=2, nrow=4, common.legend = TRUE, legend="bottom")
norte <- annotate_figure(norte, 
                         bottom=text_grob("Excess deaths are the distance between the number of observed and the number of expected deaths. \n Excess deaths by COVID-19 are the distance between the sum of expected and COVID-19 deaths and the number of expected deaths.", size=12))
g <- grid.arrange(norte, bottom= textGrob("\n Supplementary figure 1. Excess deaths by all causes and by COVID=19 by epidemiological\n week, North Region, Brazil, 2020", gp=gpar(fontsize=20)))
ggsave("output/norte.png", width = 30, height = 40, units = "cm", g)


# Supplementary Figure 2
# Excess Deaths - Northeast region
nordeste <- ggarrange(MA, PI, CE, RN, PB, PE, AL, SE, BA, ncol=2, nrow=5, common.legend = TRUE, legend="bottom")
nordeste <- annotate_figure(nordeste, 
                            bottom=text_grob("Excess deaths are the distance between the number of observed and the number of expected deaths. \n Excess deaths by COVID-19 are the distance between the sum of expected and COVID-19 deaths and the number of expected deaths.", size=12))
g <- grid.arrange(nordeste, bottom= textGrob("\n Supplementary figure 2. Excess deaths by all causes and by COVID=19 by epidemiological\n week, Northeast Region, Brazil, 2020", gp=gpar(fontsize=20)))

ggsave("output/nordeste.png", width = 30, height = 50, units = "cm", g)


# Supplementary Figure 3
# Excess Deaths - Southeast region
sudeste <- ggarrange(MG, ES, RJ, SP, ncol=2, nrow=2, common.legend = TRUE, legend="bottom")
sudeste <- annotate_figure(sudeste, 
                           bottom=text_grob("Excess deaths are the distance between the number of observed and the number of expected deaths. \n Excess deaths by COVID-19 are the distance between the sum of expected and COVID-19 deaths and the number of expected deaths.", size=12))
g <- grid.arrange(sudeste, bottom= textGrob("\n Supplementary figure 3. Excess deaths by all causes and by COVID=19 by epidemiological\n week, Southeast Region, Brazil, 2020", gp=gpar(fontsize=20)))

ggsave("output/sudeste.png", width = 30, height = 20, units = "cm", g)


# Supplementary Figure 4
# Excess Deaths - South region
sul <- ggarrange(PR, SC, RS, ncol=2, nrow=2, common.legend = TRUE, legend="bottom")
sul <- annotate_figure(sul, 
                       bottom=text_grob("Excess deaths are the distance between the number of observed and the number of expected deaths. \n Excess deaths by COVID-19 are the distance between the sum of expected and COVID-19 deaths and the number of expected deaths.", size=12))
g <- grid.arrange(sul, bottom= textGrob("\n Supplementary figure 4. Excess deaths by all causes and by COVID=19 by epidemiological\n week, South Region, Brazil, 2020", gp=gpar(fontsize=20)))
ggsave("output/sul.png", width = 30, height = 20, units = "cm", g)


# Supplementary Figure 5
# Excess Deaths - Center-West region
centro_oeste <- ggarrange(MS, MT, GO, DF, ncol=2, nrow=2, common.legend = TRUE, legend="bottom")
centro_oeste <- annotate_figure(centro_oeste, 
                                bottom=text_grob("Excess deaths are the distance between the number of observed and the number of expected deaths. \n Excess deaths by COVID-19 are the distance between the sum of expected and COVID-19 deaths and the number of expected deaths.", size=12))
g <- grid.arrange(centro_oeste, bottom= textGrob("\n Supplementary figure 5. Excess deaths by all causes and by COVID=19 by epidemiological\n week, Center-West Region, Brazil, 2020", gp=gpar(fontsize=20)))
ggsave("output/centro_oeste.png", width = 30, height = 20, units = "cm", g)


# Supplementary Figure 6
# Excess Deaths - Brazil by sex
sex <- ggarrange(Male, Female, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
sex <- annotate_figure(sex,
                       bottom=text_grob("Excess deaths are the distance between the number of observed and the number of expected deaths. \n Excess deaths by COVID-19 are the distance between the sum of expected and COVID-19 deaths and the number of expected deaths.", size=12))
g <- grid.arrange(sex, bottom= textGrob("\n Supplementary figure 6. Excess deaths by all causes and by COVID=19 by epidemiological\n week according to sex, Brazil, 2020", gp=gpar(fontsize=20)))
ggsave("output/sex.png", width = 30, height = 15, units = "cm", g)


# Supplementary Figure 7
# Excess Deaths - Brazil by race
race <- ggarrange(White, Black, Brown, Others, ncol=2, nrow=2, common.legend = TRUE, legend="bottom")
race <- annotate_figure(race, 
                        bottom=text_grob("Excess deaths are the distance between the number of observed and the number of expected deaths. \n Excess deaths by COVID-19 are the distance between the sum of expected and COVID-19 deaths and the number of expected deaths.", size=12))
g <- grid.arrange(race, bottom= textGrob("\n Supplementary figure 7. Excess deaths by all causes and by COVID=19 by epidemiological\n week according to race, Brazil, 2020", gp=gpar(fontsize=20)))
ggsave("output/race.png", width = 30, height = 20, units = "cm", g)

rm(
  RO, AC, AM, RR, PA, AP, TO, MA, PI, CE, RN, PB, PE, AL, SE, BA, MG, ES, RJ, SP, PR, SC, RS, MS, MT, GO, DF,
  Brazil, Male, Female, Age_00_to_19, Age_20_to_39, Age_40_to_59, Age_60_to_79, Age_80_or_older, White, Black, Brown, Others,
  sex, age, race, norte, nordeste, sul, sudeste, centro_oeste
)
rm(brasil, figure1, graf_line.long, grafico, test, g)





##################################################################
# Brazil. Selected causes of death by epidemiological week
# aggregating deaths by cause
##################################################################
load(file = "data/processed_data/mort.RData")

# Brazil
causabr <- mort %>%
  mutate(category = "Brazil") %>%
  mutate(category = as.factor(category)) %>%
  select(category, icd, year, week) %>%
  filter(!(icd == "COVID-19" & year == 2017)) %>%
  group_by(category, icd, year, week) %>%
  summarise(count = n())

# by state
causastate <- mort %>%
  select(state, icd, year, week) %>%
  filter(!(icd == "COVID-19" & year == 2017)) %>%
  group_by(state, icd, year, week) %>%
  summarise(count = n()) %>%
  rename(category = state)

# by sex
causasex <- mort %>%
  select(sex, icd, year, week) %>%
  filter(sex != "Missing") %>%
  mutate(sex = factor(sex)) %>%
  filter(!(icd == "COVID-19" & year == 2017)) %>%
  group_by(sex, icd, year, week) %>%
  summarise(count = n()) %>%
  rename(category = sex)

# by age
causaage <- mort %>%
  select(age, icd, year, week) %>%
  filter(is.na(age) == FALSE) %>%
  mutate(age = factor(age)) %>%
  filter(!(icd == "COVID-19" & year == 2017)) %>%
  group_by(age, icd, year, week) %>%
  summarise(count = n()) %>%
  rename(category = age)

# by race
causarace <- mort %>%
  select(race, icd, year, week) %>%
  filter(is.na(race) == FALSE) %>%
  mutate(race = factor(race)) %>%
  filter(!(icd == "COVID-19" & year == 2017)) %>%
  group_by(race, icd, year, week) %>%
  summarise(count = n()) %>%
  rename(category = race)

# Joining data in one file
brcausa <- rbind(causastate, causabr, causasex, causaage, causarace)
rm(causastate, causabr, causasex, causaage, causarace)

# adding population
pop <- read.csv(file = "data/raw_data/pop.csv", sep = ";")
names(pop) <- c("year", "category", "population")
pop <- pop %>%
  arrange(year)
pop$category <- fct_inorder(pop$category)

brcausapop <- merge(brcausa, pop)
rm(brcausa, pop)

# calculating mortality rate
brcausapop$rate <- brcausapop$count / brcausapop$population * 100000

# calculating median mortality rate for 2015 to 2019
brcausapop <- brcausapop %>%
  mutate(year = as.character(case_when(
    year >= 2015 & year <= 2019 ~ "median from 2015 to 2019",
    year == 2020 ~ "2020"
  ))) %>%
  group_by(category, icd, year, week) %>%
  summarise(median_rate = median(rate))

save(brcausapop, file = "data/processed_data/brcausapop.RData")

category <- c(
  "RO", "AC", "AM", "RR", "PA", "AP", "TO", "MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA", "MG", "ES", "RJ", "SP", "PR", "SC", "RS", "MS", "MT", "GO", "DF",
  "Brazil", "Male", "Female", "Age_00_to_19", "Age_20_to_39", "Age_40_to_59", "Age_60_to_79", "Age_80_or_older", "White", "Black", "Brown", "Others"
)

figure <- 8

for (i in category) {
  n <- i
  causa_cid <- filter(brcausapop, category == i)
  if (i == "Male" | i == "Female") {
    i <- paste(i, "sex")
  }
  else if (i == "White" | i == "Black" | i == "Brown" | i == "Others") {
    i <- paste(i, "race")
  }
  else {
    i <- i
  }
  if (i == "Brazil") {
    texto <- "\n Figure 2. Mortality rate (per 100.000) by epidemiological week according to \n selected causes, Brazil, 2015 to 2020"
    figure <- figure - 1
  }
  else {
    texto <- paste("\n Supplementary figure ", figure, ". Mortality rate (per 100.000) by epidemiological week \n according to selected causes, ", i, ", 2015 to 2020", sep = "")
  }
  print(paste("Processing", i, "figure", figure))
  cause <- ggplot(causa_cid, aes(x = week, y = median_rate, color = year)) +
    geom_line(size = 1.2) +
    facet_wrap(~icd, nrow = 4) +
    scale_x_continuous(breaks = seq(3, 55, 9), label = c("Jan", "Mar", "May", "Jul", "Sep", "Nov")) +
    theme_bw() +
    theme(
      axis.text = element_text(size = 14), axis.title = element_blank(),
      legend.position = "bottom", legend.text = element_text(size = 16),
      plot.tag = element_text(size = 16), strip.text.x = element_text(size = 14),
      plot.tag.position = "bottom"
    ) +
    scale_color_manual(name = "", values = c("blue", "black")) +
    labs(tag = texto)
  assign(n, cause)
  nome <- paste(n, ".png", sep = "")
  ggsave(nome, width = 25, height = 30, units = "cm", path="output/")
  figure <- figure + 1
}


Brazil
ggsave("figure2.svg", width = 25, height = 30, units = "cm", path="output/")

rm(
  RO, AC, AM, RR, PA, AP, TO, MA, PI, CE, RN, PB, PE, AL, SE, BA, MG, ES, RJ, SP, PR, SC, RS, MS, MT, GO, DF,
  Brazil, Male, Female, Age_00_to_19, Age_20_to_39, Age_40_to_59, Age_60_to_79, Age_80_or_older, White, Black, Brown, Others
)
rm(brcausapop, causa_cid, cause, mort)
