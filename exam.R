pacman::p_load(tidyverse, broom, mosaic, ggplot2, vcd, corrplot, 
               janitor, naniar, gdata, ggmosaic, sjPlot, haven, cutpointr)

##Prüfung Statistik

data_tbl <- read_csv("Backhouse_Isabel_Louisa.csv")
data_tbl

#Explorative Datenanalyse = Hypothesenbildende

###Chronizität
ggplot(data_tbl, aes(chronicity)) +
  geom_histogram(aes(fill = sex), binwidth = 1, color = "black", position = "dodge")

ggplot(data_tbl, aes(x = sex, y = chronicity)) +
  geom_boxplot()
###
###Akuter Schmerz
ggplot(data_tbl, aes(migraine_score)) +
  geom_histogram(aes(fill = sex), binwidth = 1, color = "black", position = "dodge")
data_tbl$migraine_score %>% summary
### 

### Verhältnis M und F
data_tbl$sex %>% tally

### Altersverteilung
data_tbl$age %>% summary
ggplot(data_tbl, aes(age))+
  geom_histogram(aes(fill = sex), binwidth = 1, color = "black", position = "dodge")


### Randomisierung ### Warum ist dies ein integraler Bestandteil der Statistik?
data_tbl$group %>% tally
## Anzahl der Frauen in jeweiligen Gruppen
filter(data_tbl, sex == "woman" & group == "accupuncture")
filter(data_tbl, sex == "woman" & group == "control")

##Anzahl der Männer in jeweiligen Gruppen
filter(data_tbl, sex == "man" & group == "accupuncture")
filter(data_tbl, sex == "man" & group == "accupuncture")
###

### Boxplot der Wirksamkeit von Akupunkter bei Migräne
ggplot(data_tbl, aes(x = group, y = migraine_score)) +
  geom_boxplot(color = "black", fill ="yellow")
###

###Explorative Analyse Ende###

### Rechnen Sie auf den Daten einen Chi-Quadrat-Test

cutpointr(data_tbl, migraine_score, group)

#Dichotomisiere migrain_score durch 22: migraine_score > 22 

mean(data_tbl$migraine_score)
score_bin <- mutate(data_tbl, score_bin = ifelse(migraine_score >22, 1,0))

table <- matrix(c(27,1,6,55), nrow = 2, byrow = FALSE)

addmargins(table)
### Daten lassen keine Berechnung zu -> Manipulation
table_manipulated <- matrix(c(27,5,6,51), nrow = 2, byrow = FALSE)
addmargins(table_manipulated)
chisq.test(table_manipulated, correct = FALSE)

#gibt mir die Erwartungswerte:
chisq.test(table_manipulated)$expected
per_Hand <- ((27-11.87)^2/11.87)+((6-21.13)^2/21.13)+((5-20.13)^2/20.13)+((51-35.87)^2/35.87)
per_Hand

###

###Odds Ratio
or <- (27*51)/(5*6)

# Zusammenhang des Chi-Quadrat Tests mit der Regressionsanalyse erstellen:
score_group_bin <- mutate(score_bin, group_bin = ifelse(group == "control",0, 1))

##Logistsiche Regression mit x binär und y binär = Chi-Quadrat-Test
ggplot(score_group_bin, aes(y = score_bin, x = group_bin)) +
  geom_point()

mosaicplot(score_bin ~ group_bin, score_group_bin, main ="Mosaicplot der beiden binären Variabeln", xlab = "Binärer Migräne Score",
           ylab = "Gruppen")

lr <- glm(score_bin~group_bin, binomial, score_group_bin)%>%  tidy (exponentiate = TRUE, conf.int = TRUE)
lr
?glm
fun()
glm(score_bin~ group_bin, binomial, score_group_bin) %>% summary


fun <- makeFun(lr)
### fun liefert mir die Eintrittswahrscheinlichkeiten 
### damit kann ich Verhältnis von Wahrscheinlichkeitsverhältnissen berechnen:
x1<-fun(1)
x2 <-1-x1  
x2
y1 <- fun(0)
y2 <- (1-y1)
(x1/x2)/(y1/y2)
### jitter.y = TRUE ermöglicht bessere Visualisierung der Datenpunkte
xyplot(score_bin~ group_bin , groups = score_bin, data = score_group_bin, jitter.y = TRUE)



###ENDE

##Densityplot
ggplot(data_tbl, aes(migraine_score)) + 
  geom_histogram(aes(y= ..density.., fill = group), binwidth= .5, color = "black") +
  geom_density() 

###Ausprobieren
data_tbl1 <- mutate(data_tbl, group = as.factor(group))
data_tbl1
v1 <- data_tbl %>% select(group, migraine_score) %>%
  mutate_if(is.character, as.factor)
v1 %>% summary
v1
control <- filter(v1, group == "control")
control
ggplot(control, aes(migraine_score, y = ..density..))+
  geom_histogram(color = "black")+
  geom_density()
accupuncture <- filter(v1, group == "accupuncture")
ggplot(accupuncture, aes(migraine_score, y = ..density..))+
  geom_histogram(color = "black")+
  geom_density()
control %>% summary
accupuncture %>% summary
mean(control$migraine_score)
sd(control$migraine_score)
mean(accupuncture$migraine_score)
sd(accupuncture$migraine_score)

d1 =rnorm(56, 17.86, 2.67)
tibble(d2 =rnorm(33, 24.89, 1.95)
       ) %>%
  gather%>%
  mutate(key = as.factor(key)) %>%
  ggplot(aes(x=value, color = key, position = "stack"))+
  geom_density()


