#set working dir
#setwd("~/Documents/CSS442 data model+R/project-R")

#import dataset
df_insurance <- read.csv("insurance.csv")
df_insurance

#EDA (Exploration Data Analysis) 
library("tidyverse")
as.tibble(df_insurance)
str(df_insurance)
head(df_insurance)

#transform data
####factor
df_insurance$sex <- factor(df_insurance$sex, c("female","male"))
df_insurance$smoker <- factor(df_insurance$smoker, c("yes","no"))
df_insurance$region <- factor(df_insurance$region, c("northeast","southeast", "southwest", "northwest"))
str(df_insurance)

##extract dataframe
age <- df_insurance$age
sex <- df_insurance$sex
bmi <- df_insurance$bmi
children <- df_insurance$children
smoker <- df_insurance$smoker
region <-  df_insurance$region
charges <- df_insurance$charges

###factor to rank
####Add rank
df_insurance$rank_sex <- rank(df_insurance$sex)
df_insurance$rank_region <- rank(df_insurance$region)
df_insurance$rank_smoker <- rank(df_insurance$smoker)
as.tibble(df_insurance)

#Descriptive Statistics
##Summarizing Data ()
summary(df_insurance)
df_insurance %>% summarise(sd_age = sd(age),
                           sd_bmi = sd(bmi),
                           sd_children = sd(children),
                           sd_charges = sd(charges))

### one-way table (factor)
table(sex)
as.data.frame(table(sex)) 
table(smoker)
as.data.frame(table(smoker))
table(region)
as.data.frame(table(region))

### two-way table (factor)
table(smoker,sex)
as.data.frame(table(smoker,sex))
table(df_insurance$region,df_insurance$sex)

freq.region.sex <-  as.data.frame( table(df_insurance$region,df_insurance$sex))
names(freq.region.sex) <- c("region","sex","Freq") 
freq.region.sex

table(df_insurance$region,df_insurance$smoker)
freq.region.smoker <- as.data.frame(table(df_insurance$region,df_insurance$smoker))
names(freq.region.smoker) <- c("region","smoker","Freq") 
freq.region.smoker

##Visualization (Descriptive)
###barplot
#### barplot one-way table
bp.sex <- barplot(table(sex),
        ylab = "insurance contractor gender", 
        xlab = "count",
        col = c("pink","blue"),
        main = "count of gender",
        horiz = TRUE
        )
text(0,bp.sex,table(sex),cex=2,pos= 4,col = c("black","mintcream"))

bp.smoker <- barplot(table(smoker),
        main = "Number of smoker",
        ylab = "smoker ", 
        xlab = "count",
        col = c("red","green"),
        horiz = TRUE
        )
text(0,bp.smoker,table(smoker),cex=2,pos= 4,col = c("mintcream","black"))

bp.region <- barplot(table(region),
                     #main = "Number of smoker",
                     xlab = "region", 
                     ylim = c(0,400),
                     ylab = "count",
                     col = c("red","green","darkgrey","black"),
                     #horiz = TRUE
                     )
text(bp.region,0,table(region),cex=2,pos= 3,col = c("white","black","black","white"))

#### barplot two-way table
library("RColorBrewer")
#display.brewer.all()
bp.smoker.sex <- barplot(table(smoker,sex), 
                         ylab = "Gender", names = c("Male", "Female"), 
                         xlab = "count", 
                         xlim = c(0,650),
                         legend = rownames(table(smoker,sex)),
                         main = "count  smoker group by genger",
                         beside=TRUE ,
                         horiz = TRUE,
                         #col = c ("pink","yellow"),
                         col=rainbow(2),
                         args.legend = list(title = "smoker",x = "topright", cex = 1)
                         )
text(0,bp.smoker.sex,table(smoker,sex),cex=1.5,pos= 4,col = c("white","black"))

bp.region.sex <- barplot(table(df_insurance$region,df_insurance$sex), 
                         xlab = "Gender", 
                         ylim = c(0,250),
                         #names = c("Male", "Female"), 
                         ylab = "count", 
                         legend = rownames(table(df_insurance$region,df_insurance$sex)),
                         main = "count  region group by smoker",
                         beside=TRUE ,
                         #horiz = TRUE,
                         col = rainbow(4),
                         args.legend = list(title = "region",x = "topright", cex = 1 ))
text(bp.region.sex,0,table(df_insurance$region,df_insurance$sex),
     cex=1.5,pos= 3,col = c("yellow","black","black","yellow"))

bp.region.smoker <- barplot(table(df_insurance$region,df_insurance$smoker), 
                         xlab = "smoker", 
                         ylim = c(0,400),
                         #names = c("Male", "Female"), 
                         ylab = "count", 
                         legend = rownames(table(df_insurance$region,df_insurance$sex)),
                         main = "count  region group by smoker",
                         beside=TRUE ,
                         #horiz = TRUE,
                         col = brewer.pal(n = 4, name = "RdBu"),
                         args.legend = list(title = "region",x = "topright", cex = 1 ))
text(bp.region.smoker,0,table(df_insurance$region,df_insurance$smoker),cex=1.5,pos= 3,col = c("snow","black","black","snow"))

##histogram
par(mfrow=c(2,2))
hist(bmi,xlab = "BMI ",col = "red",angle = 1,freq = TRUE,breaks=25)
hist(age,xlab = "age of insurance contractor",col = "green",breaks= 30)
hist(children, xlab = "Number of children",col = "pink",breaks= 5)
hist(charges,xlab = "Individual medical costs ",col = "yellow",breaks= 10)

##boxplot
par(mfrow=c(2,2))
boxplot(bmi,horizontal = TRUE,xlab = "BMI ",col = "red" )
boxplot(age,horizontal = TRUE,xlab = "age of insurance contractor",col = "green" )
boxplot(children,horizontal = TRUE,xlab = "Number of children",col = "pink" )
boxplot(charges,horizontal = TRUE,xlab = "Individual medical costs ",col = "yellow")

#Inferential Statistics
##Normally distributed 
# Shapiro-Wilk normality test and Visualization
shapiro.test(df_insurance$age) 
shapiro.test(df_insurance$bmi) 
shapiro.test(df_insurance$children) 
shapiro.test(df_insurance$charges)
###Visualization
library("ggpubr")
ggqqplot(children, ylab = "children")
ggqqplot(bmi, ylab = "bmi")
ggqqplot(age, ylab = "age")
ggqqplot(charges, ylab = "charges")

##Correlation test
##### nummeric
cor.test(age,charges, method = "spearman")
cor.test(bmi,charges, method = "spearman")
cor.test(children,charges, method = "spearman")
##### factor
cor.test(charges,rank(sex) ,method = "spearman")
cor.test(charges,rank(region) ,method = "spearman")
cor.test(charges,rank(smoker) ,method = "spearman")

###Visualization
####ggscatter
##### numn
ggscatter(df_insurance, x = "age", y = "charges", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, 
          cor.method = "spearman")
ggscatter(df_insurance, x = "bmi", y = "charges", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, 
          cor.method = "spearman")
ggscatter(df_insurance, x = "children", y = "charges", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, 
          cor.method = "spearman")
###rank
ggscatter(df_insurance, x = "rank_sex", y = "charges", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, 
         cor.method = "spearman")
ggscatter(df_insurance, x = "rank_region", y = "charges", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, 
          cor.method = "spearman")
ggscatter(df_insurance, x = "rank_smoker", y = "charges", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, 
          cor.method = "spearman")

library(corrplot)
corrplot(cor(df_insurance[c('age', 'bmi', 'children','rank_sex','rank_region','rank_smoker', 'charges')],method = "spearman"), 
         method="number",
         type = "upper",
         #col = re
         )

library("PerformanceAnalytics") 
chart.Correlation(df_insurance[c('age', 'bmi', 'children','rank_sex','rank_region','rank_smoker', 'charges')]
                  , histogram = TRUE ,pch=20 
                  , method = "spearman"
                  )

#liner regression model
## Multiple Linear Regression
model1 <- lm(charges ~ age + sex + bmi + children + smoker + region, data = df_insurance) 
model1
summary(model1)

model2 <- update(model1, ~. -sex)
model2
summary(model2)

model3 <- update(model2, ~. -region)
model3
summary(model3)

#Stepwise regression testing
library(MASS)
full.model <-  model1
step.model <- stepAIC(full.model, direction = "both",trace = TRUE)
summary(step.model)

## regression Visualization
yhat2<-predict(model2)
plot(df_insurance$charges,yhat2)
plot(model2, las = 1)




