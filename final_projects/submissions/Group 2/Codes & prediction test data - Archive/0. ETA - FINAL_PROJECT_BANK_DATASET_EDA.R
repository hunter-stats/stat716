#EDA

#install.packages('ggplot2')
#install.packages('dplyr')
#install.packages('gridExtra')
#install.packages("magrittr")

library(ggplot2)
library(dplyr)
library(gridExtra)
library(magrittr)

setwd("C:\\Users\\MEL\\Desktop\\courses2018Fall\\DataAnalysis716\\Final_Project\\dataset")
train <- read.csv('train_data.csv')
test <-  read.csv('test_data.csv')
summary(train)


overall_yes <- sum(as.numeric(train$y=="yes"))/length(train$y)
overall_yes

#AGE
age_table <- train %>%
  select(age,y) %>%
  group_by(y) %>%
  summarise(mean.age = mean(age))
age_table

plot1 <- ggplot(train,aes(age,fill=y)) +
  geom_density(alpha=0.2,aes(color=y))+
  scale_y_continuous(labels = scales::percent) +
  ylab("Density") + ggtitle("Relationship Between Term Deposit Rate and Age")
plot1

plot2 <- ggplot(train,aes(age)) + 
  geom_density(fill = 'gray')+
  scale_y_continuous(labels = scales::percent) +
  ylab("Density")
plot2

grid.arrange(plot1,plot2,nrow=2)

#JOB
ggplot(train,aes(job,fill = y)) + geom_bar(position="fill") +
  scale_fill_brewer(palette = "Paired") + 
  ylab("Term Deposit Percentage") + ggtitle("Term Deposit Rate by Job") + theme_minimal()


#MARITAL
ggplot(train,aes(marital,fill = y)) + geom_bar(position="fill") +
  scale_fill_brewer(palette = "Paired") + 
  ylab("Term Deposit Percentage") + ggtitle("Term Deposit Rate by Marital Status") + theme_minimal()



#EDUCATION
ggplot(train,aes(education,fill = y)) + geom_bar(position="fill") +
  scale_fill_brewer(palette = "Paired") + 
  ylab("Term Deposit Percentage") + ggtitle("Term Deposit Rate by Education Status") + theme_minimal()

#CREDIT DEFAULT
ggplot(train,aes(default,fill = y)) + geom_bar(position="fill") +
  scale_fill_brewer(palette = "Paired") + 
  geom_hline(yintercept = overall_yes,col="black",lty=2,size=1) +
  ylab("Term Deposit Percentage") + ggtitle("Term Deposit Rate by Credit Default Status") + theme_minimal()


#BALANCE
balance_table <- train %>%
  select(balance,y) %>%
  group_by(y) %>%
  summarise(mean.balance = mean(balance))

plot1 <- ggplot(train,aes(balance,fill=y)) +
  geom_density(alpha=0.2,aes(color=y))+
  scale_y_continuous(labels = scales::percent) +
  ylab("Density") + ggtitle("Relationship Between Term Deposit Rate and Balance") +xlim(0,1500)
plot1

plot2 <- ggplot(train,aes(balance)) + 
  geom_density(fill = 'gray')+
  scale_y_continuous(labels = scales::percent) +
  ylab("Density")+xlim(0,1500)
plot2

grid.arrange(plot1,plot2,nrow=2)

#HOUSING LOAN
ggplot(train,aes(housing,fill = y)) + geom_bar(position="fill") +
  scale_fill_brewer(palette = "Paired") + 
  geom_hline(yintercept = overall_yes,col="black",lty=2,size=1) +
  ylab("Term Deposit Percentage") + ggtitle("Term Deposit Rate by Housing Loan Status") + theme_minimal()

#LOAN
ggplot(train,aes(loan,fill = y)) + geom_bar(position="fill") +
  scale_fill_brewer(palette = "Paired") + 
  geom_hline(yintercept = overall_yes,col="black",lty=2,size=1) +
  ylab("Term Deposit Percentage") + ggtitle("Term Deposit Rate by Personal Loan Status") + theme_minimal()


#CONTACT
ggplot(train,aes(contact,fill = y)) + geom_bar(position="fill") +
  scale_fill_brewer(palette = "Paired") + 
  geom_hline(yintercept = overall_yes,col="black",lty=2,size=1) +
  ylab("Term Deposit Percentage") + ggtitle("Term Deposit Rate by Contact Mode") + theme_minimal()

#MONTH

ggplot(train,aes(month,fill = y)) + geom_bar(position="fill") +
  scale_fill_brewer(palette = "Paired") + 
  geom_hline(yintercept = overall_yes,col="black",lty=2,size=1) +
  ylab("Term Deposit Percentage") + ggtitle("Term Deposit Rate by Month of Last Contact") + theme_minimal()

.

#CAMPAIGN
campaign_table <- train %>%
  select(campaign,y) %>%
  group_by(y) %>%
  summarise(mean.campaign = mean(campaign))
campaign_table


ggplot(train,aes(x = y,y = campaign,color=y)) + geom_boxplot() + ylab("Number of Contacts") +
   ggtitle("Term Deposit Rate by Number of Contacts in This Campaign") + theme_minimal()



ggplot(train,aes(x = y,y = campaign,color=y)) + geom_boxplot() + ylab("Number of Contacts") +
  ggtitle("Term Deposit Rate by Number of Contacts in This Campaign - Zoomed In") + theme_minimal() +ylim(0,10)

#PREVIOUS

ggplot(train,aes(x = y,y = previous,color=y)) + geom_boxplot() + ylab("Number of Contacts") +
  ggtitle("Term Deposit Rate by Number of Contacts in Previous Campaign") + theme_minimal()

ggplot(train,aes(x = y,y = previous,color=y)) + geom_boxplot() + ylab("Number of Contacts") +
  ggtitle("Term Deposit Rate by Number of Contacts in Previous Campaign - Zoomed In") + theme_minimal() +ylim(0,10)

#POUTCOME
ggplot(train,aes(poutcome,fill = y)) + geom_bar(position="fill") +
  scale_fill_brewer(palette = "Paired") + 
  geom_hline(yintercept = overall_yes,col="black",lty=2,size=1) +
  ylab("Term Deposit Percentage") + ggtitle("Term Deposit Rate by Outcome of Previous Campaign") + theme_minimal()










