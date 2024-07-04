library(readr)
library(tidyverse)
library(gridExtra)
rm(list=ls())
customer <- read_csv("Customer Reaction.csv")

# Calculate means
mean_household_targeted <- mean(customer$Spending[customer$Household_Targeted == 1], na.rm = TRUE)
mean_household_not_targeted <- mean(customer$Spending[customer$Household_Targeted == 0], na.rm = TRUE)
mean_individual_targeted <- mean(customer$Spending[customer$Individual_targeted == 1], na.rm = TRUE)
mean_individual_not_targeted <- mean(customer$Spending[customer$Individual_targeted == 0], na.rm = TRUE)

# Create the first histogram
p1 <- ggplot(customer, aes(x = Spending, fill = as.factor(Household_Targeted))) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  geom_vline(aes(xintercept = mean_household_targeted), color = "red", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = mean_household_not_targeted), color = "blue", linetype = "dashed", size = 1) +
  scale_fill_manual(values = c("0" = "blue", "1" = "red")) +
  labs(title = "Customer Spending by Household Targeting", x = "Spending", fill = "Household Targeted") +
  theme_minimal()

# Create the second histogram
p2 <- ggplot(customer, aes(x = Spending, fill = as.factor(Individual_targeted))) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  geom_vline(aes(xintercept = mean_individual_targeted), color = "red", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = mean_individual_not_targeted), color = "blue", linetype = "dashed", size = 1) +
  scale_fill_manual(values = c("0" = "blue", "1" = "red")) +
  labs(title = "Customer Spending by Individual Targeting", x = "Spending", fill = "Individual Targeted") +
  theme_minimal()

# Arrange the plots side by side
grid.arrange(p1, p2, ncol = 2)

#filter individual targeted
customer_spillover <- customer[customer$Individual_targeted==0,]
mean_individual_from_household_targeted <- mean(customer_spillover$Spending[customer_spillover$Household_Targeted == 1], na.rm = TRUE)
mean_individual_from_household_not_targeted <- mean(customer_spillover$Spending[customer_spillover$Household_Targeted == 0], na.rm = TRUE)

p3 <- ggplot(customer_spillover, aes(x = Spending, fill = as.factor(Household_Targeted))) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  geom_vline(aes(xintercept = mean_individual_from_household_targeted), color = "red", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = mean_individual_from_household_not_targeted), color = "blue", linetype = "dashed", size = 1) +
  scale_fill_manual(values = c("0" = "blue", "1" = "red")) +
  labs(title = "Customer Spending by Household Targeting but Controlled Individual", x = "Spending", fill = "Household Targeted") +
  theme_minimal()

p3

HH_diff<-tapply(customer$Spending,customer$Household_Targeted,mean)
II_diff<- tapply(customer$Spending,customer$Individual_targeted,mean)

details <- function(x){
  avg <- mean(x)
  std <- sd(x)
  n <- length(x)
  return(c(avg,std,n))
}

details(customer$Spending[customer$Household_Targeted==1 & customer$Individual_targeted==0])
details(customer$Spending[customer$Household_Targeted==1 & customer$Individual_targeted==1])
details(customer$Spending[customer$Household_Targeted==0 & customer$Individual_targeted==0])
details(customer$Spending[customer$Household_Targeted==0 & customer$Individual_targeted==1])

#main test
x <- customer_spillover$Spending[customer_spillover$Household_Targeted==1]
y <- customer_spillover$Spending[customer_spillover$Household_Targeted==0]
t.test(x,y,alternative = "greater")
t.test(data=customer_spillover,Spending~as.factor(Household_Targeted), alternative = "less") #the comparision is opposite


#extra
t.test(data=customer,Spending~Household_Targeted, alternative = "less")
t.test(data=customer,Spending~Individual_targeted, alternative = "less")
t.test(data=customer_spillover,Spending~as.factor(Household_Targeted), alternative = "less", mu=2)


# Calculate the mean spending for each group
mean(customer_spillover$Spending[customer_spillover$Household_Targeted == 1])
mean(customer_spillover$Spending[customer_spillover$Household_Targeted == 0])


