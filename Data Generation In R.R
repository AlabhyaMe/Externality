library(tidyverse)
rm(list=ls())

set.seed(2059)
# Generate household IDs from 1 to 250
Household_ids <- 1:250

# Generate a random number of people in each household (between 2 and 6)
num_people <- sample(2:6, 250, replace = TRUE)

# Create a data frame for household information
household_data <- data.frame(
  Household_ids = Household_ids,
  num_people = num_people
)

# Create the expanded dataset where each household ID is repeated based on the number of people
full_table <- household_data %>%
  uncount(num_people) %>%
  group_by(Household_ids) %>%
  mutate(Person_id = row_number()) %>%
  ungroup()


# Getting the length of the observation in full_table
n_total <- nrow(full_table)

Household_Targeted <- sample(0:1,length(Household_ids),replace=T, prob=c(0.70,0.30))
targets <- data.frame(Household_ids,Household_Targeted)

full_table <- full_table %>% left_join(targets, by = "Household_ids")
full_table$Individual_targeted <- full_table$Household_Targeted * sample(0:1,nrow(full_table),replace = T)

#count of individual from each household that was targetted
prop_targeted_individuals <- full_table%>%
                                    group_by(Household_ids)%>%
                                    summarise(Prop = sum(Individual_targeted)/length(Individual_targeted))
full_table <- full_table %>% left_join(prop_targeted_individuals, by = "Household_ids")
full_table$Spending <- NA

for (i in 1:nrow(full_table)){
  full_table$Spending[i] <- rnorm(1,full_table$Prop[i]+5, 2) * 3 +rnorm(1,full_table$Household_Targeted[i]+5, 2)*2   + rnorm(1, full_table$Individual_targeted[i]+5,2)*5
}

#drop prop column in full_table
full_table <- full_table[,-5]
write.csv(full_table,"Customer Reaction.csv")

###############################################################################################
customer <- full_table
#delete everything except customer df
rm(list = setdiff(ls(), "customer"))
customer




# Plot the histograms
library(ggplot2)
library(gridExtra)

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
  labs(title = "Customer Spending by Household Targeting", x = "Spending", fill = "Household Targeted") +
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

t.test(data=customer_spillover,Spending~Household_Targeted, alternative = "greater")
