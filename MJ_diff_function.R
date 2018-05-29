
library(dplyr) #loading packages necessary for diffdiff function
library(readr)

mj <- read_csv("C:/Users/niwi8/OneDrive/Documents/Practicum/MJ_DinD/Data/MJ_proportions.csv",
                col_names = TRUE) #importing dataset containing proportions and standard errors


# the function DIFFDIFF takes the arguments state, drug of interest, age group, post-intervention year, and pre-intervention year
# 4 different datasets are created var1, var2, US2, and US2 that only contain the necessary values for simulating data
# the seed is automatically set at 10
# another 4 datasets are created that contain the 100000 simulated values for the 4 different proportion distributions
# these datasets are then combined into one dataframe (df) and the difference vectors are created (difference 1 and difference 2)
# another dataset is then created that contains a variable (DD) with the difference of difference 1 and difference2
# the 2.5 and 97.5 percentiles are then calculated for DD to represent the 95% confidence interval for the diff in diff
# the simulated mean difference and the standard deviation are also output
# all values are stored in a list and can be accessed by storing the function in an object and either requesting diff, SE, or CI or by printing the object

diffdiff <- function(df, s, d, a, y1, y2) {
     
     vars1 <- df %>% filter(state == s,
                          drug == d,
                          age == a,
                          year == y1)
     vars2 <- df %>% filter(state == s,
                          drug == d,
                          age == a,
                          year == y2)
     US1 <- df %>% filter(state == "us",
                        drug == d,
                        age == a,
                        year == y1)
     US2 <- df %>% filter(state == "us",
                        drug == d,
                        age == a,
                        year == y2)
     
     set.seed(10)
       s1 <- rnorm(n = 10000,
                     mean = vars1$percent,
                     sd = vars1$se)
     set.seed(20)
       s2 <- rnorm(n = 10000, 
                     mean = vars2$percent, 
                     sd = vars2$se)
     set.seed(30)
       s3 <- rnorm(n = 10000, 
                     mean = US1$percent, 
                     sd = US1$se)
     set.seed(40)
       s4 <- rnorm(n = 10000, 
                     mean = US2$percent, 
                     sd = US2$se)
     
     d.f <- data.frame(s1, s2, s3, s4)
       diff <- d.f %>% mutate(difference1 = s1 - s2,
                               difference2 = s3 - s4) %>%
                       select(difference1,
                              difference2)
       diffINdiff <- diff %>% mutate(DD = difference1 - difference2) %>%
                              select(DD)
       
     ci_state <- quantile(diff$difference1, c(0.025, 0.975))
     
     ci_US <- quantile(diff$difference2, c(0.025, 0.975))
     
     ci <- quantile(diffINdiff$DD, c(0.025, 0.975))
     
     delta <- mean(diffINdiff$DD)
     
     se <- sd(diffINdiff$DD) 
     
     values <- list("State difference CI" = ci_state, 
                    "US difference CI" = ci_US,
                    "diff-in-diff" = delta, 
                    "diff-in-diff SE" = se, 
                    "diff-in-diff CI" = ci)
     
     return(print(values))
}

# Example

diffdiff(mj, "colorado", "alcohol", 12, 2016, 2011)


