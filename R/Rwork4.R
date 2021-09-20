library(Lahman)
library(tidyverse)

Batting %>%
  filter(yearID == 2019) -> B2019

B2019 %>%
  group_by(playerID) %>%
  summarize(SO = sum(SO),
            AB = sum(AB),
            H = sum(H)) %>%
  filter(AB >= 400)  %>%
  mutate(BA = H / AB,
         SO_Rate = SO / AB,
         BABIP = H / (AB - SO),
         InPlay_Rate = 1 - SO_Rate) -> S400

myf <- function(x, c){c / x}

ggplot(S400, aes(InPlay_Rate, BABIP)) +
  geom_point() +
  geom_function(fun = myf, args = list(c = 0.280),
                color = "red", linetype = 2) +
  geom_function(fun = myf, args = list(c = 0.260),
                color = "red", linetype = 2) +
  geom_function(fun = myf, args = list(c = 0.300),
                color = "red", linetype = 2) +
  geom_function(fun = myf, args = list(c = 0.320),
                color = "red", linetype = 2) +
  geom_function(fun = myf, args = list(c = 0.240),
                color = "red", linetype = 2) +
  geom_function(fun = myf, args = list(c = 0.220),
                color = "red", linetype = 2) +
  geom_function(fun = myf, args = list(c = 0.200),
                color = "red", linetype = 2) +
  geom_function(fun = myf, args = list(c = 0.340),
                color = "red", linetype = 2) +
  annotate(geom = "text",
           x = 0.58,
           y = seq(.2, .34, by = .02) * 1.6 + .03,
           label = c(.200, .220, .240, .260,
                     .280, .300, .320, .340),
           color = "red")


select(S400, InPlay_Rate, BABIP) %>% cor()

select(filter(S400, BA >= .269),
       InPlay_Rate, BABIP) %>% cor()


