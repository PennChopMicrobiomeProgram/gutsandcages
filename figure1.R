setwd("/Users/huw/OneDrive - Children's Hospital of Philadelphia/gutsandcages")
library("devtools")
library("tidyverse")
library("pwr")
library("ggplot2")
library("future")
load_all()
plan(multicore)

expt_icc <- make_expt(ncage = c(10,10), mice_per_cage = c(5, 5))

icc_r <- get_power(expt_icc, seq(0.5, 3, by = 0.1), c(0.1, 0.5, 0.9), nsim = 10000) %>%

mutate(ICC = paste0(rep(c("low","medium", "high"), time = n()/3))) %>%

select(-t_test_power)



no_icc_r <- get_power(make_expt(ncage = c(10, 10), mice_per_cage = c(1, 1), nsim= 10000), seq(0.5, 3, by = 0.1), p = 0, nsim = 100) %>%

mutate(p = 0) %>%

mutate(ICC = "No")



no_icc_r_m <- no_icc_r %>% select(-t_test_power)



no_icc_50 <- get_power(make_expt(ncage = c(50, 50), mice_per_cage = c(1, 1)), seq(0.5, 3, by = 0.1), p = 0, nsim = 10000) %>%

rename(t_test_power2 = t_test_power)



rbind(icc_r, no_icc_r_m) %>%

mutate(ICC = factor(ICC, levels = c("No","low","medium", "high"))) %>%

left_join(no_icc_r %>% select(d, t_test_power), by = "d") %>%

left_join(no_icc_50 %>% select(d, t_test_power2), by = "d") %>%



  ggplot(aes(x = d, y = power, color = ICC)) +

   geom_line() +

  scale_color_brewer(palette = "Dark2") +

  theme_bw() +

   geom_line(aes(y=t_test_power), linetype = "dashed", color = "black") +

  geom_line(aes(y=t_test_power2), linetype = "dashed", color = "darkred")
