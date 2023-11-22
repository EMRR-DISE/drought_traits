library(readxl)
library(tidyverse)
library(vegan)

# data from Table 4,
# Satterthwaite, Carlson, Criss (2017) Ocean size and corresponding life history diversity 
# among the four run timings of California Central Valley Chinook Salmon. Trans Am Fish Soc, 
# DOI:10.1080/00028487.2017.1293562

dat <- tibble(run = c("CV fall", "SR fall", "SJ fall", "Mok fall", "late fall", "Winter", "Spring"),
              age2 = c(18,13,39,43,10,7,9), age3 = c(63,76,55,50,64,90,73), age4 = c(19,22,6,7,25,2,17),
              age5 = c(.3,.3,.2,.03,1,.04,.3), N = c(33773,27693,2481,3599,25230,2712,9910))

rdiv <- dat %>% 
  mutate(age2 = round(age2/100 * N, digits = 0),
         age3 = round(age3/100 * N, digits = 0),
         age4 = round(age4/100 * N, digits = 0),
         age5 = round(age5/100 * N, digits = 0)) %>% 
  select(-N) %>% 
  add_column(H = diversity(rdiv[,2:5]))

(H <- diversity(rdiv[,2:5]))
(simp <- diversity(rdiv[,2:5], "simpson"))
(invsimp <- diversity(rdiv[,2:5], "inv"))
(unbias.simp <- simpson.unb(rdiv[,2:5]))
(alpha <- fisher.alpha(rdiv[,2:5])) # ?huh?

pairs(cbind(H, simp, invsimp, unbias.simp), pch="+", col="blue")

