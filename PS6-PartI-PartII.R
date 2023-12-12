## Problem Set 6
# 12/1/2023

# Load packages
library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)

### PART I
# Read in data
d <- read.csv("/Users/jo/Desktop/Fall 2023/DemTechII/Problem Set 6/nLx_values.csv")
d = d %>%
  mutate(race = str_sub(cat_race_ed_age, 1, 2)) %>%
  mutate(education = str_sub(cat_race_ed_age, 3, 3)) %>%
  mutate(age = str_sub(cat_race_ed_age, 5))
# Do the same for education and age

## Black population
black = d %>%
  filter(race == "Lb") %>%
  mutate(age = as.numeric(age)) %>%
  mutate(education = as.numeric(education)) %>%
  select(age, race, education, nLx) %>%
  arrange(age, education) %>%
  group_by(education)

black$S<-1
for (i in 1:5) {
  for (j in c(0,5,10,15,20,25,30,35,40))
    black$S[black$age==j & black$education==i]<-
      black$nLx[black$age==j+5 & black$education==i]/
      black$nLx[black$age==j & black$education==i]
}
black<-black[order(black$age,black$education),]
black$S[black$age==45 & black$education==1]<-253566/350025
black$S[black$age==45 & black$education==2]<-314646/427965
black$S[black$age==45 & black$education==3]<-530589/696438
black$S[black$age==45 & black$education==4]<-985438/1241169
black$S[black$age==45 & black$education==5]<-1414091/1740959

Mblack <- diag(black$S[1:45], nrow=45, ncol=45)

# GRR to fertility rates
# Each 5 years long; 6 intervals (15-19, 20-24, 25-29, 30-34, 35-39, 40-44) 
black$Fx[black$education==1] <- (2.18/6)
black$Fx[black$education==2] <- (2.18/6)
black$Fx[black$education==3] <- (1.67/6)
black$Fx[black$education==4] <- (1.43/6)
black$Fx[black$education==5] <- (1.01/6)
black$Fx[black$age==0] <- 0
black$Fx[black$age==5] <- 0
black$Fx[black$age==10] <- 0
black$Fx[black$age==45] <- 0

# Scalar value
black$L[black$education==1] <- black$nLx[black$age==0 & black$education==1]/(2*100000) # 2*Radiux
black$L[black$education==2] <- black$nLx[black$age==0 & black$education==2]/(2*100000)
black$L[black$education==3] <- black$nLx[black$age==0 & black$education==3]/(2*100000)
black$L[black$education==4] <- black$nLx[black$age==0 & black$education==4]/(2*100000)
black$L[black$education==5] <- black$nLx[black$age==0 & black$education==5]/(2*100000)
black$L[black$age==0] <- 0
black$L[black$age==5] <- 0
black$L[black$age==10] <- 0
black$L[black$age==45] <- 0

# Mobility matrix
mblack <- matrix(c(0.289, 0.268, 0.243, 0.126, 0.073,
                   0.079, 0.350, 0.278, 0.164, 0.129,
                   0.025, 0.190, 0.386, 0.212, 0.188,
                   0.033, 0.038, 0.243, 0.496, 0.189,
                   0.000, 0.032, 0.163, 0.371, 0.434),
                 nrow=5, ncol=5, byrow = TRUE)  

# Scalar*(Fertility+Survival*Fertility)
black$X <- black$L*(black$Fx+black$S*black$Fx)
for (i in 1:5) {
  for (j in c(15,20,25,30,35,40)) {
    black$X[black$education==i & black$age==j]<-black$L[black$education==i & 
                                                          black$age==j]*(black$Fx[black$education==i & black$age==j]+
                                                                           black$nLx[black$education==i & black$age==j]/
                                                                           black$nLx[black$education==i & black$age==j-5]*
                                                                           black$Fx[black$education==i & black$age==j])
  }
}

black15 <- matrix(black$X[black$age==15], nrow=5, ncol=1)
black20 <- matrix(black$X[black$age==20], nrow=5, ncol=1)
black25 <- matrix(black$X[black$age==25], nrow=5, ncol=1)
black30 <- matrix(black$X[black$age==30], nrow=5, ncol=1)
black35 <- matrix(black$X[black$age==35], nrow=5, ncol=1)
black40 <- matrix(black$X[black$age==40], nrow=5, ncol=1)

# Births (5x1 matrix above multiplied by mobility matrix 5x5)
bblack15 <- as.vector(black15)*mblack
bblack20 <- as.vector(black20)*mblack
bblack25 <- as.vector(black25)*mblack
bblack30 <- as.vector(black30)*mblack
bblack35 <- as.vector(black35)*mblack
bblack40 <- as.vector(black40)*mblack

matrixSb<-diag(x=black$S[black$age<45])
matrixAb<-matrix(data=0:0,nrow=40,ncol=5)
matrixBb<-diag(x=c(black$S[black$age==45&black$education==1],
                   black$S[black$age==45&black$education==2],
                   black$S[black$age==45&black$education==3],
                   black$S[black$age==45&black$education==4],
                   black$S[black$age==45&black$education==5]))

matrixMb<-cbind(matrixSb,rbind(matrixAb,matrixBb))
zero <- matrix(data=0:0, nrow=5, ncol=5)

matrixCb <-cbind(zero, zero, zero, bblack15, bblack20, bblack25, bblack30, bblack35, bblack40, zero)
Mmatrixblack <- rbind(matrixCb, matrixMb)


## White Population  
white = d %>%
  filter(race == "Lw") %>%
  mutate(age = as.numeric(age)) %>%
  mutate(education = as.numeric(education)) %>%
  select(age, race, education, nLx) %>%
  arrange(age, education) %>%
  group_by(education)

white$S<-1
for (i in 1:5) {
  for (j in c(0,5,10,15,20,25,30,35,40))
    white$S[white$age==j & white$education==i]<-
      white$nLx[white$age==j+5 & white$education==i]/
      white$nLx[white$age==j & white$education==i]
}
white<-white[order(white$age,white$education),]
white$S[white$age==45 & white$education==1]<-612653/796390
white$S[white$age==45 & white$education==2]<-885954/1123710
white$S[white$age==45 & white$education==3]<-1305962/1615282
white$S[white$age==45 & white$education==4]<-1768697/2148809
white$S[white$age==45 & white$education==5]<-1896125/2293440

Mwhite <- diag(white$S[1:45], nrow=45, ncol=45)

# GRR to Fertility Rates
# Each 5 years long; 6 Intervals (15-19, 20-24, 25-29, 30-34, 35-39, 40-44) 
white$Fx[white$education==1] <- (1.76/6)
white$Fx[white$education==2] <- (1.81/6)
white$Fx[white$education==3] <- (1.62/6)
white$Fx[white$education==4] <- (1.54/6)
white$Fx[white$education==5] <- (1.27/6)
white$Fx[white$age==0] <- 0
white$Fx[white$age==5] <- 0
white$Fx[white$age==10] <- 0
white$Fx[white$age==45] <- 0

# Scalar value
white$L[white$education==1] <- white$nLx[white$age==0 & white$education==1]/(2*100000) # 2*Radiux
white$L[white$education==2] <- white$nLx[white$age==0 & white$education==2]/(2*100000)
white$L[white$education==3] <- white$nLx[white$age==0 & white$education==3]/(2*100000)
white$L[white$education==4] <- white$nLx[white$age==0 & white$education==4]/(2*100000)
white$L[white$education==5] <- white$nLx[white$age==0 & white$education==5]/(2*100000)
white$L[white$age==0] <- 0
white$L[white$age==5] <- 0
white$L[white$age==10] <- 0
white$L[white$age==45] <- 0

#Mobility Matrix
mwhite <- matrix(c(0.132, 0.179, 0.485, 0.130, 0.075,
                   0.034, 0.150, 0.427, 0.208, 0.180,
                   0.014, 0.061, 0.457, 0.250, 0.217,
                   0.010, 0.027, 0.243, 0.338, 0.381,
                   0.001, 0.033, 0.143, 0.259, 0.564),
                 nrow=5, ncol=5, byrow = TRUE)

# Scalar*(Fertility+Survival*Fertility)
white$X <-white$L*(white$Fx+white$S*white$Fx)
for (i in 1:5) {
  for (j in c(15,20,25,30,35,40)) {
    white$X[white$education==i &white$age==j]<-white$L[white$education==i & 
                                                         white$age==j]*(white$Fx[white$education==i &white$age==j]+
                                                                          white$nLx[white$education==i &white$age==j]/
                                                                          white$nLx[white$education==i &white$age==j-5]*
                                                                          white$Fx[white$education==i &white$age==j])
  }
}

white15 <- matrix(white$X[white$age==15], nrow=5, ncol=1)
white20 <- matrix(white$X[white$age==20], nrow=5, ncol=1)
white25 <- matrix(white$X[white$age==25], nrow=5, ncol=1)
white30 <- matrix(white$X[white$age==30], nrow=5, ncol=1)
white35 <- matrix(white$X[white$age==35], nrow=5, ncol=1)
white40 <- matrix(white$X[white$age==40], nrow=5, ncol=1)

# Births (5x1 matrix above multiplied by mobility matrix 5x5)
bwhite15 <- as.vector(white15)*mwhite
bwhite20 <- as.vector(white20)*mwhite
bwhite25 <- as.vector(white25)*mwhite
bwhite30 <- as.vector(white30)*mwhite
bwhite35 <- as.vector(white35)*mwhite
bwhite40 <- as.vector(white40)*mwhite

matrixSw<-diag(x=white$S[white$age<45])
matrixAw<-matrix(data=0:0,nrow=40,ncol=5)
matrixBw<-diag(x=c(white$S[white$age==45&white$education==1],
                   white$S[white$age==45&white$education==2],
                   white$S[white$age==45&white$education==3],
                   white$S[white$age==45&white$education==4],
                   white$S[white$age==45&white$education==5]))

matrixMw<-cbind(matrixSw,rbind(matrixAw,matrixBw))

matrixCw <-cbind(zero, zero, zero, bwhite15, bwhite20, bwhite25, bwhite30, bwhite35, bwhite40, zero)
Mmatrixwhite <- rbind(matrixCw, matrixMw)



##Final Matrices
print(Mmatrixblack)
print(Mmatrixwhite)

### PART II
# 1. I am assuming a population of 100,000 for each group, with 10,000 in each age group
# those 10,000 are then distributed according to the education distribution for that population.

blackpop <- matrix(c(3760,2900,2330,630,380), nrow=50, ncol=1)
whitepop <- matrix(c(1720,2190,4210,1190,690), nrow=50, ncol=1)

library(expm)

## 2. What equilibrium distributions of educational attainment for Black and White people are
# implied by the data in Tables 1-4?

bfive=Mmatrixblack %*% blackpop
bten=(Mmatrixblack %^% 2) %*% blackpop
btwenty=(Mmatrixblack %^% 4) %*% blackpop
bthirty=(Mmatrixblack %^% 6) %*% blackpop
bforty=(Mmatrixblack %^% 8) %*% blackpop
bfifty=(Mmatrixblack %^% 10) %*% blackpop
bsixty=(Mmatrixblack %^% 12) %*% blackpop
bseventy=(Mmatrixblack %^% 14) %*% blackpop
beighty=(Mmatrixblack %^% 16) %*% blackpop
bninety=(Mmatrixblack %^% 18) %*% blackpop
bhund=(Mmatrixblack %^% 20) %*% blackpop
bhund50=(Mmatrixblack %^% 30) %*% blackpop
btwohund=(Mmatrixblack %^% 40) %*% blackpop
btwohund50=(Mmatrixblack %^% 50) %*% blackpop
bthreehund=(Mmatrixblack%^% 60) %*% blackpop

blackdistfive = c((sum(bfive[1,1], bfive[6,1], bfive[11,1], bfive[16,1], bfive[21,1],
                       bfive[26,1], bfive[31,1], bfive[36,1], bfive[41,1], bfive[46,1])/sum(bfive)), 
                  (sum(bfive[2,1], bfive[7,1], bfive[12,1], bfive[17,1], bfive[22,1],
                       bfive[27,1], bfive[32,1], bfive[37,1], bfive[42,1], bfive[47,1])/sum(bfive)),
                  (sum(bfive[3,1], bfive[8,1], bfive[13,1], bfive[18,1], bfive[23,1],
                       bfive[28,1], bfive[33,1], bfive[38,1], bfive[43,1], bfive[48,1])/sum(bfive)),
                  (sum(bfive[4,1], bfive[9,1], bfive[14,1], bfive[19,1], bfive[24,1],
                       bfive[29,1], bfive[34,1], bfive[39,1], bfive[44,1], bfive[49,1])/sum(bfive)),
                  (sum(bfive[5,1], bfive[10,1], bfive[15,1], bfive[20,1], bfive[25,1],
                       bfive[30,1], bfive[35,1], bfive[40,1], bfive[45,1], bfive[50,1])/sum(bfive)))

blackdistten = c((sum(bten[1,1], bten[6,1], bten[11,1], bten[16,1], bten[21,1],
                      bten[26,1], bten[31,1], bten[36,1], bten[41,1], bten[46,1])/sum(bten)), 
                 (sum(bten[2,1], bten[7,1], bten[12,1], bten[17,1], bten[22,1],
                      bten[27,1], bten[32,1], bten[37,1], bten[42,1], bten[47,1])/sum(bten)),
                 (sum(bten[3,1], bten[8,1], bten[13,1], bten[18,1], bten[23,1],
                      bten[28,1], bten[33,1], bten[38,1], bten[43,1], bten[48,1])/sum(bten)),
                 (sum(bten[4,1], bten[9,1], bten[14,1], bten[19,1], bten[24,1],
                      bten[29,1], bten[34,1], bten[39,1], bten[44,1], bten[49,1])/sum(bten)),
                 (sum(bten[5,1], bten[10,1], bten[15,1], bten[20,1], bten[25,1],
                      bten[30,1], bten[35,1], bten[40,1], bten[45,1], bten[50,1])/sum(bten)))

blackdisttwenty = c((sum(btwenty[1,1], btwenty[6,1], btwenty[11,1], btwenty[16,1], btwenty[21,1],
                         btwenty[26,1], btwenty[31,1], btwenty[36,1], btwenty[41,1], btwenty[46,1])/sum(btwenty)), 
                    (sum(btwenty[2,1], btwenty[7,1], btwenty[12,1], btwenty[17,1], btwenty[22,1],
                         btwenty[27,1], btwenty[32,1], btwenty[37,1], btwenty[42,1], btwenty[47,1])/sum(btwenty)),
                    (sum(btwenty[3,1], btwenty[8,1], btwenty[13,1], btwenty[18,1], btwenty[23,1],
                         btwenty[28,1], btwenty[33,1], btwenty[38,1], btwenty[43,1], btwenty[48,1])/sum(btwenty)),
                    (sum(btwenty[4,1], btwenty[9,1], btwenty[14,1], btwenty[19,1], btwenty[24,1],
                         btwenty[29,1], btwenty[34,1], btwenty[39,1], btwenty[44,1], btwenty[49,1])/sum(btwenty)),
                    (sum(btwenty[5,1], btwenty[10,1], btwenty[15,1], btwenty[20,1], btwenty[25,1],
                         btwenty[30,1], btwenty[35,1], btwenty[40,1], btwenty[45,1], btwenty[50,1])/sum(btwenty)))

blackdistthirty = c((sum(bthirty[1,1], bthirty[6,1], bthirty[11,1], bthirty[16,1], bthirty[21,1],
                         bthirty[26,1], bthirty[31,1], bthirty[36,1], bthirty[41,1], bthirty[46,1])/sum(bthirty)), 
                    (sum(bthirty[2,1], bthirty[7,1], bthirty[12,1], bthirty[17,1], bthirty[22,1],
                         bthirty[27,1], bthirty[32,1], bthirty[37,1], bthirty[42,1], bthirty[47,1])/sum(bthirty)),
                    (sum(bthirty[3,1], bthirty[8,1], bthirty[13,1], bthirty[18,1], bthirty[23,1],
                         bthirty[28,1], bthirty[33,1], bthirty[38,1], bthirty[43,1], bthirty[48,1])/sum(bthirty)),
                    (sum(bthirty[4,1], bthirty[9,1], bthirty[14,1], bthirty[19,1], bthirty[24,1],
                         bthirty[29,1], bthirty[34,1], bthirty[39,1], bthirty[44,1], bthirty[49,1])/sum(bthirty)),
                    (sum(bthirty[5,1], bthirty[10,1], bthirty[15,1], bthirty[20,1], bthirty[25,1],
                         bthirty[30,1], bthirty[35,1], bthirty[40,1], bthirty[45,1], bthirty[50,1])/sum(bthirty)))

blackdistforty = c((sum(bforty[1,1], bforty[6,1], bforty[11,1], bforty[16,1], bforty[21,1],
                        bforty[26,1], bforty[31,1], bforty[36,1], bforty[41,1], bforty[46,1])/sum(bforty)), 
                   (sum(bforty[2,1], bforty[7,1], bforty[12,1], bforty[17,1], bforty[22,1],
                        bforty[27,1], bforty[32,1], bforty[37,1], bforty[42,1], bforty[47,1])/sum(bforty)),
                   (sum(bforty[3,1], bforty[8,1], bforty[13,1], bforty[18,1], bforty[23,1],
                        bforty[28,1], bforty[33,1], bforty[38,1], bforty[43,1], bforty[48,1])/sum(bforty)),
                   (sum(bforty[4,1], bforty[9,1], bforty[14,1], bforty[19,1], bforty[24,1],
                        bforty[29,1], bforty[34,1], bforty[39,1], bforty[44,1], bforty[49,1])/sum(bforty)),
                   (sum(bforty[5,1], bforty[10,1], bforty[15,1], bforty[20,1], bforty[25,1],
                        bforty[30,1], bforty[35,1], bforty[40,1], bforty[45,1], bforty[50,1])/sum(bforty)))

blackdistfifty = c((sum(bfifty[1,1], bfifty[6,1], bfifty[11,1], bfifty[16,1], bfifty[21,1],
                        bfifty[26,1], bfifty[31,1], bfifty[36,1], bfifty[41,1], bfifty[46,1])/sum(bfifty)), 
                   (sum(bfifty[2,1], bfifty[7,1], bfifty[12,1], bfifty[17,1], bfifty[22,1],
                        bfifty[27,1], bfifty[32,1], bfifty[37,1], bfifty[42,1], bfifty[47,1])/sum(bfifty)),
                   (sum(bfifty[3,1], bfifty[8,1], bfifty[13,1], bfifty[18,1], bfifty[23,1],
                        bfifty[28,1], bfifty[33,1], bfifty[38,1], bfifty[43,1], bfifty[48,1])/sum(bfifty)),
                   (sum(bfifty[4,1], bfifty[9,1], bfifty[14,1], bfifty[19,1], bfifty[24,1],
                        bfifty[29,1], bfifty[34,1], bfifty[39,1], bfifty[44,1], bfifty[49,1])/sum(bfifty)),
                   (sum(bfifty[5,1], bfifty[10,1], bfifty[15,1], bfifty[20,1], bfifty[25,1],
                        bfifty[30,1], bfifty[35,1], bfifty[40,1], bfifty[45,1], bfifty[50,1])/sum(bfifty)))

blackdistsixty = c((sum(bsixty[1,1], bsixty[6,1], bsixty[11,1], bsixty[16,1], bsixty[21,1],
                        bsixty[26,1], bsixty[31,1], bsixty[36,1], bsixty[41,1], bsixty[46,1])/sum(bsixty)), 
                   (sum(bsixty[2,1], bsixty[7,1], bsixty[12,1], bsixty[17,1], bsixty[22,1],
                        bsixty[27,1], bsixty[32,1], bsixty[37,1], bsixty[42,1], bsixty[47,1])/sum(bsixty)),
                   (sum(bsixty[3,1], bsixty[8,1], bsixty[13,1], bsixty[18,1], bsixty[23,1],
                        bsixty[28,1], bsixty[33,1], bsixty[38,1], bsixty[43,1], bsixty[48,1])/sum(bsixty)),
                   (sum(bsixty[4,1], bsixty[9,1], bsixty[14,1], bsixty[19,1], bsixty[24,1],
                        bsixty[29,1], bsixty[34,1], bsixty[39,1], bsixty[44,1], bsixty[49,1])/sum(bsixty)),
                   (sum(bsixty[5,1], bsixty[10,1], bsixty[15,1], bsixty[20,1], bsixty[25,1],
                        bsixty[30,1], bsixty[35,1], bsixty[40,1], bsixty[45,1], bsixty[50,1])/sum(bsixty)))

blackdistseventy = c((sum(bseventy[1,1], bseventy[6,1], bseventy[11,1], bseventy[16,1], bseventy[21,1],
                          bseventy[26,1], bseventy[31,1], bseventy[36,1], bseventy[41,1], bseventy[46,1])/sum(bseventy)), 
                     (sum(bseventy[2,1], bseventy[7,1], bseventy[12,1], bseventy[17,1], bseventy[22,1],
                          bseventy[27,1], bseventy[32,1], bseventy[37,1], bseventy[42,1], bseventy[47,1])/sum(bseventy)),
                     (sum(bseventy[3,1], bseventy[8,1], bseventy[13,1], bseventy[18,1], bseventy[23,1],
                          bseventy[28,1], bseventy[33,1], bseventy[38,1], bseventy[43,1], bseventy[48,1])/sum(bseventy)),
                     (sum(bseventy[4,1], bseventy[9,1], bseventy[14,1], bseventy[19,1], bseventy[24,1],
                          bseventy[29,1], bseventy[34,1], bseventy[39,1], bseventy[44,1], bseventy[49,1])/sum(bseventy)),
                     (sum(bseventy[5,1], bseventy[10,1], bseventy[15,1], bseventy[20,1], bseventy[25,1],
                          bseventy[30,1], bseventy[35,1], bseventy[40,1], bseventy[45,1], bseventy[50,1])/sum(bseventy)))

blackdisteighty = c((sum(beighty[1,1], beighty[6,1], beighty[11,1], beighty[16,1], beighty[21,1],
                         beighty[26,1], beighty[31,1], beighty[36,1], beighty[41,1], beighty[46,1])/sum(beighty)), 
                    (sum(beighty[2,1], beighty[7,1], beighty[12,1], beighty[17,1], beighty[22,1],
                         beighty[27,1], beighty[32,1], beighty[37,1], beighty[42,1], beighty[47,1])/sum(beighty)),
                    (sum(beighty[3,1], beighty[8,1], beighty[13,1], beighty[18,1], beighty[23,1],
                         beighty[28,1], beighty[33,1], beighty[38,1], beighty[43,1], beighty[48,1])/sum(beighty)),
                    (sum(beighty[4,1], beighty[9,1], beighty[14,1], beighty[19,1], beighty[24,1],
                         beighty[29,1], beighty[34,1], beighty[39,1], beighty[44,1], beighty[49,1])/sum(beighty)),
                    (sum(beighty[5,1], beighty[10,1], beighty[15,1], beighty[20,1], beighty[25,1],
                         beighty[30,1], beighty[35,1], beighty[40,1], beighty[45,1], beighty[50,1])/sum(beighty)))

blackdistninety = c((sum(bninety[1,1], bninety[6,1], bninety[11,1], bninety[16,1], bninety[21,1],
                         bninety[26,1], bninety[31,1], bninety[36,1], bninety[41,1], bninety[46,1])/sum(bninety)), 
                    (sum(bninety[2,1], bninety[7,1], bninety[12,1], bninety[17,1], bninety[22,1],
                         bninety[27,1], bninety[32,1], bninety[37,1], bninety[42,1], bninety[47,1])/sum(bninety)),
                    (sum(bninety[3,1], bninety[8,1], bninety[13,1], bninety[18,1], bninety[23,1],
                         bninety[28,1], bninety[33,1], bninety[38,1], bninety[43,1], bninety[48,1])/sum(bninety)),
                    (sum(bninety[4,1], bninety[9,1], bninety[14,1], bninety[19,1], bninety[24,1],
                         bninety[29,1], bninety[34,1], bninety[39,1], bninety[44,1], bninety[49,1])/sum(bninety)),
                    (sum(bninety[5,1], bninety[10,1], bninety[15,1], bninety[20,1], bninety[25,1],
                         bninety[30,1], bninety[35,1], bninety[40,1], bninety[45,1], bninety[50,1])/sum(bninety)))

blackdisthund = c((sum(bhund[1,1], bhund[6,1], bhund[11,1], bhund[16,1], bhund[21,1],
                       bhund[26,1], bhund[31,1], bhund[36,1], bhund[41,1], bhund[46,1])/sum(bhund)), 
                  (sum(bhund[2,1], bhund[7,1], bhund[12,1], bhund[17,1], bhund[22,1],
                       bhund[27,1], bhund[32,1], bhund[37,1], bhund[42,1], bhund[47,1])/sum(bhund)),
                  (sum(bhund[3,1], bhund[8,1], bhund[13,1], bhund[18,1], bhund[23,1],
                       bhund[28,1], bhund[33,1], bhund[38,1], bhund[43,1], bhund[48,1])/sum(bhund)),
                  (sum(bhund[4,1], bhund[9,1], bhund[14,1], bhund[19,1], bhund[24,1],
                       bhund[29,1], bhund[34,1], bhund[39,1], bhund[44,1], bhund[49,1])/sum(bhund)),
                  (sum(bhund[5,1], bhund[10,1], bhund[15,1], bhund[20,1], bhund[25,1],
                       bhund[30,1], bhund[35,1], bhund[40,1], bhund[45,1], bhund[50,1])/sum(bhund)))

blackdisthund50 = c((sum(bhund50[1,1], bhund50[6,1], bhund50[11,1], bhund50[16,1], bhund50[21,1],
                         bhund50[26,1], bhund50[31,1], bhund50[36,1], bhund50[41,1], bhund50[46,1])/sum(bhund50)), 
                    (sum(bhund50[2,1], bhund50[7,1], bhund50[12,1], bhund50[17,1], bhund50[22,1],
                         bhund50[27,1], bhund50[32,1], bhund50[37,1], bhund50[42,1], bhund50[47,1])/sum(bhund50)),
                    (sum(bhund50[3,1], bhund50[8,1], bhund50[13,1], bhund50[18,1], bhund50[23,1],
                         bhund50[28,1], bhund50[33,1], bhund50[38,1], bhund50[43,1], bhund50[48,1])/sum(bhund50)),
                    (sum(bhund50[4,1], bhund50[9,1], bhund50[14,1], bhund50[19,1], bhund50[24,1],
                         bhund50[29,1], bhund50[34,1], bhund50[39,1], bhund50[44,1], bhund50[49,1])/sum(bhund50)),
                    (sum(bhund50[5,1], bhund50[10,1], bhund50[15,1], bhund50[20,1], bhund50[25,1],
                         bhund50[30,1], bhund50[35,1], bhund50[40,1], bhund50[45,1], bhund50[50,1])/sum(bhund50)))

blackdisttwohund = c((sum(btwohund[1,1], btwohund[6,1], btwohund[11,1], btwohund[16,1], btwohund[21,1],
                          btwohund[26,1], btwohund[31,1], btwohund[36,1], btwohund[41,1], btwohund[46,1])/sum(btwohund)), 
                     (sum(btwohund[2,1], btwohund[7,1], btwohund[12,1], btwohund[17,1], btwohund[22,1],
                          btwohund[27,1], btwohund[32,1], btwohund[37,1], btwohund[42,1], btwohund[47,1])/sum(btwohund)),
                     (sum(btwohund[3,1], btwohund[8,1], btwohund[13,1], btwohund[18,1], btwohund[23,1],
                          btwohund[28,1], btwohund[33,1], btwohund[38,1], btwohund[43,1], btwohund[48,1])/sum(btwohund)),
                     (sum(btwohund[4,1], btwohund[9,1], btwohund[14,1], btwohund[19,1], btwohund[24,1],
                          btwohund[29,1], btwohund[34,1], btwohund[39,1], btwohund[44,1], btwohund[49,1])/sum(btwohund)),
                     (sum(btwohund[5,1], btwohund[10,1], btwohund[15,1], btwohund[20,1], btwohund[25,1],
                          btwohund[30,1], btwohund[35,1], btwohund[40,1], btwohund[45,1], btwohund[50,1])/sum(btwohund)))

blackdisttwohund50 = c((sum(btwohund50[1,1], btwohund50[6,1], btwohund50[11,1], btwohund50[16,1], btwohund50[21,1],
                            btwohund50[26,1], btwohund50[31,1], btwohund50[36,1], btwohund50[41,1], btwohund50[46,1])/sum(btwohund50)), 
                       (sum(btwohund50[2,1], btwohund50[7,1], btwohund50[12,1], btwohund50[17,1], btwohund50[22,1],
                            btwohund50[27,1], btwohund50[32,1], btwohund50[37,1], btwohund50[42,1], btwohund50[47,1])/sum(btwohund50)),
                       (sum(btwohund50[3,1], btwohund50[8,1], btwohund50[13,1], btwohund50[18,1], btwohund50[23,1],
                            btwohund50[28,1], btwohund50[33,1], btwohund50[38,1], btwohund50[43,1], btwohund50[48,1])/sum(btwohund50)),
                       (sum(btwohund50[4,1], btwohund50[9,1], btwohund50[14,1], btwohund50[19,1], btwohund50[24,1],
                            btwohund50[29,1], btwohund50[34,1], btwohund50[39,1], btwohund50[44,1], btwohund50[49,1])/sum(btwohund50)),
                       (sum(btwohund50[5,1], btwohund50[10,1], btwohund50[15,1], btwohund50[20,1], btwohund50[25,1],
                            btwohund50[30,1], btwohund50[35,1], btwohund50[40,1], btwohund50[45,1], btwohund50[50,1])/sum(btwohund50)))

blackdistthreehund = c((sum(bthreehund[1,1], bthreehund[6,1], bthreehund[11,1], bthreehund[16,1], bthreehund[21,1],
                            bthreehund[26,1], bthreehund[31,1], bthreehund[36,1], bthreehund[41,1], bthreehund[46,1])/sum(bthreehund)), 
                       (sum(bthreehund[2,1], bthreehund[7,1], bthreehund[12,1], bthreehund[17,1], bthreehund[22,1],
                            bthreehund[27,1], bthreehund[32,1], bthreehund[37,1], bthreehund[42,1], bthreehund[47,1])/sum(bthreehund)),
                       (sum(bthreehund[3,1], bthreehund[8,1], bthreehund[13,1], bthreehund[18,1], bthreehund[23,1],
                            bthreehund[28,1], bthreehund[33,1], bthreehund[38,1], bthreehund[43,1], bthreehund[48,1])/sum(bthreehund)),
                       (sum(bthreehund[4,1], bthreehund[9,1], bthreehund[14,1], bthreehund[19,1], bthreehund[24,1],
                            bthreehund[29,1], bthreehund[34,1], bthreehund[39,1], bthreehund[44,1], bthreehund[49,1])/sum(bthreehund)),
                       (sum(bthreehund[5,1], bthreehund[10,1], bthreehund[15,1], bthreehund[20,1], bthreehund[25,1],
                            bthreehund[30,1], bthreehund[35,1], bthreehund[40,1], bthreehund[45,1], bthreehund[50,1])/sum(bthreehund)))

blackprojection <- cbind(bfive, bten, btwenty, bthirty, bforty, bfifty, bsixty, bseventy, beighty,
                         bninety, bhund, bhund50, btwohund, btwohund50, bthreehund)

black.dist <- data.frame(blackdistfive,blackdistten,blackdisttwenty,blackdistthirty,blackdistforty,
                         blackdistfifty,blackdistsixty,blackdistseventy,blackdisteighty,
                         blackdistninety,blackdisthund, blackdisthund50, blackdisttwohund, 
                         blackdisttwohund50, blackdistthreehund)

## Whites
wfive=Mmatrixwhite %*% whitepop
wten=(Mmatrixwhite %^% 2) %*% whitepop
wtwenty=(Mmatrixwhite %^% 4) %*% whitepop
wthirty=(Mmatrixwhite %^% 6) %*% whitepop
wforty=(Mmatrixwhite %^% 8) %*% whitepop
wfifty=(Mmatrixwhite %^% 10) %*% whitepop
wsixty=(Mmatrixwhite %^% 12) %*% whitepop
wseventy=(Mmatrixwhite %^% 14) %*% whitepop
weighty=(Mmatrixwhite %^% 16) %*% whitepop
wninety=(Mmatrixwhite %^% 18) %*% whitepop
whund=(Mmatrixwhite %^% 20) %*% whitepop
whund50=(Mmatrixwhite %^% 30) %*% whitepop
wtwohund=(Mmatrixblack %^% 40) %*% whitepop
wtwohund50=(Mmatrixblack %^% 50) %*% whitepop
wthreehund=(Mmatrixblack%^% 60) %*% whitepop

whitedistfive = c((sum(wfive[1,1], wfive[6,1], wfive[11,1], wfive[16,1], wfive[21,1],
                       wfive[26,1], wfive[31,1], wfive[36,1], wfive[41,1], wfive[46,1])/sum(wfive)), 
                  (sum(wfive[2,1], wfive[7,1], wfive[12,1], wfive[17,1], wfive[22,1],
                       wfive[27,1], wfive[32,1], wfive[37,1], wfive[42,1], wfive[47,1])/sum(wfive)),
                  (sum(wfive[3,1], wfive[8,1], wfive[13,1], wfive[18,1], wfive[23,1],
                       wfive[28,1], wfive[33,1], wfive[38,1], wfive[43,1], wfive[48,1])/sum(wfive)),
                  (sum(wfive[4,1], wfive[9,1], wfive[14,1], wfive[19,1], wfive[24,1],
                       wfive[29,1], wfive[34,1], wfive[39,1], wfive[44,1], wfive[49,1])/sum(wfive)),
                  (sum(wfive[5,1], wfive[10,1], wfive[15,1], wfive[20,1], wfive[25,1],
                       wfive[30,1], wfive[35,1], wfive[40,1], wfive[45,1], wfive[50,1])/sum(wfive)))

whitedistten = c((sum(wten[1,1], wten[6,1], wten[11,1], wten[16,1], wten[21,1],
                      wten[26,1], wten[31,1], wten[36,1], wten[41,1], wten[46,1])/sum(wten)), 
                 (sum(wten[2,1], wten[7,1], wten[12,1], wten[17,1], wten[22,1],
                      wten[27,1], wten[32,1], wten[37,1], wten[42,1], wten[47,1])/sum(wten)),
                 (sum(wten[3,1], wten[8,1], wten[13,1], wten[18,1], wten[23,1],
                      wten[28,1], wten[33,1], wten[38,1], wten[43,1], wten[48,1])/sum(wten)),
                 (sum(wten[4,1], wten[9,1], wten[14,1], wten[19,1], wten[24,1],
                      wten[29,1], wten[34,1], wten[39,1], wten[44,1], wten[49,1])/sum(wten)),
                 (sum(wten[5,1], wten[10,1], wten[15,1], wten[20,1], wten[25,1],
                      wten[30,1], wten[35,1], wten[40,1], wten[45,1], wten[50,1])/sum(wten)))

whitedisttwenty = c((sum(wtwenty[1,1], wtwenty[6,1], wtwenty[11,1], wtwenty[16,1], wtwenty[21,1],
                         wtwenty[26,1], wtwenty[31,1], wtwenty[36,1], wtwenty[41,1], wtwenty[46,1])/sum(wtwenty)), 
                    (sum(wtwenty[2,1], wtwenty[7,1], wtwenty[12,1], wtwenty[17,1], wtwenty[22,1],
                         wtwenty[27,1], wtwenty[32,1], wtwenty[37,1], wtwenty[42,1], wtwenty[47,1])/sum(wtwenty)),
                    (sum(wtwenty[3,1], wtwenty[8,1], wtwenty[13,1], wtwenty[18,1], wtwenty[23,1],
                         wtwenty[28,1], wtwenty[33,1], wtwenty[38,1], wtwenty[43,1], wtwenty[48,1])/sum(wtwenty)),
                    (sum(wtwenty[4,1], wtwenty[9,1], wtwenty[14,1], wtwenty[19,1], wtwenty[24,1],
                         wtwenty[29,1], wtwenty[34,1], wtwenty[39,1], wtwenty[44,1], wtwenty[49,1])/sum(wtwenty)),
                    (sum(wtwenty[5,1], wtwenty[10,1], wtwenty[15,1], wtwenty[20,1], wtwenty[25,1],
                         wtwenty[30,1], wtwenty[35,1], wtwenty[40,1], wtwenty[45,1], wtwenty[50,1])/sum(wtwenty)))

whitedistthirty = c((sum(wthirty[1,1], wthirty[6,1], wthirty[11,1], wthirty[16,1], wthirty[21,1],
                         wthirty[26,1], wthirty[31,1], wthirty[36,1], wthirty[41,1], wthirty[46,1])/sum(wthirty)), 
                    (sum(wthirty[2,1], wthirty[7,1], wthirty[12,1], wthirty[17,1], wthirty[22,1],
                         wthirty[27,1], wthirty[32,1], wthirty[37,1], wthirty[42,1], wthirty[47,1])/sum(wthirty)),
                    (sum(wthirty[3,1], wthirty[8,1], wthirty[13,1], wthirty[18,1], wthirty[23,1],
                         wthirty[28,1], wthirty[33,1], wthirty[38,1], wthirty[43,1], wthirty[48,1])/sum(wthirty)),
                    (sum(wthirty[4,1], wthirty[9,1], wthirty[14,1], wthirty[19,1], wthirty[24,1],
                         wthirty[29,1], wthirty[34,1], wthirty[39,1], wthirty[44,1], wthirty[49,1])/sum(wthirty)),
                    (sum(wthirty[5,1], wthirty[10,1], wthirty[15,1], wthirty[20,1], wthirty[25,1],
                         wthirty[30,1], wthirty[35,1], wthirty[40,1], wthirty[45,1], wthirty[50,1])/sum(wthirty)))

whitedistforty = c((sum(wforty[1,1], wforty[6,1], wforty[11,1], wforty[16,1], wforty[21,1],
                        wforty[26,1], wforty[31,1], wforty[36,1], wforty[41,1], wforty[46,1])/sum(wforty)), 
                   (sum(wforty[2,1], wforty[7,1], wforty[12,1], wforty[17,1], wforty[22,1],
                        wforty[27,1], wforty[32,1], wforty[37,1], wforty[42,1], wforty[47,1])/sum(wforty)),
                   (sum(wforty[3,1], wforty[8,1], wforty[13,1], wforty[18,1], wforty[23,1],
                        wforty[28,1], wforty[33,1], wforty[38,1], wforty[43,1], wforty[48,1])/sum(wforty)),
                   (sum(wforty[4,1], wforty[9,1], wforty[14,1], wforty[19,1], wforty[24,1],
                        wforty[29,1], wforty[34,1], wforty[39,1], wforty[44,1], wforty[49,1])/sum(wforty)),
                   (sum(wforty[5,1], wforty[10,1], wforty[15,1], wforty[20,1], wforty[25,1],
                        wforty[30,1], wforty[35,1], wforty[40,1], wforty[45,1], wforty[50,1])/sum(wforty)))

whitedistfifty = c((sum(wfifty[1,1], wfifty[6,1], wfifty[11,1], wfifty[16,1], wfifty[21,1],
                        wfifty[26,1], wfifty[31,1], wfifty[36,1], wfifty[41,1], wfifty[46,1])/sum(wfifty)), 
                   (sum(wfifty[2,1], wfifty[7,1], wfifty[12,1], wfifty[17,1], wfifty[22,1],
                        wfifty[27,1], wfifty[32,1], wfifty[37,1], wfifty[42,1], wfifty[47,1])/sum(wfifty)),
                   (sum(wfifty[3,1], wfifty[8,1], wfifty[13,1], wfifty[18,1], wfifty[23,1],
                        wfifty[28,1], wfifty[33,1], wfifty[38,1], wfifty[43,1], wfifty[48,1])/sum(wfifty)),
                   (sum(wfifty[4,1], wfifty[9,1], wfifty[14,1], wfifty[19,1], wfifty[24,1],
                        wfifty[29,1], wfifty[34,1], wfifty[39,1], wfifty[44,1], wfifty[49,1])/sum(wfifty)),
                   (sum(wfifty[5,1], wfifty[10,1], wfifty[15,1], wfifty[20,1], wfifty[25,1],
                        wfifty[30,1], wfifty[35,1], wfifty[40,1], wfifty[45,1], wfifty[50,1])/sum(wfifty)))

whitedistsixty = c((sum(wsixty[1,1], wsixty[6,1], wsixty[11,1], wsixty[16,1], wsixty[21,1],
                        wsixty[26,1], wsixty[31,1], wsixty[36,1], wsixty[41,1], wsixty[46,1])/sum(wsixty)), 
                   (sum(wsixty[2,1], wsixty[7,1], wsixty[12,1], wsixty[17,1], wsixty[22,1],
                        wsixty[27,1], wsixty[32,1], wsixty[37,1], wsixty[42,1], wsixty[47,1])/sum(wsixty)),
                   (sum(wsixty[3,1], wsixty[8,1], wsixty[13,1], wsixty[18,1], wsixty[23,1],
                        wsixty[28,1], wsixty[33,1], wsixty[38,1], wsixty[43,1], wsixty[48,1])/sum(wsixty)),
                   (sum(wsixty[4,1], wsixty[9,1], wsixty[14,1], wsixty[19,1], wsixty[24,1],
                        wsixty[29,1], wsixty[34,1], wsixty[39,1], wsixty[44,1], wsixty[49,1])/sum(wsixty)),
                   (sum(wsixty[5,1], wsixty[10,1], wsixty[15,1], wsixty[20,1], wsixty[25,1],
                        wsixty[30,1], wsixty[35,1], wsixty[40,1], wsixty[45,1], wsixty[50,1])/sum(wsixty)))

whitedistseventy = c((sum(wseventy[1,1], wseventy[6,1], wseventy[11,1], wseventy[16,1], wseventy[21,1],
                          wseventy[26,1], wseventy[31,1], wseventy[36,1], wseventy[41,1], wseventy[46,1])/sum(wseventy)), 
                     (sum(wseventy[2,1], wseventy[7,1], wseventy[12,1], wseventy[17,1], wseventy[22,1],
                          wseventy[27,1], wseventy[32,1], wseventy[37,1], wseventy[42,1], wseventy[47,1])/sum(wseventy)),
                     (sum(wseventy[3,1], wseventy[8,1], wseventy[13,1], wseventy[18,1], wseventy[23,1],
                          wseventy[28,1], wseventy[33,1], wseventy[38,1], wseventy[43,1], wseventy[48,1])/sum(wseventy)),
                     (sum(wseventy[4,1], wseventy[9,1], wseventy[14,1], wseventy[19,1], wseventy[24,1],
                          wseventy[29,1], wseventy[34,1], wseventy[39,1], wseventy[44,1], wseventy[49,1])/sum(wseventy)),
                     (sum(wseventy[5,1], wseventy[10,1], wseventy[15,1], wseventy[20,1], wseventy[25,1],
                          wseventy[30,1], wseventy[35,1], wseventy[40,1], wseventy[45,1], wseventy[50,1])/sum(wseventy)))

whitedisteighty = c((sum(weighty[1,1], weighty[6,1], weighty[11,1], weighty[16,1], weighty[21,1],
                         weighty[26,1], weighty[31,1], weighty[36,1], weighty[41,1], weighty[46,1])/sum(weighty)), 
                    (sum(weighty[2,1], weighty[7,1], weighty[12,1], weighty[17,1], weighty[22,1],
                         weighty[27,1], weighty[32,1], weighty[37,1], weighty[42,1], weighty[47,1])/sum(weighty)),
                    (sum(weighty[3,1], weighty[8,1], weighty[13,1], weighty[18,1], weighty[23,1],
                         weighty[28,1], weighty[33,1], weighty[38,1], weighty[43,1], weighty[48,1])/sum(weighty)),
                    (sum(weighty[4,1], weighty[9,1], weighty[14,1], weighty[19,1], weighty[24,1],
                         weighty[29,1], weighty[34,1], weighty[39,1], weighty[44,1], weighty[49,1])/sum(weighty)),
                    (sum(weighty[5,1], weighty[10,1], weighty[15,1], weighty[20,1], weighty[25,1],
                         weighty[30,1], weighty[35,1], weighty[40,1], weighty[45,1], weighty[50,1])/sum(weighty)))

whitedistninety = c((sum(wninety[1,1], wninety[6,1], wninety[11,1], wninety[16,1], wninety[21,1],
                         wninety[26,1], wninety[31,1], wninety[36,1], wninety[41,1], wninety[46,1])/sum(wninety)), 
                    (sum(wninety[2,1], wninety[7,1], wninety[12,1], wninety[17,1], wninety[22,1],
                         wninety[27,1], wninety[32,1], wninety[37,1], wninety[42,1], wninety[47,1])/sum(wninety)),
                    (sum(wninety[3,1], wninety[8,1], wninety[13,1], wninety[18,1], wninety[23,1],
                         wninety[28,1], wninety[33,1], wninety[38,1], wninety[43,1], wninety[48,1])/sum(wninety)),
                    (sum(wninety[4,1], wninety[9,1], wninety[14,1], wninety[19,1], wninety[24,1],
                         wninety[29,1], wninety[34,1], wninety[39,1], wninety[44,1], wninety[49,1])/sum(wninety)),
                    (sum(wninety[5,1], wninety[10,1], wninety[15,1], wninety[20,1], wninety[25,1],
                         wninety[30,1], wninety[35,1], wninety[40,1], wninety[45,1], wninety[50,1])/sum(wninety)))

whitedisthund = c((sum(whund[1,1], whund[6,1], whund[11,1], whund[16,1], whund[21,1],
                       whund[26,1], whund[31,1], whund[36,1], whund[41,1], whund[46,1])/sum(whund)), 
                  (sum(whund[2,1], whund[7,1], whund[12,1], whund[17,1], whund[22,1],
                       whund[27,1], whund[32,1], whund[37,1], whund[42,1], whund[47,1])/sum(whund)),
                  (sum(whund[3,1], whund[8,1], whund[13,1], whund[18,1], whund[23,1],
                       whund[28,1], whund[33,1], whund[38,1], whund[43,1], whund[48,1])/sum(whund)),
                  (sum(whund[4,1], whund[9,1], whund[14,1], whund[19,1], whund[24,1],
                       whund[29,1], whund[34,1], whund[39,1], whund[44,1], whund[49,1])/sum(whund)),
                  (sum(whund[5,1], whund[10,1], whund[15,1], whund[20,1], whund[25,1],
                       whund[30,1], whund[35,1], whund[40,1], whund[45,1], whund[50,1])/sum(whund)))

whitedisthund50 = c((sum(whund50[1,1], whund50[6,1], whund50[11,1], whund50[16,1], whund50[21,1],
                         whund50[26,1], whund50[31,1], whund50[36,1], whund50[41,1], whund50[46,1])/sum(whund50)), 
                    (sum(whund50[2,1], whund50[7,1], whund50[12,1], whund50[17,1], whund50[22,1],
                         whund50[27,1], whund50[32,1], whund50[37,1], whund50[42,1], whund50[47,1])/sum(whund50)),
                    (sum(whund50[3,1], whund50[8,1], whund50[13,1], whund50[18,1], whund50[23,1],
                         whund50[28,1], whund50[33,1], whund50[38,1], whund50[43,1], whund50[48,1])/sum(whund50)),
                    (sum(whund50[4,1], whund50[9,1], whund50[14,1], whund50[19,1], whund50[24,1],
                         whund50[29,1], whund50[34,1], whund50[39,1], whund50[44,1], whund50[49,1])/sum(whund50)),
                    (sum(whund50[5,1], whund50[10,1], whund50[15,1], whund50[20,1], whund50[25,1],
                         whund50[30,1], whund50[35,1], whund50[40,1], whund50[45,1], whund50[50,1])/sum(whund50)))

whitedisttwohund = c((sum(wtwohund[1,1], wtwohund[6,1], wtwohund[11,1], wtwohund[16,1], wtwohund[21,1],
                          wtwohund[26,1], wtwohund[31,1], wtwohund[36,1], wtwohund[41,1], wtwohund[46,1])/sum(wtwohund)), 
                     (sum(wtwohund[2,1], wtwohund[7,1], wtwohund[12,1], wtwohund[17,1], wtwohund[22,1],
                          wtwohund[27,1], wtwohund[32,1], wtwohund[37,1], wtwohund[42,1], wtwohund[47,1])/sum(wtwohund)),
                     (sum(wtwohund[3,1], wtwohund[8,1], wtwohund[13,1], wtwohund[18,1], wtwohund[23,1],
                          wtwohund[28,1], wtwohund[33,1], wtwohund[38,1], wtwohund[43,1], wtwohund[48,1])/sum(wtwohund)),
                     (sum(wtwohund[4,1], wtwohund[9,1], wtwohund[14,1], wtwohund[19,1], wtwohund[24,1],
                          wtwohund[29,1], wtwohund[34,1], wtwohund[39,1], wtwohund[44,1], wtwohund[49,1])/sum(wtwohund)),
                     (sum(wtwohund[5,1], wtwohund[10,1], wtwohund[15,1], wtwohund[20,1], wtwohund[25,1],
                          wtwohund[30,1], wtwohund[35,1], wtwohund[40,1], wtwohund[45,1], wtwohund[50,1])/sum(wtwohund)))

whitedisttwohund50 = c((sum(wtwohund50[1,1], wtwohund50[6,1], wtwohund50[11,1], wtwohund50[16,1], wtwohund50[21,1],
                            wtwohund50[26,1], wtwohund50[31,1], wtwohund50[36,1], wtwohund50[41,1], wtwohund50[46,1])/sum(wtwohund50)), 
                       (sum(wtwohund50[2,1], wtwohund50[7,1], wtwohund50[12,1], wtwohund50[17,1], wtwohund50[22,1],
                            wtwohund50[27,1], wtwohund50[32,1], wtwohund50[37,1], wtwohund50[42,1], wtwohund50[47,1])/sum(wtwohund50)),
                       (sum(wtwohund50[3,1], wtwohund50[8,1], wtwohund50[13,1], wtwohund50[18,1], wtwohund50[23,1],
                            wtwohund50[28,1], wtwohund50[33,1], wtwohund50[38,1], wtwohund50[43,1], wtwohund50[48,1])/sum(wtwohund50)),
                       (sum(wtwohund50[4,1], wtwohund50[9,1], wtwohund50[14,1], wtwohund50[19,1], wtwohund50[24,1],
                            wtwohund50[29,1], wtwohund50[34,1], wtwohund50[39,1], wtwohund50[44,1], wtwohund50[49,1])/sum(wtwohund50)),
                       (sum(wtwohund50[5,1], wtwohund50[10,1], wtwohund50[15,1], wtwohund50[20,1], wtwohund50[25,1],
                            wtwohund50[30,1], wtwohund50[35,1], wtwohund50[40,1], wtwohund50[45,1], wtwohund50[50,1])/sum(wtwohund50)))

whitedistthreehund = c((sum(wthreehund[1,1], wthreehund[6,1], wthreehund[11,1], wthreehund[16,1], wthreehund[21,1],
                            wthreehund[26,1], wthreehund[31,1], wthreehund[36,1], wthreehund[41,1], wthreehund[46,1])/sum(wthreehund)), 
                       (sum(wthreehund[2,1], wthreehund[7,1], wthreehund[12,1], wthreehund[17,1], wthreehund[22,1],
                            wthreehund[27,1], wthreehund[32,1], wthreehund[37,1], wthreehund[42,1], wthreehund[47,1])/sum(wthreehund)),
                       (sum(wthreehund[3,1], wthreehund[8,1], wthreehund[13,1], wthreehund[18,1], wthreehund[23,1],
                            wthreehund[28,1], wthreehund[33,1], wthreehund[38,1], wthreehund[43,1], wthreehund[48,1])/sum(wthreehund)),
                       (sum(wthreehund[4,1], wthreehund[9,1], wthreehund[14,1], wthreehund[19,1], wthreehund[24,1],
                            wthreehund[29,1], wthreehund[34,1], wthreehund[39,1], wthreehund[44,1], wthreehund[49,1])/sum(wthreehund)),
                       (sum(wthreehund[5,1], wthreehund[10,1], wthreehund[15,1], wthreehund[20,1], wthreehund[25,1],
                            wthreehund[30,1], wthreehund[35,1], wthreehund[40,1], wthreehund[45,1], wthreehund[50,1])/sum(wthreehund)))


whiteprojection <- cbind(wfive, wten, wtwenty, wthirty, wforty, wfifty, wsixty, wseventy, weighty,
                         wninety, whund, whund50, wtwohund, wtwohund50, wthreehund)

white.dist <- data.frame(whitedistfive,whitedistten,whitedisttwenty,whitedistthirty,whitedistforty,
                         whitedistfifty,whitedistsixty,whitedistseventy,whitedisteighty,
                         whitedistninety,whitedisthund, whitedisthund50, whitedisttwohund,
                         whitedisttwohund50, whitedistthreehund)

whitedist <- as.data.frame(t(white.dist))
education <- c(1, 2, 3, 4, 5)

## 3. To what degree are differences in the (equilibrium) education distributions for Black and White
## people attributable to differences in fertility patterns between Black and White women?
## Black women having White Fertility Rates
{
  #Scalar*(Fertility+Survival*Fertility)
  black$Xf <- black$L*(white$Fx+black$S*white$Fx)
  for (i in 1:5) {
    for (j in c(15,20,25,30,35,40)) {
      black$Xf[black$education==i & black$age==j]<-black$L[black$education==i & 
                                                             black$age==j]*(white$Fx[black$education==i & black$age==j]+
                                                                              black$nLx[black$education==i & black$age==j]/
                                                                              black$nLx[black$education==i & black$age==j-5]*
                                                                              white$Fx[black$education==i & black$age==j])
    }
  }
  
  black15f <- matrix(black$Xf[black$age==15], nrow=5, ncol=1)
  black20f <- matrix(black$Xf[black$age==20], nrow=5, ncol=1)
  black25f <- matrix(black$Xf[black$age==25], nrow=5, ncol=1)
  black30f <- matrix(black$Xf[black$age==30], nrow=5, ncol=1)
  black35f <- matrix(black$Xf[black$age==35], nrow=5, ncol=1)
  black40f <- matrix(black$Xf[black$age==40], nrow=5, ncol=1)
  
  #Births (5x1 matrix above multiplied by mobility matrix 5x5)
  bblack15f <- as.vector(black15f)*mblack
  bblack20f <- as.vector(black20f)*mblack
  bblack25f <- as.vector(black25f)*mblack
  bblack30f <- as.vector(black30f)*mblack
  bblack35f <- as.vector(black35f)*mblack
  bblack40f <- as.vector(black40f)*mblack
  
  matrixCbf <-cbind(zero, zero, zero, bblack15f, bblack20f, bblack25f, bblack30f, bblack35f, bblack40f, zero)
  
  Mmatrixblackf <- rbind(matrixCbf, matrixMb)
  
  ffive=Mmatrixblackf %*% blackpop
  ften=(Mmatrixblackf %^% 2) %*% blackpop
  ftwenty=(Mmatrixblackf %^% 4) %*% blackpop
  fthirty=(Mmatrixblackf %^% 6) %*% blackpop
  fforty=(Mmatrixblackf %^% 8) %*% blackpop
  ffifty=(Mmatrixblackf %^% 10) %*% blackpop
  fsixty=(Mmatrixblackf %^% 12) %*% blackpop
  fseventy=(Mmatrixblackf %^% 14) %*% blackpop
  feighty=(Mmatrixblackf %^% 16) %*% blackpop
  fninety=(Mmatrixblackf %^% 18) %*% blackpop
  fhund=(Mmatrixblackf %^% 20) %*% blackpop
  fhund50=(Mmatrixblackf %^% 30) %*% blackpop
  ftwohund=(Mmatrixblackf %^% 40) %*% blackpop
  ftwohund50=(Mmatrixblackf %^% 50) %*% blackpop
  fthreehund=(Mmatrixblackf %^% 60) %*% blackpop
  
  fdistfive = c((sum(ffive[1,1], ffive[6,1], ffive[11,1], ffive[16,1], ffive[21,1],
                     ffive[26,1], ffive[31,1], ffive[36,1], ffive[41,1], ffive[46,1])/sum(ffive)), 
                (sum(ffive[2,1], ffive[7,1], ffive[12,1], ffive[17,1], ffive[22,1],
                     ffive[27,1], ffive[32,1], ffive[37,1], ffive[42,1], ffive[47,1])/sum(ffive)),
                (sum(ffive[3,1], ffive[8,1], ffive[13,1], ffive[18,1], ffive[23,1],
                     ffive[28,1], ffive[33,1], ffive[38,1], ffive[43,1], ffive[48,1])/sum(ffive)),
                (sum(ffive[4,1], ffive[9,1], ffive[14,1], ffive[19,1], ffive[24,1],
                     ffive[29,1], ffive[34,1], ffive[39,1], ffive[44,1], ffive[49,1])/sum(ffive)),
                (sum(ffive[5,1], ffive[10,1], ffive[15,1], ffive[20,1], ffive[25,1],
                     ffive[30,1], ffive[35,1], ffive[40,1], ffive[45,1], ffive[50,1])/sum(ffive)))
  
  fdistten = c((sum(ften[1,1], ften[6,1], ften[11,1], ften[16,1], ften[21,1],
                    ften[26,1], ften[31,1], ften[36,1], ften[41,1], ften[46,1])/sum(ften)), 
               (sum(ften[2,1], ften[7,1], ften[12,1], ften[17,1], ften[22,1],
                    ften[27,1], ften[32,1], ften[37,1], ften[42,1], ften[47,1])/sum(ften)),
               (sum(ften[3,1], ften[8,1], ften[13,1], ften[18,1], ften[23,1],
                    ften[28,1], ften[33,1], ften[38,1], ften[43,1], ften[48,1])/sum(ften)),
               (sum(ften[4,1], ften[9,1], ften[14,1], ften[19,1], ften[24,1],
                    ften[29,1], ften[34,1], ften[39,1], ften[44,1], ften[49,1])/sum(ften)),
               (sum(ften[5,1], ften[10,1], ften[15,1], ften[20,1], ften[25,1],
                    ften[30,1], ften[35,1], ften[40,1], ften[45,1], ften[50,1])/sum(ften)))
  
  fdisttwenty = c((sum(ftwenty[1,1], ftwenty[6,1], ftwenty[11,1], ftwenty[16,1], ftwenty[21,1],
                       ftwenty[26,1], ftwenty[31,1], ftwenty[36,1], ftwenty[41,1], ftwenty[46,1])/sum(ftwenty)), 
                  (sum(ftwenty[2,1], ftwenty[7,1], ftwenty[12,1], ftwenty[17,1], ftwenty[22,1],
                       ftwenty[27,1], ftwenty[32,1], ftwenty[37,1], ftwenty[42,1], ftwenty[47,1])/sum(ftwenty)),
                  (sum(ftwenty[3,1], ftwenty[8,1], ftwenty[13,1], ftwenty[18,1], ftwenty[23,1],
                       ftwenty[28,1], ftwenty[33,1], ftwenty[38,1], ftwenty[43,1], ftwenty[48,1])/sum(ftwenty)),
                  (sum(ftwenty[4,1], ftwenty[9,1], ftwenty[14,1], ftwenty[19,1], ftwenty[24,1],
                       ftwenty[29,1], ftwenty[34,1], ftwenty[39,1], ftwenty[44,1], ftwenty[49,1])/sum(ftwenty)),
                  (sum(ftwenty[5,1], ftwenty[10,1], ftwenty[15,1], ftwenty[20,1], ftwenty[25,1],
                       ftwenty[30,1], ftwenty[35,1], ftwenty[40,1], ftwenty[45,1], ftwenty[50,1])/sum(ftwenty)))
  
  fdistthirty = c((sum(fthirty[1,1], fthirty[6,1], fthirty[11,1], fthirty[16,1], fthirty[21,1],
                       fthirty[26,1], fthirty[31,1], fthirty[36,1], fthirty[41,1], fthirty[46,1])/sum(fthirty)), 
                  (sum(fthirty[2,1], fthirty[7,1], fthirty[12,1], fthirty[17,1], fthirty[22,1],
                       fthirty[27,1], fthirty[32,1], fthirty[37,1], fthirty[42,1], fthirty[47,1])/sum(fthirty)),
                  (sum(fthirty[3,1], fthirty[8,1], fthirty[13,1], fthirty[18,1], fthirty[23,1],
                       fthirty[28,1], fthirty[33,1], fthirty[38,1], fthirty[43,1], fthirty[48,1])/sum(fthirty)),
                  (sum(fthirty[4,1], fthirty[9,1], fthirty[14,1], fthirty[19,1], fthirty[24,1],
                       fthirty[29,1], fthirty[34,1], fthirty[39,1], fthirty[44,1], fthirty[49,1])/sum(fthirty)),
                  (sum(fthirty[5,1], fthirty[10,1], fthirty[15,1], fthirty[20,1], fthirty[25,1],
                       fthirty[30,1], fthirty[35,1], fthirty[40,1], fthirty[45,1], fthirty[50,1])/sum(fthirty)))
  
  fdistforty = c((sum(fforty[1,1], fforty[6,1], fforty[11,1], fforty[16,1], fforty[21,1],
                      fforty[26,1], fforty[31,1], fforty[36,1], fforty[41,1], fforty[46,1])/sum(fforty)), 
                 (sum(fforty[2,1], fforty[7,1], fforty[12,1], fforty[17,1], fforty[22,1],
                      fforty[27,1], fforty[32,1], fforty[37,1], fforty[42,1], fforty[47,1])/sum(fforty)),
                 (sum(fforty[3,1], fforty[8,1], fforty[13,1], fforty[18,1], fforty[23,1],
                      fforty[28,1], fforty[33,1], fforty[38,1], fforty[43,1], fforty[48,1])/sum(fforty)),
                 (sum(fforty[4,1], fforty[9,1], fforty[14,1], fforty[19,1], fforty[24,1],
                      fforty[29,1], fforty[34,1], fforty[39,1], fforty[44,1], fforty[49,1])/sum(fforty)),
                 (sum(fforty[5,1], fforty[10,1], fforty[15,1], fforty[20,1], fforty[25,1],
                      fforty[30,1], fforty[35,1], fforty[40,1], fforty[45,1], fforty[50,1])/sum(fforty)))
  
  fdistfifty = c((sum(ffifty[1,1], ffifty[6,1], ffifty[11,1], ffifty[16,1], ffifty[21,1],
                      ffifty[26,1], ffifty[31,1], ffifty[36,1], ffifty[41,1], ffifty[46,1])/sum(ffifty)), 
                 (sum(ffifty[2,1], ffifty[7,1], ffifty[12,1], ffifty[17,1], ffifty[22,1],
                      ffifty[27,1], ffifty[32,1], ffifty[37,1], ffifty[42,1], ffifty[47,1])/sum(ffifty)),
                 (sum(ffifty[3,1], ffifty[8,1], ffifty[13,1], ffifty[18,1], ffifty[23,1],
                      ffifty[28,1], ffifty[33,1], ffifty[38,1], ffifty[43,1], ffifty[48,1])/sum(ffifty)),
                 (sum(ffifty[4,1], ffifty[9,1], ffifty[14,1], ffifty[19,1], ffifty[24,1],
                      ffifty[29,1], ffifty[34,1], ffifty[39,1], ffifty[44,1], ffifty[49,1])/sum(ffifty)),
                 (sum(ffifty[5,1], ffifty[10,1], ffifty[15,1], ffifty[20,1], ffifty[25,1],
                      ffifty[30,1], ffifty[35,1], ffifty[40,1], ffifty[45,1], ffifty[50,1])/sum(ffifty)))
  
  fdistsixty = c((sum(fsixty[1,1], fsixty[6,1], fsixty[11,1], fsixty[16,1], fsixty[21,1],
                      fsixty[26,1], fsixty[31,1], fsixty[36,1], fsixty[41,1], fsixty[46,1])/sum(fsixty)), 
                 (sum(fsixty[2,1], fsixty[7,1], fsixty[12,1], fsixty[17,1], fsixty[22,1],
                      fsixty[27,1], fsixty[32,1], fsixty[37,1], fsixty[42,1], fsixty[47,1])/sum(fsixty)),
                 (sum(fsixty[3,1], fsixty[8,1], fsixty[13,1], fsixty[18,1], fsixty[23,1],
                      fsixty[28,1], fsixty[33,1], fsixty[38,1], fsixty[43,1], fsixty[48,1])/sum(fsixty)),
                 (sum(fsixty[4,1], fsixty[9,1], fsixty[14,1], fsixty[19,1], fsixty[24,1],
                      fsixty[29,1], fsixty[34,1], fsixty[39,1], fsixty[44,1], fsixty[49,1])/sum(fsixty)),
                 (sum(fsixty[5,1], fsixty[10,1], fsixty[15,1], fsixty[20,1], fsixty[25,1],
                      fsixty[30,1], fsixty[35,1], fsixty[40,1], fsixty[45,1], fsixty[50,1])/sum(fsixty)))
  
  fdistseventy = c((sum(fseventy[1,1], fseventy[6,1], fseventy[11,1], fseventy[16,1], fseventy[21,1],
                        fseventy[26,1], fseventy[31,1], fseventy[36,1], fseventy[41,1], fseventy[46,1])/sum(fseventy)), 
                   (sum(fseventy[2,1], fseventy[7,1], fseventy[12,1], fseventy[17,1], fseventy[22,1],
                        fseventy[27,1], fseventy[32,1], fseventy[37,1], fseventy[42,1], fseventy[47,1])/sum(fseventy)),
                   (sum(fseventy[3,1], fseventy[8,1], fseventy[13,1], fseventy[18,1], fseventy[23,1],
                        fseventy[28,1], fseventy[33,1], fseventy[38,1], fseventy[43,1], fseventy[48,1])/sum(fseventy)),
                   (sum(fseventy[4,1], fseventy[9,1], fseventy[14,1], fseventy[19,1], fseventy[24,1],
                        fseventy[29,1], fseventy[34,1], fseventy[39,1], fseventy[44,1], fseventy[49,1])/sum(fseventy)),
                   (sum(fseventy[5,1], fseventy[10,1], fseventy[15,1], fseventy[20,1], fseventy[25,1],
                        fseventy[30,1], fseventy[35,1], fseventy[40,1], fseventy[45,1], fseventy[50,1])/sum(fseventy)))
  
  fdisteighty = c((sum(feighty[1,1], feighty[6,1], feighty[11,1], feighty[16,1], feighty[21,1],
                       feighty[26,1], feighty[31,1], feighty[36,1], feighty[41,1], feighty[46,1])/sum(feighty)), 
                  (sum(feighty[2,1], feighty[7,1], feighty[12,1], feighty[17,1], feighty[22,1],
                       feighty[27,1], feighty[32,1], feighty[37,1], feighty[42,1], feighty[47,1])/sum(feighty)),
                  (sum(feighty[3,1], feighty[8,1], feighty[13,1], feighty[18,1], feighty[23,1],
                       feighty[28,1], feighty[33,1], feighty[38,1], feighty[43,1], feighty[48,1])/sum(feighty)),
                  (sum(feighty[4,1], feighty[9,1], feighty[14,1], feighty[19,1], feighty[24,1],
                       feighty[29,1], feighty[34,1], feighty[39,1], feighty[44,1], feighty[49,1])/sum(feighty)),
                  (sum(feighty[5,1], feighty[10,1], feighty[15,1], feighty[20,1], feighty[25,1],
                       feighty[30,1], feighty[35,1], feighty[40,1], feighty[45,1], feighty[50,1])/sum(feighty)))
  
  fdistninety = c((sum(fninety[1,1], fninety[6,1], fninety[11,1], fninety[16,1], fninety[21,1],
                       fninety[26,1], fninety[31,1], fninety[36,1], fninety[41,1], fninety[46,1])/sum(fninety)), 
                  (sum(fninety[2,1], fninety[7,1], fninety[12,1], fninety[17,1], fninety[22,1],
                       fninety[27,1], fninety[32,1], fninety[37,1], fninety[42,1], fninety[47,1])/sum(fninety)),
                  (sum(fninety[3,1], fninety[8,1], fninety[13,1], fninety[18,1], fninety[23,1],
                       fninety[28,1], fninety[33,1], fninety[38,1], fninety[43,1], fninety[48,1])/sum(fninety)),
                  (sum(fninety[4,1], fninety[9,1], fninety[14,1], fninety[19,1], fninety[24,1],
                       fninety[29,1], fninety[34,1], fninety[39,1], fninety[44,1], fninety[49,1])/sum(fninety)),
                  (sum(fninety[5,1], fninety[10,1], fninety[15,1], fninety[20,1], fninety[25,1],
                       fninety[30,1], fninety[35,1], fninety[40,1], fninety[45,1], fninety[50,1])/sum(fninety)))
  
  fdisthund = c((sum(fhund[ 1,1], fhund[6,1], fhund[11,1], fhund[16,1], fhund[21,1],
                     fhund[26,1], fhund[31,1], fhund[36,1], fhund[41,1], fhund[46,1])/sum(fhund)), 
                (sum(fhund[2,1], fhund[7,1], fhund[12,1], fhund[17,1], fhund[22,1],
                     fhund[27,1], fhund[32,1], fhund[37,1], fhund[42,1], fhund[47,1])/sum(fhund)),
                (sum(fhund[3,1], fhund[8,1], fhund[13,1], fhund[18,1], fhund[23,1],
                     fhund[28,1], fhund[33,1], fhund[38,1], fhund[43,1], fhund[48,1])/sum(fhund)),
                (sum(fhund[4,1], fhund[9,1], fhund[14,1], fhund[19,1], fhund[24,1],
                     fhund[29,1], fhund[34,1], fhund[39,1], fhund[44,1], fhund[49,1])/sum(fhund)),
                (sum(fhund[5,1], fhund[10,1], fhund[15,1], fhund[20,1], fhund[25,1],
                     fhund[30,1], fhund[35,1], fhund[40,1], fhund[45,1], fhund[50,1])/sum(fhund)))
  
  fdisthund50 = c((sum(fhund50[1,1], fhund50[6,1], fhund50[11,1], fhund50[16,1], fhund50[21,1],
                       fhund50[26,1], fhund50[31,1], fhund50[36,1], fhund50[41,1], fhund50[46,1])/sum(fhund50)), 
                  (sum(fhund50[2,1], fhund50[7,1], fhund50[12,1], fhund50[17,1], fhund50[22,1],
                       fhund50[27,1], fhund50[32,1], fhund50[37,1], fhund50[42,1], fhund50[47,1])/sum(fhund50)),
                  (sum(fhund50[3,1], fhund50[8,1], fhund50[13,1], fhund50[18,1], fhund50[23,1],
                       fhund50[28,1], fhund50[33,1], fhund50[38,1], fhund50[43,1], fhund50[48,1])/sum(fhund50)),
                  (sum(fhund50[4,1], fhund50[9,1], fhund50[14,1], fhund50[19,1], fhund50[24,1],
                       fhund50[29,1], fhund50[34,1], fhund50[39,1], fhund50[44,1], fhund50[49,1])/sum(fhund50)),
                  (sum(fhund50[5,1], fhund50[10,1], fhund50[15,1], fhund50[20,1], fhund50[25,1],
                       fhund50[30,1], fhund50[35,1], fhund50[40,1], fhund50[45,1], fhund50[50,1])/sum(fhund50)))
  
  fdisttwohund = c((sum(ftwohund[1,1], ftwohund[6,1], ftwohund[11,1], ftwohund[16,1], ftwohund[21,1],
                        ftwohund[26,1], ftwohund[31,1], ftwohund[36,1], ftwohund[41,1], ftwohund[46,1])/sum(ftwohund)), 
                   (sum(ftwohund[2,1], ftwohund[7,1], ftwohund[12,1], ftwohund[17,1], ftwohund[22,1],
                        ftwohund[27,1], ftwohund[32,1], ftwohund[37,1], ftwohund[42,1], ftwohund[47,1])/sum(ftwohund)),
                   (sum(ftwohund[3,1], ftwohund[8,1], ftwohund[13,1], ftwohund[18,1], ftwohund[23,1],
                        ftwohund[28,1], ftwohund[33,1], ftwohund[38,1], ftwohund[43,1], ftwohund[48,1])/sum(ftwohund)),
                   (sum(ftwohund[4,1], ftwohund[9,1], ftwohund[14,1], ftwohund[19,1], ftwohund[24,1],
                        ftwohund[29,1], ftwohund[34,1], ftwohund[39,1], ftwohund[44,1], ftwohund[49,1])/sum(ftwohund)),
                   (sum(ftwohund[5,1], ftwohund[10,1], ftwohund[15,1], ftwohund[20,1], ftwohund[25,1],
                        ftwohund[30,1], ftwohund[35,1], ftwohund[40,1], ftwohund[45,1], ftwohund[50,1])/sum(ftwohund)))
  
  fdisttwohund50 = c((sum(ftwohund50[1,1], ftwohund50[6,1], ftwohund50[11,1], ftwohund50[16,1], ftwohund50[21,1],
                          ftwohund50[26,1], ftwohund50[31,1], ftwohund50[36,1], ftwohund50[41,1], ftwohund50[46,1])/sum(ftwohund50)), 
                     (sum(ftwohund50[2,1], ftwohund50[7,1], ftwohund50[12,1], ftwohund50[17,1], ftwohund50[22,1],
                          ftwohund50[27,1], ftwohund50[32,1], ftwohund50[37,1], ftwohund50[42,1], ftwohund50[47,1])/sum(ftwohund50)),
                     (sum(ftwohund50[3,1], ftwohund50[8,1], ftwohund50[13,1], ftwohund50[18,1], ftwohund50[23,1],
                          ftwohund50[28,1], ftwohund50[33,1], ftwohund50[38,1], ftwohund50[43,1], ftwohund50[48,1])/sum(ftwohund50)),
                     (sum(ftwohund50[4,1], ftwohund50[9,1], ftwohund50[14,1], ftwohund50[19,1], ftwohund50[24,1],
                          ftwohund50[29,1], ftwohund50[34,1], ftwohund50[39,1], ftwohund50[44,1], ftwohund50[49,1])/sum(ftwohund50)),
                     (sum(ftwohund50[5,1], ftwohund50[10,1], ftwohund50[15,1], ftwohund50[20,1], ftwohund50[25,1],
                          ftwohund50[30,1], ftwohund50[35,1], ftwohund50[40,1], ftwohund50[45,1], ftwohund50[50,1])/sum(ftwohund50)))
  
  fdistthreehund = c((sum(fthreehund[1,1], fthreehund[6,1], fthreehund[11,1], fthreehund[16,1], fthreehund[21,1],
                          fthreehund[26,1], fthreehund[31,1], fthreehund[36,1], fthreehund[41,1], fthreehund[46,1])/sum(fthreehund)), 
                     (sum(fthreehund[2,1], fthreehund[7,1], fthreehund[12,1], fthreehund[17,1], fthreehund[22,1],
                          fthreehund[27,1], fthreehund[32,1], fthreehund[37,1], fthreehund[42,1], fthreehund[47,1])/sum(fthreehund)),
                     (sum(fthreehund[3,1], fthreehund[8,1], fthreehund[13,1], fthreehund[18,1], fthreehund[23,1],
                          fthreehund[28,1], fthreehund[33,1], fthreehund[38,1], fthreehund[43,1], fthreehund[48,1])/sum(fthreehund)),
                     (sum(fthreehund[4,1], fthreehund[9,1], fthreehund[14,1], fthreehund[19,1], fthreehund[24,1],
                          fthreehund[29,1], fthreehund[34,1], fthreehund[39,1], fthreehund[44,1], fthreehund[49,1])/sum(fthreehund)),
                     (sum(fthreehund[5,1], fthreehund[10,1], fthreehund[15,1], fthreehund[20,1], fthreehund[25,1],
                          fthreehund[30,1], fthreehund[35,1], fthreehund[40,1], fthreehund[45,1], fthreehund[50,1])/sum(fthreehund)))
  
  
  fprojection <- cbind(ffive, ften, ftwenty, fthirty, fforty, ffifty, fsixty, fseventy, feighty,
                       fninety, fhund, fhund50, ftwohund, ftwohund50, fthreehund)
  
  f.dist <- data.frame(fdistfive,fdistten,fdisttwenty,fdistthirty,fdistforty,
                       fdistfifty,fdistsixty,fdistseventy,fdisteighty,
                       fdistninety,fdisthund, fdisthund50, fdisttwohund,
                       fdisttwohund50, fdistthreehund)
}
## 4. To what degree are differences in the (equilibrium) education distributions for Black and White people 
## attributable to differences in maternal mortality patterns between Black and White women?
## Black women having White Maternal Mortality Rates
matrixSwb<-diag(x=white$S[white$age>=15 & white$age<45])
matrixSwb2<-diag(x=black$S[black$age<15])

matrixBws <- rbind(cbind(matrixSwb2,matrix(0,nrow=nrow(matrixSwb2),ncol=ncol(matrixSwb))),
                   cbind(matrix(0,nrow=nrow(matrixSwb),ncol=ncol(matrixSwb2)),matrixSwb))

matrixBb<-diag(x=c(black$S[black$age==45&black$education==1],
                   black$S[black$age==45&black$education==2],
                   black$S[black$age==45&black$education==3],
                   black$S[black$age==45&black$education==4],
                   black$S[black$age==45&black$education==5]))

matrixMb<-cbind(matrixBws,rbind(matrixAb,matrixBb))
zero <- matrix(data=0:0, nrow=5, ncol=5)

matrixCb <-cbind(zero, zero, zero, bblack15, bblack20, bblack25, bblack30, bblack35, bblack40, zero)
Mmatrixblack2 <- rbind(matrixCb, matrixMb)
{
  
  bfive=Mmatrixblack2 %*% blackpop
  bten=(Mmatrixblack2 %^% 2) %*% blackpop
  btwenty=(Mmatrixblack2 %^% 4) %*% blackpop
  bthirty=(Mmatrixblack2 %^% 6) %*% blackpop
  bforty=(Mmatrixblack2 %^% 8) %*% blackpop
  bfifty=(Mmatrixblack2 %^% 10) %*% blackpop
  bsixty=(Mmatrixblack2 %^% 12) %*% blackpop
  bseventy=(Mmatrixblack2 %^% 14) %*% blackpop
  beighty=(Mmatrixblack2 %^% 16) %*% blackpop
  bninety=(Mmatrixblack2 %^% 18) %*% blackpop
  bhund=(Mmatrixblack2 %^% 20) %*% blackpop
  bhund50=(Mmatrixblack2 %^% 30) %*% blackpop
  btwohund=(Mmatrixblack2 %^% 40) %*% blackpop
  btwohund50=(Mmatrixblack2 %^% 50) %*% blackpop
  bthreehund=(Mmatrixblack2 %^% 60) %*% blackpop
  
  blackdistfive = c((sum(bfive[1,1], bfive[6,1], bfive[11,1], bfive[16,1], bfive[21,1],
                         bfive[26,1], bfive[31,1], bfive[36,1], bfive[41,1], bfive[46,1])/sum(bfive)), 
                    (sum(bfive[2,1], bfive[7,1], bfive[12,1], bfive[17,1], bfive[22,1],
                         bfive[27,1], bfive[32,1], bfive[37,1], bfive[42,1], bfive[47,1])/sum(bfive)),
                    (sum(bfive[3,1], bfive[8,1], bfive[13,1], bfive[18,1], bfive[23,1],
                         bfive[28,1], bfive[33,1], bfive[38,1], bfive[43,1], bfive[48,1])/sum(bfive)),
                    (sum(bfive[4,1], bfive[9,1], bfive[14,1], bfive[19,1], bfive[24,1],
                         bfive[29,1], bfive[34,1], bfive[39,1], bfive[44,1], bfive[49,1])/sum(bfive)),
                    (sum(bfive[5,1], bfive[10,1], bfive[15,1], bfive[20,1], bfive[25,1],
                         bfive[30,1], bfive[35,1], bfive[40,1], bfive[45,1], bfive[50,1])/sum(bfive)))
  
  blackdistten = c((sum(bten[1,1], bten[6,1], bten[11,1], bten[16,1], bten[21,1],
                        bten[26,1], bten[31,1], bten[36,1], bten[41,1], bten[46,1])/sum(bten)), 
                   (sum(bten[2,1], bten[7,1], bten[12,1], bten[17,1], bten[22,1],
                        bten[27,1], bten[32,1], bten[37,1], bten[42,1], bten[47,1])/sum(bten)),
                   (sum(bten[3,1], bten[8,1], bten[13,1], bten[18,1], bten[23,1],
                        bten[28,1], bten[33,1], bten[38,1], bten[43,1], bten[48,1])/sum(bten)),
                   (sum(bten[4,1], bten[9,1], bten[14,1], bten[19,1], bten[24,1],
                        bten[29,1], bten[34,1], bten[39,1], bten[44,1], bten[49,1])/sum(bten)),
                   (sum(bten[5,1], bten[10,1], bten[15,1], bten[20,1], bten[25,1],
                        bten[30,1], bten[35,1], bten[40,1], bten[45,1], bten[50,1])/sum(bten)))
  
  blackdisttwenty = c((sum(btwenty[1,1], btwenty[6,1], btwenty[11,1], btwenty[16,1], btwenty[21,1],
                           btwenty[26,1], btwenty[31,1], btwenty[36,1], btwenty[41,1], btwenty[46,1])/sum(btwenty)), 
                      (sum(btwenty[2,1], btwenty[7,1], btwenty[12,1], btwenty[17,1], btwenty[22,1],
                           btwenty[27,1], btwenty[32,1], btwenty[37,1], btwenty[42,1], btwenty[47,1])/sum(btwenty)),
                      (sum(btwenty[3,1], btwenty[8,1], btwenty[13,1], btwenty[18,1], btwenty[23,1],
                           btwenty[28,1], btwenty[33,1], btwenty[38,1], btwenty[43,1], btwenty[48,1])/sum(btwenty)),
                      (sum(btwenty[4,1], btwenty[9,1], btwenty[14,1], btwenty[19,1], btwenty[24,1],
                           btwenty[29,1], btwenty[34,1], btwenty[39,1], btwenty[44,1], btwenty[49,1])/sum(btwenty)),
                      (sum(btwenty[5,1], btwenty[10,1], btwenty[15,1], btwenty[20,1], btwenty[25,1],
                           btwenty[30,1], btwenty[35,1], btwenty[40,1], btwenty[45,1], btwenty[50,1])/sum(btwenty)))
  
  blackdistthirty = c((sum(bthirty[1,1], bthirty[6,1], bthirty[11,1], bthirty[16,1], bthirty[21,1],
                           bthirty[26,1], bthirty[31,1], bthirty[36,1], bthirty[41,1], bthirty[46,1])/sum(bthirty)), 
                      (sum(bthirty[2,1], bthirty[7,1], bthirty[12,1], bthirty[17,1], bthirty[22,1],
                           bthirty[27,1], bthirty[32,1], bthirty[37,1], bthirty[42,1], bthirty[47,1])/sum(bthirty)),
                      (sum(bthirty[3,1], bthirty[8,1], bthirty[13,1], bthirty[18,1], bthirty[23,1],
                           bthirty[28,1], bthirty[33,1], bthirty[38,1], bthirty[43,1], bthirty[48,1])/sum(bthirty)),
                      (sum(bthirty[4,1], bthirty[9,1], bthirty[14,1], bthirty[19,1], bthirty[24,1],
                           bthirty[29,1], bthirty[34,1], bthirty[39,1], bthirty[44,1], bthirty[49,1])/sum(bthirty)),
                      (sum(bthirty[5,1], bthirty[10,1], bthirty[15,1], bthirty[20,1], bthirty[25,1],
                           bthirty[30,1], bthirty[35,1], bthirty[40,1], bthirty[45,1], bthirty[50,1])/sum(bthirty)))
  
  blackdistforty = c((sum(bforty[1,1], bforty[6,1], bforty[11,1], bforty[16,1], bforty[21,1],
                          bforty[26,1], bforty[31,1], bforty[36,1], bforty[41,1], bforty[46,1])/sum(bforty)), 
                     (sum(bforty[2,1], bforty[7,1], bforty[12,1], bforty[17,1], bforty[22,1],
                          bforty[27,1], bforty[32,1], bforty[37,1], bforty[42,1], bforty[47,1])/sum(bforty)),
                     (sum(bforty[3,1], bforty[8,1], bforty[13,1], bforty[18,1], bforty[23,1],
                          bforty[28,1], bforty[33,1], bforty[38,1], bforty[43,1], bforty[48,1])/sum(bforty)),
                     (sum(bforty[4,1], bforty[9,1], bforty[14,1], bforty[19,1], bforty[24,1],
                          bforty[29,1], bforty[34,1], bforty[39,1], bforty[44,1], bforty[49,1])/sum(bforty)),
                     (sum(bforty[5,1], bforty[10,1], bforty[15,1], bforty[20,1], bforty[25,1],
                          bforty[30,1], bforty[35,1], bforty[40,1], bforty[45,1], bforty[50,1])/sum(bforty)))
  
  blackdistfifty = c((sum(bfifty[1,1], bfifty[6,1], bfifty[11,1], bfifty[16,1], bfifty[21,1],
                          bfifty[26,1], bfifty[31,1], bfifty[36,1], bfifty[41,1], bfifty[46,1])/sum(bfifty)), 
                     (sum(bfifty[2,1], bfifty[7,1], bfifty[12,1], bfifty[17,1], bfifty[22,1],
                          bfifty[27,1], bfifty[32,1], bfifty[37,1], bfifty[42,1], bfifty[47,1])/sum(bfifty)),
                     (sum(bfifty[3,1], bfifty[8,1], bfifty[13,1], bfifty[18,1], bfifty[23,1],
                          bfifty[28,1], bfifty[33,1], bfifty[38,1], bfifty[43,1], bfifty[48,1])/sum(bfifty)),
                     (sum(bfifty[4,1], bfifty[9,1], bfifty[14,1], bfifty[19,1], bfifty[24,1],
                          bfifty[29,1], bfifty[34,1], bfifty[39,1], bfifty[44,1], bfifty[49,1])/sum(bfifty)),
                     (sum(bfifty[5,1], bfifty[10,1], bfifty[15,1], bfifty[20,1], bfifty[25,1],
                          bfifty[30,1], bfifty[35,1], bfifty[40,1], bfifty[45,1], bfifty[50,1])/sum(bfifty)))
  
  blackdistsixty = c((sum(bsixty[1,1], bsixty[6,1], bsixty[11,1], bsixty[16,1], bsixty[21,1],
                          bsixty[26,1], bsixty[31,1], bsixty[36,1], bsixty[41,1], bsixty[46,1])/sum(bsixty)), 
                     (sum(bsixty[2,1], bsixty[7,1], bsixty[12,1], bsixty[17,1], bsixty[22,1],
                          bsixty[27,1], bsixty[32,1], bsixty[37,1], bsixty[42,1], bsixty[47,1])/sum(bsixty)),
                     (sum(bsixty[3,1], bsixty[8,1], bsixty[13,1], bsixty[18,1], bsixty[23,1],
                          bsixty[28,1], bsixty[33,1], bsixty[38,1], bsixty[43,1], bsixty[48,1])/sum(bsixty)),
                     (sum(bsixty[4,1], bsixty[9,1], bsixty[14,1], bsixty[19,1], bsixty[24,1],
                          bsixty[29,1], bsixty[34,1], bsixty[39,1], bsixty[44,1], bsixty[49,1])/sum(bsixty)),
                     (sum(bsixty[5,1], bsixty[10,1], bsixty[15,1], bsixty[20,1], bsixty[25,1],
                          bsixty[30,1], bsixty[35,1], bsixty[40,1], bsixty[45,1], bsixty[50,1])/sum(bsixty)))
  
  blackdistseventy = c((sum(bseventy[1,1], bseventy[6,1], bseventy[11,1], bseventy[16,1], bseventy[21,1],
                            bseventy[26,1], bseventy[31,1], bseventy[36,1], bseventy[41,1], bseventy[46,1])/sum(bseventy)), 
                       (sum(bseventy[2,1], bseventy[7,1], bseventy[12,1], bseventy[17,1], bseventy[22,1],
                            bseventy[27,1], bseventy[32,1], bseventy[37,1], bseventy[42,1], bseventy[47,1])/sum(bseventy)),
                       (sum(bseventy[3,1], bseventy[8,1], bseventy[13,1], bseventy[18,1], bseventy[23,1],
                            bseventy[28,1], bseventy[33,1], bseventy[38,1], bseventy[43,1], bseventy[48,1])/sum(bseventy)),
                       (sum(bseventy[4,1], bseventy[9,1], bseventy[14,1], bseventy[19,1], bseventy[24,1],
                            bseventy[29,1], bseventy[34,1], bseventy[39,1], bseventy[44,1], bseventy[49,1])/sum(bseventy)),
                       (sum(bseventy[5,1], bseventy[10,1], bseventy[15,1], bseventy[20,1], bseventy[25,1],
                            bseventy[30,1], bseventy[35,1], bseventy[40,1], bseventy[45,1], bseventy[50,1])/sum(bseventy)))
  
  blackdisteighty = c((sum(beighty[1,1], beighty[6,1], beighty[11,1], beighty[16,1], beighty[21,1],
                           beighty[26,1], beighty[31,1], beighty[36,1], beighty[41,1], beighty[46,1])/sum(beighty)), 
                      (sum(beighty[2,1], beighty[7,1], beighty[12,1], beighty[17,1], beighty[22,1],
                           beighty[27,1], beighty[32,1], beighty[37,1], beighty[42,1], beighty[47,1])/sum(beighty)),
                      (sum(beighty[3,1], beighty[8,1], beighty[13,1], beighty[18,1], beighty[23,1],
                           beighty[28,1], beighty[33,1], beighty[38,1], beighty[43,1], beighty[48,1])/sum(beighty)),
                      (sum(beighty[4,1], beighty[9,1], beighty[14,1], beighty[19,1], beighty[24,1],
                           beighty[29,1], beighty[34,1], beighty[39,1], beighty[44,1], beighty[49,1])/sum(beighty)),
                      (sum(beighty[5,1], beighty[10,1], beighty[15,1], beighty[20,1], beighty[25,1],
                           beighty[30,1], beighty[35,1], beighty[40,1], beighty[45,1], beighty[50,1])/sum(beighty)))
  
  blackdistninety = c((sum(bninety[1,1], bninety[6,1], bninety[11,1], bninety[16,1], bninety[21,1],
                           bninety[26,1], bninety[31,1], bninety[36,1], bninety[41,1], bninety[46,1])/sum(bninety)), 
                      (sum(bninety[2,1], bninety[7,1], bninety[12,1], bninety[17,1], bninety[22,1],
                           bninety[27,1], bninety[32,1], bninety[37,1], bninety[42,1], bninety[47,1])/sum(bninety)),
                      (sum(bninety[3,1], bninety[8,1], bninety[13,1], bninety[18,1], bninety[23,1],
                           bninety[28,1], bninety[33,1], bninety[38,1], bninety[43,1], bninety[48,1])/sum(bninety)),
                      (sum(bninety[4,1], bninety[9,1], bninety[14,1], bninety[19,1], bninety[24,1],
                           bninety[29,1], bninety[34,1], bninety[39,1], bninety[44,1], bninety[49,1])/sum(bninety)),
                      (sum(bninety[5,1], bninety[10,1], bninety[15,1], bninety[20,1], bninety[25,1],
                           bninety[30,1], bninety[35,1], bninety[40,1], bninety[45,1], bninety[50,1])/sum(bninety)))
  
  blackdisthund = c((sum(bhund[1,1], bhund[6,1], bhund[11,1], bhund[16,1], bhund[21,1],
                         bhund[26,1], bhund[31,1], bhund[36,1], bhund[41,1], bhund[46,1])/sum(bhund)), 
                    (sum(bhund[2,1], bhund[7,1], bhund[12,1], bhund[17,1], bhund[22,1],
                         bhund[27,1], bhund[32,1], bhund[37,1], bhund[42,1], bhund[47,1])/sum(bhund)),
                    (sum(bhund[3,1], bhund[8,1], bhund[13,1], bhund[18,1], bhund[23,1],
                         bhund[28,1], bhund[33,1], bhund[38,1], bhund[43,1], bhund[48,1])/sum(bhund)),
                    (sum(bhund[4,1], bhund[9,1], bhund[14,1], bhund[19,1], bhund[24,1],
                         bhund[29,1], bhund[34,1], bhund[39,1], bhund[44,1], bhund[49,1])/sum(bhund)),
                    (sum(bhund[5,1], bhund[10,1], bhund[15,1], bhund[20,1], bhund[25,1],
                         bhund[30,1], bhund[35,1], bhund[40,1], bhund[45,1], bhund[50,1])/sum(bhund)))
  
  blackdisthund50 = c((sum(bhund50[1,1], bhund50[6,1], bhund50[11,1], bhund50[16,1], bhund50[21,1],
                           bhund50[26,1], bhund50[31,1], bhund50[36,1], bhund50[41,1], bhund50[46,1])/sum(bhund50)), 
                      (sum(bhund50[2,1], bhund50[7,1], bhund50[12,1], bhund50[17,1], bhund50[22,1],
                           bhund50[27,1], bhund50[32,1], bhund50[37,1], bhund50[42,1], bhund50[47,1])/sum(bhund50)),
                      (sum(bhund50[3,1], bhund50[8,1], bhund50[13,1], bhund50[18,1], bhund50[23,1],
                           bhund50[28,1], bhund50[33,1], bhund50[38,1], bhund50[43,1], bhund50[48,1])/sum(bhund50)),
                      (sum(bhund50[4,1], bhund50[9,1], bhund50[14,1], bhund50[19,1], bhund50[24,1],
                           bhund50[29,1], bhund50[34,1], bhund50[39,1], bhund50[44,1], bhund50[49,1])/sum(bhund50)),
                      (sum(bhund50[5,1], bhund50[10,1], bhund50[15,1], bhund50[20,1], bhund50[25,1],
                           bhund50[30,1], bhund50[35,1], bhund50[40,1], bhund50[45,1], bhund50[50,1])/sum(bhund50)))
  
  blackdisttwohund = c((sum(btwohund[1,1], btwohund[6,1], btwohund[11,1], btwohund[16,1], btwohund[21,1],
                            btwohund[26,1], btwohund[31,1], btwohund[36,1], btwohund[41,1], btwohund[46,1])/sum(btwohund)), 
                       (sum(btwohund[2,1], btwohund[7,1], btwohund[12,1], btwohund[17,1], btwohund[22,1],
                            btwohund[27,1], btwohund[32,1], btwohund[37,1], btwohund[42,1], btwohund[47,1])/sum(btwohund)),
                       (sum(btwohund[3,1], btwohund[8,1], btwohund[13,1], btwohund[18,1], btwohund[23,1],
                            btwohund[28,1], btwohund[33,1], btwohund[38,1], btwohund[43,1], btwohund[48,1])/sum(btwohund)),
                       (sum(btwohund[4,1], btwohund[9,1], btwohund[14,1], btwohund[19,1], btwohund[24,1],
                            btwohund[29,1], btwohund[34,1], btwohund[39,1], btwohund[44,1], btwohund[49,1])/sum(btwohund)),
                       (sum(btwohund[5,1], btwohund[10,1], btwohund[15,1], btwohund[20,1], btwohund[25,1],
                            btwohund[30,1], btwohund[35,1], btwohund[40,1], btwohund[45,1], btwohund[50,1])/sum(btwohund)))
  
  blackdisttwohund50 = c((sum(btwohund50[1,1], btwohund50[6,1], btwohund50[11,1], btwohund50[16,1], btwohund50[21,1],
                              btwohund50[26,1], btwohund50[31,1], btwohund50[36,1], btwohund50[41,1], btwohund50[46,1])/sum(btwohund50)), 
                         (sum(btwohund50[2,1], btwohund50[7,1], btwohund50[12,1], btwohund50[17,1], btwohund50[22,1],
                              btwohund50[27,1], btwohund50[32,1], btwohund50[37,1], btwohund50[42,1], btwohund50[47,1])/sum(btwohund50)),
                         (sum(btwohund50[3,1], btwohund50[8,1], btwohund50[13,1], btwohund50[18,1], btwohund50[23,1],
                              btwohund50[28,1], btwohund50[33,1], btwohund50[38,1], btwohund50[43,1], btwohund50[48,1])/sum(btwohund50)),
                         (sum(btwohund50[4,1], btwohund50[9,1], btwohund50[14,1], btwohund50[19,1], btwohund50[24,1],
                              btwohund50[29,1], btwohund50[34,1], btwohund50[39,1], btwohund50[44,1], btwohund50[49,1])/sum(btwohund50)),
                         (sum(btwohund50[5,1], btwohund50[10,1], btwohund50[15,1], btwohund50[20,1], btwohund50[25,1],
                              btwohund50[30,1], btwohund50[35,1], btwohund50[40,1], btwohund50[45,1], btwohund50[50,1])/sum(btwohund50)))
  
  blackdistthreehund = c((sum(bthreehund[1,1], bthreehund[6,1], bthreehund[11,1], bthreehund[16,1], bthreehund[21,1],
                              bthreehund[26,1], bthreehund[31,1], bthreehund[36,1], bthreehund[41,1], bthreehund[46,1])/sum(bthreehund)), 
                         (sum(bthreehund[2,1], bthreehund[7,1], bthreehund[12,1], bthreehund[17,1], bthreehund[22,1],
                              bthreehund[27,1], bthreehund[32,1], bthreehund[37,1], bthreehund[42,1], bthreehund[47,1])/sum(bthreehund)),
                         (sum(bthreehund[3,1], bthreehund[8,1], bthreehund[13,1], bthreehund[18,1], bthreehund[23,1],
                              bthreehund[28,1], bthreehund[33,1], bthreehund[38,1], bthreehund[43,1], bthreehund[48,1])/sum(bthreehund)),
                         (sum(bthreehund[4,1], bthreehund[9,1], bthreehund[14,1], bthreehund[19,1], bthreehund[24,1],
                              bthreehund[29,1], bthreehund[34,1], bthreehund[39,1], bthreehund[44,1], bthreehund[49,1])/sum(bthreehund)),
                         (sum(bthreehund[5,1], bthreehund[10,1], bthreehund[15,1], bthreehund[20,1], bthreehund[25,1],
                              bthreehund[30,1], bthreehund[35,1], bthreehund[40,1], bthreehund[45,1], bthreehund[50,1])/sum(bthreehund)))
  
  blackprojection <- cbind(bfive, bten, btwenty, bthirty, bforty, bfifty, bsixty, bseventy, beighty,
                           bninety, bhund, bhund50, btwohund, btwohund50, bthreehund)
  
  black.dist2 <- data.frame(blackdistfive,blackdistten,blackdisttwenty,blackdistthirty,blackdistforty,
                            blackdistfifty,blackdistsixty,blackdistseventy,blackdisteighty,
                            blackdistninety,blackdisthund, blackdisthund50, blackdisttwohund, 
                            blackdisttwohund50, blackdistthreehund)
}

## 5. To what degree are differences in the (equilibrium) education distributions for Black and White people
## attributable to differences in child (under 15) mortality patterns by race?
## Under 15 Mortality Patterns (Black individuals having Under15 white mortality)
{
  matrixSwb<-diag(x=white$S[white$age>=15 & white$age<45])
  matrixSwb2<-diag(x=black$S[black$age<15])
  
  matrixSw5<-diag(x=white$S[white$age>=0 & white$age<15])
  matrixSw52<-diag(x=black$S[black$age>=15 & black$age<45])
  
  matrixBws5 <- rbind(cbind(matrixSw5,matrix(0,nrow=nrow(matrixSw5),ncol=ncol(matrixSw52))),
                      cbind(matrix(0,nrow=nrow(matrixSw52),ncol=ncol(matrixSw5)),matrixSw52))
  
  matrixBb5<-diag(x=c(black$S[black$age==45&black$education==1],
                      black$S[black$age==45&black$education==2],
                      black$S[black$age==45&black$education==3],
                      black$S[black$age==45&black$education==4],
                      black$S[black$age==45&black$education==5]))
  
  matrixMb5<-cbind(matrixBws5,rbind(matrixAb,matrixBb5))
  
  matrixCb5 <-cbind(zero, zero, zero, bblack15, bblack20, bblack25, bblack30, bblack35, bblack40, zero)
  Mmatrixblack5 <- rbind(matrixCb5, matrixMb5)
  bfive=Mmatrixblack5 %*% blackpop
  bten=(Mmatrixblack5 %^% 2) %*% blackpop
  btwenty=(Mmatrixblack5 %^% 4) %*% blackpop
  bthirty=(Mmatrixblack5 %^% 6) %*% blackpop
  bforty=(Mmatrixblack5 %^% 8) %*% blackpop
  bfifty=(Mmatrixblack5 %^% 10) %*% blackpop
  bsixty=(Mmatrixblack5 %^% 12) %*% blackpop
  bseventy=(Mmatrixblack5 %^% 14) %*% blackpop
  beighty=(Mmatrixblack5 %^% 16) %*% blackpop
  bninety=(Mmatrixblack5 %^% 18) %*% blackpop
  bhund=(Mmatrixblack5 %^% 20) %*% blackpop
  bhund50=(Mmatrixblack5 %^% 30) %*% blackpop
  btwohund=(Mmatrixblack5 %^% 40) %*% blackpop
  btwohund50=(Mmatrixblack5 %^% 50) %*% blackpop
  bthreehund=(Mmatrixblack5 %^% 60) %*% blackpop
  
  blackdistfive = c((sum(bfive[1,1], bfive[6,1], bfive[11,1], bfive[16,1], bfive[21,1],
                         bfive[26,1], bfive[31,1], bfive[36,1], bfive[41,1], bfive[46,1])/sum(bfive)), 
                    (sum(bfive[2,1], bfive[7,1], bfive[12,1], bfive[17,1], bfive[22,1],
                         bfive[27,1], bfive[32,1], bfive[37,1], bfive[42,1], bfive[47,1])/sum(bfive)),
                    (sum(bfive[3,1], bfive[8,1], bfive[13,1], bfive[18,1], bfive[23,1],
                         bfive[28,1], bfive[33,1], bfive[38,1], bfive[43,1], bfive[48,1])/sum(bfive)),
                    (sum(bfive[4,1], bfive[9,1], bfive[14,1], bfive[19,1], bfive[24,1],
                         bfive[29,1], bfive[34,1], bfive[39,1], bfive[44,1], bfive[49,1])/sum(bfive)),
                    (sum(bfive[5,1], bfive[10,1], bfive[15,1], bfive[20,1], bfive[25,1],
                         bfive[30,1], bfive[35,1], bfive[40,1], bfive[45,1], bfive[50,1])/sum(bfive)))
  
  blackdistten = c((sum(bten[1,1], bten[6,1], bten[11,1], bten[16,1], bten[21,1],
                        bten[26,1], bten[31,1], bten[36,1], bten[41,1], bten[46,1])/sum(bten)), 
                   (sum(bten[2,1], bten[7,1], bten[12,1], bten[17,1], bten[22,1],
                        bten[27,1], bten[32,1], bten[37,1], bten[42,1], bten[47,1])/sum(bten)),
                   (sum(bten[3,1], bten[8,1], bten[13,1], bten[18,1], bten[23,1],
                        bten[28,1], bten[33,1], bten[38,1], bten[43,1], bten[48,1])/sum(bten)),
                   (sum(bten[4,1], bten[9,1], bten[14,1], bten[19,1], bten[24,1],
                        bten[29,1], bten[34,1], bten[39,1], bten[44,1], bten[49,1])/sum(bten)),
                   (sum(bten[5,1], bten[10,1], bten[15,1], bten[20,1], bten[25,1],
                        bten[30,1], bten[35,1], bten[40,1], bten[45,1], bten[50,1])/sum(bten)))
  
  blackdisttwenty = c((sum(btwenty[1,1], btwenty[6,1], btwenty[11,1], btwenty[16,1], btwenty[21,1],
                           btwenty[26,1], btwenty[31,1], btwenty[36,1], btwenty[41,1], btwenty[46,1])/sum(btwenty)), 
                      (sum(btwenty[2,1], btwenty[7,1], btwenty[12,1], btwenty[17,1], btwenty[22,1],
                           btwenty[27,1], btwenty[32,1], btwenty[37,1], btwenty[42,1], btwenty[47,1])/sum(btwenty)),
                      (sum(btwenty[3,1], btwenty[8,1], btwenty[13,1], btwenty[18,1], btwenty[23,1],
                           btwenty[28,1], btwenty[33,1], btwenty[38,1], btwenty[43,1], btwenty[48,1])/sum(btwenty)),
                      (sum(btwenty[4,1], btwenty[9,1], btwenty[14,1], btwenty[19,1], btwenty[24,1],
                           btwenty[29,1], btwenty[34,1], btwenty[39,1], btwenty[44,1], btwenty[49,1])/sum(btwenty)),
                      (sum(btwenty[5,1], btwenty[10,1], btwenty[15,1], btwenty[20,1], btwenty[25,1],
                           btwenty[30,1], btwenty[35,1], btwenty[40,1], btwenty[45,1], btwenty[50,1])/sum(btwenty)))
  
  blackdistthirty = c((sum(bthirty[1,1], bthirty[6,1], bthirty[11,1], bthirty[16,1], bthirty[21,1],
                           bthirty[26,1], bthirty[31,1], bthirty[36,1], bthirty[41,1], bthirty[46,1])/sum(bthirty)), 
                      (sum(bthirty[2,1], bthirty[7,1], bthirty[12,1], bthirty[17,1], bthirty[22,1],
                           bthirty[27,1], bthirty[32,1], bthirty[37,1], bthirty[42,1], bthirty[47,1])/sum(bthirty)),
                      (sum(bthirty[3,1], bthirty[8,1], bthirty[13,1], bthirty[18,1], bthirty[23,1],
                           bthirty[28,1], bthirty[33,1], bthirty[38,1], bthirty[43,1], bthirty[48,1])/sum(bthirty)),
                      (sum(bthirty[4,1], bthirty[9,1], bthirty[14,1], bthirty[19,1], bthirty[24,1],
                           bthirty[29,1], bthirty[34,1], bthirty[39,1], bthirty[44,1], bthirty[49,1])/sum(bthirty)),
                      (sum(bthirty[5,1], bthirty[10,1], bthirty[15,1], bthirty[20,1], bthirty[25,1],
                           bthirty[30,1], bthirty[35,1], bthirty[40,1], bthirty[45,1], bthirty[50,1])/sum(bthirty)))
  
  blackdistforty = c((sum(bforty[1,1], bforty[6,1], bforty[11,1], bforty[16,1], bforty[21,1],
                          bforty[26,1], bforty[31,1], bforty[36,1], bforty[41,1], bforty[46,1])/sum(bforty)), 
                     (sum(bforty[2,1], bforty[7,1], bforty[12,1], bforty[17,1], bforty[22,1],
                          bforty[27,1], bforty[32,1], bforty[37,1], bforty[42,1], bforty[47,1])/sum(bforty)),
                     (sum(bforty[3,1], bforty[8,1], bforty[13,1], bforty[18,1], bforty[23,1],
                          bforty[28,1], bforty[33,1], bforty[38,1], bforty[43,1], bforty[48,1])/sum(bforty)),
                     (sum(bforty[4,1], bforty[9,1], bforty[14,1], bforty[19,1], bforty[24,1],
                          bforty[29,1], bforty[34,1], bforty[39,1], bforty[44,1], bforty[49,1])/sum(bforty)),
                     (sum(bforty[5,1], bforty[10,1], bforty[15,1], bforty[20,1], bforty[25,1],
                          bforty[30,1], bforty[35,1], bforty[40,1], bforty[45,1], bforty[50,1])/sum(bforty)))
  
  blackdistfifty = c((sum(bfifty[1,1], bfifty[6,1], bfifty[11,1], bfifty[16,1], bfifty[21,1],
                          bfifty[26,1], bfifty[31,1], bfifty[36,1], bfifty[41,1], bfifty[46,1])/sum(bfifty)), 
                     (sum(bfifty[2,1], bfifty[7,1], bfifty[12,1], bfifty[17,1], bfifty[22,1],
                          bfifty[27,1], bfifty[32,1], bfifty[37,1], bfifty[42,1], bfifty[47,1])/sum(bfifty)),
                     (sum(bfifty[3,1], bfifty[8,1], bfifty[13,1], bfifty[18,1], bfifty[23,1],
                          bfifty[28,1], bfifty[33,1], bfifty[38,1], bfifty[43,1], bfifty[48,1])/sum(bfifty)),
                     (sum(bfifty[4,1], bfifty[9,1], bfifty[14,1], bfifty[19,1], bfifty[24,1],
                          bfifty[29,1], bfifty[34,1], bfifty[39,1], bfifty[44,1], bfifty[49,1])/sum(bfifty)),
                     (sum(bfifty[5,1], bfifty[10,1], bfifty[15,1], bfifty[20,1], bfifty[25,1],
                          bfifty[30,1], bfifty[35,1], bfifty[40,1], bfifty[45,1], bfifty[50,1])/sum(bfifty)))
  
  blackdistsixty = c((sum(bsixty[1,1], bsixty[6,1], bsixty[11,1], bsixty[16,1], bsixty[21,1],
                          bsixty[26,1], bsixty[31,1], bsixty[36,1], bsixty[41,1], bsixty[46,1])/sum(bsixty)), 
                     (sum(bsixty[2,1], bsixty[7,1], bsixty[12,1], bsixty[17,1], bsixty[22,1],
                          bsixty[27,1], bsixty[32,1], bsixty[37,1], bsixty[42,1], bsixty[47,1])/sum(bsixty)),
                     (sum(bsixty[3,1], bsixty[8,1], bsixty[13,1], bsixty[18,1], bsixty[23,1],
                          bsixty[28,1], bsixty[33,1], bsixty[38,1], bsixty[43,1], bsixty[48,1])/sum(bsixty)),
                     (sum(bsixty[4,1], bsixty[9,1], bsixty[14,1], bsixty[19,1], bsixty[24,1],
                          bsixty[29,1], bsixty[34,1], bsixty[39,1], bsixty[44,1], bsixty[49,1])/sum(bsixty)),
                     (sum(bsixty[5,1], bsixty[10,1], bsixty[15,1], bsixty[20,1], bsixty[25,1],
                          bsixty[30,1], bsixty[35,1], bsixty[40,1], bsixty[45,1], bsixty[50,1])/sum(bsixty)))
  
  blackdistseventy = c((sum(bseventy[1,1], bseventy[6,1], bseventy[11,1], bseventy[16,1], bseventy[21,1],
                            bseventy[26,1], bseventy[31,1], bseventy[36,1], bseventy[41,1], bseventy[46,1])/sum(bseventy)), 
                       (sum(bseventy[2,1], bseventy[7,1], bseventy[12,1], bseventy[17,1], bseventy[22,1],
                            bseventy[27,1], bseventy[32,1], bseventy[37,1], bseventy[42,1], bseventy[47,1])/sum(bseventy)),
                       (sum(bseventy[3,1], bseventy[8,1], bseventy[13,1], bseventy[18,1], bseventy[23,1],
                            bseventy[28,1], bseventy[33,1], bseventy[38,1], bseventy[43,1], bseventy[48,1])/sum(bseventy)),
                       (sum(bseventy[4,1], bseventy[9,1], bseventy[14,1], bseventy[19,1], bseventy[24,1],
                            bseventy[29,1], bseventy[34,1], bseventy[39,1], bseventy[44,1], bseventy[49,1])/sum(bseventy)),
                       (sum(bseventy[5,1], bseventy[10,1], bseventy[15,1], bseventy[20,1], bseventy[25,1],
                            bseventy[30,1], bseventy[35,1], bseventy[40,1], bseventy[45,1], bseventy[50,1])/sum(bseventy)))
  
  blackdisteighty = c((sum(beighty[1,1], beighty[6,1], beighty[11,1], beighty[16,1], beighty[21,1],
                           beighty[26,1], beighty[31,1], beighty[36,1], beighty[41,1], beighty[46,1])/sum(beighty)), 
                      (sum(beighty[2,1], beighty[7,1], beighty[12,1], beighty[17,1], beighty[22,1],
                           beighty[27,1], beighty[32,1], beighty[37,1], beighty[42,1], beighty[47,1])/sum(beighty)),
                      (sum(beighty[3,1], beighty[8,1], beighty[13,1], beighty[18,1], beighty[23,1],
                           beighty[28,1], beighty[33,1], beighty[38,1], beighty[43,1], beighty[48,1])/sum(beighty)),
                      (sum(beighty[4,1], beighty[9,1], beighty[14,1], beighty[19,1], beighty[24,1],
                           beighty[29,1], beighty[34,1], beighty[39,1], beighty[44,1], beighty[49,1])/sum(beighty)),
                      (sum(beighty[5,1], beighty[10,1], beighty[15,1], beighty[20,1], beighty[25,1],
                           beighty[30,1], beighty[35,1], beighty[40,1], beighty[45,1], beighty[50,1])/sum(beighty)))
  
  blackdistninety = c((sum(bninety[1,1], bninety[6,1], bninety[11,1], bninety[16,1], bninety[21,1],
                           bninety[26,1], bninety[31,1], bninety[36,1], bninety[41,1], bninety[46,1])/sum(bninety)), 
                      (sum(bninety[2,1], bninety[7,1], bninety[12,1], bninety[17,1], bninety[22,1],
                           bninety[27,1], bninety[32,1], bninety[37,1], bninety[42,1], bninety[47,1])/sum(bninety)),
                      (sum(bninety[3,1], bninety[8,1], bninety[13,1], bninety[18,1], bninety[23,1],
                           bninety[28,1], bninety[33,1], bninety[38,1], bninety[43,1], bninety[48,1])/sum(bninety)),
                      (sum(bninety[4,1], bninety[9,1], bninety[14,1], bninety[19,1], bninety[24,1],
                           bninety[29,1], bninety[34,1], bninety[39,1], bninety[44,1], bninety[49,1])/sum(bninety)),
                      (sum(bninety[5,1], bninety[10,1], bninety[15,1], bninety[20,1], bninety[25,1],
                           bninety[30,1], bninety[35,1], bninety[40,1], bninety[45,1], bninety[50,1])/sum(bninety)))
  
  blackdisthund = c((sum(bhund[1,1], bhund[6,1], bhund[11,1], bhund[16,1], bhund[21,1],
                         bhund[26,1], bhund[31,1], bhund[36,1], bhund[41,1], bhund[46,1])/sum(bhund)), 
                    (sum(bhund[2,1], bhund[7,1], bhund[12,1], bhund[17,1], bhund[22,1],
                         bhund[27,1], bhund[32,1], bhund[37,1], bhund[42,1], bhund[47,1])/sum(bhund)),
                    (sum(bhund[3,1], bhund[8,1], bhund[13,1], bhund[18,1], bhund[23,1],
                         bhund[28,1], bhund[33,1], bhund[38,1], bhund[43,1], bhund[48,1])/sum(bhund)),
                    (sum(bhund[4,1], bhund[9,1], bhund[14,1], bhund[19,1], bhund[24,1],
                         bhund[29,1], bhund[34,1], bhund[39,1], bhund[44,1], bhund[49,1])/sum(bhund)),
                    (sum(bhund[5,1], bhund[10,1], bhund[15,1], bhund[20,1], bhund[25,1],
                         bhund[30,1], bhund[35,1], bhund[40,1], bhund[45,1], bhund[50,1])/sum(bhund)))
  
  blackdisthund50 = c((sum(bhund50[1,1], bhund50[6,1], bhund50[11,1], bhund50[16,1], bhund50[21,1],
                           bhund50[26,1], bhund50[31,1], bhund50[36,1], bhund50[41,1], bhund50[46,1])/sum(bhund50)), 
                      (sum(bhund50[2,1], bhund50[7,1], bhund50[12,1], bhund50[17,1], bhund50[22,1],
                           bhund50[27,1], bhund50[32,1], bhund50[37,1], bhund50[42,1], bhund50[47,1])/sum(bhund50)),
                      (sum(bhund50[3,1], bhund50[8,1], bhund50[13,1], bhund50[18,1], bhund50[23,1],
                           bhund50[28,1], bhund50[33,1], bhund50[38,1], bhund50[43,1], bhund50[48,1])/sum(bhund50)),
                      (sum(bhund50[4,1], bhund50[9,1], bhund50[14,1], bhund50[19,1], bhund50[24,1],
                           bhund50[29,1], bhund50[34,1], bhund50[39,1], bhund50[44,1], bhund50[49,1])/sum(bhund50)),
                      (sum(bhund50[5,1], bhund50[10,1], bhund50[15,1], bhund50[20,1], bhund50[25,1],
                           bhund50[30,1], bhund50[35,1], bhund50[40,1], bhund50[45,1], bhund50[50,1])/sum(bhund50)))
  
  blackdisttwohund = c((sum(btwohund[1,1], btwohund[6,1], btwohund[11,1], btwohund[16,1], btwohund[21,1],
                            btwohund[26,1], btwohund[31,1], btwohund[36,1], btwohund[41,1], btwohund[46,1])/sum(btwohund)), 
                       (sum(btwohund[2,1], btwohund[7,1], btwohund[12,1], btwohund[17,1], btwohund[22,1],
                            btwohund[27,1], btwohund[32,1], btwohund[37,1], btwohund[42,1], btwohund[47,1])/sum(btwohund)),
                       (sum(btwohund[3,1], btwohund[8,1], btwohund[13,1], btwohund[18,1], btwohund[23,1],
                            btwohund[28,1], btwohund[33,1], btwohund[38,1], btwohund[43,1], btwohund[48,1])/sum(btwohund)),
                       (sum(btwohund[4,1], btwohund[9,1], btwohund[14,1], btwohund[19,1], btwohund[24,1],
                            btwohund[29,1], btwohund[34,1], btwohund[39,1], btwohund[44,1], btwohund[49,1])/sum(btwohund)),
                       (sum(btwohund[5,1], btwohund[10,1], btwohund[15,1], btwohund[20,1], btwohund[25,1],
                            btwohund[30,1], btwohund[35,1], btwohund[40,1], btwohund[45,1], btwohund[50,1])/sum(btwohund)))
  
  blackdisttwohund50 = c((sum(btwohund50[1,1], btwohund50[6,1], btwohund50[11,1], btwohund50[16,1], btwohund50[21,1],
                              btwohund50[26,1], btwohund50[31,1], btwohund50[36,1], btwohund50[41,1], btwohund50[46,1])/sum(btwohund50)), 
                         (sum(btwohund50[2,1], btwohund50[7,1], btwohund50[12,1], btwohund50[17,1], btwohund50[22,1],
                              btwohund50[27,1], btwohund50[32,1], btwohund50[37,1], btwohund50[42,1], btwohund50[47,1])/sum(btwohund50)),
                         (sum(btwohund50[3,1], btwohund50[8,1], btwohund50[13,1], btwohund50[18,1], btwohund50[23,1],
                              btwohund50[28,1], btwohund50[33,1], btwohund50[38,1], btwohund50[43,1], btwohund50[48,1])/sum(btwohund50)),
                         (sum(btwohund50[4,1], btwohund50[9,1], btwohund50[14,1], btwohund50[19,1], btwohund50[24,1],
                              btwohund50[29,1], btwohund50[34,1], btwohund50[39,1], btwohund50[44,1], btwohund50[49,1])/sum(btwohund50)),
                         (sum(btwohund50[5,1], btwohund50[10,1], btwohund50[15,1], btwohund50[20,1], btwohund50[25,1],
                              btwohund50[30,1], btwohund50[35,1], btwohund50[40,1], btwohund50[45,1], btwohund50[50,1])/sum(btwohund50)))
  
  blackdistthreehund = c((sum(bthreehund[1,1], bthreehund[6,1], bthreehund[11,1], bthreehund[16,1], bthreehund[21,1],
                              bthreehund[26,1], bthreehund[31,1], bthreehund[36,1], bthreehund[41,1], bthreehund[46,1])/sum(bthreehund)), 
                         (sum(bthreehund[2,1], bthreehund[7,1], bthreehund[12,1], bthreehund[17,1], bthreehund[22,1],
                              bthreehund[27,1], bthreehund[32,1], bthreehund[37,1], bthreehund[42,1], bthreehund[47,1])/sum(bthreehund)),
                         (sum(bthreehund[3,1], bthreehund[8,1], bthreehund[13,1], bthreehund[18,1], bthreehund[23,1],
                              bthreehund[28,1], bthreehund[33,1], bthreehund[38,1], bthreehund[43,1], bthreehund[48,1])/sum(bthreehund)),
                         (sum(bthreehund[4,1], bthreehund[9,1], bthreehund[14,1], bthreehund[19,1], bthreehund[24,1],
                              bthreehund[29,1], bthreehund[34,1], bthreehund[39,1], bthreehund[44,1], bthreehund[49,1])/sum(bthreehund)),
                         (sum(bthreehund[5,1], bthreehund[10,1], bthreehund[15,1], bthreehund[20,1], bthreehund[25,1],
                              bthreehund[30,1], bthreehund[35,1], bthreehund[40,1], bthreehund[45,1], bthreehund[50,1])/sum(bthreehund)))
  
  blackprojection <- cbind(bfive, bten, btwenty, bthirty, bforty, bfifty, bsixty, bseventy, beighty,
                           bninety, bhund, bhund50, btwohund, btwohund50, bthreehund)
  
  black.dist3 <- data.frame(blackdistfive,blackdistten,blackdisttwenty,blackdistthirty,blackdistforty,
                            blackdistfifty,blackdistsixty,blackdistseventy,blackdisteighty,
                            blackdistninety,blackdisthund, blackdisthund50, blackdisttwohund, 
                            blackdisttwohund50, blackdistthreehund)
  
  
}
## 6. To what degree are differences in the education distributions for Black and White people after three 
## generations attributable to differences in intergenerational mobility patterns between Black and White people?
## Black individuals having White mobility rates
{
  bb6lack15 <- as.vector(black15)*mwhite
  bb6lack20 <- as.vector(black20)*mwhite
  bb6lack25 <- as.vector(black25)*mwhite
  bb6lack30 <- as.vector(black30)*mwhite
  bb6lack35 <- as.vector(black35)*mwhite
  bb6lack40 <- as.vector(black40)*mwhite
  
  matrixSb<-diag(x=black$S[black$age<45])
  matrixAb<-matrix(data=0:0,nrow=40,ncol=5)
  matrixBb<-diag(x=c(black$S[black$age==45&black$education==1],
                     black$S[black$age==45&black$education==2],
                     black$S[black$age==45&black$education==3],
                     black$S[black$age==45&black$education==4],
                     black$S[black$age==45&black$education==5]))
  
  matrixMb<-cbind(matrixSb,rbind(matrixAb,matrixBb))
  zero <- matrix(data=0:0, nrow=5, ncol=5)
  
  matrixCb6 <-cbind(zero, zero, zero, bb6lack15, bb6lack20, bb6lack25, bb6lack30, bb6lack35, bb6lack40, zero)
  Mmatrixblack6 <- rbind(matrixCb6, matrixMb)
  
  bfive=Mmatrixblack6 %*% blackpop
  bten=(Mmatrixblack6 %^% 2) %*% blackpop
  btwenty=(Mmatrixblack6 %^% 4) %*% blackpop
  bthirty=(Mmatrixblack6 %^% 6) %*% blackpop
  bforty=(Mmatrixblack6 %^% 8) %*% blackpop
  bfifty=(Mmatrixblack6 %^% 10) %*% blackpop
  bsixty=(Mmatrixblack6 %^% 12) %*% blackpop
  bseventy=(Mmatrixblack6 %^% 14) %*% blackpop
  beighty=(Mmatrixblack6 %^% 16) %*% blackpop
  bninety=(Mmatrixblack6 %^% 18) %*% blackpop
  bhund=(Mmatrixblack6 %^% 20) %*% blackpop
  bhund50=(Mmatrixblack6 %^% 30) %*% blackpop
  btwohund=(Mmatrixblack6 %^% 40) %*% blackpop
  btwohund50=(Mmatrixblack6 %^% 50) %*% blackpop
  bthreehund=(Mmatrixblack6 %^% 60) %*% blackpop
  
  blackdistfive = c((sum(bfive[1,1], bfive[6,1], bfive[11,1], bfive[16,1], bfive[21,1],
                         bfive[26,1], bfive[31,1], bfive[36,1], bfive[41,1], bfive[46,1])/sum(bfive)), 
                    (sum(bfive[2,1], bfive[7,1], bfive[12,1], bfive[17,1], bfive[22,1],
                         bfive[27,1], bfive[32,1], bfive[37,1], bfive[42,1], bfive[47,1])/sum(bfive)),
                    (sum(bfive[3,1], bfive[8,1], bfive[13,1], bfive[18,1], bfive[23,1],
                         bfive[28,1], bfive[33,1], bfive[38,1], bfive[43,1], bfive[48,1])/sum(bfive)),
                    (sum(bfive[4,1], bfive[9,1], bfive[14,1], bfive[19,1], bfive[24,1],
                         bfive[29,1], bfive[34,1], bfive[39,1], bfive[44,1], bfive[49,1])/sum(bfive)),
                    (sum(bfive[5,1], bfive[10,1], bfive[15,1], bfive[20,1], bfive[25,1],
                         bfive[30,1], bfive[35,1], bfive[40,1], bfive[45,1], bfive[50,1])/sum(bfive)))
  
  blackdistten = c((sum(bten[1,1], bten[6,1], bten[11,1], bten[16,1], bten[21,1],
                        bten[26,1], bten[31,1], bten[36,1], bten[41,1], bten[46,1])/sum(bten)), 
                   (sum(bten[2,1], bten[7,1], bten[12,1], bten[17,1], bten[22,1],
                        bten[27,1], bten[32,1], bten[37,1], bten[42,1], bten[47,1])/sum(bten)),
                   (sum(bten[3,1], bten[8,1], bten[13,1], bten[18,1], bten[23,1],
                        bten[28,1], bten[33,1], bten[38,1], bten[43,1], bten[48,1])/sum(bten)),
                   (sum(bten[4,1], bten[9,1], bten[14,1], bten[19,1], bten[24,1],
                        bten[29,1], bten[34,1], bten[39,1], bten[44,1], bten[49,1])/sum(bten)),
                   (sum(bten[5,1], bten[10,1], bten[15,1], bten[20,1], bten[25,1],
                        bten[30,1], bten[35,1], bten[40,1], bten[45,1], bten[50,1])/sum(bten)))
  
  blackdisttwenty = c((sum(btwenty[1,1], btwenty[6,1], btwenty[11,1], btwenty[16,1], btwenty[21,1],
                           btwenty[26,1], btwenty[31,1], btwenty[36,1], btwenty[41,1], btwenty[46,1])/sum(btwenty)), 
                      (sum(btwenty[2,1], btwenty[7,1], btwenty[12,1], btwenty[17,1], btwenty[22,1],
                           btwenty[27,1], btwenty[32,1], btwenty[37,1], btwenty[42,1], btwenty[47,1])/sum(btwenty)),
                      (sum(btwenty[3,1], btwenty[8,1], btwenty[13,1], btwenty[18,1], btwenty[23,1],
                           btwenty[28,1], btwenty[33,1], btwenty[38,1], btwenty[43,1], btwenty[48,1])/sum(btwenty)),
                      (sum(btwenty[4,1], btwenty[9,1], btwenty[14,1], btwenty[19,1], btwenty[24,1],
                           btwenty[29,1], btwenty[34,1], btwenty[39,1], btwenty[44,1], btwenty[49,1])/sum(btwenty)),
                      (sum(btwenty[5,1], btwenty[10,1], btwenty[15,1], btwenty[20,1], btwenty[25,1],
                           btwenty[30,1], btwenty[35,1], btwenty[40,1], btwenty[45,1], btwenty[50,1])/sum(btwenty)))
  
  blackdistthirty = c((sum(bthirty[1,1], bthirty[6,1], bthirty[11,1], bthirty[16,1], bthirty[21,1],
                           bthirty[26,1], bthirty[31,1], bthirty[36,1], bthirty[41,1], bthirty[46,1])/sum(bthirty)), 
                      (sum(bthirty[2,1], bthirty[7,1], bthirty[12,1], bthirty[17,1], bthirty[22,1],
                           bthirty[27,1], bthirty[32,1], bthirty[37,1], bthirty[42,1], bthirty[47,1])/sum(bthirty)),
                      (sum(bthirty[3,1], bthirty[8,1], bthirty[13,1], bthirty[18,1], bthirty[23,1],
                           bthirty[28,1], bthirty[33,1], bthirty[38,1], bthirty[43,1], bthirty[48,1])/sum(bthirty)),
                      (sum(bthirty[4,1], bthirty[9,1], bthirty[14,1], bthirty[19,1], bthirty[24,1],
                           bthirty[29,1], bthirty[34,1], bthirty[39,1], bthirty[44,1], bthirty[49,1])/sum(bthirty)),
                      (sum(bthirty[5,1], bthirty[10,1], bthirty[15,1], bthirty[20,1], bthirty[25,1],
                           bthirty[30,1], bthirty[35,1], bthirty[40,1], bthirty[45,1], bthirty[50,1])/sum(bthirty)))
  
  blackdistforty = c((sum(bforty[1,1], bforty[6,1], bforty[11,1], bforty[16,1], bforty[21,1],
                          bforty[26,1], bforty[31,1], bforty[36,1], bforty[41,1], bforty[46,1])/sum(bforty)), 
                     (sum(bforty[2,1], bforty[7,1], bforty[12,1], bforty[17,1], bforty[22,1],
                          bforty[27,1], bforty[32,1], bforty[37,1], bforty[42,1], bforty[47,1])/sum(bforty)),
                     (sum(bforty[3,1], bforty[8,1], bforty[13,1], bforty[18,1], bforty[23,1],
                          bforty[28,1], bforty[33,1], bforty[38,1], bforty[43,1], bforty[48,1])/sum(bforty)),
                     (sum(bforty[4,1], bforty[9,1], bforty[14,1], bforty[19,1], bforty[24,1],
                          bforty[29,1], bforty[34,1], bforty[39,1], bforty[44,1], bforty[49,1])/sum(bforty)),
                     (sum(bforty[5,1], bforty[10,1], bforty[15,1], bforty[20,1], bforty[25,1],
                          bforty[30,1], bforty[35,1], bforty[40,1], bforty[45,1], bforty[50,1])/sum(bforty)))
  
  blackdistfifty = c((sum(bfifty[1,1], bfifty[6,1], bfifty[11,1], bfifty[16,1], bfifty[21,1],
                          bfifty[26,1], bfifty[31,1], bfifty[36,1], bfifty[41,1], bfifty[46,1])/sum(bfifty)), 
                     (sum(bfifty[2,1], bfifty[7,1], bfifty[12,1], bfifty[17,1], bfifty[22,1],
                          bfifty[27,1], bfifty[32,1], bfifty[37,1], bfifty[42,1], bfifty[47,1])/sum(bfifty)),
                     (sum(bfifty[3,1], bfifty[8,1], bfifty[13,1], bfifty[18,1], bfifty[23,1],
                          bfifty[28,1], bfifty[33,1], bfifty[38,1], bfifty[43,1], bfifty[48,1])/sum(bfifty)),
                     (sum(bfifty[4,1], bfifty[9,1], bfifty[14,1], bfifty[19,1], bfifty[24,1],
                          bfifty[29,1], bfifty[34,1], bfifty[39,1], bfifty[44,1], bfifty[49,1])/sum(bfifty)),
                     (sum(bfifty[5,1], bfifty[10,1], bfifty[15,1], bfifty[20,1], bfifty[25,1],
                          bfifty[30,1], bfifty[35,1], bfifty[40,1], bfifty[45,1], bfifty[50,1])/sum(bfifty)))
  
  blackdistsixty = c((sum(bsixty[1,1], bsixty[6,1], bsixty[11,1], bsixty[16,1], bsixty[21,1],
                          bsixty[26,1], bsixty[31,1], bsixty[36,1], bsixty[41,1], bsixty[46,1])/sum(bsixty)), 
                     (sum(bsixty[2,1], bsixty[7,1], bsixty[12,1], bsixty[17,1], bsixty[22,1],
                          bsixty[27,1], bsixty[32,1], bsixty[37,1], bsixty[42,1], bsixty[47,1])/sum(bsixty)),
                     (sum(bsixty[3,1], bsixty[8,1], bsixty[13,1], bsixty[18,1], bsixty[23,1],
                          bsixty[28,1], bsixty[33,1], bsixty[38,1], bsixty[43,1], bsixty[48,1])/sum(bsixty)),
                     (sum(bsixty[4,1], bsixty[9,1], bsixty[14,1], bsixty[19,1], bsixty[24,1],
                          bsixty[29,1], bsixty[34,1], bsixty[39,1], bsixty[44,1], bsixty[49,1])/sum(bsixty)),
                     (sum(bsixty[5,1], bsixty[10,1], bsixty[15,1], bsixty[20,1], bsixty[25,1],
                          bsixty[30,1], bsixty[35,1], bsixty[40,1], bsixty[45,1], bsixty[50,1])/sum(bsixty)))
  
  blackdistseventy = c((sum(bseventy[1,1], bseventy[6,1], bseventy[11,1], bseventy[16,1], bseventy[21,1],
                            bseventy[26,1], bseventy[31,1], bseventy[36,1], bseventy[41,1], bseventy[46,1])/sum(bseventy)), 
                       (sum(bseventy[2,1], bseventy[7,1], bseventy[12,1], bseventy[17,1], bseventy[22,1],
                            bseventy[27,1], bseventy[32,1], bseventy[37,1], bseventy[42,1], bseventy[47,1])/sum(bseventy)),
                       (sum(bseventy[3,1], bseventy[8,1], bseventy[13,1], bseventy[18,1], bseventy[23,1],
                            bseventy[28,1], bseventy[33,1], bseventy[38,1], bseventy[43,1], bseventy[48,1])/sum(bseventy)),
                       (sum(bseventy[4,1], bseventy[9,1], bseventy[14,1], bseventy[19,1], bseventy[24,1],
                            bseventy[29,1], bseventy[34,1], bseventy[39,1], bseventy[44,1], bseventy[49,1])/sum(bseventy)),
                       (sum(bseventy[5,1], bseventy[10,1], bseventy[15,1], bseventy[20,1], bseventy[25,1],
                            bseventy[30,1], bseventy[35,1], bseventy[40,1], bseventy[45,1], bseventy[50,1])/sum(bseventy)))
  
  blackdisteighty = c((sum(beighty[1,1], beighty[6,1], beighty[11,1], beighty[16,1], beighty[21,1],
                           beighty[26,1], beighty[31,1], beighty[36,1], beighty[41,1], beighty[46,1])/sum(beighty)), 
                      (sum(beighty[2,1], beighty[7,1], beighty[12,1], beighty[17,1], beighty[22,1],
                           beighty[27,1], beighty[32,1], beighty[37,1], beighty[42,1], beighty[47,1])/sum(beighty)),
                      (sum(beighty[3,1], beighty[8,1], beighty[13,1], beighty[18,1], beighty[23,1],
                           beighty[28,1], beighty[33,1], beighty[38,1], beighty[43,1], beighty[48,1])/sum(beighty)),
                      (sum(beighty[4,1], beighty[9,1], beighty[14,1], beighty[19,1], beighty[24,1],
                           beighty[29,1], beighty[34,1], beighty[39,1], beighty[44,1], beighty[49,1])/sum(beighty)),
                      (sum(beighty[5,1], beighty[10,1], beighty[15,1], beighty[20,1], beighty[25,1],
                           beighty[30,1], beighty[35,1], beighty[40,1], beighty[45,1], beighty[50,1])/sum(beighty)))
  
  blackdistninety = c((sum(bninety[1,1], bninety[6,1], bninety[11,1], bninety[16,1], bninety[21,1],
                           bninety[26,1], bninety[31,1], bninety[36,1], bninety[41,1], bninety[46,1])/sum(bninety)), 
                      (sum(bninety[2,1], bninety[7,1], bninety[12,1], bninety[17,1], bninety[22,1],
                           bninety[27,1], bninety[32,1], bninety[37,1], bninety[42,1], bninety[47,1])/sum(bninety)),
                      (sum(bninety[3,1], bninety[8,1], bninety[13,1], bninety[18,1], bninety[23,1],
                           bninety[28,1], bninety[33,1], bninety[38,1], bninety[43,1], bninety[48,1])/sum(bninety)),
                      (sum(bninety[4,1], bninety[9,1], bninety[14,1], bninety[19,1], bninety[24,1],
                           bninety[29,1], bninety[34,1], bninety[39,1], bninety[44,1], bninety[49,1])/sum(bninety)),
                      (sum(bninety[5,1], bninety[10,1], bninety[15,1], bninety[20,1], bninety[25,1],
                           bninety[30,1], bninety[35,1], bninety[40,1], bninety[45,1], bninety[50,1])/sum(bninety)))
  
  blackdisthund = c((sum(bhund[1,1], bhund[6,1], bhund[11,1], bhund[16,1], bhund[21,1],
                         bhund[26,1], bhund[31,1], bhund[36,1], bhund[41,1], bhund[46,1])/sum(bhund)), 
                    (sum(bhund[2,1], bhund[7,1], bhund[12,1], bhund[17,1], bhund[22,1],
                         bhund[27,1], bhund[32,1], bhund[37,1], bhund[42,1], bhund[47,1])/sum(bhund)),
                    (sum(bhund[3,1], bhund[8,1], bhund[13,1], bhund[18,1], bhund[23,1],
                         bhund[28,1], bhund[33,1], bhund[38,1], bhund[43,1], bhund[48,1])/sum(bhund)),
                    (sum(bhund[4,1], bhund[9,1], bhund[14,1], bhund[19,1], bhund[24,1],
                         bhund[29,1], bhund[34,1], bhund[39,1], bhund[44,1], bhund[49,1])/sum(bhund)),
                    (sum(bhund[5,1], bhund[10,1], bhund[15,1], bhund[20,1], bhund[25,1],
                         bhund[30,1], bhund[35,1], bhund[40,1], bhund[45,1], bhund[50,1])/sum(bhund)))
  
  blackdisthund50 = c((sum(bhund50[1,1], bhund50[6,1], bhund50[11,1], bhund50[16,1], bhund50[21,1],
                           bhund50[26,1], bhund50[31,1], bhund50[36,1], bhund50[41,1], bhund50[46,1])/sum(bhund50)), 
                      (sum(bhund50[2,1], bhund50[7,1], bhund50[12,1], bhund50[17,1], bhund50[22,1],
                           bhund50[27,1], bhund50[32,1], bhund50[37,1], bhund50[42,1], bhund50[47,1])/sum(bhund50)),
                      (sum(bhund50[3,1], bhund50[8,1], bhund50[13,1], bhund50[18,1], bhund50[23,1],
                           bhund50[28,1], bhund50[33,1], bhund50[38,1], bhund50[43,1], bhund50[48,1])/sum(bhund50)),
                      (sum(bhund50[4,1], bhund50[9,1], bhund50[14,1], bhund50[19,1], bhund50[24,1],
                           bhund50[29,1], bhund50[34,1], bhund50[39,1], bhund50[44,1], bhund50[49,1])/sum(bhund50)),
                      (sum(bhund50[5,1], bhund50[10,1], bhund50[15,1], bhund50[20,1], bhund50[25,1],
                           bhund50[30,1], bhund50[35,1], bhund50[40,1], bhund50[45,1], bhund50[50,1])/sum(bhund50)))
  
  blackdisttwohund = c((sum(btwohund[1,1], btwohund[6,1], btwohund[11,1], btwohund[16,1], btwohund[21,1],
                            btwohund[26,1], btwohund[31,1], btwohund[36,1], btwohund[41,1], btwohund[46,1])/sum(btwohund)), 
                       (sum(btwohund[2,1], btwohund[7,1], btwohund[12,1], btwohund[17,1], btwohund[22,1],
                            btwohund[27,1], btwohund[32,1], btwohund[37,1], btwohund[42,1], btwohund[47,1])/sum(btwohund)),
                       (sum(btwohund[3,1], btwohund[8,1], btwohund[13,1], btwohund[18,1], btwohund[23,1],
                            btwohund[28,1], btwohund[33,1], btwohund[38,1], btwohund[43,1], btwohund[48,1])/sum(btwohund)),
                       (sum(btwohund[4,1], btwohund[9,1], btwohund[14,1], btwohund[19,1], btwohund[24,1],
                            btwohund[29,1], btwohund[34,1], btwohund[39,1], btwohund[44,1], btwohund[49,1])/sum(btwohund)),
                       (sum(btwohund[5,1], btwohund[10,1], btwohund[15,1], btwohund[20,1], btwohund[25,1],
                            btwohund[30,1], btwohund[35,1], btwohund[40,1], btwohund[45,1], btwohund[50,1])/sum(btwohund)))
  
  blackdisttwohund50 = c((sum(btwohund50[1,1], btwohund50[6,1], btwohund50[11,1], btwohund50[16,1], btwohund50[21,1],
                              btwohund50[26,1], btwohund50[31,1], btwohund50[36,1], btwohund50[41,1], btwohund50[46,1])/sum(btwohund50)), 
                         (sum(btwohund50[2,1], btwohund50[7,1], btwohund50[12,1], btwohund50[17,1], btwohund50[22,1],
                              btwohund50[27,1], btwohund50[32,1], btwohund50[37,1], btwohund50[42,1], btwohund50[47,1])/sum(btwohund50)),
                         (sum(btwohund50[3,1], btwohund50[8,1], btwohund50[13,1], btwohund50[18,1], btwohund50[23,1],
                              btwohund50[28,1], btwohund50[33,1], btwohund50[38,1], btwohund50[43,1], btwohund50[48,1])/sum(btwohund50)),
                         (sum(btwohund50[4,1], btwohund50[9,1], btwohund50[14,1], btwohund50[19,1], btwohund50[24,1],
                              btwohund50[29,1], btwohund50[34,1], btwohund50[39,1], btwohund50[44,1], btwohund50[49,1])/sum(btwohund50)),
                         (sum(btwohund50[5,1], btwohund50[10,1], btwohund50[15,1], btwohund50[20,1], btwohund50[25,1],
                              btwohund50[30,1], btwohund50[35,1], btwohund50[40,1], btwohund50[45,1], btwohund50[50,1])/sum(btwohund50)))
  
  blackdistthreehund = c((sum(bthreehund[1,1], bthreehund[6,1], bthreehund[11,1], bthreehund[16,1], bthreehund[21,1],
                              bthreehund[26,1], bthreehund[31,1], bthreehund[36,1], bthreehund[41,1], bthreehund[46,1])/sum(bthreehund)), 
                         (sum(bthreehund[2,1], bthreehund[7,1], bthreehund[12,1], bthreehund[17,1], bthreehund[22,1],
                              bthreehund[27,1], bthreehund[32,1], bthreehund[37,1], bthreehund[42,1], bthreehund[47,1])/sum(bthreehund)),
                         (sum(bthreehund[3,1], bthreehund[8,1], bthreehund[13,1], bthreehund[18,1], bthreehund[23,1],
                              bthreehund[28,1], bthreehund[33,1], bthreehund[38,1], bthreehund[43,1], bthreehund[48,1])/sum(bthreehund)),
                         (sum(bthreehund[4,1], bthreehund[9,1], bthreehund[14,1], bthreehund[19,1], bthreehund[24,1],
                              bthreehund[29,1], bthreehund[34,1], bthreehund[39,1], bthreehund[44,1], bthreehund[49,1])/sum(bthreehund)),
                         (sum(bthreehund[5,1], bthreehund[10,1], bthreehund[15,1], bthreehund[20,1], bthreehund[25,1],
                              bthreehund[30,1], bthreehund[35,1], bthreehund[40,1], bthreehund[45,1], bthreehund[50,1])/sum(bthreehund)))
  
  blackprojection <- cbind(bfive, bten, btwenty, bthirty, bforty, bfifty, bsixty, bseventy, beighty,
                           bninety, bhund, bhund50, btwohund, btwohund50, bthreehund)
  
  black.dist4 <- data.frame(blackdistfive,blackdistten,blackdisttwenty,blackdistthirty,blackdistforty,
                            blackdistfifty,blackdistsixty,blackdistseventy,blackdisteighty,
                            blackdistninety,blackdisthund, blackdisthund50, blackdisttwohund, 
                            blackdisttwohund50, blackdistthreehund)
}
## 7. What would be the equilibrium education distributions for Black and White people if there were 
## NO intergenerational educational mobility?
{
  mblack0 <- matrix(c(1, 0.000, 0.000, 0.000, 0.000,
                      0.000, 1, 0.000, 0.000, 0.000,
                      0.000, 0.000, 1, 0.000, 0.000,
                      0.000, 0.000, 0.000, 1, 0.000,
                      0.000, 0.000, 0.000, 0.000, 1),
                    nrow=5, ncol=5, byrow = TRUE)  
  mwhite0 <- matrix(c(1, 0.000, 0.000, 0.000, 0.000,
                      0.000, 1, 0.000, 0.000, 0.000,
                      0.000, 0.000, 1, 0.000, 0.000,
                      0.000, 0.000, 0.000, 1, 0.000,
                      0.000, 0.000, 0.000, 0.000, 1),
                    nrow=5, ncol=5, byrow = TRUE)
  
  bblack15 <- as.vector(black15)*mblack0
  bblack20 <- as.vector(black20)*mblack0
  bblack25 <- as.vector(black25)*mblack0
  bblack30 <- as.vector(black30)*mblack0
  bblack35 <- as.vector(black35)*mblack0
  bblack40 <- as.vector(black40)*mblack0
  
  matrixSb<-diag(x=black$S[black$age<45])
  matrixAb<-matrix(data=0:0,nrow=40,ncol=5)
  matrixBb<-diag(x=c(black$S[black$age==45&black$education==1],
                     black$S[black$age==45&black$education==2],
                     black$S[black$age==45&black$education==3],
                     black$S[black$age==45&black$education==4],
                     black$S[black$age==45&black$education==5]))
  
  matrixMb<-cbind(matrixSb,rbind(matrixAb,matrixBb))
  zero <- matrix(data=0:0, nrow=5, ncol=5)
  
  matrixCb <-cbind(zero, zero, zero, bblack15, bblack20, bblack25, bblack30, bblack35, bblack40, zero)
  Mmatrixblack7 <- rbind(matrixCb, matrixMb)
  
  bwhite15 <- as.vector(white15)*mwhite0
  bwhite20 <- as.vector(white20)*mwhite0
  bwhite25 <- as.vector(white25)*mwhite0
  bwhite30 <- as.vector(white30)*mwhite0
  bwhite35 <- as.vector(white35)*mwhite0
  bwhite40 <- as.vector(white40)*mwhite0
  
  matrixSw<-diag(x=white$S[white$age<45])
  matrixAw<-matrix(data=0:0,nrow=40,ncol=5)
  matrixBw<-diag(x=c(white$S[white$age==45&white$education==1],
                     white$S[white$age==45&white$education==2],
                     white$S[white$age==45&white$education==3],
                     white$S[white$age==45&white$education==4],
                     white$S[white$age==45&white$education==5]))
  
  matrixMw<-cbind(matrixSw,rbind(matrixAw,matrixBw))
  
  matrixCw <-cbind(zero, zero, zero, bwhite15, bwhite20, bwhite25, bwhite30, bwhite35, bwhite40, zero)
  Mmatrixwhite7 <- rbind(matrixCw, matrixMw)
  
  bfive=Mmatrixblack7 %*% blackpop
  bten=(Mmatrixblack7 %^% 2) %*% blackpop
  btwenty=(Mmatrixblack7 %^% 4) %*% blackpop
  bthirty=(Mmatrixblack7 %^% 6) %*% blackpop
  bforty=(Mmatrixblack7 %^% 8) %*% blackpop
  bfifty=(Mmatrixblack7 %^% 10) %*% blackpop
  bsixty=(Mmatrixblack7 %^% 12) %*% blackpop
  bseventy=(Mmatrixblack7 %^% 14) %*% blackpop
  beighty=(Mmatrixblack7 %^% 16) %*% blackpop
  bninety=(Mmatrixblack7 %^% 18) %*% blackpop
  bhund=(Mmatrixblack7 %^% 20) %*% blackpop
  bhund50=(Mmatrixblack7 %^% 30) %*% blackpop
  btwohund=(Mmatrixblack7 %^% 40) %*% blackpop
  btwohund50=(Mmatrixblack7 %^% 50) %*% blackpop
  bthreehund=(Mmatrixblack7 %^% 60) %*% blackpop
  
  blackdistfive = c((sum(bfive[1,1], bfive[6,1], bfive[11,1], bfive[16,1], bfive[21,1],
                         bfive[26,1], bfive[31,1], bfive[36,1], bfive[41,1], bfive[46,1])/sum(bfive)), 
                    (sum(bfive[2,1], bfive[7,1], bfive[12,1], bfive[17,1], bfive[22,1],
                         bfive[27,1], bfive[32,1], bfive[37,1], bfive[42,1], bfive[47,1])/sum(bfive)),
                    (sum(bfive[3,1], bfive[8,1], bfive[13,1], bfive[18,1], bfive[23,1],
                         bfive[28,1], bfive[33,1], bfive[38,1], bfive[43,1], bfive[48,1])/sum(bfive)),
                    (sum(bfive[4,1], bfive[9,1], bfive[14,1], bfive[19,1], bfive[24,1],
                         bfive[29,1], bfive[34,1], bfive[39,1], bfive[44,1], bfive[49,1])/sum(bfive)),
                    (sum(bfive[5,1], bfive[10,1], bfive[15,1], bfive[20,1], bfive[25,1],
                         bfive[30,1], bfive[35,1], bfive[40,1], bfive[45,1], bfive[50,1])/sum(bfive)))
  
  blackdistten = c((sum(bten[1,1], bten[6,1], bten[11,1], bten[16,1], bten[21,1],
                        bten[26,1], bten[31,1], bten[36,1], bten[41,1], bten[46,1])/sum(bten)), 
                   (sum(bten[2,1], bten[7,1], bten[12,1], bten[17,1], bten[22,1],
                        bten[27,1], bten[32,1], bten[37,1], bten[42,1], bten[47,1])/sum(bten)),
                   (sum(bten[3,1], bten[8,1], bten[13,1], bten[18,1], bten[23,1],
                        bten[28,1], bten[33,1], bten[38,1], bten[43,1], bten[48,1])/sum(bten)),
                   (sum(bten[4,1], bten[9,1], bten[14,1], bten[19,1], bten[24,1],
                        bten[29,1], bten[34,1], bten[39,1], bten[44,1], bten[49,1])/sum(bten)),
                   (sum(bten[5,1], bten[10,1], bten[15,1], bten[20,1], bten[25,1],
                        bten[30,1], bten[35,1], bten[40,1], bten[45,1], bten[50,1])/sum(bten)))
  
  blackdisttwenty = c((sum(btwenty[1,1], btwenty[6,1], btwenty[11,1], btwenty[16,1], btwenty[21,1],
                           btwenty[26,1], btwenty[31,1], btwenty[36,1], btwenty[41,1], btwenty[46,1])/sum(btwenty)), 
                      (sum(btwenty[2,1], btwenty[7,1], btwenty[12,1], btwenty[17,1], btwenty[22,1],
                           btwenty[27,1], btwenty[32,1], btwenty[37,1], btwenty[42,1], btwenty[47,1])/sum(btwenty)),
                      (sum(btwenty[3,1], btwenty[8,1], btwenty[13,1], btwenty[18,1], btwenty[23,1],
                           btwenty[28,1], btwenty[33,1], btwenty[38,1], btwenty[43,1], btwenty[48,1])/sum(btwenty)),
                      (sum(btwenty[4,1], btwenty[9,1], btwenty[14,1], btwenty[19,1], btwenty[24,1],
                           btwenty[29,1], btwenty[34,1], btwenty[39,1], btwenty[44,1], btwenty[49,1])/sum(btwenty)),
                      (sum(btwenty[5,1], btwenty[10,1], btwenty[15,1], btwenty[20,1], btwenty[25,1],
                           btwenty[30,1], btwenty[35,1], btwenty[40,1], btwenty[45,1], btwenty[50,1])/sum(btwenty)))
  
  blackdistthirty = c((sum(bthirty[1,1], bthirty[6,1], bthirty[11,1], bthirty[16,1], bthirty[21,1],
                           bthirty[26,1], bthirty[31,1], bthirty[36,1], bthirty[41,1], bthirty[46,1])/sum(bthirty)), 
                      (sum(bthirty[2,1], bthirty[7,1], bthirty[12,1], bthirty[17,1], bthirty[22,1],
                           bthirty[27,1], bthirty[32,1], bthirty[37,1], bthirty[42,1], bthirty[47,1])/sum(bthirty)),
                      (sum(bthirty[3,1], bthirty[8,1], bthirty[13,1], bthirty[18,1], bthirty[23,1],
                           bthirty[28,1], bthirty[33,1], bthirty[38,1], bthirty[43,1], bthirty[48,1])/sum(bthirty)),
                      (sum(bthirty[4,1], bthirty[9,1], bthirty[14,1], bthirty[19,1], bthirty[24,1],
                           bthirty[29,1], bthirty[34,1], bthirty[39,1], bthirty[44,1], bthirty[49,1])/sum(bthirty)),
                      (sum(bthirty[5,1], bthirty[10,1], bthirty[15,1], bthirty[20,1], bthirty[25,1],
                           bthirty[30,1], bthirty[35,1], bthirty[40,1], bthirty[45,1], bthirty[50,1])/sum(bthirty)))
  
  blackdistforty = c((sum(bforty[1,1], bforty[6,1], bforty[11,1], bforty[16,1], bforty[21,1],
                          bforty[26,1], bforty[31,1], bforty[36,1], bforty[41,1], bforty[46,1])/sum(bforty)), 
                     (sum(bforty[2,1], bforty[7,1], bforty[12,1], bforty[17,1], bforty[22,1],
                          bforty[27,1], bforty[32,1], bforty[37,1], bforty[42,1], bforty[47,1])/sum(bforty)),
                     (sum(bforty[3,1], bforty[8,1], bforty[13,1], bforty[18,1], bforty[23,1],
                          bforty[28,1], bforty[33,1], bforty[38,1], bforty[43,1], bforty[48,1])/sum(bforty)),
                     (sum(bforty[4,1], bforty[9,1], bforty[14,1], bforty[19,1], bforty[24,1],
                          bforty[29,1], bforty[34,1], bforty[39,1], bforty[44,1], bforty[49,1])/sum(bforty)),
                     (sum(bforty[5,1], bforty[10,1], bforty[15,1], bforty[20,1], bforty[25,1],
                          bforty[30,1], bforty[35,1], bforty[40,1], bforty[45,1], bforty[50,1])/sum(bforty)))
  
  blackdistfifty = c((sum(bfifty[1,1], bfifty[6,1], bfifty[11,1], bfifty[16,1], bfifty[21,1],
                          bfifty[26,1], bfifty[31,1], bfifty[36,1], bfifty[41,1], bfifty[46,1])/sum(bfifty)), 
                     (sum(bfifty[2,1], bfifty[7,1], bfifty[12,1], bfifty[17,1], bfifty[22,1],
                          bfifty[27,1], bfifty[32,1], bfifty[37,1], bfifty[42,1], bfifty[47,1])/sum(bfifty)),
                     (sum(bfifty[3,1], bfifty[8,1], bfifty[13,1], bfifty[18,1], bfifty[23,1],
                          bfifty[28,1], bfifty[33,1], bfifty[38,1], bfifty[43,1], bfifty[48,1])/sum(bfifty)),
                     (sum(bfifty[4,1], bfifty[9,1], bfifty[14,1], bfifty[19,1], bfifty[24,1],
                          bfifty[29,1], bfifty[34,1], bfifty[39,1], bfifty[44,1], bfifty[49,1])/sum(bfifty)),
                     (sum(bfifty[5,1], bfifty[10,1], bfifty[15,1], bfifty[20,1], bfifty[25,1],
                          bfifty[30,1], bfifty[35,1], bfifty[40,1], bfifty[45,1], bfifty[50,1])/sum(bfifty)))
  
  blackdistsixty = c((sum(bsixty[1,1], bsixty[6,1], bsixty[11,1], bsixty[16,1], bsixty[21,1],
                          bsixty[26,1], bsixty[31,1], bsixty[36,1], bsixty[41,1], bsixty[46,1])/sum(bsixty)), 
                     (sum(bsixty[2,1], bsixty[7,1], bsixty[12,1], bsixty[17,1], bsixty[22,1],
                          bsixty[27,1], bsixty[32,1], bsixty[37,1], bsixty[42,1], bsixty[47,1])/sum(bsixty)),
                     (sum(bsixty[3,1], bsixty[8,1], bsixty[13,1], bsixty[18,1], bsixty[23,1],
                          bsixty[28,1], bsixty[33,1], bsixty[38,1], bsixty[43,1], bsixty[48,1])/sum(bsixty)),
                     (sum(bsixty[4,1], bsixty[9,1], bsixty[14,1], bsixty[19,1], bsixty[24,1],
                          bsixty[29,1], bsixty[34,1], bsixty[39,1], bsixty[44,1], bsixty[49,1])/sum(bsixty)),
                     (sum(bsixty[5,1], bsixty[10,1], bsixty[15,1], bsixty[20,1], bsixty[25,1],
                          bsixty[30,1], bsixty[35,1], bsixty[40,1], bsixty[45,1], bsixty[50,1])/sum(bsixty)))
  
  blackdistseventy = c((sum(bseventy[1,1], bseventy[6,1], bseventy[11,1], bseventy[16,1], bseventy[21,1],
                            bseventy[26,1], bseventy[31,1], bseventy[36,1], bseventy[41,1], bseventy[46,1])/sum(bseventy)), 
                       (sum(bseventy[2,1], bseventy[7,1], bseventy[12,1], bseventy[17,1], bseventy[22,1],
                            bseventy[27,1], bseventy[32,1], bseventy[37,1], bseventy[42,1], bseventy[47,1])/sum(bseventy)),
                       (sum(bseventy[3,1], bseventy[8,1], bseventy[13,1], bseventy[18,1], bseventy[23,1],
                            bseventy[28,1], bseventy[33,1], bseventy[38,1], bseventy[43,1], bseventy[48,1])/sum(bseventy)),
                       (sum(bseventy[4,1], bseventy[9,1], bseventy[14,1], bseventy[19,1], bseventy[24,1],
                            bseventy[29,1], bseventy[34,1], bseventy[39,1], bseventy[44,1], bseventy[49,1])/sum(bseventy)),
                       (sum(bseventy[5,1], bseventy[10,1], bseventy[15,1], bseventy[20,1], bseventy[25,1],
                            bseventy[30,1], bseventy[35,1], bseventy[40,1], bseventy[45,1], bseventy[50,1])/sum(bseventy)))
  
  blackdisteighty = c((sum(beighty[1,1], beighty[6,1], beighty[11,1], beighty[16,1], beighty[21,1],
                           beighty[26,1], beighty[31,1], beighty[36,1], beighty[41,1], beighty[46,1])/sum(beighty)), 
                      (sum(beighty[2,1], beighty[7,1], beighty[12,1], beighty[17,1], beighty[22,1],
                           beighty[27,1], beighty[32,1], beighty[37,1], beighty[42,1], beighty[47,1])/sum(beighty)),
                      (sum(beighty[3,1], beighty[8,1], beighty[13,1], beighty[18,1], beighty[23,1],
                           beighty[28,1], beighty[33,1], beighty[38,1], beighty[43,1], beighty[48,1])/sum(beighty)),
                      (sum(beighty[4,1], beighty[9,1], beighty[14,1], beighty[19,1], beighty[24,1],
                           beighty[29,1], beighty[34,1], beighty[39,1], beighty[44,1], beighty[49,1])/sum(beighty)),
                      (sum(beighty[5,1], beighty[10,1], beighty[15,1], beighty[20,1], beighty[25,1],
                           beighty[30,1], beighty[35,1], beighty[40,1], beighty[45,1], beighty[50,1])/sum(beighty)))
  
  blackdistninety = c((sum(bninety[1,1], bninety[6,1], bninety[11,1], bninety[16,1], bninety[21,1],
                           bninety[26,1], bninety[31,1], bninety[36,1], bninety[41,1], bninety[46,1])/sum(bninety)), 
                      (sum(bninety[2,1], bninety[7,1], bninety[12,1], bninety[17,1], bninety[22,1],
                           bninety[27,1], bninety[32,1], bninety[37,1], bninety[42,1], bninety[47,1])/sum(bninety)),
                      (sum(bninety[3,1], bninety[8,1], bninety[13,1], bninety[18,1], bninety[23,1],
                           bninety[28,1], bninety[33,1], bninety[38,1], bninety[43,1], bninety[48,1])/sum(bninety)),
                      (sum(bninety[4,1], bninety[9,1], bninety[14,1], bninety[19,1], bninety[24,1],
                           bninety[29,1], bninety[34,1], bninety[39,1], bninety[44,1], bninety[49,1])/sum(bninety)),
                      (sum(bninety[5,1], bninety[10,1], bninety[15,1], bninety[20,1], bninety[25,1],
                           bninety[30,1], bninety[35,1], bninety[40,1], bninety[45,1], bninety[50,1])/sum(bninety)))
  
  blackdisthund = c((sum(bhund[1,1], bhund[6,1], bhund[11,1], bhund[16,1], bhund[21,1],
                         bhund[26,1], bhund[31,1], bhund[36,1], bhund[41,1], bhund[46,1])/sum(bhund)), 
                    (sum(bhund[2,1], bhund[7,1], bhund[12,1], bhund[17,1], bhund[22,1],
                         bhund[27,1], bhund[32,1], bhund[37,1], bhund[42,1], bhund[47,1])/sum(bhund)),
                    (sum(bhund[3,1], bhund[8,1], bhund[13,1], bhund[18,1], bhund[23,1],
                         bhund[28,1], bhund[33,1], bhund[38,1], bhund[43,1], bhund[48,1])/sum(bhund)),
                    (sum(bhund[4,1], bhund[9,1], bhund[14,1], bhund[19,1], bhund[24,1],
                         bhund[29,1], bhund[34,1], bhund[39,1], bhund[44,1], bhund[49,1])/sum(bhund)),
                    (sum(bhund[5,1], bhund[10,1], bhund[15,1], bhund[20,1], bhund[25,1],
                         bhund[30,1], bhund[35,1], bhund[40,1], bhund[45,1], bhund[50,1])/sum(bhund)))
  
  blackdisthund50 = c((sum(bhund50[1,1], bhund50[6,1], bhund50[11,1], bhund50[16,1], bhund50[21,1],
                           bhund50[26,1], bhund50[31,1], bhund50[36,1], bhund50[41,1], bhund50[46,1])/sum(bhund50)), 
                      (sum(bhund50[2,1], bhund50[7,1], bhund50[12,1], bhund50[17,1], bhund50[22,1],
                           bhund50[27,1], bhund50[32,1], bhund50[37,1], bhund50[42,1], bhund50[47,1])/sum(bhund50)),
                      (sum(bhund50[3,1], bhund50[8,1], bhund50[13,1], bhund50[18,1], bhund50[23,1],
                           bhund50[28,1], bhund50[33,1], bhund50[38,1], bhund50[43,1], bhund50[48,1])/sum(bhund50)),
                      (sum(bhund50[4,1], bhund50[9,1], bhund50[14,1], bhund50[19,1], bhund50[24,1],
                           bhund50[29,1], bhund50[34,1], bhund50[39,1], bhund50[44,1], bhund50[49,1])/sum(bhund50)),
                      (sum(bhund50[5,1], bhund50[10,1], bhund50[15,1], bhund50[20,1], bhund50[25,1],
                           bhund50[30,1], bhund50[35,1], bhund50[40,1], bhund50[45,1], bhund50[50,1])/sum(bhund50)))
  
  blackdisttwohund = c((sum(btwohund[1,1], btwohund[6,1], btwohund[11,1], btwohund[16,1], btwohund[21,1],
                            btwohund[26,1], btwohund[31,1], btwohund[36,1], btwohund[41,1], btwohund[46,1])/sum(btwohund)), 
                       (sum(btwohund[2,1], btwohund[7,1], btwohund[12,1], btwohund[17,1], btwohund[22,1],
                            btwohund[27,1], btwohund[32,1], btwohund[37,1], btwohund[42,1], btwohund[47,1])/sum(btwohund)),
                       (sum(btwohund[3,1], btwohund[8,1], btwohund[13,1], btwohund[18,1], btwohund[23,1],
                            btwohund[28,1], btwohund[33,1], btwohund[38,1], btwohund[43,1], btwohund[48,1])/sum(btwohund)),
                       (sum(btwohund[4,1], btwohund[9,1], btwohund[14,1], btwohund[19,1], btwohund[24,1],
                            btwohund[29,1], btwohund[34,1], btwohund[39,1], btwohund[44,1], btwohund[49,1])/sum(btwohund)),
                       (sum(btwohund[5,1], btwohund[10,1], btwohund[15,1], btwohund[20,1], btwohund[25,1],
                            btwohund[30,1], btwohund[35,1], btwohund[40,1], btwohund[45,1], btwohund[50,1])/sum(btwohund)))
  
  blackdisttwohund50 = c((sum(btwohund50[1,1], btwohund50[6,1], btwohund50[11,1], btwohund50[16,1], btwohund50[21,1],
                              btwohund50[26,1], btwohund50[31,1], btwohund50[36,1], btwohund50[41,1], btwohund50[46,1])/sum(btwohund50)), 
                         (sum(btwohund50[2,1], btwohund50[7,1], btwohund50[12,1], btwohund50[17,1], btwohund50[22,1],
                              btwohund50[27,1], btwohund50[32,1], btwohund50[37,1], btwohund50[42,1], btwohund50[47,1])/sum(btwohund50)),
                         (sum(btwohund50[3,1], btwohund50[8,1], btwohund50[13,1], btwohund50[18,1], btwohund50[23,1],
                              btwohund50[28,1], btwohund50[33,1], btwohund50[38,1], btwohund50[43,1], btwohund50[48,1])/sum(btwohund50)),
                         (sum(btwohund50[4,1], btwohund50[9,1], btwohund50[14,1], btwohund50[19,1], btwohund50[24,1],
                              btwohund50[29,1], btwohund50[34,1], btwohund50[39,1], btwohund50[44,1], btwohund50[49,1])/sum(btwohund50)),
                         (sum(btwohund50[5,1], btwohund50[10,1], btwohund50[15,1], btwohund50[20,1], btwohund50[25,1],
                              btwohund50[30,1], btwohund50[35,1], btwohund50[40,1], btwohund50[45,1], btwohund50[50,1])/sum(btwohund50)))
  
  blackdistthreehund = c((sum(bthreehund[1,1], bthreehund[6,1], bthreehund[11,1], bthreehund[16,1], bthreehund[21,1],
                              bthreehund[26,1], bthreehund[31,1], bthreehund[36,1], bthreehund[41,1], bthreehund[46,1])/sum(bthreehund)), 
                         (sum(bthreehund[2,1], bthreehund[7,1], bthreehund[12,1], bthreehund[17,1], bthreehund[22,1],
                              bthreehund[27,1], bthreehund[32,1], bthreehund[37,1], bthreehund[42,1], bthreehund[47,1])/sum(bthreehund)),
                         (sum(bthreehund[3,1], bthreehund[8,1], bthreehund[13,1], bthreehund[18,1], bthreehund[23,1],
                              bthreehund[28,1], bthreehund[33,1], bthreehund[38,1], bthreehund[43,1], bthreehund[48,1])/sum(bthreehund)),
                         (sum(bthreehund[4,1], bthreehund[9,1], bthreehund[14,1], bthreehund[19,1], bthreehund[24,1],
                              bthreehund[29,1], bthreehund[34,1], bthreehund[39,1], bthreehund[44,1], bthreehund[49,1])/sum(bthreehund)),
                         (sum(bthreehund[5,1], bthreehund[10,1], bthreehund[15,1], bthreehund[20,1], bthreehund[25,1],
                              bthreehund[30,1], bthreehund[35,1], bthreehund[40,1], bthreehund[45,1], bthreehund[50,1])/sum(bthreehund)))
  
  blackprojection <- cbind(bfive, bten, btwenty, bthirty, bforty, bfifty, bsixty, bseventy, beighty,
                           bninety, bhund, bhund50, btwohund, btwohund50, bthreehund)
  
  black.dist7 <- data.frame(blackdistfive,blackdistten,blackdisttwenty,blackdistthirty,blackdistforty,
                            blackdistfifty,blackdistsixty,blackdistseventy,blackdisteighty,
                            blackdistninety,blackdisthund, blackdisthund50, blackdisttwohund, 
                            blackdisttwohund50, blackdistthreehund)
  
  wfive=Mmatrixwhite7 %*% whitepop
  wten=(Mmatrixwhite7 %^% 2) %*% whitepop
  wtwenty=(Mmatrixwhite7 %^% 4) %*% whitepop
  wthirty=(Mmatrixwhite7 %^% 6) %*% whitepop
  wforty=(Mmatrixwhite7 %^% 8) %*% whitepop
  wfifty=(Mmatrixwhite7 %^% 10) %*% whitepop
  wsixty=(Mmatrixwhite7 %^% 12) %*% whitepop
  wseventy=(Mmatrixwhite7 %^% 14) %*% whitepop
  weighty=(Mmatrixwhite7 %^% 16) %*% whitepop
  wninety=(Mmatrixwhite7 %^% 18) %*% whitepop
  whund=(Mmatrixwhite7 %^% 20) %*% whitepop
  whund50=(Mmatrixwhite7 %^% 30) %*% whitepop
  wtwohund=(Mmatrixwhite7 %^% 40) %*% whitepop
  wtwohund50=(Mmatrixwhite7 %^% 50) %*% whitepop
  wthreehund=(Mmatrixwhite7 %^% 60) %*% whitepop
  
  whitedistfive = c((sum(wfive[1,1], wfive[6,1], wfive[11,1], wfive[16,1], wfive[21,1],
                         wfive[26,1], wfive[31,1], wfive[36,1], wfive[41,1], wfive[46,1])/sum(wfive)), 
                    (sum(wfive[2,1], wfive[7,1], wfive[12,1], wfive[17,1], wfive[22,1],
                         wfive[27,1], wfive[32,1], wfive[37,1], wfive[42,1], wfive[47,1])/sum(wfive)),
                    (sum(wfive[3,1], wfive[8,1], wfive[13,1], wfive[18,1], wfive[23,1],
                         wfive[28,1], wfive[33,1], wfive[38,1], wfive[43,1], wfive[48,1])/sum(wfive)),
                    (sum(wfive[4,1], wfive[9,1], wfive[14,1], wfive[19,1], wfive[24,1],
                         wfive[29,1], wfive[34,1], wfive[39,1], wfive[44,1], wfive[49,1])/sum(wfive)),
                    (sum(wfive[5,1], wfive[10,1], wfive[15,1], wfive[20,1], wfive[25,1],
                         wfive[30,1], wfive[35,1], wfive[40,1], wfive[45,1], wfive[50,1])/sum(wfive)))
  
  whitedistten = c((sum(wten[1,1], wten[6,1], wten[11,1], wten[16,1], wten[21,1],
                        wten[26,1], wten[31,1], wten[36,1], wten[41,1], wten[46,1])/sum(wten)), 
                   (sum(wten[2,1], wten[7,1], wten[12,1], wten[17,1], wten[22,1],
                        wten[27,1], wten[32,1], wten[37,1], wten[42,1], wten[47,1])/sum(wten)),
                   (sum(wten[3,1], wten[8,1], wten[13,1], wten[18,1], wten[23,1],
                        wten[28,1], wten[33,1], wten[38,1], wten[43,1], wten[48,1])/sum(wten)),
                   (sum(wten[4,1], wten[9,1], wten[14,1], wten[19,1], wten[24,1],
                        wten[29,1], wten[34,1], wten[39,1], wten[44,1], wten[49,1])/sum(wten)),
                   (sum(wten[5,1], wten[10,1], wten[15,1], wten[20,1], wten[25,1],
                        wten[30,1], wten[35,1], wten[40,1], wten[45,1], wten[50,1])/sum(wten)))
  
  whitedisttwenty = c((sum(wtwenty[1,1], wtwenty[6,1], wtwenty[11,1], wtwenty[16,1], wtwenty[21,1],
                           wtwenty[26,1], wtwenty[31,1], wtwenty[36,1], wtwenty[41,1], wtwenty[46,1])/sum(wtwenty)), 
                      (sum(wtwenty[2,1], wtwenty[7,1], wtwenty[12,1], wtwenty[17,1], wtwenty[22,1],
                           wtwenty[27,1], wtwenty[32,1], wtwenty[37,1], wtwenty[42,1], wtwenty[47,1])/sum(wtwenty)),
                      (sum(wtwenty[3,1], wtwenty[8,1], wtwenty[13,1], wtwenty[18,1], wtwenty[23,1],
                           wtwenty[28,1], wtwenty[33,1], wtwenty[38,1], wtwenty[43,1], wtwenty[48,1])/sum(wtwenty)),
                      (sum(wtwenty[4,1], wtwenty[9,1], wtwenty[14,1], wtwenty[19,1], wtwenty[24,1],
                           wtwenty[29,1], wtwenty[34,1], wtwenty[39,1], wtwenty[44,1], wtwenty[49,1])/sum(wtwenty)),
                      (sum(wtwenty[5,1], wtwenty[10,1], wtwenty[15,1], wtwenty[20,1], wtwenty[25,1],
                           wtwenty[30,1], wtwenty[35,1], wtwenty[40,1], wtwenty[45,1], wtwenty[50,1])/sum(wtwenty)))
  
  whitedistthirty = c((sum(wthirty[1,1], wthirty[6,1], wthirty[11,1], wthirty[16,1], wthirty[21,1],
                           wthirty[26,1], wthirty[31,1], wthirty[36,1], wthirty[41,1], wthirty[46,1])/sum(wthirty)), 
                      (sum(wthirty[2,1], wthirty[7,1], wthirty[12,1], wthirty[17,1], wthirty[22,1],
                           wthirty[27,1], wthirty[32,1], wthirty[37,1], wthirty[42,1], wthirty[47,1])/sum(wthirty)),
                      (sum(wthirty[3,1], wthirty[8,1], wthirty[13,1], wthirty[18,1], wthirty[23,1],
                           wthirty[28,1], wthirty[33,1], wthirty[38,1], wthirty[43,1], wthirty[48,1])/sum(wthirty)),
                      (sum(wthirty[4,1], wthirty[9,1], wthirty[14,1], wthirty[19,1], wthirty[24,1],
                           wthirty[29,1], wthirty[34,1], wthirty[39,1], wthirty[44,1], wthirty[49,1])/sum(wthirty)),
                      (sum(wthirty[5,1], wthirty[10,1], wthirty[15,1], wthirty[20,1], wthirty[25,1],
                           wthirty[30,1], wthirty[35,1], wthirty[40,1], wthirty[45,1], wthirty[50,1])/sum(wthirty)))
  
  whitedistforty = c((sum(wforty[1,1], wforty[6,1], wforty[11,1], wforty[16,1], wforty[21,1],
                          wforty[26,1], wforty[31,1], wforty[36,1], wforty[41,1], wforty[46,1])/sum(wforty)), 
                     (sum(wforty[2,1], wforty[7,1], wforty[12,1], wforty[17,1], wforty[22,1],
                          wforty[27,1], wforty[32,1], wforty[37,1], wforty[42,1], wforty[47,1])/sum(wforty)),
                     (sum(wforty[3,1], wforty[8,1], wforty[13,1], wforty[18,1], wforty[23,1],
                          wforty[28,1], wforty[33,1], wforty[38,1], wforty[43,1], wforty[48,1])/sum(wforty)),
                     (sum(wforty[4,1], wforty[9,1], wforty[14,1], wforty[19,1], wforty[24,1],
                          wforty[29,1], wforty[34,1], wforty[39,1], wforty[44,1], wforty[49,1])/sum(wforty)),
                     (sum(wforty[5,1], wforty[10,1], wforty[15,1], wforty[20,1], wforty[25,1],
                          wforty[30,1], wforty[35,1], wforty[40,1], wforty[45,1], wforty[50,1])/sum(wforty)))
  
  whitedistfifty = c((sum(wfifty[1,1], wfifty[6,1], wfifty[11,1], wfifty[16,1], wfifty[21,1],
                          wfifty[26,1], wfifty[31,1], wfifty[36,1], wfifty[41,1], wfifty[46,1])/sum(wfifty)), 
                     (sum(wfifty[2,1], wfifty[7,1], wfifty[12,1], wfifty[17,1], wfifty[22,1],
                          wfifty[27,1], wfifty[32,1], wfifty[37,1], wfifty[42,1], wfifty[47,1])/sum(wfifty)),
                     (sum(wfifty[3,1], wfifty[8,1], wfifty[13,1], wfifty[18,1], wfifty[23,1],
                          wfifty[28,1], wfifty[33,1], wfifty[38,1], wfifty[43,1], wfifty[48,1])/sum(wfifty)),
                     (sum(wfifty[4,1], wfifty[9,1], wfifty[14,1], wfifty[19,1], wfifty[24,1],
                          wfifty[29,1], wfifty[34,1], wfifty[39,1], wfifty[44,1], wfifty[49,1])/sum(wfifty)),
                     (sum(wfifty[5,1], wfifty[10,1], wfifty[15,1], wfifty[20,1], wfifty[25,1],
                          wfifty[30,1], wfifty[35,1], wfifty[40,1], wfifty[45,1], wfifty[50,1])/sum(wfifty)))
  
  whitedistsixty = c((sum(wsixty[1,1], wsixty[6,1], wsixty[11,1], wsixty[16,1], wsixty[21,1],
                          wsixty[26,1], wsixty[31,1], wsixty[36,1], wsixty[41,1], wsixty[46,1])/sum(wsixty)), 
                     (sum(wsixty[2,1], wsixty[7,1], wsixty[12,1], wsixty[17,1], wsixty[22,1],
                          wsixty[27,1], wsixty[32,1], wsixty[37,1], wsixty[42,1], wsixty[47,1])/sum(wsixty)),
                     (sum(wsixty[3,1], wsixty[8,1], wsixty[13,1], wsixty[18,1], wsixty[23,1],
                          wsixty[28,1], wsixty[33,1], wsixty[38,1], wsixty[43,1], wsixty[48,1])/sum(wsixty)),
                     (sum(wsixty[4,1], wsixty[9,1], wsixty[14,1], wsixty[19,1], wsixty[24,1],
                          wsixty[29,1], wsixty[34,1], wsixty[39,1], wsixty[44,1], wsixty[49,1])/sum(wsixty)),
                     (sum(wsixty[5,1], wsixty[10,1], wsixty[15,1], wsixty[20,1], wsixty[25,1],
                          wsixty[30,1], wsixty[35,1], wsixty[40,1], wsixty[45,1], wsixty[50,1])/sum(wsixty)))
  
  whitedistseventy = c((sum(wseventy[1,1], wseventy[6,1], wseventy[11,1], wseventy[16,1], wseventy[21,1],
                            wseventy[26,1], wseventy[31,1], wseventy[36,1], wseventy[41,1], wseventy[46,1])/sum(wseventy)), 
                       (sum(wseventy[2,1], wseventy[7,1], wseventy[12,1], wseventy[17,1], wseventy[22,1],
                            wseventy[27,1], wseventy[32,1], wseventy[37,1], wseventy[42,1], wseventy[47,1])/sum(wseventy)),
                       (sum(wseventy[3,1], wseventy[8,1], wseventy[13,1], wseventy[18,1], wseventy[23,1],
                            wseventy[28,1], wseventy[33,1], wseventy[38,1], wseventy[43,1], wseventy[48,1])/sum(wseventy)),
                       (sum(wseventy[4,1], wseventy[9,1], wseventy[14,1], wseventy[19,1], wseventy[24,1],
                            wseventy[29,1], wseventy[34,1], wseventy[39,1], wseventy[44,1], wseventy[49,1])/sum(wseventy)),
                       (sum(wseventy[5,1], wseventy[10,1], wseventy[15,1], wseventy[20,1], wseventy[25,1],
                            wseventy[30,1], wseventy[35,1], wseventy[40,1], wseventy[45,1], wseventy[50,1])/sum(wseventy)))
  
  whitedisteighty = c((sum(weighty[1,1], weighty[6,1], weighty[11,1], weighty[16,1], weighty[21,1],
                           weighty[26,1], weighty[31,1], weighty[36,1], weighty[41,1], weighty[46,1])/sum(weighty)), 
                      (sum(weighty[2,1], weighty[7,1], weighty[12,1], weighty[17,1], weighty[22,1],
                           weighty[27,1], weighty[32,1], weighty[37,1], weighty[42,1], weighty[47,1])/sum(weighty)),
                      (sum(weighty[3,1], weighty[8,1], weighty[13,1], weighty[18,1], weighty[23,1],
                           weighty[28,1], weighty[33,1], weighty[38,1], weighty[43,1], weighty[48,1])/sum(weighty)),
                      (sum(weighty[4,1], weighty[9,1], weighty[14,1], weighty[19,1], weighty[24,1],
                           weighty[29,1], weighty[34,1], weighty[39,1], weighty[44,1], weighty[49,1])/sum(weighty)),
                      (sum(weighty[5,1], weighty[10,1], weighty[15,1], weighty[20,1], weighty[25,1],
                           weighty[30,1], weighty[35,1], weighty[40,1], weighty[45,1], weighty[50,1])/sum(weighty)))
  
  whitedistninety = c((sum(wninety[1,1], wninety[6,1], wninety[11,1], wninety[16,1], wninety[21,1],
                           wninety[26,1], wninety[31,1], wninety[36,1], wninety[41,1], wninety[46,1])/sum(wninety)), 
                      (sum(wninety[2,1], wninety[7,1], wninety[12,1], wninety[17,1], wninety[22,1],
                           wninety[27,1], wninety[32,1], wninety[37,1], wninety[42,1], wninety[47,1])/sum(wninety)),
                      (sum(wninety[3,1], wninety[8,1], wninety[13,1], wninety[18,1], wninety[23,1],
                           wninety[28,1], wninety[33,1], wninety[38,1], wninety[43,1], wninety[48,1])/sum(wninety)),
                      (sum(wninety[4,1], wninety[9,1], wninety[14,1], wninety[19,1], wninety[24,1],
                           wninety[29,1], wninety[34,1], wninety[39,1], wninety[44,1], wninety[49,1])/sum(wninety)),
                      (sum(wninety[5,1], wninety[10,1], wninety[15,1], wninety[20,1], wninety[25,1],
                           wninety[30,1], wninety[35,1], wninety[40,1], wninety[45,1], wninety[50,1])/sum(wninety)))
  
  whitedisthund = c((sum(whund[1,1], whund[6,1], whund[11,1], whund[16,1], whund[21,1],
                         whund[26,1], whund[31,1], whund[36,1], whund[41,1], whund[46,1])/sum(whund)), 
                    (sum(whund[2,1], whund[7,1], whund[12,1], whund[17,1], whund[22,1],
                         whund[27,1], whund[32,1], whund[37,1], whund[42,1], whund[47,1])/sum(whund)),
                    (sum(whund[3,1], whund[8,1], whund[13,1], whund[18,1], whund[23,1],
                         whund[28,1], whund[33,1], whund[38,1], whund[43,1], whund[48,1])/sum(whund)),
                    (sum(whund[4,1], whund[9,1], whund[14,1], whund[19,1], whund[24,1],
                         whund[29,1], whund[34,1], whund[39,1], whund[44,1], whund[49,1])/sum(whund)),
                    (sum(whund[5,1], whund[10,1], whund[15,1], whund[20,1], whund[25,1],
                         whund[30,1], whund[35,1], whund[40,1], whund[45,1], whund[50,1])/sum(whund)))
  
  whitedisthund50 = c((sum(whund50[1,1], whund50[6,1], whund50[11,1], whund50[16,1], whund50[21,1],
                           whund50[26,1], whund50[31,1], whund50[36,1], whund50[41,1], whund50[46,1])/sum(whund50)), 
                      (sum(whund50[2,1], whund50[7,1], whund50[12,1], whund50[17,1], whund50[22,1],
                           whund50[27,1], whund50[32,1], whund50[37,1], whund50[42,1], whund50[47,1])/sum(whund50)),
                      (sum(whund50[3,1], whund50[8,1], whund50[13,1], whund50[18,1], whund50[23,1],
                           whund50[28,1], whund50[33,1], whund50[38,1], whund50[43,1], whund50[48,1])/sum(whund50)),
                      (sum(whund50[4,1], whund50[9,1], whund50[14,1], whund50[19,1], whund50[24,1],
                           whund50[29,1], whund50[34,1], whund50[39,1], whund50[44,1], whund50[49,1])/sum(whund50)),
                      (sum(whund50[5,1], whund50[10,1], whund50[15,1], whund50[20,1], whund50[25,1],
                           whund50[30,1], whund50[35,1], whund50[40,1], whund50[45,1], whund50[50,1])/sum(whund50)))
  
  whitedisttwohund = c((sum(wtwohund[1,1], wtwohund[6,1], wtwohund[11,1], wtwohund[16,1], wtwohund[21,1],
                            wtwohund[26,1], wtwohund[31,1], wtwohund[36,1], wtwohund[41,1], wtwohund[46,1])/sum(wtwohund)), 
                       (sum(wtwohund[2,1], wtwohund[7,1], wtwohund[12,1], wtwohund[17,1], wtwohund[22,1],
                            wtwohund[27,1], wtwohund[32,1], wtwohund[37,1], wtwohund[42,1], wtwohund[47,1])/sum(wtwohund)),
                       (sum(wtwohund[3,1], wtwohund[8,1], wtwohund[13,1], wtwohund[18,1], wtwohund[23,1],
                            wtwohund[28,1], wtwohund[33,1], wtwohund[38,1], wtwohund[43,1], wtwohund[48,1])/sum(wtwohund)),
                       (sum(wtwohund[4,1], wtwohund[9,1], wtwohund[14,1], wtwohund[19,1], wtwohund[24,1],
                            wtwohund[29,1], wtwohund[34,1], wtwohund[39,1], wtwohund[44,1], wtwohund[49,1])/sum(wtwohund)),
                       (sum(wtwohund[5,1], wtwohund[10,1], wtwohund[15,1], wtwohund[20,1], wtwohund[25,1],
                            wtwohund[30,1], wtwohund[35,1], wtwohund[40,1], wtwohund[45,1], wtwohund[50,1])/sum(wtwohund)))
  
  whitedisttwohund50 = c((sum(wtwohund50[1,1], wtwohund50[6,1], wtwohund50[11,1], wtwohund50[16,1], wtwohund50[21,1],
                              wtwohund50[26,1], wtwohund50[31,1], wtwohund50[36,1], wtwohund50[41,1], wtwohund50[46,1])/sum(wtwohund50)), 
                         (sum(wtwohund50[2,1], wtwohund50[7,1], wtwohund50[12,1], wtwohund50[17,1], wtwohund50[22,1],
                              wtwohund50[27,1], wtwohund50[32,1], wtwohund50[37,1], wtwohund50[42,1], wtwohund50[47,1])/sum(wtwohund50)),
                         (sum(wtwohund50[3,1], wtwohund50[8,1], wtwohund50[13,1], wtwohund50[18,1], wtwohund50[23,1],
                              wtwohund50[28,1], wtwohund50[33,1], wtwohund50[38,1], wtwohund50[43,1], wtwohund50[48,1])/sum(wtwohund50)),
                         (sum(wtwohund50[4,1], wtwohund50[9,1], wtwohund50[14,1], wtwohund50[19,1], wtwohund50[24,1],
                              wtwohund50[29,1], wtwohund50[34,1], wtwohund50[39,1], wtwohund50[44,1], wtwohund50[49,1])/sum(wtwohund50)),
                         (sum(wtwohund50[5,1], wtwohund50[10,1], wtwohund50[15,1], wtwohund50[20,1], wtwohund50[25,1],
                              wtwohund50[30,1], wtwohund50[35,1], wtwohund50[40,1], wtwohund50[45,1], wtwohund50[50,1])/sum(wtwohund50)))
  
  whitedistthreehund = c((sum(wthreehund[1,1], wthreehund[6,1], wthreehund[11,1], wthreehund[16,1], wthreehund[21,1],
                              wthreehund[26,1], wthreehund[31,1], wthreehund[36,1], wthreehund[41,1], wthreehund[46,1])/sum(wthreehund)), 
                         (sum(wthreehund[2,1], wthreehund[7,1], wthreehund[12,1], wthreehund[17,1], wthreehund[22,1],
                              wthreehund[27,1], wthreehund[32,1], wthreehund[37,1], wthreehund[42,1], wthreehund[47,1])/sum(wthreehund)),
                         (sum(wthreehund[3,1], wthreehund[8,1], wthreehund[13,1], wthreehund[18,1], wthreehund[23,1],
                              wthreehund[28,1], wthreehund[33,1], wthreehund[38,1], wthreehund[43,1], wthreehund[48,1])/sum(wthreehund)),
                         (sum(wthreehund[4,1], wthreehund[9,1], wthreehund[14,1], wthreehund[19,1], wthreehund[24,1],
                              wthreehund[29,1], wthreehund[34,1], wthreehund[39,1], wthreehund[44,1], wthreehund[49,1])/sum(wthreehund)),
                         (sum(wthreehund[5,1], wthreehund[10,1], wthreehund[15,1], wthreehund[20,1], wthreehund[25,1],
                              wthreehund[30,1], wthreehund[35,1], wthreehund[40,1], wthreehund[45,1], wthreehund[50,1])/sum(wthreehund)))
  
  whiteprojection <- cbind(wfive, wten, wtwenty, wthirty, wforty, wfifty, wsixty, wseventy, weighty,
                           wninety, whund, whund50, wtwohund, wtwohund50, wthreehund)
  
  white.dist7 <- data.frame(whitedistfive,whitedistten,whitedisttwenty,whitedistthirty,whitedistforty,
                            whitedistfifty,whitedistsixty,whitedistseventy,whitedisteighty,
                            whitedistninety,whitedisthund, whitedisthund50, whitedisttwohund, 
                            whitedisttwohund50, whitedistthreehund)
}
## 8. What would the equilibrium distributions be if mother's and daughter's educational 
## attainments were statistically independent?
{
  ind <- matrix(c(0.2, 0.2, 0.2, 0.2, 0.2,
                  0.2, 0.2, 0.2, 0.2, 0.2,
                  0.2, 0.2, 0.2, 0.2, 0.2,
                  0.2, 0.2, 0.2, 0.2, 0.2,
                  0.2, 0.2, 0.2, 0.2, 0.2),
                nrow=5, ncol=5, byrow = TRUE)
  
  bblack15 <- as.vector(black15)*ind
  bblack20 <- as.vector(black20)*ind
  bblack25 <- as.vector(black25)*ind
  bblack30 <- as.vector(black30)*ind
  bblack35 <- as.vector(black35)*ind
  bblack40 <- as.vector(black40)*ind
  
  matrixSb<-diag(x=black$S[black$age<45])
  matrixAb<-matrix(data=0:0,nrow=40,ncol=5)
  matrixBb<-diag(x=c(black$S[black$age==45&black$education==1],
                     black$S[black$age==45&black$education==2],
                     black$S[black$age==45&black$education==3],
                     black$S[black$age==45&black$education==4],
                     black$S[black$age==45&black$education==5]))
  
  matrixMb<-cbind(matrixSb,rbind(matrixAb,matrixBb))
  zero <- matrix(data=0:0, nrow=5, ncol=5)
  
  matrixCb <-cbind(zero, zero, zero, bblack15, bblack20, bblack25, bblack30, bblack35, bblack40, zero)
  Mmatrixblack8 <- rbind(matrixCb, matrixMb)
  
  bwhite15 <- as.vector(white15)*ind
  bwhite20 <- as.vector(white20)*ind
  bwhite25 <- as.vector(white25)*ind
  bwhite30 <- as.vector(white30)*ind
  bwhite35 <- as.vector(white35)*ind
  bwhite40 <- as.vector(white40)*ind
  
  matrixSw<-diag(x=white$S[white$age<45])
  matrixAw<-matrix(data=0:0,nrow=40,ncol=5)
  matrixBw<-diag(x=c(white$S[white$age==45&white$education==1],
                     white$S[white$age==45&white$education==2],
                     white$S[white$age==45&white$education==3],
                     white$S[white$age==45&white$education==4],
                     white$S[white$age==45&white$education==5]))
  
  matrixMw<-cbind(matrixSw,rbind(matrixAw,matrixBw))
  
  matrixCw <-cbind(zero, zero, zero, bwhite15, bwhite20, bwhite25, bwhite30, bwhite35, bwhite40, zero)
  Mmatrixwhite7 <- rbind(matrixCw, matrixMw)
  
  bfive=Mmatrixblack8 %*% blackpop
  bten=(Mmatrixblack8 %^% 2) %*% blackpop
  btwenty=(Mmatrixblack8 %^% 4) %*% blackpop
  bthirty=(Mmatrixblack8 %^% 6) %*% blackpop
  bforty=(Mmatrixblack8 %^% 8) %*% blackpop
  bfifty=(Mmatrixblack8 %^% 10) %*% blackpop
  bsixty=(Mmatrixblack8 %^% 12) %*% blackpop
  bseventy=(Mmatrixblack8 %^% 14) %*% blackpop
  beighty=(Mmatrixblack8 %^% 16) %*% blackpop
  bninety=(Mmatrixblack8 %^% 18) %*% blackpop
  bhund=(Mmatrixblack8 %^% 20) %*% blackpop
  bhund50=(Mmatrixblack8 %^% 30) %*% blackpop
  btwohund=(Mmatrixblack8 %^% 40) %*% blackpop
  btwohund50=(Mmatrixblack8 %^% 50) %*% blackpop
  bthreehund=(Mmatrixblack8 %^% 60) %*% blackpop
  
  blackdistfive = c((sum(bfive[1,1], bfive[6,1], bfive[11,1], bfive[16,1], bfive[21,1],
                         bfive[26,1], bfive[31,1], bfive[36,1], bfive[41,1], bfive[46,1])/sum(bfive)), 
                    (sum(bfive[2,1], bfive[7,1], bfive[12,1], bfive[17,1], bfive[22,1],
                         bfive[27,1], bfive[32,1], bfive[37,1], bfive[42,1], bfive[47,1])/sum(bfive)),
                    (sum(bfive[3,1], bfive[8,1], bfive[13,1], bfive[18,1], bfive[23,1],
                         bfive[28,1], bfive[33,1], bfive[38,1], bfive[43,1], bfive[48,1])/sum(bfive)),
                    (sum(bfive[4,1], bfive[9,1], bfive[14,1], bfive[19,1], bfive[24,1],
                         bfive[29,1], bfive[34,1], bfive[39,1], bfive[44,1], bfive[49,1])/sum(bfive)),
                    (sum(bfive[5,1], bfive[10,1], bfive[15,1], bfive[20,1], bfive[25,1],
                         bfive[30,1], bfive[35,1], bfive[40,1], bfive[45,1], bfive[50,1])/sum(bfive)))
  
  blackdistten = c((sum(bten[1,1], bten[6,1], bten[11,1], bten[16,1], bten[21,1],
                        bten[26,1], bten[31,1], bten[36,1], bten[41,1], bten[46,1])/sum(bten)), 
                   (sum(bten[2,1], bten[7,1], bten[12,1], bten[17,1], bten[22,1],
                        bten[27,1], bten[32,1], bten[37,1], bten[42,1], bten[47,1])/sum(bten)),
                   (sum(bten[3,1], bten[8,1], bten[13,1], bten[18,1], bten[23,1],
                        bten[28,1], bten[33,1], bten[38,1], bten[43,1], bten[48,1])/sum(bten)),
                   (sum(bten[4,1], bten[9,1], bten[14,1], bten[19,1], bten[24,1],
                        bten[29,1], bten[34,1], bten[39,1], bten[44,1], bten[49,1])/sum(bten)),
                   (sum(bten[5,1], bten[10,1], bten[15,1], bten[20,1], bten[25,1],
                        bten[30,1], bten[35,1], bten[40,1], bten[45,1], bten[50,1])/sum(bten)))
  
  blackdisttwenty = c((sum(btwenty[1,1], btwenty[6,1], btwenty[11,1], btwenty[16,1], btwenty[21,1],
                           btwenty[26,1], btwenty[31,1], btwenty[36,1], btwenty[41,1], btwenty[46,1])/sum(btwenty)), 
                      (sum(btwenty[2,1], btwenty[7,1], btwenty[12,1], btwenty[17,1], btwenty[22,1],
                           btwenty[27,1], btwenty[32,1], btwenty[37,1], btwenty[42,1], btwenty[47,1])/sum(btwenty)),
                      (sum(btwenty[3,1], btwenty[8,1], btwenty[13,1], btwenty[18,1], btwenty[23,1],
                           btwenty[28,1], btwenty[33,1], btwenty[38,1], btwenty[43,1], btwenty[48,1])/sum(btwenty)),
                      (sum(btwenty[4,1], btwenty[9,1], btwenty[14,1], btwenty[19,1], btwenty[24,1],
                           btwenty[29,1], btwenty[34,1], btwenty[39,1], btwenty[44,1], btwenty[49,1])/sum(btwenty)),
                      (sum(btwenty[5,1], btwenty[10,1], btwenty[15,1], btwenty[20,1], btwenty[25,1],
                           btwenty[30,1], btwenty[35,1], btwenty[40,1], btwenty[45,1], btwenty[50,1])/sum(btwenty)))
  
  blackdistthirty = c((sum(bthirty[1,1], bthirty[6,1], bthirty[11,1], bthirty[16,1], bthirty[21,1],
                           bthirty[26,1], bthirty[31,1], bthirty[36,1], bthirty[41,1], bthirty[46,1])/sum(bthirty)), 
                      (sum(bthirty[2,1], bthirty[7,1], bthirty[12,1], bthirty[17,1], bthirty[22,1],
                           bthirty[27,1], bthirty[32,1], bthirty[37,1], bthirty[42,1], bthirty[47,1])/sum(bthirty)),
                      (sum(bthirty[3,1], bthirty[8,1], bthirty[13,1], bthirty[18,1], bthirty[23,1],
                           bthirty[28,1], bthirty[33,1], bthirty[38,1], bthirty[43,1], bthirty[48,1])/sum(bthirty)),
                      (sum(bthirty[4,1], bthirty[9,1], bthirty[14,1], bthirty[19,1], bthirty[24,1],
                           bthirty[29,1], bthirty[34,1], bthirty[39,1], bthirty[44,1], bthirty[49,1])/sum(bthirty)),
                      (sum(bthirty[5,1], bthirty[10,1], bthirty[15,1], bthirty[20,1], bthirty[25,1],
                           bthirty[30,1], bthirty[35,1], bthirty[40,1], bthirty[45,1], bthirty[50,1])/sum(bthirty)))
  
  blackdistforty = c((sum(bforty[1,1], bforty[6,1], bforty[11,1], bforty[16,1], bforty[21,1],
                          bforty[26,1], bforty[31,1], bforty[36,1], bforty[41,1], bforty[46,1])/sum(bforty)), 
                     (sum(bforty[2,1], bforty[7,1], bforty[12,1], bforty[17,1], bforty[22,1],
                          bforty[27,1], bforty[32,1], bforty[37,1], bforty[42,1], bforty[47,1])/sum(bforty)),
                     (sum(bforty[3,1], bforty[8,1], bforty[13,1], bforty[18,1], bforty[23,1],
                          bforty[28,1], bforty[33,1], bforty[38,1], bforty[43,1], bforty[48,1])/sum(bforty)),
                     (sum(bforty[4,1], bforty[9,1], bforty[14,1], bforty[19,1], bforty[24,1],
                          bforty[29,1], bforty[34,1], bforty[39,1], bforty[44,1], bforty[49,1])/sum(bforty)),
                     (sum(bforty[5,1], bforty[10,1], bforty[15,1], bforty[20,1], bforty[25,1],
                          bforty[30,1], bforty[35,1], bforty[40,1], bforty[45,1], bforty[50,1])/sum(bforty)))
  
  blackdistfifty = c((sum(bfifty[1,1], bfifty[6,1], bfifty[11,1], bfifty[16,1], bfifty[21,1],
                          bfifty[26,1], bfifty[31,1], bfifty[36,1], bfifty[41,1], bfifty[46,1])/sum(bfifty)), 
                     (sum(bfifty[2,1], bfifty[7,1], bfifty[12,1], bfifty[17,1], bfifty[22,1],
                          bfifty[27,1], bfifty[32,1], bfifty[37,1], bfifty[42,1], bfifty[47,1])/sum(bfifty)),
                     (sum(bfifty[3,1], bfifty[8,1], bfifty[13,1], bfifty[18,1], bfifty[23,1],
                          bfifty[28,1], bfifty[33,1], bfifty[38,1], bfifty[43,1], bfifty[48,1])/sum(bfifty)),
                     (sum(bfifty[4,1], bfifty[9,1], bfifty[14,1], bfifty[19,1], bfifty[24,1],
                          bfifty[29,1], bfifty[34,1], bfifty[39,1], bfifty[44,1], bfifty[49,1])/sum(bfifty)),
                     (sum(bfifty[5,1], bfifty[10,1], bfifty[15,1], bfifty[20,1], bfifty[25,1],
                          bfifty[30,1], bfifty[35,1], bfifty[40,1], bfifty[45,1], bfifty[50,1])/sum(bfifty)))
  
  blackdistsixty = c((sum(bsixty[1,1], bsixty[6,1], bsixty[11,1], bsixty[16,1], bsixty[21,1],
                          bsixty[26,1], bsixty[31,1], bsixty[36,1], bsixty[41,1], bsixty[46,1])/sum(bsixty)), 
                     (sum(bsixty[2,1], bsixty[7,1], bsixty[12,1], bsixty[17,1], bsixty[22,1],
                          bsixty[27,1], bsixty[32,1], bsixty[37,1], bsixty[42,1], bsixty[47,1])/sum(bsixty)),
                     (sum(bsixty[3,1], bsixty[8,1], bsixty[13,1], bsixty[18,1], bsixty[23,1],
                          bsixty[28,1], bsixty[33,1], bsixty[38,1], bsixty[43,1], bsixty[48,1])/sum(bsixty)),
                     (sum(bsixty[4,1], bsixty[9,1], bsixty[14,1], bsixty[19,1], bsixty[24,1],
                          bsixty[29,1], bsixty[34,1], bsixty[39,1], bsixty[44,1], bsixty[49,1])/sum(bsixty)),
                     (sum(bsixty[5,1], bsixty[10,1], bsixty[15,1], bsixty[20,1], bsixty[25,1],
                          bsixty[30,1], bsixty[35,1], bsixty[40,1], bsixty[45,1], bsixty[50,1])/sum(bsixty)))
  
  blackdistseventy = c((sum(bseventy[1,1], bseventy[6,1], bseventy[11,1], bseventy[16,1], bseventy[21,1],
                            bseventy[26,1], bseventy[31,1], bseventy[36,1], bseventy[41,1], bseventy[46,1])/sum(bseventy)), 
                       (sum(bseventy[2,1], bseventy[7,1], bseventy[12,1], bseventy[17,1], bseventy[22,1],
                            bseventy[27,1], bseventy[32,1], bseventy[37,1], bseventy[42,1], bseventy[47,1])/sum(bseventy)),
                       (sum(bseventy[3,1], bseventy[8,1], bseventy[13,1], bseventy[18,1], bseventy[23,1],
                            bseventy[28,1], bseventy[33,1], bseventy[38,1], bseventy[43,1], bseventy[48,1])/sum(bseventy)),
                       (sum(bseventy[4,1], bseventy[9,1], bseventy[14,1], bseventy[19,1], bseventy[24,1],
                            bseventy[29,1], bseventy[34,1], bseventy[39,1], bseventy[44,1], bseventy[49,1])/sum(bseventy)),
                       (sum(bseventy[5,1], bseventy[10,1], bseventy[15,1], bseventy[20,1], bseventy[25,1],
                            bseventy[30,1], bseventy[35,1], bseventy[40,1], bseventy[45,1], bseventy[50,1])/sum(bseventy)))
  
  blackdisteighty = c((sum(beighty[1,1], beighty[6,1], beighty[11,1], beighty[16,1], beighty[21,1],
                           beighty[26,1], beighty[31,1], beighty[36,1], beighty[41,1], beighty[46,1])/sum(beighty)), 
                      (sum(beighty[2,1], beighty[7,1], beighty[12,1], beighty[17,1], beighty[22,1],
                           beighty[27,1], beighty[32,1], beighty[37,1], beighty[42,1], beighty[47,1])/sum(beighty)),
                      (sum(beighty[3,1], beighty[8,1], beighty[13,1], beighty[18,1], beighty[23,1],
                           beighty[28,1], beighty[33,1], beighty[38,1], beighty[43,1], beighty[48,1])/sum(beighty)),
                      (sum(beighty[4,1], beighty[9,1], beighty[14,1], beighty[19,1], beighty[24,1],
                           beighty[29,1], beighty[34,1], beighty[39,1], beighty[44,1], beighty[49,1])/sum(beighty)),
                      (sum(beighty[5,1], beighty[10,1], beighty[15,1], beighty[20,1], beighty[25,1],
                           beighty[30,1], beighty[35,1], beighty[40,1], beighty[45,1], beighty[50,1])/sum(beighty)))
  
  blackdistninety = c((sum(bninety[1,1], bninety[6,1], bninety[11,1], bninety[16,1], bninety[21,1],
                           bninety[26,1], bninety[31,1], bninety[36,1], bninety[41,1], bninety[46,1])/sum(bninety)), 
                      (sum(bninety[2,1], bninety[7,1], bninety[12,1], bninety[17,1], bninety[22,1],
                           bninety[27,1], bninety[32,1], bninety[37,1], bninety[42,1], bninety[47,1])/sum(bninety)),
                      (sum(bninety[3,1], bninety[8,1], bninety[13,1], bninety[18,1], bninety[23,1],
                           bninety[28,1], bninety[33,1], bninety[38,1], bninety[43,1], bninety[48,1])/sum(bninety)),
                      (sum(bninety[4,1], bninety[9,1], bninety[14,1], bninety[19,1], bninety[24,1],
                           bninety[29,1], bninety[34,1], bninety[39,1], bninety[44,1], bninety[49,1])/sum(bninety)),
                      (sum(bninety[5,1], bninety[10,1], bninety[15,1], bninety[20,1], bninety[25,1],
                           bninety[30,1], bninety[35,1], bninety[40,1], bninety[45,1], bninety[50,1])/sum(bninety)))
  
  blackdisthund = c((sum(bhund[1,1], bhund[6,1], bhund[11,1], bhund[16,1], bhund[21,1],
                         bhund[26,1], bhund[31,1], bhund[36,1], bhund[41,1], bhund[46,1])/sum(bhund)), 
                    (sum(bhund[2,1], bhund[7,1], bhund[12,1], bhund[17,1], bhund[22,1],
                         bhund[27,1], bhund[32,1], bhund[37,1], bhund[42,1], bhund[47,1])/sum(bhund)),
                    (sum(bhund[3,1], bhund[8,1], bhund[13,1], bhund[18,1], bhund[23,1],
                         bhund[28,1], bhund[33,1], bhund[38,1], bhund[43,1], bhund[48,1])/sum(bhund)),
                    (sum(bhund[4,1], bhund[9,1], bhund[14,1], bhund[19,1], bhund[24,1],
                         bhund[29,1], bhund[34,1], bhund[39,1], bhund[44,1], bhund[49,1])/sum(bhund)),
                    (sum(bhund[5,1], bhund[10,1], bhund[15,1], bhund[20,1], bhund[25,1],
                         bhund[30,1], bhund[35,1], bhund[40,1], bhund[45,1], bhund[50,1])/sum(bhund)))
  
  blackdisthund50 = c((sum(bhund50[1,1], bhund50[6,1], bhund50[11,1], bhund50[16,1], bhund50[21,1],
                           bhund50[26,1], bhund50[31,1], bhund50[36,1], bhund50[41,1], bhund50[46,1])/sum(bhund50)), 
                      (sum(bhund50[2,1], bhund50[7,1], bhund50[12,1], bhund50[17,1], bhund50[22,1],
                           bhund50[27,1], bhund50[32,1], bhund50[37,1], bhund50[42,1], bhund50[47,1])/sum(bhund50)),
                      (sum(bhund50[3,1], bhund50[8,1], bhund50[13,1], bhund50[18,1], bhund50[23,1],
                           bhund50[28,1], bhund50[33,1], bhund50[38,1], bhund50[43,1], bhund50[48,1])/sum(bhund50)),
                      (sum(bhund50[4,1], bhund50[9,1], bhund50[14,1], bhund50[19,1], bhund50[24,1],
                           bhund50[29,1], bhund50[34,1], bhund50[39,1], bhund50[44,1], bhund50[49,1])/sum(bhund50)),
                      (sum(bhund50[5,1], bhund50[10,1], bhund50[15,1], bhund50[20,1], bhund50[25,1],
                           bhund50[30,1], bhund50[35,1], bhund50[40,1], bhund50[45,1], bhund50[50,1])/sum(bhund50)))
  
  blackdisttwohund = c((sum(btwohund[1,1], btwohund[6,1], btwohund[11,1], btwohund[16,1], btwohund[21,1],
                            btwohund[26,1], btwohund[31,1], btwohund[36,1], btwohund[41,1], btwohund[46,1])/sum(btwohund)), 
                       (sum(btwohund[2,1], btwohund[7,1], btwohund[12,1], btwohund[17,1], btwohund[22,1],
                            btwohund[27,1], btwohund[32,1], btwohund[37,1], btwohund[42,1], btwohund[47,1])/sum(btwohund)),
                       (sum(btwohund[3,1], btwohund[8,1], btwohund[13,1], btwohund[18,1], btwohund[23,1],
                            btwohund[28,1], btwohund[33,1], btwohund[38,1], btwohund[43,1], btwohund[48,1])/sum(btwohund)),
                       (sum(btwohund[4,1], btwohund[9,1], btwohund[14,1], btwohund[19,1], btwohund[24,1],
                            btwohund[29,1], btwohund[34,1], btwohund[39,1], btwohund[44,1], btwohund[49,1])/sum(btwohund)),
                       (sum(btwohund[5,1], btwohund[10,1], btwohund[15,1], btwohund[20,1], btwohund[25,1],
                            btwohund[30,1], btwohund[35,1], btwohund[40,1], btwohund[45,1], btwohund[50,1])/sum(btwohund)))
  
  blackdisttwohund50 = c((sum(btwohund50[1,1], btwohund50[6,1], btwohund50[11,1], btwohund50[16,1], btwohund50[21,1],
                              btwohund50[26,1], btwohund50[31,1], btwohund50[36,1], btwohund50[41,1], btwohund50[46,1])/sum(btwohund50)), 
                         (sum(btwohund50[2,1], btwohund50[7,1], btwohund50[12,1], btwohund50[17,1], btwohund50[22,1],
                              btwohund50[27,1], btwohund50[32,1], btwohund50[37,1], btwohund50[42,1], btwohund50[47,1])/sum(btwohund50)),
                         (sum(btwohund50[3,1], btwohund50[8,1], btwohund50[13,1], btwohund50[18,1], btwohund50[23,1],
                              btwohund50[28,1], btwohund50[33,1], btwohund50[38,1], btwohund50[43,1], btwohund50[48,1])/sum(btwohund50)),
                         (sum(btwohund50[4,1], btwohund50[9,1], btwohund50[14,1], btwohund50[19,1], btwohund50[24,1],
                              btwohund50[29,1], btwohund50[34,1], btwohund50[39,1], btwohund50[44,1], btwohund50[49,1])/sum(btwohund50)),
                         (sum(btwohund50[5,1], btwohund50[10,1], btwohund50[15,1], btwohund50[20,1], btwohund50[25,1],
                              btwohund50[30,1], btwohund50[35,1], btwohund50[40,1], btwohund50[45,1], btwohund50[50,1])/sum(btwohund50)))
  
  blackdistthreehund = c((sum(bthreehund[1,1], bthreehund[6,1], bthreehund[11,1], bthreehund[16,1], bthreehund[21,1],
                              bthreehund[26,1], bthreehund[31,1], bthreehund[36,1], bthreehund[41,1], bthreehund[46,1])/sum(bthreehund)), 
                         (sum(bthreehund[2,1], bthreehund[7,1], bthreehund[12,1], bthreehund[17,1], bthreehund[22,1],
                              bthreehund[27,1], bthreehund[32,1], bthreehund[37,1], bthreehund[42,1], bthreehund[47,1])/sum(bthreehund)),
                         (sum(bthreehund[3,1], bthreehund[8,1], bthreehund[13,1], bthreehund[18,1], bthreehund[23,1],
                              bthreehund[28,1], bthreehund[33,1], bthreehund[38,1], bthreehund[43,1], bthreehund[48,1])/sum(bthreehund)),
                         (sum(bthreehund[4,1], bthreehund[9,1], bthreehund[14,1], bthreehund[19,1], bthreehund[24,1],
                              bthreehund[29,1], bthreehund[34,1], bthreehund[39,1], bthreehund[44,1], bthreehund[49,1])/sum(bthreehund)),
                         (sum(bthreehund[5,1], bthreehund[10,1], bthreehund[15,1], bthreehund[20,1], bthreehund[25,1],
                              bthreehund[30,1], bthreehund[35,1], bthreehund[40,1], bthreehund[45,1], bthreehund[50,1])/sum(bthreehund)))
  
  blackprojection <- cbind(bfive, bten, btwenty, bthirty, bforty, bfifty, bsixty, bseventy, beighty,
                           bninety, bhund, bhund50, btwohund, btwohund50, bthreehund)
  
  black.dist8 <- data.frame(blackdistfive,blackdistten,blackdisttwenty,blackdistthirty,blackdistforty,
                            blackdistfifty,blackdistsixty,blackdistseventy,blackdisteighty,
                            blackdistninety,blackdisthund, blackdisthund50, blackdisttwohund, 
                            blackdisttwohund50, blackdistthreehund)
  
  wfive=Mmatrixwhite7 %*% whitepop
  wten=(Mmatrixwhite7 %^% 2) %*% whitepop
  wtwenty=(Mmatrixwhite7 %^% 4) %*% whitepop
  wthirty=(Mmatrixwhite7 %^% 6) %*% whitepop
  wforty=(Mmatrixwhite7 %^% 8) %*% whitepop
  wfifty=(Mmatrixwhite7 %^% 10) %*% whitepop
  wsixty=(Mmatrixwhite7 %^% 12) %*% whitepop
  wseventy=(Mmatrixwhite7 %^% 14) %*% whitepop
  weighty=(Mmatrixwhite7 %^% 16) %*% whitepop
  wninety=(Mmatrixwhite7 %^% 18) %*% whitepop
  whund=(Mmatrixwhite7 %^% 20) %*% whitepop
  whund50=(Mmatrixwhite7 %^% 30) %*% whitepop
  wtwohund=(Mmatrixwhite7 %^% 40) %*% whitepop
  wtwohund50=(Mmatrixwhite7 %^% 50) %*% whitepop
  wthreehund=(Mmatrixwhite7 %^% 60) %*% whitepop
  
  whitedistfive = c((sum(wfive[1,1], wfive[6,1], wfive[11,1], wfive[16,1], wfive[21,1],
                         wfive[26,1], wfive[31,1], wfive[36,1], wfive[41,1], wfive[46,1])/sum(wfive)), 
                    (sum(wfive[2,1], wfive[7,1], wfive[12,1], wfive[17,1], wfive[22,1],
                         wfive[27,1], wfive[32,1], wfive[37,1], wfive[42,1], wfive[47,1])/sum(wfive)),
                    (sum(wfive[3,1], wfive[8,1], wfive[13,1], wfive[18,1], wfive[23,1],
                         wfive[28,1], wfive[33,1], wfive[38,1], wfive[43,1], wfive[48,1])/sum(wfive)),
                    (sum(wfive[4,1], wfive[9,1], wfive[14,1], wfive[19,1], wfive[24,1],
                         wfive[29,1], wfive[34,1], wfive[39,1], wfive[44,1], wfive[49,1])/sum(wfive)),
                    (sum(wfive[5,1], wfive[10,1], wfive[15,1], wfive[20,1], wfive[25,1],
                         wfive[30,1], wfive[35,1], wfive[40,1], wfive[45,1], wfive[50,1])/sum(wfive)))
  
  whitedistten = c((sum(wten[1,1], wten[6,1], wten[11,1], wten[16,1], wten[21,1],
                        wten[26,1], wten[31,1], wten[36,1], wten[41,1], wten[46,1])/sum(wten)), 
                   (sum(wten[2,1], wten[7,1], wten[12,1], wten[17,1], wten[22,1],
                        wten[27,1], wten[32,1], wten[37,1], wten[42,1], wten[47,1])/sum(wten)),
                   (sum(wten[3,1], wten[8,1], wten[13,1], wten[18,1], wten[23,1],
                        wten[28,1], wten[33,1], wten[38,1], wten[43,1], wten[48,1])/sum(wten)),
                   (sum(wten[4,1], wten[9,1], wten[14,1], wten[19,1], wten[24,1],
                        wten[29,1], wten[34,1], wten[39,1], wten[44,1], wten[49,1])/sum(wten)),
                   (sum(wten[5,1], wten[10,1], wten[15,1], wten[20,1], wten[25,1],
                        wten[30,1], wten[35,1], wten[40,1], wten[45,1], wten[50,1])/sum(wten)))
  
  whitedisttwenty = c((sum(wtwenty[1,1], wtwenty[6,1], wtwenty[11,1], wtwenty[16,1], wtwenty[21,1],
                           wtwenty[26,1], wtwenty[31,1], wtwenty[36,1], wtwenty[41,1], wtwenty[46,1])/sum(wtwenty)), 
                      (sum(wtwenty[2,1], wtwenty[7,1], wtwenty[12,1], wtwenty[17,1], wtwenty[22,1],
                           wtwenty[27,1], wtwenty[32,1], wtwenty[37,1], wtwenty[42,1], wtwenty[47,1])/sum(wtwenty)),
                      (sum(wtwenty[3,1], wtwenty[8,1], wtwenty[13,1], wtwenty[18,1], wtwenty[23,1],
                           wtwenty[28,1], wtwenty[33,1], wtwenty[38,1], wtwenty[43,1], wtwenty[48,1])/sum(wtwenty)),
                      (sum(wtwenty[4,1], wtwenty[9,1], wtwenty[14,1], wtwenty[19,1], wtwenty[24,1],
                           wtwenty[29,1], wtwenty[34,1], wtwenty[39,1], wtwenty[44,1], wtwenty[49,1])/sum(wtwenty)),
                      (sum(wtwenty[5,1], wtwenty[10,1], wtwenty[15,1], wtwenty[20,1], wtwenty[25,1],
                           wtwenty[30,1], wtwenty[35,1], wtwenty[40,1], wtwenty[45,1], wtwenty[50,1])/sum(wtwenty)))
  
  whitedistthirty = c((sum(wthirty[1,1], wthirty[6,1], wthirty[11,1], wthirty[16,1], wthirty[21,1],
                           wthirty[26,1], wthirty[31,1], wthirty[36,1], wthirty[41,1], wthirty[46,1])/sum(wthirty)), 
                      (sum(wthirty[2,1], wthirty[7,1], wthirty[12,1], wthirty[17,1], wthirty[22,1],
                           wthirty[27,1], wthirty[32,1], wthirty[37,1], wthirty[42,1], wthirty[47,1])/sum(wthirty)),
                      (sum(wthirty[3,1], wthirty[8,1], wthirty[13,1], wthirty[18,1], wthirty[23,1],
                           wthirty[28,1], wthirty[33,1], wthirty[38,1], wthirty[43,1], wthirty[48,1])/sum(wthirty)),
                      (sum(wthirty[4,1], wthirty[9,1], wthirty[14,1], wthirty[19,1], wthirty[24,1],
                           wthirty[29,1], wthirty[34,1], wthirty[39,1], wthirty[44,1], wthirty[49,1])/sum(wthirty)),
                      (sum(wthirty[5,1], wthirty[10,1], wthirty[15,1], wthirty[20,1], wthirty[25,1],
                           wthirty[30,1], wthirty[35,1], wthirty[40,1], wthirty[45,1], wthirty[50,1])/sum(wthirty)))
  
  whitedistforty = c((sum(wforty[1,1], wforty[6,1], wforty[11,1], wforty[16,1], wforty[21,1],
                          wforty[26,1], wforty[31,1], wforty[36,1], wforty[41,1], wforty[46,1])/sum(wforty)), 
                     (sum(wforty[2,1], wforty[7,1], wforty[12,1], wforty[17,1], wforty[22,1],
                          wforty[27,1], wforty[32,1], wforty[37,1], wforty[42,1], wforty[47,1])/sum(wforty)),
                     (sum(wforty[3,1], wforty[8,1], wforty[13,1], wforty[18,1], wforty[23,1],
                          wforty[28,1], wforty[33,1], wforty[38,1], wforty[43,1], wforty[48,1])/sum(wforty)),
                     (sum(wforty[4,1], wforty[9,1], wforty[14,1], wforty[19,1], wforty[24,1],
                          wforty[29,1], wforty[34,1], wforty[39,1], wforty[44,1], wforty[49,1])/sum(wforty)),
                     (sum(wforty[5,1], wforty[10,1], wforty[15,1], wforty[20,1], wforty[25,1],
                          wforty[30,1], wforty[35,1], wforty[40,1], wforty[45,1], wforty[50,1])/sum(wforty)))
  
  whitedistfifty = c((sum(wfifty[1,1], wfifty[6,1], wfifty[11,1], wfifty[16,1], wfifty[21,1],
                          wfifty[26,1], wfifty[31,1], wfifty[36,1], wfifty[41,1], wfifty[46,1])/sum(wfifty)), 
                     (sum(wfifty[2,1], wfifty[7,1], wfifty[12,1], wfifty[17,1], wfifty[22,1],
                          wfifty[27,1], wfifty[32,1], wfifty[37,1], wfifty[42,1], wfifty[47,1])/sum(wfifty)),
                     (sum(wfifty[3,1], wfifty[8,1], wfifty[13,1], wfifty[18,1], wfifty[23,1],
                          wfifty[28,1], wfifty[33,1], wfifty[38,1], wfifty[43,1], wfifty[48,1])/sum(wfifty)),
                     (sum(wfifty[4,1], wfifty[9,1], wfifty[14,1], wfifty[19,1], wfifty[24,1],
                          wfifty[29,1], wfifty[34,1], wfifty[39,1], wfifty[44,1], wfifty[49,1])/sum(wfifty)),
                     (sum(wfifty[5,1], wfifty[10,1], wfifty[15,1], wfifty[20,1], wfifty[25,1],
                          wfifty[30,1], wfifty[35,1], wfifty[40,1], wfifty[45,1], wfifty[50,1])/sum(wfifty)))
  
  whitedistsixty = c((sum(wsixty[1,1], wsixty[6,1], wsixty[11,1], wsixty[16,1], wsixty[21,1],
                          wsixty[26,1], wsixty[31,1], wsixty[36,1], wsixty[41,1], wsixty[46,1])/sum(wsixty)), 
                     (sum(wsixty[2,1], wsixty[7,1], wsixty[12,1], wsixty[17,1], wsixty[22,1],
                          wsixty[27,1], wsixty[32,1], wsixty[37,1], wsixty[42,1], wsixty[47,1])/sum(wsixty)),
                     (sum(wsixty[3,1], wsixty[8,1], wsixty[13,1], wsixty[18,1], wsixty[23,1],
                          wsixty[28,1], wsixty[33,1], wsixty[38,1], wsixty[43,1], wsixty[48,1])/sum(wsixty)),
                     (sum(wsixty[4,1], wsixty[9,1], wsixty[14,1], wsixty[19,1], wsixty[24,1],
                          wsixty[29,1], wsixty[34,1], wsixty[39,1], wsixty[44,1], wsixty[49,1])/sum(wsixty)),
                     (sum(wsixty[5,1], wsixty[10,1], wsixty[15,1], wsixty[20,1], wsixty[25,1],
                          wsixty[30,1], wsixty[35,1], wsixty[40,1], wsixty[45,1], wsixty[50,1])/sum(wsixty)))
  
  whitedistseventy = c((sum(wseventy[1,1], wseventy[6,1], wseventy[11,1], wseventy[16,1], wseventy[21,1],
                            wseventy[26,1], wseventy[31,1], wseventy[36,1], wseventy[41,1], wseventy[46,1])/sum(wseventy)), 
                       (sum(wseventy[2,1], wseventy[7,1], wseventy[12,1], wseventy[17,1], wseventy[22,1],
                            wseventy[27,1], wseventy[32,1], wseventy[37,1], wseventy[42,1], wseventy[47,1])/sum(wseventy)),
                       (sum(wseventy[3,1], wseventy[8,1], wseventy[13,1], wseventy[18,1], wseventy[23,1],
                            wseventy[28,1], wseventy[33,1], wseventy[38,1], wseventy[43,1], wseventy[48,1])/sum(wseventy)),
                       (sum(wseventy[4,1], wseventy[9,1], wseventy[14,1], wseventy[19,1], wseventy[24,1],
                            wseventy[29,1], wseventy[34,1], wseventy[39,1], wseventy[44,1], wseventy[49,1])/sum(wseventy)),
                       (sum(wseventy[5,1], wseventy[10,1], wseventy[15,1], wseventy[20,1], wseventy[25,1],
                            wseventy[30,1], wseventy[35,1], wseventy[40,1], wseventy[45,1], wseventy[50,1])/sum(wseventy)))
  
  whitedisteighty = c((sum(weighty[1,1], weighty[6,1], weighty[11,1], weighty[16,1], weighty[21,1],
                           weighty[26,1], weighty[31,1], weighty[36,1], weighty[41,1], weighty[46,1])/sum(weighty)), 
                      (sum(weighty[2,1], weighty[7,1], weighty[12,1], weighty[17,1], weighty[22,1],
                           weighty[27,1], weighty[32,1], weighty[37,1], weighty[42,1], weighty[47,1])/sum(weighty)),
                      (sum(weighty[3,1], weighty[8,1], weighty[13,1], weighty[18,1], weighty[23,1],
                           weighty[28,1], weighty[33,1], weighty[38,1], weighty[43,1], weighty[48,1])/sum(weighty)),
                      (sum(weighty[4,1], weighty[9,1], weighty[14,1], weighty[19,1], weighty[24,1],
                           weighty[29,1], weighty[34,1], weighty[39,1], weighty[44,1], weighty[49,1])/sum(weighty)),
                      (sum(weighty[5,1], weighty[10,1], weighty[15,1], weighty[20,1], weighty[25,1],
                           weighty[30,1], weighty[35,1], weighty[40,1], weighty[45,1], weighty[50,1])/sum(weighty)))
  
  whitedistninety = c((sum(wninety[1,1], wninety[6,1], wninety[11,1], wninety[16,1], wninety[21,1],
                           wninety[26,1], wninety[31,1], wninety[36,1], wninety[41,1], wninety[46,1])/sum(wninety)), 
                      (sum(wninety[2,1], wninety[7,1], wninety[12,1], wninety[17,1], wninety[22,1],
                           wninety[27,1], wninety[32,1], wninety[37,1], wninety[42,1], wninety[47,1])/sum(wninety)),
                      (sum(wninety[3,1], wninety[8,1], wninety[13,1], wninety[18,1], wninety[23,1],
                           wninety[28,1], wninety[33,1], wninety[38,1], wninety[43,1], wninety[48,1])/sum(wninety)),
                      (sum(wninety[4,1], wninety[9,1], wninety[14,1], wninety[19,1], wninety[24,1],
                           wninety[29,1], wninety[34,1], wninety[39,1], wninety[44,1], wninety[49,1])/sum(wninety)),
                      (sum(wninety[5,1], wninety[10,1], wninety[15,1], wninety[20,1], wninety[25,1],
                           wninety[30,1], wninety[35,1], wninety[40,1], wninety[45,1], wninety[50,1])/sum(wninety)))
  
  whitedisthund = c((sum(whund[1,1], whund[6,1], whund[11,1], whund[16,1], whund[21,1],
                         whund[26,1], whund[31,1], whund[36,1], whund[41,1], whund[46,1])/sum(whund)), 
                    (sum(whund[2,1], whund[7,1], whund[12,1], whund[17,1], whund[22,1],
                         whund[27,1], whund[32,1], whund[37,1], whund[42,1], whund[47,1])/sum(whund)),
                    (sum(whund[3,1], whund[8,1], whund[13,1], whund[18,1], whund[23,1],
                         whund[28,1], whund[33,1], whund[38,1], whund[43,1], whund[48,1])/sum(whund)),
                    (sum(whund[4,1], whund[9,1], whund[14,1], whund[19,1], whund[24,1],
                         whund[29,1], whund[34,1], whund[39,1], whund[44,1], whund[49,1])/sum(whund)),
                    (sum(whund[5,1], whund[10,1], whund[15,1], whund[20,1], whund[25,1],
                         whund[30,1], whund[35,1], whund[40,1], whund[45,1], whund[50,1])/sum(whund)))
  
  whitedisthund50 = c((sum(whund50[1,1], whund50[6,1], whund50[11,1], whund50[16,1], whund50[21,1],
                           whund50[26,1], whund50[31,1], whund50[36,1], whund50[41,1], whund50[46,1])/sum(whund50)), 
                      (sum(whund50[2,1], whund50[7,1], whund50[12,1], whund50[17,1], whund50[22,1],
                           whund50[27,1], whund50[32,1], whund50[37,1], whund50[42,1], whund50[47,1])/sum(whund50)),
                      (sum(whund50[3,1], whund50[8,1], whund50[13,1], whund50[18,1], whund50[23,1],
                           whund50[28,1], whund50[33,1], whund50[38,1], whund50[43,1], whund50[48,1])/sum(whund50)),
                      (sum(whund50[4,1], whund50[9,1], whund50[14,1], whund50[19,1], whund50[24,1],
                           whund50[29,1], whund50[34,1], whund50[39,1], whund50[44,1], whund50[49,1])/sum(whund50)),
                      (sum(whund50[5,1], whund50[10,1], whund50[15,1], whund50[20,1], whund50[25,1],
                           whund50[30,1], whund50[35,1], whund50[40,1], whund50[45,1], whund50[50,1])/sum(whund50)))
  
  whitedisttwohund = c((sum(wtwohund[1,1], wtwohund[6,1], wtwohund[11,1], wtwohund[16,1], wtwohund[21,1],
                            wtwohund[26,1], wtwohund[31,1], wtwohund[36,1], wtwohund[41,1], wtwohund[46,1])/sum(wtwohund)), 
                       (sum(wtwohund[2,1], wtwohund[7,1], wtwohund[12,1], wtwohund[17,1], wtwohund[22,1],
                            wtwohund[27,1], wtwohund[32,1], wtwohund[37,1], wtwohund[42,1], wtwohund[47,1])/sum(wtwohund)),
                       (sum(wtwohund[3,1], wtwohund[8,1], wtwohund[13,1], wtwohund[18,1], wtwohund[23,1],
                            wtwohund[28,1], wtwohund[33,1], wtwohund[38,1], wtwohund[43,1], wtwohund[48,1])/sum(wtwohund)),
                       (sum(wtwohund[4,1], wtwohund[9,1], wtwohund[14,1], wtwohund[19,1], wtwohund[24,1],
                            wtwohund[29,1], wtwohund[34,1], wtwohund[39,1], wtwohund[44,1], wtwohund[49,1])/sum(wtwohund)),
                       (sum(wtwohund[5,1], wtwohund[10,1], wtwohund[15,1], wtwohund[20,1], wtwohund[25,1],
                            wtwohund[30,1], wtwohund[35,1], wtwohund[40,1], wtwohund[45,1], wtwohund[50,1])/sum(wtwohund)))
  
  whitedisttwohund50 = c((sum(wtwohund50[1,1], wtwohund50[6,1], wtwohund50[11,1], wtwohund50[16,1], wtwohund50[21,1],
                              wtwohund50[26,1], wtwohund50[31,1], wtwohund50[36,1], wtwohund50[41,1], wtwohund50[46,1])/sum(wtwohund50)), 
                         (sum(wtwohund50[2,1], wtwohund50[7,1], wtwohund50[12,1], wtwohund50[17,1], wtwohund50[22,1],
                              wtwohund50[27,1], wtwohund50[32,1], wtwohund50[37,1], wtwohund50[42,1], wtwohund50[47,1])/sum(wtwohund50)),
                         (sum(wtwohund50[3,1], wtwohund50[8,1], wtwohund50[13,1], wtwohund50[18,1], wtwohund50[23,1],
                              wtwohund50[28,1], wtwohund50[33,1], wtwohund50[38,1], wtwohund50[43,1], wtwohund50[48,1])/sum(wtwohund50)),
                         (sum(wtwohund50[4,1], wtwohund50[9,1], wtwohund50[14,1], wtwohund50[19,1], wtwohund50[24,1],
                              wtwohund50[29,1], wtwohund50[34,1], wtwohund50[39,1], wtwohund50[44,1], wtwohund50[49,1])/sum(wtwohund50)),
                         (sum(wtwohund50[5,1], wtwohund50[10,1], wtwohund50[15,1], wtwohund50[20,1], wtwohund50[25,1],
                              wtwohund50[30,1], wtwohund50[35,1], wtwohund50[40,1], wtwohund50[45,1], wtwohund50[50,1])/sum(wtwohund50)))
  
  whitedistthreehund = c((sum(wthreehund[1,1], wthreehund[6,1], wthreehund[11,1], wthreehund[16,1], wthreehund[21,1],
                              wthreehund[26,1], wthreehund[31,1], wthreehund[36,1], wthreehund[41,1], wthreehund[46,1])/sum(wthreehund)), 
                         (sum(wthreehund[2,1], wthreehund[7,1], wthreehund[12,1], wthreehund[17,1], wthreehund[22,1],
                              wthreehund[27,1], wthreehund[32,1], wthreehund[37,1], wthreehund[42,1], wthreehund[47,1])/sum(wthreehund)),
                         (sum(wthreehund[3,1], wthreehund[8,1], wthreehund[13,1], wthreehund[18,1], wthreehund[23,1],
                              wthreehund[28,1], wthreehund[33,1], wthreehund[38,1], wthreehund[43,1], wthreehund[48,1])/sum(wthreehund)),
                         (sum(wthreehund[4,1], wthreehund[9,1], wthreehund[14,1], wthreehund[19,1], wthreehund[24,1],
                              wthreehund[29,1], wthreehund[34,1], wthreehund[39,1], wthreehund[44,1], wthreehund[49,1])/sum(wthreehund)),
                         (sum(wthreehund[5,1], wthreehund[10,1], wthreehund[15,1], wthreehund[20,1], wthreehund[25,1],
                              wthreehund[30,1], wthreehund[35,1], wthreehund[40,1], wthreehund[45,1], wthreehund[50,1])/sum(wthreehund)))
  
  whiteprojection <- cbind(wfive, wten, wtwenty, wthirty, wforty, wfifty, wsixty, wseventy, weighty,
                           wninety, whund, whund50, wtwohund, wtwohund50, wthreehund)
  
  white.dist8 <- data.frame(whitedistfive,whitedistten,whitedisttwenty,whitedistthirty,whitedistforty,
                            whitedistfifty,whitedistsixty,whitedistseventy,whitedisteighty,
                            whitedistninety,whitedisthund, whitedisthund50, whitedisttwohund, 
                            whitedisttwohund50, whitedistthreehund)
}

# 9. Statistically independency among Blacks
blackdist.graph <- as.data.frame(t(black.dist8))
education <- c(1, 2, 3, 4, 5)
colnames(blackdist.graph) <- education

byears <- rep(c(5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100), each=5)
Education <- rep(c("0-8 Years","09-11 Years","12 Years","13-15 Years","16+ Years"), times=11)
bprob <- c(blackdistfive,blackdistten,blackdisttwenty,blackdistthirty,blackdistforty,
           blackdistfifty,blackdistsixty,blackdistseventy,blackdisteighty,
           blackdistninety,blackdisthund)
bdata <- data.frame(byears, bprob, Education)
ggplot(bdata, aes(x=byears, y=bprob, fill=Education)) + geom_area() + xlab("Years") + ylab("Proportion")


# White
whitedist.graph <- as.data.frame(t(white.dist8))
education <- c(1, 2, 3, 4, 5)
colnames(whitedist.graph) <- education

wyears <- rep(c(5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200, 250, 300), each=5)
Education <- rep(c("0-8 Years","09-11 Years","12 Years","13-15 Years","16+ Years"), times=15)
wprow <- c(whitedistfive,whitedistten,whitedisttwenty,whitedistthirty,whitedistforty,
           whitedistfifty,whitedistsixty,whitedistseventy,whitedisteighty,
           whitedistninety,whitedisthund, whitedisthund50, whitedisttwohund,
           whitedisttwohund50, whitedistthreehund)

wdata <- data.frame(wyears, wprow, Education)

ggplot(wdata, aes(x=wyears, y=wprow, fill=Education)) + geom_area()+ xlab("Years") + ylab("Proportion")