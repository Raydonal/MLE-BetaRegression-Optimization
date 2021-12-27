library(readr)
library(tidyverse)
library(Metrics)

#LEITURA DAS INSTANCIAS COM PARAMETROS:
# b0 = 4  b1 = -0.8  phi = 12 = exp(2.5) 
#
d1 <- read_rds("..\\DADOS RESULTADOS\\Result_uni_4_neg08_12_n30.rds")

d2 <- read_rds("..\\DADOS RESULTADOS\\Result_uni_4_neg08_12_n60.rds")

d3 <- read_rds("..\\DADOS RESULTADOS\\Result_uni_4_neg08_12_n90.rds")

d4 <- read_rds("..\\DADOS RESULTADOS\\Result_uni_4_neg08_12_n120.rds")



#LEITURA DAS INSTANCIAS COM PARAMETROS:
# b0 = 4  b1 = -0.8  phi = 148 = exp(5) 
#
d5 <- read_rds("..\\DADOS RESULTADOS\\Result_uni_4_neg08_148_n30.rds")

d6 <- read_rds("..\\DADOS RESULTADOS\\Result_uni_4_neg08_148_n60.rds")

d7 <- read_rds("..\\DADOS RESULTADOS\\Result_uni_4_neg08_148_n90.rds")

d8 <- read_rds("..\\DADOS RESULTADOS\\Result_uni_4_neg08_148_n120.rds")



#LEITURA DAS INSTANCIAS COM PARAMETROS:
# b0 = -2.5  b1 = -1.2  phi = 12 = exp(2.5) 
#
d9 <- read_rds("..\\DADOS RESULTADOS\\Result_uni_neg25_neg12_12_n30.rds")

d10 <- read_rds("..\\DADOS RESULTADOS\\Result_uni_neg25_neg12_12_n60.rds")

d11 <- read_rds("..\\DADOS RESULTADOS\\Result_uni_neg25_neg12_12_n90.rds")

d12 <- read_rds("..\\DADOS RESULTADOS\\Result_uni_neg25_neg12_12_n120.rds")



#LEITURA DAS INSTANCIAS COM PARAMETROS:
# b0 = -2.5  b1 = -1.2  phi = 148 = exp(5) 
#
d13 <- read_rds("..\\DADOS RESULTADOS\\Result_uni_neg25_neg12_148_n30.rds")

d14 <- read_rds("..\\DADOS RESULTADOS\\Result_uni_neg25_neg12_148_n60.rds")

d15 <- read_rds("..\\DADOS RESULTADOS\\Result_uni_neg25_neg12_148_n90.rds")

d16 <- read_rds("..\\DADOS RESULTADOS\\Result_uni_neg25_neg12_148_n120.rds")


d1$inst <- 1 
d2$inst <- 2 
d3$inst <- 3 
d4$inst <- 4
d5$inst <- 5
d6$inst <- 6
d7$inst <- 7
d8$inst <- 8
d9$inst <- 9
d10$inst <- 10
d11$inst <- 11
d12$inst <- 12
d13$inst <- 13
d14$inst <- 14
d15$inst <- 15
d16$inst <- 16

d1$grupo <- "Conjunto 1"
d2$grupo <- "Conjunto 1"
d3$grupo <- "Conjunto 1"
d4$grupo <- "Conjunto 1"

d5$grupo <- "Conjunto 2"
d6$grupo <- "Conjunto 2"
d7$grupo <- "Conjunto 2"
d8$grupo <- "Conjunto 2"

d9$grupo <- "Conjunto 3"
d10$grupo <- "Conjunto 3"
d11$grupo <- "Conjunto 3"
d12$grupo <- "Conjunto 3"

d13$grupo <- "Conjunto 4"
d14$grupo <- "Conjunto 4"
d15$grupo <- "Conjunto 4"
d16$grupo <- "Conjunto 4"

#DADOS SEM OS NA
d1na <- na.omit(d1)
d2na <- na.omit(d2)
d3na <- na.omit(d3)
d4na <- na.omit(d4)
d5na <- na.omit(d5)
d6na <- na.omit(d6)
d7na <- na.omit(d7)
d8na <- na.omit(d8)
d9na <- na.omit(d9)
d10na <- na.omit(d10)
d11na <- na.omit(d11)
d12na <- na.omit(d12)
d13na <- na.omit(d13)
d14na <- na.omit(d14)
d15na <- na.omit(d15)
d16na <- na.omit(d16)


data <- rbind(d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13, d14, d15, d16)

datana <- na.omit(data)

########################################
### round2, checar o EQM de cada metodo
### COMECA AQUI
########################################

d1 <- d1 %>% filter(Method == "ga")
d2 <- d2 %>% filter(Method == "ga")
d3 <- d3 %>% filter(Method == "ga")
d4 <- d4 %>% filter(Method == "ga")
d5 <- d5 %>% filter(Method == "ga")
d6 <- d6 %>% filter(Method == "ga")
d7 <- d7 %>% filter(Method == "ga")
d8 <- d8 %>% filter(Method == "ga")
d9 <- d9 %>% filter(Method == "ga")
d10 <- d10 %>% filter(Method == "ga")
d11 <- d11 %>% filter(Method == "ga")
d12 <- d12 %>% filter(Method == "ga")
d13 <- d13 %>% filter(Method == "ga")
d14 <- d14 %>% filter(Method == "ga")
d15 <- d15 %>% filter(Method == "ga")
d16 <- d16 %>% filter(Method == "ga")


ga_med_b0 <-  c(mean(as.numeric(d1$e_b0), na.rm = TRUE), mean(as.numeric(d2$e_b0), na.rm = TRUE),
                        mean(as.numeric(d3$e_b0), na.rm = TRUE), mean(as.numeric(d4$e_b0), na.rm = TRUE),
                        mean(as.numeric(d5$e_b0), na.rm = TRUE), mean(as.numeric(d6$e_b0), na.rm = TRUE),
                        mean(as.numeric(d7$e_b0), na.rm = TRUE), mean(as.numeric(d8$e_b0), na.rm = TRUE),
                        mean(as.numeric(d9$e_b0), na.rm = TRUE), mean(as.numeric(d10$e_b0), na.rm = TRUE),
                        mean(as.numeric(d11$e_b0), na.rm = TRUE), mean(as.numeric(d12$e_b0), na.rm = TRUE),
                        mean(as.numeric(d13$e_b0), na.rm = TRUE), mean(as.numeric(d14$e_b0), na.rm = TRUE),
                        mean(as.numeric(d15$e_b0), na.rm = TRUE), mean(as.numeric(d16$e_b0), na.rm = TRUE))


ga_dp_b0 <- c(sd(as.numeric(d1$e_b0), na.rm = TRUE), sd(as.numeric(d2$e_b0), na.rm = TRUE),
           sd(as.numeric(d3$e_b0), na.rm = TRUE), sd(as.numeric(d4$e_b0), na.rm = TRUE),
           sd(as.numeric(d5$e_b0), na.rm = TRUE), sd(as.numeric(d6$e_b0), na.rm = TRUE),
           sd(as.numeric(d7$e_b0), na.rm = TRUE), sd(as.numeric(d8$e_b0), na.rm = TRUE),
           sd(as.numeric(d9$e_b0), na.rm = TRUE), sd(as.numeric(d10$e_b0), na.rm = TRUE),
           sd(as.numeric(d11$e_b0), na.rm = TRUE), sd(as.numeric(d12$e_b0), na.rm = TRUE),
           sd(as.numeric(d13$e_b0), na.rm = TRUE), sd(as.numeric(d14$e_b0), na.rm = TRUE),
           sd(as.numeric(d15$e_b0), na.rm = TRUE), sd(as.numeric(d16$e_b0), na.rm = TRUE))

ga_vies_b0 <- c(-1*bias(d1$b0, d1$e_b0), -1*bias(d2$b0, d2$e_b0),
             -1*bias(d3$b0, d3$e_b0), -1*bias(d4$b0, d4$e_b0),
             -1*bias(d5$b0, d5$e_b0), -1*bias(d6$b0, d6$e_b0),
             -1*bias(d7$b0, d7$e_b0), -1*bias(d8$b0, d8$e_b0),
             -1*bias(d9$b0, d9$e_b0), -1*bias(d10$b0, d10$e_b0),
             -1*bias(d11$b0, d11$e_b0), -1*bias(d12$b0, d12$e_b0),
             -1*bias(d13$b0, d13$e_b0), -1*bias(d14$b0, d14$e_b0),
             -1*bias(d15$b0, d15$e_b0), -1*bias(d16$b0, d16$e_b0))

#var(d1na$e_b0) + (-1*bias(d1na$b0, d1na$e_b0))^2 ou Metrics::mse()
#
ga_EQM_b0 <- c(Metrics::mse(d1$b0, d1$e_b0), Metrics::mse(d2$b0, d2$e_b0),
            Metrics::mse(d3$b0, d3$e_b0), Metrics::mse(d4$b0, d4$e_b0),
            Metrics::mse(d5$b0, d5$e_b0), Metrics::mse(d6$b0, d6$e_b0),
            Metrics::mse(d7$b0, d7$e_b0), Metrics::mse(d8$b0, d8$e_b0),
            Metrics::mse(d9$b0, d9$e_b0), Metrics::mse(d10$b0, d10$e_b0),
            Metrics::mse(d11$b0, d11$e_b0), Metrics::mse(d12$b0, d12$e_b0),
            Metrics::mse(d13$b0, d13$e_b0), Metrics::mse(d14$b0, d14$e_b0),
            Metrics::mse(d15$b0, d15$e_b0), Metrics::mse(d16$b0, d16$e_b0))

ga_med_b1 <- c(mean(as.numeric(d1$e_b1), na.rm = TRUE), mean(as.numeric(d2$e_b1), na.rm = TRUE),
            mean(as.numeric(d3$e_b1), na.rm = TRUE), mean(as.numeric(d4$e_b1), na.rm = TRUE),
            mean(as.numeric(d5$e_b1), na.rm = TRUE), mean(as.numeric(d6$e_b1), na.rm = TRUE),
            mean(as.numeric(d7$e_b1), na.rm = TRUE), mean(as.numeric(d8$e_b1), na.rm = TRUE),
            mean(as.numeric(d9$e_b1), na.rm = TRUE), mean(as.numeric(d10$e_b1), na.rm = TRUE),
            mean(as.numeric(d11$e_b1), na.rm = TRUE), mean(as.numeric(d12$e_b1), na.rm = TRUE),
            mean(as.numeric(d13$e_b1), na.rm = TRUE), mean(as.numeric(d14$e_b1), na.rm = TRUE),
            mean(as.numeric(d15$e_b1), na.rm = TRUE), mean(as.numeric(d16$e_b1), na.rm = TRUE))


ga_dp_b1 <- c(sd(as.numeric(d1$e_b1), na.rm = TRUE), sd(as.numeric(d2$e_b1), na.rm = TRUE),
           sd(as.numeric(d3$e_b1), na.rm = TRUE), sd(as.numeric(d4$e_b1), na.rm = TRUE),
           sd(as.numeric(d5$e_b1), na.rm = TRUE), sd(as.numeric(d6$e_b1), na.rm = TRUE),
           sd(as.numeric(d7$e_b1), na.rm = TRUE), sd(as.numeric(d8$e_b1), na.rm = TRUE),
           sd(as.numeric(d9$e_b1), na.rm = TRUE), sd(as.numeric(d10$e_b1), na.rm = TRUE),
           sd(as.numeric(d11$e_b1), na.rm = TRUE), sd(as.numeric(d12$e_b1), na.rm = TRUE),
           sd(as.numeric(d13$e_b1), na.rm = TRUE), sd(as.numeric(d14$e_b1), na.rm = TRUE),
           sd(as.numeric(d15$e_b1), na.rm = TRUE), sd(as.numeric(d16$e_b1), na.rm = TRUE))


ga_vies_b1 <- c(-1*bias(d1$b1, d1$e_b1), -1*bias(d2$b1, d2$e_b1),
             -1*bias(d3$b1, d3$e_b1), -1*bias(d4$b1, d4$e_b1),
             -1*bias(d5$b1, d5$e_b1), -1*bias(d6$b1, d6$e_b1),
             -1*bias(d7$b1, d7$e_b1), -1*bias(d8$b1, d8$e_b1),
             -1*bias(d9$b1, d9$e_b1), -1*bias(d10$b1, d10$e_b1),
             -1*bias(d11$b1, d11$e_b1), -1*bias(d12$b1, d12$e_b1),
             -1*bias(d13$b1, d13$e_b1), -1*bias(d14$b1, d14$e_b1),
             -1*bias(d15$b1, d15$e_b1), -1*bias(d16$b1, d16$e_b1))


ga_EQM_b1 <- c(Metrics::mse(d1$b1, d1$e_b1), Metrics::mse(d2$b1, d2$e_b1),
            Metrics::mse(d3$b1, d3$e_b1), Metrics::mse(d4$b1, d4$e_b1),
            Metrics::mse(d5$b1, d5$e_b1), Metrics::mse(d6$b1, d6$e_b1),
            Metrics::mse(d7$b1, d7$e_b1), Metrics::mse(d8$b1, d8$e_b1),
            Metrics::mse(d9$b1, d9$e_b1), Metrics::mse(d10$b1, d10$e_b1),
            Metrics::mse(d11$b1, d11$e_b1), Metrics::mse(d12$b1, d12$e_b1),
            Metrics::mse(d13$b1, d13$e_b1), Metrics::mse(d14$b1, d14$e_b1),
            Metrics::mse(d15$b1, d15$e_b1), Metrics::mse(d16$b1, d16$e_b1))





tableres_ga <- cbind(inst, ga_med_b0, ga_dp_b0, ga_vies_b0, ga_EQM_b0, ga_med_b1, 
                     ga_dp_b1, ga_vies_b1, ga_EQM_b1)

df_tableres_ga <- as.data.frame(tableres_ga)


########################################
### round2, checar o EQM de cada metodo
### TERMINA AQUI
########################################


##### original

inst <- c(1, 2, 3, 4, 5, 6, 7, 8, 9 , 10, 11, 12, 13, 14 ,15, 16)

med_b0 <- c(mean(as.numeric(d1$e_b0), na.rm = TRUE), mean(as.numeric(d2$e_b0), na.rm = TRUE),
            mean(as.numeric(d3$e_b0), na.rm = TRUE), mean(as.numeric(d4$e_b0), na.rm = TRUE),
            mean(as.numeric(d5$e_b0), na.rm = TRUE), mean(as.numeric(d6$e_b0), na.rm = TRUE),
            mean(as.numeric(d7$e_b0), na.rm = TRUE), mean(as.numeric(d8$e_b0), na.rm = TRUE),
            mean(as.numeric(d9$e_b0), na.rm = TRUE), mean(as.numeric(d10$e_b0), na.rm = TRUE),
            mean(as.numeric(d11$e_b0), na.rm = TRUE), mean(as.numeric(d12$e_b0), na.rm = TRUE),
            mean(as.numeric(d13$e_b0), na.rm = TRUE), mean(as.numeric(d14$e_b0), na.rm = TRUE),
            mean(as.numeric(d15$e_b0), na.rm = TRUE), mean(as.numeric(d16$e_b0), na.rm = TRUE))

dp_b0 <- c(sd(as.numeric(d1$e_b0), na.rm = TRUE), sd(as.numeric(d2$e_b0), na.rm = TRUE),
           sd(as.numeric(d3$e_b0), na.rm = TRUE), sd(as.numeric(d4$e_b0), na.rm = TRUE),
           sd(as.numeric(d5$e_b0), na.rm = TRUE), sd(as.numeric(d6$e_b0), na.rm = TRUE),
           sd(as.numeric(d7$e_b0), na.rm = TRUE), sd(as.numeric(d8$e_b0), na.rm = TRUE),
           sd(as.numeric(d9$e_b0), na.rm = TRUE), sd(as.numeric(d10$e_b0), na.rm = TRUE),
           sd(as.numeric(d11$e_b0), na.rm = TRUE), sd(as.numeric(d12$e_b0), na.rm = TRUE),
           sd(as.numeric(d13$e_b0), na.rm = TRUE), sd(as.numeric(d14$e_b0), na.rm = TRUE),
           sd(as.numeric(d15$e_b0), na.rm = TRUE), sd(as.numeric(d16$e_b0), na.rm = TRUE))

vies_b0 <- c(-1*bias(d1na$b0, d1na$e_b0), -1*bias(d2na$b0, d2na$e_b0),
             -1*bias(d3na$b0, d3na$e_b0), -1*bias(d4na$b0, d4na$e_b0),
             -1*bias(d5na$b0, d5na$e_b0), -1*bias(d6na$b0, d6na$e_b0),
             -1*bias(d7na$b0, d7na$e_b0), -1*bias(d8na$b0, d8na$e_b0),
             -1*bias(d9na$b0, d9na$e_b0), -1*bias(d10na$b0, d10na$e_b0),
             -1*bias(d11na$b0, d11na$e_b0), -1*bias(d12na$b0, d12na$e_b0),
             -1*bias(d13na$b0, d13na$e_b0), -1*bias(d14na$b0, d14na$e_b0),
             -1*bias(d15na$b0, d15na$e_b0), -1*bias(d16na$b0, d16na$e_b0))

#var(d1na$e_b0) + (-1*bias(d1na$b0, d1na$e_b0))^2 ou Metrics::mse()
#
EQM_b0 <- c(Metrics::mse(d1na$b0, d1na$e_b0), Metrics::mse(d2na$b0, d2na$e_b0),
            Metrics::mse(d3na$b0, d3na$e_b0), Metrics::mse(d4na$b0, d4na$e_b0),
            Metrics::mse(d5na$b0, d5na$e_b0), Metrics::mse(d6na$b0, d6na$e_b0),
            Metrics::mse(d7na$b0, d7na$e_b0), Metrics::mse(d8na$b0, d8na$e_b0),
            Metrics::mse(d9na$b0, d9na$e_b0), Metrics::mse(d10na$b0, d10na$e_b0),
            Metrics::mse(d11na$b0, d11na$e_b0), Metrics::mse(d12na$b0, d12na$e_b0),
            Metrics::mse(d13na$b0, d13na$e_b0), Metrics::mse(d14na$b0, d14na$e_b0),
            Metrics::mse(d15na$b0, d15na$e_b0), Metrics::mse(d16na$b0, d16na$e_b0))

tableres2 <- cbind(inst, med_b0, dp_b0, vies_b0, EQM_b0)


med_b1 <- c(mean(as.numeric(d1$e_b1), na.rm = TRUE), mean(as.numeric(d2$e_b1), na.rm = TRUE),
            mean(as.numeric(d3$e_b1), na.rm = TRUE), mean(as.numeric(d4$e_b1), na.rm = TRUE),
            mean(as.numeric(d5$e_b1), na.rm = TRUE), mean(as.numeric(d6$e_b1), na.rm = TRUE),
            mean(as.numeric(d7$e_b1), na.rm = TRUE), mean(as.numeric(d8$e_b1), na.rm = TRUE),
            mean(as.numeric(d9$e_b1), na.rm = TRUE), mean(as.numeric(d10$e_b1), na.rm = TRUE),
            mean(as.numeric(d11$e_b1), na.rm = TRUE), mean(as.numeric(d12$e_b1), na.rm = TRUE),
            mean(as.numeric(d13$e_b1), na.rm = TRUE), mean(as.numeric(d14$e_b1), na.rm = TRUE),
            mean(as.numeric(d15$e_b1), na.rm = TRUE), mean(as.numeric(d16$e_b1), na.rm = TRUE))


dp_b1 <- c(sd(as.numeric(d1$e_b1), na.rm = TRUE), sd(as.numeric(d2$e_b1), na.rm = TRUE),
           sd(as.numeric(d3$e_b1), na.rm = TRUE), sd(as.numeric(d4$e_b1), na.rm = TRUE),
           sd(as.numeric(d5$e_b1), na.rm = TRUE), sd(as.numeric(d6$e_b1), na.rm = TRUE),
           sd(as.numeric(d7$e_b1), na.rm = TRUE), sd(as.numeric(d8$e_b1), na.rm = TRUE),
           sd(as.numeric(d9$e_b1), na.rm = TRUE), sd(as.numeric(d10$e_b1), na.rm = TRUE),
           sd(as.numeric(d11$e_b1), na.rm = TRUE), sd(as.numeric(d12$e_b1), na.rm = TRUE),
           sd(as.numeric(d13$e_b1), na.rm = TRUE), sd(as.numeric(d14$e_b1), na.rm = TRUE),
           sd(as.numeric(d15$e_b1), na.rm = TRUE), sd(as.numeric(d16$e_b1), na.rm = TRUE))


vies_b1 <- c(-1*bias(d1na$b1, d1na$e_b1), -1*bias(d2na$b1, d2na$e_b1),
             -1*bias(d3na$b1, d3na$e_b1), -1*bias(d4na$b1, d4na$e_b1),
             -1*bias(d5na$b1, d5na$e_b1), -1*bias(d6na$b1, d6na$e_b1),
             -1*bias(d7na$b1, d7na$e_b1), -1*bias(d8na$b1, d8na$e_b1),
             -1*bias(d9na$b1, d9na$e_b1), -1*bias(d10na$b1, d10na$e_b1),
             -1*bias(d11na$b1, d11na$e_b1), -1*bias(d12na$b1, d12na$e_b1),
             -1*bias(d13na$b1, d13na$e_b1), -1*bias(d14na$b1, d14na$e_b1),
             -1*bias(d15na$b1, d15na$e_b1), -1*bias(d16na$b1, d16na$e_b1))


EQM_b1 <- c(Metrics::mse(d1na$b1, d1na$e_b1), Metrics::mse(d2na$b1, d2na$e_b1),
            Metrics::mse(d3na$b1, d3na$e_b1), Metrics::mse(d4na$b1, d4na$e_b1),
            Metrics::mse(d5na$b1, d5na$e_b1), Metrics::mse(d6na$b1, d6na$e_b1),
            Metrics::mse(d7na$b1, d7na$e_b1), Metrics::mse(d8na$b1, d8na$e_b1),
            Metrics::mse(d9na$b1, d9na$e_b1), Metrics::mse(d10na$b1, d10na$e_b1),
            Metrics::mse(d11na$b1, d11na$e_b1), Metrics::mse(d12na$b1, d12na$e_b1),
            Metrics::mse(d13na$b1, d13na$e_b1), Metrics::mse(d14na$b1, d14na$e_b1),
            Metrics::mse(d15na$b1, d15na$e_b1), Metrics::mse(d16na$b1, d16na$e_b1))

med_phi <- c(mean(as.numeric(d1$e_phi), na.rm = TRUE), mean(as.numeric(d2$e_phi), na.rm = TRUE),
             mean(as.numeric(d3$e_phi), na.rm = TRUE), mean(as.numeric(d4$e_phi), na.rm = TRUE),
             mean(as.numeric(d5$e_phi), na.rm = TRUE), mean(as.numeric(d6$e_phi), na.rm = TRUE),
             mean(as.numeric(d7$e_phi), na.rm = TRUE), mean(as.numeric(d8$e_phi), na.rm = TRUE),
             mean(as.numeric(d9$e_phi), na.rm = TRUE), mean(as.numeric(d10$e_phi), na.rm = TRUE),
             mean(as.numeric(d11$e_phi), na.rm = TRUE), mean(as.numeric(d12$e_phi), na.rm = TRUE),
             mean(as.numeric(d13$e_phi), na.rm = TRUE), mean(as.numeric(d14$e_phi), na.rm = TRUE),
             mean(as.numeric(d15$e_phi), na.rm = TRUE), mean(as.numeric(d16$e_phi), na.rm = TRUE))


dp_phi <- c(sd(as.numeric(d1$e_phi), na.rm = TRUE), sd(as.numeric(d2$e_phi), na.rm = TRUE),
            sd(as.numeric(d3$e_phi), na.rm = TRUE), sd(as.numeric(d4$e_phi), na.rm = TRUE),
            sd(as.numeric(d5$e_phi), na.rm = TRUE), sd(as.numeric(d6$e_phi), na.rm = TRUE),
            sd(as.numeric(d7$e_phi), na.rm = TRUE), sd(as.numeric(d8$e_phi), na.rm = TRUE),
            sd(as.numeric(d9$e_phi), na.rm = TRUE), sd(as.numeric(d10$e_phi), na.rm = TRUE),
            sd(as.numeric(d11$e_phi), na.rm = TRUE), sd(as.numeric(d12$e_phi), na.rm = TRUE),
            sd(as.numeric(d13$e_phi), na.rm = TRUE), sd(as.numeric(d14$e_phi), na.rm = TRUE),
            sd(as.numeric(d15$e_phi), na.rm = TRUE), sd(as.numeric(d16$e_phi), na.rm = TRUE))


vies_phi <- c(-1*bias(d1na$phi, d1na$e_phi), -1*bias(d2na$phi, d2na$e_phi),
              -1*bias(d3na$phi, d3na$e_phi), -1*bias(d4na$phi, d4na$e_phi),
              -1*bias(d5na$phi, d5na$e_phi), -1*bias(d6na$phi, d6na$e_phi),
              -1*bias(d7na$phi, d7na$e_phi), -1*bias(d8na$phi, d8na$e_phi),
              -1*bias(d9na$phi, d9na$e_phi), -1*bias(d10na$phi, d10na$e_phi),
              -1*bias(d11na$phi, d11na$e_phi), -1*bias(d12na$phi, d12na$e_phi),
              -1*bias(d13na$phi, d13na$e_phi), -1*bias(d14na$phi, d14na$e_phi),
              -1*bias(d15na$phi, d15na$e_phi), -1*bias(d16na$phi, d16na$e_phi))


EQM_phi <- c(Metrics::mse(d1na$phi, d1na$e_phi), Metrics::mse(d2na$phi, d2na$e_phi),
             Metrics::mse(d3na$phi, d3na$e_phi), Metrics::mse(d4na$phi, d4na$e_phi),
             Metrics::mse(d5na$phi, d5na$e_phi), Metrics::mse(d6na$phi, d6na$e_phi),
             Metrics::mse(d7na$phi, d7na$e_phi), Metrics::mse(d8na$phi, d8na$e_phi),
             Metrics::mse(d9na$phi, d9na$e_phi), Metrics::mse(d10na$phi, d10na$e_phi),
             Metrics::mse(d11na$phi, d11na$e_phi), Metrics::mse(d12na$phi, d12na$e_phi),
             Metrics::mse(d13na$phi, d13na$e_phi), Metrics::mse(d14na$phi, d14na$e_phi),
             Metrics::mse(d15na$phi, d15na$e_phi), Metrics::mse(d16na$phi, d16na$e_phi))


tableres2 <- cbind(inst, med_b0, dp_b0, vies_b0, EQM_b0, med_b1, 
                  dp_b1, vies_b1, EQM_b1, med_phi, dp_phi, vies_phi, EQM_phi)

tableres2 <- as.data.frame(tableres2)

#
Tempo <- data %>% group_by(Method) %>%  summarise(TempoMed = mean(runtime, na.rm = TRUE))
#

### separar data total por método e tirar os NA

ga <- data %>% filter(Method == "ga")
ga <- na.omit(ga)

DEoptim <- data %>% filter(Method == "DEoptim")
DEoptim <- na.omit(DEoptim)

cma_es <- data %>% filter(Method == "cma_es")
cma_es <- na.omit(cma_es)

GenSA <- data %>% filter(Method == "GenSA")
GenSA <- na.omit(GenSA)

nloptr_crs <- data %>% filter(Method == "nloptr_crs")
nloptr_crs <- na.omit(nloptr_crs)

nloptr_d <- data %>% filter(Method == "nloptr_d")
nloptr_d <- na.omit(nloptr_d)

nloptr_d_l <- data %>% filter(Method == "nloptr_d_l")
nloptr_d_l <- na.omit(nloptr_d_l)

nloptr_i <- data %>% filter(Method == "nloptr_i")
nloptr_i <- na.omit(nloptr_i)

malschains <- data %>% filter(Method == "malschains")
malschains <- na.omit(malschains)

PSopt <- data %>% filter(Method == "PSopt")
PSopt <- na.omit(PSopt)

# separar d1,d2,d3,d4... por método



#########################


### GRAFICOS ###

#1
datana %>% #filter(!(Method %in% c("cma_es", "malschains"))) %>%
  filter(Fitness > 0) %>% 
  ggplot() + 
  geom_boxplot(aes(x = reorder(Method, -Fitness),
                   y = Fitness,
                   fill = factor(Method), alpha = 0.7), outlier.size = 0,
               na.rm = TRUE) + guides(alpha = FALSE) + 
  labs(x="Método", y = "Função Objetivo (log-verossimilhança)", fill = "Método")

#### GRAFICOS CERTINHO

datana %>%
  ggplot() + 
  geom_boxplot(aes(x = reorder(Method, -Fitness),
                   y = Fitness,
                   fill = factor(grupo)), outlier.size = 0,
               na.rm = TRUE) + ylim(0,500)


#2
datana %>% filter(Fitness > 0) %>% 
  ggplot() + 
  geom_boxplot(aes(x = factor(grupo),
                   y = Fitness,
                   fill = reorder(Method, -Fitness)), alpha = 0.7, outlier.size = 0,
               na.rm = TRUE) + guides(alpha = FALSE) + 
  labs(x="Conjunto de parâmetros", y = "Função Objetivo (log-verossimilhança)", fill = "Método")



#3
datana %>% 
  filter(e_b0 > -4) %>% filter(e_b0 < 5) %>%  
  ggplot() + 
  geom_boxplot(aes(x = reorder(Method, -e_b0),
                            y = e_b0,
                            fill = factor(samplesize)), outlier.size = 0,
                        na.rm = TRUE)  + facet_wrap(.~grupo, scales = "free") +
  geom_hline(data = datana, aes(yintercept = b0),
           size = 0.3, color = "red", na.rm = TRUE) +
  labs(x="Métodos", y = expression(paste(beta,"0 estimado")), fill = "Tamanho amostral")+
  theme(axis.text.x=element_text(angle=90, vjust=0.4,hjust=1))

#talvez?
datana %>% filter(!(Method %in% c("cma_es"))) %>%
  filter(e_b0 > -10) %>% 
  ggplot() + 
  geom_boxplot(aes(x = reorder(Method, -e_b0),
                   y = e_b0,
                   fill = factor(samplesize)), outlier.size = 0,
               na.rm = TRUE)  + facet_wrap(.~grupo, scales = "free") +
  geom_hline(data = datana, aes(yintercept = b0),
             size = 0.3, color = "red", na.rm = TRUE) +
  labs(x="Métodos", y = expression(paste(beta,"0 estimado")), fill = "Tamanho amostral")+
  theme(axis.text.x=element_text(angle=90, vjust=0.4,hjust=1))


#4
datana %>% 
  filter(e_b1 < 2) %>% filter(e_b1 > -3) %>% 
  ggplot() + 
  geom_boxplot(aes(x = reorder(Method, -e_b0),
                   y = e_b1,
                   fill = factor(samplesize)), outlier.size = 0,
               na.rm = TRUE)  + facet_wrap(.~grupo, scales = "free") +
  geom_hline(data = datana, aes(yintercept = b1),
             size = 1, color = "red", na.rm = TRUE) +
  labs(x="Métodos", y = expression(paste(beta,"1 estimado")), fill = "Tamanho amostral")+
  theme(axis.text.x=element_text(angle=90, vjust=0.4,hjust=1))


#?
datana %>% filter(!(Method %in% c("cma_es"))) %>%
  filter(e_b1 < 10) %>% 
  ggplot() + 
  geom_boxplot(aes(x = reorder(Method, -e_phi),
                   y = e_phi,
                   fill = factor(samplesize)), outlier.size = 0,
               na.rm = TRUE)  + facet_wrap(.~grupo, scales = "free") +
  geom_hline(data = datana, aes(yintercept = phi),
             size = 1, color = "red", na.rm = TRUE) +
  labs(x="Métodos", y = expression(paste(phi," estimado")), fill = "Tamanho amostral")+
  theme(axis.text.x=element_text(angle=90, vjust=0.4,hjust=1))


#### TEMPO DE EXECUCAO

Tempo <- data %>% group_by(Method) %>%  summarise(TempoMed = mean(runtime, na.rm = TRUE))

g1 <- datana %>% filter(grupo == "Conjunto 1")
Tempo1 <- g1 %>% group_by(Method) %>%  summarise(TempoMed = mean(runtime, na.rm = TRUE))
Tempo1$grupo <- "Conjunto 1"

g2 <- datana %>% filter(grupo == "Conjunto 2")
Tempo2 <- g2 %>% group_by(Method) %>%  summarise(TempoMed = mean(runtime, na.rm = TRUE))
Tempo2$grupo <- "Conjunto 2"

g3 <- datana %>% filter(grupo == "Conjunto 3")
Tempo3 <- g3 %>% group_by(Method) %>%  summarise(TempoMed = mean(runtime, na.rm = TRUE))
Tempo3$grupo <- "Conjunto 3"

g4 <- datana %>% filter(grupo == "Conjunto 4")
Tempo4 <- g4 %>% group_by(Method) %>%  summarise(TempoMed = mean(runtime, na.rm = TRUE))
Tempo4$grupo <- "Conjunto 4"

Tempogrupo <- rbind(Tempo1, Tempo2, Tempo3, Tempo4)


### GRAFICO 1, TEMPO TOTAL

Tempo %>% ggplot() + geom_col(aes(x = reorder(Method, TempoMed), 
                                  y = TempoMed,
                                  fill = Method, alpha = 0.7), color = "black")+
  labs(x="Método", y = "Tempo médio por execução em segundos", fill = "Método") + guides(alpha = FALSE)
#+  theme(axis.text.x=element_text(angle=90, vjust=0.4,hjust=1))

# datana %>% ggplot() + geom_boxplot(aes(x = reorder(Method, runtime),
#                                        y = runtime,
#                                        fill = Method)) +ylim(0,2)


  
### GRAFICO 2, TEMPO POR GRUPO

Tempogrupo %>% ggplot() + geom_col(aes(x = reorder(Method, TempoMed), 
                                  y = TempoMed,
                                  fill = Method, alpha = 0.7), color = "black")+
  labs(x="Método", y = "Tempo médio por execução em segundos", fill = "Método") + 
  facet_wrap(.~grupo) +  theme(axis.text.x=element_text(angle=90, vjust=0.4,hjust=1)) + guides(alpha = FALSE)


# ### GRAFICO 3, BOXPLOT TOTAL
# 
# datana %>% ggplot() + geom_boxplot(aes(x = reorder(Method, runtime),
#                                        y = runtime,
#                                        fill = factor(samplesize)), outlier.size = 0) + facet_wrap(.~grupo) +ylim(0,2)
#   
#   
  
  
  
  
#### ROUND 2 GRAFICOS


#1
datana %>% filter(!(Method %in% c("cma_es", "malschains", "ga", "nloptr_d", "nloptr_d_l"))) %>%
  filter(Fitness > 0) %>% 
  ggplot() + 
  geom_boxplot(aes(x = reorder(Method, -Fitness),
                   y = Fitness,
                   fill = factor(Method), alpha = 0.7), outlier.size = 0,
               na.rm = TRUE) + guides(alpha = FALSE) + 
  labs(x="Método", y = "Função Objetivo (log-verossimilhança)", fill = "Método")

#2
datana %>% filter(Fitness > 0) %>%
  filter(!(Method %in% c("cma_es", "malschains", "ga", "nloptr_d", "nloptr_d_l"))) %>%
  ggplot() + 
  geom_boxplot(aes(x = factor(grupo),
                   y = Fitness,
                   fill = reorder(Method, -Fitness)), alpha = 0.7, outlier.size = 0,
               na.rm = TRUE) + guides(alpha = FALSE) + 
  labs(x="Conjunto de parâmetros", y = "Função Objetivo (log-verossimilhança)", fill = "Método")



#3
datana %>% 
  filter(!(Method %in% c("cma_es", "malschains", "ga", "nloptr_d", "nloptr_d_l"))) %>%
  filter(e_b0 > -4) %>% filter(e_b0 < 5) %>%  
  ggplot() + 
  geom_boxplot(aes(x = reorder(Method, -e_b0),
                   y = e_b0,
                   fill = factor(samplesize)), outlier.size = 0,
               na.rm = TRUE)  + facet_wrap(.~grupo, scales = "free") +
  geom_hline(data = datana, aes(yintercept = b0),
             size = 0.3, color = "red", na.rm = TRUE) +
  labs(x="Métodos", y = expression(paste(beta,"0 estimado")), fill = "Tamanho amostral")+
  theme(axis.text.x=element_text(angle=90, vjust=0.4,hjust=1))

#4
datana %>% 
  filter(!(Method %in% c("cma_es", "malschains", "ga", "nloptr_d", "nloptr_d_l"))) %>%
  filter(e_b1 < 2) %>% filter(e_b1 > -3) %>% 
  ggplot() + 
  geom_boxplot(aes(x = reorder(Method, -e_b0),
                   y = e_b1,
                   fill = factor(samplesize)), outlier.size = 0,
               na.rm = TRUE)  + facet_wrap(.~grupo, scales = "free") +
  geom_hline(data = datana, aes(yintercept = b1),
             size = 1, color = "red", na.rm = TRUE) +
  labs(x="Métodos", y = expression(paste(beta,"1 estimado")), fill = "Tamanho amostral")+
  theme(axis.text.x=element_text(angle=90, vjust=0.4,hjust=1))



