library(robustbase)

ramka <- read.csv2("123.csv", skip = 6, header = FALSE, stringsAsFactors = FALSE)

elo <- rbind(ramka$V14)
colnames(elo) <- rbind(ramka$V3)
elo <- matrix(elo)
nowe <- matrix(data = elo, nrow = 8, ncol = 12)
rownames(nowe) <- c("A", "B", "C", "D", "E", "F", "G", "H")
colnames(nowe) <- seq(1,12)

# BCD 2 , EFG 2, BCD 3, EFG 3 to 4 pożywki dla jednego szczepu

species <- c("5270|EAEC", "5271|EAEC", "5272|EAEC", "5275|EAEC", "5276|EcN?")

mediums <- c("LB10", "TSB", 
             "BHI", "M63")

nowe <- nowe[-1,]
nowe <- nowe[-7,]

# usuwam kontrole

nowe <- nowe[,-1]
nowe <- nowe[,-11]


# każda kolumna to oddzielna pożywka oprócz dwóch pierwszych, bo to jakieś kontrole czy coś i dwóch ostatnich (dopytać dziewczyn co to)
nowa_ramka <- matrix(nowe,nrow = 3, ncol = 20)
colnames(nowa_ramka) <- rep(mediums, 5)
# dodać wiersz u góry i na dole z pożywkami, nazwa kolumny to szczep 

colMeans(nowa_ramka)
mediany <- colMedians(nowa_ramka)

### przepisać dane z CV

absorbance <- c(0.204, 0.345, 0.251, 0.158, 0.141, 0.283, 
                0.202, 0.303, 0.522, 0.303, 0.183, 0.184, 
                0.791, 0.126, 0.186, 0.207, 0.219, 0.316, 
                0.22, 0.327, 0.46, 0.267, 0.202, 0.174, 
                0.195, 0.408, 0.611, 0.859, 0.872, 0.665, 
                0.29, 0.469, 0.679, 0.31, 0.265, 0.199, 
                0.44, 0.149, 0.172, 0.152, 0.189, 0.138, 
                0.185, 0.209, 0.206, 0.18, 0.168, 0.142, 
                0.032, 0.031, 0.031, 0.031, 0.032, 0.032, 
                0.146, 0.316, 0.243, 0.409, 0.189, 0.154)

absorbance <- matrix(absorbance, nrow = 6, ncol = 10)

rownames(absorbance) <- c("B", "C", "D", "E", "F", "G")
colnames(absorbance) <- seq(2,11)

new_absorbance <- matrix(absorbance,nrow = 3, ncol = 20)
colnames(new_absorbance) <- rep(mediums, 5)

absorbance_median <- colMedians(new_absorbance)
# pearson correlation

cor(mediany, absorbance_median, method = "pearson")


###########################
cv_vs <- read.csv2("cv_and_videoscan.csv", header = TRUE, stringsAsFactors = FALSE)
cv_vs <- cv_vs[-c(1:105),]
nkol <- nrow(cv_vs)
cv_vs <- cv_vs[-c(6:nkol),]
rownames(cv_vs) <- c("5270", "5271", "5272", "5275", "5276")

CV_LB <- apply(cv_vs[, 3:14], 1, median) 
VS_LB <- apply(cv_vs[, 15:17], 1, median) 
VS_TSB <- apply(cv_vs[, 18:20], 1, median) 
VS_BHI <- apply(cv_vs[, 21:23], 1, median) 
VS_M63 <- apply(cv_vs[, 24:26], 1, median) 

## tylko próby
abs_med <- absorbance_median[c(1, 5, 9, 13, 17)]
abs_med<- matrix(abs_med, nrow = 1, ncol = 5)
cv_lb <- matrix(CV_LB, nrow = 1, ncol = 5)



for (i in seq(1:5)) {
  per_cor <- cor(CV_LB[i], abs_med[i], method = "pearson")
  print(per_cor)
}

# CV by Michal -----------------------------------------------

library(dplyr)
library(reshape2)

get_last_character <- function(x) {
  len_x <- nchar(x)
  substr(x, len_x, len_x)
}

raw_dat <- read.csv2("cv_and_videoscan.csv") %>% 
  mutate(strain = factor(strain)) %>% 
  melt(variable.name = "experiment") %>% 
  mutate(method = ifelse(grepl("CV", experiment), "CV", "Videoscan"),
         replicate = get_last_character(as.character(experiment)),
         medium = substr(experiment, 0, 3)) 


CV2018 <- absorbance %>% 
  t %>% 
  data.frame() %>% 
  slice((1L:5)*2 - 1) %>% 
  select(B, C, D) %>% 
  mutate(strain = c("5270", "5271", "5272", "5275", "5276"),
         method = "CV",
         date = "R2018") %>% 
  melt(variable.name = "replicate") %>% 
  mutate(replicate = as.character(as.numeric(replicate)))

CV_all <- bind_rows(select(raw_dat, strain, method, value, replicate) %>% 
                      filter(method == "CV", strain %in% c("5270", "5271", "5272", "5275", "5276")) %>% 
                      mutate(date = "R2016"),
                    CV2018)

library(ggplot2)

ggplot(CV_all, aes(x = date, y = value)) +
  geom_point() +
  facet_wrap(~ strain)

library(magrittr)

CV_aggr <- group_by(CV_all, strain, date, replicate) %>% 
  summarise(value = median(value)) %>% 
  summarise(value = median(value)) %>% 
  dcast(strain ~ date)

ggplot(CV_aggr, aes(x = R2016, y = R2018)) +
  geom_point()

# Michal - VS ---------------------------------------------

raw_vs <- nowa_ramka

colnames(raw_vs) <- paste0(colnames(raw_vs), "_", c("5270", "5271", "5272", "5275", "5276") %>% lapply(rep, 4) %>% unlist)

VS_all <- bind_rows(select(raw_dat, strain, method, value, replicate, medium) %>% 
            filter(method == "Videoscan", strain %in% c("5270", "5271", "5272", "5275", "5276")) %>% 
            mutate(date = "R2016"),
          melt(raw_vs) %>% 
            rename(replicate = Var1) %>% 
            mutate(medium = sapply(strsplit(as.character(Var2), split = "_"), first),
                   strain = sapply(strsplit(as.character(Var2), split = "_"), last),
                   replicate = as.character(replicate)) %>% 
            select(-Var2) %>% 
            mutate(medium = ifelse(medium == "LB10", "LB_", medium),
                   date = "R2018",
                   method = "Videoscan"))

ggplot(VS_all, aes(x = date, y = value)) +
  geom_point() +
  facet_grid(medium ~ strain)


VS_aggr <- group_by(VS_all, strain, medium, date, replicate) %>% 
  summarise(value = median(value)) %>% 
  summarise(value = median(value)) %>% 
  dcast(strain + medium ~ date)

ggplot(VS_aggr, aes(x = R2016, y = R2018)) +
  geom_point() +
  facet_wrap(~ medium)

tmp <- function(x, y) {
  browser()
}

group_by(VS_aggr, medium) %>% 
  summarise(rho = cor(R2016, R2018, method = "spearman"))
