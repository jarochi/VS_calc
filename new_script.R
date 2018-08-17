library(dplyr)
library(reshape2)
library(ggplot2)
library(magrittr)

get_last_character <- function(x) {
  len_x <- nchar(x)
  substr(x, len_x, len_x)
}

raw_dat <- read.csv2("data/cv_and_videoscan.csv") %>% 
  mutate(strain = factor(strain)) %>% 
  melt(variable.name = "experiment") %>% 
  mutate(method = ifelse(grepl("CV", experiment), "CV", "Videoscan"),
         replicate = get_last_character(as.character(experiment)),
         medium = substr(experiment, 0, 3)) 


strains <- c("5270", "5271", "5272", "5275", "5276")

mediums <- c("LB10", "TSB", 
             "BHI", "M63")

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


ggplot(CV_all, aes(x = date, y = value)) +
  geom_point() +
  facet_wrap(~ strain)


CV_aggr <- group_by(CV_all, strain, date, replicate) %>% 
  summarise(value = median(value)) %>% 
  summarise(value = median(value)) %>% 
  dcast(strain ~ date)

ggplot(CV_aggr, aes(x = R2016, y = R2018)) +
  geom_point()

# Michal - VS ---------------------------------------------


raw_vs_frame <- read.csv2("data/ProjektDevC003707-2018-07-26-Koza-Biofilm.csv", skip = 6, header = FALSE, stringsAsFactors = FALSE)
# raw_vs_frame <- read.csv2("data/ProjektDevC003709-2018-07-27-Ania-Biofilm.csv", skip = 6, header = FALSE, stringsAsFactors = FALSE)

vs_frame <- rbind(raw_vs_frame$V14)
colnames(vs_frame) <- rbind(raw_vs_frame$V3)
vs_frame <- matrix(vs_frame)
vs_frame <- matrix(data = vs_frame, nrow = 8, ncol = 12)
rownames(vs_frame) <- c("A", "B", "C", "D", "E", "F", "G", "H")
colnames(vs_frame) <- seq(1,12)

vs_frame <- vs_frame[-c(1,8), -c(1,12)]
vs_frame <- matrix(vs_frame,nrow = 3, ncol = 20)
colnames(vs_frame) <- rep(mediums, 5)

raw_vs <- vs_frame

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

group_by(VS_aggr, medium) %>% 
  summarise(rho = cor(R2016, R2018, method = "spearman"))









##############################
# create data.frame with 2 col (CV, VS)
aa <- list.files("./test")
bb <- grep(pattern = ".ods", x = aa)
cc <- grep(pattern = ".csv", x = aa)
# xx <- cbind(aa[bb], aa[cc])
xx1 <- cbind(aa[bb])
xx1 <- cbind(xx1, as.numeric(order(xx1[, 1])))
colnames(xx1) <- c("CV_filename", "order_nr")

xx2 <- cbind(aa[cc])

ll <- str_split(xx2[, 1], "-")
df <- data.frame(matrix(unlist(ll), nrow = 6))
df <- t(df)

xx2 <- cbind(xx2, df[,5])
xx2 <- cbind(xx2, as.numeric(order(xx2[, 2])))
colnames(xx2) <- c("CV_filename", "short_name", "order_nr")


inner_join(xx1, xx2, by = c("order_nr" == "order_nr"))

# sort by grep cv filename and find vs









