library(dplyr)
library(readODS)
library(reshape2)

ith_file <- list.files("./CrystalViolet")
ith_file <- ith_file[1]

read_cv <- function(cv_dir) {
  all_files <- list.files(cv_dir, full.names = TRUE)
  
  lapply(all_files, function(ith_file) {
    cv_data <- read_ods(ith_file, col_names = FALSE) %>%
    # cv_data <- read_ods("./CrystalViolet/2018-08-16_AL_S1_R1.ods", col_names = FALSE) %>%
      as.matrix
    
    colnames(cv_data) <- LETTERS[1L:12]
    
    mcv_data <- melt(cv_data, varnames = c("row", "col")) %>% 
      mutate(row = factor(row), 
             col = factor(col))
    
    experiment_metadata <- strsplit(ith_file, "/") %>% 
      unlist %>% 
      last %>% 
      strsplit(".", fixed = TRUE) %>%
      unlist %>% 
      first %>% 
      strsplit("_") %>% 
      unlist
##### check if works correctly on more templates    
    scheme_name <- paste0("./CrystalViolet-scheme/", experiment_metadata[3], ".csv")
    
    plate_scheme <- read.csv(scheme_name, header = FALSE) %>% 
      as.matrix()
    colnames(plate_scheme) <- LETTERS[1L:12]
    mplate_scheme <- melt(plate_scheme, varnames = c("row", "col"), value.name = "description") %>% 
      mutate(row = factor(row), 
             col = factor(col))
    
    
    inner_join(mplate_scheme, mcv_data, by = c("row" = "row", "col" = "col")) %>% 
      filter(!is.na(description)) %>% 
      mutate(description = as.character(description), 
             strain = sapply(strsplit(description, split = "-"), first),
             medium = sapply(strsplit(description, split = "-"), last),
             experimentator = experiment_metadata[2],
             scheme = experiment_metadata[3],
             replicate = experiment_metadata[4],
             exp_date = experiment_metadata[1])  %>%
      select(strain, medium, experimentator, replicate, value, exp_date)
  }) %>%
    bind_rows()
}

res <- read_cv("./CrystalViolet")

library(ggplot2)
library(ggbeeswarm)

ggplot(res, aes(x = strain, y = value, color = experimentator)) +
  geom_boxplot() +
  facet_wrap(~ medium) +
  theme_bw()


ggplot(res, aes(x = strain, y = value, color = experimentator)) +
  geom_quasirandom() +
  facet_wrap(~ medium) +
  theme_bw()

group_by(res, strain, medium, experimentator, replicate) %>% 
  summarise(value = median(value)) %>% 
  ggplot(aes(x = strain, y = value, color = experimentator)) +
  geom_point(size = 3) +
  facet_wrap(~ medium) +
  theme_bw()

