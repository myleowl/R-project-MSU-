#### Libs ####

library(tidyverse)
library(readxl)

# 1 Download data

get_data <- function(url, dest, filename) {
  
  path <- paste0(dest, filename)
  
  download.file(url, destfile = path)
  
  xlsfile_1 <- read_xlsx(path, sheet = 1, skip = 3)
  xlsfile_2 <- read_xlsx(path, sheet = 2, skip = 2)
  
  return(list(xlsfile_1, xlsfile_2))
  
}

# 2 Clean data

clean_data <- function(data_list) {
  
  newdata <- data_list[[1]][-1, ] %>% 
    setNames(nm = c("Region", 
                    paste0(names(data_list[[1]])[2:(length(names(data_list[[1]])))])))
  
  x <- str_detect(newdata$Region, "федеральный округ")
  
  newdata <- cbind(rep(newdata$Region[x], 
                       diff(c(which(x), length(x) + 1))),
                   newdata) %>% 
    filter(!x) %>% 
    na.omit() %>% 
    merge(data_list[[2]], by.x = "Region", by.y = "...1") %>% 
    setNames(c(names(newdata)[1], "Location", 
               names(newdata)[2:length(names(newdata))], "2018"))
  
  newdata[newdata == "…"] <- NA
  
  newdata[, 3:ncol(newdata)] <- apply(newdata[, 3:ncol(newdata)], 2, as.numeric)
  
  return(newdata)
  
}



# 3 Plots

plot_data <- function(table_plot) {
  
  # 1
  plot1 <- table_plot %>% 
    select(-Region) %>% 
    group_by(Location) %>% 
    summarise_all(.funs = c(mean), na.rm = TRUE) %>% 
    ungroup() %>% 
    gather(Year, Value, -Location) %>% 
    mutate(Year = as.numeric(Year)) %>% 
    ggplot(aes(x = Year, y = Value, color = Location)) +
    geom_line() 
  
  # 2
  plot2 <- table_plot %>% 
    group_by(Location) %>% 
    gather(Year, Value, -Location, -Region) %>% 
    mutate(Year = as.numeric(Year)) %>% 
    ggplot(aes(x = factor(Year), y = Value)) +
    geom_boxplot(outlier.alpha = 0) +
    geom_jitter(width = 0.2, alpha = 0.3) 
  
  # 3
  plot3 <- cbind(table_plot[, 1:2], 
        apply(table_plot[, 3:ncol(table_plot)], 1, function(x) {x - 100}) %>%
          t()) %>% 
    gather(Year, Value, -Location, -Region) %>% 
    mutate(Year = as.numeric(Year)) %>% 
    na.omit() %>% 
    ggplot(aes(x = factor(Year), y = Region, fill = Value)) +
    geom_tile() +
    scale_fill_gradient2(low = "red", high = "green", mid = "white", 
                         midpoint = 0, 
                         limit = c(min(table_plot[, -(1:2)], na.rm = TRUE) - 100, 
                                   max(table_plot[, -(1:2)], na.rm = TRUE) - 100), 
                         space = "Lab",
                         name = "Inc/Dec in Payment") 
  
  print(plot1)
  print(plot2)
  print(plot3)
  
}

# Test

# data: 
# Реальная среднемесячная начисленная заработная плата работников по субъектам Российской Федерации за 2000-2018гг. 
# в % к предыдущем году

url <- "https://www.gks.ru/storage/mediabank/t5.xlsx"
dest <- "~/Desktop/proj/"
filename <- "task_data.xls"

data_gks <- get_data(url, dest, filename)
table_plot <- clean_data(data_gks)
plot_data(table_plot)

