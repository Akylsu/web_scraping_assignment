library(rvest)
library(data.table)
library(dplyr)
library(readr)
library(ggplot2)
library(scales)
library(gridExtra)
library(tidyr)

#### ---- 1 --- scraping with a function -----------------------------
scrape_nba <- function(my_year) {
  my_url <- read_html(paste0("https://hoopshype.com/salaries/players/", my_year, "-", my_year+1, "/"))
  data <- my_url %>% html_nodes("table") %>% .[[1]] %>% html_table() 
  data1 <- cbind(data, season=paste0(my_year, "-", my_year+1)) %>% .[-1,-4] 
  }

output_nba <- lapply(1990:2018,scrape_nba)

df_nba <- rbindlist(output_nba)

View(df_nba)

#### ---- 2 --- cleanig df -----------------------------
df_nba <- df_nba %>% rename(index=X1, name=X2, salary=X3)

df_nba[,3] <- sapply(df_nba[,3], parse_number)
View(df_nba)

(f1 <- df_nba %>% filter(index=="1") %>% 
  ggplot(aes(x=as.factor(season),y=salary, group=1)) +
  geom_line()+
  geom_point()+
  scale_y_continuous( breaks= c(seq(0,45000000, by = 5000000)),labels = comma)+
  theme(legend.position="none", axis.text.x = element_text(size = 10, angle = 90)) +
  geom_text(aes(label = name), position = position_dodge2(), color = "blue", angle = 0.) +
  labs(title = "Figure 1. Salary trends", y = "Season", y = "Salary, in USD"))
  

(f2 <- df_nba %>% filter(index=="1") %>% .[1:15,]%>% 
  ggplot(aes(x=as.factor(season),y=salary, group=1)) +
  geom_line()+
  geom_point()+
  scale_y_continuous( breaks= c(seq(0,45000000, by = 5000000)),labels = comma)+
  theme(legend.position="none", axis.text.x = element_text(size = 10, angle = 90)) +
  geom_text(aes(label = name), vjust=-1, color = "blue") +
  geom_text(aes(label = scales::dollar(salary)), vjust=1, 
            color = "red")+
  labs(title = "Figure 2. Top Earners in seasons 1990/1991 - 2004-2005", x = "Season", y = "Salary, in USD"))

(f3 <- df_nba %>% filter(index=="1") %>% .[16:30,]%>% 
  ggplot(aes(x=as.factor(season),y=salary, group=1)) +
  geom_line()+
  geom_point()+
  scale_y_continuous( breaks= c(seq(0,45000000, by = 5000000)),labels = comma)+
  theme(legend.position="none", axis.text.x = element_text(size = 10, angle = 90)) +
  geom_text(aes(label = name), vjust=-1, color = "blue") +
  geom_text(aes(label = scales::dollar(salary)), vjust=1, 
            color = "red")+
  labs(title = "Figure 3. Top Earners in seasons 2006/2007 - 2018-2019", x = "Season", y = "Salary, in USD"))


ggsave("Figure 1. Salary trends.png", plot = f1)
ggsave("Figure 2. Top Earners 1990-2005.png", plot = f2)
ggsave("Figure 3. Top Earners 2006-2019.png", plot = f3)


saveRDS(df_nba, file="NBA_salaries.Rda")
write_csv(df_nba, "NBA_salaries.csv")
