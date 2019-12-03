library(rvest)
library(data.table)


t <-  read_html("https://hoopshype.com/salaries/players/")

write_html(t, "t.html")

my_list <-  
  t %>% 
  html_table()
  
t %>% 
  html_table()


class(my_list)


DF <- my_list[[1]]
DF


class(DF)


DF


View(DF)
names(DF)
DF[1,]
DF[1,1]

DF[1,1] <- 'Index'
DF[1,]

names(DF) <- DF[1,]
DF[-1,]




DF[4:6, 1:2]
