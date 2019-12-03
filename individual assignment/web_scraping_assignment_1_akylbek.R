
library(rvest)
library(data.table)



# FUNCTION

my_dw_news <-  function(my_start) {
  
  # specify link, next page 10, 20, 30
  my_link <- paste0("https://www.dw.com/search/?languageCode=en&item=business%20analytics&searchNavigationId=9097&sort=DATE&resultsCounter=")
  t <- read_html(my_link)
  
  write_html(t, 't.html')
  
  # 1. titles and dates
  my_titles <- 
      t %>%
      html_nodes('h2')%>%
      html_text()
  
  # remove \n
  my_titles <- trimws(my_titles)
  
  # pull title
  my_article_title <- 
    unlist(
      lapply(my_titles, function(x){
        trimws(strsplit(x, '\n', fixed = T)[[1]][1])
      })
    )
  # pull date
  my_article_dates <- 
    unlist(
      lapply(my_titles, function(x){
        trimws(strsplit(x, '\n', fixed = T)[[1]][2])
      })
    )
  #convert date
  my_article_date <-  as.Date(my_article_dates, format = '%d.%m.%y')
  
  
  # 2. titles and dates
  
  my_body_text <- 
      t %>%
      html_nodes('#searchResult p')%>%
      html_text()
  
  my_links <- 
    paste0('https://www.dw.com/search/en/',
           t %>%
             html_nodes('.searchResult a')%>%
             html_attr('href')
    
    )
  return(data.frame('title'= my_article_title, 'summary'= my_body_text, 
                  'Date'= my_article_date, 'link'=my_links ))
}



all_news <- rbindlist(lapply(seq(10, 100, 10), my_dw_news))
View(all_news)

saveRDS(all_news, file="Deutsche Welle News on Business Analytics.Rda")
write.csv(all_news, "dw_ba_news.csv")


#-----------------------function2
# having to change the website name inside the function is inconvenient.
# try another function

my_dw_news2 <-  function(my_page) {
  
  # specify link, next page 10, 20, 30
  t <- read_html(my_page)
  
  write_html(t, 't.html')
  
  # 1. titles and dates
  my_titles <- 
    t %>%
    html_nodes('h2')%>%
    html_text()
  
  # remove \n
  my_titles <- trimws(my_titles)
  
  # pull title
  my_article_title <- 
    unlist(
      lapply(my_titles, function(x){
        trimws(strsplit(x, '\n', fixed = T)[[1]][1])
      })
    )
  # pull date
  my_article_dates <- 
    unlist(
      lapply(my_titles, function(x){
        trimws(strsplit(x, '\n', fixed = T)[[1]][2])
      })
    )
  #convert date
  my_article_date <-  as.Date(my_article_dates, format = '%d.%m.%y')
  
  
  # 2. titles and dates
  
  my_body_text <- 
    t %>%
    html_nodes('#searchResult p')%>%
    html_text()
  
  my_links <- 
    paste0('https://www.dw.com/search/en/',
           t %>%
             html_nodes('.searchResult a')%>%
             html_attr('href')
           )
  return(data.frame('title'= my_article_title, 'summary'= my_body_text, 
                    'Date'= my_article_date, 'link'=my_links ))
}

my_urls <- paste0("https://www.dw.com/search/?languageCode=en&item=bundesliga&searchNavigationId=9097&sort=DATE&resultsCounter=",
                  seq(from=10, to=200, by=10) )



all_news_bundesliga <- rbindlist(lapply(my_urls, my_dw_news2))
View(all_news_bundesliga)

saveRDS(all_news_bundesliga, file="Deutsche Welle News on Bundesliga.Rda")
write.csv(all_news, "dw_bundesliga_news.csv")

