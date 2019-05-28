library(data.table)

article_vectors <- as.matrix(article_vectors)
article_vectors[is.na(article_vectors)] <- 0
AuthorVectors <- as.matrix(AuthorVectors)
author_list <- row.names(AuthorVectors)

formatted_column <- sapply(1:27560,function(x) paste(
  c("<h4>",as.character(article_df[x,]$title),"</h4>",
    "<p style='font-size:12px'>", as.character(article_df[x,]$authorlist), "</p>",
    "<p style='font-size:11px'>", as.character(article_df[x,]$journal), ", ",
    as.character(article_df[x,]$issue), ",", as.character(article_df[x,]$pages),"</p>",
    "<p style='font-size:11px'>", as.character(article_df[x,]$abstract), "</p>"), collapse=""
))

article_df$formatted_column <- formatted_column

dictionary_words <- as.character(row.names(WordVectors))

wrap_title<-c()
for(i in 1:dim(article_df)[1]){
  wrap_title[i] <-wrap_string(article_df$title[i])
}

article_df <- cbind(article_df,wrap_title)

article_df <- cbind(article_df, index = 1:dim(article_df)[1])
article_df$wrap_title <- as.character(article_df$wrap_title)

save(list = ls(all.names = TRUE),file="allData/allData.RData")

# test author select
library(stringr)
test <-article_df %>%
  filter(str_detect(as.character(authorlist),"Bruce Milliken"))


  filter("Bruce Milliken" %in% unlist(strsplit(authorlist,split="; ")))
  


         