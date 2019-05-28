setwd("~/Public")

load("AuthorVectors.rda")

vec_norm <- function(x){
  #return(x/sqrt(sum(x^2)))
  x/sqrt(as.numeric(crossprod(x)))
}


library(irlba)
AuthorVectors[is.na(AuthorVectors)]<-0
AuthorVectors<-AuthorVectors[-15643,]

for(i in 1:dim(AuthorVectors)[1]){
  if(i%%1000==0){
    print(i)
  }
  AuthorVectors[i,]<-vec_norm(AuthorVectors[i,])
}


svd_test <-irlba(AuthorVectors, nv=5)

df_eig<-data.frame(PC1=svd_test$u[,1],
                   PC2=svd_test$u[,2],
                   PC3=svd_test$u[,3],
                   PC4=svd_test$u[,4],
                   PC5=svd_test$u[,5],
                   sentence=row.names(AuthorVectors))
library(plotly)

plot_ly(df_eig[sample(nrow(df_eig),5000),], x = ~PC4, y= ~PC5, type = "scatter", mode="markers", text=~sentence, color=~PC1)

plot_ly(df_eig, x = ~PC1, y= ~PC2, z=~PC3, text=~sentence, color=~PC4) %>%
  add_markers(marker = list(size = 1))  


library(Rcpp)
library(knitr)

cppFunction('NumericVector rowSumsSq(NumericMatrix x) {
            int nrow = x.nrow(), ncol = x.ncol();
            NumericVector out(nrow);
            
            for (int j = 0; j < ncol; ++j) {
            for (int i = 0; i < nrow; ++i) {
            out[i] += std::pow(x(i, j), 2);
            }
            }
            
            return out;
            }')

cosine_x_to_m  <- function(x, m) {
  x <- x / as.numeric(sqrt(crossprod(x)))
  sims<-(m %*% x / sqrt(rowSumsSq(m)))
  return(sims)
}


library(lsa)


sign(AuthorVectors[1:10,1:100])

svd_test <-irlba(log(AuthorVectors+1500), nv=20,center=colMeans(log(AuthorVectors+1500)))

row.names(svd_test$u)<-row.names(AuthorVectors)
inds <- sample(1:20,10)
get_cos<-cosine(svd_test$u["Crump, Matthew J. C.",inds],t(svd_test$u[,inds]))


hist(get_cos)

get_cos[get_cos>.75]

# article similarity

load("data/article_vectors.rda")

article_csv <- read.csv(file="all_journals_rev.csv", header=TRUE, sep=",")
article_vectors<-as.matrix(article_vectors)
article_vectors[is.na(article_vectors)]<-0

for(i in 1:dim(article_vectors)[1]){
  if(i%%1000==0){
    print(i)
  }
  article_vectors[i,]<-vec_norm(article_vectors[i,])
}

svd_test <- irlba(article_vectors, nv=100,center=colMeans(article_vectors))

library(ggplot2)

plot_df<-data.frame(PC1=svd_test$u[,1],
                    year=article_csv$year,
                    title=article_csv$title,
                    PC2=svd_test$u[,2],
                    PC3=svd_test$u[,3],
                    PC4=svd_test$u[,4])

ggplot(plot_df,aes(x=year, y=PC1))+
  geom_point()

library(plotly)

plot_ly(plot_df[plot_df$year>2000,], x = ~year, y= ~PC1, type = "scatter", mode="markers", text=~title, color=~PC2)

plot_ly(plot_df[sample(nrow(plot_df),500),], x = ~PC1, y= ~PC2, z=~PC3, text=~title, color=~PC4) %>%
  add_markers(marker = list(size = 1))

library(lsa)
library(knitr)
get_cos<-cosine(article_vectors[13421,],t(article_vectors))
hist(get_cos)

new_df<-data.frame(sims=get_cos,titles=article_csv$title)
kable(new_df[new_df$sims>.965,])

get_cos[get_cos>.95]


get_cos<-cosine(AuthorVectors["Crump, Matthew J. C.",],t(article_vectors))
hist(get_cos)

new_df<-data.frame(sims=get_cos,titles=article_csv$title)
new_df<-new_df[order(new_df$sims,decreasing=T),]
kable(new_df[new_df$sims>.96,])

get_cos[get_cos>.95]

get_cos<-cosine(colSums(article_vectors[c(13692,17448),]),t(article_vectors))


#with svd
save_all<-data.frame()
for(i in 1:100){
inds<-sample(1:100,80)
get_cos<-cosine(svd_test$u[2443,inds],t(svd_test$u[,inds]))
#hist(get_cos)

new_df<-data.frame(sims=get_cos,titles=article_csv$title)
new_df<-new_df[order(new_df$sims,decreasing=T),]
new_df<-new_df[1:20,]
new_df$sims<-1:20
new_df<-cbind(new_df,monte=rep(i,20))
save_all<-rbind(save_all,new_df)
}

save_all$titles<-factor(save_all$titles)
sum_df<-sort(table(save_all$titles),decreasing=T)
sum_df<-data.frame(titles=names(sum_df),counts=as.numeric(sum_df))


kable(new_df[1:20,])


# author vector monte carlo with noise

load("AuthorVectors.rda")
AuthorVectors[is.na(AuthorVectors)]<-0
AuthorVectors<-AuthorVectors[-15643,]

for(i in 1:dim(AuthorVectors)[1]){
  if(i%%1000==0){
    print(i)
  }
  AuthorVectors[i,]<-vec_norm(AuthorVectors[i,])
}

save_all<-data.frame()
for(i in 1:100){
  author<-AuthorVectors["Crump, Matthew J. C.",]
  noise<-sample(c(rep(1,(length(author)-200)),rep(0,200)))
  author<-author*noise

  get_cos<-cosine(author,t(AuthorVectors))

  new_df<-data.frame(sims=get_cos,titles=row.names(AuthorVectors))
  new_df<-new_df[order(new_df$sims,decreasing=T),]
  new_df<-new_df[1:200,]
  new_df$sims<-1:200
  new_df<-cbind(new_df,monte=rep(i,200))
  save_all<-rbind(save_all,new_df)
}

save_all$titles<-factor(save_all$titles)
sum_df<-sort(table(save_all$titles),decreasing=T)
sum_df<-data.frame(titles=names(sum_df),counts=as.numeric(sum_df))


# get abstract word lengths

abstracts<-article_csv$abstract
length(unlist(strsplit(as.character(abstracts[1]),split=" ")))

numwords<-c()
for(i in 1:length(abstracts)){
  numwords<-c(numwords,length(unlist(strsplit(as.character(abstracts[i]),split=" "))))
}

article_df<-cbind(article_df,numwords)
library(ggplot2)
ggplot(aggregate(numwords~year,article_df,mean),aes(x=year,y=numwords))+geom_point()

# issue number of words in abstract varies across years
# influences average similarity across years

#26572 george miller paper

get_cos<-cosine(article_vectors[26572,],t(article_vectors))
plot_df<-data.frame(sims=get_cos,year=article_csv$year)
ggplot(aggregate(sims~year,plot_df,mean),aes(x=year,y=sims))+geom_point()

weighted_avs<-article_vectors/article_df$numwords

get_cos<-cosine(weighted_avs[26572,],t(weighted_avs))
plot_df<-data.frame(sims=get_cos,year=article_df$year)
ggplot(aggregate(sims~year,plot_df,mean),aes(x=year,y=sims))+geom_point()

save_cor<-c()
for(s in 1:50){
sample_inds<-sample(1:27560,200)
get_cos<-cosine(t(article_vectors[sample_inds,]))
mean_similarity<-rowMeans(get_cos)
#plot(mean_similarity,numwords[sample_inds])
save_cor<-c(save_cor,cor(mean_similarity,numwords[sample_inds]))
}
mean(save_cor)


load("data/article_vectors.rda")

article_vectors<-as.matrix(article_vectors)
article_vectors[is.na(article_vectors)]<-0
article_vectors<-t(apply(article_vectors,1,zscore))
article_vectors<-article_vectors-rowMeans(article_vectors)

article_vectors<-log(article_vectors+26)

sample_inds<-sample(1:27560,500)
get_cos<-cosine(t(article_vectors[sample_inds,]))
mean_similarity<-rowMeans(get_cos)
cor(mean_similarity,numwords[sample_inds])
plot(mean_similarity)

cor(numwords,rowMeans(article_vectors))


### tools 

# add an html column to the article_df data.frame for formatting

formatted_column<-sapply(1:197,function(x) paste(
  c("<p>",as.character(article_df[x,]$title),"</p>",
    "<p style='font-size:11px' onclick=\"shinyjs.goauthor(this.textContent);\">", as.character(article_df[x,]$authorlist), "</p>",
    "<p style='font-size:11px'>", as.character(article_df[x,]$session), ", ", 
          article_df[x,]$sessiontopic,", ",
          article_df[x,]$affiliation, "</p>"), collapse=""
))

article_df<-cbind(article_df,formatted_column)
article_df$formatted_column<-formatted_column
save(article_df,file="article_df.rda")
article_df[1:2,]


# add an html column to the article_df data.frame for formatting

formatted_column<-sapply(1:27560,function(x) paste(
  c("<p onclick=\"shinyjs.goarticle(this.textContent);\">",as.character(article_df[x,]$title),"</p>",
    "<p style='font-size:11px' onclick=\"shinyjs.goauthor(this.textContent);\">", as.character(article_df[x,]$authorlist), "</p>",
    "<p style='font-size:11px'>", as.character(article_df[x,]$journal), ", ", 
    article_df[x,]$issue,", ",
    article_df[x,]$pages, "</p>"), collapse=""
))

article_df<-cbind(article_df,formatted_column)
article_df$formatted_column<-formatted_column
save(article_df,file="article_df.rda")
article_df[1:2,]

author_list<-article_df$authorlist

formatted_author_list<-list()

for(i in 1:length(author_list)){
  temp_authors<-strsplit(as.character(author_list[i]),split=";")
  formatted_author_list[i]<-temp_authors
}

new_author_list<-list()
for(i in 1:length(author_list)){
  new_author_list[[i]]<-trimws(formatted_author_list[[i]],"left")
}

for(i in 1:length(author_list)){
  temp_unlist<-unlist(new_author_list[i])
  for (j in 1:length(temp_unlist)){
    temp_unlist[j]<-paste("<p style='font-size:11px' onclick=\"shinyjs.goauthor(this.textContent);\">", temp_unlist[j], "</p>",sep="",collapse="")
  }
  new_author_list[[i]]<-temp_unlist
}

pasted_authors<-c()
for(i in 1:length(author_list)){
  pasted_authors<-c(pasted_authors,paste(unlist(new_author_list[[i]]),collapse=""))
}

# add an html column to the article_df data.frame for formatting

formatted_column<-sapply(1:197,function(x) paste(
  c("<p onclick=\"shinyjs.goarticle(this.textContent);\">",as.character(article_df[x,]$title),"</p>",
    pasted_authors[x],
    "<p style='font-size:11px'>", as.character(article_df[x,]$journal), ", ", 
    article_df[x,]$issue,", ",
    article_df[x,]$pages, "</p>"), collapse=""
))

article_df<-cbind(article_df,formatted_column)
article_df$formatted_column<-formatted_column
save(article_df,file="article_df.rda")
