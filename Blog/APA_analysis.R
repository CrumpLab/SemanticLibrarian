load("~/bigFiles/APA_sims.RData")

# mean similarity to all other articles
mean_sim <- colMeans(sims)
article_df <- cbind(article_df,mean_sim)

save(article_df,file="article_df.RData")

names(mean_sim) <- article_df$

hist(mean_sim)

# standard deviation similarity
sd_sim <- apply(sims,2,FUN = function(x){sd(x)})
article_df <-cbind(article_df,sd_sim)

##### Journals

pr_id <-which(article_df$journal == "Psychological Review")

psyc_review_sims <- colMeans(sims[pr_id,pr_id])
psyc_review_sds <- apply(sims[pr_id,pr_id],2,FUN = function(x){sd(x)})
psyc_review_df <- article_df %>%
                    filter(journal == "Psychological Review") %>%
                    mutate(pr_sims = psyc_review_sims,
                           pr_sds = psyc_review_sds,
                           pr_mean_sds = pr_sims/pr_sds)

save(psyc_review_df,file="psyc_review_df.RData")

## authors

dim(AuthorVectors)
library(RsemanticLibrarian)

author_sims <- matrix(0,ncol=dim(AuthorVectors)[1],nrow=dim(AuthorVectors)[1])
for(i in 1:dim(AuthorVectors)[1]){
  if(i%%100 == 0){ 
    print(i)
  }
 author_sims[i,]<-cosine_x_to_m(AuthorVectors[i,],AuthorVectors)
}

author_sims[is.na(author_sims)]<-0
save(author_sims,file="author_sims")

author_mean_sims <- colMeans(author_sims)

save(author_mean_sims,author_list,file="author_mean_sims.RData")


# fast svd

library(irlba)
ids <- which(mean_sim > .85)
a<- irlba(1-sims[ids,ids],nv=2, center=mean_sim[ids])
a<-svdr(1-sims[ids,],k=2)


a<- irlba(article_vectors[ids,],nv=2, center = colMeans(article_vectors[ids,]))
b<-data.frame(X=a$u[,1],Y=a$u[,2])
clusters <- kmeans(a$u , centers=10)

restrict_articles <- article_df[ids,]
restrict_articles <- cbind(restrict_articles,b,
                           cluster =as.factor(clusters$cluster))
restrict_articles$year <- as.factor(restrict_articles$year)
levels(restrict_articles$journal)<-c("CJEP","JEP:ABP","JEP:ALC","JEP:A","JEP:G","JEP:HLM","JEP:HPP","JEP:LMC","PsycRev")


library(ggplot2)
p<-ggplot(restrict_articles, aes(x=X,y=Y,
                              color=cluster,
                              label=journal,
                              group=year))+
  geom_text()+
  coord_cartesian(ylim=c(-.015,.015),
                  xlim=c(-.015,.015))+
  theme(legend.position ="none")+
  ggtitle("APA abstract space {closest_state}")+
  theme_classic()

library(gganimate)
anim <- p +
    transition_states(year, transition_length = 1,
                      state_length=4) +
                      enter_fade()+
                      exit_shrink()
  
animate(anim, nframes = 123*4)

save(restrict_articles, file="year_sim.RData")


# testing

restrict_article_df <- article_df %>%
  {
  filter(str_detect(as.character(author_list),""),
         str_detect(as.character(journals),"Psychological Review"),
         year >= 1990,
         year <= 2016)}%>%
  rename(Abstracts = formatted_column) %>%
  slice(1:5)



