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

