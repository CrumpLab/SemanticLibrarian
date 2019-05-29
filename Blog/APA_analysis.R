load("~/bigFiles/APA_sims.RData")

mean_sim <- colMeans(sims)
article_df <- cbind(article_df,mean_sim)

save(article_df,file="article_df.RData")

names(mean_sim) <- article_df$

hist(mean_sim)
