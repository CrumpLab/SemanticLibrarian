## helper functions
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

wrap_string <- function(x) {paste(strwrap(x,50), collapse=" <br> ")}
normalize_vector <- function (x) {return(x/abs(max(x)))}

## vectorize search terms

get_search_terms <- function(words,dictionary){
  breakdown_words <- breakdown(words)
  search_items <- unlist(strsplit(breakdown_words,split=" "))
  search_items <- search_items[search_items %in% dictionary==TRUE]
  return(search_items)
}

## get similarities between search terms and articles

get_search_article_similarities <- function(search_terms,
                                            d_words = dictionary_words,
                                            w_vectors = WordVectors,
                                            a_vectors = article_vectors,
                                            a_df = article_df,
                                            query_type){
  
  search_index <- which(d_words %in% search_terms)
  if(length(search_index > 0)){
    # compound vectors
    if (query_type == 1){
      
      if(length(search_index) > 1) {
        query_vector <- colSums(w_vectors[search_index,])
      }else{
        query_vector <- w_vectors[search_index,]
      }
      
      get_cos <-  cosine_x_to_m(query_vector,
                                a_vectors)
      
      article_sims_SS <- a_df %>%
                          mutate(Similarity = as.numeric(get_cos))%>%
                          select(formatted_column,
                                 title,
                                 wrap_title,
                                 year,
                                 index,
                                 Similarity) %>%
                          arrange(desc(Similarity))
    } else {
      if(length(search_index) > 1) {
          query_matrix     <- w_vectors[search_index,]
          get_cos_matrix   <- apply(query_matrix,1,
                                    function(x) cosine_x_to_m(x,a_vectors))
          if (query_type == 2) {
            multiply_columns <- apply(get_cos_matrix,1,prod)
          } else if (query_type == 3){
            get_cos_matrix <- apply(get_cos_matrix,2,normalize_vector)
            multiply_columns <- apply(get_cos_matrix,1,max)
          }
          
          article_sims_SS <- a_df %>%
            mutate(Similarity = round(multiply_columns,digits=4))%>%
            select(formatted_column,
                   title,
                   wrap_title,
                   year,
                   index,
                   Similarity) %>%
            arrange(desc(Similarity))
      }
    }
  }
}

# get multidimensional scaling coordinates

get_mds_article_fits <- function(num_articles,
                                 num_clusters,
                                 year_range,
                                 input_df,
                                 a_vectors=article_vectors){
      if(!is.null(input_df)){
      article_ids <- input_df %>%
                      filter(year >= year_range[1],
                             year <= year_range[2]) %>%
                      slice(1:num_articles) %>%
                      select(index)
       # input_df[1:num_articles,]$index
      temp_article_matrix <- a_vectors[article_ids$index,]
      mdistance <- cosine(t(temp_article_matrix))
      fit <- cmdscale(1-mdistance,eig=TRUE, k=2)
      colnames(fit$points) <- c("X","Y")
      cluster <- kmeans(fit$points,num_clusters)
      input_df <- input_df %>%
                    filter(year >= year_range[1],
                           year <= year_range[2]) %>%
                    slice(1:num_articles)
      input_df <- cbind(input_df,fit$points,
                        cluster=cluster$cluster)
      return(input_df)
      }
}
                
# article to all article similarities

get_article_article_similarities <- function(a_title,
                                             a_vectors = article_vectors,
                                             a_df = article_df){
  
  a_id <- a_df[a_df$title == a_title,]$index

  get_cos <-  cosine_x_to_m(a_vectors[a_id,],a_vectors)
  article_sims <- a_df %>%
    mutate(Similarity = round(as.numeric(get_cos),digits=4)) %>%
    select(formatted_column,
           title,
           wrap_title,
           year,
           index,
           Similarity) %>%
    arrange(desc(Similarity))
  return(article_sims)
}

get_author_similarities <- function(a_name,
                                    auth_vectors = AuthorVectors,
                                    authors=author_list){
  auth_id <- which(authors %in% a_name)
  get_cos <-  cosine_x_to_m(auth_vectors[auth_id,],auth_vectors)
  selected_author <- rep("other",length(authors))
  selected_author[auth_id] <- "selected"
  selected_author <- as.factor(selected_author)
  author_sims <- data.frame(author=authors,
                            Similarity = round(as.numeric(get_cos),digits=4),
                            selected_author = selected_author,
                            index = 1:length(authors)) %>%
                            arrange(desc(Similarity))
  return(author_sims)
}

get_mds_author_fits <- function (num_authors,
                                 num_clusters,
                                 input_df,
                                 auth_vectors=AuthorVectors){
  auth_ids <- input_df[1:num_authors,]$index
  temp_author_matrix <- auth_vectors[auth_ids,]
  mdistance <- cosine(t(temp_author_matrix))
  fit <- cmdscale(1-mdistance,eig=TRUE, k=2)
  colnames(fit$points) <- c("X","Y")
  cluster <- kmeans(fit$points,num_clusters)
  input_df <- slice(input_df,1:num_authors)
  input_df <- cbind(input_df,fit$points,
                    cluster=cluster$cluster)
  return(input_df)
}

get_article_author_similarities <- function(a_title,
                                            a_vectors = article_vectors,
                                            a_df = article_df,
                                            auth_vectors = AuthorVectors,
                                            authors=author_list){
  a_id <- a_df[a_df$title == a_title,]$index
  get_cos <-  cosine_x_to_m(a_vectors[a_id,],auth_vectors)
  author_sims <- data.frame(author=authors,
                            Similarity = round(as.numeric(get_cos),digits=4),
                            index = 1:length(authors)) %>%
                            arrange(desc(Similarity))
  return(author_sims)
}

## get article similarities from selection of articles

get_articles_mds_from_selection <- function(a_df,
                                            num_clusters,
                                            a_vectors = article_vectors){
  a_ids <- a_df$index
  mdistance <- cosine(t(article_vectors[a_ids,]))
  fit <- cmdscale(1-mdistance,eig=TRUE, k=2)
  colnames(fit$points) <- c("X","Y")
  cluster <- kmeans(fit$points,num_clusters)
  a_df <- cbind(a_df, fit$points, 
                    cluster=cluster$cluster)
  return(a_df)
  
}

    
#     #article_sims_SS<-article_sims_SS()
#     article_sims_SS$Similarity <- round(get_cos,digits=4)
#     article_sims_SS <- cbind(article_sims_SS,
#                              year=article_df$year,
#                              title=article_df$title,
#                              index=seq(1,dim(article_vectors)[1]))
#     
#     article_sims_SS <- article_sims_SS[article_sims_SS$year >= input$slider_num_year_SS[1] &
#                                          article_sims_SS$year <= input$slider_num_year_SS[2],  ]
#     
#     article_sims_SS <- article_sims_SS[order(article_sims_SS$Similarity,decreasing=T),]
#     
#     article_sims_SS <- article_sims_SS[1:input$slider_num_articles,]
#     
#     top_terms <- as.character(article_sims_SS$title)
#     
#     article_index <- article_sims_SS$index
#     
#     temp_article_matrix <- article_vectors[article_index,]
#     
#     row.names(temp_article_matrix) <- top_terms
#     
#   } else {
#     
#     query_matrix     <- WordVectors[search_index,]
#     get_cos_matrix   <- apply(query_matrix[,new_article_beagle_inds$a],1,function(x) cosine_x_to_m(x,
#                                                                                                    article_vectors[,new_article_beagle_inds$a]))
#     if (input$select_query_type == 2) {
#       multiply_columns <- apply(get_cos_matrix,1,prod)
#     } else if (input$select_query_type == 3){
#       get_cos_matrix <- apply(get_cos_matrix,2,normalize_vector)
#       multiply_columns <- apply(get_cos_matrix,1,max)
#     }
#     article_sims_SS<-article_sims_SS()
#     article_sims_SS$Similarity <- round(multiply_columns,digits=4)
#     article_sims_SS <- cbind(article_sims_SS,
#                              year=article_df$year,
#                              title=article_df$title,
#                              index=seq(1,dim(article_vectors)[1]))
#     article_sims_SS <- article_sims_SS[article_sims_SS$year >= input$slider_num_year_SS[1] &
#                                          article_sims_SS$year <= input$slider_num_year_SS[2],  ]
#     article_sims_SS <- article_sims_SS[order(article_sims_SS$Similarity,decreasing=T),]
#     article_sims_SS <- article_sims_SS[1:input$slider_num_articles,]
#     top_terms <- as.character(article_sims_SS$title)
#     article_index <- article_sims_SS$index
#     temp_article_matrix <- article_vectors[article_index,]
#     row.names(temp_article_matrix) <- top_terms
#   }
#   
# }

#   search_terms <- get_search_terms_SS()                                        
#   #if(!is.null(input$server_dictionary))
#   if(!is.null(search_terms))
#     
#     #search_index <-   which(dictionary_words %in% input$server_dictionary)
