#################
#  The Semantic Librarian
#  Semantic Search based on  Semantic Vectors
#  Shiny Code: Matthew Crump, 2018 <- Blame Matt for things that don't work right now
#  Semantic Vectors: Harinder Aujla
#  Model Development: Randall Jamieson
#################



##################
# Load libraries
##################

library(lsa)
library(LSAfun)
library(ggplot2)
library(shiny)
library(shinyjs)
library(V8)
library(DT)
library(plotly)
library(Rcpp)
library(ggrepel)
library(rvest)

##################
# LOAD FUNCTIONS
##################

# Load fast cosine function, requires rcpp

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

wrap_string <- function(x) {paste(strwrap(x,50), collapse="<br>")}
normalize_vector <- function (x) {return(x/abs(max(x)))}

####################
# Load Databases (main call in global.r)
#####################

dictionary_words  <- row.names(WordVectors)[,1]
author_names_vec  <- row.names(AuthorVectors)
article_titles    <- unique(article_df$title)
article_vectors   <- as.matrix(article_vectors)
article_vectors[is.na(article_vectors)] <- 0

####################
# Start Shiny Server
####################

server <- function(input, output, session) {
  
  hide("slider_BEAGLE_article")
  hide("slider_BEAGLE_SS")
  hide("redo_beagle_vecs_SS")
  #hide("article_plotly")
  
  ###############################
  # Initiliaze Reactive variables
  ###############################
  
  #initialize reactive dataframe for author similarity values
  author_sims<-reactive({
    return(data.frame(Authors=author_names_vec,Similarity=rep(0,length(author_names_vec))))
  })
  
  article_author_sims<-reactive({
    return(data.frame(Authors=author_names_vec,Similarity=rep(0,length(author_names_vec))))
  })
  
  #initialize reactive dataframe for article similarity values
  article_sims <-reactive({
    return(data.frame(articles=article_df$formatted_column,Similarity=rep(0,length(article_df$title))))
  })
  
  #initialize reactive dataframe for article similarity values for Semantic Search queries
  article_sims_SS <- reactive({
    return(data.frame(articles=article_df$formatted_column,Similarity=rep(0,length(article_df$title))))
  })
  
  #initialize reactive dataframe for author similarity values for Semantic Search queries
  author_sims_SS <- reactive({
    return(data.frame(authors=author_names_vec,Similarity=rep(0,dim(AuthorVectors)[1])))
  })
  
  # initialize reactive dataframe for term similarity
  sim_terms_SS <- reactive({
    return(data.frame(terms=dictionary_words,Similarity=rep(0,length(dictionary_words))))
  })
  
  # initialize reactive variable for sampling number of BEAGLE vector indexes
  new_article_beagle_inds<-reactiveValues(a=1:1024)
  
  # initialize reactive variable for semantic search tab search terms
  search_terms_SS <- reactive({
    return(character())
  })
    
  #######################################################################################################    
  # Semantic Search tab
  #######################
  
  # Get search terms that are in dictionary
  # observeEvent({input$vectorize_search_terms},{
  #   get_search_terms <- breakdown(input$search_terms)
  #   search_items <- unlist(strsplit(get_search_terms,split=" "))
  #   search_items <- search_items[search_items %in% dictionary_words==TRUE]
  #   print(search_items)
  #   search_terms_SS <- search_terms_SS()
  #   search_terms_SS <- search_items
  #   #updateSelectizeInput(session,'server_dictionary',selected=search_items)
  #   })
  
  observeEvent(input$SShelp, {
    showModal(modalDialog(
      title = "Semantic Search Help",
      includeMarkdown("markdown/SShelp.md"),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$SShelp2, {
    showModal(modalDialog(
      title = "More information",
      includeMarkdown("markdown/SShelp2.md"),
      easyClose = TRUE
    ))
  })
  
  observe({
    click("vectorize_search_terms")
    #invalidateLater(3000)
  })
  
  get_search_terms_SS <-eventReactive(input$vectorize_search_terms,{
    get_search_terms <- breakdown(input$search_terms)
    search_items <- unlist(strsplit(get_search_terms,split=" "))
    search_items <- search_items[search_items %in% dictionary_words==TRUE]
    #print(search_items)
    search_terms_SS <- search_terms_SS()
    search_terms_SS <- search_items
    return(search_terms_SS)
  })
  
  # Update current search terms found in dictionary
  #updateSelectizeInput(session, 'server_dictionary', choices = dictionary_words, server = TRUE)
  
  # update number of BEAGLE vectors to use in calculations
  observeEvent({input$slider_BEAGLE_SS
    input$redo_beagle_vecs_SS},{
      new_article_beagle_inds$a<-sample(1:1024,input$slider_BEAGLE_SS)
    })
  
  # compute similarities between search term vector and articles
  # for plot of article similarity
  get_sim_md_articles_SS <- eventReactive(({input$server_dictionary
                                            input$vectorize_search_terms
                                            input$slider_num_articles
                                            input$slider_num_k
                                            input$select_query_type
                                            input$slider_num_year_SS}),{
                                              
    search_terms <- get_search_terms_SS()                                        
    #if(!is.null(input$server_dictionary))
    if(!is.null(search_terms))
      
      #search_index <-   which(dictionary_words %in% input$server_dictionary)
      search_index <-   which(dictionary_words %in% search_terms)
      
      if (input$select_query_type == 1){
      
        if(length(search_index)>1) {
          query_vector <- colSums(WordVectors[search_index,])
        }else{
          query_vector <- WordVectors[search_index,] 
        }
        
        get_cos <-        cosine_x_to_m(query_vector[new_article_beagle_inds$a],
                                        article_vectors[,new_article_beagle_inds$a])
        
        article_sims_SS<-article_sims_SS()
        
        article_sims_SS$Similarity <- round(get_cos,digits=4)
        
        article_sims_SS <- cbind(article_sims_SS,
                                 year=article_df$year,
                                 title=article_df$title,
                                 index=seq(1,dim(article_vectors)[1]))
        
        article_sims_SS <- article_sims_SS[article_sims_SS$year >= input$slider_num_year_SS[1] &
                                             article_sims_SS$year <= input$slider_num_year_SS[2],  ]
        
        article_sims_SS <- article_sims_SS[order(article_sims_SS$Similarity,decreasing=T),]
        
        article_sims_SS <- article_sims_SS[1:input$slider_num_articles,]
        
        top_terms <- as.character(article_sims_SS$title)
    
        article_index <- article_sims_SS$index
        
        temp_article_matrix <- article_vectors[article_index,]
        
        row.names(temp_article_matrix) <- top_terms
      
      } else {
        
        query_matrix     <- WordVectors[search_index,]
        get_cos_matrix   <- apply(query_matrix[,new_article_beagle_inds$a],1,function(x) cosine_x_to_m(x,
                                                                           article_vectors[,new_article_beagle_inds$a]))
        if (input$select_query_type == 2) {
          multiply_columns <- apply(get_cos_matrix,1,prod)
        } else if (input$select_query_type == 3){
          get_cos_matrix <- apply(get_cos_matrix,2,normalize_vector)
          multiply_columns <- apply(get_cos_matrix,1,max)
        }
        article_sims_SS<-article_sims_SS()
        article_sims_SS$Similarity <- round(multiply_columns,digits=4)
        article_sims_SS <- cbind(article_sims_SS,
                                 year=article_df$year,
                                 title=article_df$title,
                                 index=seq(1,dim(article_vectors)[1]))
        article_sims_SS <- article_sims_SS[article_sims_SS$year >= input$slider_num_year_SS[1] &
                                             article_sims_SS$year <= input$slider_num_year_SS[2],  ]
        article_sims_SS <- article_sims_SS[order(article_sims_SS$Similarity,decreasing=T),]
        article_sims_SS <- article_sims_SS[1:input$slider_num_articles,]
        top_terms <- as.character(article_sims_SS$title)
        article_index <- article_sims_SS$index
        temp_article_matrix <- article_vectors[article_index,]
        row.names(temp_article_matrix) <- top_terms
      }
      
      mdistance <- cosine(t(temp_article_matrix))
      fit <- cmdscale(1-mdistance,eig=TRUE, k=2)
      
      #importance <- normalize_vector(rowMeans(mdistance^7))
      
      cluster <- kmeans(fit$points,input$slider_num_k)
      get_cluster_indexes <- as.numeric(cluster$cluster)
      fit <- data.frame(fit$points)
      fit <-cbind(fit,cluster=get_cluster_indexes,orig_ind=article_index)
      return(fit)
    
  })
  
    
  # PLOT similar articles
  output$sim_articles_plot_SS <- renderPlotly({
    
     fit <- get_sim_md_articles_SS()
     fit <<- fit
     
     row.names(fit) <- sapply(row.names(fit),wrap_string)
    # print(head(fit))
     
     ax <- list(
       title = "",
       zeroline = TRUE,
       showline = FALSE,
       showticklabels = FALSE,
       showgrid = FALSE
     )
    
     p   <- plot_ly(fit, x = ~X1, y= ~X2, 
                    type = "scatter", 
                    mode = "markers", 
                    color = ~cluster,
                    text = ~row.names(fit),
                    symbol = ~cluster,
                    source = "article_SS_plot",
                    hoverinfo = 'text' ) %>% 
                    hide_colorbar %>%
                    layout(xaxis = ax, yaxis = ax,
                           showlegend = FALSE) 
    p$elementId <- NULL
    p 
    
  })
  
  # plotly click
  output$article_plotly <- renderUI({
    d<-event_data("plotly_click", source="article_SS_plot")
    #print(d)
   # selected_title <- article_df[fit[d$pointNumber,]$orig_ind,]$formatted_column
    if (is.null(d)) {
      return(HTML("<p>click point in article space to view abstract here</p>"))
      }
    else {
      current_click<-fit[fit$X1==d$x & fit$X2 == d$y,]

      selected_article<-article_df[fit[(d$pointNumber+1),]$orig_ind,]
      selected_title<-selected_article$formatted_column
      abstract<-selected_article$abstract
      title<-as.character(selected_article$title)
      year<-as.character(selected_article$year)
      title<-paste(unlist(strsplit(title,split=" ")),collapse="+")
      search_scholar<-paste(c("https://scholar.google.com/scholar?hl=en&as_sdt=0%2C33&q=",
                              title,
                              "&btnG="),collapse="")
    
      
      #Specifying the url for desired website to be scrapped
     # url <- search_scholar
    #  webpage <- read_html(url)
    #  a<-html_nodes(webpage, "div.gs_fl")
    #  a_l<-length(a)
    #  citations<-paste(unlist(strsplit(html_text(a[a_l]),split=" "))[3:5], collapse=" ")
      
      
      t_abstract<-HTML(paste(c(selected_title,
                   "<p>",year,"</p>",
                   "<a style='font-size:11px' href=",search_scholar,"> Find on Google Scholar </a>",
                   "<br>",
                   #"<p>",citations,"</p>",
                   "<p>",as.character(abstract),"</p>"),collapse = " "))
      showElement("article_plotly")
      return(t_abstract)
    }
  })
  
  
  # compute similarities between search term vector and authors
  # for plot of author similarity
  get_sim_md_authors_SS <- eventReactive(({input$vectorize_search_terms
    input$slider_num_authors
    input$slider_num_k_authors
    input$select_query_type}),{
      
      search_terms <- get_search_terms_SS()    
      
      #if(!is.null(input$server_dictionary))
      if(!is.null(search_terms))
        
       # search_index <-   which(dictionary_words%in% input$server_dictionary)
        search_index <-   which(dictionary_words%in% search_terms)
      
      if (input$select_query_type == 1){
        
        if(length(search_index)>1) {
          query_vector <- colSums(WordVectors[search_index,])
        }else{
          query_vector <- WordVectors[search_index,] 
        }
        
        get_cos <-        cosine_x_to_m(query_vector[new_article_beagle_inds$a],
                                        AuthorVectors[,new_article_beagle_inds$a])
        
        author_sims_SS<-author_sims_SS()
        
        author_sims_SS$Similarity <- round(get_cos,digits=4)
        
        author_sims_SS <- cbind(author_sims_SS,
                                 index=seq(1,dim(AuthorVectors)[1]))
        
        author_sims_SS <- author_sims_SS[order(author_sims_SS$Similarity,decreasing=T),]
        
        author_sims_SS <- author_sims_SS[1:input$slider_num_authors,]
        
        top_terms <- as.character(author_sims_SS$authors)
        
        author_index <- author_sims_SS$index
        
        temp_author_matrix <- AuthorVectors[author_index,]
        
        row.names(temp_author_matrix) <- top_terms
        
      } else {
        
        query_matrix     <- WordVectors[search_index,]
        get_cos_matrix   <- apply(query_matrix[,new_article_beagle_inds$a],1,
                                  function(x) cosine_x_to_m(x,AuthorVectors[,new_article_beagle_inds$a]))
        if (input$select_query_type == 2) {
          multiply_columns <- apply(get_cos_matrix,1,prod)
        } else if (input$select_query_type == 3){
          get_cos_matrix <- apply(get_cos_matrix,2,normalize_vector)
          multiply_columns <- apply(get_cos_matrix,1,max)
        }
        author_sims_SS<-author_sims_SS()
        author_sims_SS$Similarity <- round(multiply_columns,digits=4)
        author_sims_SS <- cbind(author_sims_SS,
                                 index=seq(1,dim(AuthorVectors)[1]))
        author_sims_SS <- author_sims_SS[order(author_sims_SS$Similarity,decreasing=T),]
        author_sims_SS <- author_sims_SS[1:input$slider_num_authors,]
        top_terms <- as.character(author_sims_SS$authors)
        author_index <- author_sims_SS$index
        temp_author_matrix <- AuthorVectors[author_index,]
        row.names(temp_author_matrix) <- top_terms
      }
      
      mdistance <- cosine(t(temp_author_matrix))
      fit <- cmdscale(1-mdistance,eig=TRUE, k=2)
      
      cluster <- kmeans(fit$points,input$slider_num_k)
      get_cluster_indexes <- as.numeric(cluster$cluster)
      fit <- data.frame(fit$points)
      fit <-cbind(fit,cluster=get_cluster_indexes)
      return(fit)
      
    })
  
  
  # PLOT similar authors
  output$sim_authors_plot_SS <- renderPlot({
  
    fit <- data.frame(get_sim_md_authors_SS())
    
    p<-ggplot(fit,aes(x=X1,y=X2))+geom_label_repel(aes(label=row.names(fit)))
    
    return(p)
    
  })
  
  # DATATABLE of top 100 articles
  # Compute similarities between dictionary input selector and all articles
  # Sums all chosen Dictionary term vectors into a single query vector
  get_article_sims_SS <- function(article_sims_SS){
    
    search_terms <- get_search_terms_SS()  
    
    #if(!is.null(input$server_dictionary))
    if(!is.null(search_terms))
      
     #search_index <-   which(dictionary_words%in%input$server_dictionary)
     search_index <-   which(dictionary_words %in% search_terms)
    
      if (input$select_query_type ==1 ){
    
        if(length(search_index)>1) {
          query_vector <- colSums(WordVectors[search_index,])
        }else{
          query_vector <- WordVectors[search_index,] 
        }
      
        get_cos <-        cosine_x_to_m(query_vector[new_article_beagle_inds$a],
                                 article_vectors[,new_article_beagle_inds$a])
        
        article_sims_SS$Similarity <- round(get_cos,digits=4)
        
        article_sims_SS <- cbind(article_sims_SS,
                                 year=article_df$year,
                                 title=article_df$title)
        
        article_sims_SS <- article_sims_SS[order(article_sims_SS$Similarity,decreasing=T),]
      
        } else {
          
          query_matrix     <- WordVectors[search_index,]
          get_cos_matrix   <- apply(query_matrix[,new_article_beagle_inds$a],
                                    1,
                                    function(x) cosine_x_to_m(x,article_vectors[,new_article_beagle_inds$a]))
          
          if (input$select_query_type == 2) {
            multiply_columns <- apply(get_cos_matrix,1,prod)
          } else if (input$select_query_type == 3){
            get_cos_matrix <- apply(get_cos_matrix,2,normalize_vector)
            multiply_columns <- apply(get_cos_matrix,1,max)
          }

          article_sims_SS<-article_sims_SS()
          article_sims_SS$Similarity <- round(multiply_columns,digits=4)
          article_sims_SS <- cbind(article_sims_SS,
                                   year=article_df$year,
                                   title=article_df$title,
                                   index=seq(1,dim(article_vectors)[1]))
          article_sims_SS <- article_sims_SS[order(article_sims_SS$Similarity,decreasing=T),]
      }
      
    return(article_sims_SS[1:100,])
  }
    
    
  # Render datatable of article similarities for Semantic Search tab
  output$articledataset_SS <- DT::renderDT({
    
    article_sims_SS <- get_article_sims_SS(article_sims_SS())

    return(article_sims_SS)
    
  }, options = list(pageLength=10, 
                    columnDefs = list(list(visible=FALSE, targets=c(0,4)))), 
                    escape=FALSE
  )  
  
  ###########################################################################
  #ARTICLE SIMILARITY TAB
  #######################
  
  # Article search bar
  updateSelectizeInput(session, 'server_article', choices = article_titles, server = TRUE)
  
  # compute similarities between search term vector and articles
  # for plot of article similarity
  get_sim_md_articles_AS <- eventReactive(({input$server_article 
    input$slider_num_articles_AS
    input$slider_num_k_AS}),{
      
      if(!is.null(input$server_article))
        
        article_index  <- which(article_df$title==input$server_article)[1]
        get_cos   <- cosine_x_to_m(article_vectors[article_index,new_article_beagle_inds$a],
                                 article_vectors[,new_article_beagle_inds$a])
  
        
        article_sims<-article_sims()
        
        article_sims$Similarity <- round(get_cos,digits=4)
        
        article_sims <- cbind(article_sims,
                                 year=article_df$year,
                                 title=article_df$title,
                                 index=seq(1,dim(article_vectors)[1]))
        
        article_sims <- article_sims[order(article_sims$Similarity,decreasing=T),]
        
        article_sims <- article_sims[1:input$slider_num_articles_AS,]
        
        top_terms <- as.character(article_sims$title)
        
        article_index <- article_sims$index
        
        temp_article_matrix <- article_vectors[article_index,]
        
        row.names(temp_article_matrix) <- top_terms
      
      mdistance <- cosine(t(temp_article_matrix))
      fit <- cmdscale(1-mdistance,eig=TRUE, k=2)
      cluster <- kmeans(fit$points,input$slider_num_k_AS)
      get_cluster_indexes <- as.numeric(cluster$cluster)
      fit <- data.frame(fit$points)
      fit <-cbind(fit,cluster=get_cluster_indexes,orig_ind=article_index)
      return(fit)
      
    })
  
  
  # PLOT similar articles
  output$sim_articles_plot_AS <- renderPlotly({
    
    fit <- get_sim_md_articles_AS()
    fit2 <<- fit
    row.names(fit2) <- sapply(row.names(fit2),wrap_string)
    
    ax <- list(
      title = "",
      zeroline = TRUE,
      showline = FALSE,
      showticklabels = FALSE,
      showgrid = FALSE
    )
    
    p   <- plot_ly(fit2, x = ~X1, y= ~X2, 
                   type = "scatter", 
                   mode = "markers", 
                   color = ~cluster,
                   text = ~row.names(fit),
                   source = "article_AS_plot",
                   symbol = ~cluster,
                   hoverinfo = 'text')%>% 
                   hide_colorbar %>%
                   layout(xaxis = ax, yaxis = ax,
                         showlegend = FALSE) 
    p$elementId <- NULL
    p
    
  })
  
  ### plotly click function to generate abstract for article similarity tab
  
  output$article_plotly2 <- renderUI({
    #browser()
    d<-event_data("plotly_click", source="article_AS_plot")
    #print(d)
    # selected_title <- article_df[fit[d$pointNumber,]$orig_ind,]$formatted_column
    if (is.null(d)) {
      return(HTML("<p>click point in article space to view abstract here</p>"))
    }
    else {
      current_click <- fit2[fit2$X1==d$x & fit2$X2 == d$y,]
      
      selected_article <- article_df[fit2[(d$pointNumber+1),]$orig_ind,]
      selected_title <- selected_article$formatted_column
      abstract <- selected_article$abstract
      title <- as.character(selected_article$title)
      year <- as.character(selected_article$year)
      title <- paste(unlist(strsplit(title,split=" ")),collapse="+")
      search_scholar <- paste(c("https://scholar.google.com/scholar?hl=en&as_sdt=0%2C33&q=",
                              title,
                              "&btnG="),collapse="")
      
      t_abstract <- HTML(paste(c(selected_title,
                               "<p>",year,"</p>",
                               "<a style='font-size:11px' href=",search_scholar,"> Find on Google Scholar </a>",
                               "<br>",
                               #"<p>",citations,"</p>",
                               "<p>",as.character(abstract),"</p>"),collapse = " "))
      showElement("article_plotly2")
      return(t_abstract)
    }
  })
  
  
  # Compute similarities between one article and all others
  get_article_sims <- function(article_sims){
    
    if(!is.null(input$server_article))
      
      article_index  <- which(article_df$title==input$server_article)[1]
    
      get_cos   <- cosine_x_to_m(article_vectors[article_index,new_article_beagle_inds$a],
                               article_vectors[,new_article_beagle_inds$a])
      article_sims$Similarity  <-  round(get_cos,digits=4)
    
      article_sims  <-  cbind(article_sims,
                            year=article_df$year,
                            title=article_df$title)
    
      article_sims  <-  article_sims[order(article_sims$Similarity,decreasing=T),]
    
    return(article_sims[1:100,])
  }
  
  
  # Render datatable of article similarities
  output$articledataset <- DT::renderDT({
    
    article_sims <- get_article_sims(article_sims())
    
    return(article_sims)
  }, options = list(pageLength=10, 
                    columnDefs = list(list(visible=FALSE, targets=c(0,4)))), 
                    escape=FALSE)  
  
  
    
        
  
  ############################################################
  #Author Similarity
  ###################
  
  # Author search bar
  updateSelectizeInput(session, 'server_author', choices = author_names_vec, server = TRUE)
  
  # compute similarities between search author and all authors
  # for plot of author similarity
  get_sim_md_articles_AuS <- eventReactive(({input$server_author
    input$slider_num_articles_AuS
    input$slider_num_k_AuS}),{
      
      if(!is.null(input$server_author))
        
      get_cos    <- cosine_x_to_m(AuthorVectors[input$server_author,],
                                    AuthorVectors)
      
      author_sims<-author_sims()
      
      author_sims$Similarity <- round(get_cos,digits=4)
      
      author_sims <- cbind(author_sims, index=seq(1,dim(AuthorVectors)[1]))
      
      author_sims <- author_sims[order(author_sims$Similarity,decreasing=T),]
      
      author_sims <- author_sims[1:input$slider_num_articles_AuS,]
    
      author_index <- author_sims$index
      
      temp_author_matrix <- AuthorVectors[author_index,]
      
      mdistance <- cosine(t(temp_author_matrix))
      fit <- cmdscale(1-mdistance,eig=TRUE, k=2)
      
      #importance <- normalize_vector(rowMeans(mdistance^7))
      
      cluster <- kmeans(fit$points,input$slider_num_k_AuS)
      get_cluster_indexes <- as.numeric(cluster$cluster)
      fit <- data.frame(fit$points)
      fit <-cbind(fit,cluster=get_cluster_indexes)
      return(fit)
      
    })
  
  
  # PLOT similar Authors
  output$sim_articles_plot_AuS <- renderPlotly({
    
    fit <- get_sim_md_articles_AuS()
    
    row.names(fit) <- sapply(row.names(fit),wrap_string)
    
    ax <- list(
      title = "",
      zeroline = TRUE,
      showline = FALSE,
      showticklabels = FALSE,
      showgrid = FALSE
    )
    
    p   <- plot_ly(fit, x = ~X1, y= ~X2, 
                   type = "scatter", 
                   mode = "markers", 
                   color = ~cluster,
                   text = ~row.names(fit),
                   symbol = ~cluster,
                   hoverinfo = 'text') %>% 
                  hide_colorbar %>%
                  layout(xaxis = ax, yaxis = ax,
                        showlegend = FALSE) 
    p$elementId <- NULL
    p
    
  })
  
  #function to compute similarity from one author to all authors
  get_author_sims <- function(author_sims){
    
    if(!is.null(input$server_author))

      get_cos    <- cosine_x_to_m(AuthorVectors[input$server_author,],
                                   AuthorVectors)
      author_sims$Similarity <- round(get_cos,digits=4)
      
      author_sims <- author_sims[order(author_sims$Similarity,decreasing=T),]
      
      return(author_sims[1:100,])
      
     }
  
  # Render the datatable of author similarity values in AuthorSimilarity tab
  output$authordataset <- renderDT({
    
    if(!is.null(input$server_author)){
      
      author_sims <- get_author_sims(author_sims())
      
      return(author_sims)
      
      } else
        
      return(author_sims())
    
   }, options = list(pageLength=10), rownames=FALSE)
  

  # Reactive handler for common sample indexes for BEAGLE vectors
  # applied to article similarity list and plot
  observeEvent({input$slider_BEAGLE_article
                input$redo_beagle_vecs},{
      
    new_article_beagle_inds$a <- sample(1:1024,input$slider_BEAGLE_article)
    
  })
    
  ###### article to author similarity
    
  # Handle Article title input selector in ArticleAuthor tab
  output$article_author <- renderUI({
    opts <- article_titles
    updateSelectInput(session, "show_article_author",
                        label = "Article",
                        choices = opts)
    selectizeInput("show_article_author", "Article", choices = " ", multiple = FALSE)
    })
    
  #function to compute similarity from one article to all authors
  get_article_author_sims <- function(article_author_sims){
    if(!is.null(input$show_article_author))
      article_index <- which(article_df$title==input$show_article_author)[1]
      get_cos       <- cosine_x_to_m(article_vectors[article_index,],AuthorVectors)
      article_author_sims$Similarity <- round(get_cos,digits=4)
      article_author_sims <- article_author_sims[order(article_author_sims$Similarity,decreasing=T),]
      return(article_author_sims[1:100,])
    }
    
  # Render the datatable of author similarity values in AuthorSimilarity tab
  output$article_authordataset <- renderDT({
    if(!is.null(input$show_article_author)){
      article_author_sims <- get_article_author_sims(article_author_sims())
      return(article_author_sims)
    }
    else
      return(article_author_sims())
    }, options = list(pageLength=10), rownames=FALSE)
    
  # allow tabs to run when in background  
  sapply(names(outputOptions(output)),function(x) outputOptions(output, x, suspendWhenHidden = TRUE))
  
}

