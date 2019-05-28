## The Semantic Librarian ####
#  Semantic Search based on  Semantic Vectors
#  Shiny Code: Matthew Crump, 2018 <- Blame Matt for things that don't work right now
#  Semantic Vectors: Harinder Aujla
#  Model Development: Randall Jamieson


## Load Libraries ####

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
library(stringr)

## Load Functions ####

source("functions/semanticL.R")

server <- function(input, output, session) {
  
  ## Initiliaze Reactive variables ####
  
  values <- reactiveValues()
  
  ## Semantic Search Tab ####
  #_______________________
  
  ## ..search help ####
  
  observeEvent(input$SShelp, {
    showModal(modalDialog(
      title = "Semantic Search Help",
      includeMarkdown("markdown/SShelp.md"),
      easyClose = TRUE
    ))
  })
  
  ## ..slider help ####
  
  observeEvent(input$SShelp2, {
    showModal(modalDialog(
      title = "More information",
      includeMarkdown("markdown/SShelp2.md"),
      easyClose = TRUE
    ))
  })
  
  ## ..main ####
  
  observe({
    # run on search button click
    input$vectorize_search_terms
    
    #isolate({
      ## ....search terms ####
      
      search_terms_SS <- get_search_terms(input$search_terms,
                                          dictionary_words)
      
      ## ....similarities ####
      
      article_sims_SS <- get_search_article_similarities(search_terms_SS,
                                                         query_type = input$select_query_type)
      
      ## ....MDS for plot ####
      
      article_mds_SS <- get_mds_article_fits(num_articles = input$slider_num_articles,
                                             num_clusters = input$slider_num_k,
                                             year_range = input$slider_num_year_SS,
                                             input_df = article_sims_SS)
      values$article_mds_SS <- article_mds_SS
      
      ## ....table ####
      
      output$articledataset_SS <- DT::renderDT({
        
        return(article_mds_SS %>%
                 select(formatted_column,Similarity) %>%
                 rename(Abstracts = formatted_column) %>%
                 mutate(Similarity = round(Similarity,digits=2))
               )
  
      }, options = list(pageLength=10,
                        columnDefs = list(list(visible=FALSE, targets=c(0)))), 
                        escape=FALSE)
      
      ## ....plot ####
      
      output$sim_articles_plot_SS <- renderPlotly({
        ggp <- ggplot(article_mds_SS, aes(x= X, y= Y,
                                          color=as.factor(cluster),
                                          text=wrap_title))+
          geom_hline(yintercept=0, color="grey")+
          geom_vline(xintercept=0, color="grey")+
          geom_point(aes(size=Similarity), alpha=.75)+
          theme_void()+
          theme(legend.position = "none")
        
        ax <- list(
          title = "",
          zeroline = TRUE,
          showline = FALSE,
          showticklabels = FALSE,
          showgrid = FALSE
        )
        
        p <- ggplotly(ggp, tooltip="text",
                      source = "article_SS_plot",
                      hoverinfo="text") %>%
                layout(xaxis = ax, yaxis = ax,showlegend = FALSE) %>%
                #style(hoverinfo = 'title') %>%
                config(displayModeBar = F) %>%
                layout(xaxis=list(fixedrange=TRUE)) %>%
                layout(yaxis=list(fixedrange=TRUE)) %>%
                event_register('plotly_click')
        
         p$elementId <- NULL
         p
  
      })
    
   # })
    
   })
  
  ## ....plotly clicks ####
  
  output$article_plotly <- renderUI({
    d <- event_data("plotly_click", source="article_SS_plot")
    if (is.null(d)) {
      return(HTML("<p>click point in abstract space to view abstract here</p>"))
      }
    else {
      selected_title <- values$article_mds_SS %>% 
                          filter(round(X,digits=6)==round(d$x,digits=6),
                                 round(Y,digits=6)==round(d$y,digits=6)) %>%
                          select(formatted_column)
      t_abstract <- HTML(paste(c(selected_title)))
      showElement("article_plotly")
      return(t_abstract)
    }
  })
  
  ## Abstract Similarity ######################################
 
  # ..Article search bar ####
  
  updateSelectizeInput(session, 'server_article', choices = article_df$title, server = TRUE)
  
  # ..main ####
  
  observe({
    # run on search button click
    input$server_article

    # ....similarities ####
    
    if(input$server_article %in% article_df$title ==TRUE){
      article_sims <- get_article_article_similarities(a_title = input$server_article)
    
    # ....MDS for plot ####
      
      article_article_mds_SS <- get_mds_article_fits(num_articles = input$slider_num_articles_AS,
                                           num_clusters = input$slider_num_k_AS,
                                           year_range = input$slider_num_year_AS,
                                           input_df = article_sims)
      values$article_article_mds_SS <- article_article_mds_SS
    
    # ....table ####
      
      output$articledataset <- DT::renderDT({
        return(article_article_mds_SS %>%
                 select(formatted_column,Similarity) %>%
                 rename(Abstracts = formatted_column) %>%
                 mutate(Similarity = round(Similarity,digits=2)))
      }, options = list(pageLength=10,
                        columnDefs = list(list(visible=FALSE, targets=c(0)))),
                        escape=FALSE)
      
    # ....plot ####
      
      output$sim_articles_plot_AS <- renderPlotly({

        ggp <- ggplot(article_article_mds_SS, aes(x= X, y= Y,
                                          color=as.factor(cluster),
                                          text=wrap_title))+
          geom_hline(yintercept=0, color="grey")+
          geom_vline(xintercept=0, color="grey")+
          geom_point(aes(size=Similarity), alpha=.75)+
          theme_void()+
          theme(legend.position = "none")
        
        ax <- list(
          title = "",
          zeroline = TRUE,
          showline = FALSE,
          showticklabels = FALSE,
          showgrid = FALSE
        )
        
        p <- ggplotly(ggp, tooltip="text",
                      source = "article_AS_plot",
                      hoverinfo="text") %>%
          layout(xaxis = ax, yaxis = ax,showlegend = FALSE) %>%
          #style(hoverinfo = 'title') %>%
          config(displayModeBar = F) %>%
          layout(xaxis=list(fixedrange=TRUE)) %>%
          layout(yaxis=list(fixedrange=TRUE)) %>%
          event_register('plotly_click')

      })
    }
    
  })
  
  # ....plotly clicks ####
  
  output$article_plotly2 <- renderUI({
    d <- event_data("plotly_click", source="article_AS_plot")
    if (is.null(d)) {
      return(HTML("<p>click point in abstract space to view abstract here</p>"))
    }
    else {
      selected_title <- values$article_article_mds_SS %>% 
        filter(round(X,digits=6)==round(d$x,digits=6),
               round(Y,digits=6)==round(d$y,digits=6)) %>%
        select(formatted_column)
      t_abstract <- HTML(paste(c(selected_title)))
      showElement("article_plotly2")
      return(t_abstract)
    }
  })
  
  
  # Author Similarity ################################

  # ..Author search bar ####
  
   updateSelectizeInput(session, 'server_author', choices = sort(author_list), server = TRUE)
  
  # ..main ####
  
  observe({
    # run on author select
    input$server_author
    
    # ....similarities ####
    
    if(input$server_author %in% author_list ==TRUE){
      author_sims <- get_author_similarities(a_name = input$server_author)
      
    # ....MDS for plot ####
      
      author_mds_SS <- get_mds_author_fits(num_authors = input$slider_num_articles_AuS,
                                           num_clusters = input$slider_num_k_AuS,
                                           input_df = author_sims)
    }
    
    # ....table ####
    
      output$authordataset <- renderDT({
          return(author_mds_SS %>%
                   select(author,
                          Similarity))
       }, options = list(pageLength=10), rownames=FALSE)
    
    # ....ggplot author similarity ####
    
    output$authors_plot <- renderPlot({
      ggplot(author_mds_SS, aes(x=X, y=Y, color=as.factor(cluster),
                                label=author, shape=selected_author))+
        geom_hline(yintercept=0, color="grey")+
        geom_vline(xintercept=0, color="grey")+
        geom_point(aes(size=Similarity), alpha=.75)+
        geom_text_repel(aes(size=Similarity),color="black", force=1.5)+
        scale_size(range = c(4, 7))+
        theme_void()+
        theme(legend.position = "none")
    })
      
    })
  
  # Abstract Author Similarity ################################
  
  # ..Article search bar ####
  
  updateSelectizeInput(session, 'article_author', choices = article_df$title, server = TRUE)
  
  # ..main ####
  
  observe({
    # run on author select
    input$article_author
    
    
     if(input$article_author %in% article_df$title ==TRUE){
     # ....similarities ####
       
       article_author_sims <- get_article_author_similarities(a_title = input$article_author)
       
     # ....MDS for plot ####
       
       article_author_mds_SS <- get_mds_author_fits(num_authors = input$slider_num_articles_AAS,
                                            num_clusters = input$slider_num_k_AAS,
                                            input_df = article_author_sims)
     }
    
    # ....table ####
    
    output$articleauthordataset <- renderDT({
      return(article_author_mds_SS %>%
               select(author,
                      Similarity))
    }, options = list(pageLength=10), rownames=FALSE)
    
    # ....ggplot article author similarity ####
    
    output$article_authors_plot <- renderPlot({
      ggplot(article_author_mds_SS, aes(x=X, y=Y, color=as.factor(cluster),
                                label=author))+
        geom_hline(yintercept=0, color="grey")+
        geom_vline(xintercept=0, color="grey")+
        geom_point(aes(size=Similarity), alpha=.75)+
        geom_text_repel(aes(size=Similarity),color="black", force=1.5)+
        scale_size(range = c(4, 7))+
        theme_void()+
        theme(legend.position = "none")
    })
    
  })

 
  ## SEARCH ALL ####
 
   # Author search bar
   updateSelectizeInput(session, 'server_author2', choices = sort(author_list), server = TRUE, selected='')
  
  # journal search
   journals <- levels(article_df$journal)
  
   updateSelectizeInput(session, 'journals', choices = journals, server = TRUE, selected='')
  # 
  # # session search
  # sessions <- article_df$session
  # sessions <- sort(sessions)
  # updateSelectizeInput(session, 'the_session', choices = sessions, server = TRUE, selected='')
  # 
  ## ..reset ####
   
   observeEvent(input$resetOptions, {
     
     updateSelectizeInput(session, 'server_author2', choices = sort(author_list), server = TRUE, selected='')
     updateSelectizeInput(session, 'journals', choices = journals, server = TRUE, selected='')
     
   })
   
  output$all_abstracts <- renderDT({
    restrict_article_df <- article_df %>%
      filter(str_detect(as.character(authorlist),input$server_author2),
             str_detect(as.character(journals),input$journals)) %>%
      rename(Abstracts = formatted_column) %>%
      select(Abstracts,year) %>%
      slice(1:100)
    return(restrict_article_df)
  }, options = list(pageLength=10), rownames=FALSE,
  escape=FALSE)

  # # allow tabs to run when in background  
  # sapply(names(outputOptions(output)),function(x) outputOptions(output, x, suspendWhenHidden = TRUE))
  
}

