#################
#  The Semantic Librarian
#  Semantic Search based on  Semantic Vectors
#  Shiny Code: Matthew Crump, 2018 <- Blame Matt for things that don't work right now
#  Semantic Vectors: Harinder Aujla
#  Model Development: Randall Jamieson
#################


jscode <- "shinyjs.goauthor = function(x){
      Shiny.onInputChange('show_author', x);
Shiny.onInputChange('change_author',x);
tabs = $('.tabbable .nav.nav-tabs li a');
$(tabs[1]).click();
};

shinyjs.goarticle = function(x){
      Shiny.onInputChange('show_article', x);
Shiny.onInputChange('change_article',x);
tabs = $('.tabbable .nav.nav-tabs li a');
$(tabs[0]).click();
};

shinyjs.goarticle = function(x){
      Shiny.onInputChange('show_article_SS', x);
Shiny.onInputChange('change_article_SS',x);
tabs = $('.tabbable .nav.nav-tabs li a');
$(tabs[0]).click();
};

"
library(shinyjs)
library(plotly)

ui <- fluidPage(useShinyjs(),
                extendShinyjs(text = jscode),
                tags$head(
                  tags$style(HTML("hr {border-top: 1px solid #000000;}"))
                ),
  tabsetPanel(
    tabPanel("SemanticSearch",
             sidebarLayout(
               sidebarPanel(textInput("search_terms", label="Search Terms"),
                            actionButton("vectorize_search_terms", label = "Search"),
                            #uiOutput("dictionary"),
                            #selectizeInput("server_dictionary",label="dictionary",choices=NULL,multiple=TRUE),
                            br(),
                            br(),
                            selectInput("select_query_type", label = "Query Style", 
                                        choices = list("Compound Terms" = 1, "AND" = 2, "OR" = 3), 
                                        selected = 1),
                            hidden(
                              sliderInput("slider_BEAGLE_SS", label = "# of BEAGLE dimensions", min = 2, 
                                          max = 1024, value = 1024),
                              actionButton("redo_beagle_vecs_SS", label = "Recompute")
                            ),
                            hr(),
                            #h5("Selected Abstract"),
                            htmlOutput("article_plotly")),
               mainPanel(textOutput("articleformatted_SS"),
                         h4("Article Space"),
                         plotlyOutput("sim_articles_plot_SS", height='100%'),
                         fluidRow(
                           column(4,
                            sliderInput("slider_num_articles", label = "number of articles", min = 10, 
                                       max = 500, value = 100, step=1)
                            ),
                           column(4,
                            sliderInput("slider_num_k", label = "number of clusters", min = 1, 
                                       max = 10, value = 3, step=1)
                           ),
                           column(4,
                                  sliderInput("slider_num_year_SS", label = "years", min = 1890, 
                                              max = 2015, value = c(1890,2015), step=1)
                           )
                         ),
                         #h4("Search Term Space"),
                         #plotOutput("sim_terms_plot_SS"),
                         h4("Top 100 Articles"),
                         DT::dataTableOutput("articledataset_SS"),
                         br(),
                         h4("Author Space"),
                         plotOutput("sim_authors_plot_SS"),
                         fluidRow(
                           column(4,
                                  sliderInput("slider_num_authors", label = "number of authors", min = 10, 
                                              max = 100, value = 25, step=1)
                           ),
                           column(4,
                                  sliderInput("slider_num_k_author", label = "number of clusters", min = 1, 
                                              max = 10, value = 3, step=1)
                           )
                         )
                         #h4("Mean Similarity by Year"),
                         #plotOutput("sim_hist_SS"),
                         #sliderInput("slider_sim_hist_year", label = "year", min = 1890, 
                          #           max = 2015, value = c(1890,2015), step=1)
               )
             )
    ),
    
    tabPanel("ArticleSimilarity",
             sidebarLayout(
               sidebarPanel(#uiOutput("article"),
                            selectizeInput("server_article",
                                           label="article",
                                           choices=NULL),
                            sliderInput("slider_BEAGLE_article", label = "# of BEAGLE dimensions", min = 2, 
                                        max = 1024, value = 1024),
                            actionButton("redo_beagle_vecs", label = "Recompute"),
                            hr(),
                            #h5("Selected Abstract"),
                            htmlOutput("article_plotly2")),
               mainPanel(h4("Article Space"),
                         plotlyOutput("sim_articles_plot_AS", height='100%'),
                         fluidRow(
                           column(4,
                            sliderInput("slider_num_articles_AS", label = "number of articles", min = 10, 
                                     max = 500, value = 100, step=1)
                            ),
                           column(4,
                            sliderInput("slider_num_k_AS", label = "number of clusters", min = 1, 
                                     max = 10, value = 3, step=1)
                            )
                         ),
                         h4("Top Articles"),
                         DT::dataTableOutput("articledataset"),
                         br()
                        # h4("Mean Similarity by Year"),
                        # plotOutput("sim_hist")
               )
             )
    ),
    
    tabPanel("AuthorSimilarity",
             sidebarLayout(
               sidebarPanel(#uiOutput("author"),
                            selectizeInput("server_author",
                                           label="author",
                                           choices=NULL)),
              #sliderInput("slider_BEAGLE", label = "# of BEAGLE dimensions", min = 2, 
              #                          max = 1024, value = 1024),
              # checkboxInput("check_svd", label = "Use SVD", value = FALSE),
              # sliderInput("slider_svd", label = "choose SVDs", min = 1, 
              #             max = 500, value = c(1,500)),
              # downloadButton("downloadAuthorData", "Download")
              # ),
               mainPanel(h4("Author Space"),
                         plotlyOutput("sim_articles_plot_AuS", height='100%'),
                         fluidRow(
                           column(4,
                            sliderInput("slider_num_articles_AuS", label = "number of authors", min = 10, 
                                     max = 500, value = 100, step=1)
                            ),
                           column(4,
                            sliderInput("slider_num_k_AuS", label = "number of clusters", min = 1, 
                                     max = 10, value = 3, step=1)
                            )
                         ),
                         h4("Top Authors"),
                         DT::dataTableOutput("authordataset"))
               )
             ),
    
    #tabPanel("Database",
    #         verticalLayout(
    #           mainPanel(DT::dataTableOutput("alldataset"))
    #         )
    #),
    
    #tabPanel("MultipleAuthor",
    #         sidebarLayout(
    #           sidebarPanel(uiOutput("multiple_author"),
    #                        actionButton("compute_msim",label="compute")),
    #           mainPanel(plotlyOutput("m_author_sim_plot"))
    #         )
    #),
    
    tabPanel("ArticleAuthor",
             sidebarLayout(
               sidebarPanel(uiOutput("article_author")),
               mainPanel(DT::dataTableOutput("article_authordataset"))
             )
    )
    
  )
)
  

  
