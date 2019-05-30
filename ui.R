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
library(shinythemes)
library(plotly)

ui <- tagList(tags$head(tags$style(HTML("hr {border-top: 1px solid #000000;}")),
                        tags$style(HTML(".fa { font-size: 20px; }")),
                        tags$head(includeHTML("www/ga_script.js"))
                        #tags$style(HTML(".hovertext text { font-size: 12px !important;}"))
                        ),
  useShinyjs(),
  extendShinyjs(text = jscode),
  navbarPage("SemanticLibrarian",
             theme = shinytheme("yeti"),
    tabPanel("SemanticSearch",
             sidebarLayout(
               sidebarPanel(h3("Search for APA abstracts"),
                            textInput("search_terms", label="Search Terms",
                                      value="remembering"),
                            actionButton("vectorize_search_terms", label = "Search"),
                            actionButton("SShelp", "Help"),
                            br(),
                            br(),
                            selectInput("select_query_type", label = "Query Style", 
                                        choices = list("Compound Terms" = 1, "AND" = 2, "OR" = 3), 
                                        selected = 1),
                            hr(),
                            htmlOutput("article_plotly")),
               mainPanel(h3("Find the most semantically similar abstracts"),
                         textOutput("articleformatted_SS"),
                         h4("Abstract Space"),
                         plotlyOutput("sim_articles_plot_SS", height='100%'),
                         actionButton("SShelp2", "More info"),
                         fluidRow(
                           column(4,
                            sliderInput("slider_num_articles", label = "number of articles", min = 10, 
                                       max = 100, value = 25, step=1)
                            ),
                           column(4,
                            sliderInput("slider_num_k", label = "number of clusters", min = 1, 
                                       max = 10, value = 3, step=1)
                           ),
                           column(4,
                                  sliderInput("slider_num_year_SS", label = "years", min = 1890, 
                                              max = 2016, value = c(1890,2016), step=1)
                           )
                         ),
                         h4("Top Abstracts"),
                         DT::dataTableOutput("articledataset_SS")
               )
             )
    ),
    
    tabPanel("AbstractSimilarity",
             sidebarLayout(
               sidebarPanel(#uiOutput("article"),
                            h3("Search an APA abstract"),
                            selectizeInput("server_article",
                                           label="Abstract",
                                           choices=NULL),
                            hr(),
                            #h5("Selected Abstract"),
                            htmlOutput("article_plotly2")),
               mainPanel(h3("Find similar articles"),
                         h4("Article Space"),
                         plotlyOutput("sim_articles_plot_AS", height='100%'),
                         fluidRow(
                           column(4,
                            sliderInput("slider_num_articles_AS", label = "number of articles", min = 10, 
                                     max = 384, value = 25, step=1)
                            ),
                           column(4,
                            sliderInput("slider_num_k_AS", label = "number of clusters", min = 1, 
                                     max = 10, value = 3, step=1)
                            ),
                           column(4,
                                  sliderInput("slider_num_year_AS", label = "years", min = 1890, 
                                              max = 2016, value = c(1890,2016), step=1)
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
                            h3("Search an author"),
                            selectizeInput("server_author",
                                           label="Author",
                                           choices=NULL)),
               mainPanel(h3("Find similar authors"),
                         h4("Author Space"),
                         #plotlyOutput("sim_authors_plot_AuS", height='100%'),
                         plotOutput("authors_plot"),
                         fluidRow(
                           column(4,
                            sliderInput("slider_num_articles_AuS", label = "number of authors", min = 10, 
                                     max = 500, value = 15, step=1)
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
    
    tabPanel("AbstractAuthor",
             sidebarLayout(
               sidebarPanel(
                 h3("Search an APA abstract"),
                 selectizeInput("article_author",
                                label="Abstract",
                                choices=NULL)),
               mainPanel(
                 h3("Most similar authors to the article"),
                 #plotlyOutput("sim_authors_plot_AAS", height='100%'),
                 plotOutput("article_authors_plot"),
                 fluidRow(
                   column(4,
                          sliderInput("slider_num_articles_AAS", label = "number of authors", min = 10, 
                                      max = 500, value = 15, step=1)
                   ),
                   column(4,
                          sliderInput("slider_num_k_AAS", label = "number of clusters", min = 1, 
                                      max = 10, value = 3, step=1)
                   )
                          ),
                 h4("Top Authors"),
                 DT::dataTableOutput("articleauthordataset")
                 )
                )
             ),
    tabPanel("Search All",
             sidebarLayout(
               sidebarPanel(
                 h3("Search all APA abstracts"),
                 selectizeInput("server_author2",
                                label="Author",
                                choices=NULL),
                 selectizeInput("journals",
                                label="Journal",
                                choices=NULL),
                 actionButton("resetOptions",
                              "Reset")
                 ),
               mainPanel(
                 h3("Abstracts"),
                 DT::dataTableOutput("all_abstracts"))
             )),
             
   tabPanel("About",
            mainPanel(
              div(includeMarkdown("markdown/about.md"),
                  style = "margin-left:10%;margin-right:10%;")
            )
        ),
   tabPanel("Github", icon = icon("fab fa-github"),
            mainPanel(
              div(includeMarkdown("markdown/git.md"),
                  style = "margin-left:10%;margin-right:10%;")
            ))
  )
)
  

  
