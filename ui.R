shinyUI(
  navbarPage(
    theme = "bootstrap.min.css",
    title = a(
                href = "http://zsoc"
            ),
    tabPanel(
      HTML("ZSOC: Social Meida Analytics</a></li>
          <li><a href=\"#\">Home</a></li>"),
      sidebarLayout(
        sidebarPanel(
          h5("Response Controls"),
          selectizeInput(
          'company', 'Company', choices = list("All", "Amazon", "Flipkart", "Infibeam", "ShopClues",
                                          "Snapdeal", "JustDial"), multiple = TRUE, selected = "All"
          ),
        selectInput("Year", 
                    label = "Year",
                    choices = rev(list("2009", "2010", "2011",
                                   "2012", "2013", "2014", "2015", "2016")),
                    selected = "Flipkart"),
        HTML("<hr>"),
        h5("Status Controls"),
        selectInput('statusType', label = 'Type of Post', choices = list("Event", "Link", "Offer", "Photo", "Status",
                                                     "Video"), 
                                                  selected = "Link"),
        HTML("<hr>"),
        h5("Post Time Controls"),
        selectInput('month', label = 'Month', choices = c("All", month.abb),
                    selected = "All"),
        width=3),
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Responses", 
                           helpText("Response (Reactions - Likes, haha,...etc, Comments, Shares) all combined Analysis of Facebook Page"),
                           fluidRow(
                             column(6, plotOutput("reactionplot", width = "450px", height = "225px")),
                             column(6, plotOutput("commentsplot", width = "450px", height = "225px"))
                             ),
                           fluidRow(
                             column(6, offset = 3, plotOutput("shareplot", width = "450px", height = "225px"))
                           )
                           ),
                  tabPanel("Statuses",
                           helpText("Status Analysis of a Facebook Post"),
                           fluidRow(
                             column(6, plotOutput("postplot", width = "450px", height = "225px")),
                             column(6, plotOutput("postreactionplot", width = "450px", height = "225px"))
                           ),
                           fluidRow(
                             column(6, plotOutput("postcommentsplot", width = "450px", height = "225px")),
                             column(6, plotOutput("postsharedplot", width = "450px", height = "225px"))
                           )
                           ),
                  tabPanel("Post Time",
                           helpText("Post Time Analysis of a Facebook Post"),
                           plotOutput("posttimeplot")
                           ),
                  #tabPanel("Sentiment",
                  #         helpText("Sentiment Analysis of status of facebook post"),
                  #         plotOutput("sentcoplot"),
                  #         plotOutput("sentallplot")
                  #),
                  tabPanel("Top Post",
                           helpText("Top Posts of Page/s"),
                           DT::dataTableOutput("toppost")
                  )
                  ),
      width = 9
      
      )
    )
  ),
  fluidRow(
    HTML(
      '
      <link href="https://maxcdn.bootstrapcdn.com/font-awesome/4.5.0/css/font-awesome.min.css" rel="stylesheet" integrity="sha384-XdYbMnZ/QjLh6iI4ogqCTaIjrFk87ip+ekIjefZch0Y+PvJ8CDYtEs1ipDmPorQ+" crossorigin="anonymous">
      '
      ),
    HTML('<div class="container">
            <div class="row">
                <div class="col-lg-12">
                    <div class="col-md-8">
                        <!--<a href="#">Terms of Service</a> | <a href="#">Privacy</a>-->
                    </div>
                    <div class="col-md-4">
                        <p class="muted pull-right">© 2016 Dcrucs. All rights reserved</p>
                    </div>
                </div>
            </div>
        </div>'
    )
  )
))