
# following Melissa Salazar
# https://stackoverflow.com/questions/69817284/dynamic-image-carousel-r-shiny

# some figures were made by hand at github.com/vjcitn/bcpics
# text snippets improvised from papers or github sites
# links

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(DT)
library(shinyWidgets)
library(slickR)

# setup for the images, text and links
df <- data.frame(
  Picture = c("https://github.com/vjcitn/bcpics/raw/main/cside.jpg", "https://github.com/vjcitn/bcpics/raw/main/propell.jpg", "https://github.com/vjcitn/bcpics/raw/main/scbub.jpg"),
  Text = c(
"Here, we introduce a statistical method, cell type-specific inference of differential expression (C-SIDE), that identifies cell type-specific DE in spatial transcriptomics, accounting for localization of other cell types. We model gene expression as an additive mixture across cell types of log-linear cell type-specific expression functions. C-SIDE’s framework applies to many contexts: DE due to pathology, anatomical regions, cell-to-cell interactions and cellular microenvironment. Furthermore, C-SIDE enables statistical inference across multiple/replicates.",
"We have developed propeller, a robust and flexible method that leverages biological replication to find statistically significant differences in cell type proportions between groups. Using simulated cell type proportions data, we show that propeller performs well under a variety of scenarios. We applied propeller to test for significant changes in cell type proportions related to human heart development, ageing and COVID-19 disease severity." , "scBubbletree was designed to overcome challenges associated with the rapid growth of the scale of scRNA-seq datasets, such as overplotting, and to accurately preserve the local and global structure of the data. The workflow of scBubbletree is easy-to-use and allows seamless integration with popular approaches for scRNA-seq data analysis."),
  Link=c("https://github.com/dmcable/spacexr", "https://phipsonlab.github.io/propeller-paper-analysis/",
    "https://github.com/snaketron/scBubbletree")
)

# simple UI layout
ui <- dashboardPage(
  dashboardHeader(title = "Bioconductor Carousel", 
                  titleWidth =300
  ),
  
  dashboardSidebar(width = 300,
                   
          helpText("This is a demonstration carousel"),
          helpText("We will have choices of topics to be traversed, e.g., transcriptomics,
blogs, twitter feeds, ..."),
	  radioButtons("sltype", "Selection", choices=c("methods", "blogs", "feeds"), selected="methods")
       ),
  dashboardBody(
  fluidRow(
   box(width=12,
     tags$head( 
        tags$style(HTML(".main-sidebar { font-size: 20px; }")) #change the font size to 20
        ),
     slickROutput("slick_output", width = "70%", height = "550px")
      )
     ) 
  )
)

server <- function(input, output) {

## BLOG DATA FOR IFRAMES
# most sites here referenced at Gelman's
  blog_sites = list( "https://statmodeling.stat.columbia.edu/",
    "https://errorstatistics.com/", "https://xianblog.wordpress.com/category/statistics/", 
    "https://notstatschat.rbind.io/" , "https://www.r-bloggers.com/", "https://junkcharts.typepad.com/numbersruleyourworld/", "https://simplystatistics.org/", "https://robjhyndman.com/hyndsight/", "http://www.johndcook.com/blog/")

  blogH = lapply(blog_sites, function(x) htmltools::tags$iframe(src=x, height="500px", width="95%"))
  blogL = lapply(blog_sites, function(x) htmltools::tags$a(href=x, x))


## TWITTER FEED PROCESSING  -- FOR SOME REASON THE IFRAMES ARE VERY SHORT IN VERTICAL SPACE

tmpl = '<a class="twitter-timeline" data-tweet-limit="10" href="https://twitter.com/%s?ref_src=twsrc^tfw">Tweets by %s</a> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>'
twsrcs = c("3D_Genome", "4DNucleome", "AllAboutTFs", "anshulkundaje", "EpigenChromatin", "generegulation", "GenomeResearch")
twH = lapply(twsrcs, function(x) HTML(sprintf(tmpl, x, x)))
twL = lapply(twsrcs, function(x) htmltools::tags$a(href=paste0("https://twitter.com/", x), x))
  
## LEGACY, THIS WAS TO BE THE "BRAIN" AND MAY STILL BE BUT JUST
## HOLDS THE 'methods' CONTENT
  filtered <- reactive({
    df
  })
  
## IMAGES FOR METHODS CONTENT
  images <- reactive({
    images <- lapply(filtered()$Picture,
     function(x){
      htmltools::tags$img(src = x, width = "400px", 
             height = "500px", style="margin-left: auto;  margin-right: auto;")
    })
    return(images)
  })

## LINKS FOR METHODS CONTENT
   txts = reactive({
          pp = function(x,y) htmltools::tags$body(h4(x, align='left', color='black'), a(href=y,"Link"))
          fdf = filtered()
          N = nrow(fdf)
          lapply(seq_len(N), function(i) pp(fdf$Text[i], fdf$Link[i]))
      })
  
## BRANCHING CODE FOR PRESENTING DIFFERNT TYPES OF CONTENT
  output$slick_output <- renderSlickR({
   AUTOPLAYSPEED=3800
   if (input$sltype == "methods") {
       slickR(txts(), slideType='a') %synch% 
          slickR(images()) + settings(arrows=FALSE, dots = TRUE,
               slidesToShow = 1,
               slidesToScroll = 1,
               autoplay = TRUE, autoplaySpeed=AUTOPLAYSPEED, pauseOnDotsHover=TRUE) 
       } else if (input$sltype == "blogs") {
       NOBJ <- 2
       m1 <- slickR::slickR(blogH,
               slideType = 'iframe') + settings(dots  = TRUE, arrows=TRUE, 
                   slidesToShow   = NOBJ, slidesToScroll = NOBJ, autoplay=TRUE, autoplaySpeed=AUTOPLAYSPEED,
                     pauseOnDotsHover=TRUE)
       m2 <- slickR::slickR(blogL, slideType="a") + settings(dots  = TRUE, arrows=TRUE, 
                   slidesToShow   = NOBJ, slidesToScroll = NOBJ, autoplay=TRUE, pauseOnDotsHover=TRUE,
                      autoplaySpeed=AUTOPLAYSPEED)
       m1 %synch% m2
       } else if (input$sltype == "feeds") {
       NOBJ <- 2
       m1 <- slickR::slickR(twH,slideType="iframe") + settings(dots  = TRUE, arrows=TRUE, 
                   slidesToShow   = NOBJ, slidesToScroll = NOBJ, autoplay=TRUE, autoplaySpeed=AUTOPLAYSPEED,
                     pauseOnDotsHover=TRUE)
       m2 <- slickR::slickR(twL, slideType="a") + settings(dots  = TRUE, arrows=TRUE, 
                   slidesToShow   = NOBJ, slidesToScroll = NOBJ, autoplay=TRUE, pauseOnDotsHover=TRUE,
                      autoplaySpeed=AUTOPLAYSPEED)
       m2 %synch% m1
       }
    })
}

shinyApp(ui = ui, server = server)

