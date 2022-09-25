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


ui <- dashboardPage(
  dashboardHeader(title = "Bioconductor Carousel", 
                  titleWidth =300
  ),
  
  dashboardSidebar(width = 300,
                   
          helpText("This is our carousel"),
          helpText("We will have choices of topics to be traversed, e.g., transcriptomics,
blogs, grants")
       ),
  dashboardBody(
 tags$head( 
    tags$style(HTML(".main-sidebar { font-size: 20px; }")) #change the font size to 20
   ),
    fluidRow(
             
              box(width = 12,
                slickROutput("slick_output", width = "70%", height = "550px")
              )
             
             )
    
  )
)

server <- function(input, output) {
  
# eventually we can have different topic types in the base table and filter on it
  filtered <- reactive({
    df
  })
  
  images <- reactive({
    
    images <- lapply(filtered()$Picture,
     function(x){
      htmltools::tags$img(src = x, width = "400px", 
             height = "500px", style="margin-left: auto;  margin-right: auto;")
    })
    
    return(images)
    
  })

   txts = reactive({
          pp = function(x,y) htmltools::tags$body(h4(x, align='left', color='black'), a(href=y,"Link"))
          fdf = filtered()
          N = nrow(fdf)
          lapply(seq_len(N), function(i) pp(fdf$Text[i], fdf$Link[i]))
      })
  
  output$slick_output <- renderSlickR({
    
       slickR(txts(), slideType='a') %synch% 
          slickR(images(), slideId = 'myslick') + settings(arrows=FALSE, dots = TRUE,
               slidesToShow = 1,
               slidesToScroll = 1,
               autoplay = TRUE) 
    
  })
  
  
}

shinyApp(ui = ui, server = server)

