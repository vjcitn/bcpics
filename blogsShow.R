library(slickR)

get_help <- function(fn,pkg){
 
  ff <- get('index.search',envir = asNamespace('utils'))(
    topic = fn, 
    paths = find.package(pkg),
    TRUE)
  
  path    <- dirname(ff)
  dirpath <- dirname(path)
  pkgname <- basename(dirpath)
  RdDB    <- file.path(path, pkgname)
  
  paste0(
    utils::capture.output({
      tools::Rd2HTML(
        Rd = get('fetchRdDB', envir = asNamespace('tools'))(RdDB, basename(ff))
      )
    }),
  collapse='\n')
  
}

#help_files <- lapply(
#    ls("package:stats",pattern = '^r')[-1],
#    get_help,
#    pkg = 'stats')

blog_sites = list( "https://statmodeling.stat.columbia.edu/",
    "https://errorstatistics.com/", "https://xianblog.wordpress.com/category/statistics/", 
    "https://notstatschat.rbind.io/" , "https://simplystatistics.org/")

blogH = lapply(blog_sites, function(x) htmltools::tags$iframe(src=x, height="500px", width="500px"))
blogL = lapply(blog_sites, function(x) htmltools::tags$a(href=x, x))

m1 <- slickR::slickR(blogH,
               slideType = 'iframe') + settings(dots  = TRUE, arrows=TRUE, slidesToShow   = 3, slidesToScroll = 3, autoplay=TRUE)
m2 <- slickR::slickR(blogL, slideType="a") +
  settings(dots  = TRUE, arrows=TRUE, slidesToShow   = 3, slidesToScroll = 3, autoplay=TRUE)

m1 %synch% m2
