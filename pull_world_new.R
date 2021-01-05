
pull_world <- function() {
  url <-paste(
    "https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",
    format(Sys.time(), "%Y-%m-%d"),".xlsx",sep = "")
  
  GET(url,authenticate(":", ":", type = "ntlm"),write_disk(tf <- tempfile(fileext = ".xlsx")))
  
  world <- read_excel(tf)
  return(world)
} 

pull_world()

url <-paste(
  "https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",
  # whaatever the most recent Thursday was: 
  as.Date("2020-12-14"),".xlsx",sep = "")

GET(url,authenticate(":", ":", type = "ntlm"),write_disk(tf <- tempfile(fileext = ".xlsx")))

world <- read_excel(tf)
world
