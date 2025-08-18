# Scraping BOE 
# June 2025

# Load packages and set working directory  -----------------------------------------------------

library(httr)
library(tidyverse)
library(curl)
library(rvest)
library(purrr)
library(xml2)

# Set own working directory where the output will go
# Have to crate 2 folders: "output" and "files"
setwd("")

# Define URL with all results ---------------------------------------------

### URL from searching at https://boe.es/buscar/personal.php, and then fixing the page range from 0 to 2000
url <- ""


# Load functions ----------------------------------------------------------

scrape_links <- function(url){ # Function to scrape only the hyperlinks leading to hits
  webpage <- xml2::read_html(url)
  url_ <- webpage %>%
    rvest::html_nodes("a.resultado-busqueda-link-otro") %>%
    rvest::html_attr("href")
  
  link_ <- webpage %>%
    rvest::html_nodes("a.resultado-busqueda-link-otro") %>%
    rvest::html_text()
  return(tibble(reference = link_, url = url_))
}

scrape_descriptions <- function(url){ 
  webpage <- xml2::read_html(url)
  url_ <- webpage %>%
    rvest::html_nodes("p") %>%
    rvest::html_elements("p")
  link_ <- webpage %>%
    rvest::html_nodes("") %>%
    rvest::html_text()
  return(tibble(description = link_))
}

scrape_titles <- function(url){ 
  webpage <- xml2::read_html(url)
  url_ <- webpage %>%
    rvest::html_nodes("h3.documento.tit") %>%
    html_text()
  link_ <- webpage %>%
    rvest::html_nodes("li") %>%
    rvest::html_text()
  return(tibble(title = link_))
}

scrape_text <- function(text){
  rvest::read_html() %>%
    rvest::html_nodes("p.parrafo") %>%
    rvest::html_text()
}

scrape_pdfs <- function(url){
  webpage <- xml2::read_html(url)
  url_ <- webpage %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href")
  link_ <- webpage %>%
    rvest::html_nodes("a") %>%
    rvest::html_text()
  return(tibble(link = link_, url = url_))
}

scrape_xmls <- function(url){
  webpage <- xml2::read_html(url)
  url_ <- webpage %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href")
  link_ <- webpage %>%
    rvest::html_nodes("a") %>%
    rvest::html_text()
  return(tibble(link = link_, url = url_))
}



# Generate table for all hits -------------------------------------------------------------

# Extracting all hyperlinks 
hyperlinks <- read_html(url) %>% html_nodes("a.resultado-busqueda-link-otro") %>% html_attr("href") # Those are all hyperlinks in the front page
hits <- scrape_links(url)

hits$url <- paste("https://boe.es/", hits$url, sep = "") # Pasting the base url over each individual link 
hits$reference <- gsub('[Más... (Referencia )]','', hits$reference)

fecha <- read_html(url) %>% html_nodes("h3") %>% html_text()
hits$date <- as.Date(gsub(".*de ", "", fecha),format="%d/%m/%Y")

nombres <- read_html(url) %>% html_nodes("p") %>% html_text()
hits$title <- nombres[6:(length(nombres)-3)]

hits$pdf_url <- NA 
hits$xml_url <- NA

for(i in 1:nrow(hits)){
  pdf_link <- scrape_pdfs(hits$url[i]) %>% filter(link == "PDF")  
  hits$pdf_url[i] <- paste0("https://www.boe.es/", pdf_link$url)[[1]]
  xml_link <- scrape_xmls(hits$url[i]) %>% filter(link == "XML") 
  hits$xml_url[i] <- paste0("https://www.boe.es/", xml_link$url)[[1]]
  if(i %% 20 == 0){
    cat(".")
  }
}


hits <- hits %>%
  relocate(reference, date, title, url, pdf_url, xml_url)

# Save it
write_csv(hits, "../output/hits_boe.csv")


# Download PDF, XML, and HTML files --------------------------------------

for(i in 1:nrow(hits)){
  download.file(hits$pdf_url[i], paste0("../files/pdf/",hits$reference[i],".pdf"), mode="wb", quiet=T)
  download.file(hits$xml_url[i], paste0("../files/xml/",hits$reference[i],".xml"), mode="wb", quiet=T)
  download.file(hits$url[i], paste0("../files/html/",hits$reference[i],".html"), mode="wb", quiet=T)
}


