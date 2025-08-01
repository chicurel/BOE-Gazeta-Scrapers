# Scraping Gazeta 
# June 2025

# Load packages and set working directory  -----------------------------------------------------

library(httr)
library(tidyverse)
library(curl)
library(rvest)
library(purrr)
library(xml2)
library(janitor)

# Set own working directory where the output will go
setwd("")

# URL from searching at https://boe.es/buscar/gazeta.php , and then fixing the page range from 0 to 2000 
url <- ""


# Scraper ----------------------------------------------------------

link_html <- url %>% read_html()

titles <- link_html %>%
  xml_find_all("//ul//li[@class='resultado-busqueda']/h4") %>%
  xml_text()

hits <- data.frame(title = titles)

pdf_url <- link_html %>%
  xml_find_all("//li[@class='puntoPDF2'][1]/a") %>%
  xml_attr("href")


pdf_url <- paste("https://www.boe.es",pdf_url, sep = "")


date <- link_html %>%
  xml_find_all("//dl[@class='nosize']/dd[1]") %>%
  xml_text()


reference <- link_html %>%
  xml_find_all("//li[@class='puntoPDF2'][1]/a") %>%
  xml_text()


hits <- data.frame(reference = reference,
                   date = date,
                   title = titles,
                   url = pdf_url
                   )


hits$title <- trimws(hits$title)
hits$date <- as.Date(gsub(".* (\\d{2}/\\d{2}/\\d{4}).*", "\\1", date),format="%d/%m/%Y")
hits$reference <- gsub("PDF \\(Referencia ", "", hits$reference)
hits$reference <- gsub("\\)", "", hits$reference)


# Save it
write_csv(hits, "hits.csv")
