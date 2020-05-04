
library(rvest)
library(tidyverse)

page<-read_html("https://cogcc.state.co.us/weblink/results.aspx?id=08306642")

table<-page%>%
  html_node("#WQResultGridView")%>%
  html_table(fill = T)%>%
  as_tibble(.name_repair = "unique")

links<-page%>%
  html_nodes("#WQResultGridView a")%>%
  html_attr("href")%>%
  as_tibble()%>%
  filter(str_detect(value, "DownloadDocumentPDF"))%>%
  rename(url = value)

urls_to_download<-bind_cols(table, links)%>%
  janitor::clean_names()%>%
  filter(document_name %in% c("well log", "abandonment"))%>%
  pull(url)

urls_to_download[4]

download.file(url = paste0("https://cogcc.state.co.us/weblink/",urls_to_download[4]), destfile = "test_download/test_name.tif", mode="wb")
