
#' List all files with a certain extension from a URL
#' @importFrom magrittr %>%
#' @param url string of URL
#' @param extension string of file extension required (without the dot)
#' @examples
#' find_files("http://www.abc.net.au/", "htm")
#' aec_website <- "http://www.aec.gov.au/Electorates/gis/gis_datadownload.htm"
#' find_files(aec_website, "zip")
#' find_files(aec_website, "zip") %>% url_convert(aec_website)
#' (find_files(aec_website, "zip") %>% url_convert(aec_website))[c(1,2)] %>% download.files()
#' @return Character vector of URLs
#' @export
find_files <- function(url, extension){
  ext_pattern <- paste("*.", extension, sep = "")
  stem <- dirname(url)

  pg <- xml2::read_html(url)
  fl <- pg %>% rvest::html_nodes("a") %>% rvest::html_attr("href")
  vector_for_url<- fl[grep(ext_pattern, fl)]

  real_files <- fl[grep(ext_pattern, fl)]
  vector_for_user <- basename(real_files)

  file_url <- real_files

  return(file_url)
}

#' Convert relative URLs to absolute URLs
#' @export
#' @param url string of URL
#' @param base string of domain URL
#' @examples
#' aec_website <- "http://www.aec.gov.au/Electorates/gis/gis_datadownload.htm"
#' find_files(aec_website, "zip") %>% url_convert(aec_website)
#' @return character vector of absolute URLs
url_convert <- function(url, base){

  htx <- urltools::scheme(url)
  if(is.na(htx) == FALSE){
    output_url <- url
  } else {
    full_domain <- paste(dirname(base), "/", sep = "")
    output_url <- xml2::url_absolute(url, full_domain)
  }
  return(output_url)
}

#' Download multiple files given a list of URLs
#' @export
#' @param url character vector of URLs
#' @param destfile character vector of resulting filenames
#' @return VOID
download.files <- function(url, destfile = basename(url)){
  for(i in 1:length(url)){
    download.file(url[i], destfile[i])
  }
}


