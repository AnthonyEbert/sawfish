
#' List all files with a certain extension from a URL
#' @importFrom magrittr %>%
#' @param url string of URL
#' @param extension string of file extension required (without the dot)
#' @examples
#' find_files("http://www.abc.net.au/", "htm")
#' aec_website <- "http://www.aec.gov.au/Electorates/gis/gis_datadownload.htm"
#' find_files(aec_website, "zip")
#' find_files(aec_website, "zip") %>% url_convert(aec_website)
#' \dontrun{
#' (find_files(aec_website, "zip") %>% url_convert(aec_website))[c(1,2)] %>% download.files()
#' }
#' @return Character vector of URLs
#' @export
find_files <- function(url, extension){
  ext_pattern <- paste("*.", extension, sep = "")
  stem <- dirname(url)

  pg <- xml2::read_html(url)
  fl <- pg %>% rvest::html_nodes("a") %>% rvest::html_attr("href")

  file_url <- fl[grep(ext_pattern, fl)]
  attr(file_url, "base") = url

  return(file_url)
}

#' Convert URLs (relative or absolute) to absolute URLs
#' @export
#' @param url string of URL
#' @param base string of domain URL
#' @examples
#' aec_website <- "http://www.aec.gov.au/Electorates/gis/gis_datadownload.htm"
#' find_files(aec_website, "zip") %>% url_convert(aec_website)
#' find_files("https://catalog.data.gov/dataset?metadata_type=geospatial&_metadata_type_limit=0&q=%22Congressional%20Districts%22", "zip") %>% url_convert()
#' @return character vector of absolute URLs
url_convert <- function(url, base = attr(url, "base")){

  output_url <- rep(NA, length(url))

  for(i in 1:length(url)){
    htx <- urltools::scheme(url[i])
    if(is.na(htx) == FALSE){
      output_url[i] <- url[i]
    } else {
      full_domain <- paste(dirname(base), "/", sep = "")
      output_url[i] <- xml2::url_absolute(url[i], full_domain)
    }
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


