
#' List all files with a certain extension from a URL
#' @importFrom magrittr %>%
#' @param url string of URL
#' @param extension string of file extension required (without the dot). To get all files set to \code{NULL}.
#' @param url_convert boolean. If \code{TRUE} then relative URLs are converted to absolute URLs
#' @examples
#' find_files("http://www.abc.net.au/", "htm")
#' aec_website <- "http://www.aec.gov.au/Electorates/gis/gis_datadownload.htm"
#' find_files(aec_website, "zip")
#' \dontrun{
#' find_files(aec_website, "zip") %>% .[c(1,2)] %>% download.file(destfile = basename(.))
#' }
#' @return Character vector of URLs
#' @export
find_files <- function(url, extension, url_convert = TRUE){
  pg <- xml2::read_html(url)
  fl <- pg %>% rvest::html_nodes("a") %>% rvest::html_attr("href")

  if(!is.null(extension)){
    file_url <- fl[which(tools::file_ext(fl) == extension)]
  } else {
    file_url <- fl
  }

  attr(file_url, "base") = url

  if(url_convert){
    file_url <- url_convert(file_url)
  }

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
      if(tools::file_ext(base) == ""){
        full_domain <- base
      } else {
        full_domain <- paste(dirname(base), "/", sep = "")
      }
      output_url[i] <- xml2::url_absolute(url[i], full_domain)
    }
  }

  return(output_url)
}


