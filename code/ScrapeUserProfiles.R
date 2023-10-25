# Scrape Google Scholar Profiles by institution
## https://www.whatismybrowser.com/detect/what-is-my-user-agent/
# user.agent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:101.0) Gecko/20100101 Firefox/101.0"
# library(dplyr)
# tnc <- google_scholar_profiles(university_name="The Nature Conservancy")
#   tnc <- data.frame(name = unlist(tnc$profile_name), 
#                     affiliation = unlist(tnc$profile_affiliations),
#                     profile = unlist(tnc$profile_link))
# audubon <- google_scholar_profiles("Audubon")
#   audubon <- data.frame(name = unlist(audubon$profile_name), 
#                     affiliation = unlist(audubon$profile_affiliations),
#                     profile = unlist(audubon$profile_link))
# 					
#*********************************************
google_scholar_profiles <- function(university_name, label=NULL, agent = NULL) {
  #c("httr", "rvest", "jsonlite", "purrr", "stringr", "glue","dplyr")
  if(is.null(agent)){
    headers <- c("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/113.0.0.0 Safari/537.36")
  } else {
    headers = agent
  }
  
  university_name <- trimws(university_name)
  if(!is.null(label)) {
    label <- trimws(label)
    params <- list(
      view_op = "search_authors",
      mauthors = glue::glue("label:{label} '{university_name}'"),
      hl = "en",
      astart = 0
    )
  } else {
    params <- list(
      view_op = "search_authors",
      mauthors = glue::glue("'{university_name}'"),
      hl = "en",
      astart = 0
    )  
  }
  all_profile_results <- list()
  profiles_is_present <- TRUE
    while (profiles_is_present) {
      response <- httr::GET("https://scholar.google.com/citations", query = params, httr::add_headers(.headers = headers))
      page <- xml2::read_html(httr::content(response, "text"))
      print(paste0("extracting authors at page #", params$astart))
      profiles <- page %>% 
	    rvest::html_elements(".gs_ai_chpr")
      profile_results <- purrr::map(profiles, function(profile) {
        name <- profile %>% 
		  rvest::html_element(".gs_ai_name a") %>% 
		    rvest::html_text()
        link <- paste0("https://scholar.google.com", profile %>% 
		  rvest::html_element(".gs_ai_name a") %>% 
		    rvest::html_attr("href"))
        affiliations <- profile %>% 
		  rvest::html_element(".gs_ai_aff") %>% 
		    rvest::html_text(trim = TRUE)
        email <- profile %>% 
		  rvest::html_element(".gs_ai_eml") %>% 
		    rvest::html_text()
        cited_by <- profile %>% 
		  rvest::html_element(".gs_ai_cby") %>% 
		    rvest::html_text() %>% 
			  gsub(pattern = "[^0-9]", replacement = "") 
        interests <- profile %>% 
		  rvest::html_elements(".gs_ai_one_int") %>% 
		    rvest::html_text()
        list(
          profile_name = name[[1]],
          profile_link = link[[1]],
          profile_affiliations = affiliations[[1]],
          profile_email = email[[1]],
          profile_city_by_count = cited_by[[1]],
          profile_interests = interests
        )
      })
      all_profile_results <- c(all_profile_results, profile_results)
      next_page_button <- page %>% 
	    rvest::html_elements("button.gs_btnPR") %>% 
		  rvest::html_attr("onclick")
      if(length(next_page_button) == 0) {
        stop("Too many requests so, Google cut you off")	  
      } else if (!is.na(next_page_button)) {
        params$after_author <- stringr::str_match(next_page_button, "after_author\\\\x3d(.*)\\\\x26")[, 2]
        params$astart <- params$astart + 10
      } else {
        profiles_is_present <- FALSE
      }
    }
  all_profile_results <- data.frame(do.call(rbind, all_profile_results), 
                                    stringsAsFactors = FALSE)
  return(all_profile_results)
}
