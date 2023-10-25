#' @title Scrubs Google Scholar for research metrics
#' @description Scrubs Google Scholar and returns members of organization 
#'              and research metrics
#' 
#' @return Data frame with;
#' name - Name of scientists 
#' organization - name of affiliate organization
#' google.id - Google Scholar ID
#' n.citations - Number of citations
#' h.index - maximum value of h such that the given author/journal has published at least 
#'           h papers that have each been cited at least h times 
#' i10.index - number of publications written with at least 10 citations
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org> 
#'
# requires c("httr", "rvest", "jsonlite", "purrr", "stringr", "glue","dplyr")
library(dplyr)
setwd("C:/evans/GITS/TNC_googlescholar")
source(file.path(getwd(), "code", "ScrapeUserProfiles.R"))

# Pulls basic individual profile information by organization
orgs <- c("The Nature Conservancy", 
          "Wildlife Conservation Society", 
          "World Wildlife Fund", 
          #"Conservation International", 
          "NatureServe", 
          "Audubon")  
# profiles <- sapply(orgs, function(x) NULL)

profiles <- lapply(orgs, function(i) {
    org <- google_scholar_profiles(i)
      data.frame(orginization = i, name = unlist(org$profile_name), 
                 affiliation = unlist(org$profile_affiliations),
                 profile = unlist(org$profile_link))  
  })

 
#****************************************************						  
# Affiliation, Google ID and number citations
aff <- c("TNC", "Audubon", "CI", "WWF", "NatureServe", "WCS")
affiliation <- vector(mode = "list", length = length(aff))
  names(affiliation) <- aff
  for(o in aff) {
    cat("pulling metrics for", o, "\n")
    org <- unlist(orgs.google[[o]])	 
    scientists <- vector()
    citations <- vector()
    user.ids <- vector()
      for(j in 1:length(org)) {
           page <- read_html(org[j])
          memb <- page %>% html_nodes(".gs_ai_name a") %>% html_text()
           scientists <- append(scientists, memb)
          cit <- page %>% html_nodes(".gs_ai_chpr") %>% html_text()	  
               cit <- regmatches(cit, gregexpr("[[:digit:]]+", cit))
             cit <- unlist(lapply(cit, function(x) as.numeric(ifelse(length(x) <= 0, 0, x))))
        citations <- append(citations, cit)
        ids <- page %>%
              html_nodes('a') %>% 
                html_attr('href')
             ids <- ids[grep("citations?hl=en&user=", ids, fixed=TRUE)]
           ids <- unique(unlist(lapply(strsplit(ids, "="), function(x) x[3]))) 
      user.ids <- append(user.ids, ids) 
      } 
    # xml2::as_list(page) # list nodes in read_html object 
    metrics <- data.frame(name=scientists, orginization = o, google.id=user.ids,  
                          n.citations=citations) 	
        didx <- which(duplicated(metrics$name))
          if(length(didx) > 0) metrics <- metrics[-didx,] 
    
    #****************************************************	
    # User indices, from their 
    metrics <- data.frame(metrics, h.index=NA, i10.index=NA) 
      for(i in 1:nrow(metrics)) {
        cat("processing", as.character(metrics[i,][1]), i, "of", nrow(metrics), "\n")  
        page <- read_html(paste0(usr.url, metrics$ google.id[i]))
        indices = page %>% 
          html_nodes(".gsc_rsb_std") %>% 
            html_text()
          indices <- as.numeric(indices[c(3,5)])
    	metrics[i,][5:6] <- indices  	
      }
    if(o == "TNC") {
      # Add Katharine Hayhoe
      page <- read_html(paste0(usr.url, "LwiZJosAAAAJ"))
        indices = page %>% 
          html_nodes(".gsc_rsb_std") %>% 
            html_text()
          indices <- as.numeric(indices[c(1,3,5)])	
      	i <- nrow(metrics)+1
        metrics[i,][1:3] <- c("Katharine Hayhoe", "TNC", "LwiZJosAAAAJ")
       metrics[i,][4:6] <- indices  	
    }
	  if(any(metrics$n.citations < 1)) 
	    metrics <- metrics[which(metrics$n.citations > 0),]
      affiliation[[o]] <- metrics
    # write.csv(metrics, paste0(aff,"_science_metrics_2022.csv"), row.names=FALSE)
  }

affiliation <- do.call("rbind", affiliation)
  write.csv(affiliation, "science_metrics_2022.csv", row.names=FALSE)

################# Plot results #################
plot.col <- c("#008B8B", "#00CDCD", "#EE6A50", "#8B7D6B")

pdf("science_metrics_plots_2022.pdf", width=10, height=10)
par(mfrow=c(2,2))
# TNC H-Index
h.index <- as.numeric(affiliation[affiliation$orginization == "TNC",]$h.index) 
d <- density(h.index)
plot(d, type="n", main = paste("TNC Science - H-Index", paste("Median: ", 
	 median(h.index), sep=": "),sep="\n"), 
     xlab="", xlim=c(0,100)) 
  polygon(d, col=plot.col[1])
  abline(v=median(h.index), lty = 3)
  legend("topright", legend="Median H-Index", lty = 3, col="black")
# WWF H-Index
h.index <- as.numeric(affiliation[affiliation$orginization == "WWF",]$h.index) 
d <- density(h.index)
plot(d, type="n", main = paste("WWF Science - H-Index", paste("Median: ", 
	 median(h.index), sep=": "),sep="\n"), 
     xlab="", xlim=c(0,100)) 
  polygon(d, col=plot.col[2])
  abline(v=median(h.index), lty = 3)
  legend("topright", legend="Median H-Index", lty = 3, col="black")  
# WCS H-Index
h.index <- as.numeric(affiliation[affiliation$orginization == "WCS",]$h.index) 
d <- density(h.index)
plot(d, type="n", main = paste("WCS Science - H-Index", paste("Median: ", 
	 median(h.index), sep=": "),sep="\n"), 
     xlab="", xlim=c(0,100)) 
  polygon(d, col=plot.col[3])
  abline(v=median(h.index), lty = 3)
  legend("topright", legend="Median H-Index", lty = 3, col="black")    
# CI H-Index
h.index <- as.numeric(affiliation[affiliation$orginization == "CI",]$h.index) 
d <- density(h.index)
plot(d, type="n", main = paste("CI Science - H-Index", paste("Median: ", 
	 median(h.index), sep=": "),sep="\n"), 
     xlab="", xlim=c(0,100)) 
  polygon(d, col=plot.col[4])
  abline(v=median(h.index), lty = 3)
  legend("topright", legend="Median H-Index", lty = 3, col="black")    

par(mfrow=c(2,2))
# TNC i10-Index
i10.index <- affiliation[affiliation$orginization == "TNC",]$i10.index 
d <- density(i10.index)
plot(d, type="n", main = paste("TNC Science - i10-Index", paste("Median: ", 
	 median(i10.index), sep=": "),sep="\n"), 
     xlab="", xlim=c(0,100)) 
  polygon(d, col=plot.col[1])
  abline(v=median(i10.index), lty = 3)
  legend("topright", legend="Median i10-Index", lty = 3, col="black")
# WWF i10-Index
i10.index <- affiliation[affiliation$orginization == "WWF",]$i10.index 
d <- density(i10.index)
plot(d, type="n", main = paste("WWF Science - i10-Index", paste("Median: ", 
	 median(i10.index), sep=": "),sep="\n"), 
     xlab="", xlim=c(0,100)) 
  polygon(d, col=plot.col[2])
  abline(v=median(i10.index), lty = 3)
  legend("topright", legend="Median i10-Index", lty = 3, col="black")  
# WCS i10-Index
i10.index <- affiliation[affiliation$orginization == "WCS",]$i10.index 
d <- density(i10.index)
plot(d, type="n", main = paste("WCS Science - i10-Index", paste("Median: ", 
	 median(i10.index), sep=": "),sep="\n"), 
     xlab="", xlim=c(0,100)) 
  polygon(d, col=plot.col[3])
  abline(v=median(i10.index), lty = 3)
  legend("topright", legend="Median i10-Index", lty = 3, col="black")    
# CI i10-Index
i10.index <- affiliation[affiliation$orginization == "CI",]$i10.index 
d <- density(i10.index)
plot(d, type="n", main = paste("CI Science - i10-Index", paste("Median: ", 
	 median(i10.index), sep=": "),sep="\n"), 
     xlab="", xlim=c(0,100)) 
  polygon(d, col=plot.col[4])
  abline(v=median(i10.index), lty = 3)
  legend("topright", legend="Median i10-Index", lty = 3, col="black")   

par(mfrow=c(2,2))
# TNC Number of Citations
n.citations <- affiliation[affiliation$orginization == "TNC",]$n.citations 
d <- density(n.citations)
plot(d, type="n", main = paste("TNC Science - Number of Citations", paste("Median: ", 
	 median(n.citations), sep=": "),sep="\n"), 
     xlab="", xlim=c(0,20000)) 
  polygon(d, col=plot.col[1])
  abline(v=median(n.citations), lty = 3)
  legend("topright", legend="Median Number of Citations", lty = 3, col="black")
# WWF Number of Citations
n.citations <- affiliation[affiliation$orginization == "WWF",]$n.citations 
d <- density(n.citations)
plot(d, type="n", main = paste("WWF Science - Number of Citations", paste("Median: ", 
	 median(n.citations), sep=": "),sep="\n"), 
     xlab="", xlim=c(0,20000)) 
  polygon(d, col=plot.col[2])
  abline(v=median(n.citations), lty = 3)
  legend("topright", legend="Median Number of Citations", lty = 3, col="black")  
# WCS Number of Citations
n.citations <- affiliation[affiliation$orginization == "WCS",]$n.citations 
d <- density(n.citations)
plot(d, type="n", main = paste("WCS Science - Number of Citations", paste("Median: ", 
	 median(n.citations), sep=": "),sep="\n"), 
     xlab="", xlim=c(0,20000)) 
  polygon(d, col=plot.col[3])
  abline(v=median(n.citations), lty = 3)
  legend("topright", legend="Median Number of Citations", lty = 3, col="black")    
# CI Number of Citations
n.citations <- affiliation[affiliation$orginization == "CI",]$n.citations 
d <- density(n.citations)
plot(d, type="n", main = paste("CI Science - Number of Citations", paste("Median: ", 
	 median(n.citations), sep=": "),sep="\n"), 
     xlab="", xlim=c(0,20000)) 
  polygon(d, col=plot.col[4])
  abline(v=median(n.citations), lty = 3)
  legend("topright", legend="Median Number of Citations", lty = 3, col="black")
dev.off()

save.image("NGO_GoogleScholarMetrics.RData")