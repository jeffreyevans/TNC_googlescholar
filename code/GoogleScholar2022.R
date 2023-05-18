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
library(rvest)
setwd("C:/evans/GITS/GoogleScholar/metrics")

usr.url <- "https://scholar.google.com/citations?hl=en&user="

# The Nature Conservancy, Wildlife Conservation Society, World Wildlife Fund, Conservation International, NatureServe, Audubon  
orgs.google <- list( 
    TNC = list(
      "https://scholar.google.com/citations?view_op=view_org&hl=en&org=12990604570217991302",
      "https://scholar.google.com/citations?view_op=view_org&hl=en&org=12990604570217991302&after_author=jEmWAF7i__8J&astart=10",
	  "https://scholar.google.com/citations?view_op=view_org&hl=en&org=12990604570217991302&after_author=smkDAIzr__8J&astart=20",
	  "https://scholar.google.com/citations?view_op=view_org&hl=en&org=12990604570217991302&after_author=JsBkANDy__8J&astart=30",
	  "https://scholar.google.com/citations?view_op=view_org&hl=en&org=12990604570217991302&after_author=51scAO72__8J&astart=40",
	  "https://scholar.google.com/citations?view_op=view_org&hl=en&org=12990604570217991302&after_author=jcCVAOz4__8J&astart=50",
	  "https://scholar.google.com/citations?view_op=view_org&hl=en&org=12990604570217991302&after_author=sPm2AP35__8J&astart=60",
	  "https://scholar.google.com/citations?view_op=view_org&hl=en&org=12990604570217991302&after_author=rOXYAEf7__8J&astart=70",
	  "https://scholar.google.com/citations?view_op=view_org&hl=en&org=12990604570217991302&after_author=dTsYAGL8__8J&astart=80",
	  "https://scholar.google.com/citations?view_op=view_org&hl=en&org=12990604570217991302&after_author=vcuWAOf8__8J&astart=90",
	  "https://scholar.google.com/citations?view_op=view_org&hl=en&org=12990604570217991302&after_author=rNOXABH9__8J&astart=100",
	  "https://scholar.google.com/citations?view_op=view_org&hl=en&org=12990604570217991302&after_author=WuWVAHX9__8J&astart=110",
	  "https://scholar.google.com/citations?view_op=view_org&hl=en&org=12990604570217991302&after_author=TxozAN39__8J&astart=120",
	  "https://scholar.google.com/citations?view_op=view_org&hl=en&org=12990604570217991302&after_author=0tN2ACL-__8J&astart=130",
	  "https://scholar.google.com/citations?view_op=view_org&hl=en&org=12990604570217991302&after_author=87-VAE3-__8J&astart=140",
	  "https://scholar.google.com/citations?view_op=view_org&hl=en&org=12990604570217991302&after_author=GKwDAHj-__8J&astart=150",
	  "https://scholar.google.com/citations?view_op=view_org&hl=en&org=12990604570217991302&after_author=Z2MDAKP-__8J&astart=160",
	  "https://scholar.google.com/citations?view_op=view_org&hl=en&org=12990604570217991302&after_author=mPCPANj-__8J&astart=170",
	  "https://scholar.google.com/citations?view_op=view_org&hl=en&org=12990604570217991302&after_author=PrkLAAH___8J&astart=180",
	  "https://scholar.google.com/citations?view_op=view_org&hl=en&org=12990604570217991302&after_author=2s-VACb___8J&astart=190",
	  "https://scholar.google.com/citations?view_op=view_org&hl=en&org=12990604570217991302&after_author=9qo_AUH___8J&astart=200",
	  "https://scholar.google.com/citations?view_op=view_org&hl=en&org=12990604570217991302&after_author=zGKxAHP___8J&astart=210",
	  "https://scholar.google.com/citations?view_op=view_org&hl=en&org=12990604570217991302&after_author=euqWAI3___8J&astart=220",
	  "https://scholar.google.com/citations?view_op=view_org&hl=en&org=12990604570217991302&after_author=mNvlAKn___8J&astart=230",
	  "https://scholar.google.com/citations?view_op=view_org&hl=en&org=12990604570217991302&after_author=sMzHALr___8J&astart=240",
	  "https://scholar.google.com/citations?view_op=view_org&hl=en&org=12990604570217991302&after_author=BwILAdH___8J&astart=250",
	  "https://scholar.google.com/citations?view_op=view_org&hl=en&org=12990604570217991302&after_author=KgPMAOX___8J&astart=260",
	  "https://scholar.google.com/citations?view_op=view_org&hl=en&org=12990604570217991302&after_author=2fbuAPz___8J&astart=270"),
    WCS = list("https://scholar.google.com/citations?view_op=view_org&hl=en&org=6250747833927551605",
      "https://scholar.google.com/citations?view_op=view_org&hl=en&org=6250747833927551605&after_author=OsPaAFTk__8J&astart=10",
      "http://scholar.google.com/citations?view_op=search_authors&hl=en&mauthors=wcs.org&after_author=SJkeAGD8__8J&astart=20",
      "http://scholar.google.com/citations?view_op=search_authors&hl=en&mauthors=wcs.org&after_author=v91GANL-__8J&astart=30",
	  "http://scholar.google.com/citations?view_op=search_authors&hl=en&mauthors=wcs.org&after_author=KO6cAN7___8J&astart=40",
	  "https://scholar.google.com/citations?view_op=view_org&hl=en&org=6250747833927551605&after_author=g6jsAPP___8J&astart=50",
	  "https://scholar.google.com/citations?view_op=view_org&hl=en&org=6250747833927551605&after_author=g6jsAPP___8J&astart=60",
	 "https://scholar.google.com/citations?view_op=view_org&hl=en&org=6250747833927551605&after_author=g6jsAPP___8J&astart=70"),   
    WWF = list("https://scholar.google.com/citations?view_op=view_org&hl=en&org=14023426901827038419",
               "https://scholar.google.com/citations?view_op=view_org&hl=en&org=14023426901827038419&after_author=-isDAOz8__8J&astart=10"),   
    CI = list("https://scholar.google.com/citations?view_op=view_org&hl=en&org=12483482970504157936",
              "https://scholar.google.com/citations?view_op=view_org&hl=en&org=12483482970504157936&after_author=D75SANz3__8J&astart=10",
	          "https://scholar.google.com/citations?view_op=view_org&hl=en&org=12483482970504157936&after_author=DzSLAHD-__8J&astart=20",
	          "https://scholar.google.com/citations?view_op=view_org&hl=en&org=12483482970504157936&after_author=YgB6AK____8J&astart=30"), 
    NatureServe = list("https://scholar.google.com/citations?hl=en&view_op=search_authors&mauthors=natureserve.org&btnG=%22",
                       "https://scholar.google.com/citations?view_op=search_authors&hl=en&mauthors=natureserve.org&after_author=ZuQHAZ3-__8J&astart=10"),
    Audubon = list("https://scholar.google.com/citations?hl=en&view_op=search_authors&mauthors=audubon.org&btnG=))",
                   "https://scholar.google.com/citations?view_op=search_authors&hl=en&mauthors=audubon.org&after_author=4RYzAYz___8J&astart=10")) 
 
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