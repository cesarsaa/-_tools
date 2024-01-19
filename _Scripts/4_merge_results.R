g = gc;rm(list=ls())
# Set directorty 
root <- "D:/OneDrive - CGIAR/Documents/GitHub/XX_tool/"
sl_pth1 <- paste0(root, "_Results/_PDF/")
sl_pth2 <- paste0(root, "_Results/_WEBS/")
# Get the files names
# scrapping, bigram, idf
files = list.files(path=sl_pth1, pattern=paste0("PDF_scrapping_",".*.csv$"), full.names = TRUE); files
files2 = list.files(path=sl_pth2, pattern=paste0("web_scrapping_",".*.csv$"), full.names = TRUE); files2
# First apply read.csv, then rbind
myfiles = do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))
myfiles$X <- NULL
names(myfiles)
names(myfiles)[1] <- "category"
# institution

#
myfiles2 = do.call(rbind, lapply(files2, function(x) read.csv(x, stringsAsFactors = FALSE)))
myfiles2$X <- NULL
names(myfiles2)
#
myfiles$type <- "Website"
myfiles2$type <- "PDF"

df <- rbind(myfiles,myfiles2)
unique(df$word)
# Save CSV merge files
write.csv(df, paste0(root, "_Results/scrapping.csv"))


################################################################################
