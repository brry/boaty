# Boaty MC Boatface 
# Name listing by Berry Boessenkool, berry-b@gmx.de, 2019-01-13

pack <- function(p) if(!requireNamespace(p, quietly=TRUE)) install.packages(p)
pack("berryFunctions")
pack("stringr")
pack("openxlsx")


# NERC Name our Ship Website is down, using the latest copy in the Wayback Machine

b_url <- "https://web.archive.org/web/20180217185619/https://nameourship.nerc.ac.uk/entries.html"
download.file(b_url, destfile="boaty.html") # 2.4 MB

boaty <- readLines("boaty.html")
grep(pattern="boatface", x=boaty) # 366
nchar(boaty[366]) # 2401270
boaty <- strsplit(boaty[366], "{'title': ", fixed=TRUE)[[1]]
length(boaty) # 7035
head(boaty)
tail(boaty)
boaty <- boaty[-1]

# can we use "|" as a column separator later on?
grep("|", boaty, fixed=T) # a single instance in 1519, so yes, we can.
boaty <- sub("|", "_", boaty, fixed=TRUE) # replace with underscore


boaty_df <- strsplit(boaty, '\",')
table(sapply(boaty_df, length)) # all have 7 elements
boaty_df <- berryFunctions::l2df(boaty_df)
colnames(boaty_df) <- c("Name","Author","TwitterAccount","Category","LLI","Description","ImageURL")
boaty_df$Name           <- gsub("\"", "", boaty_df$Name)
boaty_df$Author         <- gsub("'author': \"", "", boaty_df$Author)
boaty_df$TwitterAccount <- gsub("'twitterAccount': \"", "", boaty_df$TwitterAccount)
boaty_df$Category       <- gsub("'category': \"", "", boaty_df$Category)
boaty_df$Description    <- gsub("'description': \"", "", boaty_df$Description)
boaty_df$ImageURL       <- gsub("'imageUrl': '", "", boaty_df$ImageURL)
boaty_df$ImageURL       <- gsub("'},", "", boaty_df$ImageURL, fixed=TRUE)

LLI <- berryFunctions::l2df(strsplit(boaty_df$LLI, ","))
LLI$V1 <- gsub("'likes': ",      "", LLI$V1)
LLI$V2 <- gsub("'likesToday': ", "", LLI$V2)
LLI$V3 <- gsub("'id': \"",       "", LLI$V3)
colnames(LLI) <- c("Likes", "LikesToday", "ID")
LLI$Likes <- as.numeric(LLI$Likes)
LLI$LikesToday <- as.numeric(LLI$LikesToday)

boaty_df <- cbind(boaty_df,LLI)
rm(LLI)
sum(boaty_df$ImageURL != boaty_df$ID) # 1
boaty_df[boaty_df$ImageURL != boaty_df$ID,] # the last imageURL has additional signs

sum(boaty_df$Likes!=boaty_df$LikesToday) # 11
boaty_df[boaty_df$Likes!=boaty_df$LikesToday,c(1,8,9)]


boaty_df <- boaty_df[,c(1,8,4,10,2,3,6)]
boaty_df <- berryFunctions::sortDF(boaty_df, "Likes")
rownames(boaty_df) <- NULL

boaty_df[grepl("\\u", boaty_df$Name, fixed=TRUE), 1:2] # 47 unicode occurences
# MOToeRBOAT (2192) and CiRDAN (22) are the only with >20 votes

berryFunctions::logHist(boaty_df$Likes, breaks=20)
sum(boaty_df$Likes>100)

boaty_write <- boaty_df
boaty_write$Author <- paste(boaty_write$Author, boaty_write$TwitterAccount)
boaty_write$TwitterAccount <- NULL
cn <- "Name"    ;boaty_write[,cn] <- stringr::str_pad(boaty_write[,cn], width=30, side="right") # max(nchar(boaty_write[,cn]))
cn <- "Category";boaty_write[,cn] <- stringr::str_pad(boaty_write[,cn], width= 5, side="right")
cn <- "Author"  ;boaty_write[,cn] <- stringr::str_pad(boaty_write[,cn], width=21, side="right")
cn <- "Likes"   ;boaty_write[,cn] <- stringr::str_pad(boaty_write[,cn], width= 6, side="left")
write.table(boaty_write, file="Boaty.txt", quote=FALSE, row.names=FALSE, sep="|")

openxlsx::write.xlsx(boaty_df, "Boaty.xlsx")


