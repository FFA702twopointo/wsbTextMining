
library(tidytext)
library(RJSONIO)
library(tidyverse)
library(tesseract)
library(magick)
library(stringr)
library(igraph)
library(dplyr)
library(stringr)
library(reshape2)
library(readr)
library(ggplot2)
library(textrank)
library(textdata)
library(rvest)
library(fuzzyjoin)
library(topicmodels)

#global tables variables
post <- setNames(data.frame(matrix(ncol = 7, nrow = 0)), c("PKPostId", "FKParentPost","FKThreadID", "UpVote", "Text", "Author", "Awards"))
replyThread <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("PKPostId", "TextWithContext"))
thread <- setNames(data.frame(matrix(ncol = 1, nrow = 0)), c("PKThreadID"))
ticker <- read.csv(file = 'ticker.csv')
commonword <- read.csv(file = 'google-10000-english-usa.txt')
commonword$value <- rep(1,nrow(commonword))

currentPost <- ""

#adapted form
#https://stackoverflow.com/questions/37198364/r-remove-words-with-3-or-more-repeating-letters-using-gsub
rm.repeatLetters <- function(x){
  xvec <- unlist(strsplit(x, " "))
  rmword <- grepl("(\\w)\\1{1, }", xvec)
  return(paste(xvec[!rmword], collapse = " "))
}
#adapted from
#https://stackoverflow.com/questions/35790652/removing-words-featured-in-character-vector-from-string
removeWords <- function(str, stopwords) {
  x <- unlist(strsplit(str, " "))
  paste(x[!x %in% stopwords], collapse = " ")
}

refreshExtract <- function(){
  #We download all the required post from the wsb subreddit in a staging directory
  #To fetch a json representation of any reddit page, it suffice to insert .json at the end of the directory
    download.file("https://www.reddit.com/r/wallstreetbets.json?limit=100", "stagingarea/sublanding.json")
    
    landingPage <- fromJSON("stagingarea/sublanding.json")
    landingPage <- landingPage[['data']][['children']] 
    
    pcunt <- 0
        for (page in landingPage) {
          link <- page[['data']][['permalink']]
          link <- print(paste0("https://www.reddit.com",str_sub(link,1,nchar(link)-1),".json?limit=100"))
          tryCatch(
            {
          download.file(link, paste0("stagingarea/page", pcunt, ".json"))}, error=function(cond){})
          
          pcunt <- pcunt + 1
          
        }
    file.remove("stagingarea/sublanding.json")
}
#recursive function to fetch all replies from a comment to maximum depth
fetchReplies <- function(reply){
  rp_PKPostID <- reply[['data']][['id']]
  rp_FKParentPost  <- str_sub(reply[['data']][['parent_id']], 4)
  rp_UpVote <- reply[['data']][['ups']]
  rp_Text <- reply[['data']][['body']]
  rp_Author <- reply[['data']][['author']]
  rp_Awards <- reply[['data']][['total_awards_received']]
  
  post[nrow(post)+1,] <<- c(rp_PKPostID, rp_FKParentPost, currentPost ,rp_UpVote, rp_Text, rp_Author, rp_Awards)
  
  if(reply[['data']][['replies']] != ""){
    replies <- reply[['data']][['replies']][['data']][['children']]

    for (rooteply in replies[-length(replies)]) {
      fetchReplies(rooteply)
    }
  }
}
#recursive function to reconstruct a thread of replies, allowing an analysis of context
constructreplythread <- function(postN, Text){
  Text <- paste(postN["Text"], Text)
  if (isTRUE(postN["FKParentPost"] != 0)) {
    
    constructreplythread(
                          post[which(post["PKPostId"] == postN[1,"FKParentPost"]),]
                         , Text)
  }else
  {
    return(Text)
  }
  
}

refreshExtract() #Uncomment to refresh, comment out to use existing files

files <- list.files(path="stagingarea/", pattern="*.json", full.names=TRUE, recursive=FALSE)
for (file in files) {
  
    page <- fromJSON(file)
    if (!grepl( "v.redd.it", page[[1]][['data']][['children']][[1]][['data']][['url']] , fixed = TRUE)) {#check if video
          
        
        #Add the mainpost
        mp_PKPostID <- str_sub(page[[1]][['data']][['children']][[1]][['data']][['name']], 4)
        mp_FKParentPost <- 0
        currentPost <<-  str_sub(page[[1]][['data']][['children']][[1]][['data']][['name']], 4)
        mp_UpVote <- page[[1]][['data']][['children']][[1]][['data']][['ups']]
        mp_Author <- page[[1]][['data']][['children']][[1]][['data']][['author']]
        mp_Awards <- page[[1]][['data']][['children']][[1]][['data']][['total_awards_received']]
        
        if (isTRUE(page[[1]][['data']][['children']][[1]][['data']][['selftext']] != "")) {
          mp_Text <- paste(page[[1]][['data']][['children']][[1]][['data']][['title']], 
                           page[[1]][['data']][['children']][[1]][['data']][['selftext']])
          
        }else
        {
          if(grepl( "x.redd.it", page[[1]][['data']][['children']][[1]][['data']][['url']] , fixed = TRUE)){
            img_path <- image_read(page[[1]][['data']][['children']][[1]][['data']][['url']])
            img <- img_path
            
            
            mp_Text <- paste(page[[1]][['data']][['children']][[1]][['data']][['title']], rm.repeatLetters(image_ocr(img)))
          }else{
            mp_Text <- page[[1]][['data']][['children']][[1]][['data']][['title']]
          }
          

        }
        
        
        
        post[nrow(post)+1,] <- c(mp_PKPostID, mp_FKParentPost, currentPost, mp_UpVote, mp_Text, mp_Author, mp_Awards)
        
        thread[nrow(post)+1,] <- c(currentPost)
        #Add the comments
        
        comments <- page[[2]][['data']][['children']]
        for (comment in comments[-length(comments)]) {
          #Add the comment
          cp_PKPostID <- comment[['data']][['id']]
          cp_FKParentPost  <- str_sub(comment[['data']][['parent_id']], 4)
          cp_UpVote <- comment[['data']][['ups']]
          cp_Text <- comment[['data']][['body']]
          cp_Author <- comment[['data']][['author']]
          cp_Awards <- comment[['data']][['total_awards_received']]
          
          post[nrow(post)+1,] <- c(cp_PKPostID, cp_FKParentPost, currentPost, cp_UpVote, cp_Text, cp_Author, cp_Awards)
          #Add the replies, for each root replies in a recursive manner
          if(comment[['data']][['replies']] != ""){
            replies <- comment[['data']][['replies']][['data']][['children']]
            for (rooteply in replies[-length(replies)]) {
              fetchReplies(rooteply)
            }
          }
          
        }
    }
}
column_to_rownames(post, var = "PKPostId")
thread <- thread[complete.cases(thread), ,drop = FALSE]

for (row in 1:nrow(post)) {
  
  replyThread[nrow(replyThread)+1,] <- c(post[row, "PKPostId"], constructreplythread(post[row,], ""))
}
post <- merge(post, replyThread, by = "PKPostId")



#dictionnaire afinn
afinn<-get_sentiments("afinn")
ticker_match <- ticker
ticker_match$Symbol <- tolower(ticker_match$Symbol)
ticker_match$SymbolFK <- ticker_match$Symbol
ticker_matchFiltered <- ticker_match

ticker_matchFiltered <- left_join(x = ticker_matchFiltered, y = commonword, by = c("Symbol" = "word"))%>%
filter(is.na(value))
ticker_matchFiltered <- subset(ticker_matchFiltered, select = -c(value))
ticker_matchFiltered <- filter(ticker_matchFiltered, !Symbol == "amcx")

Post_Words<-post%>%
unnest_tokens(word, TextWithContext)%>%
  select(PKPostId, word)

Reply_Words<-post%>%
  unnest_tokens(word, Text)%>%
  select(PKPostId, word)

Post_Words$word <- tolower(Post_Words$word)

ticker_match$FirstName <- word(ticker_match$Name, 1)
ticker_match$FirstName <- tolower(ticker_match$FirstName)
ticker_match <- subset(ticker_match, select = c(FirstName, SymbolFK))
ticker_match <- anti_join(ticker_match, commonword, by = c("FirstName"="word"))

Post_Words <- left_join(x = Post_Words, y = ticker_matchFiltered, by = c("word" = "Symbol"))%>%
  select(PKPostId, word,SymbolFK)

ticker_match <- semi_join(ticker_match, Post_Words, by = "SymbolFK")

Post_Words <- left_join(x = Post_Words, y = ticker_match, by = c("word" = "FirstName"))
Post_Words$SymbolFK = paste0(Post_Words$Symbol.x, Post_Words$SymbolFK.y)
Post_Words <- Post_Words %>% select(PKPostId, word,SymbolFK)

stop_words <- stop_words
stop_words[nrow(stop_words)+1,] <- list("https", "WSB")
stop_words[nrow(stop_words)+1,] <- list("amp", "WSB")
stop_words[nrow(stop_words)+1,] <- list("png", "WSB")
stop_words[nrow(stop_words)+1,] <- list("free_emotes_pack", "WSB")
stop_words[nrow(stop_words)+1,] <- list("gif", "WSB")
stop_words[nrow(stop_words)+1,] <- list("wwww.reddit.com", "WSB")
stop_words[nrow(stop_words)+1,] <- list("preview.redd.it", "WSB")
stop_words[nrow(stop_words)+1,] <- list("webp", "WSB")
stop_words[nrow(stop_words)+1,] <- list("width", "WSB")
stop_words[nrow(stop_words)+1,] <- list("www.pickmojo.com", "WSB")

Post_Words <- Post_Words %>%
  anti_join(stop_words, by = "word")%>% 
  filter(!str_detect(word, "[0-9]"))

Reply_Words <- Reply_Words %>%
  anti_join(stop_words, by = "word")%>% 
  filter(!str_detect(word, "[0-9]"))

#Inspiré du cours de Paul Alexandre
Post_Sentiments<-left_join(x= Post_Words, y = afinn, by = "word")%>%
  rename(score=value)%>%
filter(!score == "NA" | !SymbolFK == "NA" | !score == "NaN")

Post_Sentiments$Position <- ave(Post_Sentiments$word , Post_Sentiments$PKPostId, FUN = seq_along)

Ticker_Sentiments <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("PKPostId", "SymbolFK", "score"))

for (i in 1:nrow(Post_Sentiments)) {
  x<-i
  if (!Post_Sentiments[x,'SymbolFK'] == "NA") {

    
    PKPostId <- Post_Sentiments[x,'PKPostId']
    SymbolFK <- Post_Sentiments[x,'SymbolFK']
    
    post_subset <- filter(Post_Sentiments, PKPostId == Post_Sentiments[x,'PKPostId'])
    post_subset$distance <- abs(as.numeric(Post_Sentiments[x,'Position']) - as.numeric(post_subset$Position))
    post_subset$w = max(post_subset$distance) - post_subset$distance
    #post_subset$score <- post_subset$score / pmax(as.numeric((post_subset$distance) - 5) , 1)
    
    post_subset <- filter(post_subset, !score == "NA")
    
    Ticker_Sentiments[nrow(Ticker_Sentiments)+1,] <- c(PKPostId, SymbolFK,weighted.mean(post_subset$score, post_subset$w))
  }
}

Ticker_Sentiments <-  Ticker_Sentiments%>%
  filter(!score == "NaN")

#tppic 
#inspiré de https://m-clark.github.io/text-analysis-with-R/topic-modeling.html
Reply_Words <- select(Reply_Words, PKPostId, word)
Post_Words_g <- aggregate(Reply_Words, by=list(Reply_Words$PKPostId, Reply_Words$word), FUN=length);
Post_Words_g <- select(Post_Words_g,Group.1, Group.2, word)
Post_Words_g <- rename(Post_Words_g, document = Group.1,  term = Group.2 , count = word)
documentTermMatrix <-  cast_dtm(Post_Words_g, document, term, count)

wsb_10 <- LDA(documentTermMatrix, k = 10)


wsb_topics <- tidy(wsb_10, matrix = "beta")
wsb_topics

topic_modeling <- wsb_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

topic_modeling %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

wsb_documents <- tidy(wsb_10, matrix = "gamma")
wsb_documents <- wsb_documents %>% group_by(document) %>% slice(which.max(gamma))

post <- left_join(x =  post, y = wsb_documents, by = c("PKPostId" = "document"))


#write all tables to csv
write.csv(post,"bd/post.csv", row.names = FALSE)
write.csv(thread,"bd/thread.csv", row.names = FALSE)
write.csv(ticker,"bd/ticker.csv", row.names = FALSE)

write.csv(topic_modeling,"bd/topic.csv", row.names = FALSE)

write.csv(Post_Sentiments,"bd/sentiment_post.csv", row.names = FALSE)
write.csv(Ticker_Sentiments,"bd/sentiment.csv", row.names = FALSE)
write.csv(afinn,"bd/afinn.csv", row.names = FALSE)

