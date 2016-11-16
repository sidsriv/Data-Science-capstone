## Quiz 1

## Q1

blogs_info <- file.info("en_US.blogs.txt")
size_MB <- (blogs_info$size)/1000000

## Q2

twitter_input <- file("en_US.twitter.txt", "r")
twitter <- readLines(twitter_input, skipNul=TRUE)
length(twitter)
close(twitter_input)

## Q3

INPUT <- paste0(c("en_US.blogs.txt","en_US.news.txt","en_US.twitter.txt"))
for(line in INPUT) {
  print(line)
  con <- file(line,"r")
  texts <- readLines(con, n=-1, skipNul=TRUE)
  close(con)
  print(max(sapply(texts,nchar)))
  print(length(texts))
}

## Q4

twitter_input <- "en_US.twitter.txt"
con <- file(twitter_input,"r")
tweets <- readLines(twitter_input, n=-1, skipNul=TRUE)
love <- length(grep("love",tweets))
hate <- length(grep("hate",tweets))
theThinLine <- love/hate
close(con)

## Q5

twitter_input <- "en_US.twitter.txt"
con <- file(twitter_input,"r")
tweets <- readLines(twitter_input, n=-1, skipNul=TRUE)
biostatsTweet <- tweets[grep("biostats",tweets)]
close(con)

## Q6

twitter_input <- "en_US.twitter.txt"
con <- file(twitter_input,"r")
tweets <- readLines(twitter_input, n=-1, skipNul=TRUE)
tweet <- tweets[grep("A computer once beat me at chess, but it was no match for me at kickboxing",tweets)]
length(tweet)
close(con)