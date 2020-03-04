# install packages and load libraries
pacman::p_load(tidyverse, readr, skimr, qdap, sentimentr, tm, wordcloud, magrittr, tidytext, radarchart,
               textdata, recosystem, dplyr, tidyr, reshape2, compiler, Matrix, lexicon, textcat, data.table,
               softImpute)


# set working directory
setwd("C:/Users/Shenc/Documents/NUS EBAC/EBA5002/CA2 - Recommender Project/Data")
# setwd('C:/Users/DELL/Desktop')

# load data
listings <- read_csv("listings.csv")
reviews <- read_csv("reviews.csv")

# view data
skim(listings)
skim(reviews)

################################1. Data Cleaning #########################################

# 1.1 Review Data =======================================================================

# select useful columns
reviews_select <- reviews[,c(1,2,4,6)]
head(reviews_select)
dim(reviews_select)

skim(reviews_select)

# 1.2 Clean Data ========================================================================

# reviews.csv
# issue 1: 73 missing value in "comments" column
# drop rows with missing values
reviews_clean <- na.omit(reviews_select)
dim(reviews_clean)
skim(reviews_clean)

# issue 2: comments in foreign language  (non-english alphabets)
# drop non-ASCII
reviews_clean <- reviews_clean[which(!grepl("[^\x01-\x7F]+", reviews_clean$comments)),]
# drop non-unicode
reviews_clean <- reviews_clean[which(!grepl("[^\u0001-\u007F]+", reviews_clean$comments)),]
dim(reviews_clean)

# issue 3: comments in foreign language (english alphabets)
reviews_clean$Languages <- textcat(reviews_clean$comments) #recognize the language of each comments
reviews_clean <-reviews_clean %>% 
  filter(Languages == "english")  #extract comments in english

# issue 4: system generated comment for cancelled reservation
# drop all rows with such comment
reviews_clean <- reviews_clean %>% 
  filter(!str_detect(comments, "an automated posting"))
dim(reviews_clean)


# print cleaned data to excel
# write.csv(reviews_clean, file = "reviews_cleaned.csv",row.names=FALSE)

################################2. Exploratory Analysis  #################################

# 2.1 Reviews Data ======================================================================

# Distribution of reviews overtime
reviews_per_yr <- reviews %>% mutate(year = lubridate::year(date)) %>% #create a column containing year from the date of review
  group_by(year, listing_id) %>%
  count(comments, listing_id, year) %>%
  summarize(count = sum(n)) #count total reviews per year

ggplot(reviews_per_yr, aes(x = year, y = count)) +
  geom_col() +
  labs(x = "Year", y = "No.of reviews", 
       title = "Number of reviews per year") +
  scale_x_continuous(breaks = 0:2100) +
  theme_classic()

# Revies per listing
reviews_per_lisiting <- reviews_clean %>% 
  group_by(listing_id) %>%
  count(comments) %>%
  summarise(count = sum(n)) %>%
  arrange(desc(count)) %>%
  mutate(index = row_number())

ggplot(reviews_per_lisiting, aes(x = index, y = count)) +
  labs(x = "Listing index", y = "No.of reviews", 
       title = "Number of reviews per listing") +
  geom_point(alpha = 0.6, color = "salmon") +
  theme_classic()


################################3. Predict user ratings  #################################

# 3.1 Polarity scoring ==================================================================

# calculate polarity score
# polarity() - Scoring with positive or negative values only (same as "bing" in tidytext pkg)
rating_pol <- polarity(reviews_clean$comments)

# review object
rating_pol
summary(rating_pol$all$polarity)

# add polarity score column to df
#reviews_clean$rating_pol <- rating_pol$all$polarity
#summary(reviews_clean)

# get counts
pos_counts <- counts(rating_pol)
n_good <- length(unlist(pos_counts$pos.words)) # number of positive words
n_bad <- length(unlist(pos_counts$neg.words)) # number of negative words
n_words <- sum(pos_counts$wc) # Total number of words

gsub("[special_string]", "", c)

# 3.2 Polarity scoring visualization ====================================================

# Plot polarity all element
ggplot(rating_pol$all, aes(x = polarity, y = ..density..)) + 
  geom_histogram(binwidth = 0.25, fill = "#F08080", colour = "grey60") +
  geom_density(size = 0.75) +
  labs(x = "Polarity scoring", y = "Density", 
       title = "Overall review sentiments") +
  theme_classic() 

# using polarity on a comparison cloud
# custom function to extract postive and negative words
pol_subsections <- function(df) {
  x.pos <- subset(df$text, df$polarity > 0)
  x.neg <- subset(df$text, df$polarity < 0)
  x.pos <- paste(x.pos, collapse = " ")
  x.neg <- paste(x.neg, collapse = " ")
  all.terms <- c(x.pos, x.neg)
  return(all.terms)
}

# create tdm
all_tdm  <- rating_pol$all %>% 
  select(text = text.var, polarity = polarity) %>%  #select text.var as text and polarity
  pol_subsections() %>% #apply custom function pol_subsections()
  VectorSource() %>% #source from a vector
  VCorpus() %>% # ake a volatile corpus 
  TermDocumentMatrix(#create TDM from corpus
    control = list(removePunctuation = T, #remove the punctuation
                   stopwords = c(stopwords(kind = "en"), "day", "thanks","highly","well",
                                 "host","great", "place","stay","singapore","really","everything",
                                 "recommend","super","enjoyed","wonderful","amazing", "beautiful",
                                 "recommended","excellent","located","thank","also","easy","definitely",
                                 "good","perfect","lovely","dont","didnt","one","bad","airbnb",
                                 "loved","staying","away","will","best","love","told",
                                 "never","said","near","fantastic","visit","area", "another",
                                 "booked","booking","however", "stop","alaways","home","right",
                                 "house"))) %>% #use English stopwords
  as.matrix() %>% #Convert to matrix
  set_colnames(c("positive", "negative")) #set column names

# plot comparison cloud
comparison.cloud(all_tdm, max.words = 50, colors = c("darkgreen", "darkred"))

# Plutchik's wheel of emotions using NRC
nrc <- get_sentiments("nrc")
tidy_reviews <- reviews_clean %>% 
  unnest_tokens(word, comments) %>%
  count(reviewer_id,word) %>%
  rename(total_words = n) %>%
  inner_join(nrc, by = c("word" = "word")) %>% 
  filter(!grepl("positive|negative", sentiment)) %>% 
  count(sentiment) 

# plot radar chart
chartJSRadar(tidy_reviews, 
             width = 1,
             height = 1,
             showLegend = FALSE)

# author effort
tidy_reviews1 <- reviews_clean %>% 
  unnest_tokens(word, comments) %>% 
  group_by(reviewer_id) %>% 
  mutate(original_word_order = seq_along(word))

# Calculate polarity for each review
bing <- get_sentiments("bing")

pos_neg <- tidy_reviews1 %>% 
  inner_join(bing) %>%
  count(sentiment) %>%
  spread(sentiment, n, fill = 0) %>% 
  mutate(polarity = positive - negative)

pos_neg_pol <- tidy_reviews1 %>% 
  count(reviewer_id) %>% 
  inner_join(pos_neg) %>% 
  mutate(pol = ifelse(polarity >= 0, "Positive", "Negative"))

# Plot n vs. polarity, colored by pol
ggplot(pos_neg_pol, aes(x = polarity, y=n, color = pol)) + 
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Polarity score", y = "Length of reviews",
       title = "Relationship between word effort & polarity") +
  theme_classic() 


# rating1 have been created
# write.csv(reviewres, file = "reviews_rating1.csv",row.names=FALSE)

#reviewNA = subset(reviewres,rating1q == "NaN")
subset(reviews_clean,rating1q >5.0)
subset(reviews_clean,rating1q <(-1))

# 3.3 Sentimentr ========================================================================

# update dictionary
# updated_dict <- update_polarity_table(hash_sentiment_jockers_rinker, 
#                                       x=data.frame(words=c("instantly", "incredibly", "significant",
#                                                            "boom", "booming","indebted", "evocative",
#                                                            "tempting", "immense", "unabashed",
#                                                            "formidable", "stunned", "unreal",
#                                                            "unrestricted", "indulgence", "extravagant",
#                                                            "zealous", "fanciful", "simplistic", 
#                                                            "eccentric", "servere","fainthearted", 
#                                                            "heartbreaking","decadence"), 
#                                                    polarity=c(rep(1, times=19), rep(-5, time=5)), stringAsFactors =FALSE))

# sentimentr
# sentiment <- sentiment_by(reviews_clean$comments, polarity_dt = lexicon::hash_sentiment_huliu)
sentiment_jr <- sentiment_by(reviews_clean$comments)

# summary(reviewres$comments)
summary(sentiment$ave_sentiment)

# see the distribution of the ratings
qplot(sentiment$ave_sentiment, geom="histogram", binwidth = 0.1, main = "Review Sentiment Histogram")

# get counts
# sentiment_words <- extract_sentiment_terms(reviews_clean$comments, polarity_dt = lexicon::hash_sentiment_huliu)
# sentiment_count <- attributes(sentiment_words)$counts
# n_good_senr <- sentiment_count %>%
#   filter(polarity > 0) %>%
#   summarize(npos = sum(n))
# n_bad_senr <- sentiment_count %>%
#   filter(polarity < 0) %>%
#   summarize(nneg = sum(n))
# n_words_senr <- sentiment_count %>%
#   summarize(nwords = sum(n))

sentiment_words_jr <- extract_sentiment_terms(reviews_clean$comments)
sentiment_count_jr <- attributes(sentiment_words_jr)$counts
n_good_senr_jr <- sentiment_count_jr %>%
  filter(polarity > 0) %>%
  summarize(npos = sum(n))
n_bad_senr_jr <- sentiment_count_jr %>%
  filter(polarity < 0) %>%
  summarize(nneg = sum(n))
n_words_senr_jr <- sentiment_count_jr %>%
  summarize(nwords = sum(n))

# 3.4 Comparison ========================================================================

# compare words list
senhl_neg <- unlist(sentiment_words$negative)
senhl_pos <- unlist(sentiment_words$positive)
senjr_neg <- unlist(sentiment_words_jr$negative)
senjr_pos <- unlist(sentiment_words_jr$positive)
qdap_neg <- unlist(pos_counts$neg.words)
qdap_pos <- unlist(pos_counts$pos.words)

# count words
# qdap includes "-" as a word
length(unique(senhl_neg))
length(unique(senhl_pos))
length(unique(senjr_neg))
length(unique(senjr_pos))
length(unique(qdap_neg))
length(unique(qdap_pos))

#write.csv(senhl_neg, file = "senhl_neg.csv",row.names=FALSE)
#write.csv(senhl_pos, file = "senhl_pos.csv",row.names=FALSE)
#write.csv(senjr_neg, file = "senjr_neg.csv",row.names=FALSE)
#write.csv(senjr_pos, file = "senjr_pos.csv",row.names=FALSE)
#write.csv(qdap_neg, file = "qdap_neg.csv",row.names=FALSE)
#write.csv(qdap_pos, file = "qdap_pos.csv",row.names=FALSE)

# 3.5 Normalize ratings =================================================================

# add ratings columns to df
reviews_clean$ratings <- sentiment_jr$ave_sentiment

# normalise rating scores
normalize <- function(x) {
  return (1+2*(x +1))
}
dfNorm <- as.data.frame(lapply(reviews_clean$ratings, normalize))
head(dfNorm)
reviews_clean$ratings_nm <- 2*(reviews_clean$ratings +1)+1

dim(reviews_clean)
# print polarity rating data to csv
# final_rating = reviews_clean[,c(1,2,3,6)]
# write.csv(final_rating, file = "reviews_rating.csv",row.names=FALSE)

# 3.6 Data reduction ====================================================================

# duplications: numbers of customers who have rated the same products
nrow(reviews_clean) - nrow(distinct(reviews_clean[c("reviewer_id", "listing_id", "ratings_nm")], listing_id, 
                                    reviewer_id))

# Dedup with mean function
reviews_aggre <- aggregate(data = reviews_clean, ratings_nm ~ reviewer_id + listing_id, FUN = mean)
dim(reviews_aggre)

# eliminate users with too few ratings
cnts = aggregate(listing_id ~ reviewer_id, data = reviews_aggre, FUN=length)
colnames(cnts) = c("active_user","num_reviews") 
activeusers = cnts$active_user[cnts$num_reviews >= 2] ; length(activeusers)
reviews_reduced = reviews_aggre[reviews_aggre$reviewer_id %in% activeusers,]
dim(reviews_reduced)

# eliminate listings with too few reviews 
cnts = aggregate(reviewer_id ~ listing_id, data = reviews_clean, FUN=length)
colnames(cnts) = c("listing","num_users") 
popularhouses = cnts$listing[cnts$num_users >= 2] ; length(popularhouses)
reviews_reduced = reviews_reduced[reviews_reduced$listing_id %in% popularhouses,]
dim(reviews_reduced)

# convert to tabular format
users = acast(reviews_reduced, reviewer_id ~ listing_id, value.var = "ratings_nm")
dim(users) # 3635 2425
head(users)
str(users)

# 3.7 Custom functions for rating =======================================================

# Make recommendations for the target user using User-based CF
getrecommendations_UU <- function(targetuser, users, topN=5, simfun=pearsonsim) {
  sims = apply(users,1,function(user) simfun(user,targetuser)) 
  sims = sims[!is.na(sims) & sims >=0]
  wavrats = apply(users[names(sims),is.na(targetuser), drop=FALSE],2,function(rats) weighted.mean(rats, sims, na.rm=TRUE))
  s = sort(wavrats[!is.na(wavrats)], decreasing = TRUE)
  if (topN == FALSE) s else s[1:min(topN,length(s))] # get topN items
}
getrecommendations_UU = cmpfun(getrecommendations_UU)

# get recommedations for the target user using Item-based CF
getrecommendations_II <- function(targetuser, itemsims, topN=5) {
  targetuser = targetuser[colnames(itemsims)] # ensure the item order is the same as simmatrix
  seenitems  = !is.na(targetuser)
  unseenitems = is.na(targetuser)
  seenrats = targetuser[seenitems]
  preds = apply(itemsims[unseenitems,seenitems, drop=FALSE], 1, function(simrow) my.weighted.mean(seenrats, simrow))
  sp = sort(preds[!is.na(preds)] , decreasing = TRUE)
  sp[1:min(topN,length(sp))]  # get topN items
}
getrecommendations_II = cmpfun(getrecommendations_II)

# computes average, mean absolute error
# each row contains prediction, actual, prediction, actual etc, hence errors are just the diff between consecutive cells
avgMAE = function(preds) {
  plist = unlist(preds)
  errors = sapply(1:(length(plist)/2),function(i) abs(plist[i*2-1]-plist[i*2]))
  errors = errors[errors != Inf]
  mean(errors,na.rm=TRUE)
}

showCM = function(preds, like) {
  plist = unlist(preds)
  cnts = sapply(1:(length(plist)/2), function(i) {
    pred = plist[i*2-1] ; actual = plist[i*2]
    if (!is.na(pred) & !is.nan(actual)) {
      if (pred>=like) {if(actual>=like) c(1,0,0,0) else c(0,1,0,0)}
      else if(actual<like) c(0,0,1,0) else c(0,0,0,1) 
    } else c(0,0,0,0)
  })
  s = rowSums(cnts)   #returns cnts for: TP, FP, TN, FN
  
  cat(sprintf("TN=%5d FP=%5d\n",s[3],s[2]))
  cat(sprintf("FN=%5d TP=%5d  (total=%d)\n",s[4],s[1], sum(s)))
  cat(sprintf("accuracy  = %0.1f%%\n",(s[1]+s[3])*100/sum(s)))
  cat(sprintf("precision = %3.1f%%\n",s[1]*100/(s[1]+s[2])))
  cat(sprintf("recall    = %3.1f%%\n",s[1]*100/(s[1]+s[4])))
}

# compute the item-item similarity matrix (the matrix is symmetric so can compute half & then copy)
# (setting dir=1 generates the user similarity matrix)
getitemsimsmatrix = function(users, simfun=cosinesim, dir=2) {
  rw <<- 1; 
  itemsims = apply(users, dir, function(itemA) {
    rw <<- rw + 1 ; cl <<- 1; 
    apply(users,dir,function(itemB) {cl<<-cl+1; if (cl<rw) NA else if (cl==rw) NA else simfun(itemA,itemB)})
  })
  m = Matrix::forceSymmetric(itemsims,uplo="L") # copy lower half to upper half
  as.matrix(m)
}

getitemsimsmatrix = cmpfun(getitemsimsmatrix)

# similarity functions
euclidsim = function(x,y) { z=(y-x)^2; sz=sqrt(sum(z,na.rm=TRUE));
if (sz!=0) 1/(1+sz) else if (length(which(!is.na(z)))==0) NA else 1/(1+sz)}

cosinesim = function(x,y) { xy = x*y; sum(xy, na.rm=TRUE)/(sqrt(sum(x[!is.na(xy)]^2)*sum(y[!is.na(xy)]^2)))}

pearsonsim= function(x,y) { suppressWarnings(cor(unlist(x),unlist(y),use="pairwise.complete.obs")) }

pearsonRM = function(x,y) { mx=mean(x,na.rm=TRUE);my=mean(y,na.rm=TRUE);
xy=x*y;x=x[!is.na(xy)]; y=y[!is.na(xy)]
sum((x-mx)*(y-my))/(sqrt(sum((x-mx)^2)*sum((y-my)^2)))}

jacardsim = function(x,y) { validx= !is.na(x); validy= !is.na(y); 
sum(as.integer(validx&validy))/sum(as.integer(validx|validy))}

# make predicted ratings for a sample of items for each test user
predictCF = function(testusers, trainusers=NULL, itemsims=NULL, numtestitems=10, random=FALSE, simfun=cosinesim) {
  preds = sapply(1:nrow(testusers),function(i) {
    cat(".")
    predictuser(testusers[i,],trainusers=trainusers,itemsims=itemsims,numtestitems=numtestitems,random=random,simfun=simfun)})
  colnames(preds) = rownames(testusers)
  preds
}

predictuser <- function(testuser, trainusers=NULL, itemsims=NULL, numtestitems=10, random=FALSE, simfun=cosinesim) {
  seenitemnames   = names(testuser)[!is.na(testuser)]
  if (random) testitemnames = sample(seenitemnames,min(numtestitems,length(seenitemnames))) # test a random N items
  else testitemnames = seenitemnames[1:min(numtestitems,length(seenitemnames))] # test first N items
  preds = list()
  for (testitemname in testitemnames) {
    truerating = testuser[testitemname] 
    testuser[testitemname] = NA
    if (!is.null(trainusers)) {
      # do user-based CF
      usersims = apply(trainusers,1,function(trainuser) simfun(trainuser,testuser))
      usersims = usersims[!is.na(usersims) & usersims >=0]
      predictedrating = my.weighted.mean(trainusers[names(usersims),testitemname], usersims)
    }
    else {
      # do item-based CF
      predictedrating = my.weighted.mean(testuser[seenitemnames], itemsims[seenitemnames,testitemname])
    }
    testuser[testitemname] = truerating # restore the actual rating
    preds = c(preds,predictedrating,truerating)
  }
  preds = unname(preds)
  m = as.matrix(preds)
  if (length(m) < numtestitems*2) for (i in (length(m)+1):(numtestitems*2)) { m = rbind(m,NA)}
  return(m)
}
predictuser= cmpfun(predictuser)


# a weighted mean that handles NA's in both arguments (ratings and similarities)
my.weighted.mean = function(x,y) {
  xy = x*y; 
  z = sum(abs(y[!is.na(xy)]))
  if (z == 0) as.numeric(NA) else sum(xy,na.rm=TRUE)/z 
}
my.weighted.mean = cmpfun(my.weighted.mean)

# extract only prediction or only actual ratings from the output of predictCF()
listpreds= function(results) {unlist(results)[c(TRUE,FALSE)]}
listrats = function(results) {unlist(results)[c(FALSE,TRUE)]}
validcnt = function(x) length(which(is.finite(x)))

# 3.8 Predict user rating - UU ==========================================================

# setup the train/test scheme
set.seed(100)
numtestusers = 40 
test  = sample(rownames(users), min(numtestusers,nrow(users)))
train = setdiff(rownames(users),test)

# #test UBCF (try different similarity metrics)
# preds = predictCF(users[test,], users[train,], numtestitems=10, random=FALSE, simfun=cosinesim) 
# cat("avg MAE =",avgMAE(preds), "from", validcnt(listpreds(preds)), "tests")

# normalised data
normalizedusers = sweep(users, 1, rowMeans(users, na.rm=TRUE) ) 

# test UBCF (try different similarity metrics)
st=Sys.time()

preds1 = predictCF(users[test,], users[train,], numtestitems=50, random=FALSE, simfun=euclidsim)
preds2 = predictCF(users[test,], users[train,], numtestitems=50, random=FALSE, simfun=cosinesim)
preds3 = predictCF(users[test,], users[train,], numtestitems=50, random=FALSE, simfun=pearsonsim)
preds4 = predictCF(users[test,], users[train,], numtestitems=50, random=FALSE, simfun=pearsonRM)
preds5 = predictCF(users[test,], users[train,], numtestitems=50, random=FALSE, simfun=jacardsim)
preds6 = predictCF(normalizedusers[test,], normalizedusers[train,], numtestitems=50, random=FALSE, simfun=euclidsim)
preds7 = predictCF(normalizedusers[test,], normalizedusers[train,], numtestitems=50, random=FALSE, simfun=cosinesim)

preds11 = predictCF(users[test,], users[train,], numtestitems=200, random=FALSE, simfun=euclidsim)
preds21 = predictCF(users[test,], users[train,], numtestitems=200, random=FALSE, simfun=cosinesim)
preds31 = predictCF(users[test,], users[train,], numtestitems=200, random=FALSE, simfun=pearsonsim)
preds41 = predictCF(users[test,], users[train,], numtestitems=200, random=FALSE, simfun=pearsonRM)
preds51 = predictCF(users[test,], users[train,], numtestitems=200, random=FALSE, simfun=jacardsim)
preds61 = predictCF(normalizedusers[test,], normalizedusers[train,], numtestitems=200, random=FALSE, simfun=euclidsim)
preds71 = predictCF(normalizedusers[test,], normalizedusers[train,], numtestitems=200, random=FALSE, simfun=cosinesim)


set.seed(100)
numtestusers = 80 
test1  = sample(rownames(users), min(numtestusers,nrow(users)))
train1 = setdiff(rownames(users),test)

preds12 = predictCF(users[test1,], users[train1,], numtestitems=50, random=FALSE, simfun=euclidsim)
preds22 = predictCF(users[test1,], users[train1,], numtestitems=50, random=FALSE, simfun=cosinesim)
preds32 = predictCF(users[test1,], users[train1,], numtestitems=50, random=FALSE, simfun=pearsonsim)
preds42 = predictCF(users[test1,], users[train1,], numtestitems=50, random=FALSE, simfun=pearsonRM)
preds52 = predictCF(users[test1,], users[train1,], numtestitems=50, random=FALSE, simfun=jacardsim)
preds62 = predictCF(normalizedusers[test1,], normalizedusers[train1,], numtestitems=50, random=FALSE, simfun=euclidsim)
preds72 = predictCF(normalizedusers[test1,], normalizedusers[train1,], numtestitems=50, random=FALSE, simfun=cosinesim)

preds111 = predictCF(users[test1,], users[train1,], numtestitems=200, random=FALSE, simfun=euclidsim)
preds211 = predictCF(users[test1,], users[train1,], numtestitems=200, random=FALSE, simfun=cosinesim)
preds311 = predictCF(users[test1,], users[train1,], numtestitems=200, random=FALSE, simfun=pearsonsim)
preds411 = predictCF(users[test1,], users[train1,], numtestitems=200, random=FALSE, simfun=pearsonRM)
preds511 = predictCF(users[test1,], users[train1,], numtestitems=200, random=FALSE, simfun=jacardsim)
preds611 = predictCF(normalizedusers[test1,], normalizedusers[train1,], numtestitems=200, random=FALSE, simfun=euclidsim)
preds711 = predictCF(normalizedusers[test1,], normalizedusers[train1,], numtestitems=200, random=FALSE, simfun=cosinesim)


avgMAE <- c(avgMAE(preds1), avgMAE(preds2),avgMAE(preds3),avgMAE(preds4),avgMAE(preds5), avgMAE(preds6), 
            avgMAE(preds7),avgMAE(preds11), avgMAE(preds21),avgMAE(preds31),avgMAE(preds41),avgMAE(preds51), 
            avgMAE(preds61), avgMAE(preds71),avgMAE(preds12), avgMAE(preds22),avgMAE(preds32),avgMAE(preds42),
            avgMAE(preds52), avgMAE(preds62),avgMAE(preds72),avgMAE(preds111), avgMAE(preds211),avgMAE(preds311),
            avgMAE(preds411),avgMAE(preds511),avgMAE(preds611), avgMAE(preds711)) 

testnum <- c(validcnt(listpreds(preds1)),validcnt(listpreds(preds2)),validcnt(listpreds(preds3)),
             validcnt(listpreds(preds4)),validcnt(listpreds(preds5)),validcnt(listpreds(preds6)),
             validcnt(listpreds(preds7)),validcnt(listpreds(preds11)),validcnt(listpreds(preds21)),
             validcnt(listpreds(preds31)),validcnt(listpreds(preds41)),validcnt(listpreds(preds51)),
             validcnt(listpreds(preds61)),validcnt(listpreds(preds71)),validcnt(listpreds(preds12)),
             validcnt(listpreds(preds22)),validcnt(listpreds(preds32)),validcnt(listpreds(preds42)),
             validcnt(listpreds(preds52)),validcnt(listpreds(preds62)),validcnt(listpreds(preds72)),
             validcnt(listpreds(preds111)),validcnt(listpreds(preds211)), validcnt(listpreds(preds311)),
             validcnt(listpreds(preds411)),validcnt(listpreds(preds511)),validcnt(listpreds(preds611)),
             validcnt(listpreds(preds711)))


algo_list <-rep(c("euclidsim","cosinesim","pearsonsim","pearsonRM","jacardsim", "norm_euclidsim", "norm_cosinesim"), times=4)

num_users <- c(rep("40 Test subjects",each =14),rep("80 Test subjects", each=14))
num_test <- rep(rep(c("50 Test items","200 Test items"),each = 7), times=2)

outcome <- cbind.data.frame(avgMAE,testnum,num_users,num_test,algo_list)
colnames(outcome) <- c("avgMAE", "testnum","num_test_users","num_test_items","alogrithm")

# write.csv(outcome, file = "outcome.csv")

# plot outcome
ggplot(outcome, aes(x = reorder(alogrithm, -avgMAE) , y = avgMAE))+
  geom_point(alpha = 0.6) +
  labs(x = "Algothrim", y = "Average MAE", 
       title = "Comparing outcome of user-based similarity matrix algorithm") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(num_test_users ~ num_test_items) +
  theme(strip.background = element_rect(
    color="indianred", fill="indianred", size=1.5, linetype="solid"))

Sys.time()-st

# 3.8 Predict user rating - II ==========================================================

# test IBCF (try different similarity metrics)
st=Sys.time()

itemsims1 = getitemsimsmatrix(users[train,], simfun=cosinesim)
itemsims2 = getitemsimsmatrix(users[train,], simfun=euclidsim)
itemsims3 = getitemsimsmatrix(users[train,], simfun=pearsonRM)
itemsims4 = getitemsimsmatrix(users[train,], simfun=jacardsim)
itemsims5 = getitemsimsmatrix(normalizedusers[train,], simfun=euclidsim)
itemsims6 = getitemsimsmatrix(normalizedusers[train,], simfun=cosinesim)

preds13 = predictCF(users[test,], itemsims=itemsims1, numtestitems=50, random=FALSE)
preds23 = predictCF(users[test,], itemsims=itemsims2, numtestitems=50, random=FALSE)
preds33 = predictCF(users[test,], itemsims=itemsims3, numtestitems=50, random=FALSE)
preds43 = predictCF(users[test,], itemsims=itemsims4, numtestitems=50, random=FALSE)
preds53 = predictCF(normalizedusers[test,], normalizedusers[train,], itemsims=itemsims5, numtestitems=50, random=FALSE)
preds63 = predictCF(normalizedusers[test,], normalizedusers[train,], itemsims=itemsims6, numtestitems=50, random=FALSE)

preds131 = predictCF(users[test,], itemsims=itemsims1, numtestitems=200, random=FALSE)
preds231 = predictCF(users[test,], itemsims=itemsims2, numtestitems=200, random=FALSE)
preds331 = predictCF(users[test,], itemsims=itemsims3, numtestitems=200, random=FALSE)
preds431 = predictCF(users[test,], itemsims=itemsims4, numtestitems=200, random=FALSE)
preds531 = predictCF(normalizedusers[test,], normalizedusers[train,], itemsims=itemsims5, numtestitems=200, random=FALSE)
preds631 = predictCF(normalizedusers[test,], normalizedusers[train,], itemsims=itemsims6, numtestitems=200, random=FALSE)

itemsims11 = getitemsimsmatrix(users[train1,], simfun=cosinesim)
itemsims21 = getitemsimsmatrix(users[train1,], simfun=euclidsim)
itemsims31 = getitemsimsmatrix(users[train1,], simfun=pearsonRM)
itemsims41 = getitemsimsmatrix(users[train1,], simfun=jacardsim)
itemsims51 = getitemsimsmatrix(normalizedusers[train1,], simfun=euclidsim)
itemsims61 = getitemsimsmatrix(normalizedusers[train1,], simfun=cosinesim)

preds14 = predictCF(users[test1,], itemsims=itemsims11, numtestitems=50, random=FALSE)
preds24 = predictCF(users[test1,], itemsims=itemsims21, numtestitems=50, random=FALSE)
preds34 = predictCF(users[test1,], itemsims=itemsims31, numtestitems=50, random=FALSE)
preds44 = predictCF(users[test1,], itemsims=itemsims41, numtestitems=50, random=FALSE)
preds54 = predictCF(normalizedusers[test1,], normalizedusers[train,], itemsims=itemsims51, numtestitems=50, random=FALSE)
preds64 = predictCF(normalizedusers[test1,], normalizedusers[train,], itemsims=itemsims61, numtestitems=50, random=FALSE)

preds141 = predictCF(users[test1,], itemsims=itemsims11, numtestitems=200, random=FALSE)
preds241 = predictCF(users[test1,], itemsims=itemsims21, numtestitems=200, random=FALSE)
preds341 = predictCF(users[test1,], itemsims=itemsims31, numtestitems=200, random=FALSE)
preds441 = predictCF(users[test1,], itemsims=itemsims41, numtestitems=200, random=FALSE)
preds541 = predictCF(normalizedusers[test1,], normalizedusers[train1,], itemsims=itemsims51, numtestitems=200, random=FALSE)
preds641 = predictCF(normalizedusers[test1,], normalizedusers[train1,], itemsims=itemsims61, numtestitems=200, random=FALSE)

# create dataframe to collect outcome
avgMAE1 <- c(avgMAE(preds13), avgMAE(preds23),avgMAE(preds33),avgMAE(preds43),avgMAE(preds53), avgMAE(preds63), 
            avgMAE(preds131), avgMAE(preds231),avgMAE(preds331),avgMAE(preds431),avgMAE(preds531), 
            avgMAE(preds631), avgMAE(preds14), avgMAE(preds24),avgMAE(preds34),avgMAE(preds44),
            avgMAE(preds54), avgMAE(preds64),avgMAE(preds141), avgMAE(preds241),avgMAE(preds341),
            avgMAE(preds441),avgMAE(preds541),avgMAE(preds641)) 

testnum1 <- c(validcnt(listpreds(preds13)),validcnt(listpreds(preds23)),validcnt(listpreds(preds33)),
             validcnt(listpreds(preds43)),validcnt(listpreds(preds53)),validcnt(listpreds(preds63)),
             validcnt(listpreds(preds131)),validcnt(listpreds(preds231)),
             validcnt(listpreds(preds331)),validcnt(listpreds(preds431)),validcnt(listpreds(preds531)),
             validcnt(listpreds(preds631)),validcnt(listpreds(preds14)),
             validcnt(listpreds(preds24)),validcnt(listpreds(preds34)),validcnt(listpreds(preds44)),
             validcnt(listpreds(preds54)),validcnt(listpreds(preds64)),
             validcnt(listpreds(preds141)),validcnt(listpreds(preds241)), validcnt(listpreds(preds341)),
             validcnt(listpreds(preds441)),validcnt(listpreds(preds541)),validcnt(listpreds(preds641)))

algo_list1 <-rep(c("euclidsim","cosinesim","pearsonsim","jacardsim", "norm_euclidsim", "norm_cosinesim"), times=4)

num_users1 <- c(rep("40 Test subjects",each =12),rep("80 Test subjects", each=12))
num_test1 <- rep(rep(c("50 Test items","200 Test items"),each = 6), times=2)

outcome1 <- cbind.data.frame(avgMAE1,testnum1,num_users1,num_test1,algo_list1)
colnames(outcome1) <- c("avgMAE", "testnum","num_test_users","num_test_items","alogrithm")

# plot outcome
ggplot(outcome1, aes(x = reorder(alogrithm, -avgMAE) , y = avgMAE))+
  geom_point(alpha = 0.6) +
  labs(x = "Algothrim", y = "Average MAE", 
       title = "Comparing outcome of item-based similarity matrix algorithm") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(num_test_users ~ num_test_items) +
  theme(strip.background = element_rect(
    color="indianred", fill="indianred", size=1.5, linetype="solid"))

# write.csv(outcome1, file = "outcome1.csv",row.names=FALSE)

Sys.time()-st

# 3.9 Predict user rating - ALS & SVD ===================================================

# ensuring users and items are read as factors
reviews_aggre$reviewer_id = factor(reviews_aggre$reviewer_id)
reviews_aggre$listing_id = factor(reviews_aggre$listing_id)
str(reviews_aggre)
rating <- reviews_aggre
colnames(rating) = c("user","item","rating")

# convert to tabular format
users = acast(rating, user ~ item, value.var = "rating")
colnames(users) = sort(unique(rating$item))
rownames(users) = sort(unique(rating$user))
users[1:10,1:15]

# split the events using the same split (train_ind & test_ind) as used earlier
# set.seed(123)
set.seed(239)
smp_size <- floor(0.9 * nrow(rating))
train_indexes <- sample(1: nrow(rating), size = smp_size)
trainevents <- rating[train_indexes, ]; dim(trainevents)
testevents  <- rating[-train_indexes, ]; dim(testevents)

# make a copy and then blank out the test events (ie set test ratings for the test (user,item) pairs to NA)

trainusers = users
fillrate(trainusers)
x = apply(testevents,1,function(row) trainusers[row[1],row[2]] <<- NA) # row[1] ~ user, row[2] ~ item
fillrate(trainusers)

# factorize into U * D * V using 30 latent features
trainusers=as(trainusers,"Incomplete") # coerce into correct matrix format with missing entries

# do one of the below
fit1=softImpute(trainusers, rank.max=250, type="als") # als is the default
# fit1=softImpute(trainusers, rank.max=10, type="svd") # for comparison

# take a look at the factorised matrixes
dim(fit1$u) ; fit1$u[1:10,1:5] # the user latent features
dim(fit1$v) ; fit1$v[1:10,1:5] # the item latent features
# length(fit1$d); head(fit1$d)   # the singular values

# make predictions for all of the empty (user,item) pairs (the test pairs + those missing in orginal dataset)
trainuserscompleted = complete(trainusers, fit1)
#write.csv(trainuserscompleted, file = "ALS Model1.csv")

# compute the MAE for the predictions made for the test events
rownames(trainuserscompleted) = rownames(users) # copy across the user names
colnames(trainuserscompleted) = colnames(users) # copy across the item names
abserrs = apply(testevents, 1, function(row) abs(trainuserscompleted[row[1],row[2]] - users[row[1],row[2]])) # row[1] ~ user, row[2] ~ item
mean(t(abserrs), na.rm=TRUE) # show the MAE


alsoutcome <- read_csv("ALS.csv")
ggplot(alsoutcome, aes(x = factor(iterations) , y = MAE, group = factor(method), color=factor(method)))+
  geom_line(linetype = "longdash")+
  geom_point(alpha = 0.6, size = 3) +
  labs(x = "Iterations", y = "Average MAE",
       title = "Comparing outcome of Matrix Factorization approach",
       color = "Method") +
  theme_light() +
  facet_wrap(~perc_test_data) +
  theme(strip.background = element_rect(
    color="indianred", fill="indianred", size=1.5, linetype="solid")) 




################################4. Collborative filtering  ###############################


# 4.1 SVD ===============================================================================


# Output recommendation using ALS.
trainuserscompleted[1:10,1:10]
dim(trainuserscompleted) # 64036  4729
outcome = as.data.frame(trainuserscompleted)
#outcome = outcome[,-1]

x = integer(64036)
y = integer(64036)
z = integer(64036)
a = integer(64036)
b = integer(64036)
c = integer(64036)

for (i in 1:64036) {
  a = as.matrix(outcome[i,])[1,]
  x[i] = names(a[order(a,decreasing=TRUE)[1]])
  y[i] = names(a[order(a,decreasing=TRUE)[2]])
  z[i] = names(a[order(a,decreasing=TRUE)[3]])
  a[i] = names(a[order(a,decreasing=TRUE)[4]])
  b[i] = names(a[order(a,decreasing=TRUE)[5]])
  c[i] = names(a[order(a,decreasing=TRUE)[6]])
}


df <- data.frame(x,y,z,a,b,c,stringsAsFactors = FALSE)
rownames(df) = rownames(users)
write.csv(df, file = "ALS outcome1.csv")

# 4.2 testing ===========================================================================

count_review <- reviews_aggre %>%
  count(reviewer_id) %>%
  arrange(desc(n)) %>%
  filter(n > 5)

review_user <- reviews_aggre %>%
  inner_join(count_review) %>%
  select(reviewer_id, listing_id) 

set_key <- setorder(setkey(setDT(review_user), reviewer_id), reviewer_id, -listing_id)[,.SD[1:2], by=reviewer_id]
set_key$key <- paste(set_key$reviewer_id, set_key$listing_id)


reviews_data <- reviews_aggre %>%
  mutate(key = paste(reviewer_id, listing_id)) %>%
  anti_join(set_key, by = "key") %>%
  select(reviewer_id, listing_id, ratings_nm)

rating1 <- reviews_data
colnames(rating1) = c("user","item","rating")

users1 = acast(rating1, user ~ item, value.var = "rating")
colnames(users1) = sort(unique(rating1$item))
rownames(users1) = sort(unique(rating1$user))


# make a copy and then blank out the test events (ie set test ratings for the test (user,item) pairs to NA)
trainusers1 = users1
fillrate(trainusers1)
y = apply(testevents,1,function(row) trainusers1[row[1],row[2]] <<- NA) # row[1] ~ user, row[2] ~ item
fillrate(trainusers1)


# factorize into U * D * V using 30 latent features
trainusers1=as(trainusers1,"Incomplete") # coerce into correct matrix format with missing entries

# run model
fit_als=softImpute(trainusers1, rank.max=250, type="als") # als is the default
# fit1=softImpute(trainusers, rank.max=10, type="svd") # for comparison

str(fit_als)
# make predictions for all of the empty (user,item) pairs (the test pairs + those missing in orginal dataset)
trainuserscompleted_als = complete(trainusers1, fit_als)
rownames(trainuserscompleted_als) = rownames(users1) # copy across the user names
colnames(trainuserscompleted_als) = colnames(users1) # copy across the item names
#write.csv(trainuserscompleted, file = "ALS Model1.csv")


dim(trainuserscompleted_als) # 64036  4583
outcome_als = as.data.frame(trainuserscompleted_als)

# user_test <- as.integer(unique(set_key$reviewer_id))
# str(user_test)

# d = integer(45)
# e = integer(45)
# f = integer(45)
# g = integer(45)
# h = integer(45)
# i = integer(45)
# j = integer(45)
# k = integer(45)
# l = integer(45)
# m = integer(45)
# n = integer(45)
# o = integer(45)
# p = integer(45)
# q = integer(45)
# r = integer(45)
# s = integer(45)
# t = integer(45)
# u = integer(45)
# v = integer(45)
# w = integer(45)


# for (i in user_test) {
#   a = as.matrix(outcome_als[i,])[1,]
#   d[i] = names(a[order(a,decreasing=TRUE)[1]])
#   e[i] = names(a[order(a,decreasing=TRUE)[2]])
#   f[i] = names(a[order(a,decreasing=TRUE)[3]])
#   g[i] = names(a[order(a,decreasing=TRUE)[4]])
#   h[i] = names(a[order(a,decreasing=TRUE)[5]])
#   i[i] = names(a[order(a,decreasing=TRUE)[6]])
#   j[i] = names(a[order(a,decreasing=TRUE)[7]])
#   k[i] = names(a[order(a,decreasing=TRUE)[8]])
#   l[i] = names(a[order(a,decreasing=TRUE)[9]])
#   m[i] = names(a[order(a,decreasing=TRUE)[10]])
#   n[i] = names(a[order(a,decreasing=TRUE)[11]])
#   o[i] = names(a[order(a,decreasing=TRUE)[12]])
#   p[i] = names(a[order(a,decreasing=TRUE)[13]])
#   q[i] = names(a[order(a,decreasing=TRUE)[14]])
#   r[i] = names(a[order(a,decreasing=TRUE)[15]])
#   s[i] = names(a[order(a,decreasing=TRUE)[16]])
#   t[i] = names(a[order(a,decreasing=TRUE)[17]])
#   u[i] = names(a[order(a,decreasing=TRUE)[18]])
#   v[i] = names(a[order(a,decreasing=TRUE)[19]])
#   w[i] = names(a[order(a,decreasing=TRUE)[20]])
  
#   }


# df <- data.frame(x,y,z,stringsAsFactors = FALSE)
# rownames(df) = rownames(users)
# write.csv(df, file = "ALS outcome1.csv")
