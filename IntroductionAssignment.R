###########################################################################
######## Introduction to assignment #########
###########################################################################

rm(list=ls()) #Clean the environment
setwd("/Users/wouterdewitte/Documents/1e Master Business Engineering_Data Analytics/Semester 2/Social Media and Web Analytics/Project")
# Install and load packages
if (!require("pacman")) install.packages("pacman") ; require("pacman")
p_load(tidyverse, rtweet, httpuv)

#Make sure you have your authorization keys available 
#Do this with your own access tokens
source('tokensandkeys.R')

get_token()
token

#provide your Twitter screen name (just an example)
my_name <- "UGentDA"
#provide the Twitter screen name of 4 of your followers
follower_names <- c("dauwevercamer",
                    "mma_crm",
                    "_3s_",
                    "matthbogaertFEB")

#get the user- object for my_name (make sure we don't bump into extraction limits)
#lookup_users does not have a retryonratelimit
status <- FALSE
while (status==FALSE) {
  rate <- rate_limits(get_token(), "lookup_users")
  status <- as.numeric(rate$remaining) > 50
  if (status) {
    
    cat('Extracting...')
    userInfo <- lookup_users(my_name)
  } else {
    cat("Waiting...")
    Sys.sleep(600) # wait 10 minutes
  }
}

#-# Extracting...

# this results in a user object (userInfo) with all kinds of information
glimpse(userInfo)

# get user- objects for all followers of my_name
# get_followers function has a retryonratelimit option, so you don't need the loop
# at this point we extract all followers and not just the ones
# This function returns the user IDs
firstdegree <- get_followers(
  my_name, n = userInfo$followers_count, retryonratelimit = TRUE
)

#If you want to get the users names you have to look up the IDs (NOT NEEDED!)
followers <- lookup_users(firstdegree$user_id) %>% 
  pull(screen_name)

# this can take a while, depending on the size of the network

#this results in a tbl with rows equal to our number of followers.
#Let's check if the length is correct.
length(followers)
# 1549

userInfo$followers_count
# 1549

#Take a look at the followers
head(followers)

#now select indices of our 4 followers in follower_names
#and retrieve their user objects to get the second degree followers
#(If you are afraid of bumping into the rate limits, use the above loop)
seconddegree <- lookup_users(follower_names)

#this should be four:
nrow(seconddegree)

#see if we can find these followers
ind <- which(followers %in% seconddegree$screen_name)
followers[ind]

#Now extract user ids of followers-of-followers

l <- list()
for (i in 1:nrow(seconddegree)) {
  
  cat('... Scraping: ', seconddegree$screen_name[i], '\n')
  
  followersseconddegree <- character(seconddegree$followers_count[i]) #preallocate vector
  
  #If you would want to work with the names you should first get the follower IDs
  #then look up the users and store these ones in the list
  
  followersseconddegree <- get_followers(seconddegree$user_id[i], 
                                         retryonratelimit = TRUE) %>% pull(user_id)
  l[[i]] <- lookup_users(followersseconddegree) %>% pull(screen_name)
  
  #If you would solely work with the IDs, one line of code is enough
  #l[[i]] <- get_followers(seconddegree$user_ids[i], 
   #                       retryonratelimit = TRUE) %>% pull(user_id)
}

#let's have a look
glimpse(l)

#now we have all the followers of four of our followers
#let's add our followers to that list
l[[length(l)+1]] <- followers
names(l) <- c(seconddegree$screen_name,userInfo$screen_name)

glimpse(l)

#transform that list to a character vector of length 5.
#Each element in the vector contains all the followers of a user.

mm <- do.call("c", lapply(l, paste, collapse=" "))

############### Process data to create adjacency matrix ###########

# Note that we could also use another approach, e.g. start from edge lists; 
# then, we should have added the user id to all his followers ids
# Now, we show a different method that splits the character vector and creates an incidence matrix

#Install and load the text mining package to preprocess the data
p_load(SnowballC, tm)

# transform that vector using the tm package to structure the unstructered data
# we will dive deeper in the tm package when discussing 'the message' on SM
myCorpus <- Corpus(VectorSource(mm))

#inspect the result
inspect(myCorpus)

# this creates a matrix, in which the rows are our sources of interest and the columns are the friends (see later for a discussion of this matrix)
# This thus resembles an incidence matrix
userfollower <- DocumentTermMatrix(myCorpus, control = list(wordLengths = c(0, Inf)))

#we can also look at the actual matrix
inspect(userfollower)
#There are five rows (i.e., four followers and myself)
#The columns are all the friends for the users in the rows
#The values in the cells are either 0 or 1, to indicate a whether
#the users in the columns are following the users in the rows


########################## Network Analysis ##############################
#Targeting social network users using restricted network information
#load the required packages
p_load(igraph)

# Compute the adjacency matrix using matrix multiplication.
# Note that this is not a sparse matrix format, so using larger datasets it could run slowly
A <- t(as.matrix(userfollower)) %*% as.matrix(userfollower)

#Look at its dimensions
dim(A)

#What does it look like?
A[1:10,1:10]

#This gives us the likelihood (a score from 1 to 5) that two users are connected.
#This likelihood is determined based on our 5 initial users. For example:
#If u1 (user 1) follows all of our five initial users and u2 also follows all of our
#five users then u1 and u2 are most likely part of the same community
#(as the five initial users) and connected.
#If u1 follows all five users and u2 follows none of them then u1 is likely
#to be part of the same community as the five initial users while u2 is not
#(and hence not connected to u1).
#If neither u1 and u2 follow any of the five initial users then they could be connected
#but we have no way to know, unless we would be able to extract full network data.
#However, as we extract information about more initial users we will become increasingly
#sure that two users are (not) connected.

# if the adjacency matrix is too large, then you can consider to only take the first 500
# followers. This works good on a computer with 8GB RAM
if (ncol(A) > 500) A <- A[1:500,1:500]

#Create a graph object based on the adjacency matrix & remove loop edges
g <- graph.adjacency(A, weighted=TRUE,
                     mode ='undirected') %>% simplify()

# set labels and degrees of vertices
V(g)$label <- V(g)$name
V(g)$degree <- igraph::degree(g)
#Plot the Graph
set.seed(3952)
#prepare graph
layout <- layout.auto(g)
#Give the graph lots of room

mar <- par()$mar #store this for later
par(mar=rep(0, 4))
plot(g, layout=layout, vertex.label=NA,
     edge.curved=TRUE,vertex.size=3,
     vertex.color=c("green","red","blue")[ifelse(V(g)$name %in%
                                                   names(igraph::degree(g)[tail(order(igraph::degree(g)),5)]) ==TRUE,1,
                                                 ifelse(V(g)$name %in%
                                                          names(igraph::degree(g)[tail(order(igraph::betweenness(g)),10)]) ==TRUE,2,3))])

# The top 5 vertices based on degree are in green
# The top 10 vertices based on betweenness (and not based on degree) are in red
# All the other vertices are in blue
