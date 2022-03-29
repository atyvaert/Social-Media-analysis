# THE FIRST LINES ARE HIDDEN AS THEY ARE THE SAME AS IN THE FIRST METHOD

###############################################
rm(list=ls()) #Clean the environment
# setwd("C:\\Users\\vikto\\OneDrive\\Documenten\\UGent\\EersteMaster\\SMWA\\GroupWork")
setwd("/Users/Artur/Desktop/uni jaar 5 sem 2/SMWA/coding")
# Install and load packages
if (!require("pacman")) install.packages("pacman") ; require("pacman")
p_load(tidyverse, rtweet, httpuv, statnet, sna)

# Make sure you have your authorization keys available 
# Do this with your own access tokens
source('tokensandkeys.R')

get_token()
token

#provide the twitter screen name of which you want to scrape the followers
my_name <- "viktor_vdb2"

# Get the user- object for 'my_name'
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

# this results in a user object (userInfo) with all kinds of information
glimpse(userInfo)

# get user- objects for all followers of my_name
# at this point we extract all followers and not just the ones
# This function returns the user IDs
firstdegree <- get_followers(
  my_name, n = userInfo$followers_count, retryonratelimit = TRUE)

# We want to get the user names of the followers for our further analysis
followers <- lookup_users(firstdegree$user_id) %>% pull(screen_name)
followers

# Let's check if this list has the same length as the amount of followers of 'my_name'
# Bot have a length of 144
# If there is a difference, it is normally due to privacy settings
length(followers) # 144
userInfo$followers_count # 144

#Take a look at the followers
head(followers)


# find the second degree followers for 'my_name'
# first get the followers of 'myname', by selecting the indices of these followers

# 1) This should be 144 
seconddegree <- lookup_users(followers)
nrow(seconddegree)

#see if we can find these followers
ind <- which(followers %in% seconddegree$screen_name)
followers[ind]
length(ind)

######################################################
############################ 
#Load the scraped followers from Viktor_vdb2
all_followers <- load('/all_followersViktor_vdb2.RData')
#####################################
######################################################
# 2) #Now extract user ids of followers-of-followers
# However, as this takes a really long time due to twitter scraping limits,
# it is more efficient to load in the rdata file 'all_followersViktor_vdb2_1.rdata' 
# which is also provided in the assignment

all_followers <- list()
for (i in 1:nrow(seconddegree)) {
  
  cat('... Scraping: ', seconddegree$screen_name[i], '\n')
  
  followersseconddegree <- character(seconddegree$followers_count[i]) #preallocate vector
  
  #If you would want to work with the names you should first get the follower IDs
  #then look up the users and store these ones in the list
  
  followersseconddegree <- get_followers(seconddegree$user_id[i], 
                                         retryonratelimit = TRUE) %>% pull(user_id)
  all_followers[[i]] <- lookup_users(followersseconddegree) %>% pull(screen_name)
  
  #If you would solely work with the IDs, one line of code is enough
  #l[[i]] <- get_followers(seconddegree$user_ids[i], 
  #                       retryonratelimit = TRUE) %>% pull(user_id)
}


class(all_followers)


#let's have a look
glimpse(all_followers)


# when we look at the length of this list, we notice that there are 144 followers
# This is correct if we look at he number of followers from Viktor_vdb2
length(all_followers)

#now we have all the followers of our followers
#let's add our followers to that list
all_followers[[length(all_followers)+1]] <- followers
length(all_followers)

# We also get the names of all the followers for later, this includes Viktor_vdb2
# the length is thus 145 = the number of follower + my_name himself
names(all_followers) <- c(seconddegree$screen_name,userInfo$screen_name)
names(all_followers) 


length(all_followers)
# As we were unable to scrape the full network of accounts with more than 4998 followers,
# we have decided to leave these accounts out of our analysis as their number of degrees with the original users
# might not be corrected
# further, they heavily increase the computation time
for (i in length(all_followers):1) {
  if (length(unlist(all_followers[i])) > 4998)
    all_followers <- all_followers[-i]
}

# This removes 4 followers out of our analysis
length(all_followers)

# Further, we also remove the private accounts between our followers, as we were unable to 
# scrape their followers
for (i in length(all_followers):1) {
  if (is.na(all_followers[i]))
    all_followers <- all_followers[-i]
}

# This removes 21 followers out of our analysis
length(all_followers)


# These removals leave us with 120 followers (including my_name)
glimpse(all_followers)
length(all_followers)

# transform that list to a character vector of length 120 (= number of followers + 1)
# Each element in the vector contains all the followers of a user.
mm <- do.call("c", lapply(all_followers, paste, collapse=" "))
length(mm)

############### Process data to create adjacency matrix ###########

# Note that we could also use another approach, e.g. start from edge lists; 
# then, we should have added the user id to all his followers ids
# Now, we show a different method that splits the character vector and creates an incidence matrix

#Install and load the text mining package to preprocess the data
p_load(SnowballC, tm)

# transform that vector using the tm package to structure the unstructured data
myCorpus <- Corpus(VectorSource(mm))

#inspect the result (not really usefull and time consuming)
#inspect(myCorpus)


# this creates a matrix, in which the rows are our sources of interest and the columns are the friends 
# This thus resembles an incidence matrix
# In the rows are the first degree followers and the columns the followers of the followers
userfollower <- DocumentTermMatrix(myCorpus, control = list(wordLengths = c(0, Inf)))

#we can also look at the actual matrix
inspect(userfollower)
dim(userfollower)

#There are 120 rows (i.e., number of followers and myself) and 13720 columns
#The columns are all the friends for the users in the rows
#The values in the cells are either 0 or 1, to indicate a whether
#the users in the columns are following the users in the rows











###############################################
# In this new approach, we calculate the adjacency matrix based on the cosine similarity.
# The cosine similarity is another similarity metric on which the adjacency matrix can be build.
# The smaller the distance, the bigger the similarity.
# However, in this method it will not be possible to compute the degree as there is always a value for 
# the connection between two nodes. Namely, the cosine similarity is never zero.
# Therefore, we will look at which nodes have the highest similarity compared to the other nodes.
# In other words, we will look at the nodes with the smallest distance to other nodes and rank our 
# ground truth based on this ranking
################################################


# only look at the first degree followers to compute the ground truth
# further, we also delete 'my_name' out of the analysis as it is his network
followers <- tolower(names(all_followers))
followers
followers <- followers[-length(followers)]
followers

# First, create a data frame of the userfollower object in order to calculate 
# the euclidian distance between first order followers
# Then, we store the cosine similarities in a dataframe
p_load(philentropy)
p_load(lsa)
cosine_similarity_all <- distance(as.matrix(userfollower), method = 'cosine')
rownames(cosine_similarity_all) <- rownames(as.matrix(userfollower))
cosine_similarity <- data.frame(similarity = rowSums(cosine_similarity_all), 
                           Followers = rownames(cosine_similarity_all))
cosine_similarity

# look at the most central followers, by adding their similarity to other followers
# and giving the smallest distance the highest rank (biggest similarity)
cosine_similarity <- cosine_similarity[tolower(cosine_similarity$Followers) %in% followers,]
cosine_similarity <- cosine_similarity[order(cosine_similarity$similarity, decreasing = TRUE),]
cosine_similarity

# rank all of your followers based on their rank. This is considered the ground truth
cosine_similarity$rank <- 1:nrow(cosine_similarity)
rownames(cosine_similarity) <- 1:nrow(cosine_similarity)
ground_truth <- cosine_similarity
ground_truth


# Creating a bar plot of the ground truth
# site: http://www.sthda.com/english/articles/32-r-graphics-essentials/133-plot-one-variable-frequency-graph-density-distribution-and-more/#dot-charts
p_load(ggpubr)
library(dplyr)
bar_plot_top10 <- ggplot(ground_truth, aes(x = reorder(Followers, rank), y = similarity)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  xlab('Followers') + 
  ylab('Similarity') +
  ggtitle('Ground truth top 10 ranked (cosine similarity)') +
  ylim(0,25) +
  theme_pubclean() + 
  scale_x_discrete(limits = c(ground_truth[1:10, 2])) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.title = element_text(hjust = 0.5))

bar_plot_top10

bar_plot_all <- ggplot(ground_truth, aes(x = reorder(Followers, rank), y = similarity)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  xlab('Followers') + 
  ylab('Similarity') +
  ggtitle('Ground truth evolution (cosine similarity)') +
  ylim(0,25) +
  theme_pubclean() + 
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.title = element_text(hjust = 0.5))

bar_plot_all


###################################################
# Method 1: same principle as with the dot product
###################################################

# Creating a function that computes rank for a given matrix
rank_followers <- function(matrix){
  cosine_similarity_all <- distance(as.matrix(matrix), method="cosine")
  cosine_similarity_all[is.nan(cosine_similarity_all)] <- 0
  rownames(cosine_similarity_all) <- rownames(as.matrix(matrix))
  cosine_similarity <- data.frame(similarity = rowSums(cosine_similarity_all), 
                                  Followers = rownames(cosine_similarity_all))
  
  cosine_similarity <- cosine_similarity[tolower(cosine_similarity$Followers) %in% followers,]
  cosine_similarity <- cosine_similarity[order(cosine_similarity$similarity, decreasing = TRUE),]
  
  cosine_similarity$rank <- 1:nrow(cosine_similarity)
  rownames(cosine_similarity) <- 1:nrow(cosine_similarity)
  return(cosine_similarity)
}

# Inspect the data
inspect(userfollower)

# Create a matrix object of userfollower
M1 <- as.matrix(userfollower)

# Leave out my_name for the analysis
# Normally, this should be the last row of the matrix
M1 <- M1[-nrow(M1),]
dim(M1)

# Define a vector to store the correlation created in the loop
# Define a vector to keep track of the dimension of the adjacency matrix
correlation.list <- c()
i.list <- c()

# Create a for loop to increase the nr of followers of followers on which
# you build your adjacency matrix
for (i in 2:5000) {
  print(i)
  # Take the first i followers of followers to create the adjacency matrix
  M1_subset <- M1[,1:i]
  A <- M1_subset %*% t(M1_subset)
  A
  # Ensure that elements going on the diagonal are 0
  for (j in 1:nrow(A)) {
    A[j,j] <- 0
  }
  
  # Compute the current rank of the followers with rank_followers
  current_rank <- rank_followers(A)
  
  # Merge current_rank with the ground truth degree and match followers
  rank_matrix <- merge(x = ground_truth, y = current_rank, by = "Followers")
  
  # Calculate correlation
  correlation <- cor(rank_matrix$rank.x, rank_matrix$rank.y, method= "spearman")
  
  # Store both i and correlation
  correlation.list <- append(correlation.list,correlation)
  i.list <- append(i.list,i)
  
}

correlation.list

# Creating a plot the correlations
df <- data.frame( size= i.list, correlation = correlation.list)
corr_plot <- ggplot(df, aes(x = size, y = correlation)) + 
  geom_line(colour="blue") + 
  xlab('Network size') + 
  ylab('Rank based correlation') +
  ggtitle('Ground truth approximation method 1 (cosine similarity)') +
  ylim(-0.5, 1) +
  geom_hline(yintercept = 1,color = "black", linetype = "dashed") +
  theme_pubclean() +
  theme(plot.title = element_text(face = "bold"))

corr_plot




###################################################
# Method 2: same principle as with the dot product
###################################################

# Inspect the data
inspect(userfollower)

# Create a matrix object of userfollower
M2 <- as.matrix(userfollower)

# Leave out my_name for the analysis
# Normally, this should be the last row of the matrix
M2 <- M2[-nrow(M2),]
dim(M2)

# Define a new vector to store the correlation created in the loop of method 2
# Define a vector to keep track of the dimension of the adjacency matrix in method 2
correlation.list2 <- c()
i.list2 <- c()
i.list2
# Create a for loop to increase the nr of followers of followers on which
# you build your adjacency matrix
# and add an outer for loop to create this process five times
for(j in 1:5) {
  for (i in 2:5000) {
    print(i)
    # Take a random subset of i followers of followers to create the adjacency matrix
    random_draw <- sample(1:ncol(M2), i, replace = FALSE)
    random_draw
    M2_subset <- M2[,random_draw]
    
    A <- M2_subset %*% t(M2_subset)
    A
    #  Ensure that elements going on the diagonal are 0
    for (j in 1:nrow(A)) {
      A[j,j] <- 0
    }
    
    # Compute the current rank of the followers with rank_followers
    current_rank <- rank_followers(A)
    
    # Merge current_rank with the ground truth degree and match followers
    rank_matrix <- merge(x = ground_truth, y = current_rank, by = "Followers")
    
    # Calculate correlation
    correlation <- cor(rank_matrix$rank.x, rank_matrix$rank.y, method= "spearman")
    
    # Store both i and correlation
    correlation.list2 <- append(correlation.list2,correlation)
    i.list2 <- append(i.list2,i)
    
  }
}


# Take an average over 5 runs for robustness

run_1 <- correlation.list2[1:4999]
run_2 <- correlation.list2[5000:9998]
run_3 <- correlation.list2[9999:14997]
run_4 <- correlation.list2[14998:19996]
run_5 <- correlation.list2[19997:24995]

all_runs <- do.call(rbind, Map(cbind, run_1, run_2, run_3, run_4, run_5))
all_runs
average_correlation <- rowMeans(all_runs)
average_correlation
length(average_correlation)
length(i.list2)/5


# Plot the average of the correlations over the 5 runs against the size of the run
p_load(Hmisc)

df <- data.frame(size= i.list, average_correlation = average_correlation)

correlation_overview <- ggplot(df, aes(x = size, y = average_correlation)) + 
  geom_line(colour="blue") + 
  xlab('Network size') + 
  ylab('Average correlation') +
  ggtitle('Ground truth approximation method 2 (cosine similarity)') +
  ylim(-0.5, 1) +
  geom_hline(yintercept = 1,color = "black", linetype = "dashed") +
  theme_pubclean() +
  theme(plot.title = element_text(face = "bold")) +
  theme(panel.grid.minor.y = element_line(size = 0.5, linetype = 'dashed',
                                          colour = "gray"))
correlation_overview

#############################################################
# In the final part of the assignment, we will try to determine
# on how many users you should extract to build your adjacency matrix on,
# while achieving similar performance
# In other words, we will try to determine an optimal cut off point
# to limit the number of followers to extract to a minimum

# In this approach, we will look at the learning rate of our average
# correlation
# If additional followers of followers to compute the adjacency matrix
# do not result in an improvement of the performance, we will use this point
# as a cut off point for the number of followers to extract
#############################################################

# First, add the average per row to the dataframe with all the runs
all_runs <- as.data.frame(all_runs)
all_runs$average <- average_correlation
all_runs
# However, to calculate the learning rate of the curve, we need a smooth curve
# In the current averages, there is too much variation depending on the random subset we took
# Therefore, we apply a smooth spline function to our averages
# With this smooth curve, we can determine the learning rates of our curve (thus with the increasing network)
all_runs$size <- 1:nrow(all_runs)

smooth_curve <- smooth.spline(all_runs$size, all_runs$average, cv = TRUE)
all_runs$smoothness <- smooth_curve$y
learning_rates <- all_runs$smoothness[2:nrow(all_runs)] - all_runs$smoothness[1:(nrow(all_runs)-1)]
all_runs$learning_rates <- append(learning_rates, NA, 0)

# look at the differences in learning rates when building our model
# if the learning rate is less than 0.001, the loop is stopped
for (i in 2:length(all_runs$learning_rates)) {
  if (all_runs$learning_rates[i] < 0.001){
    print(i)
    break
  }
} 
# In this case, an adjacency matrix based on 161 followers of our followers is sufficient

# if the learning rate is less than 0.0001, the loop is stopped
for (i in 2:length(all_runs$learning_rates)) {
  if (all_runs$learning_rates[i] < 0.0001){
    print(i)
    break
  }
}
# In this case, an adjacency matrix based on 644 followers of our followers is sufficient

# Create a plot of the smoothed curve
smooth_curve_plot <- ggplot(all_runs, aes(x = size, y = smooth_curve$y)) + 
  geom_line(colour="blue") + 
  xlab('Network size') + 
  ylab('Average correlation') +
  ggtitle('Smooth curve of correlation (cosine similarity)') +
  ylim(-0.5, 1) +
  geom_hline(yintercept = 1,color = "black", linetype = "dashed") +
  theme_pubclean() +
  theme(plot.title = element_text(face = "bold")) +
  theme(panel.grid.minor.y = element_line(size = 0.5, linetype = 'dashed',
                                          colour = "gray"))

smooth_curve_plot

# Create a final plot with all the average correlations, the cut off points and 
# the confidence intervals
final_plot <- ggplot(all_runs, aes(x = size, y = average)) + 
  geom_line(colour="blue") + 
  xlab('Network size') + 
  ylab('Average correlation') +
  ggtitle('Cut off points for a 0.001 and a 0.001 learning rate (cosine similarity)') +
  geom_vline(xintercept = 161, col = 'yellow') +
  geom_vline(xintercept = 644, col = 'darkgreen') +
  ylim(-0.5, 1) +
  geom_hline(yintercept = 1,color = "black", linetype = "dashed") +
  theme_pubclean() +
  theme(plot.title = element_text(face = "bold"))

final_plot




