# test script for Laura's social media cuourse
# 

# lets begin with a clean workspace
rm(list = ls())

# PACKAGES ----------------------------------------------------------------

# provide arguments why each package is included
install.packages("twitteR") 
install.packages("rio")

# make sure any data files are saved where you want htem to be
setwd() 

library("twitteR")
library("rio")

# ANALYE TWITTER DATA -----------------------------------------------------
# 
# GENERAL SET UP: CREDENTIALS

# authenticate

# provide instructions for how to do this. add links to other web pages for more elaborate
# explanations
credentials <- setup_twitter_oauth(consumer_key='YourConsumerKey', 
                                   consumer_secret='YourConsumerSecret', 
                                   access_token='YourAccessToken', 
                                   access_secret='YourAccessSecret')
# discuss pro/con of saving credentials somwehwere --> add url
# 
# OPTION 1: 
# EXTRACT USER DATA + FRIENDS & FOLLOWERS
# danger: large list of followers and friends --> long time for data extraction. 
# discuss API limites.
# discuss live streaming of data collection. provide link to resources for how to set up
# offer consultation if this is wished for

# user timeline:
# explain all the options in noormal english
# reference to api documentation
userTl = userTimeline(userName,n=3200, includeRts = TRUE)

# search for users with a specific keyword in the descirption, title
# explain all the options in noormal english
# reference to api documentation
users <- search_users(q = 'your query',
                      n = 1000,
                      parse = TRUE)


# WHAT ANALYSIS
# SNA 2:COPY OUTPUT OF PEW RESEARCH
# SNA 3: GEO MAPPING
# SNA 4: (ONLY MENTION METHOD): KEYWORD CO-OCCURENCE IN DESCCRIPTION --> SIMILARITY OF USERS (NAMED ENTITY RECOGNITION)
# 
# OPTION 2:
# EXTRACT TWITTER DATA ON A TOPIC (KEYWORD, HASTAG)
# users with a keyword
# replace your query with your search keywords. limit n to whatever you want. remember large n
# means long time. computer might run high on cpu


# twitter search
searchTerm = "Your search term"
tweets = searchTwitter(searchTerm, n=1000, lang = "en")
dfT = twListToDF(tweets)
# CREATE NETWORK BASED ON REPLIES AND MENTIONS
# SNA 1: DENSITY, INDEGREE DISTIRBUTION, OUTDEGREE DISTIRBUTION, RECIPROCITY (%), 
# SNA 2: COMMUNITIES: WHO IS IN IT: COMMMONALITIES FOR COMMUNITIES
# SNA 3: BROKERS, STRUCTURAL HOLES
# 

# ANALYZE FACEBOOK DATA ---------------------------------------------------


# OPTION 1: 
# EXTRAC MEMBERS OF GROUP/PAGE
# SNA 1:
# SNA 2:
# OPTION 2: EXTRACT USER NETWORK 9
# SNA 1:
# SNA 2
# 
# ANALYZE INSTAGRAM DATA
# OPTION 1:
# EXTRACT USERS NETWORK
# SNA 1
# OPTION 2: EXTRACT KEYWORD NETWORK
# SNA2:
# 
# ANALYZE YOUTUBE DATA
# OPTION 1:
# EXTRACT USER NETWORK
# 