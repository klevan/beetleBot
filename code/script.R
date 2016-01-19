# Making a beetle-of-the-day twitter bot
rm(list = ls())
# Functions

#Libraries
library(httr)
library(twitteR)
library(ROAuth)
library(XML)

# Accessing Twitter
tokens <- read.csv("C:/Users/klevan/Desktop/my stuff/twitterBOT/tokens.txt", sep="", stringsAsFactors=FALSE)
api_key <- tokens$tokens[1]
api_secret <- tokens$tokens[2]
access_token <- tokens$tokens[3]
access_token_secret <- tokens$tokens[4]
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

# Import data
life <- read.csv("~/GitHub/beetleBot/data/lifehist.csv", stringsAsFactors=FALSE)

# Choose a beetle
beetle_name <- sample(life$scientificName,1)
if(nchar(life$beetle_photo[grepl(beetle_name,life$scientificName)])>0){
  beetle_photo <- paste("~/GitHub/beetleBot/images",
                        life$beetle_photo[grepl(beetle_name,life$scientificName)],
                        sep="/")
} else {
  beetle_photo <- ""
}

# Determine known facts
# 1. Find dispersal information about the beetle
dispersal <- strsplit(life$Dispersal[match(beetle_name,life$scientificName)],
                      split="\\. ")[[1]][sample(1:length(strsplit(life$Dispersal
                                                                  [match(beetle_name,life$scientificName)],split="\\. ")[[1]]),1)]

# 2. Find seasonality information about the beetle
seasonal <- substr(strsplit(life$Biology[match(beetle_name,life$scientificName)],split = "\\. ")
                   [[1]][1],14,nchar(strsplit(life$Biology[match(beetle_name,life$scientificName)],
                                              split = "\\. ")[[1]][1]))

# 3. Is the species diurnal, nocturnal or crepuscular
dayOrNight <- life$dayOrNight[match(beetle_name,life$scientificName)]

# 4. Is it predatory?
predatory <- life$predatory[match(beetle_name,life$scientificName)]

# 5. Or rare?
conservation_status <- strsplit(life$conservationStatus[match(beetle_name,life$scientificName)],
                                split="\\. ")[[1]][sample(1:length(strsplit(life$conservationStatus
                                                                            [match(beetle_name,life$scientificName)],split="\\. ")[[1]]),1)]

# 6. Can this beetle fly?
flightData <- data.frame(wings=c("Subapterous","Flightless","Subapterous; Flightless",
                                 "Macropterous","Macropterous, Although some Flightless 
                                 Specimens Were Seen that Had Fused Elytra (NY)",
                                 "Macropterous, Capable of Flight","Wing Condition Unknown",
                                 "Unknown."),status=c(rep(1,3),rep(2,3),rep(NA,2)),
                         stringsAsFactors = FALSE)
can_fly <- flightData$status[match(life$wings[match(beetle_name,life$scientificName)],flightData$wings)]
rm(flightData)

if(can_fly==1){
  can_fly <- sample(c("is totally flightless","thinks flying is for chumps","cannot fly",
                      "is earthbound","has so much in common with penguins"),1)
}
if(can_fly==2){
  can_fly <- sample(c("is a pretty great flier","will escape you on the wing",
                      "isn't afraid to fly","has large wings","has enormous wings"),1)
}

# 7. What eats this species?
prey_of <- strsplit(life$predators[match(beetle_name,life$scientificName)],
                    split=",")[[1]][sample(1:length(strsplit(life$predators
                                                             [match(beetle_name,life$scientificName)],split=", ")[[1]]),1)]

# 8. What does this species prey upon?
preys_on <- strsplit(life$prey[match(beetle_name,life$scientificName)],
                     split=",")[[1]][sample(1:length(strsplit(life$prey
                                                              [match(beetle_name,life$scientificName)],split=", ")[[1]]),1)]

# 9. Teneral time frame for this beetle (if known)
most_vulnerable <- strsplit(strsplit(life$Biology[match(beetle_name,
                                                        life$scientificName)],split="Tenerals: ")[[1]][2],split = "\\. ")[[1]][1]

# Choose one of the facts, but don't pick categories where info is unknown or non-existent
facts <- c(can_fly,conservation_status,dayOrNight,dispersal,predatory,
           preys_on,prey_of,most_vulnerable,seasonal)
random_fact <- sample(facts[is.na(facts)==FALSE & tolower(facts)!="unknown"],1); rm(facts)

# An intro phrase
intro_phrase <- c("Wow! Did you know that","Didn't know that","Just learned that",
                  "Found out that","Amazing!")
intro_phrase <- sample(intro_phrase,1)
if(intro_phrase%in%"Wow! Did you know that"){
  punctuation <- "?"
} else {
  punctuation <- "."
}

# A transitional phrase
# Which transition phrase to use?
if(random_fact%in%conservation_status|random_fact%in%dispersal){
  transitional_phrase <- "is a"  
  random_fact <- paste(c(transitional_phrase,tolower(random_fact)),collapse = " ")
}
if(random_fact%in%seasonal){
  transitional_phrase <- "is abundant"  
  random_fact <- paste(c(transitional_phrase,random_fact),collapse = " ")
}
if(random_fact%in%most_vulnerable){
  transitional_phrase <- "is most vulnerable (teneral phase) in"  
  random_fact <- paste(c(transitional_phrase,random_fact),collapse = " ")
}
if(random_fact%in%dayOrNight|random_fact%in%predatory){
  transitional_phrase <- "is"  
  random_fact <- paste(c(transitional_phrase,tolower(random_fact)),collapse = " ")
}
if(random_fact%in%preys_on){
  transitional_phrase <- c("chows down on","eats","noshes on","lunches on",
                           "dines on","loves eating","devours","ingest","consume",
                           "polish off","feast upon","attack","wolfs down",
                           "snack on","gorge on","graze","munch on","nibble on")
  transitional_phrase <- sample(transitional_phrase,1)  
  random_fact <- paste(c(transitional_phrase,tolower(random_fact)),collapse = " ")
}
if(random_fact%in%prey_of){
  transitional_phrase <- c("fear","are prey of","run away from","couldn't be caught dead near",
                           "are hunted by","are the favorite food of","are the favorite meal of",
                           "frequently become lunch for","are badgered by","are dogged by",
                           "are stalked by","are chased by", "are occasionally ambushed by",
                           "never approach","have a lot of anxiety about","have one nightmare:",
                           "have an aversion to","have trepidation around",
                           "get panicked around","get the jitters around")
  transitional_phrase <- sample(transitional_phrase,1)  
  random_fact <- paste(c(transitional_phrase,tolower(random_fact)),collapse = " ")
}

# Adjectives and nouns for a secondary sentence
transitional_phrase2 <- c("What a","Simply a","How great is this","Can't wait to learn more about this",
                          "That's some kind of","There's always some new")
transitional_phrase2 <- sample(transitional_phrase2,1) 
adjectives <- c("majestic","one-of-a-kind","wonderful","beautiful",
                "cool","fascinating","gorgeous","sweet","amazing","interesting")
numb_adjectives <- sample(seq(1,2,1),size=1)
random_adjectives <- sample(adjectives,size=numb_adjectives)

# Choose a noun
nouns <- c("insect","invert","invertebrate","species","organism")
random_noun <- sample(nouns,size=1); rm(nouns)

# Modify transitional phrase
if(transitional_phrase2%in%c("What a","Simply a")){
  if(random_adjectives%in%c("amazing","interesting")){
    transitional_phrase2 <- paste0(transitional_phrase2,"n")
  }
}

# randomly choose a hashtag
hashtags <- c("#coleoptera","#beetles","#insects","#beautifulBeetles","#meetTheBeetles")
random_hashtag <- sample(hashtags,size=1); rm(hashtags)

species_fact <- paste(c(intro_phrase,beetle_name,
                        random_fact),collapse = " ")

if(strsplit(random_fact,"?")[[1]][nchar(random_fact)]!="."){
  species_fact <- paste0(species_fact,punctuation)
}
if(strsplit(random_fact,"?")[[1]][nchar(random_fact)]=="."){
  species_fact <- paste0(substr(species_fact,1,nchar(species_fact)-1),punctuation)
}

complement <- paste(c(transitional_phrase2,random_adjectives,paste0(random_noun,"!"),
                      random_hashtag),collapse = " ")

# Is there a photo? Do I have an attribution for it yet?
if(life$mentions[grepl(beetle_name,life$scientificName)]!=""){
  species_fact <- paste(c(species_fact,"Pic via",life$mentions[match(beetle_name,life$scientificName)]),collapse = " ")
}

# Creating a tweet with the correct character length
if (beetle_photo==""){
  maxTweetLength <- 140
} else {
  maxTweetLength <- 117
}

if ((nchar(species_fact)+nchar(complement))<maxTweetLength){
  tweettxt <- paste(c(species_fact,complement),collapse = " ")
}
if ((nchar(species_fact)+nchar(complement))>maxTweetLength){
  if (nchar(species_fact)<maxTweetLength){
    tweettxt <- species_fact
  }
}

# Tweet text
if(nchar(tweettxt)<maxTweetLength){
  if(nchar(beetle_photo)>0){
    tweet(tweettxt,mediaPath=beetle_photo)
  }
  if(nchar(beetle_photo)==0){
    tweet(tweettxt)
  }
}
rm(tweettxt)
