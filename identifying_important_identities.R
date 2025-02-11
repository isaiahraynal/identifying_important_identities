setwd("~/Desktop/Dissertation/Identity Journal")

########################
# INTERCODER AGREEMENT #
########################

#LOAD PACKAGES
library(readr)
library(dplyr)

#UPLOAD DATA
coder1_coding <- read.csv('Coder1.csv')
coder2_coding <- read.csv('Coder2.csv')

#DATA CLEANING

# below is each number in the file and what variable it means
#1 age
#2 gender
#3 sexuality
#4 race, ethnicity, nationality
#5 education
#6 religion
#7 family role
#8 occupation
#9 political views
#10 hobbies and interests
#11 physical body
#12 personality behavior traits
#13 ambiguous
#14 none
#15 other


# making one demographic variable from age, gender, sexuality, race/ethnicity/nationality, education, religion, family role, occupation
# making one "none of the above" variable from ambiguous, none, and other

coder1_coding2 <- read.csv('Coder1.csv')
coder2_coding2 <- read.csv('Coder2.csv')

both <- read.csv('both.csv')
#combine demographics 1, 2, 3, 4, 5, 6, 7, 8
#both$coder1[both$coder1 == 1] <- 1
both$coder1[both$coder1 == 2] <- 1
both$coder1[both$coder1 == 3] <- 1
both$coder1[both$coder1 == 4] <- 1
both$coder1[both$coder1 == 5] <- 1
both$coder1[both$coder1 == 6] <- 1
both$coder1[both$coder1 == 7] <- 1
both$coder1[both$coder1 == 8] <- 1

#both$coder2[both2$coder2 == 1] <- 1
both$coder2[both$coder2 == 2] <- 1
both$coder2[both$coder2 == 3] <- 1
both$coder2[both$coder2 == 4] <- 1
both$coder2[both$coder2 == 5] <- 1
both$coder2[both$coder2 == 6] <- 1
both$coder2[both$coder2 == 7] <- 1
both$coder2[both$coder2 == 8] <- 1

#combine none of the above 13, 14, 15
#both$coder1[both$coder1 == 13] <- 13
both$coder1[both$coder1 == 14] <- 13
both$coder1[both$coder1 == 15] <- 13

#both$coder2[both$coder2 == 13] <- 13
both$coder2[both$coder2 == 14] <- 13
both$coder2[both$coder2 == 15] <- 13

write.csv(both2, file = "both2.csv")

#################
# MAIN ANALYSIS #
#################

### SETUP ###
data <- read.csv('data_clean.csv')

data$category[data$category == "personality_behaviors_traits"] <- "Personality, Behaviors, and Traits"
data$category[data$category == "demographics"] <- "Demographics"
data$category[data$category == "hobbies_interests"] <- "Hobbies and Interests"
data$category[data$category == "physical_characteristic"] <- "Physical Characteristics"
data$category[data$category == "political_views"] <- "Political Views"
data$category[data$category == "none_of_the_above"] <- "None of the Above"

### TOP 10 IDENTITIES ###

### Most common overall ###

#Raw counts
sum(data$age) #16
sum(data$gender) #15
sum(data$sexuality) #5
sum(data$race..ethnicity..nationality) #15
sum(data$education) #10
sum(data$religion) #22
sum(data$family.role) #38
sum(data$occupation) #17
sum(data$political.views) #10
sum(data$hobbies.and.interests) #59
sum(data$physical.body) #48
sum(data$personality..behaviors..traits) #1037
sum(data$ambiguous) #75
sum(data$none) #58
sum(data$other) #7

#Percentages
#Demographics: age, gender, sexuality, race, education, religion, family, occupation
#None of the above: ambiguous, none, other
sum(data$demographics) / 1432 * 100 #9.636872
sum(data$political.views) / 1432 * 100 #0.698324
sum(data$hobbies.and.interests) / 1432 * 100 #4.120112
sum(data$physical.body) / 1432 * 100 #3.351955
sum(data$personality..behaviors..traits) / 1432 * 100 #72.4162
sum(data$none.of.the.above) / 1432 * 100 #9.776536

sum(data$age) / 138 * 100 #11.5942
sum(data$gender) / 138 * 100 #10.86957
sum(data$sexuality) / 138 * 100 #3.623188
sum(data$race..ethnicity..nationality) / 138 * 100 #10.86957
sum(data$education) / 138 * 100 #7.246377
sum(data$religion) / 138 * 100 #15.94203
sum(data$family.role) / 138 * 100 #27.53623
sum(data$occupation) / 138 * 100 #12.31884

### SELF IDENTITIES ###

library(dplyr)
self <- data %>% filter(version == c('general', 'specific'))
self_drop <- c("top3", "top1")
self <- self[, !(names(self) %in% self_drop)]

### Top 10 Self ###

sum(self$personality..behaviors..traits) / 373 * 100 #67.56032
sum(self$demographics) / 373 * 100 #12.06434
sum(self$hobbies.and.interests) / 373 * 100 #5.36193
sum(self$physical.body) / 373 * 100 #4.825737
sum(self$political.views) / 373 * 100 #0.8042895
sum(self$none.of.the.above) / 373 * 100 #10.99196

#Bar chart
table(self$category)
self_top10 <- data.frame(
  variables = c("Personality, Behaviors, and Traits", "Demographics", "Hobbies & Interests", "Physical Characteristics", "Political Views", "None of the Above"),
  values = c(254, 44, 40, 20, 18, 1)
)
# Plot using ggplot2
ggplot(data=self_top10, aes(x=variables, y=values)) +
  geom_bar(stat="identity")

#Demographics breakdown
sum(self$age) / 45 * 100 #2.222222
sum(self$gender) / 45 * 100 #13.33333
sum(self$sexuality) / 45 * 100 #2.222222
sum(self$race..ethnicity..nationality) / 13458 * 100 #0.04458315
sum(self$education) / 45 * 100 #6.666667
sum(self$religion) / 45 * 100 #17.77778
sum(self$family.role) / 45 * 100 #37.77778
sum(self$occupation) / 45 * 100 #6.666667

### Shared with people ###

table(self$category)

self$shared_family[self$shared_family == 0] <- "Not shared"
self$shared_family[self$shared_family == 1] <- "Shared"
table(self$category, self$shared_family)

self$shared_friend[self$shared_friend == 0] <- "Not shared"
self$shared_friend[self$shared_friend == 1] <- "Shared"
table(self$category, self$shared_friend)

self$shared_acquaintance[self$shared_acquaintance == 0] <- "Not shared"
self$shared_acquaintance[self$shared_acquaintance == 1] <- "Shared"
table(self$category, self$shared_acquaintance)

self$shared_disliked[self$shared_disliked == 0] <- "Not shared"
self$shared_disliked[self$shared_disliked == 1] <- "Shared"
table(self$category, self$shared_disliked)

#Differences between general and specific?
general <- self %>% filter(version == 'general') #164
specific <- self %>% filter(version == 'specific') #213

table(general$category, general$shared_family)
table(general$category, general$shared_friend)
table(general$category, general$shared_acquaintance)
table(general$category, general$shared_disliked)

table(specific$category, specific$shared_family)
table(specific$category, specific$shared_friend)
table(specific$category, specific$shared_acquaintance)
table(specific$category, specific$shared_disliked)


### OTHERS ###

others <- data %>% filter(version %in% c('coworker', 'friend', 'neighbor'))
others_drop <- c("shared_family", "shared_friend", "shared_acquaintance", "shared_disliked")
others <- others[, !(names(others) %in% others_drop)]

table(others$category)
table(others$category, others$top3)
table(others$category, others$top1)

coworker <- others %>% filter(version == 'coworker')
table(coworker$category)
table(coworker$category, coworker$top3)
table(coworker$category, coworker$top1)

friend <- others %>% filter(version == 'friend')
table(friend$category)
table(friend$category, friend$top3)
table(friend$category, friend$top1)

neighbor <- others %>% filter(version == 'neighbor')
table(neighbor$category)
table(neighbor$category, neighbor$top3)
table(neighbor$category, neighbor$top1)


# WORD FREQUENCIES

#load packages
library(tidyr)
library(tidytext)
library(dplyr)
library(textstem)
library(SnowballC)
library(stringr)
library(wordcloud2)
library(RColorBrewer)


clean_traits <-
  data2 %>%
  unnest_tokens(word, trait, drop=T) %>% #make each word have its own line
  anti_join(stop_words, by='word') #remove stopwords

#combine similar words
clean_traits$word[clean_traits$word == "acceptance"] <- "accept"
clean_traits$word[clean_traits$word == "accepting"] <- "accept"
clean_traits$word[clean_traits$word == "adventurous"] <- "adventure"
clean_traits$word[clean_traits$word == "americanness"] <- "american"
clean_traits$word[clean_traits$word == "animals"] <- "animal"
clean_traits$word[clean_traits$word == "artist"] <- "art"
clean_traits$word[clean_traits$word == "artistic"] <- "art"
clean_traits$word[clean_traits$word == "beaultiful"] <- "beauty"
clean_traits$word[clean_traits$word == "beautiful"] <- "beauty"
clean_traits$word[clean_traits$word == "buetiful"] <- "beauty"
clean_traits$word[clean_traits$word == "bisexaul"] <- "bisexual"
clean_traits$word[clean_traits$word == "bossy"] <- "boss"
clean_traits$word[clean_traits$word == "cares"] <- "care"
clean_traits$word[clean_traits$word == "carimg"] <- "care"
clean_traits$word[clean_traits$word == "caring"] <- "care"
clean_traits$word[clean_traits$word == "charismatic"] <- "charisma"
clean_traits$word[clean_traits$word == "children"] <- "child"
clean_traits$word[clean_traits$word == "cleanliness"] <- "clean"
clean_traits$word[clean_traits$word == "cleanly"] <- "clean"
clean_traits$word[clean_traits$word == "comfortable"] <- "comfort"
clean_traits$word[clean_traits$word == "communication"] <- "communicate"
clean_traits$word[clean_traits$word == "communicative"] <- "communicate"
clean_traits$word[clean_traits$word == "communicator"] <- "communicate" 
clean_traits$word[clean_traits$word == "compassate"] <- "compassion"
clean_traits$word[clean_traits$word == "compassionate"] <- "compassion"
clean_traits$word[clean_traits$word == "competence"] <- "competent"
clean_traits$word[clean_traits$word == "complainer"] <- "complain"
clean_traits$word[clean_traits$word == "confidence"] <- "confident"
clean_traits$word[clean_traits$word == "conversationalist"] <- "conversate"
clean_traits$word[clean_traits$word == "conversations"] <- "conversate"
clean_traits$word[clean_traits$word == "craftsman"] <- "craftsperson"
clean_traits$word[clean_traits$word == "craftswomen"] <- "craftsperson"
clean_traits$word[clean_traits$word == "dedicated"] <- "dedicate"
clean_traits$word[clean_traits$word == "dedication"] <- "dedicate"
clean_traits$word[clean_traits$word == "dependable"] <- "depend"
clean_traits$word[clean_traits$word == "dependent"] <- "depend"
clean_traits$word[clean_traits$word == "dogs"] <- "dog"
clean_traits$word[clean_traits$word == "don’tknow"] <- "don’t"
clean_traits$word[clean_traits$word == "doorbell"] <- "door"
clean_traits$word[clean_traits$word == "easygoing"] <- "easy"
clean_traits$word[clean_traits$word == "educate"] <- "education"
clean_traits$word[clean_traits$word == "educated"] <- "education"
clean_traits$word[clean_traits$word == "educational"] <- "education"
clean_traits$word[clean_traits$word == "emapathy"] <- "empathy"
clean_traits$word[clean_traits$word == "empathetic"] <- "empathy"
clean_traits$word[clean_traits$word == "empathic"] <- "empathy"
clean_traits$word[clean_traits$word == "emphatic"] <- "empathy"
clean_traits$word[clean_traits$word == "employed"] <- "employment"
clean_traits$word[clean_traits$word == "enjoys"] <- "enjoy"
clean_traits$word[clean_traits$word == "ethical"] <- "ethic"
clean_traits$word[clean_traits$word == "ethics"] <- "ethic"
clean_traits$word[clean_traits$word == "excited"] <- "excite"
clean_traits$word[clean_traits$word == "exciting"] <- "excite"
clean_traits$word[clean_traits$word == "experienced"] <- "experience"
clean_traits$word[clean_traits$word == "faithful"] <- "faith"
clean_traits$word[clean_traits$word == "forgiveness"] <- "forgive"
clean_traits$word[clean_traits$word == "forgiving"] <- "forgive"
clean_traits$word[clean_traits$word == "freedom"] <- "free"
clean_traits$word[clean_traits$word == "freindly"] <- "friend"
clean_traits$word[clean_traits$word == "friendliness"] <- "friend"
clean_traits$word[clean_traits$word == "friendly"] <- "friend"
clean_traits$word[clean_traits$word == "friends"] <- "friend"
clean_traits$word[clean_traits$word == "friendship"] <- "friend"
clean_traits$word[clean_traits$word == "funby"] <- "fun"
clean_traits$word[clean_traits$word == "funny"] <- "fun"
clean_traits$word[clean_traits$word == "grew"] <- "grow"
clean_traits$word[clean_traits$word == "growing"] <- "grow"
clean_traits$word[clean_traits$word == "haapy"] <- "happy"
clean_traits$word[clean_traits$word == "happiness"] <- "happy"
clean_traits$word[clean_traits$word == "hardworker"] <- "hardwork"
clean_traits$word[clean_traits$word == "hardworkers"] <- "hardwork"
clean_traits$word[clean_traits$word == "hardworking"] <- "hardwork"
clean_traits$word[clean_traits$word == "healthy"] <- "health"
clean_traits$word[clean_traits$word == "helpfulness"] <- "helpful"
clean_traits$word[clean_traits$word == "hobbies"] <- "hobby"
clean_traits$word[clean_traits$word == "honestly"] <- "honest"
clean_traits$word[clean_traits$word == "honesty"] <- "honest"
clean_traits$word[clean_traits$word == "hopeful"] <- "hope"
clean_traits$word[clean_traits$word == "hopwful"] <- "hope"
clean_traits$word[clean_traits$word == "humbleness"] <- "humble"
clean_traits$word[clean_traits$word == "honestly"] <- "honest"
clean_traits$word[clean_traits$word == "hustlers"] <- "hustler"
clean_traits$word[clean_traits$word == "independent"] <- "independence"
clean_traits$word[clean_traits$word == "inteligence"] <- "intelligence"
clean_traits$word[clean_traits$word == "inteligent"] <- "intelligence"
clean_traits$word[clean_traits$word == "intelligent"] <- "intelligence"
clean_traits$word[clean_traits$word == "inttroverted"] <- "introvert"
clean_traits$word[clean_traits$word == "laziness"] <- "lazy"
clean_traits$word[clean_traits$word == "learner"] <- "learn"
clean_traits$word[clean_traits$word == "learning"] <- "learn"
clean_traits$word[clean_traits$word == "listener"] <- "listen"
clean_traits$word[clean_traits$word == "listening"] <- "listen"
clean_traits$word[clean_traits$word == "living"] <- "live"
clean_traits$word[clean_traits$word == "alone"] <- "lone"
clean_traits$word[clean_traits$word == "lonely"] <- "lone"
clean_traits$word[clean_traits$word == "loner"] <- "lone"
clean_traits$word[clean_traits$word == "lovable"] <- "love"
clean_traits$word[clean_traits$word == "loveable"] <- "love"
clean_traits$word[clean_traits$word == "lover"] <- "love"
clean_traits$word[clean_traits$word == "loves"] <- "love"
clean_traits$word[clean_traits$word == "loving"] <- "love"
clean_traits$word[clean_traits$word == "loyalty"] <- "loyal"
clean_traits$word[clean_traits$word == "maritsal"] <- "marital"
clean_traits$word[clean_traits$word == "minded"] <- "mind"
clean_traits$word[clean_traits$word == "mindedness"] <- "mind"
clean_traits$word[clean_traits$word == "morality"] <- "moral"
clean_traits$word[clean_traits$word == "musical"] <- "music"
clean_traits$word[clean_traits$word == "musician"] <- "music"
clean_traits$word[clean_traits$word == "neighbors"] <- "neighbor"
clean_traits$word[clean_traits$word == "opinionated"] <- "opinion"
clean_traits$word[clean_traits$word == "opionated"] <- "opinion"
clean_traits$word[clean_traits$word == "orientated"] <- "orient"
clean_traits$word[clean_traits$word == "orientation"] <- "orient"
clean_traits$word[clean_traits$word == "oriented"] <- "orient"
clean_traits$word[clean_traits$word == "outdoormen"] <- "outdoorsperson"
clean_traits$word[clean_traits$word == "outdoorswoman"] <- "outdoorsperson"
clean_traits$word[clean_traits$word == "pasionet"] <- "passion"
clean_traits$word[clean_traits$word == "passionate"] <- "passion"
clean_traits$word[clean_traits$word == "patience"] <- "patient"
clean_traits$word[clean_traits$word == "peaceful"] <- "peace"
clean_traits$word[clean_traits$word == "people"] <- "person"
clean_traits$word[clean_traits$word == "pets"] <- "pet"
clean_traits$word[clean_traits$word == "political"] <- "politics"
clean_traits$word[clean_traits$word == "punctuality"] <- "punctual"
clean_traits$word[clean_traits$word == "quietness"] <- "quiet" 
clean_traits$word[clean_traits$word == "quitness"] <- "quiet"
clean_traits$word[clean_traits$word == "reader"] <- "read"
clean_traits$word[clean_traits$word == "reliability"] <- "reliable"
clean_traits$word[clean_traits$word == "religious"] <- "religion"
clean_traits$word[clean_traits$word == "respecful"] <- "respect"
clean_traits$word[clean_traits$word == "respectable"] <- "respect"
clean_traits$word[clean_traits$word == "respectful"] <- "respect"
clean_traits$word[clean_traits$word == "responsibility"] <- "responsible"
clean_traits$word[clean_traits$word == "service"] <- "serve"
clean_traits$word[clean_traits$word == "serving"] <- "serve"
clean_traits$word[clean_traits$word == "shared"] <- "share"
clean_traits$word[clean_traits$word == "shares"] <- "share"
clean_traits$word[clean_traits$word == "sleeping"] <- "sleep"
clean_traits$word[clean_traits$word == "sleepy"] <- "sleep"
clean_traits$word[clean_traits$word == "smarts"] <- "smart"
clean_traits$word[clean_traits$word == "smiles"] <- "smile"
clean_traits$word[clean_traits$word == "sociability"] <- "social"
clean_traits$word[clean_traits$word == "sociable"] <- "social"
clean_traits$word[clean_traits$word == "spirituality"] <- "spiritual"
clean_traits$word[clean_traits$word == "sports"] <- "sport"
clean_traits$word[clean_traits$word == "sporty"] <- "sport"
clean_traits$word[clean_traits$word == "stability"] <- "stable"
clean_traits$word[clean_traits$word == "strong"] <- "strength"
clean_traits$word[clean_traits$word == "stylish"] <- "style"
clean_traits$word[clean_traits$word == "supporting"] <- "support"
clean_traits$word[clean_traits$word == "supportive"] <- "support"
clean_traits$word[clean_traits$word == "talkative"] <- "talk"
clean_traits$word[clean_traits$word == "thinker"] <- "think"
clean_traits$word[clean_traits$word == "thinking"] <- "think"
clean_traits$word[clean_traits$word == "teustworthiness"] <- "trust"
clean_traits$word[clean_traits$word == "trustful"] <- "trust"
clean_traits$word[clean_traits$word == "trusting"] <- "trust"
clean_traits$word[clean_traits$word == "trustworthy"] <- "trust"
clean_traits$word[clean_traits$word == "traveled"] <- "travel"
clean_traits$word[clean_traits$word == "traveler"] <- "travel"
clean_traits$word[clean_traits$word == "travrl"] <- "travel"
clean_traits$word[clean_traits$word == "women"] <- "woman"
clean_traits$word[clean_traits$word == "wotty"] <- "worry"
clean_traits$word[clean_traits$word == "yogi"] <- "yoga"

#frequencies of all words 
all_word_frequencies <- clean_traits %>%filter(!is.na(word)) %>% unnest_tokens(word, word) %>% count(word, sort = TRUE)

#filter out responses with certain version and obtain frequencies
friend_treatment <- clean_traits %>% filter(version == "friend")
friend_frequencies <- friend_treatment %>%filter(!is.na(word)) %>% unnest_tokens(word, word) %>% count(word, sort = TRUE)
coworker_treatment <- clean_traits %>% filter(version == "coworker")
coworker_frequencies <- coworker_treatment %>%filter(!is.na(word)) %>% unnest_tokens(word, word) %>% count(word, sort = TRUE)
neighbor_treatment <- clean_traits %>% filter(version == "neighbor")
neighbor_frequencies <- neighbor_treatment %>%filter(!is.na(word)) %>% unnest_tokens(word, word) %>% count(word, sort = TRUE)
specific_treatment <- clean_traits %>% filter(version == "specific")
specific_frequencies <- specific_treatment %>%filter(!is.na(word)) %>% unnest_tokens(word, word) %>% count(word, sort = TRUE)
general_treatment <- clean_traits %>% filter(version == "general")
general_frequencies <- general_treatment %>%filter(!is.na(word)) %>% unnest_tokens(word, word) %>% count(word, sort = TRUE)
self_treatment <- clean_traits %>% filter(version %in% c("general", "specific"))
self_frequencies <- self_treatment %>%filter(!is.na(word)) %>% unnest_tokens(word, word) %>% count(word, sort = TRUE)
other_treatment <- clean_traits %>% filter(version %in% c("coworker", "friend","neighbor"))
other_frequencies <- other_treatment %>%filter(!is.na(word)) %>% unnest_tokens(word, word) %>% count(word, sort = TRUE)
other_neither_treatment <- clean_traits %>% filter(political_party == "Other or Neither")
other_neither_frequencies <- other_neither_treatment %>%filter(!is.na(word)) %>% unnest_tokens(word, word) %>% count(word, sort = TRUE)


################
# DEMOGRAPHICS #
################

#UPLOAD DATA
demographics <- read.csv('respondent_demographics.csv')

#AGE
demographics$age[demographics$age == 18] <- "<20"
demographics$age[demographics$age == 19] <- "<20"
demographics$age[demographics$age == 20] <- "20-29"
demographics$age[demographics$age == 21] <- "20-29"
demographics$age[demographics$age == 22] <- "20-29"
demographics$age[demographics$age == 23] <- "20-29"
demographics$age[demographics$age == 24] <- "20-29"
demographics$age[demographics$age == 25] <- "20-29"
demographics$age[demographics$age == 26] <- "20-29"
demographics$age[demographics$age == 27] <- "20-29"
demographics$age[demographics$age == 28] <- "20-29"
demographics$age[demographics$age == 29] <- "20-29"
demographics$age[demographics$age == 30] <- "30-39"
demographics$age[demographics$age == 31] <- "30-39"
demographics$age[demographics$age == 32] <- "30-39"
demographics$age[demographics$age == 33] <- "30-39"
demographics$age[demographics$age == 34] <- "30-39"
demographics$age[demographics$age == 35] <- "30-39"
demographics$age[demographics$age == 36] <- "30-39"
demographics$age[demographics$age == 37] <- "30-39"
demographics$age[demographics$age == 38] <- "30-39"
demographics$age[demographics$age == 39] <- "30-39"
demographics$age[demographics$age == 40] <- "40-49"
demographics$age[demographics$age == 41] <- "40-49"
demographics$age[demographics$age == 42] <- "40-49"
demographics$age[demographics$age == 43] <- "40-49"
demographics$age[demographics$age == 44] <- "40-49"
demographics$age[demographics$age == 45] <- "40-49"
demographics$age[demographics$age == 46] <- "40-49"
demographics$age[demographics$age == 47] <- "40-49"
demographics$age[demographics$age == 48] <- "40-49"
demographics$age[demographics$age == 49] <- "40-49"
demographics$age[demographics$age == 50] <- "50-59"
demographics$age[demographics$age == 51] <- "50-59"
demographics$age[demographics$age == 52] <- "50-59"
demographics$age[demographics$age == 53] <- "50-59"
demographics$age[demographics$age == 54] <- "50-59"
demographics$age[demographics$age == 55] <- "50-59"
demographics$age[demographics$age == 56] <- "50-59"
demographics$age[demographics$age == 57] <- "50-59"
demographics$age[demographics$age == 58] <- "50-59"
demographics$age[demographics$age == 59] <- "50-59"
demographics$age[demographics$age == 60] <- "60-69"
demographics$age[demographics$age == 61] <- "60-69"
demographics$age[demographics$age == 62] <- "60-69"
demographics$age[demographics$age == 63] <- "60-69"
demographics$age[demographics$age == 64] <- "60-69"
demographics$age[demographics$age == 65] <- "60-69"
demographics$age[demographics$age == 66] <- "60-69"
demographics$age[demographics$age == 67] <- "60-69"
demographics$age[demographics$age == 68] <- "60-69"
demographics$age[demographics$age == 69] <- "60-69"
demographics$age[demographics$age == 70] <- "70-79"
demographics$age[demographics$age == 71] <- "70-79"
demographics$age[demographics$age == 72] <- "70-79"
demographics$age[demographics$age == 73] <- "70-79"
demographics$age[demographics$age == 74] <- "70-79"
demographics$age[demographics$age == 75] <- "70-79"
demographics$age[demographics$age == 76] <- "70-79"
demographics$age[demographics$age == 77] <- "70-79"
demographics$age[demographics$age == 78] <- "70-79"
demographics$age[demographics$age == 81] <- "80-89"
demographics$age[demographics$age == 94] <- "90-99"

#GENDER
demographics$gender[demographics$gender == 1] <- "Male"
demographics$gender[demographics$gender == 2] <- "Female"

#EDUCATION
demographics$education[demographics$education == 1] <- "Some high school or less"
demographics$education[demographics$education == 2] <- "High school graduate"
demographics$education[demographics$education == 3] <- "Other post high school vocational training"
demographics$education[demographics$education == 4] <- "Completed some college but no degree"
demographics$education[demographics$education == 5] <- "Associate's degree"
demographics$education[demographics$education == 6] <- "Bachelor's degree"
demographics$education[demographics$education == 7] <- "Master's or professional degree"
demographics$education[demographics$education == 8] <- "Doctorate degree"

#RACE/ETHNICITY
demographics$ethnicity[demographics$ethnicity == 1] <- "White"
demographics$ethnicity[demographics$ethnicity == 2] <- "Black or African American"
demographics$ethnicity[demographics$ethnicity == 3] <- "American Indian or Alaska Native"
demographics$ethnicity[demographics$ethnicity == 4] <- "Asian"
demographics$ethnicity[demographics$ethnicity == 5] <- "Asian"
demographics$ethnicity[demographics$ethnicity == 6] <- "Asian"
demographics$ethnicity[demographics$ethnicity == 7] <- "Asian"
demographics$ethnicity[demographics$ethnicity == 8] <- "Asian"
demographics$ethnicity[demographics$ethnicity == 9] <- "Asian"
demographics$ethnicity[demographics$ethnicity == 10] <- "Asian"
demographics$ethnicity[demographics$ethnicity == 11] <- "Pacific Islander"
demographics$ethnicity[demographics$ethnicity == 12] <- "Pacific Islander"
demographics$ethnicity[demographics$ethnicity == 13] <- "Pacific Islander"
demographics$ethnicity[demographics$ethnicity == 14] <- "Pacific Islander"
demographics$ethnicity[demographics$ethnicity == 15] <- "Other"
demographics$ethnicity[demographics$ethnicity == 16] <- "Prefer not to answer"

#HISPANIC
demographics$hispanic[demographics$hispanic == 1] <- "No"
demographics$hispanic[demographics$hispanic == 2] <- "Yes"
demographics$hispanic[demographics$hispanic == 3] <- "Yes"
demographics$hispanic[demographics$hispanic == 4] <- "Yes"
demographics$hispanic[demographics$hispanic == 5] <- "Yes"
demographics$hispanic[demographics$hispanic == 6] <- "Yes"
demographics$hispanic[demographics$hispanic == 7] <- "Yes"
demographics$hispanic[demographics$hispanic == 8] <- "Yes"
demographics$hispanic[demographics$hispanic == 9] <- "Yes"
demographics$hispanic[demographics$hispanic == 10] <- "Yes"
demographics$hispanic[demographics$hispanic == 11] <- "Yes"
demographics$hispanic[demographics$hispanic == 12] <- "Yes"
demographics$hispanic[demographics$hispanic == 13] <- "Yes"
demographics$hispanic[demographics$hispanic == 14] <- "Yes"
demographics$hispanic[demographics$hispanic == 15] <- "Prefer not to answer"

#HOUSEHOLD INCOME
demographics$hhi[demographics$hhi == 1] <- "Up to $49,999"
demographics$hhi[demographics$hhi == 2] <- "Up to $49,999"
demographics$hhi[demographics$hhi == 3] <- "Up to $49,999"
demographics$hhi[demographics$hhi == 4] <- "Up to $49,999"
demographics$hhi[demographics$hhi == 5] <- "Up to $49,999"
demographics$hhi[demographics$hhi == 6] <- "Up to $49,999"
demographics$hhi[demographics$hhi == 7] <- "Up to $49,999"
demographics$hhi[demographics$hhi == 8] <- "Up to $49,999"
demographics$hhi[demographics$hhi == 9] <- "$50,000 to $99,999"
demographics$hhi[demographics$hhi == 10] <- "$50,000 to $99,999"
demographics$hhi[demographics$hhi == 11] <- "$50,000 to $99,999"
demographics$hhi[demographics$hhi == 12] <- "$50,000 to $99,999"
demographics$hhi[demographics$hhi == 13] <- "$50,000 to $99,999"
demographics$hhi[demographics$hhi == 14] <- "$50,000 to $99,999"
demographics$hhi[demographics$hhi == 15] <- "$50,000 to $99,999"
demographics$hhi[demographics$hhi == 16] <- "$50,000 to $99,999"
demographics$hhi[demographics$hhi == 17] <- "$50,000 to $99,999"
demographics$hhi[demographics$hhi == 18] <- "$50,000 to $99,999"
demographics$hhi[demographics$hhi == 19] <- "$100,000 to $149,999"
demographics$hhi[demographics$hhi == 20] <- "$100,000 to $149,999"
demographics$hhi[demographics$hhi == 21] <- "$150,000 to $199,999"
demographics$hhi[demographics$hhi == 22] <- "$150,000 to $199,999"
demographics$hhi[demographics$hhi == 23] <- "$200,000 to $249,999"
demographics$hhi[demographics$hhi == 24] <- "$250,000 and more"
demographics$hhi[demographics$hhi == -3105] <- "Prefer not to answer"

#POLITICAL PARTY
demographics$political_party[demographics$political_party == 1] <- "Democrat"
demographics$political_party[demographics$political_party == 2] <- "Democrat"
demographics$political_party[demographics$political_party == 3] <- "Democrat"
demographics$political_party[demographics$political_party == 6] <- "Democrat"
demographics$political_party[demographics$political_party == 4] <- "Independent or neither"
demographics$political_party[demographics$political_party == 7] <- "Independent or neither"
demographics$political_party[demographics$political_party == 5] <- "Republican"
demographics$political_party[demographics$political_party == 8] <- "Republican"
demographics$political_party[demographics$political_party == 9] <- "Republican"
demographics$political_party[demographics$political_party == 10] <- "Republican"

#REGION
demographics$region[demographics$region == 1] <- "Northeast"
demographics$region[demographics$region == 2] <- "Midwest"
demographics$region[demographics$region == 3] <- "South"
demographics$region[demographics$region == 4] <- "West"

#TABLES

table(demographics$age)

table(demographics$gender)

table(demographics$education)

table(demographics$ethnicity)

table(demographics$hispanic)

table(demographics$hhi)

table(demographics$political_party)

table(demographics$region)