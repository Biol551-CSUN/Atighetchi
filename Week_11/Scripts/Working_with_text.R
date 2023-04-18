### Working with Words ####
### Created by: Cameron Atighetchi #############
### Updated on: 2022-04-18 ####################
#### Load Libraries ######
library(here)
library(tidyverse)
library(tidytext)
library(wordcloud2)
library(janeaustenr)
#### Functions ##################################
### Intro to Stringr #####################
words <- "This is a string"
words

words_vector<-c("Apples", "Bananas","Oranges")
words_vector

### Manipulation ##################
paste("High temp", "Low pH")

# Add a dash in between the words #######
paste("High temp", "Low pH", sep = "-")

# Remove space in between words
paste0("High temp", "Low pH")

# Working with vectors
shapes <- c("Square", "Circle", "Triangle")
paste("My favorite shape is a", shapes)

two_cities <- c("best", "worst")
paste("It was the", two_cities, "of times.")

# Let's say you want to know how long a string is 

shapes # vector of shapes

str_length(shapes) # how many letters are in each word?

# You can also extract specific characters

seq_data<-c("ATCCCGTC")
str_sub(seq_data, start = 2, end = 4) # extract the 2nd to 4th AA

# Modify strings by substitution

str_sub(seq_data, start = 3, end = 3) <- "A" # add an A in the 3rd position
seq_data

# You can also duplicate patterns in your strings. Here I am duplicating it 2 and 3 times

str_dup(seq_data, times = c(2, 3)) # times is the number of times to duplicate each string

###### WHITESPACE ##################

badtreatments<-c("High", " High", "High ", "Low", "Low")
badtreatments

# Remove white space

str_trim(badtreatments) # this removes both

str_trim(badtreatments, side = "left") # this removes left

#The opposite of str_trim is str_pad, to add white space to either side

str_pad(badtreatments, 5, side = "right") # add a white space to the right side after the 5th character


# add a character instead of white space

str_pad(badtreatments, 5, side = "right", pad = "1") # add a 1 to the right side after the 5th character


##### LOCALE SENSITIVE ###########################

x<-"I love R!"
str_to_upper(x) # Make everything upper case

str_to_lower(x) # Make everything lower case

str_to_title(x) # Make it title case (Cap first letter of each word)


#### PATTERN MATCHING ####################

# View a specific pattern in a vector of strings.

data<-c("AAA", "TATA", "CTAG", "GCTT")

# find all the strings with an A
str_view(data, pattern = "A")

# Detect a specific pattern
str_detect(data, pattern = "A")

str_detect(data, pattern = "AT")

#Locate a pattern

str_locate(data, pattern = "AT")

###### REGEX - Regular Expressions ####################


##### Metacharacters ################################
# Let's say that you have the following set of strings...
vals <- c("a.b", "b.c", "c.d")

# And you want to replace all the "." with a space. Here is how you would do it:
#string, pattern, replace
str_replace(vals, "\\.", " ")

# Let's say we had multiple "." in our character vector
vals<-c("a.b.c", "b.c.d","c.d.e")
str_replace(vals, "\\.", " ")

# tr_replace only replaces the first instance. Let's try str_replace_all()

str_replace_all(vals, "\\.", " ")

##### Sequences ###################################

#Let's subset the vector to only keep strings with digits

val2<-c("test 123", "test 456", "test")
str_subset(val2, "\\d")

#### Character Class ########################

# Let's count the number of lowercase vowels in each string
str_count(val2, "[aeiou]")

# Count digits
str_count(val2, "[0-9]")

# Example: Find the phone numbers

strings<-c("550-153-7578",
           "banana",
           "435.114.7586",
           "home: 672-442-6739")

phone <- "([2-9][0-9]{2})[- .]([0-9]{3})[- .]([0-9]{4})"

# Which strings contain phone numbers?
str_detect(strings, phone)

test<-str_subset(strings, phone)
test

# Now clean up these phone numbers 

test %>%
  str_replace_all(pattern = "\\.", replacement = "-") %>% # replace periods with -
  str_replace_all(pattern = "[a-zA-Z]|\\:", replacement = "") %>% # remove all the things we don't want
  str_trim() # trim the white space

##### TIDY TEXT ##################################

original_books <- austen_books() %>% # get all of Jane Austen's books
  group_by(book) %>%
  mutate(line = row_number(), # find every line
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", # count the chapters (starts with the word chapter followed by a digit or roman numeral)
                                                 ignore_case = TRUE)))) %>% #ignore lower or uppercase
  ungroup() # ungroup it so we have a dataframe again


tidy_books <- original_books %>%
  unnest_tokens(output = word, input = text) # add a column named word, with the input as the text column
head(tidy_books) # there are now >725,000 rows. Don't view the entire thing!

head(get_stopwords())

cleaned_books <- tidy_books %>%
  anti_join(get_stopwords()) # dataframe without the stopwords

head(cleaned_books)

# What's the most common word across all her books?
cleaned_books %>%
  count(word, sort = TRUE)

sent_word_counts <- tidy_books %>%
  inner_join(get_sentiments()) %>% # only keep pos or negative words
  count(word, sentiment, sort = TRUE) # count them


sent_word_counts %>%
  filter(n > 150) %>% # take only if there are over 150 instances of it
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>% # add a column where if the word is negative make the count negative
  mutate(word = reorder(word, n)) %>% # sort it so it gows from largest to smallest
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col() +
  coord_flip() +
  labs(y = "Contribution to sentiment")


words<-cleaned_books %>%
  count(word) %>% # count all the words
  arrange(desc(n))%>% # sort the words
  slice(1:100) #take the top 100
wordcloud2(words, shape = 'triangle', size=0.3) # make a wordcloud out of the top 100 words





