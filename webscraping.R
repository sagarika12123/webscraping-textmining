##let us first perform web scraping

##we'll use the rvest package developed by hadley wickham
install.packages('rvest')
library(rvest)
url <- 'http://www.imdb.com/search/title?count=100&release_date=2016,2016&title_type=feature'
webpage <- read_html(url)
##now let us scrape rank. title, description, runtime, genre, rating, metascore, votes, gross_earning_in_MIL, director, actor
##using css selectors to scrape the rankings section
rank_data_html <- html_nodes(webpage,'.text-primary')
#converting the ranking data to text
rank_data <- html_text(rank_data_html)
#lets have a look at the rankings
head(rank_data)
##preprocessing the data to make it look like in the desired format- numerical format
rank_data <- as.numeric(rank_data)
head(rank_data)
##now lets grab the titles
#Using CSS selectors to scrape the title section
title_data_html <- html_nodes(webpage,'.lister-item-header a')
#Converting the title data to text
title_data <- html_text(title_data_html)
#Let's have a look at the title
head(title_data)
#using css selectors to scrape the description sections 
description_data_html <- html_nodes(webpage,'.ratings-bar+ .text-muted')
description_data <- html_text(description_data_html)
head(description_data)
#data preprocessing for removing '\n'
description_data <- gsub("  \n","",description_data)
head(description_data)
#Using CSS selectors to scrape the Movie runtime section
runtime_data_html <- html_nodes(webpage,'.text-muted .runtime')
#Converting the runtime data to text
runtime_data <- html_text(runtime_data_html)
#Let's have a look at the runtime
head(runtime_data)
#Data-Preprocessing: removing mins and converting it to numerical
runtime_data<-gsub(" min","",runtime_data)
runtime_data<-as.numeric(runtime_data)
#Let's have another look at the runtime data
head(runtime_data)
#Using CSS selectors to scrape the Movie genre section
genre_data_html <- html_nodes(webpage,'.genre')
#Converting the genre data to text
genre_data <- html_text(genre_data_html)
#Let's have a look at the runtime
head(genre_data)
#Data-Preprocessing: removing \n
genre_data<-gsub("\n","",genre_data)
#Data-Preprocessing: removing excess spaces
genre_data<-gsub(" ","",genre_data)
#taking only the first genre of each movie
genre_data<-gsub(",.*","",genre_data)
#Convering each genre from text to factor
genre_data<-as.factor(genre_data)
#Let's have another look at the genre data
head(genre_data)
#Using CSS selectors to scrape the IMDB rating section
rating_data_html <- html_nodes(webpage,'.ratings-imdb-rating strong')
#Converting the ratings data to text
rating_data <- html_text(rating_data_html)
#Let's have a look at the ratings
head(rating_data)
#Data-Preprocessing: converting ratings to numerical
rating_data<-as.numeric(rating_data)
#Let's have another look at the ratings data
head(rating_data)
#Using CSS selectors to scrape the votes section
votes_data_html <- html_nodes(webpage,'.sort-num_votes-visible span:nth-child(2)')
#Converting the votes data to text
votes_data <- html_text(votes_data_html)
#Let's have a look at the votes data
head(votes_data)
#Data-Preprocessing: removing commas
votes_data<-gsub(",","",votes_data)
#Data-Preprocessing: converting votes to numerical
votes_data<-as.numeric(votes_data)
#Let's have another look at the votes data
head(votes_data)
#Using CSS selectors to scrape the directors section
directors_data_html <- html_nodes(webpage,'.text-muted+ p a:nth-child(1)')
#Converting the directors data to text
directors_data <- html_text(directors_data_html)
#Let's have a look at the directors data
head(directors_data)
#Data-Preprocessing: converting directors data into factors
directors_data<-as.factor(directors_data)
#Using CSS selectors to scrape the actors section
actors_data_html <- html_nodes(webpage,'.lister-item-content .ghost+ a')
#Converting the gross actors data to text
actors_data <- html_text(actors_data_html)
#Let's have a look at the actors data
head(actors_data)
actors_data<-as.factor(actors_data)
#Using CSS selectors to scrape the metascore section
metascore_data_html <- html_nodes(webpage,'.metascore')
#Converting the runtime data to text
metascore_data <- html_text(metascore_data_html)
#Let's have a look at the metascore 
head(metascore_data)
#Data-Preprocessing: removing extra space in metascore
metascore_data<-gsub(" ","",metascore_data)
#Lets check the length of metascore data
length(metascore_data)
#but there are 100 movies, so there are three missing metascores assign for them with the following code
for (i in c(39,40,70)){
  
  a<-metascore_data[1:(i-1)]
  
  b<-metascore_data[i:length(metascore_data)]
  
  metascore_data<-append(a,list("NA"))
  
  metascore_data<-append(metascore_data,b)
  
}
#Data-Preprocessing: converting metascore to numerical
metascore_data<-as.numeric(metascore_data)

#Let's have another look at length of the metascore data

length(metascore_data)
summary(metascore_data)
#Combining all the lists to form a data frame
movies_df<-data.frame(Rank = rank_data, Title = title_data,
                      
                      Description = description_data, Runtime = runtime_data,
                      
                      Genre = genre_data, Rating = rating_data,
                      
                      Metascore = metascore_data, Votes = votes_data,  
                      
                      Director = directors_data, Actor = actors_data)

#Structure of the data frame

str(movies_df)
library('ggplot2')
##plot the runtime vs the count for each genre
qplot(data = movies_df,Runtime,fill = Genre,bins = 30)
#longest runtime vs the rating. 
ggplot(movies_df,aes(x=Runtime,y=Rating))+
  geom_point(aes(size=Votes,col=Genre))
# we can see from the above graph that larger percentage of action movies with less runtime have a high rating

