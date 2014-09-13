## Quiz 2 Mike Lennon 

##### Question 1 #####
#Write a function that takes a numeric vector and calculates the mean of the 
#observations in the vector.

calcMean<-function(input){
     sum <-0
     count<-0
     for(value in input){
       sum <- sum + value
       count<- count + 1    
     }
     avg<-sum/count
     avg  
     
}
avgTest<- c(6,7,8)
(calcMean(avgTest))


#####Question 2#####
#. Modify your function in the previous question so that it can handle a
#numeric vector with missing values.


calcMeanMissingValues<-function(input){
  sum <-0
  count<-0
  for(value in input){
    
    if(!is.na(value)){
      sum <- sum + value
      count<- count + 1 
    }
  }
  avg<-sum/count
  avg  
  
}


avgMissingTest<- c(6,NA,8)
calcMeanMissingValues(avgMissingTest)


#####Question 3 #####
#Write a function that takes two numeric input values and calculates 
#the greatest common divisor of the two numbers
calcGCD<-function(a,b){
    
    gcd<-1
    min <- b
    if(a<b){ min <- a}
    for( i in 1:abs(min)){
      if(a%%i==0 && b%%i ==0)
        gcd<-i
    }
    gcd
      
    
}

calcGCD(14,-7)


#####Question 4 #####
#. Write a function that implements Euclid's algorithm 
#(you may need to do a bit of research to find this 
#algorithm) for finding the greatest common divisor of two numeric inputs.


#http://en.wikipedia.org/wiki/Euclidean_algorithm

euclidGCD <- function( m,  n) {
    
  if(m == 0 && n == 0)
    return  -1
  
  if(m < 0) 
    m = -m
  if(n < 0) 
    n = -n
  
   r<-0
  while(n != 0) {
    r <- m %% n
    m <- n
    n <- r
  }
   m
}



euclidGCD(14,-7)



#####Question 5 #####
#Write a function that takes two numeric inputs x and y and calculates 
#x^2*y +2xy - xy^2

quadratic<- function(x,y){
  answer<- x^2*y + 2*x*y - x*y^2
  answer
}

quadratic(2,1)


#####Question 6 #####

priceData<- read.table("C:\\Users\\mike\\Desktop\\Data Aquisition\\week-3-price-data.csv", header = TRUE, sep=",")
#tail(priceData)
#head(priceData)
(priceData)
# There were 28 rows in priceData

modelData<-read.table("C:\\Users\\mike\\Desktop\\Data Aquisition\\week-3-make-model-data.csv", header = TRUE, sep=",")
#tail(modelData)
#head(modelData)
(modelData)
#There are 8 in modelData

answer6<- merge (x=priceData,y=modelData,  
       by.x="ModelNumber",
       by.y="ModelNumber")
(answer6) 
#There were 27 in the answer. I did not expect that. I expected 28

##### Question 7 #####
answer7<- merge (x=priceData,y=modelData,  
                 by.x="ModelNumber",
                 by.y="ModelNumber",
                 all.x = TRUE,
                 all.y = TRUE)
(answer7) 
##There were 28 in this answer. That is what I expected.


##### Question 8 #####
#Take your result from question 7 and subset it so that only the 2010 vehicles are included.

year2010.sub <- subset(answer7, Year== 2010)
nrow(year2010.sub) # num rows  ncol for columns number
(year2010.sub) # There were 14 rows

##### Question 9 #####
#Take your result from question 7 and subset it so that only the red cars that cost more than $10,000 are 
#included.
redOver10K<- subset(answer7, Color== "Red" & Price > 10000)
nrow(redOver10K) # There were 4 rows returned
(redOver10K)

#####Question 10 #####
#Take your result from question 9 and subset it so that the ModelNumber and Color columns are removed.
answer10 <- redOver10K[, c(2, 4, 5, 6, 7, 8)]
(answer10)


#####Question 11 #####
#Write a function that takes as input a character vector and returns a 
#numeric vector with the numbers of characters in each of the elements 
#in the original vector.


#I assumed you meant a vector of strings for this question
numCharachters<- function(list){
  
  numbers<-nchar(list, type = "chars", allowNA = TRUE)
  numbers
  
}

names<-c("Joseph","Frank","Vinny","Sonny")
(numCharachters(names))

#####Question 12 #####
#Write a function that takes two character vectors of equal length and 
#concatenates them element by 
#element with a space as the separator. Have the function die gracefully 
#if the vectors are the same length.

stringConcat<- function(str1, str2){
  if(length(str1)!= length(str2))
    stop("The vectors are different lengths")
   vec<-paste(str1,str2, sep = " ")
    #vec<-vector()
  #for(i in 1:length(str1)){
   # (element<-paste(str1[i],str2[i] , sep = ""))
   
    #append(vec,element)
    
 # }
  vec

}

x<- c("Hello","Boo")
y<- c("there","Who")
(stringConcat(x,y))



##### Question 13 #####
#. Write a function that takes a character vector 
#and returns the substring of three characters that begins with 
#the first vowel in the string. Have the function handle gracefully 
#substrings where this isn't possible.


#substr p36   grep 72
##### Question 15 #####
#Illustrate the code necessary to take a string of MM-DD-YYYY format 
#and convert it to a date


##### Question 16 #####
#. Illustrate the code necessary to take a date and extract the month of the date
dateQ16 <- as.Date("2014-01-03")
as.numeric(format(dateQ16, "%m")) #pull date

##### Question 17 #####
#Create a sequence of all of the dates from January 1, 2005, to December 31, 2014

start <- as.Date("2005-1-1")
end <- as.Date("2014-12-31")
sequence <- seq.Date(to=end, from=start, by="day")





