# Tips and Tricks to speed up R code

# this mini-tutorial aims to share some of the common tricks that have
# proved useful for speeding up certain code segments for my past projects
# estimated time: 30 mins

# 0) Load Data
library(nycflights13)
data(flights)

# 1) Slowness of for loops
# using for loops with data frames in R is SLOW, as the assignment copies the data frame in each iteration
# when possible, replace the job with vectorized functions (ifelse, mean, etc)
# let's look an example: find the average distance travelled per flight for flights arriving at "BOS"
# a bad implementation with for loops might look like this:
get_avg_dist_bos <- function(){
    uniq_flights <- c()
    uniq_dist <- c()
    uniq_count <- c()
    for(i in 1:nrow(flights)){
        if(flights[i,'dest']=='BOS'){
            print(i)
            if(!flights[i,'flight'] %in% uniq_flights){
                uniq_flights <- c(uniq_flights,as.character(flights[i,'flight']))
                uniq_dist <- c(uniq_dist,as.numeric(flights[i,'distance']))
                uniq_count <- c(uniq_count,1)
            } else {
                for(j in 1:length(uniq_flights)){
                    if(uniq_flights[j] == flights[i,'flight']){
                        uniq_dist[j] <- uniq_dist[j] + flights[i,'distance']
                        uniq_count[j] <- uniq_count[j] + 1
                    }
                }
            }
        }
    }
    ret <- rbind(uniq_flights,uniq_dist/uniq_count) # ok I cheated here but I'm too lazy
    ret
}

system.time(result <- get_avg_dist_bos()) # ~200s. Feel free to not run this

# 2) Vectorized indexing
# a better way to do so is to filter the data frame via a logical index,
# and then aggregate the results by taking the mean
flights_bos <- flights[flights$dest == 'BOS',]
system.time(result <- aggregate(distance ~ flight, flights_bos, mean)) # ~0.037s :)


# 3) ETL with dplyr
# another way we can do so is to use the dplyr package, 
# which has a read-friendly syntax and runs even faster
library(dplyr)
system.time(result <- flights %>% 
                filter(dest == 'BOS') %>% 
                group_by(flight) %>% 
                summarise(avg_dist=mean(distance))) # 0.029s!
# this dataset is not too big and the query is simple so the differences aren't that pronounced
# but with production scale data, multiple joins and group_by's, dplyr's your best friend


# 4) Merging regex queries
# a common task when dealing with text is to find certain keywords or patterns from the data
# let's find all flights whose tailnum contains any vowel
# a bad implementation using grepl might look like this:
system.time(flights_vowel <- flights[grepl('A',flights$tailnum)|grepl('E',flights$tailnum)|grepl('I',flights$tailnum)|
                                         grepl('O',flights$tailnum)|grepl('U',flights$tailnum),'flight']) # 0.41s
# we can do better by merging the conditions within the regex pattern:
system.time(flights_vowel <- flights[grepl('[AEIOU]',flights$tailnum),'flight']) # 0.11s
# for more help on regex, type ?regex on your R console


# 5) Use for loops, only on matrices
# if you really really must use for loops, say when each iteration depends on the previous result (like MCMC),
# use a matrix to store the values, instead of a data frame. R copies data frames each iteration,
# which is a terrible computational overhead! example: get the average distance from flights (bad use case)

# using the original data frame
avg_dist <- 0
system.time(for(i in 1:nrow(flights)) avg_dist <- avg_dist + flights[i,'distance']/nrow(flights)) # 15.8s
# using a matrix
avg_dist <- 0
dist_matrix <- as.matrix(flights$distance)
system.time(for(i in 1:nrow(dist_matrix)) avg_dist <- avg_dist + dist_matrix[i,1]/nrow(dist_matrix)) # 0.6s


# 6) Misc

# some other options available which are alot more complicated and less worth the time to implement are
# - naive parallel processing with foreach and %dopar% constructs.
#  - requires installing and setting up some dependencies/configs which might require administrator access
#  - does not work for sequential dependencies in code
#  - bear in mind amdahl's law
# - compiling R code
#  - overhead time requirements when compiling code
#  - no speedup if underlying code is pre-compiled (esp all the basic vectorized functions)
# - re-writing slow intensive loops in C/C++
#  - make sure you're not re-inventing the wheel!

# some trade-offs to bear in mind
# - "Premature optimization is the root of all evil" - Donald Knuth
# - always do profiling (time, memory) before deciding which section of code to refactor
# - consider speed-up vs
#  - memory consumption
#  - coding effort
#  - readability
# that said, the earlier tricks introduced generally lead to large speed-ups without giving up any of these


### Practice Time

# using the same flights dataset, write code to answer the following questions:

# 1) Which days in March had more than 700 flights with either a departure or arrival delay?

# 2) Which cities had more than 50% of incoming flights travelling more than 2000(km)?

# 3) Find all flights whose tailnumber either begins with 'N' followed by 5 digits, or ends with 'B'




### Suggested Answers
# 1)
system.time(answer <- flights %>%
                filter(month==3) %>%
                group_by(day) %>%
                summarise(total_delay=sum(ifelse(dep_delay<0|arr_delay<0,1,0),na.rm=T)) %>%
                filter(total_delay > 700))
# 2)
system.time(answer <- flights %>% 
                group_by(dest) %>% 
                summarise(prop_long=mean(ifelse(distance>2000,1,0))) %>% 
                filter(prop_long > 0.5)) # 0.096s

# 3)
system.time(answer <- flights[grepl('([N][0-9]{5})|(B$)',flights$tailnum),'flight'])
