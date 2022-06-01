# Upper Confidence Bound

# Importing the dataset
dataset <- read.csv('Ads_CTR_Optimisation.csv')

#upper confidence bound

#number of observations
N <- 10000
#number of advertisements
d <- 10
#which ads have been selected - initial empty vector that the algorithm will fill - will make a histogram later to visualize which ad was selected the most 
ads_selected <- integer(0)
#number of times an ad has been selected up to round n- initial empty vector that the algorithm will fill - in the beginning, it is all 0 since no ad has been selected yet
numbers_of_selections <- integer(d)
#sum of rewards of ad i up to round n - in the beginning, it is all 0 since no ad has been selected yet
sums_of_rewards <- integer(d)
#total reward - initialize at 0 since nothing has happened in the beginning and no ads have been selected yet 
total_reward <- 0

#initializing the for loop - starting at 1 and going through the number of observations in the dataset, N
for(n in 1:N){
  ad <- 0 #initializing at 0 since no ad is selected yet
  max_upper_bound <- 0 #initializing at 0 since we have no calculated anything in the very first round
  
  #loop through the ads/thing being tested - the first 10 rounds
  for(i in 1:d){
    if(numbers_of_selections[i] > 0){
      #calculate the Average Reward - note this won't be true until after the first 10 rounds as no add selection will be greater than zero until after the first pass (10 rounds)
      average_reward <- sums_of_rewards[i] / numbers_of_selections[i]
      delta_i <- sqrt(3/2 * log(n) / numbers_of_selections[i])
      upper_bound <- average_reward + delta_i # upper end of Confidence Interval
    } 
    #set upper bound to a very large number to support the 1st round action
      else{
        upper_bound <- 1e400 # piece used for the initialization of the process, where we are testing with the first 10 we'll loop through, basically the first 10 rounds are to seed our data and in this seeding we don't have an upper bound
    } #readjust max upper bound on each round
    if (upper_bound > max_upper_bound){
      max_upper_bound <- upper_bound
      ad <- i
    }
  }
  
  #fill the ads selected vector in order to then be able to see which ad was selected the most once the algorithm is run
  ads_selected <- append(ads_selected, ad) 
  
  #ad is initialized at 0 - if it stays at 0, we will continuously go to the else statement in the for loop and not calculate the UCB. This will always add 1 to the index of the advertisement that is selected - we are keeping track of the number of times an algorithm has been selected 
  numbers_of_selections[ad] <- numbers_of_selections[ad] + 1
  
  #keeping track of the real reward provided by the dataset, which in this case will be 1 or 0 - we are subsetting the dataset to find the value in row n (1:10000) and ad [ad].
  reward <- dataset[n, ad]
  
  #keep track of the rewards received from each individual advertisement
  sums_of_rewards[ad] <- sums_of_rewards[ad] + reward
  
  #keeping track of the total reward across all advertisements
  total_reward <- total_reward + reward
  
}

# Visualizing the results
hist(ads_selected,
     col = 'blue',
     main = 'Histogram of ads selections',
     xlab = 'Ads',
     ylab = 'Number of times each ad was selected')
