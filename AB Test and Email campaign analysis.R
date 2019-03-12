chooseCRANmirror(graphics=FALSE, ind=1)
knitr::opts_chunk$set(echo = TRUE)
options(tinytex.verbose = TRUE)
library(readr)
library(dplyr)

#'Part 1 (A/B Test)

#' I copied the data in csv format and imported to in r
df_abtest <- read_csv("D:/Naresh/Lumosity/lumosity_ds_takehome/Part1-abTest.csv")
df_abtest

#' Extracting click data for Group A and formating for prop.test
df_grpa_click = df_abtest %>% filter(Group=='A') %>% mutate(noclicks=user- clicks) %>% select(clicks,noclicks) %>% as.matrix() 
prop.test(df_grpa_click,conf.level = 0.95)
#' Click rate of Snapshot Pop-up is better than Generic Pop-up by almost 20%. P value is very small, so difference is statistically significant with confidene level of more than 99% 

#' Extracting Conversion data for Group A and formating for prop.test
df_grpa_conv = df_abtest %>% filter(Group=='A') %>% mutate(noconv=`Viewed purchase page`- Conversion) %>% select(Conversion,noconv) %>% as.matrix() 
prop.test(df_grpa_conv,conf.level = 0.95)
#' Conversion rate of Snapshot Pop-up is better than Generic Pop-up but difference is not statistically significant, hence we can not claim which pop-up is better for conversion rate


#' Extracting click data for Group B and formating for prop.test
df_grpb_click = df_abtest %>% filter(Group=='B') %>% mutate(noclicks=user- clicks) %>% select(clicks,noclicks) %>% as.matrix()
prop.test(df_grpb_click,conf.level = 0.95)
#' Click rate of Personalized pop-up is better than Snapshot & Generic Pop-up by almost 42%. Difference is statistically significant with confidene level of more than 99% 

#' Extracting Conversion data for Group B and formating for prop.test
df_grpb_conv = df_abtest %>% filter(Group=='B') %>% mutate(noconv=`Viewed purchase page`- Conversion) %>% select(Conversion,noconv) %>% as.matrix() 
prop.test(df_grpb_conv,conf.level = 0.95)
#' Conversion rate of Personalized pop-up is better than Snapshot & Generic Pop-up by almost 33% but difference is not statistically significant, hence we can not claim which pop-up is better for conversion rate
#' Also since conversion numbers are small, prop.test gives warning message about approximation may be incorrect. 


#' **Part 2:** 

#' Importing Dataset1 as email data, send date converted to date format while loading, so if there are any error in dates then it will become NA and I can investigate later
df_email_data <- read_csv("D:/Naresh/Lumosity/lumosity_ds_takehome/dataset1.csv",col_types = cols(send_date = col_date(format = "%Y-%m-%d")))
#' Looking at summaty of data to find any potential data issues
summary(df_email_data)
# Getting sense of numbers of open, clicked, converted and types of emails sent
table(df_email_data$opened)
table(df_email_data$clicked)
table(df_email_data$converted)
table(df_email_data$mailing_type)

#' Importing Dataset2 as user data, signup date converted to date format while loading
df_user_data <- read_csv("D:/Naresh/Lumosity/lumosity_ds_takehome/dataset2.csv",col_types = cols(signup_date = col_date(format = "%Y-%m-%d")))
#' Looking at summaty of data to find any potential data issues, Age column has maximum value of 1995 which is not normal
summary(df_user_data)
#' Investigating age column in detail
boxplot(df_user_data$age)
#'  It seems there are two values near 1900 & 2000, It seems they are a Year of birthdate so I will calculate the age by subtracting year from 2018
df_user_data[df_user_data$age>500 & !is.na(df_user_data$age),4] = 2018 - df_user_data[df_user_data$age>500 & !is.na(df_user_data$age),4]
#' Also age is going to 150 which doesnt make sense and to keep upper bound on age I will map all ages above 75 to value 75.
df_user_data[df_user_data$age>75 & !is.na(df_user_data$age),4] = 75
#' Age box plot is shown below, now all age values are capped to maximum of 75
boxplot(df_user_data$age)

#' Importing Dataset3 as game play data, send date converted to date format while loading
df_gameplay_data <- read_csv("D:/Naresh/Lumosity/lumosity_ds_takehome/dataset3.csv",col_types = cols(send_date = col_date(format = "%Y-%m-%d")))
#' Looking at summaty of data to find any potential data issues
summary(df_gameplay_data)
# Maximum number of games played 3 days after send have value as 99999 which seems to be a very large value, checking through box plot
boxplot(df_gameplay_data$num_games_played_3days_after_send)
# Checking other data for the records where games played 3 days after send have value as 99999, data in other columns seems to be  within range
df_gameplay_data[df_gameplay_data$num_games_played_3days_after_send==99999,]
# So, I can check issue by looking at data source but for this case study I will replace these values withe median value of the column
df_gameplay_data[df_gameplay_data$num_games_played_3days_after_send==99999,7]=median(df_gameplay_data$num_games_played_3days_after_send)


#' Checking if users were sent multiple emails 
sum(duplicated(df_email_data$user_id))
sum(duplicated(df_gameplay_data$user_id))
#' 13150 records found where users were send more than 1 mails in last 3 months

#' Counting number of emails sent to each user
df_nos_emails_sent = df_email_data %>% filter(mailing_type != 'no_mail')  %>% group_by(user_id) %>% summarise(no_of_emails=n())

#' Summarizing number of emails sent
df_nos_emails_sent %>% group_by(no_of_emails) %>% summarise(no.of_user=n())
#' 3 users were sent 4 mails, 243 users were sent 3 mails and so on 

#' Checking if the user_id and send_date combination is unique and whether it can be used to join these data frames
sum(duplicated(df_email_data[,c("user_id","send_date")]))
sum(duplicated(df_gameplay_data[,c("user_id","send_date")]))

#' Checking for null values before joining two data frames
sum(is.na(df_email_data[,c("user_id","send_date")]))
sum(is.na(df_gameplay_data[,c("user_id","send_date")]))

#' Joining email data and game play data using user_id and send_date values as there are multiple emails sent to one user

master_file = inner_join(df_email_data,df_gameplay_data,by=c("user_id","send_date"))

#' Adding number of mails to user information to master file 
master_file = left_join(master_file,df_nos_emails_sent,by="user_id")

#' Checking for duplicate on null user id in user data
sum(is.na(df_user_data$user_id))
sum(duplicated(df_user_data$user_id))  # 40120 duplicate records 

#' checking if the data in all columns are duplicated for all the duplicated user
#' Number of duplicate rows of data is matching with number of duplicate user id in 'user_id' column
sum(duplicated(df_user_data))         # 40120 duplicate records  

#' I will keep list of unique user_id before merging the user data with master file
df_unique_uid = unique(df_user_data)

#' Left Joining master file with user data to master file using user_id
master_file = left_join(master_file,df_unique_uid,by="user_id")

#' Checking for missing values in data
sapply(master_file,function(x) sum(is.na(x)))
#' performance_index column has 238266 missing values, gender has 41181 missing value and age has 12895 missing values. no_of_emails missing value of 39216 means that 39216 users were not sent any mail

#' Looking at summaty of data to find any potential data issues
summary(master_file)

#' Analyzing impact of email type on click, signup and conversion rate
#' Creating a summarized view for each email and count of open, clicked and conversion count
df_openrate = master_file %>% group_by(mailing_type.x) %>% summarise(opened_nos = sum(opened), not_opened = n()-opened_nos) %>% filter(mailing_type.x != 'no_mail') %>% select(opened_nos,not_opened) %>% as.matrix() 
prop.test(df_openrate,conf.level = 0.95)
df_clickrate = master_file %>% group_by(mailing_type.x) %>% summarise(clicked_nos = sum(clicked), not_clicked = n()-clicked_nos) %>% filter(mailing_type.x != 'no_mail') %>% select(clicked_nos,not_clicked) %>% as.matrix() 
prop.test(df_clickrate,conf.level = 0.95)
df_convertrate = master_file %>% group_by(mailing_type.x) %>% summarise(conv_nos = sum(converted), not_conv = n()-conv_nos) %>% select(conv_nos,not_conv) %>% as.matrix() 
prop.test(df_convertrate,conf.level = 0.95)
# There are no significant difference in open rate and click rate for conversion and engagement mails, it can be claimed with 95% confidence
# Conversion rate for conversion type of email is almost 155% better than sending no email and even higher than engagement. Also this difference is statistically significant

#' Analyzing the effect of mails on users behaviour
#' creating two new columns to calculate differnce in number of games played after 3 days of email from 3 days prior to email and 1 month prior
master_file$increase_gameplayed_3day_prior = master_file$num_games_played_3days_after_send-master_file$num_games_played_3days_prior_send
master_file$increase_gameplayed_1month_prior = master_file$num_games_played_3days_after_send-master_file$num_games_played_1_month_prior_send
# checking newly created columns for any extreme values. Values of both columns seems to be in range 
boxplot(master_file$increase_gameplayed_1month_prior)
boxplot(master_file$increase_gameplayed_3day_prior)
# For user behaviour analysis I will consider games played 3 days prior and 3 days after sending the email, as it will capture most recent behaviour of user. I will not consider games played 1 month earlier
# Here we can see that total number of games played in either sending email or no email, but sending a email tends to result in larger increase in number of games played   
master_file %>% group_by(mailing_type.x) %>% summarise(game_play_increase = sum(increase_gameplayed_3day_prior)) 
# Statistical testing of user behaviour with email or no email
# creating flag for email increase in number of games played
master_file$increase_flag = ifelse(master_file$increase_gameplayed_3day_prior>0,1,0)
# Test 
df_increase_game = master_file %>% group_by(mailing_type.x) %>% summarise(increase = sum(increase_flag), not_increase = n()-increase) %>% select(increase,not_increase) %>% as.matrix() 
prop.test(df_increase_game,conf.level = 0.95)
# User engagement defination : Users playing more numbers of game 3 days after the email compared to games played 3 days prior to email
# Sending conversion email results in to 48% more user engagement compared to not sending email, Sending engagement email results in to 34% more user engagement compared to not sending email 


# Difference in type of users interacting with the conversion and engagement email
# Checking proportion of conversion based on gender and email type
ftable(master_file$mailing_type.x,master_file$converted,master_file$gender)
prop.table(table(master_file$mailing_type.x,master_file$converted,master_file$gender),margin = 1)
# From table it seems that female as gender is responding better overall comapred to male, lets check it whether it is statistically significant

df_gender_conv_convertrate = master_file %>% filter(mailing_type.x=='conversion') %>% group_by(gender) %>% summarise(conv_nos = sum(converted), not_conv = n()-conv_nos) %>% select(conv_nos,not_conv) %>% as.matrix() 
prop.test(df_gender_conv_convertrate,conf.level = 0.95)
df_gender_engag_convertrate = master_file %>% filter(mailing_type.x=='engagement') %>% group_by(gender) %>% summarise(conv_nos = sum(converted), not_conv = n()-conv_nos) %>% select(conv_nos,not_conv) %>% as.matrix() 
prop.test(df_gender_engag_convertrate,conf.level = 0.95)
df_gender_nomail_convertrate = master_file %>% filter(mailing_type.x=='no_mail') %>% group_by(gender) %>% summarise(conv_nos = sum(converted), not_conv = n()-conv_nos) %>% select(conv_nos,not_conv) %>% as.matrix() 
prop.test(df_gender_nomail_convertrate,conf.level = 0.95)
# Female(prop 1) as gender group converted 25% more than male gender(prop 2), also the difference is statistically significant. Please note prop3 id for null gender, I am ignoring that result


# Analyzing user engagement to mail type by gender, and checking whether it is statistically significant
df_gender_conv_behaviour = master_file %>% filter(mailing_type.x=='conversion') %>% group_by(gender) %>% summarise(increased_nos = sum(increase_flag), not_incr = n()-increased_nos) %>% select(increased_nos,not_incr) %>% as.matrix() 
prop.test(df_gender_conv_behaviour,conf.level = 0.95)
df_gender_engag_behaviour = master_file %>% filter(mailing_type.x=='engagement') %>% group_by(gender) %>% summarise(increased_nos = sum(increase_flag), not_incr = n()-increased_nos) %>% select(increased_nos,not_incr) %>% as.matrix() 
prop.test(df_gender_engag_behaviour,conf.level = 0.95)
df_gender_nomail_behaviour = master_file %>% filter(mailing_type.x=='no_mail') %>% group_by(gender) %>% summarise(increased_nos = sum(increase_flag), not_incr = n()-increased_nos) %>% select(increased_nos,not_incr) %>% as.matrix() 
prop.test(df_gender_nomail_behaviour,conf.level = 0.95)
# From results it is clear that Female users are more engaged and are playing more games compared to Male irrespective of sending any kind of email or no email
# Among 3 types of email, difference between male and female engagement is highest for 'engagement' type of mail

# creating new dataframe by removing records with null age records for user interaction analysis based on age 
df_master_nonull_age = master_file[!(is.na(master_file$age)),]
aggregate(df_master_nonull_age$age,list(df_master_nonull_age$mailing_type.x,df_master_nonull_age$converted),mean)

# Based on data, its clear that user with average age of 52 are purchasing subscription with conversion mail as well as no-mail. Checking statistical significance

# Binning age values in categories
master_file$age_categorized <- case_when(
  master_file$age <= 25 ~ "25 & Below",
  master_file$age > 25 & master_file$age <= 35 ~ "25 - 35",
  master_file$age > 35 & master_file$age <= 45 ~ "35 - 45",
  master_file$age > 45 & master_file$age <= 55 ~ "45 - 55",
  master_file$age > 55 & master_file$age <= 65 ~ "55 - 65",
  master_file$age > 65 & master_file$age <= 150 ~ "65 & Above")

# Analyzing subscription behaviour of users in different age group
df_age_conv_convertrate = master_file %>% filter(mailing_type.x=='conversion') %>% group_by(age_categorized) %>% summarise(conv_nos = sum(converted), not_conv = n()-conv_nos) %>% select(conv_nos,not_conv) %>% as.matrix() 
prop.test(df_age_conv_convertrate,conf.level = 0.95)
# Users in age group of 65 & above(Prop 6 in result) are subscribing at highest rate compared to any other age group, results are statistically significant 
df_age_engag_convertrate = master_file %>% filter(mailing_type.x=='engagement') %>% group_by(age_categorized) %>% summarise(conv_nos = sum(converted), not_conv = n()-conv_nos) %>% select(conv_nos,not_conv) %>% as.matrix() 
prop.test(df_age_engag_convertrate,conf.level = 0.95)
df_age_nomail_convertrate = master_file %>% filter(mailing_type.x=='no_mail') %>% group_by(age_categorized) %>% summarise(conv_nos = sum(converted), not_conv = n()-conv_nos) %>% select(conv_nos,not_conv) %>% as.matrix() 
prop.test(df_age_nomail_convertrate,conf.level = 0.95)
# For no-email and engagement type of email, there is no distinct subscription behaviour among different age group 


df_age_conv_behaviour = master_file %>% filter(mailing_type.x=='conversion') %>% group_by(age_categorized) %>% summarise(increased_nos = sum(increase_flag), not_incr = n()-increased_nos) %>% select(increased_nos,not_incr) %>% as.matrix() 
prop.test(df_age_conv_behaviour,conf.level = 0.95)
df_age_engag_behaviour = master_file %>% filter(mailing_type.x=='engagement') %>% group_by(age_categorized) %>% summarise(increased_nos = sum(increase_flag), not_incr = n()-increased_nos) %>% select(increased_nos,not_incr) %>% as.matrix() 
prop.test(df_age_engag_behaviour,conf.level = 0.95)
# Users in age bracket of 45-65 (Prop 4 & prop 5 in results) are responding better to engagement emails by playing more games than they played earlier, results are statistically significant
df_age_nomail_behaviour = master_file %>% filter(mailing_type.x=='no_mail') %>% group_by(age_categorized) %>% summarise(increased_nos = sum(increase_flag), not_incr = n()-increased_nos) %>% select(increased_nos,not_incr) %>% as.matrix() 
prop.test(df_age_nomail_behaviour,conf.level = 0.95)
# For no-email and subscription type of email, there is no distinct engagement behaviour among different age group 

# Checking for Impact of performance_index on conversion and engagement behaviour
boxplot(master_file$performance_index~master_file$converted,col="yellow")
boxplot(master_file$performance_index~master_file$increase_flag,col="yellow")
# From box plot we can see that there is some difference in median values of performance_index of users who subscribed and others

# To investigate further, I created new column to categorize the score
master_file$performance_index_categ <- case_when(
  master_file$performance_index <= 250 ~ "250 & Below",
  master_file$performance_index > 250 & master_file$performance_index <= 500 ~ "250 - 500",
  master_file$performance_index > 500 & master_file$performance_index <= 750 ~ "500 - 750",
  master_file$performance_index > 750 & master_file$performance_index <= 1000 ~ "750 - 1000",
  master_file$performance_index > 1000 & master_file$performance_index <= 1250 ~ "1000 - 1250",
  master_file$performance_index > 1250 & master_file$performance_index <= 2000 ~ "1250 & Above")

# Analyzing subscription behaviour of users in different performance_index group
df_score_conv_convertrate = master_file %>% filter(mailing_type.x=='conversion') %>% group_by(performance_index_categ) %>% summarise(conv_nos = sum(converted), not_conv = n()-conv_nos) %>% select(conv_nos,not_conv) %>% as.matrix() 
prop.test(df_score_conv_convertrate,conf.level = 0.95)
# Prop.1 1000 - 1250                 
# Prop.2 1250 & Above                
# Prop.3 250 - 500                    
# Prop.4 250 & Below                  
# Prop.5 500 - 750                    
# Prop.6 750 - 1000                   
# Prop.7 <NA>                        
# As we can see in result, higher the performance_index score higher the chance of subscription to services. with prop 2 which is users with performance_index greater than 1250 has 7% subscription rate which is very high
# Also 1000-1250 and 750-1000 performance_index scores also have high subscription rate

# Checking for impact of performance_index score on user engagement behaviour
df_score_engag_behaviour = master_file %>% filter(mailing_type.x=='engagement') %>% group_by(performance_index_categ) %>% summarise(increased_nos = sum(increase_flag), not_incr = n()-increased_nos) %>% select(increased_nos,not_incr) %>% as.matrix() 
prop.test(df_age_engag_behaviour,conf.level = 0.95)
# User with performance_index score of 500-750 and 250 & Below are engaging more with engagement email type


# As additional check I ran logistic regression for significance and impact of variables

converted_model=glm(converted~mailing_type.x+num_games_played_3days_prior_send+num_games_played_1_month_prior_send+signup_date+gender+age,family = binomial(logit),data = master_file)
summary(converted_model)
# From summary also it is evident that female as gender group are subscribing to our service (as genderm has negative estimate)
# Also we see that no_mail and engagement mail has negative estimate value which means subscription mails are resulting in higher click rate
# Age has positive estimate, which means as age increases, user subscribing rate increases.
# Logistic regression in a way validates finding/insights we have gathered till now

# Validating the logistic model results
library(caret)
# Use click_model model to make predictions
pdata = predict(converted_model, newdata = master_file, type = "response")
# use caret and compute a confusion matrix
confusionMatrix(table(data = as.numeric(pdata>0.5), reference = master_file$converted))
# However, logistic regerssion accuracy is not that great ffor prediction but we can use its summary statistics to validate our findings

#' In Summary of data analysis:
#' Errors In Data:
#' In user data, for two records age was given birth year which I changed it to age by subtracting it with todays date
#' Also age values were till 150 and seemed to be not real so I capped any value in age greater than 75 to 75
#' Duplicate records in user data
#' In gameplay data, 3 days after mail column had value as 99999 which doesnt make sense, I would ideally do the RCA for this data error but for this case study I replaced it with median value of the column
#' There were many misssing values in performance_index, age, gender columns

#' Effect of Email-type:
#' On Conversion rate:
#' There are no significant difference in open rate and click rate for conversion and engagement mails, it can be claimed with 95% confidence
#' Conversion rate for conversion type of email is almost 155% better than sending no email and even higher than engagement. Also this difference is statistically significant
#' On User engagement rate:
#' User engagement defination: Users playing more numbers of game 3 days after the email compared to games played 3 days prior to email
#' Sending conversion email results in to 48% more user engagement compared to not sending email, Sending engagement email results in to 34% more user engagement compared to not sending email 

#' User Interaction with different type of email:
#' Female as gender group converted 25% more than male gender on sending conversion type email, also the difference is statistically significant.
#' Female as gender group also had better engagement rate post engagement type email compared to male
#' Users in age group of 65 & above(Prop 6 in result) are subscribing at highest rate compared to any other age group, results are statistically significant
#' Users in age bracket of 45-65 are responding better to engagement emails by playing more games than they played earlier, results are statistically significant
#' Higher the performance_index score higher the chance of subscription to services. users with performance_index greater than 1250 has 7% subscription rate which is very high
#' Also 1000-1250 and 750-1000 performance_index scores also have high subscription rate
#' User with performance_index score of 500-750 and 250 & Below are engaging more with engagement email type

# Recommendation to product and email team
# For better conversion rate for conversion type of email, we should focus on High performance index, female users with age group above 65. These user should be first priority.
# For better engagement rate post engagement mail, users with low performance index score(500 to 750), female in age group of 45-65.
# email team can send emails to group with score between 500-750 regaring how to improve performance_score and once they reach around 1250 score then we can send them subscription email
# Product team should focus on how to engage more male users


# Other questions that I would like to answer with this data set are
# 1. Combining user gender, age, performance_index and years of engagement into clusters and doing cluster based analysis to generalize the findings and recommendation
# 2. If, emailing data of one year is provided then I can look for seasonality in subscription/engagement i.e. during which time of year if the particular age group is engaging more or subscribing more


# Other data that can help in my analysis
# 1. Information of content of email and whether subscription email was of same time
# 2. Complete performance_index data will help in analyzing engagement and subscription behaviour of high performance_index user in greater detail
# 3. Geography specific data if available can help in finding any interesting patterns related to geography


