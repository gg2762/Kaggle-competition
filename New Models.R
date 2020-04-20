library(ISLR);
library(ggplot2);
library(caret);
library(caTools);
library(ggplot2);
library(dplyr);
library(tidyr);
library(lm.beta);
library(glmnet);
library(leaps);

setwd("/Users/gregoriogalletti/Desktop/Kaggle/")
data=read.csv("analysisData.csv",na.strings = c("NA","N/A","na"," "))
scoringData= read.csv("scoringData.csv", na.strings = c("NA","N/A","na"," "))
View(data)
nrow(data)

#variables already cleaned
host_is_superhost+security_deposit+host_listings_count+host_identity_verified+is_location_exact+accommodates+
  bedrooms+bathrooms+beds+bed_type+minimum_nights+availability_30+availability_60+availability_90+availability_365+
  review_scores_rating+review_scores_accuracy+review_scores_cleanliness+review_scores_checkin+review_scores_communication+review_scores_location+
  review_scores_value+reviews_per_month+number_of_reviews+maximum_nights+security_deposit+neighbourhood_group_cleansed+amenities_char_count+
  host_response_rate_numeric+space_char_count+host_response_rate+cancellation_policy+instant_bookable+
  require_guest_profile_picture+require_guest_phone_verification+number_of_reviews_ltm+host_response_time_char+
  host_response_time_char+calculated_host_listings_count_entire_homes+calculated_host_listings_count_private_rooms+cleaning_fee_numeric+
  first_review+last_review+minimum_nights_avg_ntm+maximum_nights_avg_ntm+extra_people+host_verifications_count+host_response_time+host_since

##HOST VERIFICATION
host_verifications

data%>%summarise(count= sum(is.na(host_verifications)))

data$host_verifications_count<-nchar(as.character(data$host_verifications), type = "chars",keepNA = FALSE)
scoringData$host_verifications_count<-nchar(as.character(scoringData$host_verifications), type = "chars",keepNA = FALSE)

##ACCOMMODATES
data%>%
  summarise(count= sum(is.na(accommodates)))

##BATHROOOMS & BEDROOMS
data%>%
  summarise(count= sum(is.na(bathrooms)))

data%>%
  summarise(count= sum(is.na(bedrooms)))

cor(data$bathrooms,data$bedrooms)

##BEDS
class(data$beds)
data%>%
  summarise(count= sum(is.na(beds)))

data$beds[which(is.na(data$beds))]<- median(data$beds, na.rm = TRUE)
scoringData$beds[which(is.na(scoringData$beds))]<- median(scoringData$beds, na.rm = TRUE)

scoringData%>%
  summarise(count= sum(is.na(beds))) ##verified NAs are gone

##BED TYPE
class(data$bed_type)
data%>%
  summarise(count= sum(is.na(bed_type)))

##PEOPLE PER BEDROOM
?mutate

peopleperbedroom= data%>%mutate(peopleperbedroom= accommodates/bedrooms)

data$peopleperbedroom<- as.numeric(peopleperbedroom)

## Last Review
data$last_review<-as.Date(data$last_review)
scoringData$last_review<-as.Date(scoringData$last_review)
class(data$last_review)
data%>%summarise(count= sum(is.na(last_review)))

data$last_review[which(is.na(data$last_review))]<-  median(data$last_review, na.rm = TRUE)
scoringData$last_review[which(is.na(scoringData$last_review))]<- median(scoringData$last_review, na.rm = TRUE)

data$last_review<-as.numeric(data$last_review)

##FIRST REVIEW
data$first_review<-as.Date(data$first_review)
scoringData$first_review<-as.Date(scoringData$first_review)
class(data$first_review)
data%>%summarise(count= sum(is.na(first_review)))

data$first_review[which(is.na(data$first_review))]<- median(data$first_review, na.rm = TRUE)
scoringData$first_review[which(is.na(scoringData$first_review))]<- median(scoringData$first_review, na.rm = TRUE)

##HOST SINCE

data$host_since<-as.Date(data$host_since)
scoringData$host_since<-as.Date(scoringData$host_since)
class(data$host_since)
data%>%summarise(count= sum(is.na(host_since)))

data$host_since[which(is.na(data$host_since))]<- median(data$host_since, na.rm = TRUE)
scoringData$host_since[which(is.na(scoringData$host_since))]<- median(scoringData$host_since, na.rm = TRUE)

#EXTRA PEOPLE
class(data$extra_people)
data%>%
  summarise(count= sum(is.na(extra_people)))

##MINIMUM NIGHTS
class(data$minimum_nights)
data%>%
  summarise(count= sum(is.na(minimum_nights)))

##MAXIMUM NIGHTS
class(data$maximum_nights)
data%>%
  summarise(count= sum(is.na(maximum_nights)))

##number_of_reviews
class(data$number_of_reviews)
data%>%
  summarise(count= sum(is.na(number_of_reviews)))

cor(data$number_of_reviews,data$number_of_reviews_ltm)


##HOST LISTINGS COUNT
class(data$host_total_listings_count)

data%>%
  summarise(count= sum(is.na(host_total_listings_count)))

data$host_total_listings_count[which(is.na(data$host_total_listings_count))]<- median(data$host_total_listings_count, na.rm = TRUE)
scoringData$host_total_listings_count[which(is.na(scoringData$host_total_listings_count))]<- median(scoringData$host_total_listings_count, na.rm = TRUE)
scoringData%>%
  summarise(count= sum(is.na(host_total_listings_count)))

##AVAILABILITY
class(data$availability_30)
data%>%
  summarise(count= sum(is.na(availability_30)))

class(data$availability_60)
data%>%
  summarise(count= sum(is.na(availability_60)))

class(data$availability_90)
data%>%
  summarise(count= sum(is.na(availability_90)))

class(data$availability_365)
data%>%
  summarise(count= sum(is.na(availability_365)))

cor(data$availability_30,data$availability_365)

##REVIEWS
class(data$review_scores_rating)
data%>%
  summarise(count= sum(is.na(review_scores_rating)))

class(data$review_scores_cleanliness)
data%>%
  summarise(count= sum(is.na(review_scores_cleanliness)))

class(data$review_scores_checkin)
data%>%
  summarise(count= sum(is.na(review_scores_checkin)))

class(data$review_scores_communication)
data%>%
  summarise(count= sum(is.na(review_scores_communication)))

class(data$review_scores_location)
data%>%
  summarise(count= sum(is.na(review_scores_location)))

class(data$review_scores_value)
data%>%
  summarise(count= sum(is.na(review_scores_value)))


##REVIEWS PER MONTH
class(data$reviews_per_month)
data%>%
  summarise(count= sum(is.na(reviews_per_month)))

data$reviews_per_month[which(is.na(data$reviews_per_month))]<- median(data$reviews_per_month, na.rm = TRUE)
scoringData$reviews_per_month[which(is.na(scoringData$reviews_per_month))]<- median(scoringData$reviews_per_month, na.rm = TRUE)

data%>%
  summarise(count= sum(is.na(reviews_per_month)))
scoringData%>%
  summarise(count= sum(is.na(reviews_per_month)))  
## IS LOCATION EXACT
data%>%
  summarise(count= sum(is.na(is_location_exact)))

##SUPER HOST --> not significant
class(data$host_is_superhost)
data%>%summarise(count= sum(is.na(host_is_superhost)))


##HOST IDENTY VERIFIED
class(data$host_identity_verified)
data%>%
  summarise(count= sum(is.na(host_identity_verified)))

##SECURITY DEPOSIT
#security_deposit - remove $#
data$security_deposit <- as.numeric(gsub('[$,]', '', data$security_deposit))
scoringData$security_deposit <- as.numeric(gsub('[$,]', '', scoringData$security_deposit))

data%>%
  summarise(count= sum(is.na(security_deposit)))
scoringData%>%
  summarise(count= sum(is.na(security_deposit)))

data$security_deposit[which(is.na(data$security_deposit))]<- 0
scoringData$security_deposit[which(is.na(scoringData$security_deposit))]<- 0

##NEIGHBOURHOOD GROUP CLENSED
class(data$neighbourhood_group_cleansed)
data%>%
  summarise(count= sum(is.na(neighbourhood_group_cleansed)))

##AMENITIES
#count number of characters 
class(data$amenities)
data$amenities_char_count<-nchar(as.character(data$amenities), type = "chars",keepNA = FALSE)
scoringData$amenities_char_count<-nchar(as.character(scoringData$amenities), type = "chars",keepNA = FALSE)

data%>%
  summarise(count= sum(is.na(amenities_char_count)))
scoringData%>%
  summarise(count= sum(is.na(amenities_char_count)))


## SPACE

class(data$space)
data$space_char_count<-nchar(as.character(data$space), type = "chars",keepNA = FALSE)
scoringData$space_char_count<-nchar(as.character(scoringData$space), type = "chars",keepNA = FALSE)

data%>%
  summarise(count= sum(is.na(space_char_count)))
scoringData%>%
  summarise(count= sum(is.na(space_char_count)))


##CANCELLATION POLICY
class(data$cancellation_policy)

data%>%summarise(count= sum(is.na(cancellation_policy)))
scoringData%>%summarise(count= sum(is.na(cancellation_policy)))

##REQUIRE GUEST PROFILE PIC AND PHONE NUMBER

data%>%summarise(count= sum(is.na(require_guest_profile_picture)))
scoringData%>%summarise(count= sum(is.na(require_guest_profile_picture)))

data%>%summarise(count= sum(is.na(require_guest_phone_verification)))
scoringData%>%summarise(count= sum(is.na(require_guest_phone_verification)))

##HOST RESPONSE RATE
class(data$host_response_rate)

data%>%
  summarise(count= sum(is.na(host_response_rate)))
scoringData%>%
  summarise(count= sum(is.na(host_response_rate)))

data$host_response_rate_numeric<-as.numeric(data$host_response_rate)
scoringData$host_response_rate_numeric<-as.numeric(scoringData$host_response_rate)


data$host_response_rate_numeric[which(is.na(data$host_response_rate_numeric))]<- median(data$host_response_rate_numeric,na.rm = TRUE)
scoringData$host_response_rate_numeric[which(is.na(scoringData$host_response_rate_numeric))]<- median(scoringData$host_response_rate_numeric,na.rm = TRUE)

data%>%
  summarise(count= sum(is.na(host_response_rate_numeric)))
scoringData%>%
  summarise(count= sum(is.na(host_response_rate_numeric)))

##INSTANT BOOKABLE

class(data$instant_bookable)

data%>%
  summarise(count= sum(is.na(instant_bookable)))
scoringData%>%
  summarise(count= sum(is.na(instant_bookable)))

## GUEST INCLUDED
class(data$guests_included)

data%>%
  summarise(count= sum(is.na(guests_included)))
scoringData%>%
  summarise(count= sum(is.na(guests_included)))

## ROOM TYPE

data%>%
  summarise(count= sum(is.na(guests_included)))
scoringData%>%
  summarise(count= sum(is.na(guests_included)))

##CALCULATED HOST LISTINGS 

calculated_host_listings_count+calculated_host_listings_count_entire_homes+
  calculated_host_listings_count_private_rooms+calculated_host_listings_count_shared_rooms
#calculated_host_listings_count
data%>%
  summarise(count= sum(is.na(calculated_host_listings_count)))
scoringData%>%
  summarise(count= sum(is.na(calculated_host_listings_count)))

#calculated_host_listings_count_entire_homes
data%>%
  summarise(count= sum(is.na(calculated_host_listings_count_entire_homes)))
scoringData%>%
  summarise(count= sum(is.na(calculated_host_listings_count_entire_homes)))

#calculated_host_listings_count_private_rooms
data%>%
  summarise(count= sum(is.na(calculated_host_listings_count_private_rooms)))
scoringData%>%
  summarise(count= sum(is.na(calculated_host_listings_count_private_rooms)))

#calculated_host_listings_count_shared_rooms --> removed because showed NA in regression 
data%>%
  summarise(count= sum(is.na(calculated_host_listings_count_shared_rooms)))
scoringData%>%
  summarise(count= sum(is.na(calculated_host_listings_count_shared_rooms)))

class(data$calculated_host_listings_count_shared_rooms)
class(data$calculated_host_listings_count_private_rooms)

#NUMBER OF REVIEWS ITM
data%>%
  summarise(count= sum(is.na(number_of_reviews_ltm)))
scoringData%>%
  summarise(count= sum(is.na(number_of_reviews_ltm)))

##CLEANING FEE
data%>%summarise(count= sum(is.na(cleaning_fee)))
scoringData%>%summarise(count= sum(is.na(cleaning_fee)))

data$cleaning_fee[which(is.na(data$cleaning_fee))]<- 0
scoringData$cleaning_fee[which(is.na(scoringData$cleaning_fee))]<- 0

class(data$cleaning_fee)
data$cleaning_fee_numeric= as.numeric(data$cleaning_fee)
scoringData$cleaning_fee_numeric= as.numeric(scoringData$cleaning_fee)

class(data$cleaning_fee_numeric)
class(scoringData$cleaning_fee_numeric)

data%>%summarise(count= sum(is.na(cleaning_fee_numeric)))
scoringData%>%summarise(count= sum(is.na(cleaning_fee_numeric)))

##HOST RESPONSE TIME

class(data$host_response_time)
data%>%summarise(count= sum(is.na(host_response_time)))

#unfactorize and then re-factorize
data$host_response_time<-as.character(data$host_response_time)
scoringData$host_response_time<-as.character(scoringData$host_response_time)

data$host_response_time[which(is.na(data$host_response_time))]<- "missing"
scoringData$host_response_time[which(is.na(scoringData$host_response_time))]<- "missing"

data$host_response_time<-as.factor(data$host_response_time)
scoringData$host_response_time<-as.factor(scoringData$host_response_time)


##MINIMUM and MAXIMUM NIGHTS AVG 

data%>%summarise(count= sum(is.na(minimum_nights_avg_ntm)))
data%>%summarise(count= sum(is.na(maximum_nights_avg_ntm)))

#####REGULAR EXPRESSION########
#clean amenities
class(data$amenities)
data$amenities<-as.character()
data$amenities[which(is.na(data$amenities))]<- 0
scoringData$amenities[which(is.na(scoringData$amenities))]<- 0

install.packages("stringr")
library(stringr)
#washer
data$amenities_washer<-grepl("Washer",data$amenities,ignore.case=TRUE)
scoringData$amenities_washer<-grepl("Washer",scoringData$amenities,ignore.case=TRUE)

data$amenities_washer<-as.factor(data$amenities_washer)
scoringData$amenities_washer<-as.factor(scoringData$amenities_washer)
#swimming pools
data$amenities_pool<-grepl("Pool",data$amenities,ignore.case=TRUE)
scoringData$amenities_pool<-grepl("Pool",scoringData$amenities,ignore.case=TRUE)

data$amenities_pool<-as.factor(data$amenities_pool)
scoringData$amenities_pool<-as.factor(scoringData$amenities_pool)

#Wifi
data$amenities_Wifi<-grepl("Wifi",data$amenities,ignore.case=TRUE)
scoringData$amenities_Wifi<-grepl("Wifi",scoringData$amenities,ignore.case=TRUE)

data$amenities_Wifi<-as.factor(data$amenities_Wifi)
scoringData$amenities_Wifi<-as.factor(scoringData$amenities_Wifi)

#Parking
data$amenities_Parking<-grepl("Parking",data$amenities,ignore.case=TRUE)
scoringData$amenities_Parking<-grepl("Parking",scoringData$amenities,ignore.case=TRUE)

data$amenities_Parking<-as.factor(data$amenities_Parking)
scoringData$amenities_Parking<-as.factor(scoringData$amenities_Parking)

#Cable
data$amenities_Cable<-grepl("Cable",data$amenities,ignore.case=TRUE)
scoringData$amenities_Cable<-grepl("Cable",scoringData$amenities,ignore.case=TRUE)

data$amenities_Cable<-as.factor(data$amenities_Cable)
scoringData$amenities_Cable<-as.factor(scoringData$amenities_Cable)
#Air Conditioning
data$amenities_AirConditioning<-grepl("Air Conditioning",data$amenities,ignore.case=TRUE)
scoringData$amenities_AirConditioning<-grepl("Air Conditioning",scoringData$amenities,ignore.case=TRUE)

data$amenities_AirConditioning<-as.factor(data$amenities_AirConditioning)
scoringData$amenities_AirConditioning<-as.factor(scoringData$amenities_AirConditioning)

#Doorman
data$amenities_Doorman<-grepl("Doorman",data$amenities,ignore.case=TRUE)
scoringData$amenities_Doorman<-grepl("Doorman",scoringData$amenities,ignore.case=TRUE)

data$amenities_Doorman<-as.factor(data$amenities_Doorman)
scoringData$amenities_Doorman<-as.factor(scoringData$amenities_Doorman)

#Gym
data$amenities_Gym<-grepl("Gym",data$amenities,ignore.case=TRUE)
scoringData$amenities_Gym<-grepl("Gym",scoringData$amenities,ignore.case=TRUE)
data$amenities_Gym<-as.factor(data$amenities_Gym)
scoringData$amenities_Gym<-as.factor(scoringData$amenities_Gym)

#Elevator
data$amenities_Elevator<-grepl("Elevator",data$amenities,ignore.case=TRUE)
scoringData$amenities_Elevator<-grepl("Elevator",scoringData$amenities,ignore.case=TRUE)

data$amenities_Elevator<-as.factor(data$amenities_Elevator)
scoringData$amenities_Elevator<-as.factor(scoringData$amenities_Elevator)


#Heating
data$amenities_Heating<-grepl("Heating",data$amenities,ignore.case=TRUE)
scoringData$amenities_Heating<-grepl("Heating",scoringData$amenities,ignore.case=TRUE)

data$amenities_Heating<-as.factor(data$amenities_Heating)
scoringData$amenities_Heating<-as.factor(scoringData$amenities_Heating)

#Self check-in 
data$amenities_Self<-grepl("Self check-in",data$amenities,ignore.case=TRUE)
scoringData$amenities_Self<-grepl("Self check-in",scoringData$amenities,ignore.case=TRUE)

data$amenities_Self<-as.factor(data$amenities_Self)
scoringData$amenities_Self<-as.factor(scoringData$amenities_Self)
#detector

data$amenities_Detector<-grepl("detector",data$amenities,ignore.case=TRUE)
scoringData$amenities_Detector<-grepl("detector",scoringData$amenities,ignore.case=TRUE)

data$amenities_Detector<-as.factor(data$amenities_Detector)
scoringData$amenities_Detector<-as.factor(scoringData$amenities_Detector)

#clean name
data$name[which(is.na(data$name))]<- 0
scoringData$name[which(is.na(scoringData$name))]<- 0

#Luxury
data$name_luxury<-grepl("Luxury",data$name,ignore.case=TRUE)
scoringData$name_luxury<-grepl("Cable",scoringData$name,ignore.case=TRUE)

data$name_luxury<-as.factor(data$name_luxury)
scoringData$name_luxury<-as.factor(scoringData$name_luxury)

#Great
data$summary_great<-grepl("Great",data$summary,ignore.case=TRUE)
scoringData$summary_great<-grepl("Great",scoringData$name,ignore.case=TRUE)

data$summary_great<-as.factor(data$summary_great)
scoringData$summary_great<-as.factor(scoringData$summary_great)

#parties
data$house_rules_parties<-grepl("Parties",data$house_rules,ignore.case=TRUE)
scoringData$house_rules_parties<-grepl("Parties",scoringData$house_rules,ignore.case=TRUE)

data$house_rules_parties<-as.factor(data$house_rules_parties)
scoringData$house_rules_parties<-as.factor(scoringData$house_rules_parties)

#smoking
data$house_rules_smoking<-grepl("Smoking",data$house_rules,ignore.case=TRUE)
scoringData$house_rules_smoking<-grepl("Smoking",scoringData$house_rules,ignore.case=TRUE)

data$house_rules_smoking<-as.factor(data$house_rules_smoking)
scoringData$house_rules_smoking<-as.factor(scoringData$house_rules_smoking)

#walking
data$summary_walking<-grepl("Walking",data$summary,ignore.case=TRUE)
scoringData$summary_walking<-grepl("Walking",scoringData$summary,ignore.case=TRUE)

data$summary_walking<-as.factor(data$summary_walking)
scoringData$summary_walking<-as.factor(scoringData$summary_walking)

#large
data$space_large<-grepl("Large",data$space,ignore.case=TRUE)
scoringData$space_large<-grepl("Large",scoringData$space,ignore.case=TRUE)

data$space_large<-as.factor(data$space_large)
scoringData$space_large<-as.factor(scoringData$space_large)

####MODEL 15: R2=0.5519,Adj=0.5515

model15=lm(price~accommodates+bedrooms+bathrooms+availability_365+
             beds+minimum_nights+review_scores_rating+review_scores_accuracy+review_scores_cleanliness+review_scores_checkin+review_scores_communication+
             review_scores_location+reviews_per_month+number_of_reviews+neighbourhood_group_cleansed+
             security_deposit+amenities_char_count+host_response_rate_numeric+space_char_count+
             cancellation_policy+calculated_host_listings_count+calculated_host_listings_count_entire_homes+
             calculated_host_listings_count_private_rooms+number_of_reviews_ltm+guests_included+
             +room_type+cleaning_fee_numeric,data)


summary(model15)

###RUN VARIABLES FROM MODEL 15 & 23--> R2=0.5565, Adj=0.556 
model24= lm(price~accommodates+beds+review_scores_location+security_deposit+cancellation_policy+calculated_host_listings_count_private_rooms+
              room_type+bedrooms+minimum_nights+reviews_per_month+amenities_char_count+calculated_host_listings_count+
              number_of_reviews_ltm+cleaning_fee_numeric+bathrooms+review_scores_rating+number_of_reviews+host_response_rate_numeric+
              calculated_host_listings_count_entire_homes+guests_included+availability_365+review_scores_accuracy+
              neighbourhood_group_cleansed+space_char_count+review_scores_cleanliness+review_scores_checkin+
              review_scores_communication+host_total_listings_count+review_scores_value+last_review+minimum_nights_avg_ntm+
              maximum_nights+calculated_host_listings_count_shared_rooms+maximum_nights_avg_ntm+
              extra_people,data)

summary(model24)

###BOOST MODEL 24
library(gbm)
set.seed(617)
boost_model24=gbm(price~accommodates+beds+review_scores_location+security_deposit+cancellation_policy+calculated_host_listings_count_private_rooms+
                    room_type+bedrooms+minimum_nights+reviews_per_month+amenities_char_count+calculated_host_listings_count+
                    number_of_reviews_ltm+cleaning_fee_numeric+bathrooms+review_scores_rating+number_of_reviews+host_response_rate_numeric+
                    calculated_host_listings_count_entire_homes+guests_included+availability_365+review_scores_accuracy+
                    neighbourhood_group_cleansed+space_char_count+review_scores_cleanliness+review_scores_checkin+
                    review_scores_communication+host_total_listings_count+review_scores_value+last_review+minimum_nights_avg_ntm+
                    maximum_nights+calculated_host_listings_count_shared_rooms+maximum_nights_avg_ntm+
                    extra_people,data=data,distribution="gaussian",
                  n.trees = 100000,interaction.depth = 3,shrinkage = 0.001)

summary(boost_model24)

predBoost_model24_scoringdata= predict(boost_model24,newdata=scoringData,n.trees = 100000)

submissionFile = data.frame(id = scoringData$id, price = predBoost_model24_scoringdata)
write.csv(submissionFile, 'submission_Boost_24.csv',row.names = F)

###MODEL 25: USE ONLY SIGNIFICANT VARIABLES FROM MODEL 24: R2=0.544

model25= lm(price~accommodates+beds+review_scores_location+cancellation_policy+
              calculated_host_listings_count_private_rooms+room_type+bedrooms+minimum_nights+amenities_char_count+calculated_host_listings_count+
              number_of_reviews_ltm+bathrooms+review_scores_rating+number_of_reviews+calculated_host_listings_count_entire_homes+
              guests_included+availability_365+review_scores_accuracy+neighbourhood_group_cleansed+space_char_count+review_scores_cleanliness+
              review_scores_checkin+review_scores_communication+host_total_listings_count+review_scores_value+last_review+minimum_nights_avg_ntm+
              maximum_nights+maximum_nights_avg_ntm+extra_people,data)

summary(model25)


##MODEL 26: USE ONLY ***, REMOVE CANCELLATION POLICY. R2=0.5435

model26= lm(price~accommodates+beds+review_scores_location+
              calculated_host_listings_count_private_rooms+room_type+bedrooms+minimum_nights+amenities_char_count+calculated_host_listings_count+
              number_of_reviews_ltm+bathrooms+review_scores_rating+number_of_reviews+calculated_host_listings_count_entire_homes+
              guests_included+availability_365+review_scores_accuracy+space_char_count+review_scores_cleanliness+
              review_scores_checkin+review_scores_communication+host_total_listings_count+review_scores_value+last_review+minimum_nights_avg_ntm+
              maximum_nights+maximum_nights_avg_ntm+extra_people+neighbourhood_group_cleansed,data)

summary(model26)

##MODEL 27, USE ALL VARIABLES IN EXCEL FILE, R2=0.5591, Adj=0.5583
model27= lm(price~accommodates+beds+review_scores_location+security_deposit+cancellation_policy+calculated_host_listings_count_private_rooms+
              room_type+bedrooms+minimum_nights+reviews_per_month+amenities_char_count+calculated_host_listings_count+number_of_reviews_ltm+
              cleaning_fee_numeric+bathrooms+review_scores_rating+number_of_reviews+host_response_rate_numeric+calculated_host_listings_count_entire_homes+
              guests_included+availability_365+review_scores_accuracy+neighbourhood_group_cleansed+space_char_count+review_scores_cleanliness+
              review_scores_checkin+review_scores_communication+host_total_listings_count+review_scores_value+last_review+minimum_nights_avg_ntm+
              maximum_nights+calculated_host_listings_count_shared_rooms+maximum_nights_avg_ntm+extra_people+host_is_superhost+require_guest_profile_picture+
              first_review+host_response_time+require_guest_phone_verification+host_since+host_identity_verified+is_location_exact+
              bed_type+instant_bookable+availability_30+availability_60+host_verifications_count+availability_90,data)

summary(model27)

##MODEL 28-> REMOVE NA ans non significant variables: security_deposit+amenities_char_count+number_of_reviews+
#host_response_rate_numeric+calculated_host_listings_count_shared_rooms+host_is_superhost+require_guest_profile_picture+host_response_time+
#require_guest_phone_verification+bed_type+instant_bookable+host_verifications_count

model28= lm(price~accommodates+beds+review_scores_location+cancellation_policy+calculated_host_listings_count_private_rooms+
              room_type+bedrooms+minimum_nights+reviews_per_month+calculated_host_listings_count+number_of_reviews_ltm+
              cleaning_fee_numeric+bathrooms+review_scores_rating+calculated_host_listings_count_entire_homes+
              guests_included+availability_365+review_scores_accuracy+neighbourhood_group_cleansed+space_char_count+review_scores_cleanliness+
              review_scores_checkin+review_scores_communication+host_total_listings_count+review_scores_value+last_review+minimum_nights_avg_ntm+
              maximum_nights+maximum_nights_avg_ntm+extra_people+
              first_review+host_since+host_identity_verified+is_location_exact+
              availability_30+availability_60+availability_90,data)

summary(model28)

###MODEL 30--> add REGULAR EXPRESSIONS R2=0.573
model30= lm(price~accommodates+beds+review_scores_location+cancellation_policy+calculated_host_listings_count_private_rooms+
              room_type+bedrooms+minimum_nights+reviews_per_month+calculated_host_listings_count+number_of_reviews_ltm+
              cleaning_fee_numeric+bathrooms+review_scores_rating+calculated_host_listings_count_entire_homes+
              guests_included+availability_365+review_scores_accuracy+neighbourhood_group_cleansed+space_char_count+review_scores_cleanliness+
              review_scores_checkin+review_scores_communication+host_total_listings_count+review_scores_value+last_review+minimum_nights_avg_ntm+
              maximum_nights+maximum_nights_avg_ntm+extra_people+
              first_review+host_since+host_identity_verified+is_location_exact+
              availability_30+availability_60+availability_90+amenities_washer+amenities_pool+amenities_Wifi+
              name_luxury+amenities_Doorman+amenities_Gym+amenities_Elevator+amenities_Heating+
              amenities_Self+amenities_Detector+summary_great+house_rules_parties+house_rules_smoking,data)

summary(model30)

class(data$host_since)

data$host_since<-as.numeric(data$host_since)
scoringData$host_since<-as.numeric(scoringData$host_since)
###MODEL 31: USE ONLY SIGNIFICANT VARIABLES
model31= lm(price~accommodates+beds+review_scores_location+cancellation_policy+calculated_host_listings_count_private_rooms+
              room_type+bedrooms+minimum_nights+reviews_per_month+calculated_host_listings_count+number_of_reviews_ltm+
              cleaning_fee_numeric+bathrooms+review_scores_rating+calculated_host_listings_count_entire_homes+
              guests_included+availability_365+review_scores_accuracy+neighbourhood_group_cleansed+space_char_count+review_scores_cleanliness+
              review_scores_checkin+review_scores_communication+host_total_listings_count+review_scores_value+last_review+minimum_nights_avg_ntm+
              maximum_nights+maximum_nights_avg_ntm+extra_people+
              first_review+
              availability_30+availability_60+availability_90+amenities_washer+
              name_luxury+amenities_Doorman+amenities_Gym+amenities_Elevator+
              amenities_Self+summary_great+house_rules_parties+house_rules_smoking+space_large,data)

summary(model31)

###BOOST Model 30
set.seed(617)
boost_model30=gbm(price~accommodates+beds+review_scores_location+cancellation_policy+calculated_host_listings_count_private_rooms+
                    room_type+bedrooms+minimum_nights+reviews_per_month+calculated_host_listings_count+number_of_reviews_ltm+
                    cleaning_fee_numeric+bathrooms+review_scores_rating+calculated_host_listings_count_entire_homes+
                    guests_included+availability_365+review_scores_accuracy+neighbourhood_group_cleansed+space_char_count+review_scores_cleanliness+
                    review_scores_checkin+review_scores_communication+host_total_listings_count+review_scores_value+last_review+minimum_nights_avg_ntm+
                    maximum_nights+maximum_nights_avg_ntm+extra_people+
                    first_review+host_since+host_identity_verified+is_location_exact+
                    availability_30+availability_60+availability_90+amenities_washer+amenities_pool+amenities_Wifi+
                    name_luxury+amenities_Doorman+amenities_Gym+amenities_Elevator+amenities_Heating+
                    amenities_Self+amenities_Detector+amenities_AirConditioning+amenities_Parking+
                    amenities_Cable,data= data,distribution="gaussian",
                  n.trees = 100000,interaction.depth = 3,shrinkage = 0.001)


pred_model30_scoringData=predict(boost_model30,newdata=scoringData,n.trees = 100000)

submissionFile = data.frame(id = scoringData$id, price = pred_model30_scoringData)
write.csv(submissionFile, 'submission_model_30.csv',row.names = F)

data$host_since<-as.numeric(data$host_since)
scoringData$host_since<-as.numeric(scoringData$host_since)
####NEW DF 

dfnew= subset(data, select = c("price","accommodates","beds","review_scores_location","security_deposit","cancellation_policy","calculated_host_listings_count_private_rooms",
                                 "room_type","bedrooms","minimum_nights","reviews_per_month","amenities_char_count","calculated_host_listings_count","number_of_reviews_ltm",
                                 "cleaning_fee_numeric","bathrooms","review_scores_rating","number_of_reviews","host_response_rate_numeric","calculated_host_listings_count_entire_homes",
                                 "guests_included","availability_365","review_scores_accuracy","neighbourhood_group_cleansed","space_char_count","review_scores_cleanliness",
                                 "review_scores_checkin","review_scores_communication","host_total_listings_count","review_scores_value","last_review","minimum_nights_avg_ntm",
                                 "maximum_nights","calculated_host_listings_count_shared_rooms","maximum_nights_avg_ntm","extra_people","host_is_superhost","require_guest_profile_picture",
                                 "first_review","host_response_time","require_guest_phone_verification","host_since","host_identity_verified","is_location_exact",
                                 "bed_type","instant_bookable","availability_30","availability_60","host_verifications_count","availability_90","amenities_washer","amenities_pool","amenities_Wifi",
                                 "name_luxury","amenities_Doorman","amenities_Gym","amenities_Elevator","amenities_Heating",
                                 "amenities_Self","amenities_Detector","amenities_AirConditioning","amenities_Parking",
                                 "amenities_Cable","summary_great"))

dfnew_scoring= subset(scoringData, select = c("accommodates","beds","review_scores_location","security_deposit","cancellation_policy","calculated_host_listings_count_private_rooms",
                               "room_type","bedrooms","minimum_nights","reviews_per_month","amenities_char_count","calculated_host_listings_count","number_of_reviews_ltm",
                               "cleaning_fee_numeric","bathrooms","review_scores_rating","number_of_reviews","host_response_rate_numeric","calculated_host_listings_count_entire_homes",
                               "guests_included","availability_365","review_scores_accuracy","neighbourhood_group_cleansed","space_char_count","review_scores_cleanliness",
                               "review_scores_checkin","review_scores_communication","host_total_listings_count","review_scores_value","last_review","minimum_nights_avg_ntm",
                               "maximum_nights","calculated_host_listings_count_shared_rooms","maximum_nights_avg_ntm","extra_people","host_is_superhost","require_guest_profile_picture",
                               "first_review","host_response_time","require_guest_phone_verification","host_since","host_identity_verified","is_location_exact",
                               "bed_type","instant_bookable","availability_30","availability_60","host_verifications_count","availability_90","amenities_washer","amenities_pool","amenities_Wifi",
                               "name_luxury","amenities_Doorman","amenities_Gym","amenities_Elevator","amenities_Heating",
                               "amenities_Self","amenities_Detector","amenities_AirConditioning","amenities_Parking",
                               "amenities_Cable","summary_great"))



str(dfnew)

library(caret)
set.seed(1031)
split = createDataPartition(y=dfnew$price,p = 0.7,list = F,groups = 100)
train_dfnew = dfnew[split,]
test_dfnew = dfnew[-split,]

str(train_dfnew)

cor(train_dfnew[,-50])


start_mod = lm(price~1,data=train_dfnew)
empty_mod = lm(price~1,data=train_dfnew)
full_mod = lm(price~.,data=train_dfnew)

##HYBRID STEPWISE R2=0.5625,A0.5617

hybridStepwise = step(start_mod,
                      scope=list(upper=full_mod,lower=empty_mod),
                      direction='both')

summary(hybridStepwise)

#FORWARD STEPWISE : R2=0.5625, Adj.=0.5617
forwardStepwise = step(start_mod,
                       scope=list(upper=full_mod,lower=empty_mod),
                       direction='forward')

summary(forwardStepwise)

#BACKWARD STEPWISE
backwardStepwise = step(start_mod,
                        scope=list(upper=full_mod,lower=empty_mod),
                        direction='backward')
summary(backwardStepwise)

##RIDGE MODEL
#install.packages('glmnet')
library(glmnet)
x = model.matrix(price~.-1,data=train_dfnew)
y = train_dfnew$price
ridgeModel = glmnet(x,y,alpha=0)

plot(ridgeModel,xvar='lambda',label=T)

set.seed(617)
cv.ridge = cv.glmnet(x,y,alpha=0) 
plot(cv.ridge)

coef(cv.ridge)

####BUILD LM WITH RIDGE

####DIMENSION REDUCTION--> DID NOT WORK BECAUSE THEY ARE NOT ALL NUMERIC
trainPredictors = train_dfnew[,-50]
testPredictors = test_dfnew[,-50]

pca = prcomp(trainPredictors,scale. = T)
train_components = data.frame(cbind(pca$x[,1:6], price = train_dfnew$price))

###RUN BOOST ON FORWARD STEPWISE 
data$first_review<-as.numeric(data$first_review)
scoringData$first_review<-as.numeric(scoringData$first_review)
dfnew$first_review<-as.numeric(dfnew$first_review)
dfnew_scoring$first_review<-as.numeric(dfnew_scoring$first_review)


library(gbm)
set.seed(617)
model29=gbm(price~accommodates+neighbourhood_group_cleansed+cleaning_fee_numeric+room_type+bathrooms+bedrooms+
              review_scores_location+availability_30+number_of_reviews_ltm+minimum_nights+minimum_nights+last_review+
              review_scores_cleanliness+review_scores_value+review_scores_rating+cancellation_policy+calculated_host_listings_count+
              review_scores_checkin+host_is_superhost+availability_365+guests_included+space_char_count+beds+review_scores_communication+
              is_location_exact+calculated_host_listings_count_shared_rooms+extra_people+review_scores_accuracy+host_total_listings_count+
              minimum_nights_avg_ntm+host_response_time+availability_90+reviews_per_month+first_review+
              host_response_rate_numeric+availability_60+amenities_char_count,data=train_dfnew, distribution="gaussian",
            n.trees = 100000,interaction.depth = 3,shrinkage = 0.001)

summary(model29)

pred_model29_test= predict(model29,newdata=test_dfnew,n.trees = 100000)

rmse29 = sqrt(mean((pred_model29_test-test_dfnew$price)^2)); rmse29

pred_model29_scoringData=predict(model29,newdata=scoringData,n.trees = 100000)


library(gbm)
set.seed(617)
model_final=gbm(price~accommodates+neighbourhood_group_cleansed+cleaning_fee_numeric+room_type+bathrooms+bedrooms+
              review_scores_location+availability_30+number_of_reviews_ltm+minimum_nights+minimum_nights+last_review+
              review_scores_cleanliness+review_scores_value+review_scores_rating+cancellation_policy+calculated_host_listings_count+
              review_scores_checkin+host_is_superhost+availability_365+guests_included+space_char_count+beds+review_scores_communication+
              is_location_exact+calculated_host_listings_count_shared_rooms+extra_people+review_scores_accuracy+host_total_listings_count+
              minimum_nights_avg_ntm+host_response_time+availability_90+reviews_per_month+first_review+
              host_response_rate_numeric+availability_60+amenities_char_count+amenities_Gym+amenities_Elevator+
                name_luxury+amenities_Cable+amenities_Doorman+amenities_washer+amenities_Parking+
                amenities_AirConditioning+summary_great+amenities_Wifi,data=data, distribution="gaussian",
            n.trees = 100000,interaction.depth = 3,shrinkage = 0.001)

summary(model_final)
pred_final=predict(model_final,newdata=scoringData,n.trees = 100000)

###TRY FORWARD SELECTION ON EVERYTHING


submissionFile = data.frame(id = scoringData$id, price = pred_final)
write.csv(submissionFile, 'last_one.csv',row.names = F)

