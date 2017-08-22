#The following is code that I wrote as a part of a online course. The dataset used features information on 30 day 
#mortality rates (percentages) for heart attacks, heart failure, and pneumonia. The data provides information about 
#the quality of care at over 4,000 Medicare-certified hospitals in the U.S. The function, rankhospital, takes three arguments.
#The first argument is the character abbreviated name of a state, the second is an outcome name which takes the 
#value "heart attack", "heart failure", or "pneumonia", and the last argument is the ranking of a hospital
#for that state outcome. The last argument takes the value "best", "worst", or any numerical value.

#The function reads the Outcome of Care Measures.csv file and prints the name of the hospital 
#that has the ranking specified by the num argument for the 30 day mortality rate of the outcome specified. 
#I will be placing  a description of the code below the line the code is written one. 

rankhospital <- function(state, outcome, num) {
#The function takes three arguments that the user inputs: state, outcome, and num
  
	my_data <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
	#The code reads (or imports) a csv file "outcome-of-care-measures.csv". na.strings is used to specify
	#which values in the csv file are missing or not available. stringAsFactors is set to FALSE so that 
	#charachter values are not converted to factors (i.e. values that have text are not saved as numerical data).

	outcomes <- c("heart attack"=11,"heart failure"=17,"pneumonia"=23)
	#This code creates a character vector called "outcomes" and gives it the names of the three outcome inputs. 
	#Those values are then assigned a number which corresponds to the column number within the inputed csv file 
	#for that outcome (i.e the data for the mortality rates of heart attacks is stored in the 11th column of the data set).

	if(state %in% my_data$State & outcome %in% names(outcomes)) {
	  #This code checks the validity of the users arguments
	  
	  my_data2 <- my_data[, c(2,7,outcomes[outcome])]
	  #If the user's arguments are valid, then from the csv file the code subsets or extracts the 2nd column, the 7th column, and then 
	  #whatever column that corresponds to the user's outcome argument.It then creates a new data set with only the extracted columns
	}
	else if(state %in% my_data$State == FALSE) { stop("invalid state")
	  #if the user's argument is not valid because the state abbreviated characters does not exist then they are given
	  #this error code. The function stops and the user will have to input a valid state argument
	}
  else if(outcome %in% names(outcomes) == FALSE) { stop("invalid outcome")
	  #If the user's argument is not valid because of their outcome argument then the user will receive
	  #this error code. The function stops and the user will have to input a valid outcome argument
  }
	my_data3 <- na.omit(my_data2)
	#This code removes all of the records that are missing a piece of data. This is so that hospitals that are missing that particular 
	#outcome are excluded from the set of hospitals when deciding the ranking. This code is placed after extracting the columns
	#so that no necessary data is lost (the csv file contains a plethora of other data, some of which has missing values as well).

	names(my_data3) <- c("Hospital", "State", "Outcome")
	#The code assigns a name to each column in our data set

	my_data4 <- my_data3[order(my_data3$State, my_data3$Outcome, my_data3$Hospital),]
	#The code reorders the data in three ways. It first orders them by state in aplphabetical order,
	#then by the outcome in numerically ascending order, and lastly by the name of the hospital in alphabetical order.
	#This works to group the data by state and to make sure in the case of a tie between hospitals 
	#(outcome rate for two hospitals in that state are equal), the hospital is chosen alphabetically
	#(i.e. if hospitals "b" and "d" are tied for best, then hospital "b" is chosen).
	
	my_data5 <- split(my_data4, my_data4$State)
	#This code splits the data set by state, so that the results is a list of 54 data sets ordered by state.
	#The number is 54 because it includes U.S. properties (DC, the Virgin Islands, Guam and Puerto Rico)
	
	my_data6 <- my_data5[[state]]
	#This code extracts a specific data frame that the user specifies with the state argument (i.e. if the user specifies
	#"TX" for Texas then that corresponding data frame is extracted from the list of 54 data frames)
	
	my_data6$rank <- NA
	#This code creates a new column in my_data6 that has only missing values in it. This line of code is needed
	#to assign a rank to our data. 
	
	my_data6$rank[order(my_data6$Outcome)] <- 1:nrow(my_data6)
	#This code assigns a rank to the outcome mortality rates with the lowest rate receiving the best (i.e. numerically ascending) 
	#rank and stores the value in the newly created rank column.The left hand piece of the code orders the data according 
	#to the outcomes rates in numerically ascending order. Then the right hand piece of code assigns those outcomes a numerical rank
	#and stores the value in the rank column.
	
	if(num == "best") { best <- my_data6[1,]$Hospital
	#This code addresses the scenario that occurs if the user inputs "best" in the num argument. It locates the value in the hospital column
	#with the best (i.e. first) rank and assigns it to best variable. 
	}
	else if(num == "worst") { best <- my_data6[nrow(my_data6),]$Hospital
	#This code addresses the scenario that occurs if the user inputs "worst" in the num argument. It locates the value in the hospital column 
	#with the worst (i.e. last) rank and assigns it to the best variable
	}
	else if(num > nrow(my_data6)) { best <- "NA"
	#This code addresses the scenario that occurs if the number that the user inputs in the num argument is larger than the number of 
	#observations or records in the data set."NA" is assigned to the best variable since that rank does not exist in the data set. 
	}
	else best <- my_data6[num,]$Hospital
	#This code addresses the scenario in which the user's num argument is valid. It goes along the Hospital column, finds the number
	#that the user inputted for their num argument and assigns the name of the hospital to the best variable
	
	best
	#The variable best is then printed to show the name of the hospital with the corresponding rank that the user specified.
}