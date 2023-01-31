id	sub_id	text	to_ask	input_type	args	update_arg
AMLC0002		Active customers as of March 31, 2020?				
	a	Total number of active customers classified as high risk (i.e., received a High Financial Crimes risk rating)	Y	numericInput	min = 0, step = 1, value = 0	value
	d	Total number of active customers classified as high risk (i.e., received a High Financial Crimes risk rating)	N	numericInput	min = 0, step = 1, value = 0	value
	e	Total number of active customers owned or managed by the LoB. Response is formula driven/response is not required.	N	numericInput	min = 0, step = 1, value = 0	value
AMLC0006		"Did the LoB own customers who were individuals?
Provide information related to customers owned by the LoB as of the end of the Review Period (i.e., March 31, 2020)"				
	b	Did the LoB own customers that were individuals who were classified as High Net Worth Individuals?	Y	radioGroupButtons	choices = c('Yes', 'No')	selected
	c	Did the LoB own customers that were individuals who had Private Banking relationships with the LoB?	Y	radioGroupButtons	choices = c('Yes', 'No')	selected
AMLC0013		Select "Yes" or "No" for each higher risk customer type and/or higher risk industry outlined below for the customers owned by the LoB during the Review Period.				
	a	Higher Risk Non-Bank Financial Institutions (NBFIs)	N	radioGroupButtons	choices = c('Yes', 'No')	selected
	b	Broker-Dealer	N	radioGroupButtons	choices = c('Yes', 'No')	selected
	c	Banks Operating under an Offshore Banking License 	N	radioGroupButtons	choices = c('Yes', 'No')	selected
	d	Dealers in precious metals, stones, or jewels/pawn shops	N	radioGroupButtons	choices = c('Yes', 'No')	selected
		Individuals or entities that have been identified as Anti-Social Elements ("ASE") customers:				
	e1	Kanri-saki	N	radioGroupButtons	choices = c('Yes', 'No')	selected
	e2	Watch-saki	N	radioGroupButtons	choices = c('Yes', 'No')	selected
	e3	Confirmed Japanese Mafia	N	radioGroupButtons	choices = c('Yes', 'No')	selected
	f	Individual customers who own accounts within the LoB's country, but reside outside of the LoB's country (i.e., foreign individuals).	N	radioGroupButtons	choices = c('Yes', 'No')	selected
AMLC0019		Of the customers reported in Question 2e, Total Number of Active Customers, how many were onboarded after March 31, 2015? Exclude dormant customers. Include all customers owned by the LoB	N	sliderInput	min = 0, max = 100, value = 50, step = 1, post = '%' 	value
