
# Problem 3 - US Records 

# a. the proportion of email addresses with TLD “.com”
# the proportion : 0.732

us500 <- read.csv("us-500.csv")

email_us <- us500$email
total <- length(email_us) # total number of email addresses

com <- email_us[grepl("@.+\\.com",email_us)]
sub <- length(com) # the number of email addresses with TLD ".com"

sub/total # the proportion of email addresses with TLD “.com”

# b. the proportion of email addresses with at least one non alphanumeric character
# the proportion : 0.506  

non <- email_us[grepl("[^a-zA-Z0-9].*@.+\\..+",email_us)]
sub2 <- length(non) # the number of email addresses with a non alphanumeric character

sub2/total # the proportion of email addresses with a non alphanumeric character

# c. the top 5 most common area codes amongst all phone numbers
# the top 5 : 973, 212, 215, 410, 201

phone1_us <- us500$phone1
phone2_us <- us500$phone2
area1 <- substr(phone1_us, 1, 3) # the area code from phone1
area2 <- substr(phone2_us, 1, 3) # the area code from phone2
sum(area1 != area2) # check whether the two area codes differ or not 
area_table <- table(area1)
area_sorted <- sort(area_table, decreasing = TRUE)
names(head(area_sorted, 5)) # the 5 most common area codes amongt all phone numbers 

# d. a histogram of the log of the apartment numbers for all addresses
  
address <- us500$address
apt_num <- regmatches(address, regexpr("\\d+$", address)) # any number at the end of the an address 
apt_num <- as.numeric(apt_num)
hist(log(apt_num))


# e. Examination whether the apartment numbers appear to follow Benford’s law
# the chi-squared test : p-value = 1.158e-08 
# We can reject the hypothesis that the apartment numbers follow Benford’s law. 

chr <- as.character(apt_num) 
digit <- substr(chr,1,1) # the leading digit of the apartment numbers
digit <- as.numeric(digit)

count <- table(digit) # the number of the leading digit of the apartment numbers
ben_p <- log10(1+(1/1:9)) # the probability of real numbers satifying Benford's law 
chisq.test(count, p = ben_p) # test for compliance with Benford's law
