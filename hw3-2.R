
# Problem 2 Sakila 

# a. the year of the oldest movie and the number of movies from that year
# the year : 2006 and the number of movies : 1,000 

library(DBI)
library(RSQLite)

sakila <- dbConnect(SQLite(), "sakila_master.db")
dbListTables(sakila)
dbListFields(sakila, "film")

# This function is used to apply SQL to the Sakila database
sakila_sql <- function(query) {
   # this code is from the class notes for STATS 506
  dbGetQuery(sakila, query)
}

# This is the query for the answer of this problem

sakila_sql("
SELECT COUNT(film_id), release_year
  FROM film
 GROUP BY release_year
HAVING release_year = MIN(release_year)
")


# b. the least common genre and the number of movies in that genre 
# the genre : Music, the number of movies : 51

# 1) regular R operations

category_data <- sakila_sql("
                  SELECT category_id, name
                    FROM category 
                 ")
film_data <- sakila_sql("
              SELECT film_id, category_id
                FROM film_category 
              ")
film_data <- merge(film_data, category_data, by = "category_id", all.x=TRUE)
head(film_data)
category_table <- table(film_data$name) # the category table of genre  
min_index <- which.min(category_table) # index for the least common genre 
category_table[min_index] # at least common genre and the number of the movies 

# 2) a single SQL query 

sakila_sql("
SELECT c.name, COUNT(fc.film_id) AS count
  FROM film_category AS fc
  LEFT JOIN category AS c on fc.category_id = c.category_id
 GROUP BY fc.category_id
 ORDER BY count
 LIMIT 1
")

# c. Countries with exactly 13 customers
# the countries : Argentina, Nigeria 

# 1) regular R operations

# estract data from tables
country_data <- sakila_sql("
                 SELECT country_id, country
                   FROM country
                ")
city_data <- sakila_sql("
              SELECT city_id, country_id
                FROM city
             ")
address_data <- sakila_sql("
                 SELECT address_id, city_id
                   FROM address
                ")
customer_data <- sakila_sql("
                  SELECT customer_id, address_id
                    FROM customer
                 ")

# data merger into one data frame "customer_data"
customer_data <- merge(customer_data, address_data, by = "address_id", all.x = TRUE)
customer_data <- merge(customer_data, city_data, by = "city_id", all.x = TRUE) 
customer_data <- merge(customer_data, country_data, by = "country_id", all.x = TRUE)
head(customer_data) 

# coutries with exactly 13 customers
country_table <- table(customer_data$country)
index13 <- which(country_table==13)
country_table[index13]

# 2) a single SQL query 

sakila_sql("
SELECT acn.country, COUNT(cu.customer_id) AS customer
  FROM customer as cu
  LEFT JOIN (
      SELECT a.address_id, cn.country
        FROM address AS a
        LEFT JOIN (
            SELECT c.city_id, c.country_id, n.country
              FROM city as c 
              LEFT JOIN country AS n on c.country_id=n.country_id
             ) AS cn on cn.city_id = a.city_id  
        ) AS acn on acn.address_id = cu.address_id
 GROUP BY country
HAVING COUNT(cu.customer_id) = 13
      ")

