library(tidyverse)
library(rvest)
library(httr)

setwd(dirname(rstudioapi::getActiveDocumentContext()[[2]]))

# Willhaben Newest --------------------------------------
pages <- 1:999

start_urls <- paste0("https://www.willhaben.at/iad/immobilien/mietwohnungen/mietwohnung-angebote?xtor=SEC-311-GOO&pls_source=google&pls_medium=Social&pls_campaign=neubau&g=Both&locations=austria&site=willhabenat&gclid=Cj0KCQiAw8OeBhCeARIsAGxWtUxNHiDGZEhSsqxjkHwEd_2CCq60RX7VHu1j1_amT7biXqb47T7Fw_oaAlYtEALw_wcB&sfId=c7bdd018-c536-4fdf-8c68-41336f3b3fe0&isNavigation=true&sort=1&areaId=900&page=",
                     pages,
                     "&rows=5") %>% 
  as_tibble() %>% 
  rename(url = value) %>% 
  mutate(subpage_content = NA)

start_urls

# Trying for loop which binds rows
tmp <- start_urls

subpages_content <- vector(mode = "list", length = nrow(tmp))

for (x in 1:nrow(tmp)){
  
  url_tmp <- tmp[x, "url"] %>% 
    pull()
  
  tryCatch(
    subpages_content[[x]] <- url_tmp %>%
      GET(., timeout(90)) %>% 
      read_html(.),
    error = function(e){NA}
  )
  
  print(paste("Link", x, "retrieved"))
  
  # Wait a couple of seconds to go again, got banned at around 700 in 15 min
  Sys.sleep(15)
  
}

subpage_content <- tibble(listing = subpages_content,
                          is_null = map(listing, is.null)) %>%
  unnest(is_null) %>%
  filter(is_null == FALSE) %>% 
  select(-is_null)

# Get content
tmp <- subpage_content %>% 
  mutate(
    listing_no = 1:nrow(.),
    title = map(listing, function(.x){
      return(.x %>% 
               html_nodes(".jFSiUe") %>%
               html_text())
    }),
    price = map(listing, function(.x){
      return(.x %>% 
               html_nodes(".YNJYj") %>%
               html_text())
    }),
    sqm = map(listing, function(.x){
      return(.x %>% 
               html_nodes(".kGvvYn:nth-child(1) .hSkBmS") %>%
               html_text())
    }),
    rooms = map(listing, function(.x){
      return(.x %>% 
               html_nodes(".kGvvYn+ .kGvvYn .hSkBmS") %>%
               html_text())
    }),
    address = map(listing, function(.x){
      return(.x %>% 
               html_nodes(".cNKJte .jkEvBe") %>%
               html_text())
    }),
    landlord = map(listing, function(.x){
      return(.x %>% 
               html_nodes(".hFBTuC") %>%
               html_text())
    })
  ) %>% 
  select(-c(listing))

tmp %>% 
  write_csv("Willhaben_newest_dirty.csv")

tmp %>%
  mutate(length_title = map(title, ~ length(.x)),
         length_price = map(price, ~ length(.x)),
         length_sqm = map(sqm, ~ length(.x)),
         length_rooms = map(rooms, ~ length(.x)),
         length_address = map(address, ~ length(.x)),
         length_landlord = map(landlord, ~ length(.x)),) %>%
  unnest(contains("length_")) %>%
  mutate(length_total = length_title +
                        length_price +
                        length_sqm +
                        length_rooms +
                        length_address +
                        length_landlord) %>%
  filter(length_total == 30) %>%
  select(-contains("length_")) %>% 
  unnest(everything()) %>% 
  write_csv("Willhaben_newest_dirty.csv")
  
# Willhaben oldest --------------------------------------
pages <- 1:800

start_urls <- paste0("https://www.willhaben.at/iad/immobilien/mietwohnungen/mietwohnung-angebote?xtor=SEC-311-GOO&pls_source=google&pls_medium=Social&pls_campaign=neubau&g=Both&locations=austria&site=willhabenat&gclid=Cj0KCQiAw8OeBhCeARIsAGxWtUxNHiDGZEhSsqxjkHwEd_2CCq60RX7VHu1j1_amT7biXqb47T7Fw_oaAlYtEALw_wcB&sfId=c7bdd018-c536-4fdf-8c68-41336f3b3fe0&isNavigation=true&sort=0&areaId=900&page=",
                     pages,
                     "&rows=5") %>% 
  as_tibble() %>% 
  rename(url = value) %>% 
  mutate(subpage_content = NA)

start_urls

# Read in html from each subpage (Danger of timeout here)
# subpage_urls <- start_urls %>%
#   mutate(subpage_content = map(url, function(.x) {
#     return(GET(.x, timeout(60)) %>%
#              read_html(.))
#   }))

# Trying for loop which binds rows

tmp_oldest <- start_urls

subpages_content <- vector(mode = "list", length = nrow(tmp_oldest))

for (x in 1:nrow(tmp_oldest)){
  
  url_tmp_oldest <- tmp_oldest[x, "url"] %>% 
    pull()
  
  tryCatch(
    subpages_content[[x]] <- url_tmp_oldest %>%
      GET(., timeout(90)) %>% 
      read_html(.),
    error = function(e){NA}
  )
  
  print(paste("Link", x, "retrieved"))
  
  # Wait a couple of seconds to go again, got banned at around 700 in 15 min
  Sys.sleep(15)
  
}

subpage_content <- tibble(listing = subpages_content,
                          is_null = map(listing, is.null)) %>%
  unnest(is_null) %>%
  filter(is_null == FALSE) %>% 
  select(-is_null)

# Get content
tmp_oldest <- subpage_content %>% 
  mutate(
    listing_no = 1:nrow(.),
    title = map(listing, function(.x){
      return(.x %>% 
               html_nodes(".jFSiUe") %>%
               html_text())
    }),
    price = map(listing, function(.x){
      return(.x %>% 
               html_nodes(".YNJYj") %>%
               html_text())
    }),
    sqm = map(listing, function(.x){
      return(.x %>% 
               html_nodes(".kGvvYn:nth-child(1) .hSkBmS") %>%
               html_text())
    }),
    rooms = map(listing, function(.x){
      return(.x %>% 
               html_nodes(".kGvvYn+ .kGvvYn .hSkBmS") %>%
               html_text())
    }),
    address = map(listing, function(.x){
      return(.x %>% 
               html_nodes(".cNKJte .jkEvBe") %>%
               html_text())
    }),
    landlord = map(listing, function(.x){
      return(.x %>% 
               html_nodes(".hFBTuC") %>%
               html_text())
    })
  ) %>% 
  select(-c(listing))

tmp_oldest %>% 
  write_csv("Willhaben_oldest_dirty.csv")

tmp %>%
  mutate(length_title = map(title, ~ length(.x)),
         length_price = map(price, ~ length(.x)),
         length_sqm = map(sqm, ~ length(.x)),
         length_rooms = map(rooms, ~ length(.x)),
         length_address = map(address, ~ length(.x)),
         length_landlord = map(landlord, ~ length(.x)),) %>%
  unnest(contains("length_")) %>%
  mutate(length_total = length_title +
                        length_price +
                        length_sqm +
                        length_rooms +
                        length_address +
                        length_landlord) %>%
  filter(length_total == 30) %>%
  select(-contains("length_")) %>% 
  unnest(everything()) %>% 
  saveRDS("Willhaben_oldest_dirty.RData")


tmp %>% 
  bind_rows(tmp_oldest) %>% 
  mutate(length_title = map(title, ~ length(.x)),
         length_price = map(price, ~ length(.x)),
         length_sqm = map(sqm, ~ length(.x)),
         length_rooms = map(rooms, ~ length(.x)),
         length_address = map(address, ~ length(.x)),
         length_landlord = map(landlord, ~ length(.x)),) %>%
  unnest(contains("length_")) %>%
  mutate(length_total = length_title +
           length_price +
           length_sqm +
           length_rooms +
           length_address +
           length_landlord) %>%
  filter(length_total == 30) %>%
  select(-contains("length_")) %>% 
  unnest(everything()) %>% 
  select(-listing_no) %>% 
  mutate(price = price %>% str_replace("\\.", "") %>% 
           str_replace(",", ".") %>% parse_number() %>% 
           round(digits = 2),
         across(c(sqm, rooms, address), ~ parse_number(.x))) %>%
  rename(postcode = address) %>% 
  write_csv("Willhaben_everything_clean.csv")

# Der Standard ----
pages <- 1:379

start_urls <- paste0("https://immobilien.derstandard.at/immobiliensuche/i/mieten/wohnung/wien/seite-",
                     pages) %>% 
  as_tibble() %>% 
  rename(url = value) %>% 
  mutate(subpage_content = NA)

start_urls

# Trying for loop which binds rows
tmp <- start_urls

subpages_content <- vector(mode = "list", length = nrow(tmp))

for (x in 1:nrow(tmp)){
  
  url_tmp <- tmp[x, "url"] %>% 
    pull()
  
  tryCatch(
    subpages_content[[x]] <- url_tmp %>%
      GET(., timeout(90)) %>% 
      read_html(.),
    error = function(e){NA}
  )
  
  print(paste("Link", x, "retrieved"))
  
}

subpage_content <- tibble(listing = subpages_content,
                          is_null = map(listing, is.null)) %>%
  unnest(is_null) %>%
  filter(is_null == FALSE) %>% 
  select(-is_null)

# Get content
tmp <- subpage_content %>% 
  mutate(
    listing_no = 1:nrow(.),
    title = map(listing, function(.x){
      return(.x %>% 
               html_nodes(".result-data-container a") %>%
               html_text())
    }),
    price = map(listing, function(.x){
      return(.x %>% 
               html_nodes(".result-data:nth-child(3)") %>%
               html_text())
    }),
    sqm = map(listing, function(.x){
      return(.x %>% 
               html_nodes(".result-data:nth-child(1)") %>%
               html_text())
    }),
    rooms = map(listing, function(.x){
      return(.x %>% 
               html_nodes(".result-data:nth-child(2)") %>%
               html_text())
    }),
    address = map(listing, function(.x){
      return(.x %>% 
               html_nodes(".adress") %>%
               html_text())
    })
  ) %>% 
  select(-c(listing)) %>% 
  unnest(everything())

tmp %>% 
  print(width = Inf)

tmp %>% 
  select(-listing_no) %>% 
  mutate(across(c(title, price, sqm, rooms),
                ~ str_replace(.x, "\r\n", "")),
         across(c(title, price, sqm, rooms), ~ trimws(.x)),
         price = price %>% str_replace("\\.", "") %>% str_replace("\\,", "."),
         across(c(price, sqm, rooms, address), ~ parse_number(.x)),
         across(c(title), ~ tolower(.x))) %>% 
  rename(postcode = address) %>% 
  write_csv("DerStandard_clean.csv")




