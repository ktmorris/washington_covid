###########################################
d2020 <- rbind(fread("raw_data/death_data/DeathNamesF2020.csv.D230105.T132230.csv"),
               fread("raw_data/death_data/DeathNamesF2021.csv.D230105.T132237.csv"))
colnames(d2020) <- clean_names(d2020)

d2020 <- mutate(d2020,
                residence_street = toupper(gsub('[[:punct:] ]+', ' ', residence_street)))

d2020 <- filter(d2020, !grepl("UNKNOWN", residence_street),
                !grepl("P O BOX", residence_street),
                !grepl("PO BOX", residence_street),
                !grepl("POBOX", residence_street),
                !grepl("HOMELESS", residence_street),
                !grepl("TRANSIENT", residence_street),
                !grepl("NO PERMANENT ADDRESS", residence_street),
                trimws(residence_street) != "",
                residence_state == "WASHINGTON")

d2020 <- mutate(d2020, residence_street = trimws(gsub("\\s+", " ", residence_street)))

d2020 <- mutate(d2020, residence_street = gsub(" APT ", " ", residence_street))
d2020 <- mutate(d2020, residence_street = gsub(" APARTMENT ", " ", residence_street))

ad_cl <- fread("raw_data/address_cleaner.csv") |> 
  filter(search != replace) |> 
  mutate(across(c(search, replace), toupper)) |> 
  distinct()

for(i in c(1:nrow(ad_cl))){
  print(i)
  d2020$residence_street <- gsub(paste0("(^|\\s)", ad_cl$search[i], "(\\s|$)"),
                                 paste0("\\1", ad_cl$replace[i], "\\2"),
                                 d2020$residence_street)
}

d2020$residence_street <- gsub("NORTH", "N", d2020$residence_street)
d2020$residence_street <- gsub("SOUTH", "S", d2020$residence_street)
d2020$residence_street <- gsub("EAST", "E", d2020$residence_street)
d2020$residence_street <- gsub("WEST", "W", d2020$residence_street)

d2020 <- d2020 |> 
  mutate(across(c("decedent_last_name", "decedent_first_name", "residence_city"),
                ~ gsub("[[:punct:]]| ", "", ifelse(. == "", NA, tolower(iconv(., "WINDOWS-1252", "UTF-8"))))))

saveRDS(d2020, "temp/cleaned_death.rds")

one_per <- d2020 |> 
  mutate(death_date = as.Date(date_of_death, "%m/%d/%Y"),
         pre = death_date < "2021-05-03") |> 
  group_by(residence_street, residence_city, pre) |> 
  summarize(across(c(death_date, age), mean),
            covid = max(underlying_cod_code == "U071"),
            n_dead = n(),
            last_name = min(decedent_last_name))

one_per <- one_per |> 
  extract(residence_street, into = c("num", "rest"), "(.*?)\\s+(.*)", remove = FALSE)

tre <- filter(one_per, pre)

#####################################

k <- readRDS("temp/cleaned_file.rds")

k <- k |> 
  mutate(across(c("Residence_Addresses_City"),
                ~ gsub("[[:punct:]]| ", "", ifelse(. == "", NA, tolower(iconv(., "WINDOWS-1252", "UTF-8"))))))

k$street <- gsub("NORTH", "N", k$street)
k$street <- gsub("SOUTH", "S", k$street)
k$street <- gsub("EAST", "E", k$street)
k$street <- gsub("WEST", "W", k$street)
k$street <- gsub(" APARTMENT ", " ", k$street)
k$street <- gsub(" APT ", " ", k$street)

k <- filter(k,
            !(paste0(Voters_BirthDate,
                     street, Residence_Addresses_City)) %in% 
              with(d2020, paste0(date_of_birth, residence_street, residence_city)))


k <- left_join(k, tre,
               by = c("street" = "residence_street",
                      "Residence_Addresses_City" = "residence_city"))

k <- mutate(k,
            n_dead = ifelse(is.na(n_dead), 0, n_dead),
            treated = ifelse(is.na(covid), 0,
                             ifelse(covid, 2, 1)))
saveRDS(k, "temp/vf.rds")













k <- k |> 
  extract(street, into = c("num", "rest"), "(.*?)\\s+(.*)", remove = FALSE)


one_per$best <- amatch(one_per$rest,
                       k$rest,
                       maxDist = 2)

one_per$m <- NA

for(i in c(1:nrow(one_per))){
  ind <- one_per$best[i]
  one_per$m[i] <- k$rest[ind]
}

one_per$d <- stringdist(one_per$rest, one_per$m)

saveRDS(one_per, "temp/s.rds")

one_per <- readRDS("temp/s.rds") |> 
  mutate(residence_street = ifelse(is.na(d), residence_street, paste(num, m))) |> 
  select(-num, -rest, -best, -m)

one_per <- one_per |> 
  group_by(residence_street, d) |> 
  summarize(across(c(death_date, age), mean),
            covid = max(covid),
            n_dead = sum(n_dead)) |> 
  group_by(residence_street) |> 
  filter(d == min(d))

##### remove deceased people from the voter file (some may have died after election day
##### but they aren't the population of interest)

k <- filter(k,
            !(paste0(Voters_BirthDate,
                     street)) %in% 
              with(d2020, paste0(date_of_birth, residence_street)))

k <- left_join(k, one_per,
               by = c("street" = "residence_street"))

k <- mutate(k,
            n_dead = ifelse(is.na(n_dead), 0, n_dead),
            treated = ifelse(is.na(covid), 0,
                             ifelse(covid, 2, 1)))
saveRDS(k, "temp/vf.rds")
