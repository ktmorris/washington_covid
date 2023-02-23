
f <- "../voter_file_data/national/post_2020/VM2--WA--2020-12-09/VM2--WA--2020-12-09-DEMOGRAPHIC.tab"

k <- fread(f, sep = "\t",
           select = c("LALVOTERID", 
                      "Voters_BirthDate",
                      "Residence_Addresses_Latitude",
                      "Residence_Addresses_Longitude",
                      "Voters_Gender", "Voters_Age", "Parties_Description",
                      "Voters_OfficialRegDate", "US_Congressional_District",
                      "Voters_FIPS", "Voters_LastName",
                      "Voters_FirstName",
                      "Residence_Addresses_City",
                      "Residence_Addresses_Zip",
                      "Residence_Addresses_HouseNumber",
                      "Residence_Addresses_PrefixDirection",
                      "Residence_Addresses_StreetName",
                      "Residence_Addresses_Designator",
                      "Residence_Addresses_SuffixDirection",
                      "Residence_Addresses_ApartmentNum"))

k <- clean_streets(k, c("Residence_Addresses_HouseNumber",
                        "Residence_Addresses_PrefixDirection",
                        "Residence_Addresses_StreetName",
                        "Residence_Addresses_Designator",
                        "Residence_Addresses_SuffixDirection"))

k <- mutate(k, street = ifelse(is.na(Residence_Addresses_ApartmentNum) | 
                                 trimws(Residence_Addresses_ApartmentNum) == "",
                               street,
                               paste(street, Residence_Addresses_ApartmentNum)))

k <- mutate(k,
            street = trimws(toupper(gsub('[[:punct:] ]+', ' ', street))))

ad_cl <- fread("raw_data/address_cleaner.csv") |> 
  filter(search != replace) |> 
  mutate(across(c(search, replace), toupper)) |> 
  distinct()

for(i in c(1:nrow(ad_cl))){
  print(i)
  k$street <- gsub(paste0("(^|\\s)", ad_cl$search[i], "(\\s|$)"),
                   paste0("\\1", ad_cl$replace[i], "\\2"),
                   k$street)
}

k <- k |> 
  mutate(across(c("Voters_LastName", "Voters_FirstName"),
                ~ gsub("[[:punct:]]| ", "", ifelse(. == "", NA, tolower(iconv(., "WINDOWS-1252", "UTF-8"))))))

#####################
k <- filter(k, !is.na(Residence_Addresses_Longitude))

blocks <- tigris::blocks("WA", year = 2020, class = "sp")

pings  <- SpatialPoints(k[,c("Residence_Addresses_Longitude",
                             "Residence_Addresses_Latitude")],
                        proj4string = blocks@proj4string)

k$GEOID <- over(pings, blocks)$GEOID

k$block <- str_sub(k$GEOID, start= -4)
k$tract <- str_sub(k$GEOID, 6, 11)
k$county <- str_sub(k$GEOID, 3, 5)

k <- rename(k, surname = Voters_LastName)

cens <- readRDS("../regular_data/wru_2023_update/census_block_01_06_2023_WA.rds")

k <- filter(k,
            paste0("53", county, tract, block) %in% blocks$GEOID20)

k <- predict_race(voter.file = mutate(k, state = "WA"),
                  census.geo = "tract", year = "2020",
                  surname.year = "2020",
                  census.data = cens,
                  model = "BISG")

k <- select(k,
            LALVOTERID,
            lat = Residence_Addresses_Latitude,
            lon = Residence_Addresses_Longitude,
            gender = Voters_Gender,
            voter_age = Voters_Age,
            street,
            party = Parties_Description,
            Residence_Addresses_City,
            reg_date = Voters_OfficialRegDate,
            cong = US_Congressional_District,
            Voters_BirthDate,
            starts_with("pred."),
            GEOID,
            surname) |> 
  mutate(GEOID = substring(GEOID, 1, 12),
         male = as.numeric(gender == "M"),
         dem =  as.numeric(party == "Democratic"),
         rep =  as.numeric(party == "Republican"))

hist <- fread("../voter_file_data/national/post_2020/VM2--WA--2020-12-09/VM2--WA--2020-12-09-VOTEHISTORY.tab",
              select = c("LALVOTERID",
                         "General_2012_11_06",
                         "General_2014_11_04",
                         "General_2016_11_08",
                         "General_2018_11_06",
                         "General_2020_11_03")) |> 
  mutate(across(starts_with("Gene"), ~ ifelse(. == "Y", 1, 0)))

k <- left_join(k, hist)

c <- get_basic_census_stats("block group", 2020, "WA") |> 
  select(GEOID,
         median_income, some_college, pop_dens)

k <- left_join(k, c)

saveRDS(k, "temp/cleaned_file.rds")
