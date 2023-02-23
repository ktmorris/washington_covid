###########################################
d2020 <- rbind(fread("raw_data/death_data/DeathNamesF2020.csv.D230105.T132230.csv"))
colnames(d2020) <- clean_names(d2020)

d2020 <- mutate(d2020,
                residence_street = paste(toupper(gsub('[[:punct:] ]+', ' ', residence_street)),
                                         residence_city))


d2020 <- mutate(d2020, residence_street = trimws(gsub(" STREET", " ST", residence_street)))

d2020 <- mutate(d2020, residence_street = trimws(gsub(" UNIT ", " APT ", residence_street)))

d2020 <- mutate(d2020, residence_street = trimws(gsub(" PLACE", " PL", residence_street)))

d2020 <- mutate(d2020, residence_street = trimws(gsub(" NORTH", " N", residence_street)))

d2020 <- mutate(d2020, residence_street = trimws(gsub(" DRIVE", " DR", residence_street)))

d2020 <- mutate(d2020, residence_street = trimws(gsub(" ROAD", " RD", residence_street)))
d2020 <- mutate(d2020, residence_street = trimws(gsub(" LANE", " LN", residence_street)))

d2020 <- mutate(d2020, residence_street = trimws(gsub(" LANE", " LN", residence_street)))
d2020 <- mutate(d2020, residence_street = trimws(gsub(" AVENUE", " AVE", residence_street)))

d2020 <- mutate(d2020, residence_street = trimws(gsub("4001 CAPITAL MALL DR SW", "4001 CAPITAL MALL DR", residence_street)))

d2020 <- mutate(d2020, residence_street = trimws(gsub("MAX GOHERY", "MAX GOEHRY", residence_street)))

d2020 <- mutate(d2020, residence_street = trimws(gsub(" HIGHWAY", " HWY", residence_street)))
d2020 <- mutate(d2020, residence_street = trimws(gsub(" STATE ROUTE", " HWY", residence_street)))
d2020 <- mutate(d2020, residence_street = trimws(gsub(" COURT", " CT", residence_street)))
d2020 <- mutate(d2020, residence_street = trimws(gsub(" EAST", " E", residence_street)))
d2020 <- mutate(d2020, residence_street = trimws(gsub(" WEST", " W", residence_street)))
d2020 <- mutate(d2020, residence_street = trimws(gsub(" CIRCLE", " CIR", residence_street)))
d2020 <- mutate(d2020, residence_street = trimws(gsub(" SOUTH", " S", residence_street)))
d2020 <- mutate(d2020, residence_street = trimws(gsub(" TERRACE", " TER", residence_street)))
d2020 <- mutate(d2020, residence_street = trimws(gsub("PENNY ROYAL", "PENNY ROYAL AVE", residence_street)))
d2020 <- mutate(d2020, residence_street = trimws(gsub("262 A ARCADIA TER", "262 ARCADIA TER APT A", residence_street)))
d2020 <- mutate(d2020, residence_street = trimws(gsub("165 B ARCADIA TER", "165 ARCADIA TER APT B", residence_street)))
d2020 <- mutate(d2020, residence_street = trimws(gsub("710 S BARTHOLOMEW", "710 S BARTHOLOMEW ST", residence_street)))
d2020 <- mutate(d2020, residence_street = trimws(gsub("2216 NE 46TH ST B", "2216 NE 46TH ST APT B", residence_street)))
d2020 <- mutate(d2020, residence_street = trimws(gsub("17502 102ND AVE NE B101", "17502 102ND AVE NE APT B101", residence_street)))
d2020 <- mutate(d2020, residence_street = trimws(gsub("HWY 410 E", "HWY 410", residence_street)))
d2020 <- mutate(d2020, residence_street = trimws(gsub("ORTING KAPOWSIN RD", "ORTING KAPOWSIN HWY E", residence_street)))
d2020 <- mutate(d2020, residence_street = trimws(gsub("89 DR NE", "89TH DR NE", residence_street)))
d2020 <- mutate(d2020, residence_street = trimws(gsub("721 COOPER POINT PL SW C 2", "721 COOPER POINT PL SW APT C", residence_street)))
d2020 <- mutate(d2020, residence_street = trimws(gsub("5600 MT SOLO RD 157 LONGVIEW", "721 COOPER POINT PL SW APT C", residence_street)))
d2020 <- mutate(d2020, residence_street = ifelse(residence_street == "2501 NE 138TH AVE E 39 VANCOUVER",
                                                 "2501 NE 138TH AVE APT 39 VANCOUVER", residence_street))

d2020 <- mutate(d2020, residence_street = ifelse(residence_street == "49 LEISURE HILL DR UNION GAP",
                                                 "49 LEISURE HILL DR 49 UNION GAP", residence_street))

d2020 <- mutate(d2020, residence_street = ifelse(residence_street == "3010 10TH ST N E  E WENATCHEE",
                                                 "3010 10TH ST NE E WENATCHEE", residence_street))

d2020 <- mutate(d2020, residence_street = ifelse(residence_street == "930 TROSPER RD SW SP 48 TUMWATER",
                                                 "930 TROSPER RD SW 48 TUMWATER", residence_street))

d2020 <- mutate(d2020, residence_street = ifelse(residence_street == "1500 CATHERINE ST 202A WALLA WALLA",
                                                 "1500 CATHERINE ST A202 WALLA WALLA", residence_street))


d2020 <- mutate(d2020, residence_street = trimws(gsub("721 COOPER POINT PL SW C 2", "721 COOPER POINT PL SW APT C", residence_street)))

d2020 <- mutate(d2020, residence_street = gsub("2647 22ND AVE A", "2647 22ND AVE W APT A", residence_street))
d2020 <- mutate(d2020, residence_street = gsub("1040 TILLOTSON DR WHITE SALMON", "1040 NE TILLOTSON DR WHITE SALMON", residence_street))
d2020 <- mutate(d2020, residence_street = gsub("BLUE BELL DR", "BLUEBELL DR", residence_street))
d2020 <- mutate(d2020, residence_street = ifelse(residence_street == "500 W HENDRICKSON RD STOP 5036 SEQUIM", "500 W HENDRICKSON RD 5036 SEQUIM", residence_street))

d2020 <- mutate(d2020, residence_street = ifelse(substring(residence_street, 1, 1) == "0",
                                                 substring(residence_street, 2),
                                                 residence_street))

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
d2020 <- mutate(d2020, residence_street = gsub(" SUITE ", " ", residence_street),
                residence_street = gsub("10 N DEER LN PL W HOODSPORT",
                                        "10 N DEER LN W HOODSPORT", residence_street),
                residence_street = gsub("10 N SKOKOMISH INDIAN FLATS RD SKOKOMISH NATION",
                                        "10 N SKOKOMISH INDIAN FLATS RD SHELTON",
                                        residence_street),
                residence_street = gsub(" TRAILER ",
                                        " ",
                                        residence_street),
                residence_street = gsub("SEA BREZE",
                                        "SEA BREEZE",
                                        residence_street),
                residence_street = gsub("N NEWPORT KENNEWICK",
                                        "N NEWPORT ST KENNEWICK",
                                        residence_street),
                residence_street = gsub("SW F 204 OLYMPIA",
                                        "SW F204 OLYMPIA",
                                        residence_street),
                residence_street = gsub("1110 24TH ST B ",
                                        "1110 24TH ST B",
                                        residence_street),
                residence_street = gsub("1110 24TH ST A ",
                                        "1110 24TH ST A",
                                        residence_street),
                residence_street = gsub("2311 W 16TH AVE LOT (\\d+) SPOKANE$",
                                        "2311 W 16TH AVE \\1 SPOKANE",
                                        residence_street),
                residence_street = gsub("1121 244TH ST SW SITE (\\d+) BOTHELL",
                                        "1121 244TH ST SW \\1 BOTHELL",
                                        residence_street),
                residence_street = gsub(" ROOM ",
                                        " ",
                                        residence_street),
                residence_street = gsub(" SLIP ",
                                        " ",
                                        residence_street),
                residence_street = gsub("209 21ST AVE SW (.*) (\\d)",
                                        "209 21ST AVE SW \\1\\2",
                                        residence_street),
                residence_street = gsub("ROBINHOOD LOOP",
                                        "ROBIN HOOD LOOP",
                                        residence_street),
                residence_street = gsub(" POINDEXTER W AVE BREMERTON",
                                        " POINDEXTER AVE BREMERTON",
                                        residence_street),
                residence_street = gsub("SKYLINE DRIVE TACOMA",
                                        "N SKYLINE DRIVE TACOMA",
                                        residence_street),
                residence_street = gsub("15405 DES MOINES MEMORIAL DR S (.*) (\\d)",
                                        "15405 DES MOINES MEMORIAL DR S \\1\\2",
                                        residence_street),
                residence_street = gsub("917 LYNWOOD AVE NE RENTON",
                                        "917 LYNNWOOD AVE NE RENTON",
                                        residence_street),
                residence_street = gsub(" PARKWAY ",
                                        " PKWY",
                                        residence_street),
                residence_street = gsub(" BOULEVARD ",
                                        " BLVD",
                                        residence_street),
                residence_street = gsub(" NWEST ",
                                        " NW ",
                                        residence_street),
                residence_street = gsub(" LAFRANCE RD",
                                        " LA FRANCE RD",
                                        residence_street),
                residence_street = gsub(" SO ISLAND DR E BONNEY LAKE",
                                        " S ISLAND DR E BONNEY LAKE",
                                        residence_street),
                residence_street = gsub(" KEY ",
                                        " KY ",
                                        residence_street),
                residence_street = gsub("KELSEY CREEK LLC ",
                                        "",
                                        residence_street),
                residence_street = gsub("HILDERBRAND BLVD AFFINITY",
                                        "HILDERBRAND BLVD",
                                        residence_street),
                residence_street = gsub(" S E ",
                                        " SE ",
                                        residence_street),
                residence_street = gsub("UPPER NASELLE NASELLE",
                                        "UPPER NASELLE RD NASELLE",
                                        residence_street))

d2020 <- d2020 |> 
  mutate(across(c("decedent_last_name", "decedent_first_name"),
                ~ gsub("[[:punct:]]| ", "", ifelse(. == "", NA, tolower(iconv(., "WINDOWS-1252", "UTF-8"))))))

address_cleaner <- fread("raw_data/address_cleaner.csv") %>%
  mutate(search = toupper(paste0(" ", search, " ")),
         replace = toupper(paste0(" ", replace, " ")))

for(i in 1:nrow(address_cleaner)){
  d2020$residence_street <- gsub(address_cleaner$search[i],
                                 address_cleaner$replace[i],
                                 d2020$residence_street)
}

one_per <- d2020 |> 
  mutate(death_date = as.Date(date_of_death, "%m/%d/%Y")) |> 
  group_by(residence_street) |> 
  summarize(across(c(death_date, age), mean),
            covid = max(underlying_cod_code == "U071"),
            n_dead = n(),
            last_name = min(decedent_last_name))

one_per <- one_per |> 
  extract(residence_street, into = c("num", "rest"), "(.*?)\\s+(.*)", remove = FALSE)

#####################################

k <- readRDS("temp/cleaned_file.rds")


for(i in 1:nrow(address_cleaner)){
  k$street <- gsub(address_cleaner$search[i],
                   address_cleaner$replace[i],
                   k$street)
}


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
