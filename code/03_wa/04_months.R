
k <- readRDS("temp/vf.rds") |> 
  filter(n_dead < 3 | is.na(n_dead)) |> 
  mutate(week = month(death_date),
         reg_date = as.numeric(as.Date(reg_date, "%m/%d/%Y") - as.Date("2010-01-01")),
         death_date = as.numeric(death_date - as.Date("2010-01-01"))) |> 
  rename(deced_age = age)

coefs <- rbindlist(lapply(unique(filter(k, covid == 1)$week), function(we){
  print(we)
  if(we != 2){
    first_set <- filter(k,
                        treated > 0,
                        week == we) |> 
      mutate(t = treated == 2)
    
    first_set <- first_set[complete.cases(select(first_set,
                                                 lat, lon, voter_age,
                                                 reg_date,
                                                 dem, rep,
                                                 deced_age,
                                                 death_date,
                                                 n_dead,
                                                 starts_with("pred."),
                                                 male, 
                                                 median_income, some_college, pop_dens)), ]
    
    X <- select(first_set,
                lat, lon, voter_age,
                reg_date,
                dem, rep,
                deced_age,
                death_date,
                n_dead,
                starts_with("pred."),
                starts_with("Genera"),
                -General_2020_11_03,
                male, 
                median_income, some_college, pop_dens,
                -pred.oth)
    
    mb <- ebalance(first_set$t, X)
    
    first_set <- bind_rows(
      first_set |> 
        filter(t) |> 
        mutate(weight = 1),
      first_set |> 
        filter(!t) |> 
        mutate(weight = mb$w)
    )
    
    
    ###################################################
    
    second_set <- filter(k,
                         treated != 1,
                         is.na(week) | week == we) |> 
      mutate(t = treated == 2)
    
    second_set <- second_set[complete.cases(select(second_set,
                                                   lat, lon, voter_age,
                                                   reg_date,
                                                   dem, rep,
                                                   starts_with("pred."),
                                                   male, 
                                                   median_income, some_college, pop_dens)), ]
    
    X <- select(second_set,
                lat, lon, voter_age,
                reg_date,
                dem, rep,
                starts_with("pred."),
                starts_with("Genera"),
                -General_2020_11_03,
                male, 
                median_income, some_college, pop_dens,
                -pred.oth)
    
    mb <- ebalance(second_set$t, X)
    
    second_set <- bind_rows(
      second_set |> 
        filter(t) |> 
        mutate(weight = 1),
      second_set |> 
        filter(!t) |> 
        mutate(weight = mb$w)
    )
    
    
    ##########################
    
    full <- bind_rows(
      first_set,
      filter(second_set, !t)
    ) |> 
      pivot_longer(starts_with("Gener")) |> 
      mutate(name = as.integer(substring(name, 9, 12)),
             t1 = treated > 0,
             t2 = treated > 1) |> 
      rename(year = name,
             turnout = value)
    
    ################################
    
    m1 <- fixest::feols(turnout ~ t1 * I(year == 2020) | year + LALVOTERID, 
                        full, cluster = c("LALVOTERID", "year"),
                        weights = full$weight)
    
    dat <- confint(m1, cluster = c("LALVOTERID", "year")) |> 
      mutate(m = we)
    return(dat)
  }
}))

m <- c()

for(i in unique(filter(k, covid == 1)$week)){
  if(i != 2){
    m <- c(m, rep(i, 2))
  }
}

coefs$m <- m

coefs$coef <- rep(c("non", "cov"), 11)

coefs$e <- (coefs$`2.5 %` + coefs$`97.5 %`)/2

ggplot(filter(coefs, coef == "cov"), aes(x = m, y = e,
                                         ymin = `2.5 %`, ymax = `97.5 %`)) +
  geom_errorbar() +
  geom_point() +
  theme_bc() +
  scale_x_continuous(breaks = seq(3, 12, 1)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Month of Death (2020)",
       y = "Estimted Treatment Effect") +
  ggtitle("Additional Effect of Covid Household Death on 2020 Turnout\n(Relative to Non-Covid Death)",
          "Washington State")
