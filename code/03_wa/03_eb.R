
k <- readRDS("temp/vf.rds")
k <- k |> 
  filter(n_dead < 3 | is.na(n_dead),
         death_date <= "2020-12-31" | is.na(death_date)) |> 
  rename(deced_age = age) |> 
  mutate(reg_date = as.numeric(as.Date(reg_date, "%m/%d/%Y") - as.Date("2010-01-01")),
         death_date = as.numeric(death_date - as.Date("2010-01-01")),
         dem = as.numeric(party == "Democratic"),
         rep = as.numeric(party == "Republican"))

first_set <- filter(k,
                    treated > 0) |> 
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

ll <- first_set |> 
  group_by(t) |> 
  summarize(across(starts_with("Gener"), ~ weighted.mean(., weight))) |> 
  pivot_longer(starts_with("Gen")) |> 
  mutate(name = as.integer(substring(name, 9, 12)))

ll2 <- first_set |> 
  group_by(t) |> 
  summarize(across(starts_with("Gener"), mean)) |> 
  pivot_longer(starts_with("Gen")) |> 
  mutate(name = as.integer(substring(name, 9, 12)))

ggplot(ll, aes(x = name, y = value, color = t)) + geom_line()

check <- first_set |> 
  group_by(t) |> 
  summarize(across(c(lat, lon, voter_age,
                     reg_date,
                     deced_age,
                     dem, rep,
                     death_date,
                     n_dead,
                     starts_with("pred."),
                     male, 
                     median_income, some_college, pop_dens), ~ weighted.mean(., weight)))
###################################################

second_set <- filter(k,
                     treated != 1) |> 
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

ll <- second_set |> 
  group_by(t) |> 
  summarize(across(starts_with("Gener"), ~ weighted.mean(., weight))) |> 
  pivot_longer(starts_with("Gen")) |> 
  mutate(name = as.integer(substring(name, 9, 12)))

ggplot(ll, aes(x = name, y = value, color = t)) + geom_line()

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

ll <- full |> 
  group_by(treated, year) |> 
  summarize(`Weighted` = weighted.mean(turnout, weight),
            `Unweighted` = mean(turnout)) |> 
  pivot_longer(cols = c(Weighted, Unweighted), names_to = "pan", values_to = "turnout")


ll <- ll |> 
  mutate(group = case_when(treated == 0 ~ "No Household Deaths",
                           treated == 1 ~ "Household Non-Covid Death",
                           treated == 2 ~ "Household Covid Death"))

saveRDS(ll, "temp/weighted_full_ll.rds")

ggplot(ll, aes(x = year, y = turnout, color = group, shape = group)) + geom_line() +
  geom_point() +
  facet_grid(. ~ pan) +
  theme_bc(legend.position = "bottom") +
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5),
         shape = guide_legend(title.position = "top", title.hjust = 0.5)) +
  labs(x = "Year", y = "Turnout\n(share of voters registered in 2020)",
       color = "Treatment Group",
       shape = "Treatment Group",
       caption = "'No Household Deaths' weighted to mirror 'Household Covid Death' using entropy balancing. Balancing covariates include latitude, longitude, voter's age, registration date, BISG racial predictions, gender, party, block group median income, block group education, block group population density, and pre-treatment year turnout. 'Household Non-Covid Death' are weighted using the preceding covariates, along with decedent's age, decedent's date of death, and the number of household deaths.") +
  scale_y_continuous(labels = scales::percent)

ggsave("temp/first.png", width = 6, height = 4.5, units = "in")

#############################

m1 <- fixest::feols(turnout ~ t1 * t2 * I(year == 2020) | year + LALVOTERID, 
                    full, cluster = c("LALVOTERID", "year"),
                    weights = full$weight)
summary(m1)


m2 <- fixest::feols(turnout ~ t1 * t2 * I(year == 2020) * I(pred.whi > 0.5) | year + LALVOTERID, 
                    full, cluster = c("LALVOTERID", "year"),
                    weights = full$weight)

rows <- tribble(~term,          ~m1,  ~m2,
                "Year Fixed Effects", "$\\checkmark$", "$\\checkmark$", 
                "Voter Fixed Effects", "$\\checkmark$", "$\\checkmark$")

attr(rows, 'position') <- c(11:12)


modelsummary::modelsummary(list(m1, m2),
                           statistic = "std.error",
                           stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
                           coef_map = c("t2TRUE:I(year == 2020)TRUE" = "Household Covid Death × 2020",
                                        "t1TRUE:I(year == 2020)TRUE" = "Household Death × 2020",
                                        "I(year == 2020)TRUE:I(pred.whi > 0.5)TRUE" = "2020 × P(White > 0.5)",
                                        "t2TRUE:I(year == 2020)TRUE:I(pred.whi > 0.5)TRUE" = "Household Covid Death × 2020 × P(White > 0.5)",
                                        "t1TRUE:I(year == 2020)TRUE:I(pred.whi > 0.5)TRUE" = "Household Death × 2020 × P(White > 0.5)"),
                           gof_omit = 'DF|Deviance|AIC|BIC|Within|Pseudo|Log|Std|FE|RMSE',
                           add_rows = rows,
                           title = "\\label{tab:wa-reg-1} Household Deaths and Voter Turnout",
                           notes = list("Standard errors clustered by voter and year."),
                           output = "temp/wa_reg.tex",
                           escape = FALSE)


######################################
######################################
######################################
######################################

full <- as.data.table(filter(full, year == 2012))

unw <- full[, by = list(treated),
            c(lapply(.SD, mean)),
            .SDcols = c("reg_date",
                        "deced_age",
                        "death_date",
                        "n_dead",
                        "pred.whi", "pred.bla", "pred.his", "pred.asi",
                        "male", "voter_age", "dem", "rep",
                        "median_income", "some_college", "pop_dens")] |> 
  mutate(across(c(reg_date, death_date), ~ format((as.Date("2010-01-01") + .), "%B %d, %Y")),
         across(c(deced_age, n_dead, pop_dens, voter_age), ~ comma(., .1)),
         across(c(starts_with("pred"), male, some_college, dem, rep), ~ percent(., .1)),
         across(c(median_income), ~ dollar(., 1)))

unw <- pivot_longer(unw, cols = colnames(select(unw, -treated))) |> 
  mutate(treated = case_when(treated == 0 ~ "No Household Death",
                             treated == 1 ~ "Household Non-Covid Death",
                             T ~ "Household Covid Death")) |> 
  pivot_wider(names_from = treated, values_from = value)

w <- full[, by = list(treated),
          c(lapply(.SD, weighted.mean, weight)),
          .SDcols = c("reg_date",
                      "deced_age",
                      "death_date",
                      "n_dead",
                      "pred.whi", "pred.bla", "pred.his", "pred.asi",
                      "male", "voter_age", "dem", "rep",
                      "median_income", "some_college", "pop_dens")] |> 
  mutate(across(c(reg_date, death_date), ~ format((as.Date("2010-01-01") + .), "%B %d, %Y")),
         across(c(deced_age, n_dead, pop_dens, voter_age), ~ comma(., .1)),
         across(c(starts_with("pred"), male, some_college, dem, rep), ~ percent(., .1)),
         across(c(median_income), ~ dollar(., 1)))

w <- pivot_longer(w, cols = colnames(select(w, -treated))) |> 
  mutate(treated = case_when(treated == 0 ~ "No Household Death",
                             treated == 1 ~ "Household Non-Covid Death",
                             T ~ "Household Covid Death")) |> 
  pivot_wider(names_from = treated, values_from = value)

tab <- left_join(unw, w, by = "name")

vo <- fread("raw_data/var_orders.csv")

tab <- left_join(tab, vo) |> 
  arrange(order) |> 
  select(clean, everything(), -name, -order)

tab <- mutate(tab, across(everything(), ~ gsub("%", "\\\\%", .)))

colnames(tab) <- c(" ", rep(c("Household Covid Death", "Household Non-Covid Death", "No Household Death"), 2))

options(knitr.kable.NA = '')

for(i in c(2:nrow(tab))){
  if(i %% 2 == 0){
    tab$` `[i] <- paste0("\\rowcolor{Gray}", tab$` `[i])
  }
}

kable(tab, "latex", caption = "\\label{tab:full-bal} Balance Table for Entropy Balancing",
      linesep = "", align = c("l", rep("c", 6)),
      booktabs = T, escape = F) |> 
  add_header_above(c(" " = 1, "Means: Unweighted Data" = 3, "Means: Unweighted Data" = 3), align = "c") |> 
  column_spec(c(1), width = "4cm") |>
  column_spec(c(2:7), width = "3cm") |>
  kable_styling(latex_options = c("scale_down")) |> 
  save_kable("temp/balance.tex")

