cces <- fread("../regular_data/cces/CCES 2020/CES20_Common_OUTPUT_vv.csv") %>% 
  mutate(diagnosed = CC20_309a_5 == 2,
         diagnosed_close = CC20_309a_1 == 1 | CC20_309a_2 == 1,
         died_close = CC20_309b_1 == 1,
         died = CC20_309b_4 == 2,
         died = ifelse(is.na(died), 0, died),
         died_close = ifelse(is.na(died_close), F, died_close),
         voted = is.finite(CL_2020gvm)) %>% 
  select(caseid,
         ideo = CC20_340a,
         pid7,
         approval = CC20_320a,
         birthyr,
         gender,
         educ,
         race,
         diagnosed,
         faminc_new,
         died,
         died_close,
         diagnosed_close,
         voted,
         commonpostweight,
         vvweight_post,
         party = CC20_433a,
         state = inputstate,
         pres_choice = CC20_410,
         presvote16post,
         starts_with("CC20_441")) %>% 
  mutate_at(vars(gender, educ, race, state, party), as.factor) %>% 
  # filter(approval != 5) %>% 
  mutate(white = race == 1,
         strong = ifelse(approval %in% c(1, 4), 1, 0),
         diagnosed = as.integer(diagnosed),
         party_n = party,
         party = ifelse(party == 1, "Democrat", ifelse(party == 2, "Republican", "Other")),
         race = ifelse(race == 1, "White",
                       ifelse(race == 2, "Black",
                              ifelse(race == 3, "Latinx",
                                     ifelse(race == 4, "Asian", "Other")))),
         age = 2020 - birthyr,
         pid7 = ifelse(pid7 == 1, "Strong Democrat",
                       ifelse(pid7 == 2, "Democrat",
                              ifelse(pid7 == 7, "Strong Republican",
                                     ifelse(pid7 == 6, "Republican",
                                            ifelse(pid7 == 3, "Lean Democrat",
                                                   ifelse(pid7 == 5, "Lean Republican",
                                                          ifelse(pid7 == 4, "Neither", "Other"))))))),
         voted_16 = presvote16post != 7,
         white = race == "White",
         rr = CC20_441a +
           (6-CC20_441b) +
           (6-CC20_441e) +
           (6-CC20_441f) +
           (6-CC20_441g))

################
covid_blame <- fread("raw_data/dataverse_files/CES20_UTK_OUTPUT_vv.tab", sep = "\t") %>%
  select(caseid, UTK382, UTK383, UTK384, teamweight) %>%
  filter(!is.na(UTK382) & UTK382 != 99  & UTK382 != 98 |
         !is.na(UTK383) & UTK383 != 99  & UTK383 != 98  |
         !is.na(UTK384) & UTK384 != 99  & UTK384 != 98 ) %>%
  mutate(gov = (UTK382 %in% c(4:6)) |
               (UTK383 %in% c(4:6)) |
               (UTK384 %in% c(4:6)),
         gov_pot = T) %>%
  select(caseid, teamweight, gov_pot, gov_blame = gov)
################

cces <- left_join(cces, covid_blame)

cces$id <- c(1:nrow(cces))

cces$pid7 <- factor(cces$pid7,
                    levels = c("Strong Democrat", "Democrat", "Lean Democrat", "Neither",
                               "Lean Republican", "Republican", "Strong Republican", "Other"))
income_lu <- data.frame("faminc_new" = c(1:16),
                        "income" = c(5000, 15000, 25000, 35000, 45000, 55000, 65000, 75000,
                                     90000, 110000, 135000, 175000, 225000, 300000, 425000, 500000))

cces <- left_join(cces, income_lu)


rr <- select(cces, id, CC20_441e, CC20_441f, CC20_441g) %>% 
  mutate_at(vars(CC20_441e, CC20_441f, CC20_441g), ~ 6 - .)

rr <- rr[complete.cases(rr), ]

alpha <- psych::alpha(rr %>% 
                 select(-id))

ev <- eigen(cor(rr %>% 
                  select(-id)))
ev$values

factors_rr <- factanal(rr %>% 
                         select(-id), 
                          factors = 1, 
                          scores = "regression")

rr <- cbind(rr, factors_rr$scores) %>% 
  rename(rr_factor = Factor1) %>% 
  select(id, rr_factor)

cces <- left_join(cces, rr) %>% 
  mutate(white = as.integer(white),
         nonwhite = 1 - white)

m1 <- lm(voted ~ diagnosed_close * white +
           birthyr + gender + educ + ideo + income + party +
           voted_16 + race + state, filter(cces), weights = commonpostweight)

marg1 <- ggeffect(m1, terms = c("diagnosed_close", "white")) %>% 
  mutate(plot = "Self or Family Member Diagnosed")


m2 <- lm(voted ~ died_close * white +
           birthyr + gender + educ + ideo + income + party +
           voted_16 + race + state, cces, weights = commonpostweight)

marg2 <- ggeffect(m2, terms = c("died_close", "white")) %>% 
  mutate(plot = "Family Member Died")


#################

mp <- bind_rows(marg1, marg2) %>% 
  mutate(x = as.integer(x),
         group = ifelse(group == 1, "White", "Nonwhite"))

mp$group <- factor(mp$group, levels = c("Nonwhite", "White"))
mp$plot <- factor(mp$plot, levels = c("Self or Family Member Diagnosed", "Family Member Died"))

p2 <- ggplot(mp, aes(x = x, y = predicted, linetype = group, shape = group)) + 
  facet_wrap(~ plot) +
  geom_line() + geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = .05, linetype = "solid") +
  theme_bc() +
  scale_x_continuous(breaks = c(1, 2),
                     labels = c("No", "Yes")) +
  labs(shape = "Racial Group",
       linetype = "Racial Group",
       x = NULL,
       y = "Predicted Turnout",
       caption = "Note: Covariates include race / ethnicity; age; gender; party; ideology;
income; education; state fixed effects; and reported 2016 turnout.") +
  scale_y_continuous(labels = percent)
p2
saveRDS(p2, "temp/white_nonwhite_contact.rds")


#########################

nw <- filter(cces, race != "White")

m3 <- lm(voted ~ diagnosed_close * rr_factor +
           birthyr + gender + educ + ideo + income + party +
           voted_16 + state + race, filter(nw), weights = commonpostweight)
summary(m3)
marg1 <- ggeffect(m3, terms = c("diagnosed_close", "rr_factor[-0.592, 0.792]")) %>% 
  mutate(plot = "Self or Family Member Diagnosed")


m4 <- lm(voted ~ died_close * rr_factor +
           birthyr + gender + educ + ideo + income + party +
           voted_16 + state + race, nw, weights = commonpostweight)
summary(m4)
marg2 <- ggeffect(m4, terms = c("died_close", "rr_factor[-0.592, 0.792]")) %>% 
  mutate(plot = "Family Member Died")


#################

mp <- bind_rows(marg1, marg2) %>% 
  mutate(x = as.integer(x),
         group = ifelse(as.numeric(as.character(group)) < 0, "25th Percentile", "75th Percentile"))

mp$group <- factor(mp$group, levels = c("75th Percentile", "25th Percentile"))
mp$plot <- factor(mp$plot, levels = c("Self or Family Member Diagnosed", "Family Member Died"))

p2 <- ggplot(mp, aes(x = x, y = predicted, linetype = group, shape = group)) + 
  facet_wrap(~ plot) +
  geom_line() + geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = .05, linetype = "solid") +
  theme_bc() +
  scale_x_continuous(breaks = c(1, 2),
                     labels = c("No", "Yes")) +
  labs(shape = "Racial Resentment",
       linetype = "Racial Resentment",
       x = NULL,
       y = "Predicted Turnout",
       caption = "Note: Covariates include race / ethnicity; age; gender; party; ideology;
income; education; state fixed effects; and reported 2016 turnout.") +
  scale_y_continuous(labels = percent)
p2
saveRDS(p2, "temp/affinity.rds")

########################
gov <- filter(cces, gov_pot,
              !is.na(teamweight),
              !is.na(rr_factor)) %>% 
  mutate(gov_blame = gov_blame * 1,
         party = as.factor(party))

m4a <- feols(gov_blame ~ poly(rr_factor, 1), weight = gov$teamweight, data = gov)

m4b <- feols(gov_blame ~ poly(rr_factor, 1) +
               state, weight = gov$teamweight, data = gov)

m4 <- feols(gov_blame ~ poly(rr_factor, 1) +
           birthyr + gender + educ + ideo + income + party +
           race + state, weight = gov$teamweight, data = gov)

marg4 <- ggeffect(m4, terms = c("rr_factor[all]"))

ggplot(marg4, aes(x = x, y = predicted)) + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.3) +
  geom_line() + 
  theme_bc() +
  scale_x_continuous(breaks = c(min(marg4$x), (min(marg4$x) + max(marg4$x)) / 2, max(marg4$x)),
                     labels = c("Low", "Medium", "High")) +
  labs(x = "Racial Resentment",
       y = "Predicted Likelihood of Blaming Government",
       caption = "Note: Covariates include race / ethnicity; age; gender; party; ideology;
income; education; state fixed effects; and reported 2016 turnout.") +
  scale_y_continuous(labels = percent)

rows <- tribble(~term,          ~m4a,  ~m4b, ~m4,
                "State Fixed Effects", "", "$\\checkmark$", "$\\checkmark$",
                "Other Controls", "", "", "$\\checkmark$")

attr(rows, 'position') <- c(3:7)

modelsummary(list(m4a, m4b, m4),
             statistic = "std.error",
             stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
             coef_map = c("poly(rr_factor, 1)" = "Racial Resentment"),
             notes = list("table can be found in the Supplementary Information.",
                          "income, party, race, and state. Full regression",
                          "Covariates include age, gender, education, ideology,"),
             gof_omit = 'DF|Deviance|AIC|BIC|Within|Pseudo|Log|Std|FE',
             title = "\\label{tab:gov-reg} Blame of the Government for Covid",
             add_rows = rows,
             latex_options = "scale_down",
             output = "temp/gov_reg.tex",
             escape = FALSE)
