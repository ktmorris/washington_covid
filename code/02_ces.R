cces <- fread("../regular_data/cces/CES20_Common_OUTPUT_vv.csv") %>% 
  mutate(diagnosed = CC20_309a_5 == 2,
         diagnosed_close = CC20_309a_1 == 1 | CC20_309a_2 == 1,
         died_close = CC20_309b_1 == 1,
         died = CC20_309b_4 == 2,
         died = ifelse(is.na(died), 0, died),
         died_close = ifelse(is.na(died_close), F, died_close),
         voted = is.finite(CL_2020gvm)) %>% 
  select(ideo = CC20_340a,
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

cces$id <- c(1:nrow(cces))

cces$pid7 <- factor(cces$pid7,
                    levels = c("Strong Democrat", "Democrat", "Lean Democrat", "Neither",
                               "Lean Republican", "Republican", "Strong Republican", "Other"))
income_lu <- data.frame("faminc_new" = c(1:16),
                        "income" = c(5000, 15000, 25000, 35000, 45000, 55000, 65000, 75000,
                                     90000, 110000, 135000, 175000, 225000, 300000, 425000, 500000))

cces <- left_join(cces, income_lu)


rr <- select(cces, id, CC20_441a, CC20_441b, CC20_441e, CC20_441f, CC20_441g) %>% 
  mutate_at(vars(CC20_441b, CC20_441e, CC20_441f, CC20_441g), ~ 6 - .)

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

cces <- left_join(cces, rr)
#########################

m1 <- lm(voted ~ diagnosed_close * I(!white) +
           birthyr + gender + educ + ideo + income + party +
           voted_16 + race, cces, weights = commonpostweight)

marga <- ggeffect(m1, terms = c("diagnosed_close", "white")) %>% 
  mutate(plot = "Self or Family Member Diagnosed")
plot(marga)

m2 <- lm(voted ~ died_close * white +
           birthyr + gender + educ + ideo + income + party +
           voted_16 + race, cces, weights = commonpostweight)

margb <- ggeffect(m2, terms = c("died_close", "white")) %>% 
  mutate(plot = "Family Member Died")
plot(margb)


mpt <- bind_rows(marga, margb) %>% 
  mutate(x = as.integer(x),
         group = ifelse(as.character(group) == "TRUE", "White", "Not White"))

mpt$group <- factor(mpt$group, levels = c("Not White", "White"))
mpt$plot <- factor(mpt$plot, levels = c("Self or Family Member Diagnosed", "Family Member Died"))

ggplot(mpt, aes(x = x, y = predicted, linetype = group, shape = group)) + 
  facet_wrap(~ plot) +
  geom_line() + geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = .05, linetype = "solid") +
  theme_bc(base_family = "LM Roman 10") +
  scale_x_continuous(breaks = c(1, 2),
                     labels = c("No", "Yes")) +
  labs(shape = "Race",
       linetype = "Race",
       x = NULL,
       y = "Predicted Turnout",
       caption = "Note: Covariates include race / ethnicity; gender; age; party; ideology;
income; education; and reported 2016 turnout.") +
  scale_y_continuous(labels = percent)

#################

nw <- filter(cces, race != "White")

m3 <- lm(voted ~ diagnosed_close * rr_factor +
           birthyr + gender + educ + ideo + income + party +
           voted_16 + race, filter(nw), weights = commonpostweight)
summary(m3)
marg1 <- ggeffect(m3, terms = c("diagnosed_close", "rr_factor[-0.592, 0.792]")) %>% 
  mutate(plot = "Self or Family Member Diagnosed")
plot(marg1)

m4 <- lm(voted ~ died_close * rr_factor +
           birthyr + gender + educ + ideo + income + party +
           voted_16 + race, nw, weights = commonpostweight)
summary(m4)
marg2 <- ggeffect(m4, terms = c("died_close", "rr_factor[-0.592, 0.792]")) %>% 
  mutate(plot = "Family Member Died")
plot(marg2)

#################

mp <- bind_rows(marg1, marg2) %>% 
  mutate(x = as.integer(x),
         group = ifelse(as.numeric(as.character(group)) < 0, "25th Percentile", "75th Percentile"))

mp$group <- factor(mp$group, levels = c("75th Percentile", "25th Percentile"))
mp$plot <- factor(mp$plot, levels = c("Self or Family Member Diagnosed", "Family Member Died"))

ggplot(mp, aes(x = x, y = predicted, linetype = group, shape = group)) + 
  facet_wrap(~ plot) +
  geom_line() + geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = .05, linetype = "solid") +
  theme_bc(base_family = "LM Roman 10") +
  scale_x_continuous(breaks = c(1, 2),
                     labels = c("No", "Yes")) +
  labs(shape = "Racial Affinity",
       linetype = "Racial Affinity",
       x = NULL,
       y = "Predicted Turnout",
       caption = "Note: Covariates include race / ethnicity; age; gender; party; ideology;
income; education; and reported 2016 turnout.") +
  scale_y_continuous(labels = percent)
