library("dplyr")
library("ggplot2")
library("tidyr")
library("MatchIt")
library("lmtest")
library("sandwich")

options(scipen = 100, digits=4)

# LOAD DATA ---------------------------------------------
data("lalonde")
head(lalonde)

# COMPARE TREATMENT GROUPS ---------------------------------------------
df <- lalonde %>% 
  mutate(group=factor(ifelse(treat==1, "Treatment", "Control")))

df %>%  
  select(group, age, educ) %>% 
  gather("key", "value", -group) %>% 
  ggplot(aes(x=value, color=group, fill=group)) +
  geom_density(alpha=0.5) +
  ggtitle("Density plot by treatment groups") + 
  theme_gray() +
  theme(plot.title=element_text(hjust=0.5)) +
  facet_wrap(~key)

df %>% 
  select(group, race, married, nodegree) %>% 
  gather("key", "value", -group) %>% 
  ggplot(aes(x=value, fill=group)) +
  geom_bar(position="fill") +
  theme_gray() +
  facet_wrap(~key, scales="free")

df %>%  
  select(group, re74, re75, re78) %>% 
  gather("key", "value", -group) %>% 
  ggplot(aes(x=value, color=group, fill=group)) +
  geom_density(alpha=0.5) +
  ggtitle("Density plot by treatment groups") + 
  theme_gray() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(trans='log10') +
  facet_wrap(~ key)

# PRE-MATCHING ---------------------------------------------
summary(lm(re78~treat, data=df))
df %>% group_by(treat) %>% summarise(mean(re78))

prematch <- matchit(treat ~ age + educ + race + married + 
                      nodegree + re74 + re75, data = df,
                    method = NULL, distance='glm', estimand = "ATE")
df$propensity = prematch$distance
summary(prematch)
plot(summary(prematch))

df %>% 
  ggplot(aes(x=propensity, y=group, color=group)) +
  geom_jitter(alpha=0.5, position="jitter")

df %>% 
  select(group, propensity) %>% 
  gather("key", "value", -group) %>% 
  ggplot(aes(x=value, color=group, fill=group)) +
  geom_density(alpha=0.5)

plot(prematch, type = "jitter", interactive = FALSE)

# MATCHING -----
matching <- matchit(treat ~ age + educ + race + married + nodegree + re74 + re75, 
                    data = df, method='full', estimand='ATE')
summary(matching, un=FALSE)
plot(summary(matching))

plot(matching, type = "jitter", interactive = FALSE)

matched_df <- match.data(matching)
head(matched_df)

matched_df %>% 
  select(treat, weights) %>% 
  group_by(treat) %>% 
  summarise_all(c("mean", "median", "min", "max"))

model <- lm(re78 ~ treat + age + educ + race + married + nodegree + 
              re74 + re75, data = matched_df, weights = weights)
summary(model)
coeftest(model, vcov. = vcovCL, cluster = ~subclass)
