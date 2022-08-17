library("dplyr")
library("MatchIt")
library("lmtest")
library("sandwich")
options(scipen = 100, digits=4)

## PREPARE SAMPLE DATA --------------------
# Load lalonde data set
data("lalonde")
head(lalonde)
library("ggplot2")

# Extract sample
set.seed(0)
(df <- lalonde %>% 
    select(treat, educ, race, re78) %>% 
    sample_n(40) %>% 
    mutate(group=factor(ifelse(treat==1, "Treatment", "Control"))))

## VISUALISE DATA --------------------
df %>%
  ggplot(aes(x=educ, color=group, fill=group)) +
  geom_density(alpha=0.5) +
  ggtitle("Density plot by treatment groups") +
  theme_gray() +
  theme(plot.title=element_text(hjust=0.5))

df %>%
  ggplot(aes(x=race, fill=group)) +
  geom_bar(position="fill") +
  theme_gray()

## PERFORM MATCHING WITH DEFAULT PARAMETERS --------------------
# Do coarsened exact matching
matching1 <- matchit(treat ~ educ + race, data = df, 
                     method = 'cem', estimand = 'ATE')
summary(matching1, un=FALSE)

# Extract matched data
(matched_df1 <- match.data(matching1) %>% arrange(subclass, treat))
matched_df1 %>% group_by(treat) %>% summarise(weighted.mean(re78, weights))

# Look at ATE
model1 <- lm(re78 ~ treat, data = matched_df1, weights = weights)
coeftest(model1, vcov. = vcovCL, cluster = ~subclass)

## PERFORM MATCHING WITH CUSTOM PARAMETERS --------------------
cutpoints <- list(educ = c(6.5, 8.5, 9.5, 11.5, 12.5)) # or list(educ = 5)
grouping <- list(race = list(c("white", "black"), c("hispan")))
matching2 <- matchit(treat ~ educ + race, data = df, 
                     method = 'cem', estimand = 'ATE',
                     cutpoints=cutpoints, grouping=grouping)
summary(matching2, un=FALSE)

# Extract matched data
(matched_df2 <- match.data(matching2) %>% arrange(subclass, treat))
matched_df2 %>% group_by(treat) %>% summarise(weighted.mean(re78, weights))

# Look at ATE
model2 <- lm(re78 ~ treat, data = matched_df2, weights = weights)
coeftest(model2, vcov. = vcovCL, cluster = ~subclass)

## MANUALLY COARSEN DATA  --------------------
df$educ_binned = cut(df$educ, breaks=c(0, 6.5, 9.5, 11.5, 12.5, 15))
df$race_grouped = ifelse(df$race=="hispan", "hispan", "not hispan")
df$stratum = paste(df$educ_binned, df$race_grouped)
(matched <- df %>% group_by(stratum) %>%
  summarise(n_group = n_distinct(treat)) %>%
  filter(n_group==2))

# Get matched data
(matched_df <- df %>% filter(stratum %in% matched$stratum) %>%
  select(treat, educ, race, stratum, re78) %>%
  arrange(stratum, treat))

Ns = matched_df %>% group_by(treat) %>% summarise(N = n())
matched_df <- matched_df %>% inner_join(matched_df %>% group_by(stratum) %>% count())
matched_df <- matched_df %>% inner_join(matched_df %>% filter(treat==1) %>%
                                              group_by(stratum) %>% summarise(nt = n()))
matched_df$nc <- matched_df$n - matched_df$nt

matched_df %>% mutate(w = ifelse(treat==1, n/(nt*(1+Ns$N[1]/Ns$N[2])),
                                 n/(nc*(1+Ns$N[2]/Ns$N[1]))))

# # Check indices
# sort(row.names(matched_df)) == sort(row.names(matched_df2))