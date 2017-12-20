library(rstan)

model_string <- "
data {
int n;
int n_groups;
int x[n];
vector[n] y;
}

parameters {
vector[n_groups] mu;
vector<lower=0>[n_groups] sigma;
}

model {  
mu ~ uniform(0, 10);
sigma ~ uniform(0, 5);
y ~ normal(mu[x], sigma[x]);
}

generated quantities {
}
"

movies <- jsonlite::read_json('http://bechdeltest.com/api/v1/getAllMovies',simplifyVector = TRUE) %>%
  data.frame() %>%
  mutate(year = as.numeric(year),
         id = as.numeric(id))

ratings <- ggplot2movies::movies

movies.with.rating <- left_join(movies, ratings, by='title') %>%
  filter(!is.na(votes))

d <- movies.with.rating %>% filter(year.x >= 2015)
d$rating.x <- as.numeric(d$rating.x)+1

data_list <- list(y = d$rating.y, x = d$rating.x, n = length(d$rating.y), n_groups = max(d$rating.x))

stan_samples <- stan(model_code = model_string, data = data_list)
stan_samples
plot(stan_samples)

s <- as.data.frame(stan_samples)

st <- s %>% select(`mu[1]`,`mu[2]`,`mu[3]`,`mu[4]`) %>% gather()

ggplot(aes(x=key, y=value, fill=key), data=st) + geom_boxplot()
qplot(s$`mu[3]` - s$`mu[1]`, bins=50) + xlim(-2,2)
qplot(s$`mu[4]` - s$`mu[1]`, bins=50) + xlim(-2,2)
mean(s$`mu[3]` - s$`mu[1]` > 0)
mean(s$`mu[4]` - s$`mu[1]` > 0)

median(s$`mu[3]`)-median(s$`mu[1]`)

mean(s$`mu[3]` - s$`mu[1]` > .51)

model <- lm(rating.y ~ as.factor(rating.x), data = d)
summary(model)

aov <- aov(rating.y ~ as.factor(rating.x), data = d)
summary(aov)
TukeyHSD(aov)