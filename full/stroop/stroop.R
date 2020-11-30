rm(list=ls(all=TRUE))  ## efface les données
  source('~/thib/projects/tools/R_lib.r')
  setwd('~/thib/projects/developpement/stroop/')

data <- read_csv('dataset2_stroop.csv')

data <- data %>%
  pivot_longer(cols = c(v:ster,meanRT_comp,meanRT_incomp) , names_to = "type", values_to = "param") %>%
  mutate(n = 1:n()) %>%
  rename(age = Age_real)

save(data, file = 'data_dev.dta')
d.param <- data %>%
  pivot_wider(names_from = type, values_from = param, id_cols = n)

param_age <- ggplot(data = data, aes(x = age, y = param)) +
  geom_point() +
  facet_wrap( ~ type , scales = 'free')
print(param_age)

glimpse(data) 
param_raw <- ggplot(data = data, aes(x = param)) +
  geom_histogram()+
  facet_wrap( ~ type , scales = 'free')
print(param_raw)

prior <-  prior(normal(0, 1), nlpar = "b1", class = 'b', lb = 0) +
prior(normal(0, 1), nlpar = "b2", class = 'b', lb = 0) +
prior(normal(0, 1), nlpar = "b3", class = 'b', lb = 0)

fit.v.exp <- brm(bf(param ~  -b1  * exp(-b2 * age) + b3,
		b1 ~ 1, b2 ~ 1, b3 ~ 1, nl = TRUE),
	     data = data %>% filter(type == 'v'),
	     prior = prior,
	     family = Gamma(link = 'identity'),
	     cores = 4, chains = 4,
	     control = list(adapt_delta = .95,  max_treedepth = 12),
	     iter = 8000,  warmup = 4000, seed = 123,
	     save_pars = save_pars(all = TRUE),
	     save_model = 'v_exp.stan',file = 'v_exp'
	     )
fit.v.puissance <- brm(bf(param ~ -b1  * age ^(-b2)  + b3,
		b1 ~ 1, b2 ~ 1, b3 ~ 1, nl = TRUE),
	     data = data %>% filter(type == 'v'),
	     prior = prior,
	     family = Gamma(link = 'identity'),
	     cores = 4, chains = 4,
	     control = list(adapt_delta = .95,  max_treedepth = 12),
	     iter = 8000,  warmup = 4000, seed = 123,  
	     save_model = 'v_puissance.stan', file = 'v_puisssance',
	     save_pars = save_pars(all = TRUE)
	     )
fit.v.linear <- brm(param ~  age,
	     data = data %>% filter(type == 'v'),
	     prior = c(set_prior("normal(0,1)",  class = "b")),
	     family = Gamma(link = 'identity'),
	     cores = 4, chains = 4,
	     control = list(adapt_delta = .99,  max_treedepth = 12),
	     iter = 8000,  warmup = 4000, seed = 123,  
	     save_model = 'v_linear.stan', 
	     save_pars = save_pars(all = TRUE),
	     file = 'v_linear'
	     )

v.exp <- pp_check(fit.v.exp, nsamples = 100)
v.puissance <- pp_check(fit.v.puissance, nsamples = 100)
v.linear <- pp_check(fit.v.linear, nsamples = 100)
pp_v <- ggarrange(v.exp, v.puissance, v.linear, ncol = 1,  labels = c('exp', 'power', 'linear'))
print(pp_v)

exp <-  conditional_effects(fit.v.exp)$age %>%
				     rename(e = estimate__,  u = upper__, l = lower__) %>%
				     select(age, e, u, l) %>%
				     mutate(model = 'exp')
power <- conditional_effects(fit.v.puissance)$age %>%
					    rename(e = estimate__, u = upper__, l = lower__) %>%
					    select(age, e,  u, l) %>%
					    mutate(model = 'power')
linear <- conditional_effects(fit.v.linear)$age %>%
					  rename(e = estimate__,  u = upper__, l = lower__) %>%
					  select(age, e,  u, l) %>%
					  mutate(model = 'linear')
d <- rbind(exp, power)
d <- rbind(d, linear)

dd <- data %>% filter(type == 'v') %>% select(age,param) %>% mutate(model = 'data')

p <- ggplot(data = d, aes(x = age, y = e, group = model), colour = group) +
  geom_line(aes(colour = model)) +
  geom_ribbon(aes(ymin=l, ymax=u, fill = model), alpha = .2) +
  geom_point(data= dd, mapping = aes(x = age, y = param))
print(p)

loo.v.exp <-  loo(fit.v.exp, moment_match = TRUE)
loo.v.puissance <-  loo(fit.v.puissance, moment_match = TRUE)
loo.v.linear <-  loo(fit.v.linear, moment_match = TRUE)
loo.v <- loo_compare(loo.v.exp, loo.v.puissance, loo.v.linear)

fit.a.exp <- brm(bf(param ~  b1  * exp(-b2 * age) + b3,
		b1 ~ 1, b2 ~ 1, b3 ~ 1, nl = TRUE),
	     data = data %>% filter(type == 'a'),
	     prior = prior,
	     family = Gamma(link = 'identity'),
	     cores = 4, chains = 4,
	     control = list(adapt_delta = .95,  max_treedepth = 12),
	     iter = 8000,  warmup = 4000, seed = 123,  
	     save_model = 'a_exp.stan',
             save_pars = save_pars(all = TRUE),
             file = 'a_exp'
	     )
fit.a.puissance <- brm(bf(param ~ b1  * age ^(-b2)  + b3,
		b1 ~ 1, b2 ~ 1, b3 ~ 1, nl = TRUE),
	     data = data %>% filter(type == 'a'),
	     prior = prior,
	     family = Gamma(link = 'identity'),
	     cores = 4, chains = 4,
	     control = list(adapt_delta = .95,  max_treedepth = 12),
	     iter = 8000,  warmup = 4000, seed = 123,  
	     save_model = 'a_puissance.stan',
             save_pars = save_pars(all = TRUE),
             file = 'a_puissance'
	     )
fit.a.linear <- brm(param ~  age,
	     data = data %>% filter(type == 'a'),
	     prior = c(set_prior("normal(0,1)", class = "b")),
	     family = Gamma(link = 'identity'),
	     cores = 4, chains = 4,
	     control = list(adapt_delta = .95,  max_treedepth = 12),
	     iter = 8000,  warmup = 4000, seed = 123,  
	     save_model = 'a_linear.stan', 
	     save_pars = save_pars(all = TRUE),
             file = 'a_linear'
	     )

loo.a.exp <-  loo(fit.a.exp, moment_match = TRUE)
loo.a.puissance <-  loo(fit.a.puissance, moment_match = TRUE)
loo.a.linear <-  loo(fit.a.linear, moment_match = TRUE)
loo.a <- loo_compare(loo.a.exp, loo.a.puissance, loo.a.linear)
#loo.a
#BF_informative <- bayes_factor(fit.a.exp,fit.a.puissance)
#BF_informative

a.exp <- pp_check(fit.a.exp, nsamples = 100)
a.puissance <- pp_check(fit.a.puissance, nsamples = 100)
a.linear <- pp_check(fit.a.linear, nsamples = 100)
pp_a <- ggarrange(a.exp, a.puissance, a.linear, ncol = 1,  labels = c('exp', 'power', 'linear'))
print(pp_a)

exp <-  conditional_effects(fit.a.exp)$age %>%
				     rename(e = estimate__,  u = upper__, l = lower__) %>%
				     select(age, e, u, l) %>%
				     mutate(model = 'exp')
power <- conditional_effects(fit.a.puissance)$age %>%
					    rename(e = estimate__, u = upper__, l = lower__) %>%
					    select(age, e,  u, l) %>%
					    mutate(model = 'power')
linear <- conditional_effects(fit.a.linear)$age %>%
					  rename(e = estimate__,  u = upper__, l = lower__) %>%
					  select(age, e,  u, l) %>%
					  mutate(model = 'linear')
d <- rbind(exp, power)
d <- rbind(d, linear)

dd <- data %>% filter(type == 'a') %>% select(age,param) %>% mutate(model = 'data')

p <- ggplot(data = d, aes(x = age, y = e, group = model), colour = group) +
  geom_line(aes(colour = model)) +
  geom_ribbon(aes(ymin=l, ymax=u, fill = model), alpha = .2) +
  geom_point(data= dd, mapping = aes(x = age, y = param))
print(p)

fit.ter.exp <- brm(bf(param ~  b1  * exp(-b2 * age) + b3,
		b1 ~ 1, b2 ~ 1, b3 ~ 1, nl = TRUE),
	     data = data %>% filter(type == 'ter'),
	     prior = prior,
	     family = Gamma(link = 'identity'),
	     cores = 4, chains = 4,
	     control = list(adapt_delta = .95,  max_treedepth = 12),
	     iter = 8000,  warmup = 4000, seed = 123,  
	     save_model = 'ter_exp.stan',
             save_pars = save_pars(all = TRUE),
             file = 'ter_exp'
	     )
fit.ter.puissance <- brm(bf(param ~ b1  * age ^(-b2)  + b3,
		b1 ~ 1, b2 ~ 1, b3 ~ 1, nl = TRUE),
	     data = data %>% filter(type == 'ter'),
	     prior = prior,
	     family = Gamma(link = 'identity'),
	     cores = 4, chains = 4,
	     control = list(adapt_delta = .95,  max_treedepth = 12),
	     iter = 8000,  warmup = 4000, seed = 123,  
	     save_model = 'ter_puissance.stan',
             save_pars = save_pars(all = TRUE),
             file = 'ter_puissance'
	     )
fit.ter.linear <- brm(param ~  age,
	     data = data %>% filter(type == 'ter'),
	     prior = c(set_prior("normal(0,1)", class = "b")),
	     family = Gamma(link = 'identity'),
	     cores = 4, chains = 4,
	     control = list(adapt_delta = .95,  max_treedepth = 12),
	     iter = 8000,  warmup = 4000, seed = 123,  
	     save_model = 'ter_linear.stan',
             save_pars = save_pars(all = TRUE),
             file = 'ter_linear'
	     )

loo.ter.exp <-  loo(fit.ter.exp, moment_match = TRUE)
loo.ter.puissance <-  loo(fit.ter.puissance, moment_match = TRUE)
loo.ter.linear <-  loo(fit.ter.linear, moment_match = TRUE)
loo.ter <- loo_compare(loo.ter.exp, loo.ter.puissance, loo.ter.linear)

ter.exp <- pp_check(fit.ter.exp, nsamples = 100)
ter.puissance <- pp_check(fit.ter.puissance, nsamples = 100)
ter.linear <- pp_check(fit.ter.linear, nsamples = 100)
pp_ter <- ggarrange(ter.exp,ter.puissance,ter.linear, ncol = 1,  labels = c('exp', 'power', 'linear'))
print(pp_ter)

exp <-  conditional_effects(fit.ter.exp)$age %>%
				     rename(e = estimate__,  u = upper__, l = lower__) %>%
				     select(age, e, u, l) %>%
				     mutate(model = 'exp')
power <- conditional_effects(fit.ter.puissance)$age %>%
					    rename(e = estimate__, u = upper__, l = lower__) %>%
					    select(age, e,  u, l) %>%
					    mutate(model = 'power')
linear <- conditional_effects(fit.ter.linear)$age %>%
					  rename(e = estimate__,  u = upper__, l = lower__) %>%
					  select(age, e,  u, l) %>%
					  mutate(model = 'linear')
d <- rbind(exp, power)
d <- rbind(d, linear)

dd <- data %>% filter(type == 'ter') %>% select(age,param) %>% mutate(model = 'data')

p <- ggplot(data = d, aes(x = age, y = e, group = model), colour = group) +
  geom_line(aes(colour = model)) +
  geom_ribbon(aes(ymin=l, ymax=u, fill = model), alpha = .2) +
  geom_point(data= dd, mapping = aes(x = age, y = param))
print(p)

fit.tau.exp <- brm(bf(param ~  b1  * exp(-b2 * age) + b3,
		b1 ~ 1, b2 ~ 1, b3 ~ 1, nl = TRUE),
	     data = data %>% filter(type == 'tau'),
	     prior = prior,
	     family = Gamma(link = 'identity'),
	     cores = 4, chains = 4,
	     control = list(adapt_delta = .99,  max_treedepth = 16),
	     iter = 8000,  warmup = 5000, seed = 123,  
	     save_model = 'tau_exp.stan',
	     save_pars = save_pars(all = TRUE),
	     file = 'tau_exp'
	     )
 pp_check(fit.tau.exp)
fit.tau.puissance <- brm(bf(param ~ b1  * age ^(-b2)  + b3,
		b1 ~ 1, b2 ~ 1, b3 ~ 1, nl = TRUE),
	     data = data %>% filter(type == 'tau'),
	     prior = prior,
	     family = Gamma(link = 'identity'),
	     cores = 4, chains = 4,
	     control = list(adapt_delta = .95,  max_treedepth = 12),
	     iter = 8000,  warmup = 4000, seed = 123,  
	     save_model = 'tau_puissance.stan',
	     save_pars = save_pars(all = TRUE),
	     file = 'tau_puissance'
	     )
fit.tau.linear <- brm(param ~  age,
	     data = data %>% filter(type == 'tau'),
	     prior = c(set_prior("normal(0,1)", class = "b")),
	     family = Gamma(link = 'identity'),
	     cores = 4, chains = 4,
	     control = list(adapt_delta = .95,  max_treedepth = 12),
	     iter = 8000,  warmup = 4000, seed = 123,  
	     save_model = 'tau_linear.stan',
	     save_pars = save_pars(all = TRUE),
	     file = 'tau_linear'
	     )

loo.tau.exp <-  loo(fit.tau.exp, moment_match = TRUE, reloo = TRUE)
loo.tau.puissance <-  loo(fit.tau.puissance, moment_match = TRUE)
loo.tau.linear <-  loo(fit.tau.linear, moment_match = TRUE)
loo.tau <- loo_compare(loo.tau.exp, loo.tau.puissance, loo.tau.linear)

tau.exp <- pp_check(fit.tau.exp, nsamples = 100)
tau.puissance <- pp_check(fit.tau.puissance, nsamples = 100)
tau.linear <- pp_check(fit.tau.linear, nsamples = 100)
pp_tau <- ggarrange(tau.exp, tau.puissance, tau.linear, ncol = 1,  labels = c('exp', 'power', 'linear'))
print(pp_tau)

exp <-  conditional_effects(fit.tau.exp)$age %>%
				     rename(e = estimate__,  u = upper__, l = lower__) %>%
				     select(age, e, u, l) %>%
				     mutate(model = 'exp')
power <- conditional_effects(fit.tau.puissance)$age %>%
					    rename(e = estimate__, u = upper__, l = lower__) %>%
					    select(age, e,  u, l) %>%
					    mutate(model = 'power')
linear <- conditional_effects(fit.tau.linear)$age %>%
					  rename(e = estimate__,  u = upper__, l = lower__) %>%
					  select(age, e,  u, l) %>%
					  mutate(model = 'linear')
d <- rbind(exp, power)
d <- rbind(d, linear)

dd <- data %>% filter(type == 'tau') %>% select(age,param) %>% mutate(model = 'data')

p <- ggplot(data = d, aes(x = age, y = e, group = model), colour = group) +
  geom_line(aes(colour = model)) +
  geom_ribbon(aes(ymin=l, ymax=u, fill = model), alpha = .2) +
  geom_point(data= dd, mapping = aes(x = age, y = param))
print(p)

fit.max_ampl.exp <- brm(bf(param ~  b1  * exp(-b2 * age) + b3,
		b1 ~ 1, b2 ~ 1, b3 ~ 1, nl = TRUE),
	     data = data %>% filter(type == 'max_ampl'),
	     prior = prior,
	     family = Gamma(link = 'identity'),
	     cores = 4, chains = 4,
	     control = list(adapt_delta = .95,  max_treedepth = 12),
	     iter = 8000,  warmup = 4000, seed = 123,  
	     save_model = 'max_ampl_exp.stan',
             save_pars = save_pars(all = TRUE),
             file = 'max_ampl_exp'
	     )
fit.max_ampl.puissance <- brm(bf(param ~ b1  * age ^(-b2)  + b3,
		b1 ~ 1, b2 ~ 1, b3 ~ 1, nl = TRUE),
	     data = data %>% filter(type == 'max_ampl'),
	     prior = prior,
	     family = Gamma(link = 'identity'),
	     cores = 4, chains = 4,
	     control = list(adapt_delta = .95,  max_treedepth = 12),
	     iter = 8000,  warmup = 4000, seed = 123,  
	     save_model = 'max_ampl_puissance.stan',
             save_pars = save_pars(all = TRUE),
             file = 'max_ampl_puissance'
	     )
fit.max_ampl.linear <- brm(param ~  age,
	     data = data %>% filter(type == 'max_ampl'),
	     prior = c(set_prior("normal(0,1)", class = "b")),
	     family = Gamma(link = 'identity'),
	     cores = 4, chains = 4,
	     control = list(adapt_delta = .95,  max_treedepth = 12),
	     iter = 8000,  warmup = 4000, seed = 123,  
	     save_model = 'max_ampl_linear.stan',
	     save_pars = save_pars(all = TRUE),
             file = 'max_ampl_linear'
	     )

loo.max_ampl.exp <-  loo(fit.max_ampl.exp, moment_match = TRUE)
loo.max_ampl.puissance <-  loo(fit.max_ampl.puissance, moment_match = TRUE)
loo.max_ampl.linear <-  loo(fit.max_ampl.linear, moment_match = TRUE)
loo.max_ampl <- loo_compare(loo.max_ampl.exp, loo.max_ampl.puissance, loo.max_ampl.linear)

max_ampl.exp <- pp_check(fit.max_ampl.exp, nsamples = 100)
max_ampl.puissance <- pp_check(fit.max_ampl.puissance, nsamples = 100)
max_ampl.linear <- pp_check(fit.max_ampl.linear, nsamples = 100)
pp_max_ampl <- ggarrange(max_ampl.exp, max_ampl.puissance, max_ampl.linear, ncol = 1,  labels = c('exp', 'power', 'linear'))
print(pp_max_ampl)

exp <-  conditional_effects(fit.max_ampl.exp)$age %>%
				     rename(e = estimate__,  u = upper__, l = lower__) %>%
				     select(age, e, u, l) %>%
				     mutate(model = 'exp')
power <- conditional_effects(fit.max_ampl.puissance)$age %>%
					    rename(e = estimate__, u = upper__, l = lower__) %>%
					    select(age, e,  u, l) %>%
					    mutate(model = 'power')
linear <- conditional_effects(fit.max_ampl.linear)$age %>%
					  rename(e = estimate__,  u = upper__, l = lower__) %>%
					  select(age, e,  u, l) %>%
					  mutate(model = 'linear')
d <- rbind(exp, power)
d <- rbind(d, linear)

dd <- data %>% filter(type == 'max_ampl') %>% select(age,param) %>% mutate(model = 'data')

p <- ggplot(data = d, aes(x = age, y = e, group = model), colour = group) +
  geom_line(aes(colour = model)) +
  geom_ribbon(aes(ymin=l, ymax=u, fill = model), alpha = .2) +
  geom_point(data= dd, mapping = aes(x = age, y = param))
print(p)

fit.meanRT_comp.exp <- brm(bf(param/1000 ~  b1  * exp(-b2 * age) + b3,
		b1 ~ 1, b2 ~ 1, b3 ~ 1, nl = TRUE),
	     data = data %>% filter(type == 'meanRT_comp'),
	     prior = prior,
	     family = Gamma(link = 'identity'),
	     cores = 4, chains = 4,
	     control = list(adapt_delta = .95,  max_treedepth = 12),
	     iter = 8000,  warmup = 4000, seed = 123,  
	     save_model = 'meanRT_comp_exp.stan',
             save_pars = save_pars(all = TRUE),
             file = 'meanRT_comp_exp'
	     )
fit.meanRT_comp.puissance <- brm(bf(param/1000 ~ b1  * age ^(-b2)  + b3,
		b1 ~ 1, b2 ~ 1, b3 ~ 1, nl = TRUE),
	     data = data %>% filter(type == 'meanRT_comp'),
	     prior = prior,
	     family = Gamma(link = 'identity'),
	     cores = 4, chains = 4,
	     control = list(adapt_delta = .95,  max_treedepth = 12),
	     iter = 8000,  warmup = 4000, seed = 123,  
	     save_model = 'meanRT_comp_puissance.stan',
             save_pars = save_pars(all = TRUE),
             file = 'meanRT_comp_puissance'
	     )
fit.meanRT_comp.linear <- brm(param/1000 ~  age,
	     data = data %>% filter(type == 'meanRT_comp'),
	     prior = c(set_prior("normal(0,1)", class = "b")),
	     family = Gamma(link = 'identity'),
	     cores = 4, chains = 4,
	     control = list(adapt_delta = .95,  max_treedepth = 12),
	     iter = 8000,  warmup = 4000, seed = 123,  
	     save_model = 'meanRT_comp_linear.stan',
	     save_pars = save_pars(all = TRUE),
             file = 'meanRT_comp_linear'
	     )

loo.meanRT_comp.exp <-  loo(fit.meanRT_comp.exp, moment_match = TRUE)
loo.meanRT_comp.puissance <-  loo(fit.meanRT_comp.puissance, moment_match = TRUE)
loo.meanRT_comp.linear <-  loo(fit.meanRT_comp.linear, moment_match = TRUE)
loo.meanRT_comp <- loo_compare(loo.meanRT_comp.exp, loo.meanRT_comp.puissance, loo.meanRT_comp.linear)

meanRT_comp.exp <- pp_check(fit.meanRT_comp.exp, nsamples = 100)
meanRT_comp.puissance <- pp_check(fit.meanRT_comp.puissance, nsamples = 100)
meanRT_comp.linear <- pp_check(fit.meanRT_comp.linear, nsamples = 100)
pp_meanRT_comp <- ggarrange(meanRT_comp.exp, meanRT_comp.puissance, meanRT_comp.linear, ncol = 1,  labels = c('exp', 'power', 'linear'))
print(pp_meanRT_comp)

exp <-  conditional_effects(fit.meanRT_comp.exp)$age %>%
				     rename(e = estimate__,  u = upper__, l = lower__) %>%
				     select(age, e, u, l) %>%
				     mutate(model = 'exp')
power <- conditional_effects(fit.meanRT_comp.puissance)$age %>%
					    rename(e = estimate__, u = upper__, l = lower__) %>%
					    select(age, e,  u, l) %>%
					    mutate(model = 'power')
linear <- conditional_effects(fit.meanRT_comp.linear)$age %>%
					  rename(e = estimate__,  u = upper__, l = lower__) %>%
					  select(age, e,  u, l) %>%
					  mutate(model = 'linear')
d <- rbind(exp, power)
d <- rbind(d, linear)

dd <- data %>% filter(type == 'meanRT_comp') %>% select(age,param) %>% mutate(model = 'data')

p <- ggplot(data = d, aes(x = age, y = e, group = model), colour = group) +
  geom_line(aes(colour = model)) +
  geom_ribbon(aes(ymin=l, ymax=u, fill = model), alpha = .2) +
  geom_point(data= dd, mapping = aes(x = age, y = param/1000))
print(p)

fit.meanRT_incomp.exp <- brm(bf(param/1000 ~  b1  * exp(-b2 * age) + b3,
		b1 ~ 1, b2 ~ 1, b3 ~ 1, nl = TRUE),
	     data = data %>% filter(type == 'meanRT_incomp'),
	     prior = prior,
	     family = Gamma(link = 'identity'),
	     cores = 4, chains = 4,
	     control = list(adapt_delta = .95,  max_treedepth = 12),
	     iter = 8000,  warmup = 4000, seed = 123,  
	     save_model = 'meanRT_incomp_exp.stan',
             save_pars = save_pars(all = TRUE),
             file = 'meanRT_incomp_exp'
	     )
fit.meanRT_incomp.puissance <- brm(bf(param/1000 ~ b1  * age ^(-b2)  + b3,
		b1 ~ 1, b2 ~ 1, b3 ~ 1, nl = TRUE),
	     data = data %>% filter(type == 'meanRT_incomp'),
	     prior = prior,
	     family = Gamma(link = 'identity'),
	     cores = 4, chains = 4,
	     control = list(adapt_delta = .95,  max_treedepth = 12),
	     iter = 8000,  warmup = 4000, seed = 123,  
	     save_model = 'meanRT_incomp_puissance.stan',
             save_pars = save_pars(all = TRUE),
             file = 'meanRT_incomp_puissance'
	     )
fit.meanRT_incomp.linear <- brm(param/1000 ~  age,
	     data = data %>% filter(type == 'meanRT_incomp'),
	     prior = c(set_prior("normal(0,1)", class = "b")),
	     family = Gamma(link = 'identity'),
	     cores = 4, chains = 4,
	     control = list(adapt_delta = .95,  max_treedepth = 12),
	     iter = 8000,  warmup = 4000, seed = 123,  
	     save_model = 'meanRT_incomp_linear.stan',
	     save_pars = save_pars(all = TRUE),
             file = 'meanRT_incomp_linear'
	     )

loo.meanRT_incomp.exp <-  loo(fit.meanRT_incomp.exp, moment_match = TRUE)
loo.meanRT_incomp.puissance <-  loo(fit.meanRT_incomp.puissance, moment_match = TRUE)
loo.meanRT_incomp.linear <-  loo(fit.meanRT_incomp.linear, moment_match = TRUE)
loo.meanRT_incomp <- loo_compare(loo.meanRT_incomp.exp, loo.meanRT_incomp.puissance, loo.meanRT_incomp.linear)

meanRT_incomp.exp <- pp_check(fit.meanRT_incomp.exp, nsamples = 100)
meanRT_incomp.puissance <- pp_check(fit.meanRT_incomp.puissance, nsamples = 100)
meanRT_incomp.linear <- pp_check(fit.meanRT_incomp.linear, nsamples = 100)
pp_meanRT_incomp <- ggarrange(meanRT_incomp.exp, meanRT_incomp.puissance, meanRT_incomp.linear, ncol = 1,  labels = c('exp', 'power', 'linear'))
print(pp_meanRT_incomp)

exp <-  conditional_effects(fit.meanRT_incomp.exp)$age %>%
				     rename(e = estimate__,  u = upper__, l = lower__) %>%
				     select(age, e, u, l) %>%
				     mutate(model = 'exp')
power <- conditional_effects(fit.meanRT_incomp.puissance)$age %>%
					    rename(e = estimate__, u = upper__, l = lower__) %>%
					    select(age, e,  u, l) %>%
					    mutate(model = 'power')
linear <- conditional_effects(fit.meanRT_incomp.linear)$age %>%
					  rename(e = estimate__,  u = upper__, l = lower__) %>%
					  select(age, e,  u, l) %>%
					  mutate(model = 'linear')
d <- rbind(exp, power)
d <- rbind(d, linear)

dd <- data %>% filter(type == 'meanRT_incomp') %>% select(age,param) %>% mutate(model = 'data')

p <- ggplot(data = d, aes(x = age, y = e, group = model), colour = group) +
  geom_line(aes(colour = model)) +
  geom_ribbon(aes(ymin=l, ymax=u, fill = model), alpha = .2) +
  geom_point(data= dd, mapping = aes(x = age, y = param/1000))
print(p)

loo.v
loo.a
loo.ter
loo.tau
loo.max_ampl
loo.meanRT_comp
loo.meanRT_incomp

fixef(fit.a.linear)

fixef(fit.tau.linear)

fixef(fit.max_ampl.linear)

results <- data.frame(Parameter = character(), Estimate = numeric(), Est.Error = numeric(),  Q2.5 = numeric(), Q97.5 = numeric())
for (x in c('v','ter','meanRT_comp','meanRT_incomp'))
{
fit.x <- eval(as.name(paste('fit.', x,'.exp',sep = '')))  
row <-  c(Parameter = x, round(fixef(fit.x)[2,], digits = 2), R2 = round(bayes_R2(fit.x)[1], digits = 3))
row <- as.data.frame(t(row))
results <- rbind(results, row)
}
print(results)
