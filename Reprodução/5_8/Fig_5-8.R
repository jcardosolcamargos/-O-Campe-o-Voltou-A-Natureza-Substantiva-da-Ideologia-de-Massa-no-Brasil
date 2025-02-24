## ==========================================
## 
##  Figuras: 5 a 8
## 
## ==========================================

set.seed(3135)

pacman::p_load(rio, mirt, janitor, tidyverse,ggridges,srvyr,
               patchwork)

bd <- import("WVS_trends.rds") |> 
  filter(cow_alpha %in% "BRA") 

# Funções auxiliares --------------------------------

fixscale <- function(x){
  x <- ifelse(x < 1, NA, x)
}

revert <- function(x){ 
  x <- 11-x
}

tricotomize <- function(x){
  x <- case_when(x %in% c(1:4) ~ 1,
                 x %in% c(5:6) ~ 2,
                 x %in% c(7:10) ~ 3)
}

nahigh <- function(x){
  x <- ifelse(is.na(x), 9, x)
}

# Manipulação dos dados ----------------------------------

bd_c <- bd |>
  select(incequal = e035, privat = e036, govtres = e037,
         compet = e039,  merit = e040, race = x051,
         homo = f118, prost = f119, abort = f120,
         divor = f121, eutha = f122, suici = f123,
         weight = s019, year = s020, ideol = e033, 
         relig = f025, sex = x001, age = x003r,
         educ = x025, educ2018 = x025a_01, income = x047_wvs)  |>
  filter(year != 1997) |>
  mutate_at(vars(incequal, privat, compet, govtres,
                 merit, homo, prost, abort, divor, 
                 eutha, suici, educ, educ2018,
                 income, age, relig, race, ideol), fixscale) |>
  mutate(educ = ifelse(is.na(educ), educ2018, educ))  |>
  mutate(educ2 = ifelse(educ %in% c(6:8), 1, 0), 
         relig = case_when(relig == 1 ~ "cat",
                           relig == 2 ~ "evang",
                           TRUE ~ "outros"),
         race = ifelse(race == 76001, 1, 0)) |>
  mutate(simbolica = case_when(ideol %in% c(1:4) ~ "left",
                               ideol %in% c(5:6) ~ "center",
                               ideol %in% c(7:10) ~ "right"))  

# criacao de escalas ------------------------------

dim1991 <- bd_c |>
  filter(year == 1991) |>
  mutate_at(vars(compet, merit, privat,govtres, 
                 homo, abort, divor, prost), revert) 


dim1991eco <- dim1991 |>select(compet, merit, privat, govtres)

m1991eco <- mirt(dim1991eco, model = 1, itemtype = "graded")

lecon1991 <- as_tibble(summary(m1991eco)) |> 
  mutate(Ano = 1991,
         fator = 'Economia')
  
mod_factor1991eco <- fscores(m1991eco, full.scores=TRUE)

econscale1991 <- scale(mod_factor1991eco[, 1], center=TRUE, scale=TRUE)

dim1991soc <- dim1991 |>select(homo, abort, divor, prost)

m1991soc <- mirt(dim1991soc, model = 1, itemtype = "graded")

lmor1991 <- as_tibble(summary(m1991soc)) |> 
  mutate(Ano = 1991,
         fator = 'Moral')

mod_factor1991soc <- fscores(m1991soc, full.scores=TRUE)

moralscale1991 <- scale(mod_factor1991soc[, 1], center=TRUE, scale=TRUE)

moral_1_10_91 <- (mod_factor1991soc - min(mod_factor1991soc, na.rm = T))/
  (max(mod_factor1991soc, na.rm = T) - (min(mod_factor1991soc, na.rm = T)))* 10

econ_1_10_91 <- (mod_factor1991eco - min(mod_factor1991eco, na.rm = T))/
  (max(mod_factor1991eco, na.rm = T) - (min(mod_factor1991eco, na.rm = T)))* 10

bd_c <- bd_c |>
  mutate(econscale = ifelse(year == 1991, econscale1991, NA)) |>
  mutate(moralscale = ifelse(year == 1991, moralscale1991, NA)) |> 
  mutate(F1 = ifelse(year == 1991, moral_1_10_91, NA),
         F2 = ifelse(year == 1991, econ_1_10_91, NA))

dim2006 <- bd_c |>
  filter(year == 2006) |>
  mutate_at(vars(compet, merit, privat, 
                 homo, abort, divor, prost), revert) 

dim2006eco <- dim2006 |>select(compet, merit, privat, govtres)

m2006eco <- mirt(dim2006eco, model = 1, itemtype = "graded")

lecon2006 <- as_tibble(summary(m2006eco)) |> 
  mutate(Ano = 2006,
         fator = 'Economia')


mod_factor2006eco <- fscores(m2006eco, full.scores=TRUE)

econscale2006 <- scale(mod_factor2006eco[, 1], center=TRUE, scale=TRUE)

dim2006soc <- dim2006 |>select(homo, abort, divor, prost)

m2006soc <- mirt(dim2006soc, model = 1, itemtype = "graded")

lmor2006 <- as_tibble(summary(m2006soc)) |> 
  mutate(Ano = 2006,
         fator = 'Moral')

mod_factor2006soc <- fscores(m2006soc, full.scores=TRUE)

moralscale2006 <- scale(mod_factor2006soc[, 1], center=TRUE, scale=TRUE)

moral_1_10_06 <- (mod_factor2006soc - min(mod_factor2006soc, na.rm = T))/
  (max(mod_factor2006soc, na.rm = T) - (min(mod_factor2006soc, na.rm = T)))* 10

econ_1_10_06 <- (mod_factor2006eco - min(mod_factor2006eco, na.rm = T))/
  (max(mod_factor2006eco, na.rm = T) - (min(mod_factor2006eco, na.rm = T)))*10

bd_c <- bd_c |>
  mutate(econscale = ifelse(year == 2006, econscale2006, econscale)) |>
  mutate(moralscale = ifelse(year == 2006, moralscale2006, moralscale))  |> 
  mutate(F1 = ifelse(year == 2006, moral_1_10_06,F1),
         F2 = ifelse(year == 2006, econ_1_10_06,F2))

dim2014 <- bd_c |>
  filter(year == 2014) |>
  mutate_at(vars(compet, merit, privat, 
                 homo, abort, divor, prost), revert) 


dim2014eco <- dim2014 |>select(compet, merit, privat, govtres)

m2014eco <- mirt(dim2014eco, model = 1, itemtype = "graded")

lecon2014 <- as_tibble(summary(m2014eco)) |> 
  mutate(Ano = 2014,
         fator = 'Economia')

mod_factor2014eco <- fscores(m2014eco, full.scores=TRUE)

econscale2014 <- scale(mod_factor2014eco[, 1], center=TRUE, scale=TRUE)

dim2014soc <- dim2014 |>select(homo, abort, divor, prost)

m2014soc <- mirt(dim2014soc, model = 1, itemtype = "graded")

lmor2014 <- as_tibble(summary(m2014soc)) |> 
  mutate(Ano = 2014,
         fator = 'Moral')

mod_factor2014soc <- fscores(m2014soc, full.scores=TRUE)

moralscale2014 <- scale(mod_factor2014soc[, 1], center=TRUE, scale=TRUE)

moral_1_10_14 <- (mod_factor2014soc - min(mod_factor2014soc, na.rm = T))/
  (max(mod_factor2014soc, na.rm = T) - (min(mod_factor2014soc, na.rm = T)))*10

econ_1_10_14 <- (mod_factor2014eco - min(mod_factor2014eco, na.rm = T))/
  (max(mod_factor2014eco, na.rm = T) - (min(mod_factor2014eco, na.rm = T)))*10

bd_c <- bd_c |>
  mutate(econscale = ifelse(year == 2014, econscale2014, econscale)) |>
  mutate(moralscale = ifelse(year == 2014, moralscale2014, moralscale))  |> 
  mutate(F1 = ifelse(year == 2014, moral_1_10_14,F1),
         F2 = ifelse(year == 2014, econ_1_10_14,F2))  

dim2018 <- bd_c |>
  filter(year == 2018) |>
  mutate_at(vars(compet, merit, privat, 
                 homo, abort, divor, prost), revert) 


dim2018eco <- dim2018 |>select(compet, merit, privat, govtres)

m2018eco <- mirt(dim2018eco, model = 1, itemtype = "graded")

lecon2018 <- as_tibble(summary(m2018eco)) |> 
  mutate(Ano = 2018,
         fator = 'Economia')

mod_factor2018eco <- fscores(m2018eco, full.scores=TRUE)

econscale2018 <- scale(mod_factor2018eco[, 1], center=TRUE, scale=TRUE)

dim2018soc <- dim2018 |>select(homo, abort, divor, prost)

m2018soc <- mirt(dim2018soc, model = 1, itemtype = "graded")

lmor2018 <- as_tibble(summary(m2018soc)) |> 
  mutate(Ano = 2018,
         fator = 'Moral')

mod_factor2018soc <- fscores(m2018soc, full.scores=TRUE)

moralscale2018 <- scale(mod_factor2018soc[, 1], center=TRUE, scale=TRUE)

moral_1_10_18 <- (mod_factor2018soc - min(mod_factor2018soc, na.rm = T))/
  (max(mod_factor2018soc, na.rm = T) - (min(mod_factor2018soc, na.rm = T)))* 10

econ_1_10_18 <- (mod_factor2018eco - min(mod_factor2018eco, na.rm = T))/
  (max(mod_factor2018eco, na.rm = T) - (min(mod_factor2018eco, na.rm = T)))* 10

bd_c <- bd_c |>
  mutate(econscale = ifelse(year == 2018, econscale2018, econscale)) |>
  mutate(moralscale = ifelse(year == 2018, moralscale2018, moralscale))  |> 
  mutate(F1 = ifelse(year == 2018, moral_1_10_18,F1),
         F2 = ifelse(year == 2018, econ_1_10_18,F2)) 

# Exportando os fatores ----- 

rbind(lecon1991, lecon2006, lecon2014, lecon2018,
                 lmor1991, lmor2006, lmor2014, lmor2018) |> 
  select(-fcor) |> 
  rename('Carga Fatorial' = rotF,
         "Discriminação" = h2,
         "Fator" = fator) |> 
  mutate(Nomes = c(rep(c('Efeitos de competição','Papel do trabalho ',
                       'Condução privada x pública da economia',
                       'Responsabilidade do governo'),4),
                   rep(c('Homossexualidade','Aborto','Divorcio',
                         'Prostituição'),4))) |> 
  pivot_wider(id_cols = c('Fator','Nomes'),names_from = 'Ano',names_sep = '_',
              values_from = c('Carga Fatorial','Discriminação')) |> 
  export('Carga_fatorial.xlsx')


# Analisando os quantis ------

quantile(subset(bd_c, year == 1991)$moralscale, probs = c(0.4, 0.6), na.rm = T)
quantile(subset(bd_c, year == 1991)$econscale, probs = c(0.4, 0.6), na.rm = T)

quantile(subset(bd_c, year == 2006)$moralscale, probs = c(0.4, 0.6), na.rm = T)
quantile(subset(bd_c, year == 2006)$econscale, probs = c(0.4, 0.6), na.rm = T)

quantile(subset(bd_c, year == 2014)$moralscale, probs = c(0.4, 0.6), na.rm = T)
quantile(subset(bd_c, year == 2014)$econscale, probs = c(0.4, 0.6), na.rm = T)

quantile(subset(bd_c, year == 2018)$moralscale, probs = c(0.4, 0.6), na.rm = T)
quantile(subset(bd_c, year == 2018)$econscale, probs = c(0.4, 0.6), na.rm = T)

bd_c <- bd_c |>
  mutate(operacional = case_when(moralscale <= -0.14 & econscale <= -0.25 & year == 1991 ~ "esq_con",
                                 moralscale >= 0.40 & econscale >= 0.15 & year == 1991 ~ "dir_con",
                                 moralscale <= -0.14 & econscale >= 0.15 & year == 1991 ~ "liberais",
                                 moralscale >= 0.40 & econscale <= -0.25 & year == 1991 ~ "comunit")) |>
  mutate(operacional = case_when(moralscale <= -0.32 & econscale <= -0.28 & year == 2006 ~ "esq_con",
                                 moralscale >= 0.29 & econscale >= 0.23 & year == 2006 ~ "dir_con",
                                 moralscale <= -0.32 & econscale >= 0.23 & year == 2006 ~ "liberais",
                                 moralscale >= 0.29 & econscale <= -0.28 & year == 2006 ~ "comunit",
                                 TRUE ~ operacional)) |>
  mutate(operacional = case_when(moralscale <= -0.24 & econscale <= -0.35 & year == 2014 ~ "esq_con",
                                 moralscale >= 0.29 & econscale >= 0.26 & year == 2014 ~ "dir_con",
                                 moralscale <= -0.24 & econscale >= 0.26 & year == 2014 ~ "liberais",
                                 moralscale >= 0.29 & econscale <= -0.35 & year == 2014 ~ "comunit",
                                 TRUE ~ operacional)) |>
  mutate(operacional = case_when(moralscale <= -0.24 & econscale <= -0.25 & year == 2018 ~ "esq_con",
                                 moralscale >= 0.32 & econscale >= 0.22 & year == 2018 ~ "dir_con",
                                 moralscale <= -0.24 & econscale >= 0.22  & year == 2018 ~ "liberais",
                                 moralscale >= 0.32 & econscale <= -0.25 & year == 2018 ~ "comunit",
                                 TRUE ~ operacional)) 


# Figura 5 ------------------------------
bd_c |> 
  select(F1, F2, ideol, year) |> 
  pivot_longer(cols = c("F1", "F2", "ideol"), values_to = "Valores", 
               names_to = "Categoria") |>
  mutate(Categoria = ifelse(Categoria == "F1", "Moral",
                            ifelse(Categoria == "F2", "Economia","Auto posicionamento")),
         year = as.factor(year)) |> 
  mutate(ordem = case_when(year == '1991' ~ 4,
                           year == '2006' ~ 3,
                           year == '2014' ~ 2,
                           year == '2018' ~ 1)) |> 
  ggplot(aes(x = Valores, y = fct_reorder(year, ordem), fill = fct_reorder(year, ordem)))+
  stat_density_ridges(quantile_lines = T, quantile_fun = mean) +
  facet_wrap(~Categoria)+
  theme_bw() + 
  labs(x = "", y = "")+
  xlim(0,10)+
  theme(legend.position = "none")+
  scale_fill_manual(values = c(rgb(.9,0,0,.5), rgb(0,.9,0,.5), rgb(0,0,.9,.5), rgb(.5,.5,0,.5)))

ggsave("Figura_5.png", units = 'px', width = 1800, height = 1000)


# Correlação no Survey  --------------------------------


bd_c |>
  mutate(operacional2 = moralscale + econscale) |>
  as_survey_design(weights = weight) %>%
  group_by(year) %>%
  summarise(cor = survey_corr(ideol, operacional2, na.rm=TRUE, vartype="ci"))


f_labels <- data.frame(year = c(1991, 2006, 2014, 2018), 
                       label = c("r = 0,05", "r = - 0,04", "r = - 0,01", "r = 0,08"))

# Figura 6 ------------------------------
bd_c |>
  mutate(year = as.double(year)) |>
  mutate(operacional2 = moralscale + econscale) |>
  as_survey_design(weights = weight) |>
  ggplot(aes(ideol, operacional2)) +
  geom_smooth(method = "lm", colour = "black") +
  facet_wrap(~year) +
  theme_light() +
  labs(x = "", y = "") +
  geom_text(x = 9.5, y = -0.42, aes(label = label), data = f_labels) +
  theme(strip.background =element_rect(fill="black"), 
        text = element_text(size = 15))  

ggsave("Figura_6.png", dpi = 300, units = 'px', width = 2200, height = 1000)

# Analise de consistência ----------------


dircon <- bd_c |>
  as_survey_design(weights = weight) |>
  filter(simbolica == "right") |>
  mutate(year = as.character(year)) |>
  select(year, operacional) |>
  mutate(operacional = ifelse(operacional %in% c("comunit", "liberais"), "Direita 1 Dimensão",
                              operacional)) |>
  drop_na() |>
  group_by(year, operacional) |>
  summarise(mean = survey_prop(vartype = "ci")*100,
            total = survey_total()*100) |>
  mutate(operacional = case_when(operacional == "dir_con" ~ "Direita Consistente",
                                 operacional == "esq_con" ~ "Esquerda Consistente",
                                 TRUE ~ operacional)) |>
  filter(operacional %in% c("Direita Consistente"))

esqcon <- bd_c |>
  as_survey_design(weights = weight) |>
  filter(simbolica == "left") |>
  mutate(year = as.character(year)) |>
  select(year, operacional) |>
  mutate(operacional = ifelse(operacional %in% c("comunit", "liberais"), "Direita 1 Dimensão",
                              operacional)) |>
  drop_na() |>
  group_by(year, operacional) |>
  summarise(mean = survey_prop(vartype = "ci")*100,
            total = survey_total()*100) |>
  mutate(operacional = case_when(operacional == "dir_con" ~ "Direita Consistente",
                                 operacional == "esq_con" ~ "Esquerda Consistente",
                                 TRUE ~ operacional)) |>
  filter(operacional %in% c("Esquerda Consistente"))

# Figura 7 ------------------------------
gconsist <- bind_rows(esqcon, dircon) |>
  mutate(operacional = ifelse(operacional == "Esquerda Consistente",
                              "Consistentes de Esquerda   ", "Consistentes de Direita")) |>
  mutate(operacional = fct_relevel(operacional, "Consistentes de Esquerda   ",
                                   "Consistentes de Direita")) |>
  arrange(year) |>group_by(year, operacional) %>%
  ggplot(aes(year, mean, fill = operacional)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean_low, ymax = mean_upp), 
                width = 0.2, position = position_dodge(0.9)) +
  theme_light() +
  labs(x = "", y = "") +
  scale_fill_manual(values = c("brown3", "dodgerblue3")) +
  theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        text = element_text(size = 14)) +
  scale_y_continuous(limits = c(0, 55))

ggsave("Figura_7.png",plot = gconsist, dpi = 300, units = 'px', width = 1800, height = 1000)



#  Consistencia entre subgrupos ---------------

bd_cwr <- bd_c |>
  as_survey_design(weights = weight) %>%
  filter(simbolica == "right") |>
  mutate(operacional2 = ifelse(operacional == "dir_con", 1, 0))   


mdir1991 <- glm(operacional2 ~ educ + sex + race + relig + 
                  age, data = subset(bd_cwr, year == 1991), family = "binomial")

mdir2006 <- glm(operacional2 ~ educ + sex + race + relig + 
                  age, data = subset(bd_cwr, year == 2006), family = "binomial")

mdir2014 <- glm(operacional2 ~ educ + sex + race + relig + 
                  age, data = subset(bd_cwr, year == 2014), family = "binomial")

mdir2018 <- glm(operacional2 ~ educ + sex + race + relig + 
                  age, data = subset(bd_cwr, year == 2018), family = "binomial")

mdirtotal <- glm(operacional2 ~ educ + sex + race + relig + 
                   age, data = bd_cwr, family = "binomial")

mdirtotal_int <- glm(operacional2 ~ as.factor(year)*(educ + sex + race + relig + 
                                                       age), data = bd_cwr, family = "binomial")

stargazer::stargazer(mdir1991, mdir2006, mdir2014, mdir2018, 
                     mdirtotal,mdirtotal_int, type = "text") 


bd_cwr <- bd_c |>
  as_survey_design(weights = weight) %>%
  filter(simbolica == "left") |>
  mutate(operacional2 = ifelse(operacional == "esq_con", 1, 0))   


mesq1991 <- glm(operacional2 ~ educ + sex + race + relig + 
                  age, data = subset(bd_cwr, year == 1991), family = "binomial")

mesq2006 <- glm(operacional2 ~ educ + sex + race + relig + 
                  age, data = subset(bd_cwr, year == 2006), family = "binomial")

mesq2014 <- glm(operacional2 ~ educ + sex + race + relig + 
                  age, data = subset(bd_cwr, year == 2014), family = "binomial")

mesq2018 <- glm(operacional2 ~ educ + sex + race + relig + 
                  age, data = subset(bd_cwr, year == 2018), family = "binomial")

mesqtotal <- glm(operacional2 ~ educ + sex + race + relig + 
                   age, data = bd_cwr, family = "binomial")


stargazer::stargazer(mesq1991, mesq2006, mesq2014, mesq2018, 
                     mesqtotal, type = "text") 



bd_cwtot <- bd_c |>
  as_survey_design(weights = weight) %>%
  mutate(operacional2 = ifelse(operacional %in% c("dir_con", "esq_con"), 1, 0))   


m1991 <- glm(operacional2 ~ educ + sex + race + relig + 
               age, data = subset(bd_cwtot, year == 1991), family = "binomial")

m2006 <- glm(operacional2 ~ educ + sex + race + relig + 
               age, data = subset(bd_cwtot, year == 2006), family = "binomial")

m2014 <- glm(operacional2 ~ educ + sex + race + relig + 
               age, data = subset(bd_cwtot, year == 2014), family = "binomial")

m2018 <- glm(operacional2 ~ educ + sex + race + relig + 
               age, data = subset(bd_cwtot, year == 2018), family = "binomial")

mtotal <- glm(operacional2 ~ educ + sex + race + relig + 
                age, data = bd_cwtot, family = "binomial")


stargazer::stargazer(m1991, m2006, m2014, m2018, 
                     mtotal, type = "text") 


bd_c <- bd_c |>
  mutate(relig = factor(relig)) |>
  mutate(relig = relevel(relig, ref = "evang"))

bdesq <- bd_c |>
  as_survey_design(weights = weight) |>
  mutate(constesq = ifelse(operacional == "esq_con" & simbolica == "left",
                           1, 0)) 

mtesteesq <- glm(constesq ~ educ + sex + race + 
                   relig + age + income, data = bdesq, family = "binomial")

stargazer::stargazer(mtesteesq, type =  "text")

bddir <- bd_c |>
  as_survey_design(weights = weight) |>
  mutate(constdir = ifelse(operacional == "dir_con" & simbolica == "right",
                           1, 0)) 

bdesq |>names

mtestedir <- glm(constdir ~ educ + sex + race + 
                   relig + age + income, data = bddir, family = "binomial")

stargazer::stargazer(mtestedir, type =  "text")

# Figura 8 ------------------------------

jtools::plot_summs(mtesteesq, mtestedir, 
                   coefs = c("Escolaridade" = "educ", 
                             "Sexo (Mulheres)" = "sex",
                             "Raça (Brancos)" = "race",
                             "Relig. (Católicos)" = "religcat",
                             "Relig. (Outros)" = "religoutros",
                             "Idade" = "age",
                             "Renda" = "income"),
                   colors = c("brown3", "dodgerblue3"), 
                   model.names = c("Consistentes\nde Esquerda   ",
                                   "Consistentes\nde Direita")) +
  theme_light() +
  theme(legend.position =  "bottom", 
        legend.title = element_blank(), 
        text = element_text(size = 16)) +
  labs(x = "", y = "") +
  scale_x_continuous(limits = c(-1,1.5))

ggsave("Figura_8.jpg",dpi = 300, units = 'px', width = 1800, height = 1400)


