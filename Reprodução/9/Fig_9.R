## =====================================
## 
##  Figura 9
## 
## =====================================

rm(list = ls())

pacman::p_load(tidyverse, janitor, rio,
               srvyr, patchwork, gridExtra, 
               sjPlot)

eseb_2022 <- import("base_eseb_2022.sav")

## Funções Uteis -------------

removeraltos <- function(x){
  x <- ifelse(x >= 11, NA, x)
}

reordenar_temas <- function(x){
  x <- case_when(x == 3 ~ 2, 
                 x == 2 ~ 3, 
                 x == 1 ~ 1)
}

eseb_2022c <- eseb_2022 %>% 
  clean_names() %>% 
  select(peso, raca = d12a, renda = d09a_fx_rendaf, 
         idade = d01a_fx_id, reg, sexo = d02, 
         esc = d03, relig = d10, 
         gosta_lula = q17_5, gosta_bozo = q17_2,
         gosta_pt = q18_5, maipen = q31_1, 
         casagay = q31_2, adogay = q31_3, 
         penmor = q31_4, drogas = q31_5,
         armas = q31_6, aborto = q31_7, 
         prisaoaborto = q31_8, cotas = q31_9,
         privat = q31_11, pbf = q31_12, 
         auxilio = q31_13, ideol = q19) %>% 
  mutate_at(vars(ideol, drogas, gosta_lula, 
                 gosta_bozo, maipen, 
                 casagay, adogay, penmor, 
                 drogas, armas, aborto, 
                 prisaoaborto, cotas, privat, 
                 pbf, auxilio, raca, renda, 
                 esc, relig), removeraltos)  %>% 
  mutate_at(vars(drogas, maipen, casagay, adogay, 
                 penmor, armas, aborto, prisaoaborto, 
                 cotas, privat, 
                 pbf, auxilio), reordenar_temas) %>% 
  mutate(relig = case_when(relig == 5 ~ "evang",
                           relig == 3 ~ "cat",
                           TRUE ~ 'outros')) 

eseb_2022c <- eseb_2022c %>% 
  mutate(esc2 = ifelse(esc %in% c(8:10), 1, 0))  

## horse race models ------------------------

base_hr <- eseb_2022c %>% 
  select(maipen, armas, casagay,
         penmor, aborto, drogas,
         adogay, prisaoaborto, 
         cotas, privat, gosta_lula,
         gosta_bozo, ideol, pbf, esc2,
         gosta_pt, raca, idade, renda, 
         reg, sexo, esc, relig, peso) 

# colocar itens aa direita com valores mais altos
# centralizar variaveis

base_hr <- base_hr %>% 
  mutate(gosta_pt = ifelse(gosta_pt >= 95, NA, gosta_pt)) %>% 
  mutate(maipen = case_when(maipen == 1 ~ 3,
                            maipen == 3 ~ 1,
                            maipen == 2 ~ 2),
         penmor = case_when(penmor == 1 ~ 3,
                            penmor == 3 ~ 1,
                            penmor == 2 ~ 2),
         prisaoaborto = case_when(prisaoaborto == 1 ~ 3,
                                  prisaoaborto == 3 ~ 1,
                                  prisaoaborto == 2 ~ 2),
         privat = case_when(privat == 1 ~ 3,
                            privat == 3 ~ 1,
                            privat == 2 ~ 2)) %>% 
  mutate(operacional = (maipen + armas + casagay +
           penmor + aborto + drogas + adogay + 
           prisaoaborto + cotas + privat)/10) %>% 
  mutate_at(vars(operacional, gosta_pt, gosta_lula,
                gosta_bozo, idade, esc, 
                renda), arm::rescale)

summary(base_hr)

## densidadade da variavel operacional
# nada muito grave

base_hr %>% 
  ggplot(aes(operacional)) + 
  geom_density()

# Modelo de Regressão ---------------------------------

base_hrw <- base_hr %>% 
  as_survey_design(weights = peso)


base_hrw %>% 
  summarise(cor = survey_corr(ideol, operacional, 
                              na.rm = T))


m1 <- lm(ideol ~ gosta_bozo + 
           gosta_pt + operacional + raca + idade + 
           renda + factor(reg) + sexo + 
           esc + relig, data = base_hrw)

stargazer::stargazer(m1, type = "text")

m2 <- lm(ideol ~ gosta_bozo + 
           gosta_pt + operacional + raca + idade + 
           renda + factor(reg) + sexo + relig, 
         data = subset(base_hrw, esc2 == 0))

m3 <- lm(ideol ~ gosta_bozo + 
           gosta_pt + operacional + raca + idade + 
           renda + factor(reg) + sexo + relig, 
         data = subset(base_hrw, esc2 == 1))

stargazer::stargazer(m2, m3, type = "text")


gimp2 <- sjPlot::plot_models(m2,m3, grid = T,
                             axis.labels = c(
                               "NE", 'Sul', "CO", "SE",
                               "Relig (Outros)", "Renda",
                               "Sexo", "Idade", 
                               "Relig (Evang.)", "Raça", 
                               "Id. Operacional", "Desgosta PT", 
                               "Gosta Bolsonaro"),
                             m.labels = c("Não Sofisticados", "Sofisticados"),
                             vline.color = "red", 
                             colors = "black",
                             axis.title = 'Estimativa') +
  theme_light() +
  theme(legend.position = "none", 
        text = element_text(size = 14),
        strip.background=element_rect(fill="black")) +
  labs(x = "")

gimp2  

ggsave("Figura_9.jpg",dpi = 300, units = 'px', width = 1800, height = 1400)


