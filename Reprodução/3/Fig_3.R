## =========================================
##
##  Figura 3
##
## =========================================

pacman::p_load(tidyverse, janitor, rio, 
               srvyr, correlation, rcompanion)

cara_18 <- import("EPEB2018_wide_original.dta") %>% 
  clean_names()

# Funções uteis

valtos <- function(x){
  x <- ifelse(x > 10, NA, x)
}

# A Cara da Democracia
# estabilidade ideologia simbolica -----------------

cara_18 %>% 
  select(id_new, pan20_w1, pan20_w2, 
         peso_w1) %>% 
  mutate_at(vars(pan20_w1, pan20_w2),
            valtos) %>% 
  filter(complete.cases(.)) %>% 
  select(pan20_w1, pan20_w2) %>% 
  correlation()

ideologia1 <- cara_18 %>% 
  select(id_new, pan20_w1, pan20_w2, 
         peso_w1) %>% 
  mutate_at(vars(pan20_w1, pan20_w2),
            valtos) %>% 
  filter(complete.cases(.)) %>% 
  mutate(estab = ifelse(pan20_w1 == pan20_w2, 1, 0)) %>% 
  srvyr::as_survey_design(weights = peso_w1) %>%
  group_by(estab) %>% 
  summarise(t = survey_prop(, vartype = "ci")) %>% 
  mutate(estab = ifelse(estab == 1, "Ideologia Cont.", estab)) %>% 
  filter(estab != 0)



# estabilidade categorica ---------------------------

teste <- cara_18 %>% 
  clean_names() %>% 
  select(id_new,pan20_w1, pan20_w2, 
         peso_w1) %>% 
  mutate_at(vars(pan20_w1, pan20_w2),
            valtos) %>% 
  mutate(simb_1 = case_when(pan20_w1 %in% c(1:4) ~ 'esq',
                            pan20_w1 %in% c(5:6) ~ 'cen',
                            pan20_w1 >= 7 ~ 'dir')) %>% 
  mutate(simb_2 = case_when(pan20_w2 %in% c(1:4) ~ 'esq',
                            pan20_w2 %in% c(5:6) ~'cen',
                            pan20_w2 >= 7 ~ 'dir')) %>% 
  filter(complete.cases(.)) 

cramerV(teste$simb_1, teste$simb_2)

tb <- xtabs(~teste$simb_1 + teste$simb_2)
cramerV(tb)


ideologia <- cara_18 %>% 
  clean_names() %>% 
  select(id_new,pan20_w1, pan20_w2, 
         peso_w1) %>% 
  mutate_at(vars(pan20_w1, pan20_w2),
            valtos) %>% 
  mutate(simb_1 = case_when(pan20_w1 %in% c(1:4) ~ 'esq',
                            pan20_w1 %in% c(5:6) ~ 'cen',
                            pan20_w1 >= 7 ~ 'dir')) %>% 
  mutate(simb_2 = case_when(pan20_w2 %in% c(1:4) ~ 'esq',
                            pan20_w2 %in% c(5:6) ~'cen',
                            pan20_w2 >= 7 ~ 'dir')) %>% 
  filter(complete.cases(.)) %>% 
  mutate(estab = ifelse(simb_1 == simb_2, 1, 0)) %>% 
  srvyr::as_survey_design(weights = peso_w1) %>%
  group_by(estab) %>% 
  summarise(t = survey_prop(, vartype = "ci")) %>% 
  mutate(estab = ifelse(estab == 1, "Ideologia Cat.", estab)) %>% 
  filter(estab != 0)


## outros temas ---------------------------------------

# maioridade penal

maipen <- cara_18 %>% 
  clean_names() %>% 
  select(id_new,pan35_1_w1, pan35_1_w2, 
         peso_w1) %>% 
  mutate_at(vars(pan35_1_w1, pan35_1_w2),
            valtos) %>% 
  filter(complete.cases(.)) %>% 
  mutate(estab = ifelse(pan35_1_w1 == pan35_1_w2, 1, 0)) %>% 
  srvyr::as_survey_design(weights = peso_w1) %>%
  group_by(estab) %>% 
  summarise(t = survey_prop(, vartype = "ci")) %>% 
  mutate(estab = ifelse(estab == 1, "Maioridade Penal", estab)) %>% 
  filter(estab != 0)

# casamento gay

casagay <- cara_18 %>% 
  clean_names() %>% 
  select(id_new,pan35_2_w1, pan35_2_w2, 
         peso_w1) %>% 
  mutate_at(vars(pan35_2_w1, pan35_2_w2),
            valtos) %>% 
  filter(complete.cases(.)) %>% 
  mutate(estab = ifelse(pan35_2_w1 == pan35_2_w2, 1, 0)) %>% 
  srvyr::as_survey_design(weights = peso_w1) %>%
  group_by(estab) %>% 
  summarise(t = survey_prop(, vartype = "ci")) %>% 
  mutate(estab = ifelse(estab == 1, "Homossexualidade", estab)) %>% 
  filter(estab != 0)


# pena de morte

penmor <- cara_18 %>% 
  clean_names() %>% 
  select(id_new,pan35_4_w1, pan35_4_w2, 
         peso_w1) %>% 
  mutate_at(vars(pan35_4_w1, pan35_4_w2),
            valtos) %>% 
  filter(complete.cases(.)) %>% 
  mutate(estab = ifelse(pan35_4_w1 == pan35_4_w2, 1, 0)) %>% 
  srvyr::as_survey_design(weights = peso_w1) %>%
  group_by(estab) %>% 
  summarise(t = survey_prop(, vartype = "ci")) %>% 
  mutate(estab = ifelse(estab == 1, "Pena de Morte", estab)) %>% 
  filter(estab != 0)

# drogas

drogas <- cara_18 %>% 
  clean_names() %>% 
  select(id_new,pan35_5_w1, pan35_5_w2, 
         peso_w1) %>% 
  mutate_at(vars(pan35_5_w1, pan35_5_w2),
            valtos) %>% 
  filter(complete.cases(.)) %>% 
  mutate(estab = ifelse(pan35_5_w1 == pan35_5_w2, 1, 0)) %>% 
  srvyr::as_survey_design(weights = peso_w1) %>%
  group_by(estab) %>% 
  summarise(t = survey_prop(, vartype = "ci")) %>% 
  mutate(estab = ifelse(estab == 1, "Drogas", estab)) %>% 
  filter(estab != 0)

# armas

armas <- cara_18 %>% 
  clean_names() %>% 
  select(id_new,pan35_6_w1, pan35_6_w2, 
         peso_w1) %>% 
  mutate_at(vars(pan35_6_w1, pan35_6_w2),
            valtos) %>% 
  filter(complete.cases(.)) %>% 
  mutate(estab = ifelse(pan35_6_w1 == pan35_6_w2, 1, 0)) %>% 
  srvyr::as_survey_design(weights = peso_w1) %>%
  group_by(estab) %>% 
  summarise(t = survey_prop(, vartype = "ci")) %>% 
  mutate(estab = ifelse(estab == 1, "Armas", estab)) %>% 
  filter(estab != 0)

# aborto

aborto <- cara_18 %>% 
  clean_names() %>% 
  select(id_new,pan35_7_w1, pan35_7_w2, 
         peso_w1) %>% 
  mutate_at(vars(pan35_7_w1, pan35_7_w2),
            valtos) %>% 
  filter(complete.cases(.)) %>% 
  mutate(estab = ifelse(pan35_7_w1 == pan35_7_w2, 1, 0)) %>% 
  srvyr::as_survey_design(weights = peso_w1) %>%
  group_by(estab) %>% 
  summarise(t = survey_prop(, vartype = "ci")) %>% 
  mutate(estab = ifelse(estab == 1, "Aborto", estab)) %>% 
  filter(estab != 0)

# cotas

cotas <- cara_18 %>% 
  clean_names() %>% 
  select(id_new,pan35_9_w1, pan35_9_w2, 
         peso_w1) %>% 
  mutate_at(vars(pan35_9_w1, pan35_9_w2),
            valtos) %>% 
  filter(complete.cases(.)) %>% 
  mutate(estab = ifelse(pan35_9_w1 == pan35_9_w2, 1, 0)) %>% 
  srvyr::as_survey_design(weights = peso_w1) %>%
  group_by(estab) %>% 
  summarise(t = survey_prop(, vartype = "ci")) %>% 
  mutate(estab = ifelse(estab == 1, "Cotas", estab)) %>% 
  filter(estab != 0)

# intervencao estado

intervencao <- cara_18 %>% 
  clean_names() %>% 
  select(id_new,pan37_w1, pan37_w2, 
         peso_w1) %>% 
  mutate_at(vars(pan37_w1, pan37_w2),
            valtos) %>% 
  filter(complete.cases(.)) %>% 
  mutate(pan37_w1 = case_when(pan37_w1 %in% c(1:4) ~ 1,
                              pan37_w1 %in% c(5,6) ~ 2,
                              pan37_w1 %in% c(7:10) ~ 3),
         pan37_w2 = case_when(pan37_w2 %in% c(1:4) ~ 1,
                              pan37_w2 %in% c(5,6) ~ 2,
                              pan37_w2 %in% c(7:10) ~ 3)) %>% 
  mutate(estab = ifelse(pan37_w1 == pan37_w2, 1, 0)) %>% 
  srvyr::as_survey_design(weights = peso_w1) %>%
  group_by(estab) %>% 
  summarise(t = survey_prop(, vartype = "ci")) %>% 
  mutate(estab = ifelse(estab == 1, "Intervenção Estado", estab)) %>% 
  filter(estab != 0)

cara_2018<- bind_rows(ideologia1, ideologia, aborto, armas, casagay, 
                      cotas, drogas, intervencao, maipen, 
                      penmor) 

# Brazilian Electoral Panel Study ------------

beps_10 <- import("beps_2014.dta") %>% 
  clean_names()


# estabilidade ideologia simbolica -----------------

beps_10 %>% 
  select(id_num, l1w1, l1w2, 
         weight_wave1samplew1) %>% 
  mutate_at(vars(l1w1, l1w2),
            valtos) %>% 
  filter(complete.cases(.)) %>% 
  select(l1w1, l1w2) %>% 
  correlation()

ideologia12010 <- beps_10 %>% 
  select(id_num, l1w1, l1w2, 
         weight_wave1samplew1) %>% 
  mutate_at(vars(l1w1, l1w2),
            valtos) %>% 
  filter(complete.cases(.)) %>% 
  mutate(estab = ifelse(l1w1 == l1w2, 1, 0)) %>% 
  srvyr::as_survey_design(weights = weight_wave1samplew1) %>%
  group_by(estab) %>% 
  summarise(t = survey_prop(, vartype = "ci")) %>% 
  mutate(estab = ifelse(estab == 1, "Ideologia Cont.", estab)) %>% 
  filter(estab != 0)



# estabilidade categorica ---------------------------

teste <- beps_10 %>% 
  select(id_num, l1w1, l1w2, 
         weight_wave1samplew1) %>% 
  mutate_at(vars(l1w1, l1w2),
            valtos) %>% 
  mutate(simb_1 = case_when(l1w1 %in% c(1:3) ~ 'esq',
                            l1w1 %in% c(4) ~ 'cen',
                            l1w1 %in% c(5:7) ~ 'dir')) %>% 
  mutate(simb_2 = case_when(l1w2 %in% c(1:3)~ 'esq',
                            l1w2 %in% c(4) ~'cen',
                            l1w2 %in% c(5:7) ~ 'dir')) %>% 
  filter(complete.cases(.)) 

cramerV(teste$simb_1, teste$simb_2)

tb <- xtabs(~teste$simb_1 + teste$simb_2)
cramerV(tb)


ideologia2010 <- beps_10 %>% 
  select(id_num, l1w1, l1w2, 
         weight_wave1samplew1) %>% 
  mutate_at(vars(l1w1, l1w2),
            valtos) %>% 
  mutate(simb_1 = case_when(l1w1 %in% c(1:3) ~ 'esq',
                            l1w1 %in% c(4) ~ 'cen',
                            l1w1 %in% c(5:7) ~ 'dir')) %>% 
  mutate(simb_2 = case_when(l1w2 %in% c(1:3)~ 'esq',
                            l1w2 %in% c(4) ~'cen',
                            l1w2 %in% c(5:7) ~ 'dir')) %>% 
  filter(complete.cases(.)) %>% 
  mutate(estab = ifelse(simb_1 == simb_2, 1, 0)) %>% 
  srvyr::as_survey_design(weights = weight_wave1samplew1) %>%
  group_by(estab) %>% 
  summarise(t = survey_prop(, vartype = "ci")) %>% 
  mutate(estab = ifelse(estab == 1, "Ideologia Cat.", estab)) %>% 
  filter(estab != 0)

beps_f <- rbind(ideologia12010,ideologia2010 ) 

# Unificando os bancos

beps_f <- beps_f |> mutate(ano = 2010)

cara_2018 <- cara_2018 |> mutate(ano = 2018)

full <- rbind(beps_f, cara_2018) |> arrange(t) |> 
  mutate(order = c(2,1,5,4,6,7,8,9,3,10,11,12))

full |> 
  ggplot(aes(fct_reorder(estab, -order), t, color = as.factor(ano))) +
  geom_pointrange(aes(ymin=t_low, ymax=t_upp), position = position_dodge(1)) +
  coord_flip() + labs(x = "", y = "", color = "") +
  scale_y_continuous(breaks = c(0.2, 0.3, 0.4, 0.5, 
                                0.6, 0.7, 0.8),
                     labels = scales::percent, 
                     limits = c(0.1, 0.81)) +
  scale_color_manual(values = c('grey50','black'))+
  theme_bw()+
  theme(legend.position = 'bottom')

ggsave("Figura_3.png", units = 'px', width = 1600, height = 1000)
