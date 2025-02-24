## ===================================
## 
##    Figura 4
## 
## ===================================

pacman::p_load(tidyverse, rio, janitor,
               srvyr, hrbrthemes)

cp89 <- import("CP89.SAV")

ideol89 <- cp89 %>% 
  as_survey_design(weights = peso) %>%
  mutate(q38 = ifelse(q38 == 8, NA, q38)) %>% 
  mutate(ideol = case_when(q38 %in% c(1:3) ~ "esq",
                           q38 == 4 ~ "cen",
                           q38 %in% c(5:7) ~ "dir")) %>%
  group_by(ideol) %>% 
  summarise(media = survey_prop(vartype = "ci")) %>% 
  mutate(ano = 1989)


cp93 <- import("cp93.SAV")


ideol93 <- cp93 %>% 
  as_survey_design(weights = pesoe) %>%
  mutate(p18 = ifelse(p18 >= 11, NA, p18)) %>% 
  mutate(ideol = case_when(p18 %in% c(1:4) ~ "esq",
                           p18 %in% c(4:6) ~ "cen",
                           p18 %in% c(7:10) ~ "dir")) %>%
  group_by(ideol) %>% 
  summarise(media = survey_prop(vartype = "ci")) %>% 
  mutate(ano = 1993)


cp99 <- import("cp99.SAV")

ideol99 <- cp99 %>% 
  as_survey_design(weights = pesoe) %>%
  mutate(p20 = ifelse(p20 >= 8, NA, p20)) %>% 
  mutate(ideol = case_when(p20 %in% c(1:3) ~ "esq",
                           p20 == 4 ~ "cen",
                           p20 %in% c(5:7) ~ "dir")) %>%
  group_by(ideol) %>% 
  summarise(media = survey_prop(vartype = "ci")) %>% 
  mutate(ano = 1999)

eseb2002 <- import("base_eseb_2002.sav") %>% 
  clean_names() 

ideol02 <- eseb2002 %>%  
  as_survey_design(weights = peso_nac) %>%
  mutate(p50v1 = ifelse(p50v1 >= 66, NA, p50v1)) %>% 
  select(p50v1) %>% 
  drop_na() %>% 
  mutate(ideol = case_when(p50v1 %in% c(1:4) ~ "esq",
                           p50v1 %in% c(5:6) ~ "cen",
                           p50v1 %in% c(7:10) ~ "dir")) %>%
  group_by(ideol) %>% 
  summarise(media = survey_prop(vartype = "ci")) %>% 
  mutate(ano = 2002)


eseb2006 <- import("base_eseb_2006.sav") %>% 
  clean_names() 

ideol06 <- eseb2006 %>%  
  as_survey_design(weights = peso_2) %>%
  mutate(eseb19 = ifelse(eseb19 >= 66, NA, eseb19)) %>% 
  select(eseb19) %>% 
  drop_na() %>% 
  mutate(ideol = case_when(eseb19 %in% c(1:4) ~ "esq",
                           eseb19 %in% c(5:6) ~ "cen",
                           eseb19 %in% c(7:10) ~ "dir")) %>%
  group_by(ideol) %>% 
  summarise(media = survey_prop(vartype = "ci")) %>% 
  mutate(ano = 2006)


eseb2010 <- import("base_eseb_2010.sav") %>% 
  clean_names() 

ideol10 <- eseb2010 %>%  
  as_survey_design(weights = 1) %>%
  mutate(v70 = ifelse(v70 >= 11, NA, v70)) %>% 
  select(v70) %>% 
  drop_na() %>% 
  mutate(ideol = case_when(v70 %in% c(1:4) ~ "esq",
                           v70 %in% c(5:6) ~ "cen",
                           v70 %in% c(7:10) ~ "dir")) %>%
  group_by(ideol) %>% 
  summarise(media = survey_prop(vartype = "ci")) %>% 
  mutate(ano = 2010)

eseb2014 <- import("base_eseb_2014.sav") %>% 
  clean_names() 

ideol14 <- eseb2014 %>%  
  as_survey_design(weights = fpond) %>%
  mutate(q12 = ifelse(q12 >= 11, NA, q12)) %>% 
  select(q12) %>% 
  drop_na() %>% 
  mutate(ideol = case_when(q12 %in% c(1:4) ~ "esq",
                           q12 %in% c(5:6) ~ "cen",
                           q12 %in% c(7:10) ~ "dir")) %>%
  group_by(ideol) %>% 
  summarise(media = survey_prop(vartype = "ci")) %>% 
  mutate(ano = 2014)

eseb2018 <- import("base_eseb_2018.sav") %>% 
  clean_names() 

ideol18 <- eseb2018 %>%  
  as_survey_design(weights = 1) %>%
  mutate(q18 = ifelse(q18 >= 11, NA, q18)) %>% 
  select(q18) %>% 
  drop_na() %>% 
  mutate(ideol = case_when(q18 %in% c(1:4) ~ "esq",
                           q18 %in% c(5:6) ~ "cen",
                           q18 %in% c(7:10) ~ "dir")) %>%
  group_by(ideol) %>% 
  summarise(media = survey_prop(vartype = "ci")) %>% 
  mutate(ano = 2018)



eseb2022 <- import("base_eseb_2022.sav") %>% 
  clean_names() 

ideol22 <- eseb2022 %>%  
  as_survey_design(weights = peso) %>%
  mutate(q19 = ifelse(q19 >= 11, NA, q19)) %>% 
  select(q19) %>% 
  drop_na() %>% 
  mutate(ideol = case_when(q19 %in% c(1:4) ~ "esq",
                           q19 %in% c(5:6) ~ "cen",
                           q19 %in% c(7:10) ~ "dir")) %>%
  group_by(ideol) %>% 
  summarise(media = survey_prop(vartype = "ci")) %>% 
  mutate(ano = 2022)


total <- bind_rows(ideol89, ideol93, ideol99, 
          ideol02, ideol06, ideol10, 
          ideol14, ideol18, ideol22)


total$ideol <- factor(total$ideol, levels = c("esq", "cen", "dir"))


total %>% 
  filter(!is.na(ideol)) %>% 
  mutate(ideol = case_when(ideol == "cen" ~ "Centro  ",
                           ideol == "esq" ~ "Esquerda  ",
                           ideol == "dir" ~ "Direita  ")) %>% 
  mutate(ideol = factor(ideol, levels = c("Esquerda  ",
                                          "Centro  ", "Direita  "))) %>% 
  ggplot(aes(ano, media*100, color = ideol)) +
  geom_line() +
  geom_ribbon(aes(ymin = media_low*100, 
                  ymax = media_upp*100),
              linetype = 0,
              fill = "grey50", alpha =0.3) +
  theme_bw() +
  labs(x = "", y = "") +
  theme(legend.position = "bottom", 
        legend.title = element_blank()) +
  scale_color_manual(values = c("brown3",
                                "darkolivegreen4",
                                "darkblue"))
  
ggsave("Figura_4.jpg", dpi = 300, units = 'px', width = 2400, height = 1400)



