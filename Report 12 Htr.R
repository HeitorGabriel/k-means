# 0) Prelúdio =======

setwd('/home/heitor/Área de Trabalho/R Projects/Análise Macro/Labs/Lab 12')

library(tidyverse)
library(plotly)
library(tidymodels)

aa <- read_csv("snsdata.csv") %>% as_tibble()


# 1) Importação e Tratamento =======

aa %>% summary()
aa %>% str()

aa <- aa %>% mutate(gender = factor(gender))
aa$gender %>% summary()
aa$gender %>% table() %>% 
	prop.table() %>% round(digits = 4)

aa <- aa %>% 
	mutate( female =
				case_when(gender=='M'   ~ 0,
						  is.na(gender) ~ 0,
						  gender=='F'   ~ 1),
			unk_gender =
				case_when(is.na(gender) ~ 1,
						  gender=='M'   ~ 0,
						  gender=='F'   ~ 0))

#dds <- dds %>%
#	filter(!is.na(age)) %>% 
#	filter(between(age,13,22),
#		   .preserve = T)

sum(aa$female==1)
sum(aa$unk_gender==1)

aa$age <- replace(aa$age,
				  aa$age<=13|aa$age>20,
				  NA)
summary(aa$age)

# --- {
mg <- aa %>% 
	group_by(gradyear) %>%
	summarise(mean = mean(age, na.rm=TRUE))
mg$mean[mg$gradyear==2006]
rm(mg)
# --- }

aggregate(data = aa,
		  age ~ gradyear,
		  mean, na.rm = TRUE)

ave_age <- ave(age, gradyear,
			   FUN = function(x) mean(x, na.rm = TRUE))

aa$age <- if_else(is.na(age), ave_age, aa$age)
summary(aa$age)

# --- {
scale()
x <- matrix(log(1:10), ncol = 2)
x
centered.x <- scale(x, scale = F)
centered.x
centered.x <- scale(x, scale = T)
centered.x
centered.scaled.x <- scale(x)
centered.scaled.x
centered.x <- scale(x, scale = c(2,1))
centered.x
rm(x, centered.x, centered.scaled.x)
# --- }

bb <- aa %>% select(! c(gradyear, gender,
						age, female, unk_gender))

bb <- as.data.frame(
	lapply(bb, scale))

cc <- cbind(bb,
			'gradyear'   = aa$gradyear,
			'gender'     = aa$gender,
			'female'     = aa$female,
			'unk_gender' = aa$unk_gender)

# 2) Visualizações e Descrições =======

a1 <- aa %>% 
	select(! c(age, female, unk_gender, gender)) %>% 
	group_by(gradyear) %>% 
	summarise( across(.cols = 4:37,
					  .fns = sum)) %>%
	pivot_longer(cols = soccer:drugs,
				 names_to = 'Interests') %>% 
	pivot_wider(names_from = gradyear,
				values_from = value)

a2 <- aa %>% 
	select(! c(age, female, unk_gender, gradyear)) %>% 
	group_by(gender) %>% 
	summarise( across(.cols = 4:37,
					  .fns = sum))%>%
	pivot_longer(cols = soccer:drugs,
				 names_to = 'Interests') %>% 
	pivot_wider(names_from = gender,
				values_from = value)

gg1 <- a1 %>%
	select(Interests, `2006`) %>% 
	arrange(desc(`2006`)) %>% 
	slice_head(n = 15) %>%
	ggplot(aes(x=reorder(Interests, -`2006`),
			   y=`2006`)) +
	geom_col(fill = 'deepskyblue4')+
	theme(axis.text.x =
		  	element_text(angle = 45,
		  				 vjust = 0.8))
ggplotly(gg1)

gg2 <- a1 %>%
	select(Interests, `2007`) %>% 
	arrange(desc(`2007`)) %>% 
	slice_head(n = 15) %>%
	ggplot(aes(x=reorder(Interests, -`2007`),
			   y=`2007`)) +
	geom_col(fill = 'deepskyblue3')+
	theme(axis.text.x =
		  	element_text(angle = 45,
		  				 vjust = 0.8))
ggplotly(gg2)

gg3 <- a1 %>%
	select(Interests, `2008`) %>% 
	arrange(desc(`2008`)) %>% 
	slice_head(n = 15) %>%
	ggplot(aes(x=reorder(Interests, -`2008`),
			   y=`2008`)) +
	geom_col(fill = 'deepskyblue2')+
	theme(axis.text.x =
		  	element_text(angle = 45,
		  				 vjust = 0.8))
ggplotly(gg3)

gg4 <- a1 %>%
	select(Interests, `2009`) %>% 
	arrange(desc(`2009`)) %>% 
	slice_head(n = 15) %>%
	ggplot(aes(x=reorder(Interests, -`2009`),
			   y=`2009`)) +
	geom_col(fill = 'deepskyblue1')+
	theme(axis.text.x =
		  	element_text(angle = 45,
		  				 vjust = 0.8))
ggplotly(gg4)

gg5 <- a2 %>%
	select(Interests, M) %>% 
	arrange(desc(M)) %>% 
	slice_head(n = 15) %>%
	ggplot(aes(x=reorder(Interests, -M),
			   y=M)) +
	geom_col(fill = 'tomato3')+
	theme(axis.text.x =
		  	element_text(angle = 45,
		  				 vjust = 0.8))
ggplotly(gg5)

gg6 <- a2 %>%
	select(Interests, `F`) %>% 
	arrange(desc(`F`)) %>% 
	slice_head(n = 15) %>%
	ggplot(aes(x=reorder(Interests, -`F`),
			   y=`F`)) +
	geom_col(fill = 'tomato')+
	theme(axis.text.x =
		  	element_text(angle = 45,
		  				 vjust = 0.8))
ggplotly(gg6)

cc %>% 
	dplyr::group_by(gender) %>% 
	dplyr::summarise(mean_music = mean(music, na.rm=T),
					 mean_god = mean(god, na.rm=T),
					 mean_die = mean(die, na.rm=T),
					 mean_rock = mean(rock, na.rm=T),
					 mean_sex = mean(sex, na.rm=T)) 

# 3) O Modelo =======

set.seed(123)
sqrt(nrow(cc)/2)

km04 <- kmeans(dplyr::select(cc, -gender),
			  centers = 4)

summary(km04)
broom::tidy(km04)

broom::augment(km04, cc) %>% 
	ggplot(aes(jesus, rock, color=.cluster))+
	geom_point()

# 3.1) Escolha do K -------

kclusts <- 
	tibble(k = 1:100) %>%
	mutate(
		kclust = map(k,
					 ~kmeans(dplyr::select(cc, -gender),
					 		.x)),
		tidied = map(kclust, tidy),
		glanced = map(kclust, glance),
		augmented = map(kclust,
						augment,
						dplyr::select(cc, -gender)))

km_clusters <- 
	kclusts %>%
	unnest(cols = c(tidied))

km_assignments <- 
	kclusts %>% 
	unnest(cols = c(augmented))

km_clusterings <- 
	kclusts %>%
	unnest(cols = c(glanced))

km_clusterings %>% 
	ggplot(aes(k, tot.withinss)) +
	geom_line(alpha = 0.5,
			  size = 1.2,
			  color = "midnightblue") +
	geom_point(size = 2,
			   color = "midnightblue")

gg7 <- km_assignments %>% 
	ggplot(aes(x = jesus, y = rock)) +
	geom_point(aes(color = .cluster,
				   shape = .cluster), alpha = 0.8) + 
	facet_wrap(~ k)
gg7

km10 <- kmeans(dplyr::select(cc, -gender),
			   centers = 10)

summary(km10)
broom::tidy(km10)

broom::augment(km10, cc) %>% 
	ggplot(aes(die, bible, color=.cluster))+
	geom_point()






