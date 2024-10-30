library(dplyr)
library(tidyr)
library(purrr)
rm(list = ls())

devtools::document()
devtools::load_all()

# meta --------------------------------------------------------------------

meta <- pull.acs.metadata(year = 2019)

# vacancies --------------------------------------------------------------------


vacancyts <- multiyr.acs.wrapper('B25034'
                               ,state = 37
                               , geo = 'tract'
                               ,years = 2019
                               ,metadata = meta
                               )

vacancyts %>%  count(label, year)

## specified counties ------------------------------------------------------

# demos -------------------------------------------------------------------

# devtools::load_all()

demots <- multiyr.acs.wrapper('B03002'
                              ,state = 44
                              ,years = 2019
                              , geo = 'county subdivision'
                              ,metadata = meta)

# only some counties

demots <- multiyr.acs.wrapper('B03002'
                              ,state = 44
                              ,cofps = '001'
                              ,years = 2019
                              , geo = 'county subdivision'
                              ,metadata = meta)



tmp <- demots %>%
  acs.demographic.recode()

tmp <- tmp %>%
  group_by(year, recode) %>%
  summarise(n = sum(estimate))

tmp %>%
  ggplot( aes(x = recode
              ,y = n
              ,fill = n) ) +
  geom_col() +
  facet_wrap( vars(year)) +
  coord_flip()


# try it with dif groups pulled out.
tmp <- demots %>%
  acs.demographic.recode(other.vars = c(8:11))

tmp <- tmp %>%
  group_by(year, recode) %>%
  summarise(n = sum(estimate))

tmp %>%
  ggplot( aes(x = recode
              ,y = n
              ,fill = n) ) +
  geom_col() +
  facet_wrap( vars(year)) +
  coord_flip()

# tmp$recode <- factor(tmp$recode, levels = rev(levels(tmp$recode)))
# dope!


# commutes ----------------------------------------------------------------

commutes <- multiyr.acs.wrapper('B08006'
                              ,state = 9
                              ,years = c(2011, 2021)
                              , geo = 'county'
                              ,metadata = meta)

#devtools::load_all()

tmp <- acs.commute.recode(commutes)

tmp <- tmp %>%
  group_by(year, recode) %>%
  summarise(n = sum(estimate)) %>%
  ungroup()

tmp %>%
  ggplot( aes(  x = year
                ,color = recode
                ,group = recode
                ,y = n)
  ) +
  geom_path(alpha = .7) +
  visaux::ts.labels.geom(tmp, 'recode', 'year') +
  scale_x_discrete( name = NULL) +
  theme(legend.position = 'top') +
  guides(fill = guide_legend(reverse=TRUE))


# if(!is.null(time.col))
#   p <- p +
#   facet_wrap(vars(!!.tc))

# test rent price levels --------------------------------------------------

get.all.rentals.by.price.lvl(37, 2021,'county')


