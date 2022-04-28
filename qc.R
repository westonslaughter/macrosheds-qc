Selecting deleted bufferlibrary(macrosheds)
library(lubridate)
library(ggplot2)

ms_sites <- ms_download_site_data()

                                        # data download directory
ms_dir <- "./data/"

                                        # seperate reference and experimental sites
ref_sites <- ms_sites %>%
  filter(ws_status %in% "non-experimental")

exp_sites <- ms_sites %>%
  filter(ws_status %in% "experimental")

                                        # site from each domain with most observations
best_sites <- c()
ref_dmns <- unique(ref_sites$domain)

for(dmn in unique(ref_sites$domain)) {
  ref_dmn <- ref_sites %>%
    filter(domain == dmn)

  max_n_index <- which(ref_dmn$n_observations == max(ref_dmn$n_observations))
  max_n_site <- ref_dmn[max_n_index,]$site_code
  best_sites <- c(best_sites, max_n_site)
}

                                        # download data for these sites
## ms_download_core_data(
##   ms_dir,
##   domains = ref_dmns
## )

                                        # load in target site data
ref_q <- ms_load_product(
  ms_dir,
  "discharge",
  site_codes = best_sites
)

                                        # get var list
ms_vars <- ms_download_variables()
ms_vars[grep("Temperature", ms_vars$variable_name),]

my_vars <- c("Ca", "Mg", "Na", "K",
             "NO3", "PO4", "SiO2", "SiO3",
             "DOC", "temp")

                                        # load in target chem data
ref_chem <- ms_load_product(
  ms_dir,
  "stream_chemistry",
  ## filter_vars = my_vars,
  site_codes = best_sites,
  warn = FALSE,
  )

                                        # prepare discharge
q_is <- ref_q %>%
  filter(ms_status==0) %>%
  select(-ms_status, -ms_interp, -val_err)  %>%
  ## filter(grepl('IS', var)) %>%
  mutate(var = ms_drop_var_prefix(var),
         year = year(datetime),
         month = month(datetime),
         day = day(datetime),
         val = as.numeric(val)) %>%
  pivot_wider(names_from = var, values_from = val)

                                        # seperate grab and istantaneous
ref_gn <- ref_chem %>%
  filter(ms_status==0) %>%
  select(-ms_status, -ms_interp, -val_err)  %>%
  filter(grepl('GN_', var)) %>%
  mutate(var = ms_drop_var_prefix(var),
         year = year(datetime),
         month = month(datetime),
         day = day(datetime),
         val = as.numeric(val)) %>%
  filter(var %in% my_vars) %>%
  pivot_wider(names_from = var, values_from = val)

ref_is <- ref_chem %>%
  filter(ms_status==0) %>%
  select(-ms_status, -ms_interp, -val_err)  %>%
  filter(grepl('IS', var)) %>%
  mutate(var = ms_drop_var_prefix(var),
         year = year(datetime),
         month = month(datetime),
         day = day(datetime),
         val = as.numeric(val)) %>%
  filter(var %in% my_vars) %>%
  pivot_wider(names_from = var, values_from = val)

                                        # df
df <- ref_gn %>%
  merge(q_is,
        by=c("site_code", "datetime", "year", "month", "day")
        )

df <- df %>%
  mutate(
    decade = cut(df$year, breaks=c(1910,1919,1929,1939,1949,1959,1969,1979,1989,1999,2009,2019,2029),
                 labels=c("1910s","1920s","1930s","1940s","1950s","1960s","1970s","1980s","1990s","2000s","2010s","2020s")))

                                        # plotting libs
library(gridExtra)
library(scales)
library(grid)
library(plotly)
library(RColorBrewer)

                                        # df
paris_metro <- c("#FFCE00", "#0064B0", "#9F9825", "98D4E2", "C04191", "F28E42",
                 "83C491", "F3A4BA", "83C491", "ceadd2", "d5c900", "e3b32a",
                 "8d5e2a", "00814F", "98D4e2", "662483", "b90845", "f3a4ba",
                 "d5c900", "00a88f")

                                        # site | datetime | elements* | Q |
df %>%
  filter(decade != "2020s") %>%
  ggplot(
    aes(
      x = log(discharge),
      y = log(K),
      color = site_code
    )) +
  geom_point() +
  ## geom_smooth(method="lm") +
  facet_grid(. ~decade)

df %>%
  filter(decade != "2020s") %>%
  ggplot(
    aes(
      x = log(discharge),
      y = log(Ca),
      color = site_code
    )) +
  geom_point() +
  ## geom_smooth(method="lm") +
  facet_grid(. ~decade)

ggplotly(df %>%
         filter(decade != "2020s") %>%
         ggplot(
           aes(
             x = log(discharge),
             y = log(Mg),
             color = site_code
           )) +
         geom_point() +
         ## geom_smooth(method="lm") +
         facet_grid(. ~decade) +
         ggtitle("Q to [Mg]")+
         theme_minimal() +
         scale_fill_manual(values = paris_metro))

ggplotly(df %>%
         filter(decade != "2020s") %>%
         ggplot(
           aes(
             x = log(discharge),
             y = log(Na),
             color = site_code
           )) +
         geom_point() +
         ## geom_smooth(method="lm") +
         facet_grid(. ~decade))

ggplotly(df %>%
         filter(decade != "2020s") %>%
         ggplot(
           aes(
             x = log(discharge),
             y = log(DOC),
             color = site_code
           )) +
         geom_point() +
         ## geom_smooth(method="lm") +
         facet_grid(. ~decade))
