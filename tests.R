# Tests

f <- tibble::tibble(id = rep(1:3, each = 3), wave = c(2,3,1,2,3,1,1,2,3))
longd <- load_long_data(f, time_col = wave)
longd %>% refactor_wave(subset = c(2,3))
longd %>% refactor_wave() %>% class
.data <- longd
subset = c(2,3)



chns.data <- haven::read_sas("../stats-781/data/rst_12.sas7bdat")
chns.long <- chns.data %>% load_long_data(IDind, WAVE) %>%
  refactor_wave

types <- chns.long %>% dplyr::summarise_all(class) %>%
  dplyr::filter(dplyr::row_number() == dplyr::n()) %>%
  tidyr::gather(variable, class)

v <- integer(10000)
z <- integer(10000)
for (i in seq_len(10000)) {
  z[i] <- system.time(chns.long %>% plot_types)[3]
}

chns.data <- haven::read_sas("../stats-781/data/rst_12.sas7bdat")
chns.long <- chns.data %>% load_long_data(IDind, WAVE) %>%
  refactor_wave
chns.long %<>% dplyr::mutate(A5E = factor(A5E))
chns.long %>% plot_types %>% dplyr::filter(alluvial == TRUE)

chns.long %>% recode_long_lasang(A5E, sortorder = 0, completecases = TRUE) %>%
  #plot_long_lasang
  plot_long_alluvial

chns.long %>% recode_long_alluvial(A5E) %>%
  plot_long_alluvial


.data <- chns.long
response <- "A5E"


.data
chns.long
.data <- chns.long

foo <- chns.long %>% recode_long_lasang(A5E, sortorder = 2, completecases = FALSE)
bar <- foo[1:14,]
class(bar)
plot_long_lasang(bar)


wage_data2 <- brolgar::wages %>%
  tibble::as_tibble() %>%
  dplyr::mutate(black = as.logical(black),
                hispanic = as.logical(hispanic),
                ged = as.logical(ged),
                high_grade = as.factor(high_grade))

wages.long <- wage_data2 %>% load_long_data(id, xp)
wages.long %>% plot_long_line(ln_wages, subsetvar1 = "ged", subsetvar2 = "black") +
  facet_wrap(~high_grade)

zab <- function() {
  longZight:::plot_long_line(wages.long, ln_wages, subsetvar1 = "ged", subsetvar2 = "black")
}
debug(zab)
zab()


testfunc <- function(testpar = "test") {
  print(rlang::enexpr(testpar))
}
foo <- "test"
testfunc(foo)

loadedData <-
  longZight::refactor_wave(longZight::load_long_data(chns.data,
                                                   id_col = "IDind",
                                                   time_col = "WAVE",
                                                   ts.args = FALSE))
longZight::iz_plot_alluvial(loadedData, "A5E", completecases = TRUE, highlight = "9")

