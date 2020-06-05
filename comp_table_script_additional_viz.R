pacman::p_load(reactable, dplyr, ggplot2, ggalt, ggthemes, bdscale, scales, lubridate)
table <- read.csv("C:/Users/kplank/Essex Property Trust, Inc/Costar Data - Documents/power_bi_rent_data/table_data.csv")

table %<>% select(-X)

table_1 <- table %>% select(Submarket, Market, RPM,
                            `Property Name` =Property.Name, 
                            Comp, Team,
                            `Property Avg. Asking Rent`= Property.Avg..Asking.Rent,
                            `Property % Chg since 2/28` =Property...Chg.since.2.28, 
                            `Comps Avg. Asking Rent` =Comps.Avg..Asking.Rent,
                            `Comps % Chg since 2/28`= Comps...Chg.since.2.28,
                            `ESS Premium to Comps, %`= ESS.Premium.to.Comps...,
                            `1 Bed % Chg since 2/28` = X1.Bed...Chg.since.2.28,
                            `1 Bed % Chg since 2/28 (Comp)` = X1.Bed...Chg.since.2.28..Comp.,
                            `2 Bed % Chg since 2/28`= X2.Bed...Chg.since.2.28,
                            `2 Bed % Chg since 2/28 (Comp)` = X2.Bed...Chg.since.2.28..Comp.,
                            `Studio % Chg since 2/28` = Studio...Chg.since.2.28,
                            `Studio % Chg since 2/28 (Comp)`= Studio...Chg.since.2.28..Comp.,
                            `# of Comps` = X..of.Comps,
                            `ESS. Submarket % Change since 2/28` = ESS..Submarket...Change.since.2.28,
                            `Submarket % Change since 2/28` = Submarket...Change.since.2.28,
                            `Submarket Avg. Asking Rent` = Submarket.Avg..Asking.Rent)





table_breakout <- table %>% select(Submarket, Market, RPM, `Property Name` =Property.Name, Comp, Team,
                                   `Property Avg. Asking Rent`= Property.Avg..Asking.Rent,
                                   #`Property % Chg since 2/28` =Property...Chg.since.2.28,
                                   `Comps Avg. Asking Rent` =Comps.Avg..Asking.Rent,
                                   #`Comps % Chg since 2/28`= Comps...Chg.since.2.28,
                                   `ESS Premium to Comps, %`= ESS.Premium.to.Comps...,
                                   #`1 Bed % Chg since 2/28` = X1.Bed...Chg.since.2.28, 
                                   #`1 Bed % Chg since 2/28 (Comp)` = X1.Bed...Chg.since.2.28..Comp.,
                                   #`2 Bed % Chg since 2/28`= X2.Bed...Chg.since.2.28,
                                   #`2 Bed % Chg since 2/28 (Comp)` = X2.Bed...Chg.since.2.28..Comp.,
                                   #`Studio % Chg since 2/28` = Studio...Chg.since.2.28,
                                   #`Studio % Chg since 2/28 (Comp)`= Studio...Chg.since.2.28..Comp.,
                                   #`# of Comps` = X..of.Comps,
                                   #`ESS. Submarket % Change since 2/28` = ESS..Submarket...Change.since.2.28,
                                   #`Submarket % Change since 2/28` = Submarket...Change.since.2.28,
                                   `Submarket Avg. Asking Rent` = Submarket.Avg..Asking.Rent,
                                   `1 Bed Asking Rent` = one_bedroom_asking_unit_prop,
                                   `1 Bed Asking Rent (Comp)` =one_bedroom_asking_unit_comp,
                                   `2 Bed Asking Rent` = two_bedroom_asking_rent_unit_prop,
                                   `2 Bed Asking Rent (Comp)` = two_bedroom_asking_rent_unit_comp,
                                   `Studio Asking Rent` = studio_asking_rent_unit_prop,
                                   `Studio Asking Rent (Comp)` = studio_asking_rent_unit_comp)


# one_bedroom_avg_asking_unit_prop,
# one_bedroom_avg_asking_unit_comp,
# two_bedroom_avg_asking_unit_prop,
# two_bedroom_avg_asking_unit_comp,
# studio_avg_asking_unit_prop,
# studio_avg_asking_unit_comp




reactable(table_1, filterable = TRUE, searchable = TRUE, resizable = TRUE, highlight = TRUE, groupBy=c("Market", "RPM", "Property Name"),minRows = 10,style = list(fontFamily = "Work Sans, sans-serif", fontSize = "12px"), 
         columns = list(
 `Property Avg. Asking Rent` = colDef(aggregate = "mean", format = colFormat(prefix = "$", separators = TRUE, digits = 0)),
 `Property % Chg since 2/28` = colDef(aggregate = "mean", style = JS("
   function(rowInfo) {
     var value = rowInfo.row.extra
     if (value > 0) {
       var color = '#008000'
     } else if (value < 0) {
       var color = '#e00000'
     } else {
       var color = '#777'
     }
     return { color: color, fontWeight: 'bold' }
   }
 "), format = colFormat(percent=TRUE, digits = 1)),
 `Comps Avg. Asking Rent` = colDef(aggregate = "mean", format = colFormat(prefix = "$", separators = TRUE, digits = 0)),
 `Comps % Chg since 2/28`= colDef(aggregate = "mean", format = colFormat(percent=TRUE, digits = 1)),
 `ESS Premium to Comps, %` = colDef(aggregate = "mean", format = colFormat(percent=TRUE, digits = 1)),
 `1 Bed % Chg since 2/28` = colDef(aggregate = "mean", format = colFormat(percent=TRUE, digits = 1)),
 `1 Bed % Chg since 2/28 (Comp)` = colDef(aggregate = "mean", format = colFormat(percent=TRUE, digits = 1)),
 `2 Bed % Chg since 2/28` = colDef(aggregate = "mean", format = colFormat(percent=TRUE, digits = 1)),
 `2 Bed % Chg since 2/28 (Comp)` = colDef(aggregate = "mean", format = colFormat(percent=TRUE, digits = 1)),
 `Studio % Chg since 2/28` = colDef(aggregate = "mean", format = colFormat(percent=TRUE, digits = 1)),
 `Studio % Chg since 2/28 (Comp)` = colDef(aggregate = "mean", format = colFormat(percent=TRUE, digits = 1)),
 `Submarket % Change since 2/28` = colDef(aggregate = "mean", format = colFormat(percent=TRUE, digits = 1)),
 `Submarket Avg. Asking Rent` = colDef(aggregate = "mean", format = colFormat(prefix = "$", separators = TRUE, digits = 0)),
 `# of Comps` = colDef(aggregate = "sum", format = colFormat(digits = 0)),
 `ESS. Submarket % Change since 2/28` = colDef(aggregate = "mean", format = colFormat(percent=TRUE, digits = 1))))










reactable(table_breakout, filterable = TRUE, searchable = TRUE, resizable = TRUE, highlight = TRUE, groupBy=c("Market", "RPM", "Property Name"),minRows = 10,style = list(fontFamily = "Work Sans, sans-serif", fontSize = "12px"), 
          columns = list(
            `Property Avg. Asking Rent` = colDef(aggregate = "mean", format = colFormat(prefix = "$", separators = TRUE, digits = 0)),
            `Comps Avg. Asking Rent` = colDef(aggregate = "mean", format = colFormat(prefix = "$", separators = TRUE, digits = 0)),
            `ESS Premium to Comps, %` = colDef(aggregate = "mean", format = colFormat(percent=TRUE, digits = 1)),
            `Submarket Avg. Asking Rent` = colDef(aggregate = "mean", format = colFormat(prefix = "$", separators = TRUE, digits = 0)),
            `1 Bed Asking Rent` = colDef(aggregate = "mean", format = colFormat(prefix = "$", separators = TRUE, digits = 0)),
            `1 Bed Asking Rent (Comp)` = colDef(aggregate = "mean", format = colFormat(prefix = "$", separators = TRUE, digits = 0)),
            `2 Bed Asking Rent` = colDef(aggregate = "mean", format = colFormat(prefix = "$", separators = TRUE, digits = 0)),
            `2 Bed Asking Rent (Comp)` = colDef(aggregate = "mean", format = colFormat(prefix = "$", separators = TRUE, digits = 0)),
            `Studio Asking Rent` = colDef(aggregate = "mean", format = colFormat(prefix = "$", separators = TRUE, digits = 0)),
            `Studio Asking Rent (Comp)` = colDef(aggregate = "mean", format = colFormat(prefix = "$", separators = TRUE, digits = 0))
            ))




# EARNINGS CHART----
cum_avg_ask_mark <- read.csv("C:/Users/kplank/Essex Property Trust, Inc/Costar Data - Documents/alternate_chart_data/earnings_cum_change.csv")
business_days <- ymd(levels(as.factor(cum_avg_ask_mark$date)))
cum_avg_ask_mark$date
cum_avg_ask_mark$date <- ymd(cum_avg_ask_mark$date)
cum_avg_ask_mark <- subset(cum_avg_ask_mark, cum_avg_ask_mark$date != "2020-02-28")
business_days <- ymd(levels(as.factor(cum_avg_ask_mark$date)))
cum_avg_ask_mark_plot <- ggplot(cum_avg_ask_mark, aes(x = date, y = cum_avg_asking_change)) + 
 geom_line(aes(color = Type), size = 1) +
 ggtitle("") + 
 xlab("") + 
 ylab("") +
 scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(min(cum_avg_ask_mark$cum_avg_asking_change)-.005,.015)) +
 theme_calc() +
 scale_colour_brewer(palette = "Set1") +
 #scale_color_manual(values = c("Avg. Asking Rent (All Units)" = "black", "1 Bedroom"="brown1", "2 Bedroom" = "royalblue2", "Studio" = "springgreen3")) +
 theme(axis.title = element_text(size = rel(1)), axis.text = element_text(size = 14),
          plot.title = element_text(size=24), panel.spacing.x = unit(5, "mm"),
          axis.title.x = element_blank(), axis.text.x = element_text(angle=45, hjust = 1, vjust = 1),
       legend.box = "horizontal") +
    scale_x_bd(business.dates=business_days, max.major.breaks = 10, labels=date_format('%d-%b'))+
    facet_wrap(. ~ essex_markets)

print(cum_avg_ask_mark_plot)

#Check Most Recent Change
cum_avg_ask_mark_curr <- subset(cum_avg_ask_mark, cum_avg_ask_mark$date == "2020-05-01")


#-----





