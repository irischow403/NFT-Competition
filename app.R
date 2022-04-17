#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(tidyverse)
library(ggplot2)
library(plotrix)
library(hrbrthemes)
library(viridis)

all_nft <- read_csv('all_nfts.csv')
all_nft$duration[all_nft$duration > 6.0000e+07] <- NA
all_nft$total_price[all_nft$total_price > 3.00000e+22] <- NA
all_nft$last_sale_total_price[all_nft$last_sale_total_price > 3.00000e+22] <- NA
all_nft$num_sales[all_nft$num_sales>15000] <- NA
all_nft$total_price[is.na(all_nft$total_price)] <- mean(na.omit(all_nft$total_price))
all_nft$last_sale_total_price[is.na(all_nft$last_sale_total_price)] <- mean(na.omit(all_nft$last_sale_total_price))
all_nft$duration[is.na(all_nft$duration)] <- mean(na.omit(all_nft$duration))
all_nft$num_sales[is.na(all_nft$num_sales)] <- mean(na.omit(all_nft$num_sales))
all_nft$favorite[is.na(all_nft$favorite)] <- mean(na.omit(all_nft$favorite))


asset <- read_csv('Assets.csv')
event <- read_csv('events.csv')
asset_focus <- na.omit(asset[, c('token_id', 'asset_favorites', 'asset_category', 'last_sale_payment_token_usd_price', 'last_sale_payment_token_eth_price')])
colnames(asset_focus) <- c('token_id', 'asset_favorites', 'asset_category', 'USD', 'ETH')
event_focus <- na.omit(event[,c('asset_token_id', 'auction_type', 'ending_price', 'starting_price', 'duration', 'is_private', 'quantity')])
combined <- asset_focus %>% inner_join(event_focus, by = c('token_id' = 'asset_token_id'))

#Input of Pie Chart
event_type <- na.omit(all_nft[,'event_type'])
c_prop <- length(event_type[event_type == 'created'])/length(event_type)*100
s_prop <- length(event_type[event_type == 'successful'])/length(event_type)*100
t_prop <- length(event_type[event_type == 'transfer'])/length(event_type)*100
o_prop <- length(event_type[event_type == 'offer_entered'])/length(event_type)*100
cancel_prop <- length(event_type[event_type == 'cancelled'])/length(event_type)*100
#bid_prop <- length(event_type[event_type == 'bid_entered'])/length(event_type)*100
#bidw_prop <- length(event_type[event_type == 'bid_withdrawn'])/length(event_type)*100
a_prop <- length(event_type[event_type == 'approve'])/length(event_type)*100
slices_2 <- c(c_prop, s_prop, t_prop, o_prop, cancel_prop, a_prop)
pct_2 <- round(slices_2/sum(slices_2)*100)
lbls_2 <-c("created", "successful", "transfer", "cancelled", "offer_entered", "approve")
lbls_2 <- paste(lbls_2, pct_2) # add percents to labels
lbls_2 <- paste(lbls_2,"%",sep="") # ad % to labels

public_prop <- length(combined$is_private[combined$is_private == 'FALSE'])/nrow(combined)*100
private_prop <- length(combined$is_private[combined$is_private == 'TRUE'])/nrow(combined)*100
slices_3 <- c(public_prop, private_prop)
lbls_3<- c("Public", "Private")
pct_3<- round(slices_3/sum(slices_3)*100)
lbls_3 <- paste(lbls_3, pct_3) # add percents to labels
lbls_3 <- paste(lbls_3,"%",sep="") # ad % to labels

auction_type <- na.omit(all_nft$auction_type)
dutch_prop <- length(auction_type[auction_type == 'dutch'])/length(auction_type)*100
english_prop <- length(auction_type[auction_type == 'english'])/length(auction_type)*100
min_price_prop <- length(auction_type[auction_type == 'min_price'])/length(auction_type)*100
slices_4 <- c(dutch_prop, english_prop)
lbls_4 <- c("Dutch", "English")
pct_4 <- round(slices_4/sum(slices_4)*100)
lbls_4 <- paste(lbls_4, pct_4) # add percents to labels
lbls_4 <- paste(lbls_4,"%",sep="") # ad % to labels

# Heat Map
model_variables <- all_nft[, c('duration', 'last_sale_total_price','total_price', 'num_sales', 'asset_contract_owner', 'favorite')]
model_variables <- na.omit(model_variables)
cormat <- round(cor(model_variables), 2)
library(reshape2)
melted_cormat <- melt(cormat)
head(melted_cormat)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme('cyborg'), 
                navbarPage('NFT',
                           tabPanel('Boxplot',
                                    sidebarPanel(
                                        tags$h4('Pie Chart'),
                                        radioButtons('pie_input', '', choices = c('Event Type', 'Private or Public', 'Auction Type'), selected = 'Event Type'),
                                        plotOutput(('pieplot')),
                                        tags$h4('Heat Map'),
                                        plotOutput(('heatmap'))
                                        ), 
                                    mainPanel(h4('Asset Category'),
                                              selectInput('asset_category_input', '', choices = c('asset_favorites', 'duration', 'total_price', 'num_sales'), selected = 'asset_favorites'), #asset_favorites --> be sent to the server
                                              plotOutput("coolplot"), #coolplot --> this will be displayed
                                              tags$h4('Event Type'),
                                              plotOutput(('bubble'))
                                              )
                                    )))

# Define server logic required to draw a histogram
server <- function(input, output) {
    asset_category_subset <- reactive({
        return(all_nft[,input$asset_category_input])
    })
    x <- reactive({input$pie_input})
    
    output$coolplot <- renderPlot({
        all_nft %>% ggplot(aes(x = unlist(all_nft[,'asset_category']), y = unlist(asset_category_subset()), fill =  unlist(all_nft[,'asset_category']))) + geom_boxplot()
    })
    output$pieplot <- renderPlot({
        if (x() == 'Event Type'){
            k = slices_2
            t = lbls_2
            color = c('#4286f4','#bb3af2','#ed2f52','#efc023','#ea7441','#FFF1C9')
        }
        if (x() == 'Private or Public'){
            k = slices_3
            t = lbls_3
            color = c('pink','cyan')
        }
        if (x() == 'Auction Type'){
            k = slices_4
            t = lbls_4
            color = c('seagreen', 'yellow')
        }
        pie3D(k, labels = t,
              main="Pie Chart of Event Type", col = color , explode = 0.2, labelcex = 1)
    })
    output$heatmap <- renderPlot({
        ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + scale_x_discrete(guide = guide_axis(n.dodge=3)) + 
            geom_tile() + geom_text(aes(Var2, Var1, label = value), color = "salmon", size = 4) 
    })
    output$bubble <- renderPlot({
        all_nft %>% 
            ggplot(aes(x=asset_favorites, y= total_price, size = last_sale_total_price,fill = event_type)) +
            geom_point(alpha=0.5, shape=21, color="black") +
            scale_size(range = c(.1, 24), name="Population (M)") +
            scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A") +
            theme_ipsum() +
            theme(legend.position="bottom") +
            ylab("Total Price") +
            xlab("Asset Favorites") +
            theme(legend.position = "none")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
