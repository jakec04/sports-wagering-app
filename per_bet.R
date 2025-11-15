# app.R
library(shiny)
library(scales)
library(bslib)

TAX_PER_WAGER <- 0.50

app_theme <- bs_theme(
  version = 5,
  base_font = font_google("Roboto"),
  heading_font = font_google("Roboto"),
  bg = "#f7f7f7",
  fg = "#111111"
)

ui <- fluidPage(
  theme = app_theme,
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f7f7f7;
        font-family: 'Roboto', sans-serif;
      }
      .app-container {
        max-width: 900px;
        margin: 0 auto;
        padding: 30px 15px 60px 15px;
        background-color: #ffffff;
        border-left: 1px solid #e2e2e2;
        border-right: 1px solid #e2e2e2;
      }
      .app-headline {
        font-size: 32px;
        line-height: 1.15;
        margin-bottom: 25px;
        font-weight: 700;
      }
      .app-input-panel {
        border-top: 1px solid #e2e2e2;
        border-bottom: 1px solid #e2e2e2;
        padding: 20px 0 16px 0;
        margin-bottom: 30px;
      }
      .app-instruction {
        font-size: 16px;
        font-weight: 700;
        margin-bottom: 6px;
        color: #222222;
      }
      .app-input-note {
        font-size: 12px;
        color: #777777;
        margin-top: 4px;
        margin-bottom: 16px;
      }
      .app-big-number {
        font-size: 30px;
        font-weight: 600;
        margin-bottom: 10px;
      }
      .app-subline {
        font-size: 14px;
        color: #444444;
        margin-bottom: 25px;
      }
      .app-impact-block {
        background: #f2f2f2;
        padding: 18px 20px;
        border-radius: 4px;
        margin-bottom: 20px;
      }
      .app-impact-title {
        font-size: 16px;
        font-weight: 700;
        margin-bottom: 6px;
      }
      .app-impact-line {
        font-size: 15px;
        margin-bottom: 4px;
      }
      .app-note {
        font-size: 12px;
        color: #777777;
        margin-top: 10px;
        max-width: 650px;
      }
    "))
  ),

  div(
    class = "app-container",

    # HEADLINE -------------------------------------------------------------
    div(class = "app-headline", "How Illinois’ New Per-Wager Tax Adds Up for Bettors"),

    # INPUT PANEL -----------------------------------------------------------
    div(
      class = "app-input-panel",

      # Bold instruction inside the panel
      div(
        class = "app-instruction",
        "Enter your betting habits to see how Illinois’ per-wager tax adds up over time."
      ),
      div(
        class = "app-input-note",
        HTML(
          'Default values reflect median weekly betting patterns reported in a 2024
           <a href="https://mattbrownecon.github.io/assets/papers/jmp/sportsbetting.pdf" target="_blank">
           Stanford study</a>.'
        )
      ),

      fluidRow(
        column(
          4,
          tags$label("Bets per week", style = "font-size: 12px;"),
          numericInput("bets_per_week", NULL, value = 17, min = 0, width = "100%")
        ),
        column(
          4,
          tags$label("Average bet size ($)", style = "font-size: 12px;"),
          numericInput("bet_size", NULL, value = 10, min = 0, step = 1, width = "100%")
        ),
        column(
          4,
          tags$label("Weeks of betting", style = "font-size: 12px;"),
          numericInput("weeks", NULL, value = 4, min = 1, step = 1, width = "100%")
        )
      )
    ),

    # RESULTS ---------------------------------------------------------------
    div(class = "app-big-number", textOutput("headline_cost")),
    div(class = "app-subline", textOutput("headline_context")),

    # IMPACT LINES ----------------------------------------------------------
    div(
      class = "app-impact-block",
      div(class = "app-impact-title", "You pay:"),
      div(class = "app-impact-line", textOutput("impact_per_bet")),
      div(class = "app-impact-line", textOutput("impact_per_week")),
      div(class = "app-impact-line", textOutput("impact_per_year"))
    ),

    # FOOTNOTE --------------------------------------------------------------
    div(
      class = "app-note",
      HTML(
        "This calculator shows estimated out-of-pocket costs under Illinois’ per-wager tax. ",
        "It does not account for winnings or losses, parlays, promotional boosts, or other taxes ",
        "proposed at the local or state&nbsp;level."
      )
    ),

    div(
      class = "app-note",
      "Please bet responsibly. If you or someone you know struggles with gambling, support is available at ",
      tags$span(
        style = "white-space: nowrap;",
        a(
          "Illinois’ Gambling Hotline Page",
          href   = "https://e.helplineil.org/areyoureallywinning/",
          target = "_blank"
        ),
        "."
      )
    ),

    div(
      class = "app-note byline",
      HTML(
        "Developed by <a href='https://github.com/jakec04' target='_blank'>Jake Cox</a> / ",
        "<a href='https://communication.depaul.edu/about/initiatives/center-for-journalism-integrity-and-excellence/Pages/default.aspx' target='_blank'>
         DePaul University Center for Journalism Integrity and Excellence</a> in collaboration with ",
        "<span style='white-space: nowrap;'>
           <a href='https://www.nbcchicago.com' target='_blank'>NBC5&nbsp;Chicago</a>.
         </span>"
      )
    )
  )  # end .app-container
)    # end fluidPage

server <- function(input, output, session) {

  calc <- reactive({
    bets_week <- max(input$bets_per_week %||% 0, 0)
    bet_size  <- max(input$bet_size %||% 0, 0)
    weeks     <- max(input$weeks %||% 1, 1)

    weekly_fee    <- bets_week * TAX_PER_WAGER
    weekly_staked <- bets_week * bet_size

    total_fee    <- weekly_fee * weeks
    total_staked <- weekly_staked * weeks

    yearly_fee   <- weekly_fee * 52
    pct_per_bet  <- if (bet_size > 0) TAX_PER_WAGER / bet_size else NA_real_
    eff_rate     <- if (total_staked > 0) total_fee / total_staked else NA_real_

    list(
      bets_week     = bets_week,
      bet_size      = bet_size,
      weeks         = weeks,
      weekly_fee    = weekly_fee,
      weekly_staked = weekly_staked,
      total_fee     = total_fee,
      total_staked  = total_staked,
      yearly_fee    = yearly_fee,
      pct_per_bet   = pct_per_bet,
      eff_rate      = eff_rate
    )
  })

  # BIG HEADLINE NUMBER -------------------------------------------------------
  output$headline_cost <- renderText({
    c <- calc()
    if (c$bets_week == 0 || c$bet_size == 0) {
      return("Enter your betting habits above.")
    }
    paste0(
      "You’d pay ", dollar(c$total_fee),
      " in per-wager taxes over ", c$weeks, " weeks."
    )
  })

  # CONTEXT -------------------------------------------------------------------
  output$headline_context <- renderText({
    c <- calc()
    if (c$bets_week == 0 || c$bet_size == 0) return("")
    if (is.na(c$eff_rate)) return("")

    paste0(
      "In that period, you would place about ", dollar(c$total_staked),
      " in wagers. The per-wager fee functions as an effective surcharge of about ",
      percent(c$eff_rate, accuracy = 0.1),
      " on the total amount wagered."
    )
  })

  # IMPACT LINES --------------------------------------------------------------
  output$impact_per_bet <- renderText({
    c <- calc()
    if (c$bet_size == 0 || is.na(c$pct_per_bet)) return("")
    paste0(
      "• An effective cost increase of about ",
      percent(c$pct_per_bet, accuracy = 0.1),
      " on each wager"
    )
  })

  output$impact_per_week <- renderText({
    c <- calc()
    paste0(
      "• Around ", dollar(c$weekly_fee),
      " in per-wager taxes each week"
    )
  })

  output$impact_per_year <- renderText({
    c <- calc()
    paste0(
      "• About ", dollar(c$yearly_fee),
      " in per-wager taxes over a full year at similar betting levels"
    )
  })
}

shinyApp(ui, server)
