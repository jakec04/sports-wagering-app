# Illinois Per-Wager Sports Betting Tax Calculator

This repository contains a Shiny web application that illustrates how Illinois’ new per-wager online sports betting tax affects individual bettors. Users can enter their own betting habits to see how the tax accumulates per bet, per week and over longer periods of time.

The app was developed alongside reporting from **DePaul University’s Center for Journalism Integrity and Excellence (CJIE)** on the evolution of Illinois’ sports wagering laws, including the state’s new online per-wager tax enacted in 2024.

You can view and interact with the live app here:  
**https://mq13df-jake0cox.shinyapps.io/Sports_wagering/**

---

## 📊 About the Application

Illinois is the first state in the U.S. to implement a *per-wager* tax on online sports bets.  
Under the law:

- Sportsbooks are charged **$0.25 per wager** for the first 20 million annual online wagers  
- After that threshold, the cost increases to **$0.50 per wager**  
- Many operators pass the fee directly to bettors through increased minimum wagers or per-bet charges

This application demonstrates, in simple terms, how that per-wager fee adds up for an individual bettor based on:

- Number of bets placed per week  
- Average bet size  
- Time period of betting activity  

Default values reflect median betting habits reported in a **2024 Stanford study** on online sports wagering behavior.

---

## 🧮 Features

- Clean, accessible UI using **Shiny** and **bslib**
- Neutral, explanatory interactive copy appropriate for journalism projects
- “You pay…” summary box showing:
  - Effective cost increase per bet
  - Weekly added tax costs
  - Annualized totals based on the user’s betting levels
- Mobile-responsive design where supported
- Embedded directly into longform reporting or Quarto documents

---

## 📁 Repository Structure
