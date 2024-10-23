
simulate_betting_strategy_T <- function(initial_capital = 10000, initial_bet = 20, roulette_type = "american", rounds = 100, simulations = 1000, max_capital = 20000) {
  # Ensure no parameters are negative
  if (initial_capital < 0 || initial_bet < 0 || rounds < 0 || simulations < 0 || max_capital < 0) {
    stop("Parameters should not be negative.")
  }
  
  # Ensure initial capital and initial bet do not exceed max capital
  if (initial_capital > max_capital) {
    stop("Initial capital exceeds max capital.")
  }
  if (initial_bet > max_capital) {
    stop("Initial bet exceeds max capital.")
  }
  
  # Function to simulate a single bet
  single_bet <- function(initial_bet, win_probability) {
    outcome <- sample(c(0, 1), size = 1, prob = c(1 - win_probability, win_probability))
    
    if (outcome == 1) {
      # Win
      return(initial_bet)
    } else {
      # Loss
      return(-initial_bet)
    }
  }
  
  # Function to simulate the Martingale strategy
  martingale_strategy <- function(capital, initial_bet, win_probability, rounds, max_capital) {
    # Initialize variables
    balance <- capital
    bet <- initial_bet
    total_profit <- 0  # Initialize total profit
    
    # Simulate each round
    for (i in 1:rounds) {
      if (balance < bet) {
        break
      }
      
      # Single bet
      sb <- single_bet(bet, win_probability)
      # Place the bet
      balance <- balance + sb
      
      # Update the total profit
      total_profit <- total_profit + sb
      
      if (balance > max_capital) {
        break
      }
      
      # Update the bet size for the next round (double after a loss)
      if (balance >= capital) {
        bet <- initial_bet
      } else {
        bet <- 2 * bet
      }
    }
    return(list(balance = balance, total_profit = total_profit))
  }
  
  # Function to simulate the Doubling strategy
  doubling_strategy <- function(capital, initial_bet, win_probability, rounds, max_capital) {
    # Initialize variables
    balance <- capital
    bet <- initial_bet
    total_profit <- 0  # Initialize total profit
    
    # Simulate each round
    for (i in 1:rounds) {
      if (balance < bet) {
        break
      }
      
      # Single bet
      sb <- single_bet(bet, win_probability)
      # Place the bet
      balance <- balance + sb
      
      # Update the total profit
      total_profit <- total_profit + sb
      
      if (balance > max_capital) {
        break
      }
      
      # Double the bet size for the next round
      bet <- 2 * bet
    }
    
    return(list(balance = balance, total_profit = total_profit))
  }
  
  # Set win probabilities based on the chosen roulette type
  win_probability <- switch(roulette_type,
                            "american" = 18/38,  # Probability of winning in American roulette
                            "european" = 18/37,  # Probability of winning in European roulette
                            stop("Invalid roulette type"))
  
  # Simulate the strategies
  strategies <- c("martingale", "doubling")
  results <- list()
  for (strategy in strategies) {
    balance_after_simulations <- numeric(simulations)
    for (s in 1:simulations) {
      if (strategy == "martingale") {
        result <- martingale_strategy(initial_capital, initial_bet, win_probability, rounds, max_capital)
      } else if (strategy == "doubling") {
        result <- doubling_strategy(initial_capital, initial_bet, win_probability, rounds, max_capital)
      } else {
        cat("Invalid choice of strategy.")
        return(NULL)
      }
      balance_after_simulations[s] <- result$balance
    }
    profit_probability <- sum(balance_after_simulations > initial_capital) / simulations
    total_profit <- mean(balance_after_simulations - initial_capital)
    
    results[[strategy]] <- list(
      total_profit = total_profit,
      profit_probability = profit_probability,
      balances = balance_after_simulations
    )
  }
  best_strategy <- if (results$martingale$total_profit > results$doubling$total_profit) "martingale" else "doubling"
  
  # Display results
  cat("Initial Capital:", initial_capital, "\n")
  for (strategy in strategies) {
    cat("\nStrategy:", strategy)
    cat("\nTotal profit made:", results[[strategy]]$total_profit)
    cat("\nProbability of making a profit:", results[[strategy]]$profit_probability, "\n")
  }
  
  cat("\nThe best strategy is:", best_strategy, "based on the total profit.\n")
  
  # Plot histograms
  par(mfrow = c(2, 1))  # Set layout for two plots
  hist(results$martingale$balances, main = "Distribution of Martingale Balances", xlab = "Balance", ylab = "Frequency", col = "lightblue", border = "black")
  hist(results$doubling$balances, main = "Distribution of Doubling Balances", xlab = "Balance", ylab = "Frequency", col = "lightgreen", border = "black")
}

simulate_betting_strategy_T()
