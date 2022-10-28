# Model for the 2022 Senate Midterms

Main contains the functions to determine the mean result, standard deviation, and democrat win chance for every state.

Pollingaverage contains the polling averages created from FiveThirtyEight's polling database and pollster ratings. We augment this polling average by the actual results to find the polling error. For 2018, when there were no presidential elections, we used a heirarchy: if there was a senate election, we used that, otherwise a gubernatorial election, otherwise a mean of the error of house elections in that state.

Expert opinions predicts the closest elections (within 8 points) based on a linreg of previous years' expert opinions.

Simulation first finds a correlation between every two states based on their demographics. Then it creates a random z-score for every state, and then augments that z-score by every other states' z-score and that state's correlation. We use those final z-scores to predict who will win each seat in this iteration of the simulation. We then run this simulation 10,000 times.
