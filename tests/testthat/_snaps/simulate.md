# SimulationResult can be printed

    Code
      print(out)
    Output
       SimulationResult:
        time: 0 to 10 (11 points)
        states: 1 (a_drug_Central)
        observables: 0

# SimulationResult print truncates long state and observable lists

    Code
      print(out)
    Output
       SimulationResult:
        time: 0 to 0 (1 points)
        states: 3 (very_long_state_..., ... +2 more)
        observables: 2 (very_long_o..., ... +1 more)

# simulate returns observable trajectories

    Code
      print(out)
    Output
       SimulationResult:
        time: 0 to 10 (11 points)
        states: 1 (a_drug_Central)
        observables: 1 (C)

