# OdeModel print method uses DSL state names

    Code
      print(ode_model)
    Output
      OdeModel:
       States:
        (1) a[drug, Central], initial = A0
       ODEs:
        d/dt a[drug, Central] = -ke * a[drug, Central]
       Equations:
        (1) C = a[drug, Central]/V
       Observables:
        (1) Cobs = a[drug, Central]/V
       Dosing:
        (1) add 100 to a[drug, Central] at 0
       Parameters:
        (1) V = 10
       Free parameters: A0, ke

