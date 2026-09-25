# CompiledOdeModel print method shows compact compiled status

    Code
      print(compiled_model)
    Output
      CompiledOdeModel (pending):
       ODE model:
        States: 1
        Equations: none
        Observables: 1
        Dosing events: none
       Parameters:
        A0: required, unit signature pending
        V: default = 1 [L]
        ke: required, unit signature pending

---

    Code
      print(compiled_model)
    Output
      CompiledOdeModel (compiled):
       ODE model:
        States: 1
        Equations: none
        Observables: 1
        Dosing events: none
       Solver dimensions:
        mass: mg
        length: dm
        time: h
       Parameters:
        A0: required, [mg]
        V: default = 1 [L]
        ke: required, [1/h]

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

