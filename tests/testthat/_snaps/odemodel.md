# CompiledOdeModel print method shows compact compiled status

    Code
      print(compiled_model)
    Output
      compphysiol_desolve_compiled_model (pending):
       ODE model:
        compphysiol_states: 1
        compphysiol_equations: none
        compphysiol_observables: 1
        Dosing events: none
       compphysiol_parameters:
        A0: required, unit signature pending
        V: default = 1 [L]
        ke: required, unit signature pending

---

    Code
      print(compiled_model)
    Output
      compphysiol_desolve_compiled_model (compiled):
       ODE model:
        compphysiol_states: 1
        compphysiol_equations: none
        compphysiol_observables: 1
        Dosing events: none
       Solver dimensions:
        mass: mg
        length: dm
        time: h
       compphysiol_parameters:
        A0: required, [mg]
        V: default = 1 [L]
        ke: required, [1/h]

# OdeModel print method uses DSL state names

    Code
      print(ode_model)
    Output
      compphysiol_ode_model:
       compphysiol_states:
        (1) a[drug, Central], initial = A0
       ODEs:
        d/dt a[drug, Central] = -ke * a[drug, Central]
       compphysiol_equations:
        (1) C = a[drug, Central]/V
       compphysiol_observables:
        (1) Cobs = a[drug, Central]/V
       compphysiol_dosing:
        (1) add 100 to a[drug, Central] at 0
       compphysiol_parameters:
        (1) V = 10
       Free parameters: A0, ke

