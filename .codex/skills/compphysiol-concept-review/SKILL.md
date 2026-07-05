---
name: compphysiol-concept-review
description: Use when working in the compphysiol R package and the user proposes, designs, or asks to implement a new modelling feature, domain concept, API component, model transformation, DSL behavior, ODE/export behavior, lumping/merging behavior, or terminology-sensitive change. This skill guides Codex to read the package glossary, identify real conceptual gaps, ask focused operationalization questions only when needed, and suggest glossary updates when the change introduces a new concept or changes an existing concept.
---

# compphysiol Concept Review

Use this skill as a conceptual review gate before implementing terminology-sensitive changes in `compphysiol`.
Its purpose is to make vague modelling ideas operational without creating unnecessary friction.

## Source Of Truth

Read `vignettes/glossary.qmd` from the repository root before questioning or implementing the feature.
Treat the glossary as the maintained conceptual source of truth for package vocabulary.

If the file is missing, say so briefly and continue by inspecting nearby implementation files and existing docs.
Do not copy glossary definitions into this skill.

## Review Workflow

1. Identify the concept or feature the user wants.
2. Map it to the closest glossary concepts, such as compartment, molecule, state, transport, flow, reaction, equation, observable, parameter, dosing, source/sink, wildcard, wiring, ODE export, lumping, or analytical representation.
3. Decide whether the requested behavior is already conceptually clear enough to implement.
4. Ask questions only for real conceptual gaps that affect implementation, public API, model semantics, tests, or documentation.
5. If the request introduces a new concept or changes the meaning of an existing one, mention that the glossary should be updated after the conceptual questions are resolved.
6. Once the concept is clear, proceed with implementation using the repository's normal coding workflow.

## When To Ask Questions

Ask a focused question when the answer changes code structure or behavior.
Prefer one to three questions at a time.

Good reasons to ask:

- The user uses a term that conflicts with or overlaps a glossary term.
- It is unclear whether something is a compartment, molecule, state, transport, reaction, equation, observable, parameter, dose, or model transformation.
- It is unclear whether a process preserves molecular identity or changes molecular identity or stoichiometry.
- It is unclear whether a concept should exist before wiring, during wiring, during ODE export, or only after simulation.
- It is unclear how wildcards, source/sink compartments, units, state references, or molecule/compartment separation should behave.
- It is unclear whether behavior should be public API, an internal helper, or documentation only.
- The feature might require new terminology in the glossary.

Avoid asking questions when:

- The glossary and existing code make the intended semantics clear.
- The missing detail is ordinary implementation choice that can be inferred from local patterns.
- The question is merely academic and would not change the implementation.

## Question Style

Make questions operational and concrete.
Tie each question to a modelling consequence.

Prefer:

```text
When you say "flow", should this preserve molecule identity like a transport, or can it transform molecules like a reaction? This affects whether the implementation belongs in `Transports` or `Reactions`.
```

Avoid:

```text
Can you clarify what you mean by flow?
```

## Glossary Update Rule

If the feature introduces a concept not covered by `vignettes/glossary.qmd`, or changes the meaning of an existing concept, say so.
Propose updating the glossary after the conceptual questions are answered.

Do not edit the glossary unless the user asks for implementation or documentation changes in the current turn.
When editing it, keep the style conceptual rather than roxygen-like.

## Output Shape Before Coding

For unclear requests, respond with:

- A short mapping from the user's idea to existing glossary concepts.
- The minimal set of blocking conceptual questions.
- A note about whether a glossary update seems likely.

For clear requests, do not manufacture questions.
Briefly state the glossary mapping and proceed.
