Welcome to the efficiency course!

In this course, your ICFP knowledge will be put to the test. To communicate with the rest of the solar system, the Cult uses their Macroware Insight evaluator. However, the School of the Bound Variable also possesses a quantum computer, on which more complex expressions can be evaluated. As the energy consumption of this quantum computer is getting out of hand, students must learn to find the evaluation result of such complex expressions with other means.

# Tip of the day

Did you know that the binary call-by-name application operator `$` has two siblings? The binary operator `~` (lazy application) is a call-by-need variant on the `$` operator, and the binary operator `!` (strict application) is the call-by-value variant. Smart usage of these can help you save many beta reductions!

# Problems

The following assignments are available:

{{#problems}}
* [{{name}}] {{#your_score}}You solved it.{{/your_score}}{{^your_score}}{{#best_score}}At least one other team solved it.{{/best_score}}{{/your_score}}
{{/problems}}

Each of such ICFPs will eventually evaluate to an integer, which is the answer to the given assignment. However, students may need more than just an efficient evaluator in order to get the right answer (and likely don't possess a quantum computer). To submit an answer to a test, send:

```
solve efficiencyX answer
```

e.g. if the answer to the second assignment is `42`, then send `solve efficiency2 42`.
