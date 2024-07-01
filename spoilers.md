This document contains some pointers on intended solutions and how to unlock things.

# Hello

This is a 'dummy' subtask to teach contestants how to make `get` commands. There are 4 points to score:

* `hello1`: Doing `get index`, which can be copy & pasted from the task page.
* `hello2`: Doing `get scoreboard`, for which you need to implement string encoding & decoding.
* `hello3`: For using the `echo` service, e.g. `echo x`. This teaches you how general interactions work, but also allows contestants to check the server implementation (which was (ab)used a lot during the contest!).
* `hello4`: Doing what `get language_test` instructs you, which is sending `solve language_test 4w3s0m3`. For this you need a quite complete evaluator.

When solving any of `hello2`, `hello3` or `hello4`, you unlock `lambdaman` and `spaceship`.

# Lambdaman

The first real subtask, which might seem about path finding at first, but is really more about compression and smart state space exploration. The first few levels are simple string encodings and can be solved by hand, but then `lambdaman6` is a small expression generating an `L` followed by `199` pills. This is meant to teach you that expressions can be much smaller than the corresponding string, and you should do the same for your solutions, as the score is not based on the number of moves, but on the size of the expression.

Another important property of this problem is that lambdaman does not need to follow the given path precisely, but can run into the wall. This allows for random-walk like solutions which are much longer than the final path, and run into the wall all the time, but finally eat all pills.

Finally, in `lambdaman21` we hid a small easter-egg. The expression converts a big number to a level, but the number is actually built up as `[...very big number...] + 40255974450631082918621388`. However, when you interpret that number as a string, it spells `[3d-backdoor]`. Indeed, when you do `get 3d-backdoor` you are enrolled in the backdoor course.

# Spaceship

This is the most 'traditional' subtask, as it involves very little of the ICFP language. It is (almost) completely string encoded, and is a more standard type of optimization problem. Not many tricks here, for this subtask we do count the number of moves. The levels are designed in some tricky ways though, we have a couple that just list a subset of points visited during a random walk (so they have a quite small solution), but then many of them are variations on this where we added some jitter so that the seemingly likely path just does not work out, and it might be better to visit things in a different order. In the [config file](static/spaceship/config.yaml) there is a small description of each case.

A few weeks before ICFPC there was [CodeWeekend #1](https://codeweekend.dev/) organised by RGBTeam, which we enjoyed, so we decided to 'steal' some of their testdata and based `spaceship20` and `spaceship21` on it.

Finally, in `spaceship22`, which looks like a big lambda if you plot it, we hid another backdoor. In the beginning of the expression there is a small piece of string being thrown away, which spells `[/etc/passwd]`. If you do `get /etc/passwd` you see the username and md5 hash of a password of the headmaster, and that can get you into the `efficiency` course.

# 3D

Solving at least 5 problems for both the `lambdaman` and `spaceship` problems allow you to do `get 3d`. This is the subtasks with the longest description, and also the one with the most server CPU cycles spent. We enjoyed the [2d language](https://github.com/minoki/icfpc2006/blob/master/2d/raytrace.2d) of [ICFPC 2006](http://www.boundvariable.org/) so much that we wanted to come up with 3D, without it being too similar to 2D. After some failed tries in trying to write programs in 3-dimensional space, we found that the 2-dimensional programs are the sweet spot, but together with the space theme we realized that time could be the 3rd dimension.

This problem does not contain any other easter eggs, we found it complicated enough already. This problem cost us the most work in verifying that the problems we came up with were actually solvable, especially `3d11` and `3d12`. We are impressed how many teams solved it in just 72 hours.

Solving at least 5 of the `3d` problems finally unlocks `efficiency`. 

# Efficiency

The efficiency subtask is heavily inspired by the [Internet Problem Solving Contest](https://ipsc.ksp.sk/2013/real/problems/c.html). We also wanted to find the limits of what we could do with the ICFP language, and think that reverse engineering our expressions should be interesting. We made sure that `efficiency1` is still evaluatable, but from then on things get much heavier and actually evaluating is completely infeasible. The [config file](static/efficiency/config.yaml) gives a short description of every expression and the corresponding answer.
