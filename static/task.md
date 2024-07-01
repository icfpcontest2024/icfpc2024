# Task

As many will remember, in 2006 we [discovered](http://www.boundvariable.org/task.shtml) documents of an old society, called the Cult of the Bound Variable. With the valuable help from the ICFP community, the Cult's computing device (Universal Machine) was brought back to life, and much interesting information could be recovered. In recent observations from the [Pegovka observatory](https://icfpcontest2020.github.io/), we discovered something stunning: the Cult of the Bound Variable still exists, and has migrated their civilization to space! It is suspected that [Endo](https://save-endo.cs.uu.nl/) helped them move, after he escaped from earth again.

After a couple of months of research, we have been able to decipher most of the received messages, and set up a communication channel. People of the Cult of the Bound variable now use _Interstellar Communication Functional Programs_ (ICFP) to communicate. Our findings about ICFP expressions can be found [on this page](icfp.html).

## Challenges

While we were successful in deciphering the communication language, we unfortunately faced challenges that we cannot solve ourselves, and we once again ask for your help! The communication channel communicates with the School of the Bound Variable, [a MOOC](https://en.wikipedia.org/wiki/Massive_open_online_course) where students can follow several courses to learn about various skills necessary for life in space. Each of the courses poses a set of tests, which are scored according to some metric.

*An important note*: it seems that basic communication utilizes strings only. Therefore, we advice you to implement more advanced [ICFP expressions](icfp.html) only once you need them, and start with strings.

## Communication channel

For you to try out the communication, we have opened up the communication channel with the Cult for you. By sending a HTTP `POST` request to `https://boundvariable.space/communicate`, with the ICFP in the body, your request is sent into the galaxy and the response is returned to you. For identification purposes, you must send the `Authorization` header that you can find on [your team page](team.html).

We strongly advise that you make the `POST` request directly from your favorite programming language, but we also provide [web based communication](communicate.html). That page also shows your communication history.

Furthermore, we found out that sending `S'%4}).$%8` is a great entrypoint for communicating with the school of the Cult of the Bound Variable.

## Limits

Communicating with space is an energy consuming process, so for environmental and monetary reasons, we have put limits on the messages. The `POST` body of your request must not exceed 1 MB (`1048576` bytes), and you may send at most `20` messages per minute.

## Scoring

The tests for each course are scored according to some criterion explained to you when you enter the course. Your best score for each test is stored. On the [scoreboard page](scoreboard.html) we've rendered an overview of the ranks of all teams on each course as well as a global rank. To find the ranks for a course given all the scores of the individual tests of that course, and to find the global rank based on the ranks for the courses, we use the so called [Borda count](https://en.wikipedia.org/wiki/Borda_count).

In technical terms, the ranklist is an election, where teams are the candidates and the tests are the voters. The better you do for an individual tests, the higher this test will rank you. A more intuitive explanation is that the amount of points you score for each test, is the amount of teams scoring strictly worse than you on that test, and then the ranklist for that course is based on the sum of those points. This method is first used to compute a ranklist per course, and then again using the ranks for the individual courses to find the global rank.

While this may sound abstract, we believe this is all you need to know. And important property of this rating system is that absolute scores don't matter, only the order. It also deals naturally with ties (some tests are just correct/incorrect, there everyone solving it is tied at 1st place), and automatically balances tests with different absolute score ranges. And of course you should just try get the best scores on all tests!

## Final code submit

To be considered for prizes, please submit your code near the end of the contest via your [team page](team.html). The code submission closes 3 hours after the end of the contest. It is not necessary to make a separate submission for the lightning round, but please include a README file indicating which parts are from the first 24 hours.

## Meta note

While the introduction refers to various editions of the ICFP programming contest, we want to make explicit that the tasks set for this year are completely new, and knowledge of the earlier challenges is not necessary. However, we referred to them because we enjoyed those contests, so once this year's contest is over we advice everyone to give them a go in case you haven't yet!

To make the contest fair for all timezones, we do not intend to make any changes to the task during the contest. However, we might publish some extra information at the end of the lightning round.

And as last request to contestants: enjoy the contest, but also make sure others can enjoy it! Please do not make attempts to break our server, it should be quite robust, but remember that the [ICFPC organizers](contact.html) are volunteers that organize this contest in their free time. And so far we tremendously enjoyed ourselves in the preparations, so we hope to share as much as possible of that with you!
