This repository contains all server assets for the [ICFP Contest 2024](https://icfpcontest2024.github.io/). If you want to run the contest locally and have not seen the tasks yet, continue reading and and do not look at other files in this repository. If you have seen the task and are curious about things you might have missed, you can also look at the [spoilers](spoilers.md) document.

# Setting up

The below steps assume that you want to run the contest on the local computer. If you want to host the contest with multiple teams, we have some pointers for that in the [multiple teams section](#running-with-multiple-teams).

The server code is written in Haskell, and as the main dependency you will need the [Stack](https://www.haskellstack.org/) build tool installed. Once you have that available on the command line, you can compile the server code with:
```
stack build
```
This command will download the necessary compiler version, all dependencies, and will compile everything. It may take a while, but since we use an LTS resolver we believe that this command should still work in the future. If you are reading this from the future and that is not the case, we're happy to accept PRs to solve this.

Next to compiling, you will need to copy the database template (in sqlite format) and config template (in YAML format):
```
cp sample.local.yaml config.yaml
cp sql/template.sqlite db.sqlite
```

Depending one how much you want the 'real experience', you might want to change one configuration setting, which is `lightningEnd`. This datetime value is supposed to be set to 24 hours after the start of the contest, and in this case has the effect of 'unlocking everything' (which we won't spoil just yet). This also happened during the real contest, but for trying out you can also leave it as is and get a tiny bit of a head start by having everything unlocked already.

# Running the contest

To run the server, you can use:
```
stack exec -- icfpc2024-server config.yaml
```
It should print:
```
Listening on port 8000
```

Now you are ready to rock! Read the task [on the website](https://icfpcontest2024.github.io/task.html), and where it instructs you to send your request to `https://boundvariable.space/communicate` you should instead send your request to `http://localhost:8000/communicate` with the following header:
```
Authorization: Bearer 00000000-0000-0000-0000-000000000000
```

That's all, keep in mind that there is no memory limit set, and that your server does not queue requests, so if you send multiple heavy-to-evaluate expressions you might overload your own pc.

# Running with multiple teams

If you locally want to run with multiple teams, you have two options. The easiest is to use the above, and register some more teams with:

```
curl -X POST --data '{"name":"My awesome team", "email":"example@example.com", "password":"supersecret"}' localhost:8000/register
```

This call should return an API token which can be used as the bearer token for communication. If you also want the scoreboard to work, you should periodically run the following to recompute it:

```
stack exec -- icfpc2024-server config.yaml --recompute-scoreboard
```

The scoreboard can be fetched both as JSON via [http://localhost:8000/scoreboard](http://localhost:8000/scoreboard) as well as via communication.

This approach has a big drawback, which is that evaluation happens all on one machine without any form of queuing. This means that if multiple teams submit things at the same time, their runtimes may influence each other and they could run into time limit errors. Furthermore, there is no rate limiting and the sqlite implementation might not be concurrency safe, so things might be slighly less stable.

## Production setup

This repository also contains all code for the production setup used during the contest, but no full guide on how to use it. You will need:

* [Redis](https://redis.io/)
* [MariaDB](https://mariadb.org/)
* An S3 compatible object storage, for [MinIO](https://min.io/) might work

To compile and run, you will have to add `--stack-yaml stack.production.yaml` to all commands, e.g.:
```
stack --stack-yaml stack.production.yaml build
stack --stack-yaml stack.production.yaml exec -- icfpc2024-server config.yaml --recompute-scoreboard
```

To set up the database you need to execute the migrations from `sql/*.sql`, and of course you will need to use the `sample.production.yaml` file instead and adapt it to your needs. Then you will need to run two or more processes:

* The API server, with `stack exec -- icfpc2024-server config.yaml`, which will listen on port `8000` and will put requests in a Redis queue
* One or more workers, with `stack exec -- icfpc2024-server config.yaml --worker`, which will listen to the queue and handle the jobs. You might want to limit their memory by appending `+RTS -M3G` to this command.

# Tooling

If you want to abuse the fact that you have our code, you could use some commands to locally debug. Some commands:

* `stack exec -- icfpc2024-interpreter encode` will ICFP-encode a string from the stdin
* `stack exec -- icfpc2024-interpreter eval` will parse and evaluate an ICFP from the stdin
* `stack exec -- ttop --print-states board.brd` will run 3D evaluation for `board.brd`. By default it takes `A = 6` and `B = 7`, but you can pass other values as extra argument to the command.

# Contest data

In `sql/data_dump.sqlite` there is a dump of all raw scores of the contest, which should allow you to reconstruct the final scoreboard of the contest. Note that only the relevant submissions have been included, and that some fields have been made nullable to make the dump as small as possible, so not all server functionality might work when using this dump.

Furthermore, in the `solutions` directory we have put the overall best solution for each of the problems of the contest.
