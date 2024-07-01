|{{#scoreboard.columns}} {{.}} |{{/scoreboard.columns}}
|{{#scoreboard.columns}} --- |{{/scoreboard.columns}}
{{#scoreboard.rows}}
|{{#values}} {{#isYou}}**{{/isYou}}{{.}}{{#isYou}}**{{/isYou}} |{{/values}}
{{/scoreboard.rows}}

You scored some points for looking at the scoreboard!{{^is_subtask}} You can also do `get scoreboard <coursename>` to see the scoreboard for a specific course.{{/is_subtask}}
