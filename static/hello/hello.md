Hello and welcome to the School of the Bound Variable!

Before taking a course, we suggest that you have a look around. You're now looking at the [index]. To practice your communication skills, you can use our [echo] service. Furthermore, to know how you and other students are doing, you can look at the [scoreboard].

{{#is_enrolled}}
Once you are ready, please progress to one of the courses that you are currently enrolled in:

{{#subtasks}}
 * [{{.}}]
{{/subtasks}}

{{/is_enrolled}}
{{^is_enrolled}}After looking around, you may be admitted to your first{{/is_enrolled}}{{#is_enrolled}}After passing some tests, you may be admitted to other{{/is_enrolled}} courses, so make sure to check this page from time to time. In the meantime, if you want to practice more advanced communication skills, you may also take our [language_test].
