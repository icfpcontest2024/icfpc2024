
CREATE TABLE `score_best` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `team_id` bigint(20) NOT NULL,
  `problem_id` bigint(20) NOT NULL,
  `score` bigint(20) NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `unique_score_best_team_problem` (`team_id`,`problem_id`),
  KEY `score_best_problem_id_fkey` (`problem_id`),
  CONSTRAINT `score_best_problem_id_fkey` FOREIGN KEY (`problem_id`) REFERENCES `problem` (`id`),
  CONSTRAINT `score_best_team_id_fkey` FOREIGN KEY (`team_id`) REFERENCES `team` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;


delimiter |

CREATE TRIGGER best_score AFTER INSERT ON score
  FOR EACH ROW
  BEGIN
    INSERT INTO score_best (team_id, problem_id, score)
       VALUES ( (select team_id FROM submission WHERE submission.ID = NEW.submission_id), NEW.problem_id, NEW.score )
       ON DUPLICATE KEY UPDATE score=LEAST(score,NEW.score);
  END;
|

delimiter ;
