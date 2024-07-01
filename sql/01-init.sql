-- Create the database tables

CREATE TABLE `team` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `name` varchar(128) NOT NULL,
  `email` varchar(128) NOT NULL,
  `password_hash` text NOT NULL,
  `api_token` binary(16) NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `unique_email` (`email`),
  UNIQUE KEY `unique_api_token` (`api_token`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;

CREATE TABLE `submission` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `created_at` datetime NOT NULL,
  `uuid` binary(16) NOT NULL,
  `team_id` bigint(20) NOT NULL,
  `hash` binary(16) NOT NULL,
  `result_hash` binary(16) DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `unique_uuid` (`uuid`),
  KEY `submission_team_id_fkey` (`team_id`),
  CONSTRAINT `submission_team_id_fkey` FOREIGN KEY (`team_id`) REFERENCES `team` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;

CREATE TABLE `subtask` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `name` varchar(20) CHARACTER SET ascii COLLATE ascii_general_ci NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `unique_subtask` (`name`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;

CREATE TABLE `problem` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `name` varchar(20) CHARACTER SET ascii COLLATE ascii_general_ci NOT NULL,
  `subtask_id` bigint(20) NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `unique_problem_name` (`name`),
  KEY `problem_subtask_id_fkey` (`subtask_id`),
  CONSTRAINT `problem_subtask_id_fkey` FOREIGN KEY (`subtask_id`) REFERENCES `subtask` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;

CREATE TABLE `score` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `submission_id` bigint(20) NOT NULL,
  `problem_id` bigint(20) NOT NULL,
  `score` bigint(20) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `score_submission_id_fkey` (`submission_id`),
  KEY `score_problem_id_fkey` (`problem_id`),
  KEY `score_search` (`problem_id`,`score`),
  CONSTRAINT `score_problem_id_fkey` FOREIGN KEY (`problem_id`) REFERENCES `problem` (`id`),
  CONSTRAINT `score_submission_id_fkey` FOREIGN KEY (`submission_id`) REFERENCES `submission` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;

CREATE TABLE `scoreboard_subtask` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `subtask_id` bigint(20) NOT NULL,
  `team_id` bigint(20) NOT NULL,
  `rank` bigint(20) NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `unique_score_subtask` (`subtask_id`,`team_id`),
  KEY `scoreboard_subtask_team_id_fkey` (`team_id`),
  CONSTRAINT `scoreboard_subtask_subtask_id_fkey` FOREIGN KEY (`subtask_id`) REFERENCES `subtask` (`id`),
  CONSTRAINT `scoreboard_subtask_team_id_fkey` FOREIGN KEY (`team_id`) REFERENCES `team` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;

CREATE TABLE `scoreboard_global` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `team_id` bigint(20) NOT NULL,
  `rank` bigint(20) NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `unique_score_team` (`team_id`),
  CONSTRAINT `scoreboard_global_team_id_fkey` FOREIGN KEY (`team_id`) REFERENCES `team` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;
