CREATE TABLE `code_submission` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `team_id` bigint(20) NOT NULL,
  `submitted_at` datetime NOT NULL,
  `url` text NOT NULL,
  `languages` text NOT NULL,
  `jury_prize` text NOT NULL,
  PRIMARY KEY (`id`),
  KEY `code_submission_team_id_fkey` (`team_id`),
  CONSTRAINT `code_submission_team_id_fkey` FOREIGN KEY (`team_id`) REFERENCES `team` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;
