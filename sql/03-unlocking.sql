CREATE TABLE `subtask_unlocked` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `team_id` bigint(20) NOT NULL,
  `subtask_id` bigint(20) NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `unique_subtask_unlocked` (`team_id`,`subtask_id`),
  KEY `subtask_unlocked_subtask_id_fkey` (`subtask_id`),
  CONSTRAINT `subtask_unlocked_subtask_id_fkey` FOREIGN KEY (`subtask_id`) REFERENCES `subtask` (`id`),
  CONSTRAINT `subtask_unlocked_team_id_fkey` FOREIGN KEY (`team_id`) REFERENCES `team` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;
