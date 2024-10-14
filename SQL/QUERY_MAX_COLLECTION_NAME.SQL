SELECT                  `name`
FROM                    `collection`
WHERE                   `id_collection` = (
                            SELECT MAX(`id_collection`) FROM `collection`
                        )
