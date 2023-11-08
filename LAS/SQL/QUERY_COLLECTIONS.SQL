SELECT                      c.`id_collection` AS "collection.id_collection"
                          , c.`name` AS "collection.name"
						  , m.`name` AS "matrix.name"
FROM						`collection` c
			LEFT JOIN		`matrix` m
			ON				m.`id_matrix` = c.`id_matrix`
