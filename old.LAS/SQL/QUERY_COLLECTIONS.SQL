SELECT                      c.`id_collection` AS "collection.id_collection"
                          , c.`name` AS "collection.name"
						  , o.`name` AS "object.name"
						  , m.`name` AS "matrix.name"
FROM						`collection` c
			LEFT JOIN		`object` o
			ON				o.`id_object` = c.`id_object`
			LEFT JOIN		`matrix` m
			ON				m.`id_matrix` = c.`id_matrix`
