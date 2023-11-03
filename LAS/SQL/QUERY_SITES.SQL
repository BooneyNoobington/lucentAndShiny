SELECT                  s.`name` AS "site.name"
                      , s.`description` AS "site.description"
                      , s.`site_class` AS "site.site_class"
                      , va.`__house__number` AS "v_address.__house__number"
                      , va.`__house__number_addition` AS "v_address.__house__number_addition"
                      , va.`__street__name` AS "v_address.__street__name"
					  , va.`__city__name` AS "v_address.__city__name"
                      , va.`__district__name` AS "v_address.__district__name"
                      , va.`__district__zip` AS "v_address.__district__zip"
FROM					`site` s
        LEFT JOIN       `v_address` va
        ON		        s.`id_house` = va.`__house__id_house`
