SELECT          s.`name` AS "spot.name"
              , s.`description` AS "spot.description"
              , s.`x` AS "spot.x"
              , s.`y` AS "spot.y"
              , c.`name` AS "crs.name"
--              , c.`epsg` AS "c.epsg"
FROM            `spot` s
        JOIN    `crs` c
        ON      c.`id_crs` = s.`id_crs`;
