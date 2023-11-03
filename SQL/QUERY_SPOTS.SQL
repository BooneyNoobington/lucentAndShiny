SELECT          s.`identifier` AS "s.identifier"
              , s.`description` AS "s.description"
              , s.`x` AS "s.x"
              , s.`y` AS "s.y"
              , c.`identifier` AS "c.crs"
--              , c.`epsg` AS "c.epsg"
FROM            `spot` s
        JOIN    `crs` c
        ON      c.`id_crs` = s.`id_crs`;
