SELECT                      *

FROM                        `person` p
                LEFT JOIN  `address` a
                ON          a.`id_address` = p.`id_address`
                LEFT JOIN  `street` s
                ON          s.`id_street` = a.`id_street`
                LEFT JOIN  `district` d
                ON          d.`id_district` = s.`id_district`
                LEFT JOIN  `city` c
                ON          c.`id_city` = d.`id_city`
