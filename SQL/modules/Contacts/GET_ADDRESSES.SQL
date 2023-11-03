SELECT                  *
FROM                    `address` a
                JOIN    `j_person_address` j1
                ON      j1.`id_address` = a.`id_address`
/* Make a selection on which addresses to display
according to the person in querstion.*/
WHERE                   j1.`id_person` = selected_id
