--naive approach - sorting
find_elem_index elem [] index = -1
find_elem_index elem (el:list) index
                                    | elem == el = index
                                    | otherwise = find_elem_index elem list (index+1)

find_next_max [] max index pos = max
find_next_max (elem:list) max index pos
                                       |(elem > max && pos <= index) = find_next_max list elem index (pos+1)
                                       |(elem < max && pos <= index) = find_next_max list max index (pos+1)
                                       |pos > index                  = find_next_max [] max index (pos+1)

split myList length = splitAt length myList
 
make_flip2 (l1,l2) = (reverse(l1)++l2)
make_flip1 (l1,l2) pos = make_flip2 splitted_list 
                         where
                         splitted_list = split (reverse(l1)++l2) pos

make_flip list max index pos
                                |index_to_split == index = (-1,-1,list)
                                |otherwise               = (index_to_split,index,make_flip1 (split list index_to_split) index)
                                where
                                index_to_split = find_elem_index (find_next_max list max index pos) list 1


find_naive_solution list 1 = []
find_naive_solution list bottom = (res1,res2,nextList):(find_naive_solution nextList (bottom-1))
                                  where
                                  (res1,res2,nextList) = make_flip list 0 bottom 1

naive list = find_naive_solution list (length list)