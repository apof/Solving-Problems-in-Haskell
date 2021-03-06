import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List

--naive approach - sorting
find_elem_index elem [] index pos sol_index = sol_index
find_elem_index elem (el:list) index pos sol_index
                                    | (elem == el && index <= pos) = find_elem_index elem list (index+1) pos index
                                    | (elem /= el && index <= pos) = find_elem_index elem list (index+1) pos sol_index
                                    | index > pos                  = find_elem_index elem [] (index+1) pos sol_index

find_next_max [] max index pos = max
find_next_max (elem:list) max index pos
                                       |(elem > max && pos <= index) = find_next_max list elem index (pos+1)
                                       |(elem < max && pos <= index) = find_next_max list max index (pos+1)
                                       |pos > index                  = find_next_max [] max index (pos+1)

my_split myList length = splitAt length myList
 
make_flip2 (l1,l2) = (reverse(l1)++l2)
make_flip1 (l1,l2) pos = make_flip2 splitted_list 
                         where
                         splitted_list = my_split (reverse(l1)++l2) pos

make_flip list max index pos
                                |index_to_split == index = (-1,-1,list)
                                |otherwise               = (index_to_split,index,make_flip1 (my_split list index_to_split) index)
                                where
                                index_to_split = find_elem_index (find_next_max list max index pos) list pos index 1


find_naive_solution list 1 = []
find_naive_solution list bottom = (res1,res2,nextList):(find_naive_solution nextList (bottom-1))
                                  where
                                  (res1,res2,nextList) = make_flip list 0 bottom 1

create_final_solution [] = []
create_final_solution ((x,y,list):tail) 
                                      | x==1 = [y]:create_final_solution tail
                                      | x==(-1) = create_final_solution tail
                                      | otherwise = [x,y]:create_final_solution tail

flatten_sol [] = []
flatten_sol (l:tail) = l++(flatten_sol tail)

vis list [] = []
vis list (elem:tail) = (new_list:(vis new_list tail))
                       where
                       new_list = make_flip2(my_split list elem)

visualize list moves = list:(vis list moves)

naive list = flatten_sol(create_final_solution(find_naive_solution list (length list)))

--quicksort

quicksort [] = []
quicksort (x:xs) = quicksort small ++ (x : quicksort large)
   where small = [y | y <- xs, y <= x]
         large = [y | y <- xs, y > x]

--bfs
get_successors l 1 = []
get_successors l length = (length,flip):(get_successors l (length-1))
                        where
                        flip = make_flip2(my_split l length)

is_goal_state list = is_goal (-1000) list
is_goal el [] = True
is_goal el (elem:tail)
                     | el <= elem = is_goal elem tail
                     | el > elem = False


is_member el [] = False
is_member el (elem:tail)
                      | (el==elem) = True
                      | otherwise = is_member el tail 

deQueue (peek:tail) = (peek,tail)

construct_solution state meta_map
                             | (length st == 0) = []
                             | otherwise = action:(construct_solution st meta_map)
                             where
                             (st,action) = Map.findWithDefault ([],-1) state meta_map

bfs init_list = bfs_recursive [init_list] Set.empty (Map.insert init_list ([],-1)(Map.empty))

bfs_recursive open_set closed_set meta_map
  | (is_goal_state subtree_root == True) = reverse(construct_solution subtree_root meta_map)
  | (List.null open_set == True)         = []
  | otherwise                            = bfs_recursive new_open new_close new_map  
  where
  (subtree_root,open_without_peek) = deQueue open_set
  (new_open,new_close,new_map) = expand_with_successors subtree_root (get_successors subtree_root (length subtree_root)) open_without_peek closed_set meta_map

expand_with_successors subtree_root [] open_set closed_set meta_map = (open_set,(Set.insert subtree_root closed_set),meta_map)
expand_with_successors subtree_root ((action,move):tail) open_set closed_set meta_map
  | (in_close==True || in_open==True) = expand_with_successors subtree_root tail open_set closed_set meta_map
  | otherwise                         = expand_with_successors subtree_root tail (open_set++[move]) closed_set new_meta_map
  where
  in_close = Set.member move closed_set
  in_open = is_member move open_set
  new_meta_map = Map.insert move (subtree_root,action) meta_map


--bidirectional_search

bidirectional init_list = bdrs_recursive [init_list] [end_list] Set.empty Set.empty (Map.insert init_list ([],-1)(Map.empty)) (Map.insert end_list ([],-1)(Map.empty)) init_list
  where
  end_list = quicksort init_list

bdrs_recursive open_set_I open_set_G closed_set_I closed_set_G meta_map_I meta_map_G init_list
  |(List.null open_set_I == True && List.null open_set_G == True)  = []
  |(List.null open_set_I == False && List.null open_set_G == True) = check_I open_set_I open_set_G closed_set_I closed_set_G meta_map_I meta_map_G init_list
  |(List.null open_set_I == True && List.null open_set_G == False) = check_G open_set_I open_set_G closed_set_I closed_set_G meta_map_I meta_map_G init_list
  |(List.null open_set_I == False && List.null open_set_G == False) = check_IG open_set_I open_set_G closed_set_I closed_set_G meta_map_I meta_map_G init_list
  

check_I open_set_I open_set_G closed_set_I closed_set_G meta_map_I meta_map_G init_list
  |(is_goal_state subtree_root_I == True ) = reverse(construct_solution subtree_root_I meta_map_I) 
  |((is_member subtree_root_I open_set_G) == True) = reverse(construct_solution subtree_root_I meta_map_I) ++ construct_solution subtree_root_I meta_map_G
  |otherwise = bdrs_recursive new_open_I open_set_G new_close_I closed_set_G new_map_I meta_map_G init_list
   where 
   (subtree_root_I,open_without_peek_I) = deQueue open_set_I
   (new_open_I,new_close_I,new_map_I) = expand_with_successors subtree_root_I (get_successors subtree_root_I (length subtree_root_I)) open_without_peek_I closed_set_I meta_map_I

check_G open_set_I open_set_G closed_set_I closed_set_G meta_map_I meta_map_G init_list
  |(subtree_root_G == init_list) = construct_solution subtree_root_G meta_map_G 
  |((is_member subtree_root_G open_set_I) == True) = reverse(construct_solution subtree_root_G meta_map_I) ++ construct_solution subtree_root_G meta_map_G
  |otherwise = bdrs_recursive open_set_I new_open_G closed_set_I new_close_G meta_map_I new_map_G init_list
   where 
   (subtree_root_G,open_without_peek_G) = deQueue open_set_G
   (new_open_G,new_close_G,new_map_G) = expand_with_successors subtree_root_G (get_successors subtree_root_G (length subtree_root_G)) open_without_peek_G closed_set_G meta_map_G

check_IG open_set_I open_set_G closed_set_I closed_set_G meta_map_I meta_map_G init_list
  |(is_goal_state subtree_root_I == True) = reverse(construct_solution subtree_root_I meta_map_I) 
  |((is_member subtree_root_I open_set_G) == True) = reverse(construct_solution subtree_root_I meta_map_I) ++ construct_solution subtree_root_I meta_map_G
  |(subtree_root_G == init_list) = construct_solution subtree_root_G meta_map_G 
  |((is_member subtree_root_G open_set_I) == True) = reverse(construct_solution subtree_root_G meta_map_I) ++ construct_solution subtree_root_G meta_map_G
  |otherwise = bdrs_recursive new_open_I new_open_G new_close_I new_close_G new_map_I new_map_G init_list 
  where
  (subtree_root_G,open_without_peek_G) = deQueue open_set_G
  (subtree_root_I,open_without_peek_I) = deQueue open_set_I
  (new_open_G,new_close_G,new_map_G) = expand_with_successors subtree_root_G (get_successors subtree_root_G (length subtree_root_G)) open_without_peek_G closed_set_G meta_map_G
  (new_open_I,new_close_I,new_map_I) = expand_with_successors subtree_root_I (get_successors subtree_root_I (length subtree_root_I)) open_without_peek_I closed_set_I meta_map_I