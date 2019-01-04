--4rth problem
compute_res a b d = ((a*d-b),(b*d))
compute_sign a b d  
                   | (((a*d-b)/(b*d)) > 0) = 1
                   | (((a*d-b)/(b*d)) < 0) = 2
                   | (((a*d-b)/(b*d)) == 0) = 0


is_final (a,b) = (mod b a == 0)

fractions a b = compute (a,b) 1 0

compute (a,b) 0 flag = []
compute (a,b) c flag | (compute_sign a b c)==1 = c:(compute (compute_res a b c) (c+1) 1)
                     | ((compute_sign a b c)==2 && flag==0) = compute (a,b) (c+1) 0
                     | ((compute_sign a b c)==2 && flag==1) = compute (a,b) (c+1) 1
                     | ((compute_sign a b c)==0 && flag == 1) = c:compute (a,b) 0 1
                     | ((compute_sign a b c)==0 && flag == 0) = compute (a,b) (c+1) 1
           



--2nd problem
my3sum x y z = x+y+z

my3mult x y z = x*y*z

my_zip1 num [] = []
my_zip1 num (n:t) = ((num,n):my_zip1 num t)

my_zip2 [] l = []
my_zip2 (n:t) l = ((my_zip1 n l)++my_zip2 t l)

my_zip3 (n1,n2) [] = []
my_zip3 (n1,n2) (n3:t) = (n1,n2,n3):my_zip3 (n1,n2) t

my_zip4 [] ll = []
my_zip4 (n:l) ll = (my_zip3 n ll)++(my_zip4 l ll) 

my_zip a b c = my_zip4 (my_zip2 [1..a] [1..b]) [1..c]

sssum f a b c = find_sum f (my_zip a b c)

find_sum f [] = 0
find_sum f ((n1,n2,n3):t) = (f n1 n2 n3)+(find_sum f t)  

--1st problem
check :: Int->Int->Int
check num1 num2 | mod num1 num2 == 0 = check (div num1 num2) num2
                | otherwise = num1

check_all :: Int->Bool
check_all num = check (check (check num 2) 3) 5 == 1

multiples = [n|n<-[1..],check_all n]
