main = do
  let result = head $ [a*b*c | a<-[1..1000], b<-[1..1000], c<-[1..1000], a+b+c == 1000, a<=b, b<=c, (a*a)+(b*b)==(c*c) ]
  putStrLn $ show $ result
