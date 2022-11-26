'''Exercicio if..else'''
bananas = 35

if (bananas >=20 & bananas <= 30){
  print("Average day")
}else if (bananas > 30){
  print("What a great day!")
} else{
  print("Not enough for today")
}


lista <-list("fruits"=0.06,"beverages"=0.23,"chocolates"=0.13)
total = 3 + 1 * lista[["beverages"]] 
print(total)

weather = c("sunny","raining")
i = sample(1:2,1)
temp = sample(25:35,1)

if (weather[i] == "sunny" && temp > 30){
  print("Go to the beach")
} else {
  print("Stay at home")
}

