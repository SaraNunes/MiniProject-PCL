module Types

type CupDrink = 
    |Americano
    |Espresso
    |BlackCoffee

type CanDrink =
    |Monster
    |Cola
    |RedBull

type BottleDrink =
    |Water
    |Yogurt
    |OrganicSoftDrink

type DrinkSize =
    |Large
    |Medium
    |Small

type Drink =
    |Cup of (CupDrink * DrinkSize)
    |Can of (CanDrink * DrinkSize)
    |Bottle of (BottleDrink * DrinkSize)

type StoreItem = (Drink * int option)

// the price is dependent on the Size of the drink and on the type

let prices = [
    StoreItem(Cup(Americano, Large), Some 25); 
    StoreItem(Cup(Americano, Medium), Some 15); 
    StoreItem(Cup(Americano, Small), Some 10); 
    StoreItem(Cup(Espresso, Large), Some 25); 
    StoreItem(Cup(Espresso, Medium), Some 15); 
    StoreItem(Cup(Espresso, Small), Some 10); 
    StoreItem(Cup(BlackCoffee, Large), Some 25); 
    StoreItem(Cup(BlackCoffee, Medium), Some 15); 
    StoreItem(Cup(BlackCoffee, Small), Some 10); 
    StoreItem(Can(Monster, Large), Some 25); 
    StoreItem(Can(Monster, Medium), Some 15); 
    StoreItem(Can(Monster, Small), Some 10); 
    StoreItem(Can(Cola, Large), Some 25);  
    StoreItem(Can(Cola, Medium), Some 15); 
    StoreItem(Can(Cola, Small), Some 10); 
    StoreItem(Can(RedBull, Large), Some 25); 
    StoreItem(Can(RedBull, Medium), Some 15); 
    StoreItem(Can(RedBull, Small), Some 10); 
    StoreItem(Bottle(Water, Large), Some 25); 
    StoreItem(Bottle(Water, Medium), Some 15); 
    StoreItem(Bottle(Water, Small), Some 10); 
    StoreItem(Bottle(Yogurt, Large), Some 25); 
    StoreItem(Bottle(Yogurt, Medium), Some 15); 
    StoreItem(Bottle(Yogurt, Small), Some 10); 
    StoreItem(Bottle(OrganicSoftDrink, Large), Some 25); 
    StoreItem(Bottle(OrganicSoftDrink, Medium), Some 15); 
    StoreItem(Bottle(OrganicSoftDrink, Small), Some 10); 
]

let GetPrice (d:Drink) =
    let rec lookForPrice (ls: StoreItem list) =
        match ls with
            |[] -> None
            |head::tail -> 
                match head with
                |(x1, x2) when x1 = d  -> x2
                |_ -> lookForPrice tail
    lookForPrice prices
    
let CupDrinkName cup =
    match cup with
    |Americano -> "Americano"
    |Espresso -> "Espresso"
    |BlackCoffee -> "Black coffee"

let CanDrinkName can =
    match can with
    |Monster -> "Monster"
    |Cola -> "Cola"
    |RedBull -> "Redbull"

let BottleDrinkName bottle =
    match bottle with
    |Water -> "Water"
    |Yogurt -> "Yogurt"
    |OrganicSoftDrink -> "Organic soft drink"

type CanteenMessage =
    |OrderDrink of (Drink * int)
    |LeaveAComment of string

let NameOfDrink drink =
    match drink with
    |Cup((x1,x2)) -> CupDrinkName x1 
    |Can((x1,x2)) -> CanDrinkName x1
    |Bottle((x1,x2)) -> BottleDrinkName x1

type Agent<'T> = MailboxProcessor<'T>
let actor =
    let handleOrdering (drink:Drink, amount:int) =
        let TotalPrice drink = 
            match (GetPrice drink) with
            |Some(x) -> Some(x * amount)
            |None -> None
        let PriceOrNan x =
            match x with
            |Some(x) -> x.ToString()
            |None -> "<Not defined>"
        printfn "Pay %s for your %d %s" (PriceOrNan (TotalPrice drink)) amount (NameOfDrink drink)
    let handleComment (text:string) =
        printfn "handling comment"
        match text.ToUpper() with
        |x when x.Contains("VERY GOOD") -> printfn "We thank you for your kind words" 
        |x when x.Contains("AMAZING") -> printfn "You are amazing for shopping at our place!"
        |x when x.Contains("TERRIBLE") -> printfn "You are terrible don't ever come back here!"
        |x -> printfn "We will consider your comment"
        |_ -> printfn "don't mistreat our system"

    Agent<CanteenMessage>.Start (fun inbox ->
        let rec loop =
            async{
                let! msg = inbox.Receive()
                match msg with
                    |OrderDrink((drink,amount)) -> handleOrdering (drink,amount)
                    |LeaveAComment(comment) -> handleComment comment
                return! loop
            }
        loop)
