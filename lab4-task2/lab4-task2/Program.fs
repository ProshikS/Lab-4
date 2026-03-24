type Tree = 
    | Node of string * Tree * Tree
    | Empty

    //вывод дерева
let rec print tree space = //вывод дерева
    match tree with
    | Node (value, left, right)
        ->print right (space+ "\t")
          printfn "%sNode %s" space value
          print left (space + "\t")
    | Empty
        -> ()

let rec insert value tree = //вставка
    match tree with
    |Empty -> Node(value, Empty, Empty)
    |Node(v,left,right) -> 
        if value < v then
            Node(v, insert value left, right)
        elif value > v then
            Node(v, left, insert value right)
        else
            tree

let rnd = System.Random() 

//алфавит для создания случайной строки
let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789" 

//создание случайной строки
let rec randString (str:string) (len:int) = 
    if len = 0 then
        str
    else
        //случайный индекс от 0 до длины алфавита
        let randomIndex = rnd.Next(alphabet.Length)
        let randomChar = alphabet.[randomIndex].ToString()
        randString (str+randomChar) (len-1)

//заполнение дерева
let rec fillTree n tree = //заполнение дерева
    if n = 0 then
        tree
    else
        let value = randString "" 4
        fillTree (n - 1) (insert value tree)

//рекурсивная функция свёртки
let rec foldTree f acc tree =
    match tree with
    | Empty -> acc
    | Node (value, left, right) ->
        let accLeft = foldTree f acc left
        let accRight = foldTree f accLeft right
        f accRight value

//функция для проверки наличия элемента
let contains value tree =
    let folder acc nodeValue =
        // если нашли, оставляем true
        if acc then 
            true        
        // иначе проверяем текущий узел
        else 
            nodeValue = value    
    foldTree folder false tree

[<EntryPoint>]
let main argv =
    let binTree = fillTree 8 Empty
    printfn "Дерево:"
    print binTree ""
    printf "Введите элемент: "
    let element = System.Console.ReadLine()
    let res = contains element binTree
    if res then
        printfn"Дерево содержит элемент %s" element
    else
        printfn"Дерево не содержит элемент %s" element
    0