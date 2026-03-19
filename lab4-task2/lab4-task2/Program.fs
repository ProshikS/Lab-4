type Tree = 
    | Node of string * Tree * Tree
    | Empty

//вывод дерева
let rec Print tree space = 
    match tree with
    | Node (data, left, right)
        ->printfn "%sNode %s" space data
          Print left (space + "\t")
          Print right (space+ "\t")
    | Empty
        -> ()

let rnd = System.Random() 

//алфавит для создания случайной строки
let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789" 

//создание случайной строки
let rec RandString (str:string) (len:int) = 
    if len = 0 then
        str
    else
        //случайный индекс от 0 до длины алфавита
        let randomIndex = rnd.Next(alphabet.Length)
        let randomChar = alphabet.[randomIndex].ToString()
        RandString (str+randomChar) (len-1)

//заполнение дерева
let rec FillTree depth = 
    if depth = 0 then Empty
    else
        let value = RandString "" 4
        let left = FillTree (depth - 1)
        let right = FillTree (depth - 1)
        Node(value, left, right)

//рекурсивная функция свёртки
let rec FoldTree f acc tree =
    match tree with
    | Empty -> acc
    | Node (value, left, right) ->
        let accLeft = FoldTree f acc left
        let accRight = FoldTree f accLeft right
        f accRight value

//функция для проверки наличия элемента
let Contains value tree =
    let Folder acc nodeValue =
        // если нашли, оставляем true
        if acc then 
            true        
        // иначе проверяем текущий узел
        else 
            nodeValue = value    
    FoldTree Folder false tree

[<EntryPoint>]
let main argv =
    let binTree = FillTree(4)
    printfn "Дерево:"
    Print binTree ""
    printf "Введите элемент: "
    let element = System.Console.ReadLine()
    let res = Contains element binTree
    if res then
        printfn"Дерево содержит элемент %s" element
    else
        printfn"Дерево не содержит элемент %s" element
    0