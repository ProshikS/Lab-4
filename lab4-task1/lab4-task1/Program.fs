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
let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" 

//создание случайной строки
let rec RandString (str:string) (len:int) = 
    if len = 0 then
        str
    else
        //случайный индекс от 0 до длины алфавита
        let randomIndex = rnd.Next(alphabet.Length)
        let randomChar = alphabet.[randomIndex].ToString()
        RandString (str+randomChar) (len-1)

//создание нового дерева
let rec MapTree conc tree =
    match tree with
    | Empty -> Empty
    | Node (value, left, right) ->
        let newValue = conc value
        let newLeft = MapTree conc left
        let newRight = MapTree conc right
        Node(newValue, newLeft, newRight)

let conc (s:string) = 
    s+s

//заполнение дерева
let rec FillTree depth = 
    if depth = 0 then Empty
    else
        let value = RandString "" 4
        let left = FillTree (depth - 1)
        let right = FillTree (depth - 1)
        Node(value, left, right)

[<EntryPoint>]
let main argv =
    let binTree = FillTree(4)
    printfn "Исходное дерево:"
    Print binTree ""

    let newBinTree = MapTree conc binTree
    printfn "\nДерево после изменения:"
    Print newBinTree ""
    0