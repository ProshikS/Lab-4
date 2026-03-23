type Tree = 
    | Node of string * Tree * Tree
    | Nil

let rec print tree space = //вывод дерева
    match tree with
    | Node (value, left, right)
        ->print right (space+ "\t")
          printfn "%sNode %s" space value
          print left (space + "\t")
    | Nil
        -> ()
 
let rec insert value tree = //вставка
    match tree with
    |Nil -> Node(value, Nil, Nil)
    |Node(v,left,right) -> 
        if value < v then
            Node(v, insert value left, right)
        elif value > v then
            Node(v, left, insert value right)
        else
            tree


let rec mapTree f tree = //создание нового дерева
    match tree with
    | Nil -> Nil
    | Node (value, left, right) ->
        let newValue = f value
        let newLeft = mapTree f left
        let newRight = mapTree f right
        Node(newValue, newLeft, newRight)

let rnd = System.Random() 

//алфавит для создания случайной строки
let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" 

//создание случайной строки
let rec randString (str:string) (len:int) = 
    if len = 0 then
        str
    else
        //случайный индекс от 0 до длины алфавита
        let randomIndex = rnd.Next(alphabet.Length)
        let randomChar = alphabet.[randomIndex].ToString()
        randString (str+randomChar) (len-1)

let conc (s:string) = 
    s+s

let rec fillTree n tree = //заполнение дерева
    if n = 0 then
        tree
    else
        let value = randString "" 4
        fillTree (n - 1) (insert value tree)

[<EntryPoint>]
let main argv =
    let binTree = fillTree 8 Nil
    printfn "Исходное дерево:"
    print binTree ""

    let newBinTree = mapTree conc binTree
    printfn "\nДерево после изменения:"
    print newBinTree ""
    0