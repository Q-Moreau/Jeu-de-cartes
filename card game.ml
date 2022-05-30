(* MOREAU Quentin

OCaml Project - card game

*)



(* definition of types *)


type rank = Ace|King|Queen|Jack|Value of int;;
type suit = Heart|Spade|Diamond|Club;;
type playingCard = rank * suit;;

type 'a stack = {mutable numberOfElementsS: int; mutable contentS: 'a list}
type 'a queue = {mutable numberOfElementsQ: int; mutable contentQ: 'a list}
type game = {war: (playingCard*playingCard) stack; player1Cards: playingCard queue; player2Cards: playingCard queue};;










(******************************* STACK AND QUEUE *******************************)


(*test if a satck is empty*)

let emptyStack (stack: 'a stack) =
  stack.numberOfElementsS = 0;;

(*test if a queue is empty*)

let emptyQueue (queue: 'a queue) =
  queue.numberOfElementsQ = 0;;

(*push a stack to put an element at the top*)

let push (stack: 'a stack) element =
  stack.contentS <- element::stack.contentS;
  stack.numberOfElementsS <- stack.numberOfElementsS+1;;

(*add an element to the end of a queue*)

let add (queue: 'a queue) element =
  queue.contentQ <- queue.contentQ@[element];
  queue.numberOfElementsQ <- queue.numberOfElementsQ+1;;

(*pop a stack to get the first element*)

let pop (stack: 'a stack) =
  let head = List.hd stack.contentS in
  stack.contentS <- List.tl stack.contentS;
  stack.numberOfElementsS <- stack.numberOfElementsS-1;
  head;;

(*take the first element from a queue*)

let take (queue: 'a queue) =
  let head = List.hd queue.contentQ in
  queue.contentQ <- List.tl queue.contentQ;
  queue.numberOfElementsQ <- queue.numberOfElementsQ-1;
  head;;







(******************************* CARD *******************************)


(*return the value of a card*)

let returnValue card =
  match card with
  | (Ace,_) -> 14
  | (King,_) -> 13
  | (Queen,_) -> 12
  | (Jack,_) -> 11
  | (Value value,_) -> value;;

(*associate a rank to a number*)

let associateRank number =
  let associatedRank = number mod 13 in
  match associatedRank with
  | 12 -> Ace
  | 11 -> King
  | 10 -> Queen
  | 9 -> Jack
  | associatedRank -> Value (associatedRank+2);;

(*associate a suit to a number*)

let associateSuit number =
  let associatedSuit = number mod 4 in
  match associatedSuit with
  | 0 -> Heart
  | 1 -> Spade
  | 2 -> Diamond
  | 3 -> Club;;

(*associate a card to a number*)

let associateCard number =
  (associateRank number, associateSuit number);;






(******************************* RANDOM *******************************)

  
(*create an ordered deck*)

let createDeck : playingCard array =
  let deck = Array.make 52 (Ace,Heart) in
  for i=1 to 52 do
    deck.(i-1) <- (associateCard i);
  done;
  deck;;


(*shuffle a deck count times according to a seed*)

let shuffleDeck (orderedDeck: playingCard array) seed count : playingCard stack=
  let unorderedDeck = ({numberOfElementsS = 0; contentS = []}: playingCard stack) in
  let swapCard = ref orderedDeck.(0) in
  let fibonacci0 = ref 1 in
  let fibonacci1 = ref 1 in
  let fibonacci2 = ref seed in
  for j = 1 to count do
    for i = 52 downto 1 do
      fibonacci0 := !fibonacci2 mod i;
      fibonacci2 := (!fibonacci2 + !fibonacci1) mod i;
      fibonacci1 := !fibonacci0;
      swapCard := orderedDeck.(!fibonacci2);
      orderedDeck.(!fibonacci2) <- orderedDeck.(i-1);
      orderedDeck.(i-1) <- !swapCard;
    done;
  done;
  for i = 0 to 51 do
    push unorderedDeck orderedDeck.(i);
  done;
  unorderedDeck;;


(*create a random deck*)

let randomDeck = shuffleDeck (createDeck);;






(******************************* GAME *******************************)


(*print card value*)

let printCardValue card =
  match card with
  | (Ace,_) -> print_string "Ace of "
  | (King,_) -> print_string "King of "
  | (Queen,_) -> print_string "Queen of "
  | (Jack,_) -> print_string "Jack of "
  | (Value value,_) -> (print_int value; print_string " of ");;

(*print card suit*)

let printCardSuit card =
  match card with
  | (_,Heart) -> print_string "Hearts"
  | (_,Spade) -> print_string "Spades"
  | (_,Diamond) -> print_string "Diamonds"
  | (_,Club) -> print_string "Clubs";;

(*print card name*)

let printCard card =
  printCardValue card;
  printCardSuit card;;

  


(*deal cards and create a game*)

let rec dealCards seed count =
  let deck = randomDeck seed count in
  let player1Cards = {numberOfElementsQ = 0; contentQ = []} in
  let player2Cards = {numberOfElementsQ = 0; contentQ = []} in

  let rec dealCardsAux seed numberOfCards =
    match numberOfCards with
    | 0 -> {war = ({numberOfElementsS = 0; contentS = []}: (playingCard*playingCard) stack);
    player1Cards = player1Cards;
    player2Cards = player2Cards}
    | numberOfCards when numberOfCards mod 2 = 0 -> (add player1Cards (pop deck); dealCardsAux seed (numberOfCards-1))
    | numberOfCards -> (add player2Cards (pop deck); dealCardsAux seed (numberOfCards-1)) in

    dealCardsAux seed 52;;
    
    
    



(*the winner of the war gets all of the cards*)

let rec win game winner =
    match (emptyStack game.war) with
    | true -> game
    | false -> let (cardWon1,cardWon2) = pop game.war in
      match winner with
      | 1 -> (add game.player1Cards cardWon1; add game.player1Cards cardWon2; win game winner)
      | 2 -> (add game.player2Cards cardWon1; add game.player2Cards cardWon2; win game winner);;



(*war*)

let rec newWar game =

  if emptyQueue game.player1Cards && emptyQueue game.player2Cards then
    begin
      print_string "Players have no card anymore.\n\n";
      game;
    end
  else if emptyQueue game.player1Cards then
    begin
      print_string "Player 1 has no card anymore.\n\n";
      game;
    end
  else if emptyQueue game.player2Cards then
    begin
      print_string "Player 2 has no card anymore.\n\n";
      game;
    end

  else
    begin
      push game.war (take game.player1Cards, take game.player2Cards);
      let (card1,card2) = List.hd game.war.contentS in
        print_string "Player 1 drew a ";
        printCard card1;
        print_string " and player 2 drew a ";
        printCard card2;
        print_string ".\n";

        match ((returnValue card1), (returnValue card2)) with
        | (valueCard1, valueCard2) when valueCard1 > valueCard2 -> 
          (print_string "Player 1 won the war.\n\n"; win game 1)
        | (valueCard1, valueCard2) when valueCard1 < valueCard2 -> 
          (print_string "Player 2 won the war.\n\n"; win game 2)
        | (valueCard1, valueCard2) ->  newWar game
    end;;


(*establish the outcome of the game*)

let outcome (numberOfCards1, numberOfCards2) =
  match (numberOfCards1, numberOfCards2) with
  | (numberOfCards1, numberOfCards2) when numberOfCards1 > numberOfCards2 -> print_string "\nPlayer 1 won!\n\n\n"
  | (numberOfCards1, numberOfCards2) when numberOfCards1 < numberOfCards2 -> print_string "\nPlayer 2 won!\n\n\n"
  | (numberOfCards1, numberOfCards2) -> print_string "\nDraw...\n\n\n";;


    
(*play*)  

let rec play numberOfBattles seed count =
  let game = dealCards seed count in
  let rec playAux numberOfBattles game =
    let (numberOfCards1, numberOfCards2) = (game.player1Cards.numberOfElementsQ, game.player2Cards.numberOfElementsQ) in
    match (numberOfCards1, numberOfCards2) with
    | (numberOfCards1, numberOfCards2) when numberOfCards1=0 || numberOfCards2=0 -> outcome (numberOfCards1, numberOfCards2)
    | (numberOfCards1, numberOfCards2) -> 
      begin match numberOfBattles with
      | 0 -> outcome (numberOfCards1, numberOfCards2)
      | numberOfBattles -> playAux (numberOfBattles-1) (newWar game)
      end
  in    
  playAux numberOfBattles game;;








play 10 1 3;;

