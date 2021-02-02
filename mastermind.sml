(*
This program implements a mastermind game.
The computer is the board master and the user
has to make guesses until...

The tokens "g", "r", "y", "b" are used to represent
green, red, yellow and blue respectively.

Author: Vincent Ramdhanie
Date: 11 February 2010
*)

(*Start by opening the TextIO library for input and output of text data.*)
open TextIO;

(*Next we need a random number generator*)
val ran = Random.rand(1, 20);
fun random() = Random.randRange(1, 4) ran;

(* member returns true if c is a member of the list, false otherwise. Of course there is a built in member function. But we are trying to learn the language here are we not?*)

fun member(c, []) = false |
    member(c, h::t) = c = h orelse member(c, t);
(*inPosition returns true is c is in the ith position ina given list*)
fun inPosition(c, i, []) = false |
    inPosition(c, 0, h::_) = c = h |
    inPosition(c, i, _::t) = inPosition(c, i - 1, t);


(*We need a datatype to track character correctness*)
datatype correct = A | B | C;


(*This function takes a character and a position and returns a correct value*)
fun charCorrect(c, i, L) =
    if member(c, L) then
        if inPosition(c, i, L) then
            A
        else
            B
    else
        C;

(*Remove all newLine characters from a char list*)
fun removeNewLine([]) = [] |
    removeNewLine(#"\n"::_) = [] |
    removeNewLine(h::t) = h::removeNewLine(t);

(*Remove newline characters from a string *)
fun trimString(s) = implode(removeNewLine(explode(s)));

(*Ensure that only the characters #"g", #"r", #"b" and #"y" are in a string*)
local val chars = [#"g", #"r", #"b", #"y"] in
fun limitChars([]) = true |
    limitChars(h::t) = member(h, chars) andalso limitChars(t)
end;

fun limitString(s) = limitChars(explode(s));

(* Generate Random characters*)
fun randomChar() =
    let
        val r = random()
    in
        case r of
            1 => #"g" |
            2 => #"b" |
            3 => #"r" |
            4 => #"y"
    end;

(*Generate just n characters*)
fun nRandomChars(0) = [] |
    nRandomChars(n) = randomChar()::nRandomChars(n - 1);

(*Create a string of 4 characters*)
fun randomCharString() = implode(nRandomChars(4));

(*Some user interaction functions*)

(*This function gets an input line from the user*)
fun inLine() = valOf(inputLine(stdIn));

(*Prompt the user for input*)
fun prompt(msg) = (print(msg ^ "\r\n");inLine())

(*The Yes No datatype*)
datatype answer = Yes | No;

(*The Valid Invalid datatype*)
datatype valids = Valid | Invalid;

(*Prompt the user for a Yes No value*)
fun yesNoPrompt(msg) =
    let
        val ans = prompt(msg)
    in
        if ans = "Yes\n" then
            Yes
        else
            No
    end;

(* Get a cleaned up guess*)
fun guessPrompt() =
    let
        val guess = prompt("Please enter 4 character guess using the characters g, r, b and y")
    in
        trimString(guess)
    end;


(* Validate Guess*)
fun getValidGuess() =
    let
        val inp = guessPrompt()
        val lim = limitString(inp)
    in
        if (size(inp)  = 4) andalso lim then
            inp
        else
            (print("Sorry, That input is invalid. Please try again.\n\n");getValidGuess())
    end;

(*  Print a report*)
fun printResult([]) = print("_______________________\n") |
    printResult(h::t) =
    if h = A then
        (print("Correct Position\n");printResult(t))
    else
        if h = B then
            (print("Incorrect Position\n");printResult(t))
        else
            (print("Incorrect Token\n");printResult(t))

(*Determine the quality of a guess*)
fun processGuessAux(original, _, []) = [] |
    processGuessAux(original,i, h::t) =  charCorrect(h, i, original)::processGuessAux(original,i + 1, t);

fun processGuess(original, guess) =
    if original = guess then
        true
    else
        (printResult(processGuessAux(explode(original), 0, explode(guess)));false)
   

(*A run of the Game*)
fun gameRun(original) =
    let
        val guess = getValidGuess()
                val result = processGuess(original, guess)
    in
        if result then
            print("Congratulations. You Win!")
        else
            gameRun(original)
    end;

(*Game Control*)
fun playAgain() =
    let
        val ans = yesNoPrompt("Do you want to play again? (Yes/No):\n")
    in
        if ans = Yes then
            play()
        else
            print("Thank You for playing.")
    end
and
   play() =
    let
        val original = randomCharString()
    in
        (gameRun(original);playAgain())
    end;
