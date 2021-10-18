module Main where

import qualified Data.Maybe as Maybe
import qualified Data.List as List
import qualified Debug.Trace as Debug
import qualified System.Environment as Environment

type Cup = Int;
type CupCircle = [Cup];
data GameState = 
	GameState { current_cup_index :: Int, circle :: CupCircle }
  deriving (Eq, Show)


circle_total_size = 1000000
nb_rounds = 10000000
test_input = "389125467"
test_result = 149245887792




cup_from_char :: Char -> Cup
cup_from_char charinp = read [charinp] :: Cup

cup_to_char :: Cup -> Char
cup_to_char intinp | intinp > 9 = undefined
cup_to_char intinp = (show intinp) !! 0




showcircle :: CupCircle -> Cup -> String -> String
-- showcircle circle curr res | Debug.trace ("Showcircle: " ++ (show circle) ++ ", current cup: " ++ (show curr) ++ ", res: " ++ (show res)) False = undefined
showcircle [] curr res = res
showcircle circle curr res = showcircle (drop 1 circle) (curr-1) (res ++ " " ++ new_el ++ " ")
	where
		cupchar = cup_to_char (circle !! 0)
		new_el = if curr == 0
			  then "(" ++ [cupchar] ++ ")"
			  else " " ++ [cupchar] ++ " "

extend_circle_to :: Int -> CupCircle -> Int -> CupCircle
extend_circle_to totsize circle maxi | Debug.trace ("Extend circle size " ++ (show maxi) ++ "/" ++ (show totsize)) False = undefined
extend_circle_to totsize circle maxi | totsize == maxi = circle
extend_circle_to totsize circle maxi = extend_circle_to totsize (circle ++ [maxi + 1]) (maxi+1)

get_init_state :: String -> GameState
get_init_state input | Debug.trace ("Getting initial state from input " ++ (show input)) False = undefined
get_init_state input = GameState { current_cup_index = 0, circle = circle}
	where
		initcircle = map cup_from_char input
		circle = extend_circle_to circle_total_size initcircle (maximum initcircle)

loop_through_circle :: CupCircle -> Int -> Cup -> CupCircle -> CupCircle
loop_through_circle circle index stopat res
 | index == (length circle) = loop_through_circle circle 0 stopat res
 | (circle !! index) == stopat = res
 | otherwise = res ++ [circle !! index] ++ (loop_through_circle circle (index+1) stopat res)

get_result_from_state :: GameState -> Int
--get_result_from_state state | Debug.trace ("Final state: " ++ (show state)) False = undefined
get_result_from_state state = num1 * num2
	where
		index_of_1 = find_index 1 (circle state)
		
		ind1 = (index_of_1 + 1) `mod` (length (circle state))
		num1 = (circle state) !! ind1

		ind2 = (index_of_1 + 2) `mod` (length (circle state))
		num2 = (circle state) !! ind2





pick_cups :: CupCircle -> Int -> Int -> [Cup]
pick_cups list ind len 
  | (ind + len) > (length list) = (pick_cups list ind ((length list) - ind)) ++ take ((ind + len) - (length list)) list
pick_cups list ind len = take len . drop ind $ list

pop_circle :: CupCircle -> Int -> Int -> CupCircle
pop_circle list ind len 
  | (ind + len) > (length list) = drop ((ind + len) - (length list)) (pop_circle list ind ((length list) - ind))
pop_circle list ind len = (take ind list) ++ (drop (ind + len) list)

choose_destination :: Cup -> [Cup] -> (Int, Int) -> Cup
choose_destination dest picked range
  | dest < (fst range) = choose_destination (snd range) picked range
  | elem dest picked = choose_destination (dest - 1) picked range
  | otherwise = dest

find_index :: Cup -> [Cup] -> Int
--find_index val list | Debug.trace ("Finding val " ++ (show val) ++ " from list " ++ (show list)) False = undefined
find_index val list = Maybe.fromMaybe undefined (List.elemIndex val list)

insert_circle :: Int -> [Cup] -> CupCircle -> CupCircle
--insert_circle ind cups circle | Debug.trace ("Inserting " ++ (show cups) ++ " in circle " ++ (show circle) ++ " at index " ++ (show ind)) False = undefined
insert_circle ind cups circle = (take (ind+1) circle) ++ cups ++ (drop (ind+1) circle)

compute_game :: Int -> GameState -> GameState
compute_game nrounds state | Debug.trace ("Round " ++ (show ((nb_rounds + 1) - nrounds))) False = undefined
compute_game nrounds state = final_state
	where
		cups_picked = Debug.trace ("Circle length: " ++ (show (length (circle state)))) (pick_cups (circle state) ((current_cup_index state) + 1) 3)
		circle_popped = pop_circle (circle state) ((current_cup_index state) + 1) 3

		current_cup = (circle state) !! (current_cup_index state)
		range = (minimum (circle state), maximum (circle state))
		destination = choose_destination (current_cup - 1) cups_picked range

		destination_index = find_index destination circle_popped
		new_circle = insert_circle destination_index cups_picked circle_popped

		new_currentcup_index = find_index current_cup new_circle
		next_currentcup_index = if (new_currentcup_index + 1) >= (length (circle state))
							 then (new_currentcup_index + 1) - (length (circle state))
							 else new_currentcup_index + 1

		new_state = GameState { current_cup_index = next_currentcup_index, circle = new_circle }
		final_state = if (nrounds - 1) == 0
				   then new_state
				   else compute_game (nrounds - 1) new_state



play_test_game :: Int -> IO ()
play_test_game rounds = putStrLn ("Expected " ++ (show test_result) ++ " got " ++ (show got) ++ " -> " ++ result)
	where
		initstate = (get_init_state test_input)
		got = get_result_from_state (compute_game rounds initstate) -- (get_init_state test_input))
		result = if got == test_result then "Success" else "Failure"


main :: IO ()
main = do {
		x <- Environment.getArgs;
		if length x < 1
			then putStrLn help_msg
		else
			case (x !! 0) of
				"test" -> play_test_game nb_rounds
				"input" -> putStrLn ("Result: " ++ (show (get_result_from_state (compute_game nb_rounds (get_init_state (x !! 1))))))

				_ -> putStrLn help_msg
	  }

help_msg = "Arguments expected: \n\
				\    test: Perform a test \n\
				\    input <input>: Perform the computation on the given input"
