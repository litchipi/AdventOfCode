module Main where

import qualified Data.Maybe as Maybe
import qualified Data.List as List
import qualified Debug.Trace as Debug
import qualified System.Environment as Environment

circle_total_size = 1000000
nb_rounds = 10000000
test_input = "389125467"
test_result = 149245887792

-- Use a double-linked list to allow mutation inside the data structure
type Label = Int;
data Cup = NoCup | Cup {
	label :: Label,
	next :: Cup,
	previous :: Cup,
	previous_min :: Int,
	focused :: Bool
} deriving(Eq);

label_from_char :: Char -> Label
label_from_char charinp = read [charinp] :: Label

insert_cup :: Cup -> Label -> Bool -> Cup
insert_cup cup inslabel insfocused = new_cup
	where
		prevmin = previous_min $ cup
		nextcup = Cup {label=inslabel, previous=cup, next=(next cup), focused=insfocused, previous_min = min inslabel prevmin}
		new_cup = Cup {label=label cup, previous=previous cup, next=nextcup, focused=(focused cup), previous_min = prevmin}

pop_next_nb :: Cup -> Int -> (Cup, [Label])
pop_next_nb cups 0 = (cups, [])
pop_next_nb initcups nb = (new_cups, other_labels ++ [label $ popped])
	where
		(cups, other_labels) = pop_next_nb initcups (nb-1)
		popped = next $ cups
		new_cups = Cup {
			label=label $ cups,
			previous=previous $ cups,
			next=next $ popped,
			focused=focused $ cups,
			previous_min=previous_min $ cups
		}

find_last :: Cup -> Cup -> Maybe Cup
find_last head curr
  | (next curr) == NoCup = Just curr
  | head == curr = Nothing

build_circle :: [Char] -> Cup
build_circle (fstchar:input) = new_headcup
	where
		extended_input = (map label_from_char input) ++ [((length input) + 1)..circle_total_size]
		initlabel = label_from_char fstchar
		initcup = Cup {
			label = initlabel,
			previous = NoCup,
			next = NoCup,
			focused = True,
			previous_min = initlabel
		}
		boundcups = foldr (\char cup -> insert_cup cup char False) initcup extended_input
		tailcup = Maybe.fromJust $ find_last boundcups boundcups
		new_headcup = Cup {
			label= label $ boundcups,
			previous = new_tailcup,
			next = next $ boundcups,
			focused = focused $ boundcups,
			previous_min = previous_min $ boundcups
		}
		new_tailcup = Cup {
			label = label $ tailcup,
			previous = previous $ tailcup,
			next = new_headcup,
			focused = focused $ tailcup,
			previous_min = previous_min $ tailcup
		}

cup_to_char :: Cup -> Char
cup_to_char intinp | (label intinp) > 9 = undefined
cup_to_char cup = (show $ label $ cup) !! 0

get_result_from_cups :: Cup -> Int
--get_result_from_state state | Debug.trace ("Final state: " ++ (show state)) False = undefined
get_result_from_cups cups = (label num1) * (label num2)
	where
		cup_1 = Maybe.fromJust $ find_by_label cups 1
		num1 = next $ cup_1
		num2 = next $ num2




choose_destination :: Label -> [Label] -> (Int, Int) -> Int
choose_destination dest picked range
  | dest < (fst range) = choose_destination (snd range) picked range
  | elem dest picked = choose_destination (dest - 1) picked range
  | otherwise = dest

find_by_label :: Cup -> Label -> Maybe Cup
find_by_label cups searchlabel
  | (label cups) == searchlabel = Just cups
  | (previous_min cups) > searchlabel = find_by_label (next cups) searchlabel
find_by_label cups searchlabel = if Maybe.isNothing prevres 
	   then if (next cups) == NoCup
			then Nothing
			else find_by_label (next cups) searchlabel
	   else prevres
		   where 
			prevres = find_by_label (previous cups) searchlabel

compute_game :: Int -> Cup -> (Int, Int) -> Cup
--compute_game nrounds state | Debug.trace ("Round " ++ (show ((nb_rounds + 1) - nrounds))) False = undefined
compute_game nrounds currcup range = final_cups
	where
		(newcups, picked) = pop_next_nb currcup 3

		destination_label = choose_destination ((label currcup) - 1) picked range
		destination = Maybe.fromJust $ find_by_label currcup destination_label

		new_cups = foldl (\cup label -> insert_cup cup label False) destination picked
		new_currcup = Maybe.fromJust $ find_by_label new_cups (label currcup)
		
		final_cups = if (nrounds - 1) == 0
				   then new_currcup
				   else compute_game (nrounds - 1) (next new_currcup) range



play_real_game :: Int -> [Char] -> IO ()
play_real_game rounds input = putStrLn ("Result: " ++ (show res))
	where
		initstate = build_circle input
		res = get_result_from_cups $ compute_game nb_rounds initstate (1, circle_total_size)

play_test_game :: Int -> IO ()
play_test_game rounds = putStrLn ("Expected " ++ (show test_result) ++ " got " ++ (show got) ++ " -> " ++ result)
	where
		initstate = build_circle test_input
		got = get_result_from_cups (compute_game rounds initstate (1, circle_total_size))
		result = if got == test_result then "Success" else "Failure"


main :: IO ()
main = do {
		x <- Environment.getArgs;
		if length x < 1
			then putStrLn help_msg
		else
			case (x !! 0) of
				"test" -> play_test_game nb_rounds
				"input" -> play_real_game nb_rounds (x !! 1)

				_ -> putStrLn help_msg
	  }

help_msg = "Arguments expected: \n\
				\    test: Perform a test \n\
				\    input <input>: Perform the computation on the given input"
