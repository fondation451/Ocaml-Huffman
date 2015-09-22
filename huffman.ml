type tree =
	| Node of int * char * tree * tree
	| Empty
;;

let make_freq_tab txt =
	let tab = Array.make 256 0 in
	for i = 0 to String.length txt - 1 do
		tab.(int_of_char txt.[i]) <- tab.(int_of_char txt.[i]) + 1
	done ;
	tab
;;

let make_node_list freq_tab =
	let rec insert_node i freq out =
		match out with
		|[] -> (Node(freq, char_of_int i, Empty, Empty))::[]
		|a::b
			-> match a with
				 |Node(p, _, _, _) -> if p < freq
				 											then a::(insert_node i freq b)
				 											else (Node(freq, char_of_int i, Empty, Empty))::out
	in
	let rec loop freq_tab i out =
		if i = 256
		then out
		else if freq_tab.(i) <> 0
				 then loop freq_tab (i + 1) (insert_node i freq_tab.(i) out)
				 else loop freq_tab (i + 1) out
	in
	loop freq_tab 0 []
;;

let rec make_huffman_tree nodes =
	let rec insert_min x l =
		if l = []
		then x::[]
		else 
			match x, (List.hd l) with
			|Node(p, _, _, _), Node(pl, _, _, _)
				-> if p <= pl
					 then x::l
					 else (List.hd l)::(insert_min x (List.tl l))
	in
	match nodes with
	|[] -> Empty
	|a::[] -> a
	|a::b::tail ->
		match a, b with
		|Node(p1, _, _, _), Node(p2, _, _, _)
			-> make_huffman_tree (insert_min (Node(p1 + p2, '0', a, b)) tail)
;;

let make_huffman_table huff_tree =
	let rec loop huff_tree dict tmp_list =
		match huff_tree with
		|Node(p, l, Empty, Empty) -> (l, List.rev tmp_list)::dict
		|Node(p, l, left, rigth) -> loop rigth (loop left dict (false::tmp_list)) (true::tmp_list)
	in
	let rec list8_to_bitv l i b =
		if l = []
		then b(*Bitv.sub b 0 i*)
		else (Bitv.fill b i 1 (List.hd l) ; list8_to_bitv (List.tl l) (i + 1) b)
	in
	let rec alist_of_bitv l lb =
		match l with
		|[] -> lb
		|(c, bool_list)::b -> alist_of_bitv b ((c, list8_to_bitv bool_list 0 (Bitv.create (List.length bool_list) true))::lb)
	in
	alist_of_bitv (loop huff_tree [] []) []
;;

let huff_compress txt huff_table =
	let pos = ref 0
	and tab = Bitv.create ((String.length txt)*8) true
	in
	for i = 0 to String.length txt - 1 do
		let trans_carac = List.assoc txt.[i] huff_table in
		let dpos = Bitv.length trans_carac in
		Bitv.blit trans_carac 0 tab !pos dpos ;
		pos := !pos + dpos
	done ;
	Bitv.sub tab 0 !pos
;;

let huff_decompress comp_txt huff_tree =
	let len = Bitv.length comp_txt
	in
	let rec find_letter comp_txt pos huff_tree =
		match huff_tree with
		|Node(p, l, Empty, Empty) -> l
		|Node(p, l, left, rigth) -> if Bitv.get comp_txt !pos
																then (pos := !pos + 1 ; find_letter comp_txt pos rigth)
																else (pos := !pos + 1 ; find_letter comp_txt pos left)
	in
	let rec loop comp_txt pos huff_tree txt =
		if !pos = len
		then txt
		else loop comp_txt pos huff_tree ((find_letter comp_txt pos huff_tree)::txt)
	in
	let rec make_string l out i =
		if i < 0
		then Bytes.unsafe_to_string out
		else (Bytes.set out i (List.hd l) ; make_string (List.tl l) out (i - 1))
	in
	let txt_l = loop comp_txt (ref 0) huff_tree [] in
	let txt_len = List.length txt_l in
	make_string txt_l (Bytes.create txt_len) (txt_len - 1)
;;

let compress txt =
	let freq_tab = make_freq_tab txt in
	let node_list = make_node_list freq_tab in
	let huff_tree = make_huffman_tree node_list in
	let huff_table = make_huffman_table huff_tree in
	let txt_comp = huff_compress txt huff_table in
	(huff_tree, txt_comp)
;;


let decompress mess =
	match mess with
	|(huff_tree, txt_comp)
		-> huff_decompress txt_comp huff_tree
;;
